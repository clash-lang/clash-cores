{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Bit-exact models of Intel's Floating Point Functions IP (@altera_fp_functions@)
in half precision, see Note [Bit-exact half precision models].
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.BitExactHalf
  ( sqrtHalf
  , rsqrtHalf
  , recipHalf
  ) where

import Prelude

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
import Data.Int (Int64)
import Data.Word (Word16)

import Clash.Cores.Intel.Floating.Internal.BitExact (Multipliers(..))
import Clash.Cores.Intel.Floating.Internal.CoefficientsHalf
  (rsqrtHalfCoefficients, sqrtHalfCoefficients)

{- Note [Bit-exact half precision models]
The IP supports half precision as a custom format with 5 exponent and 10
fraction bits. It computes the same functions as in single precision (see
Note [Bit-exact models] in "Clash.Cores.Intel.Floating.Internal.BitExact"),
with the same algorithms scaled down: the polynomials have degree one,

  A = C0 + C1 * y

with the top bits of the reduced argument selecting the segment and y the
bits below them. As in single precision, the results depend on the device
family only ('Multipliers'), not on the latency or the target frequency; the
two datapaths differ in how the product C1 * y is truncated.

Since half precision has only 2^16 inputs, the structure and the models were
checked on every input, for every device family supported by Quartus Prime
Lite 21.1 and at three target frequencies (and so latencies) on MAX 10 and
Cyclone V. The way the structure was inferred:

  * The synthesis report of the IP (generated with Intel's generator for MAX 10
    devices, with and without truncated multipliers) shows a single multiplier
    per polynomial, so the polynomials have degree one, and its size gives the
    number of bits of y and of C1. Where the tables are in memory, the report
    gives the table sizes too.
  * With only a few dozen inputs per segment, the IP's outputs leave several
    candidate coefficient pairs in some segments. So the coefficient format
    (the position of the least significant bit of each coefficient, and where
    the product is truncated) was chosen as the one for which the coefficients
    computed from first principles (see
    "Clash.Cores.Intel.Floating.Internal.CoefficientsHalf") reproduce the IP's
    outputs in every segment with a single rounding constant. The models were
    then compared with the IP on all inputs.

Square root (x = m * 2^e with m in [1, 2), p = e mod 2): the table index is
p * 16 + the top 4 fraction bits, y the low 6 fraction bits, C0 and C1 (the
slope over the whole segment) in units of 2^-13. The significand is

  O = (C0 + 4 + floor (C1 * y / 2^6)) / 2^3   (rounded down)

on Cyclone V; on the families with 18x18 multipliers the product is
truncated at 2^-16 instead and 2^-16 is added: floor ((C1 * y + 8) / 2^6). The
rounding constant 4 is half a unit in the last place. A significand of 2 (from
rounding) carries into the exponent. The special cases are those of single
precision.

Reciprocal square root: the table index is p * 32 + the top 5 fraction bits
(p = 1 for an even biased exponent), y the low 5 fraction bits, the segment
approximating 2 / sqrt (m * 2^p) in (1, 2]. As for the square root, with the
product C1 * y truncated to 2^-13 (Cyclone V) or to 2^-16 plus 2^-16 (other
families), and the rounding constant 4. The result's biased exponent is
(44 - E) / 2 (rounded down) for the input's biased exponent E, plus the carry.

The reciprocal is correctly rounded, with the quirk of the single precision
reciprocal at the bottom of the exponent range: a result whose biased exponent
would be 0 keeps its 10 fraction bits with an exponent field of 0.

The IP generates the NaN 0x7C01.
-}

-- | The NaN the IP generates
nanHalf :: Word16
nanHalf = 0x7C01

-- | Positive infinity
infHalf :: Word16
infHalf = 0x7C00

-- | A degree-one coefficient table as two arrays: C1, C0
data Table1 = Table1 !(UArray Int Int64) !(UArray Int Int64)

mkTable1 :: [(Int, Int)] -> Table1
mkTable1 cs = Table1 (arr (map fst cs)) (arr (map snd cs))
 where
  arr xs = listArray (0, length cs - 1) (map fromIntegral xs)

-- | Look up the coefficients of a segment. Callers guarantee that the index is
-- in range.
coefs1 :: Table1 -> Int -> (Int64, Int64)
coefs1 (Table1 c1 c0) i = (unsafeAt c1 i, unsafeAt c0 i)
{-# INLINE coefs1 #-}

sqrtTable, rsqrtTable :: Table1
sqrtTable = mkTable1 sqrtHalfCoefficients
{-# NOINLINE sqrtTable #-}
rsqrtTable = mkTable1 rsqrtHalfCoefficients
{-# NOINLINE rsqrtTable #-}

-- | Fields of a half precision number: sign bit, biased exponent, fraction
fields :: Word16 -> (Bool, Int, Int64)
fields w =
  (testBit w 15, fromIntegral ((w `shiftR` 10) .&. 0x1F), fromIntegral (w .&. 0x3FF))
{-# INLINE fields #-}

-- | The polynomial of the square root and the reciprocal square root: the
-- significand in units of 2^-10 (2^10 to 2^11) for the segment @s@ and the
-- @wy@ low fraction bits @y@
rootPoly :: Table1 -> Multipliers -> Int -> Int -> Int64 -> Int64
rootPoly table mults wy s y = (c0 + 4 + prod) `shiftR` 3
 where
  (c1, c0) = coefs1 table s
  -- C1 * y at 2^-13
  prod = case mults of
    FullMultipliers -> (c1 * y) `shiftR` wy
    -- Truncated at 2^-16, plus 2^-16
    TruncatedMultipliers -> (c1 * y + bit (wy - 3)) `shiftR` wy

-- | The IP's square root in half precision, on bit patterns
sqrtHalf :: Multipliers -> Word16 -> Word16
sqrtHalf mults w
  | e == 31 = if f /= 0 || neg then nanHalf else w
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 15
  | neg = nanHalf
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = (fromIntegral (k + 15) `shiftL` 10) + fromIntegral (o - bit 10)
 where
  (neg, e, f) = fields w
  k = (e - 15) `shiftR` 1
  p = (e - 15) .&. 1
  o = rootPoly sqrtTable mults 6 (p * 16 + fromIntegral (f `shiftR` 6)) (f .&. 63)

-- | The IP's reciprocal square root in half precision, on bit patterns
rsqrtHalf :: Multipliers -> Word16 -> Word16
rsqrtHalf mults w
  | e == 31 = if f /= 0 || neg then nanHalf else 0
  -- Zeros and subnormal numbers: an infinity of the same sign
  | e == 0 = (w .&. bit 15) .|. infHalf
  | neg = nanHalf
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = (fromIntegral ((44 - e) `shiftR` 1) `shiftL` 10) + fromIntegral (o - bit 10)
 where
  (neg, e, f) = fields w
  p = if even e then 1 else 0
  o = rootPoly rsqrtTable mults 5 (p * 32 + fromIntegral (f `shiftR` 5)) (f .&. 31)

-- | The IP's reciprocal in half precision, on bit patterns: correctly rounded,
-- see Note [Bit-exact half precision models]
recipHalf :: Word16 -> Word16
recipHalf w
  | e == 31 = if f /= 0 then nanHalf else s
  -- Zeros and subnormal numbers: an infinity of the same sign
  | e == 0 = s .|. infHalf
  | h < -15 = s
  | h > 15 = s .|. infHalf
  -- h = -15 gives an exponent field of 0 with the fraction bits of q
  | otherwise = s .|. (fromIntegral (h + 15) `shiftL` 10) .|. fromIntegral (q - bit 10)
 where
  (_, e, f) = fields w
  s = w .&. bit 15
  m = f .|. bit 10
  (q0, r) = bit 21 `quotRem` m
  -- 2^21 / m, rounded to nearest, ties to even
  q1 | 2 * r > m || (2 * r == m && odd q0) = q0 + 1
     | otherwise = q0
  -- The result is q * 2^(h - 10); only m = 2^10 gives q1 = 2^11
  (h, q) | q1 >= bit 11 = (15 - e, q1 `shiftR` 1)
         | otherwise = (14 - e, q1)
