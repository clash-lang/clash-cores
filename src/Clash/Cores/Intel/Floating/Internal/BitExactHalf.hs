{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Bit-exact models of Intel's Floating Point Functions IP (@altera_fp_functions@)
in half precision: square root, cube root, hypotenuse, reciprocal square
root, reciprocal, exponentials, logarithms, the sine, cosine and tangent
of pi times x and the arcsine (in radians and divided by pi), see
Note [Bit-exact half precision models].
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.BitExactHalf
  ( sqrtHalf
  , rsqrtHalf
  , recipHalf
  , expHalf
  , exp2Half
  , exp10Half
  , logHalf
  , log2Half
  , log10Half
  , log1pHalf
  , cbrtHalf
  , hypotHalf
  , sinPiHalf
  , cosPiHalf
  , tanPiHalf
  , asinHalf
  , asinPiHalf
  ) where

import Prelude

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
import Data.Int (Int64)
import Data.Word (Word16)

import Clash.Cores.Intel.Floating.Internal.BitExact (Multipliers(..))
import Clash.Cores.Intel.Floating.Internal.CoefficientsHalf
  ( cbrtHalfCoefficients, exp10HalfCoefficients, exp2HalfCoefficients, expHalfCoefficients
  , log10HalfCoefficients, log2HalfCoefficients, logHalfCoefficients, rsqrtHalfCoefficients
  , sinPiHalfCoefficients, sqrtHalfCoefficients, tanPiRecipHalfCoefficients
  , asinHalfCoefficients, asinLargeHalfCoefficients )

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

In the exponentials and the logarithms the product C1 * y is handled the same
way: rounded down to the LSB of A on Cyclone V, and truncated three bits below
it with one unit added there on the other families.

Exponential (the structure of single precision with 2^-11 for 2^-24 and
2^-13 for 2^-26):

  1. X = floor (x * 2^11) in two's complement; zeros and subnormal numbers
     keep their hidden bit, so e^-0 is 0x3BFF. |x| >= 16 overflows or
     underflows.
  2. k = floor (x / ln 2), estimated from the significand times 1/ln 2
     (rounded down to 34 fraction bits) truncated to 14 fraction bits relative
     to the significand; just below a multiple of ln 2 it can be one too high,
     then r below is negative and clamped to 0.
  3. r = x - k * ln 2 in units of 2^-13, with k * ln 2 computed from ln 2
     rounded down to 21 fraction bits, rounded up to 13.
  4. e^r with a 45-entry table indexed by the top 6 bits of r, y the 7 bits
     below them, C0 and C1 at 2^-13, rounding constant 4 (half a unit in the
     last place), the significand truncated to 2^-10.
  5. A significand of 2 or more is normalised; a biased exponent of 31 or more
     gives +inf, of 0 or less +0. NaN gives NaN, +inf +inf and -inf +0.

Base-2 exponential: X = floor (x * 2^10) (with the hidden bit kept for zeros
and subnormal numbers); k = X / 2^10 is the result's exponent, and 2^f for the
10 fraction bits f of X uses a 32-entry table indexed by the top 5 bits of f,
y the 5 bits below them, otherwise as the exponential.

Base-10 exponential: as the exponential, with log2 10 rounded to nearest at
26 fraction bits for the estimate of k (13 fraction bits relative to the
significand: rounded down on Cyclone V, but rounded up on the other
families), r = x - k * log10 2 in units of 2^-14 with log10 2 rounded to
nearest at 34 fraction bits and k * log10 2 rounded up to 14, and a 39-entry
table indexed by the top 7 bits of r. The rounding constant in C0 is a whole
unit in the last place (8), so 10^0 is 1 + 2^-10 (0x3C01).

These were found as in single precision: the outputs depend on
floor (x * 2^11) only (2^10 for the base-2 exponential); on the inputs with
k = 0 the coefficient format search gave the table sizes and formats; the
constants (the precision of ln 2 and log10 2, the estimators of k and the
roundings) were scanned on all inputs, and the choices above are the ones of
single precision among the ones that give the IP's results on all inputs. The
base-10 exponential's estimate of k differs between the device families on
only four inputs: 0x34D1 (just below log10 2) and its multiples by 2, 4 and
8.

Logarithms (natural, base 2 and base 10): the structure of single precision,
with 32 segments per region (the fraction bits 8 to 4, y the 4 bits below
them) in the four regions of x in [1, 1.5), [1.5, 2), [0.5, 0.75) and
[0.75, 1), and the result rounded once to nearest, ties away from zero:

  * For x in [0.5, 1) the result is (x - 1) * P with P truncated to 2^-10
    (natural), 2^-11 (base 2) or 2^-12 (base 10).
  * Otherwise it is e * L + (x * 2^-e - 1) * P for x = m * 2^e, with P
    truncated to 2^-11 (natural and base 2) or 2^-13 (base 10), and L = ln 2
    and log10 2 rounded to nearest at 12 fraction bits (1 for base 2). For
    base 10, P is evaluated one bit finer here, as if the coefficients were
    doubled, which only shows in the truncated product.
  * The coefficients are at 2^-15 (natural), 2^-14 (base 2) and 2^-16
    (base 10), with a rounding constant of 4.

log (1 + x) uses the natural logarithm's table with P one bit finer (2^-12 for
1 + x in [1, 2), evaluated one bit finer; 2^-11 for 1 + x in [0.5, 1)), with
the structure of single precision: x * P (1 + x) with 1 + x rounded down for
the table lookup if 1 + x is in [0.5, 2), otherwise e * ln 2 + g * P for
1 + x truncated to 11 significant bits, (1 + g * 2^-10) * 2^e.

These were found by porting the structure of single precision: a search over
the coefficient formats, the truncation of P and the precision of the
constants on all inputs gave a unique combination for which the generated
coefficients reproduce the IP's results; the constant of the truncated product
and the finer evaluation for base 10 and log (1 + x) were found on all inputs
of the families with 18x18 multipliers.

Cube root: the square root's datapath on a 96-entry table, indexed by
r * 32 + the top 5 fraction bits for x = m * 2^(3k + r), y the low 5 fraction
bits, with C0 and C1 at 2^-14 and the rounding constant 4 (a quarter unit in
the last place, as in single precision). NaN keeps its sign.

Three-dimensional hypotenuse: as in single precision, with the squares
truncated to 11 bits, aligned to the largest at 2^-12 (a square shifted by
more than 11 places is dropped), the sum rounded to nearest at 11 bits (ties
up) and the square root's datapath with a rounding constant of 8 (an extra
half unit in the last place). These widths were confirmed on all pairs of
non-negative inputs and on random triples: a shift limit of 12, other
alignments or ties to even give different results.

Sine, cosine and tangent of pi times x, as in single precision:

  * Sine: X = floor (|x| * 2^11) mod 2^12, q = X / 2^10 the half period and r
    in [0, 1/2] at 2^-11. P = sin (pi * r) / r is a 64-entry table indexed by
    r * 128 (r = 1/2 is segment 63 with y = 16), y the 4 bits below, C0 and C1
    at 2^-13 with rounding constant 4, truncated to 2^-10. The result is
    m * P rounded to nearest (ties away from zero), with m the significand of
    x for |x| < 2^-6 (and P = P(0)), the significand without its last bit for
    |x| in [2^-6, 1/4) (the outputs ignore the last input bit exactly there),
    and r otherwise. A NaN keeps its sign (in single precision it gets the
    opposite sign); an infinity gives NaN with its sign.
  * Cosine: the distance to the nearest half integer computed exactly (at
    2^-24), the same polynomial, and the distance truncated to 11 significant
    bits times P, rounded to nearest. The product is exact on all families.
  * Tangent: the sine times R, a polynomial approximation of the reciprocal of
    the cosine's significand (128 segments indexed by the top 7 fraction bits,
    y the 3 bits below, C0 and C1 at 2^-16, rounding constant 4, R truncated
    to 2^-13), rounded to nearest. |x| < 2^-7 gives x times pi rounded to half
    precision (3.140625), rounded.

These were found by porting the single precision structure: the intervals of P
per r from the inputs whose r is exact contain a point of the 2^-10 grid
(and not of 2^-9); the coefficient format search on them and on the
reciprocal of the tangent gave the formats; the thresholds of the special
paths and the fixed-point widths were scanned on all inputs.

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

sqrtTable, rsqrtTable, expTable, exp2Table, exp10Table, logTable, log2Table, log10Table :: Table1
sqrtTable = mkTable1 sqrtHalfCoefficients
{-# NOINLINE sqrtTable #-}
rsqrtTable = mkTable1 rsqrtHalfCoefficients
{-# NOINLINE rsqrtTable #-}
expTable = mkTable1 expHalfCoefficients
{-# NOINLINE expTable #-}
exp2Table = mkTable1 exp2HalfCoefficients
{-# NOINLINE exp2Table #-}
exp10Table = mkTable1 exp10HalfCoefficients
{-# NOINLINE exp10Table #-}
logTable = mkTable1 logHalfCoefficients
{-# NOINLINE logTable #-}
log2Table = mkTable1 log2HalfCoefficients
{-# NOINLINE log2Table #-}
log10Table = mkTable1 log10HalfCoefficients
{-# NOINLINE log10Table #-}

cbrtTable, sinPiTable, tanPiRecipTable :: Table1
cbrtTable = mkTable1 cbrtHalfCoefficients
{-# NOINLINE cbrtTable #-}
sinPiTable = mkTable1 sinPiHalfCoefficients
{-# NOINLINE sinPiTable #-}
tanPiRecipTable = mkTable1 tanPiRecipHalfCoefficients
{-# NOINLINE tanPiRecipTable #-}

-- | Fields of a half precision number: sign bit, biased exponent, fraction
fields :: Word16 -> (Bool, Int, Int64)
fields w =
  (testBit w 15, fromIntegral ((w `shiftR` 10) .&. 0x1F), fromIntegral (w .&. 0x3FF))
{-# INLINE fields #-}

-- | The polynomial @C0 + rc + C1 * y@ in the units of the table, for the
-- segment @s@ and the @wy@ bits @y@ below the segment's bits: the product is
-- rounded down to the LSB of the table (Cyclone V), or truncated three bits
-- below it with one LSB there added (families with 18x18 multipliers)
poly1 :: Table1 -> Multipliers -> Int64 -> Int -> Int -> Int64 -> Int64
poly1 table mults rc wy s y = c0 + rc + prod1 mults wy 0 c1 y
 where
  (c1, c0) = coefs1 table s

-- | The product @C1 * y / 2^wy@ of 'poly1', with @C1@ doubled @x@ times
prod1 :: Multipliers -> Int -> Int -> Int64 -> Int64 -> Int64
prod1 mults wy x c1 y = case mults of
  FullMultipliers -> ((c1 * y) `shiftL` x) `shiftR` wy
  TruncatedMultipliers -> (((c1 * y) `shiftL` x) + bit (wy - 3)) `shiftR` wy
{-# INLINE prod1 #-}

-- | The polynomial of the square root and the reciprocal square root: the
-- significand in units of 2^-10 (2^10 to 2^11) for the segment @s@ and the
-- @wy@ low fraction bits @y@. Coefficients at 2^-13, rounding constant 4.
rootPoly :: Table1 -> Multipliers -> Int -> Int -> Int64 -> Int64
rootPoly table mults wy s y = poly1 table mults 4 wy s y `shiftR` 3

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

-- * Exponentials

-- | @floor (x * 2^n)@ in two's complement for @|x| = m * 2^(sh - n)@ with
-- @m < 2^11@ (below 2^-n the significand is shifted out completely)
expFixed :: Bool -> Int -> Int64 -> Int64
expFixed neg sh m = if neg then negate xAbs else xAbs
 where
  -- |x| rounded down for positive x and up for negative x
  xAbs | sh >= 0 = m `shiftL` sh
       | otherwise =
           let d = min 12 (negate sh)
           in  (m `shiftR` d) + (if neg && m .&. (bit d - 1) /= 0 then 1 else 0)

-- | The result of the exponentials from the unbiased exponent @k@ and the
-- significand @o0@ in units of 2^-10 (in [1, 4)): normalised, with overflow
-- to +inf and underflow to +0 (no subnormal results)
expResult :: Int64 -> Int64 -> Word16
expResult k o0
  | be >= 31 = infHalf
  | be <= 0 = 0
  | otherwise = (fromIntegral be `shiftL` 10) .|. fromIntegral (o .&. 0x3FF)
 where
  (o, be) | o0 >= bit 11 = (o0 `shiftR` 1, k + 16)
          | otherwise = (o0, k + 15)

-- | The IP's exponential in half precision, on bit patterns
expHalf :: Multipliers -> Word16 -> Word16
expHalf mults w
  | e == 31 = if f /= 0 then nanHalf else if neg then 0 else infHalf
  -- |x| >= 16
  | e >= 19 = if neg then 0 else infHalf
  | otherwise = expResult k (poly1 expTable mults 4 7 (fromIntegral (r `shiftR` 7)) (r .&. 127) `shiftR` 3)
 where
  (neg, e, f) = fields w
  -- The hidden bit is set even for zeros and subnormal numbers
  m = f .|. bit 10
  -- x = floor (x * 2^11)
  x = expFixed neg (e - 14) m
  -- m / ln 2 to 14 fraction bits (relative to m), with 1/ln 2 rounded down to
  -- 34 fraction bits
  q = (if neg then negate else id) ((m * 24785312074) `shiftR` 30)
  -- k = floor (x / ln 2), estimated
  k = q `shiftR` (29 - e)
  -- k * ln 2 with ln 2 rounded down to 21 fraction bits, rounded up to 13
  kLn2 = negate (negate (k * 1453634) `shiftR` 8)
  -- r = x - k * ln 2 in units of 2^-13; when k is one too high, r = 0
  r = max 0 (4 * x - kLn2)

-- | The IP's base-2 exponential in half precision, on bit patterns
exp2Half :: Multipliers -> Word16 -> Word16
exp2Half mults w
  | e == 31 = if f /= 0 then nanHalf else if neg then 0 else infHalf
  -- |x| >= 32
  | e >= 20 = if neg then 0 else infHalf
  | otherwise =
      expResult (x `shiftR` 10)
        (poly1 exp2Table mults 4 5 (fromIntegral ((x `shiftR` 5) .&. 31)) (x .&. 31) `shiftR` 3)
 where
  (neg, e, f) = fields w
  -- x = floor (x * 2^10), with the hidden bit set even for zeros and
  -- subnormal numbers
  x = expFixed neg (e - 15) (f .|. bit 10)

-- | The IP's base-10 exponential in half precision, on bit patterns
exp10Half :: Multipliers -> Word16 -> Word16
exp10Half mults w
  | e == 31 = if f /= 0 then nanHalf else if neg then 0 else infHalf
  -- |x| >= 16
  | e >= 19 = if neg then 0 else infHalf
  | otherwise = expResult k (poly1 exp10Table mults 8 7 (fromIntegral (r `shiftR` 7)) (r .&. 127) `shiftR` 3)
 where
  (neg, e, f) = fields w
  -- The hidden bit is set even for zeros and subnormal numbers
  m = f .|. bit 10
  -- x = floor (x * 2^11)
  x = expFixed neg (e - 14) m
  -- m * log2 10 to 13 fraction bits (relative to m), with log2 10 rounded to
  -- nearest at 26 fraction bits; rounded down on Cyclone V and up on the
  -- other families
  q = (if neg then negate else id) ((m * 222930821 + qUp) `shiftR` 23)
  qUp = case mults of
    FullMultipliers -> 0
    TruncatedMultipliers -> bit 23 - 1
  -- k = floor (x * log2 10), estimated
  k = q `shiftR` (28 - e)
  -- k * log10 2 with log10 2 rounded to nearest at 34 fraction bits, rounded
  -- up to 14 fraction bits
  kLog = negate (negate (k * 5171655946) `shiftR` 20)
  -- r = x - k * log10 2 in units of 2^-14; when k is one too high, r = 0
  r = max 0 (8 * x - kLog)

-- * Logarithms

-- | Round a non-zero value in units of 2^-g to half precision: to nearest,
-- ties away from zero. The result must be a normal number.
roundAt :: Int -> Int64 -> Word16
roundAt g s =
      (if s < 0 then bit 15 else 0)
  .|. (fromIntegral be `shiftL` 10)
  .|. fromIntegral (o .&. 0x3FF)
 where
  a = abs s
  -- The number of bits to drop
  d = 64 - countLeadingZeros a - 11
  o0 | d <= 0 = a `shiftL` negate d
     | otherwise = (a + bit (d - 1)) `shiftR` d
  -- The biased exponent: 2^-g * 2^d * 2^10 * 2^15
  (o, be) | o0 >= bit 11 = (o0 `shiftR` 1, d - g + 26)
          | otherwise = (o0, d - g + 25)

-- | The polynomial of the logarithms, @C0 + 4 + C1 * y@ (in the units of the
-- table, or one bit finer when @x@ is 1), for the region @r@ (see
-- 'logHalfCoefficients'), the 10 fraction bits @f@ of the significand
-- (segment @f[8:4]@, @y = f[3:0]@)
logPoly :: Table1 -> Multipliers -> Int -> Int -> Int64 -> Int64
logPoly table mults x r f = ((c0 + 4) `shiftL` x) + prod1 mults 4 x c1 (f .&. 15)
 where
  (c1, c0) = coefs1 table (r * 32 + fromIntegral ((f `shiftR` 4) .&. 31))

-- | The region of 'logHalfCoefficients' of a significand with fraction @f@,
-- for x in @[1, 2)@ (@low@ is 'False') or @[0.5, 1)@ (@low@ is 'True')
logRegion :: Bool -> Int64 -> Int
logRegion low f = (if low then 2 else 0) + (if testBit f 9 then 1 else 0)

-- | The common structure of the logarithms: the special cases, @x@ in
-- @[0.5, 1)@ and the rest
logCommon ::
  -- | The result for x in [0.5, 1) from the fraction
  (Int64 -> Word16) ->
  -- | The result for other x from the unbiased exponent and the fraction
  (Int64 -> Int64 -> Word16) ->
  Word16 ->
  Word16
logCommon lowRes res w
  | e == 31 = if f /= 0 || neg then nanHalf else infHalf
  -- Zeros and subnormal numbers
  | e == 0 = 0xFC00
  | neg = nanHalf
  | e == 15 && f == 0 = 0
  | e == 14 = lowRes f
  | otherwise = res (fromIntegral e - 15) f
 where
  (neg, e, f) = fields w

-- | The IP's natural logarithm in half precision, on bit patterns
logHalf :: Multipliers -> Word16 -> Word16
logHalf mults = logCommon lowRes res
 where
  -- x in [0.5, 1): z = x - 1 at 2^-11, P truncated to 2^-10
  lowRes f = roundAt 21 ((f - bit 10) * (logPoly logTable mults 0 (logRegion True f) f `shiftR` 5))
  -- z at 2^-10, P truncated to 2^-11, ex * ln 2 with ln 2 rounded to 12
  -- fraction bits
  res ex f = roundAt 21 (f * (logPoly logTable mults 0 (logRegion False f) f `shiftR` 4) + (ex * 2839) `shiftL` 9)

-- | The IP's base 2 logarithm in half precision, on bit patterns
log2Half :: Multipliers -> Word16 -> Word16
log2Half mults = logCommon lowRes res
 where
  -- x in [0.5, 1): z = x - 1 at 2^-11, P truncated to 2^-11
  lowRes f = roundAt 22 ((f - bit 10) * (logPoly log2Table mults 0 (logRegion True f) f `shiftR` 3))
  -- z at 2^-10, P truncated to 2^-11, the exponent exactly
  res ex f = roundAt 21 (f * (logPoly log2Table mults 0 (logRegion False f) f `shiftR` 3) + ex `shiftL` 21)

-- | The IP's base 10 logarithm in half precision, on bit patterns
log10Half :: Multipliers -> Word16 -> Word16
log10Half mults = logCommon lowRes res
 where
  -- x in [0.5, 1): z = x - 1 at 2^-11, P truncated to 2^-12
  lowRes f = roundAt 23 ((f - bit 10) * (logPoly log10Table mults 0 (logRegion True f) f `shiftR` 4))
  -- z at 2^-10, P (evaluated one bit finer) truncated to 2^-13,
  -- ex * log10 2 with log10 2 rounded to 12 fraction bits
  res ex f = roundAt 23 (f * (logPoly log10Table mults 1 (logRegion False f) f `shiftR` 4) + (ex * 1233) `shiftL` 11)

-- | The IP's @log (1 + x)@ in half precision, on bit patterns
log1pHalf :: Multipliers -> Word16 -> Word16
log1pHalf mults w
  | e == 31 = if f /= 0 || neg then nanHalf else infHalf
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 15
  -- x <= -1
  | neg && e >= 15 = if e == 15 && f == 0 then 0xFC00 else nanHalf
  -- 1 + x in [0.5, 2): x * P (1 + x), with x = mx * 2^(e - 25)
  | small =
      if v >= bit 11
        then
          -- 1 + x in [1, 2), at 2^-10; P (one bit finer) truncated to 2^-12
          let v' = (v - bit 11) `shiftR` 1
          in  roundAt (37 - e) (mx * (logPoly logTable mults 1 (logRegion False v') v' `shiftR` 4))
        else
          -- 1 + x in [0.5, 1), at 2^-11; P truncated to 2^-11
          let v' = v - bit 10
          in  roundAt (36 - e) (mx * (logPoly logTable mults 0 (logRegion True v') v' `shiftR` 4))
  -- Otherwise 1 + x = (1 + g * 2^-10) * 2^ex truncated to 11 bits:
  -- ex * ln 2 + g * P at 2^-22, ln 2 rounded to 12 fraction bits
  | otherwise =
      roundAt 22
        (g * (logPoly logTable mults 1 (logRegion False g) g `shiftR` 4) + (fromIntegral ex * 2839) `shiftL` 10)
 where
  (neg, e, f) = fields w
  m = f .|. bit 10
  mx = if neg then negate m else m
  -- 1 + x in [0.5, 2): x in (-0.5, 1)
  small = if neg then e <= 13 else e <= 14
  -- 1 + x at 2^-11, rounded down (x = +-m * 2^-(sh + 11))
  sh = 14 - e
  v | neg = bit 11 + (negate m `shiftR` sh)
    | otherwise = bit 11 + (m `shiftR` sh)
  -- 1 + x with its 11 leading bits, as 1 + g * 2^-10 times 2^ex
  (ex, g)
    -- 1 is below the last bit of x
    | e > 25 = (e - 15, f)
    | otherwise =
        let -- 1 + x at 2^-(25 - e), x is positive or in (-1, -0.5]
            u = bit (25 - e) + mx
            nb = 64 - countLeadingZeros u
            u' | nb >= 11 = u `shiftR` (nb - 11)
               | otherwise = u `shiftL` (11 - nb)
        in  (nb - 1 - (25 - e), u' - bit 10)

-- * Cube root

-- | The IP's cube root in half precision, on bit patterns
cbrtHalf :: Multipliers -> Word16 -> Word16
cbrtHalf mults w
  -- NaN keeps its sign
  | e == 31 = if f /= 0 then sgn .|. nanHalf else w
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = sgn
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = sgn .|. ((fromIntegral (k + 15) `shiftL` 10) + fromIntegral (o - bit 10))
 where
  (_, e, f) = fields w
  sgn = w .&. bit 15
  -- x = m * 2^(3k + r), cbrt x = cbrt (m * 2^r) * 2^k
  (k, r) = (e - 15) `divMod` 3
  -- Coefficients at 2^-14, rounding constant 4 (a quarter unit in the last
  -- place)
  o = poly1 cbrtTable mults 4 5 (r * 32 + fromIntegral (f `shiftR` 5)) (f .&. 31) `shiftR` 4

-- * Hypotenuse

-- | The IP's three-dimensional hypotenuse, sqrt (x^2 + y^2 + z^2), in half
-- precision, on bit patterns
hypotHalf :: Multipliers -> Word16 -> Word16 -> Word16 -> Word16
hypotHalf mults x y z
  | any isNaN' xs = nanHalf
  | any isInf' xs = infHalf
  | null sqs = 0
  | be >= 31 = infHalf
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = (fromIntegral (k + 15) `shiftL` 10) + fromIntegral (o - bit 10)
 where
  xs = [x, y, z]
  isNaN' w = let (_, e, f) = fields w in e == 31 && f /= 0
  isInf' w = let (_, e, f) = fields w in e == 31 && f == 0
  -- The squares of the normal inputs as floating point numbers: the exponent
  -- and the significand, truncated to 11 bits, at 2^-10
  sqs = [square e f | w <- xs, let (_, e, f) = fields w, e /= 0]
  square e f =
    let m = f .|. bit 10
        s = m * m
        h = fromIntegral (s `shiftR` 21)
     in (2 * (e - 15) + h, s `shiftR` (10 + h))
  eMax = maximum (map fst sqs)
  -- Aligned to the largest square at 2^-12, rounded down; a square shifted by
  -- more than 11 places counts as zero. The sum is in [1, 12) (at 2^-12).
  sumSq = sum [(m `shiftL` 2) `shiftR` d | (e, m) <- sqs, let d = eMax - e, d <= 11]
  l | sumSq >= bit 14 = 2
    | sumSq >= bit 13 = 1
    | otherwise = 0 :: Int
  -- Rounded to nearest at 11 bits, ties up
  q0 = (sumSq `shiftR` (1 + l) + 1) `shiftR` 1
  (q, l') = if q0 >= bit 11 then (q0 `shiftR` 1, l + 1) else (q0, l)
  -- The square root of q * 2^(eMax + l' - 10), with an extra half unit in the
  -- last place before the truncation
  (k, p) = (eMax + l') `divMod` 2
  f' = q - bit 10
  o = (poly1 sqrtTable mults 8 6 (p * 16 + fromIntegral (f' `shiftR` 6)) (f' .&. 63)) `shiftR` 3
  be = k + 15 + fromIntegral (o `shiftR` 11)

-- * Sine, cosine and tangent of pi times x

-- | P = sin (pi * r) / r at 2^-10 for r = (j + y * 2^-4) / 128; r = 1/2 is
-- segment 63 with y = 16
sinPiPoly :: Multipliers -> Int64 -> Int64
sinPiPoly mults r = poly1 sinPiTable mults 4 4 j y `shiftR` 3
 where
  (j, y) | r >= bit 10 = (63, 16)
         | otherwise = (fromIntegral (r `shiftR` 4), r .&. 15)

-- | The IP's @sin (pi * x)@ in half precision, on bit patterns
sinPiHalf :: Multipliers -> Word16 -> Word16
sinPiHalf mults w
  -- NaN with the input's sign, for infinities and NaNs
  | e == 31 = sgn .|. nanHalf
  -- Zeros, subnormal numbers and integers give a zero of the input's sign
  | e == 0 || e >= 25 = sgn
  -- |x| < 2^-6: the significand times sin (pi r) / r at r = 0
  | e <= 8 = sgn .|. roundAt (35 - e) (m * sinPiPoly mults 0)
  | r == 0 = sgn
  -- |x| < 1/4: the significand truncated to 10 bits
  | e <= 13 = sgn' .|. roundAt (34 - e) ((m `shiftR` 1) * p)
  -- Otherwise r is exact
  | otherwise = sgn' .|. roundAt 21 (r * p)
 where
  (_, e, f) = fields w
  sgn = w .&. bit 15
  m = f .|. bit 10
  -- floor (|x| * 2^11) mod 2^12: q is the half period, r in [0, 2^10] the
  -- distance to the nearest integer, in units of 2^-11
  xx | e >= 14 = (m `shiftL` (e - 14)) .&. (bit 12 - 1)
     | otherwise = m `shiftR` (14 - e)
  q = xx `shiftR` 10
  lo = xx .&. (bit 10 - 1)
  r = if odd q then bit 10 - lo else lo
  sgn' = if q >= 2 then sgn `xor` bit 15 else sgn
  p = sinPiPoly mults r

-- | The IP's @cos (pi * x)@ in half precision, on bit patterns
cosPiHalf :: Multipliers -> Word16 -> Word16
cosPiHalf mults w
  | e == 31 = nanHalf
  -- x + 1/2 is an integer
  | rf == 0 = 0
  | otherwise = sgn .|. roundAt g (m * sinPiPoly mults (rf `shiftR` 13))
 where
  (_, e, f) = fields w
  -- floor (|x| * 2^24) mod 2^25 (exact for all normal x; zeros and subnormal
  -- numbers keep the hidden bit): q is the half period, rf in [0, 2^23] the
  -- distance to the nearest half integer, in units of 2^-24
  xx | e >= 1 = ((f .|. bit 10) `shiftL` (e - 1)) .&. (bit 25 - 1)
     | otherwise = (f .|. bit 10) `shiftR` 1
  q = xx `shiftR` 23
  lo = xx .&. (bit 23 - 1)
  rf = if odd q then lo else bit 23 - lo
  sgn = if q == 1 || q == 2 then bit 15 else 0
  -- rf truncated to 11 significant bits, m * 2^-(g - 10)
  nb = 64 - countLeadingZeros rf
  (m, g) | nb > 11 = (rf `shiftR` (nb - 11), 45 - nb)
         | otherwise = (rf, 34)

-- | The IP's @tan (pi * x)@ in half precision, on bit patterns: the IP's sine
-- of pi times x divided by its cosine of pi times x
tanPiHalf :: Multipliers -> Word16 -> Word16
tanPiHalf mults w
  | e == 31 = nanHalf
  -- Zeros and subnormal numbers are returned unchanged
  | e == 0 = w
  -- |x| < 2^-7: x times pi rounded to half precision (1608 * 2^-9), rounded
  | e <= 7 = (w .&. bit 15) .|. roundAt (34 - e) ((bit 10 .|. f) * 1608)
  -- Integers: a zero
  | s .&. 0x7FFF == 0 = sgn
  -- Half integers: an infinity
  | c .&. 0x7FFF == 0 = sgn .|. infHalf
  -- The quotient at 2^-(23 - es + ec)
  | otherwise = sgn .|. roundAt (23 - es + ec) ((bit 10 .|. fs) * r)
 where
  (_, e, f) = fields w
  s = sinPiHalf mults w
  c = cosPiHalf mults w
  sgn = (s `xor` c) .&. bit 15
  (_, es, fs) = fields s
  (_, ec, fc) = fields c
  -- 1 / (1 + fc * 2^-10) at 2^-13; coefficients at 2^-16, rounding constant 4
  r | fc == 0 = bit 13
    | otherwise = poly1 tanPiRecipTable mults 4 3 (fromIntegral (fc `shiftR` 3)) (fc .&. 7) `shiftR` 3

-- * Arcsine

{- Note [Bit-exact half precision arcsine]
The arcsine ('asinHalf', the \"ARCSIN\" function) has the two paths of the
single precision arcsine (Note [Bit-exact arcsine] in
"Clash.Cores.Intel.Floating.Internal.BitExact"), scaled down, and is odd.

For |x| <= 1/2 the polynomial of @asin r / r@ keeps degree two: X =
floor (|x| * 2^13), the top 4 bits of X (16 segments of [0, 1), of which
segments 0 to 8 are used, 8 only for x = 1/2) select the coefficients (on the
grid 2^-15, 'asinHalfCoefficients') and its 9 low bits are y:

  T1 = floor (C2 * (y >> 2) / 2^6)    (at 2^-16)
  S1 = 2 C1 + T1
  T2 = floor (S1 * y / 2^8)           (at 2^-17)
  P  = floor ((4 C0 + 16 + T2) / 2^5) (asin r / r at 2^-12)

and the result is the significand of x times P, rounded to nearest with ties
away from zero. On the families with 18x18 multipliers T1 and T2 are rounded
to nearest (ties up) instead of down.

For 1/2 < |x| < 1, s is the correctly rounded half precision square root of
@2 (1 - |x|)@ and @Z = floor (s * 2^11)@ (z = s / 2 truncated to 2^-12). The
top 6 bits of Z (64 segments of [0, 1/2)) select a degree-one approximation of
@2 asin z@ ('asinLargeHalfCoefficients', coefficients at 2^-15) and y is its
5 low bits. On Cyclone V

  D = floor (pi / 2 * 2^15) - C0 - floor (C1 * y / 2^5) - 2   (at 2^-15)

and on the families with 18x18 multipliers the product is kept whole:

  D = floor (pi / 2 * 2^20) - 2^5 C0 - C1 * y - 2^6           (at 2^-20)

D is rounded to half precision, to nearest with ties away from zero.
@asin 1@ is pi / 2 rounded to nearest (0x3E48), subnormal inputs give a zero
of the same sign and NaN, the infinities and |x| > 1 give NaN.

The structure was found as for single precision. The synthesis report shows a
degree-two polynomial (two multipliers) for the first path and a degree-one
polynomial with a 128-entry table for the second; the coefficient formats,
the segment counts and the rounding constants are the ones for which the
coefficients computed from first principles reproduce the IP's outputs on all
inputs of each path (searched over the formats; the result is unique).

The arcsine divided by pi ('asinPiHalf') multiplies the arcsine, rounded to
half precision, by 1 / pi rounded to half precision (0x3518), rounding to
nearest with ties away from zero; results below the smallest normal number
become a zero of the same sign.
-}

-- | A degree-two table: @(C2, C1, C0)@ per segment
data Table2 = Table2 !(UArray Int Int64) !(UArray Int Int64) !(UArray Int Int64)

mkTable2 :: [(Int, Int, Int)] -> Table2
mkTable2 cs =
  Table2 (arr [c | (c, _, _) <- cs]) (arr [c | (_, c, _) <- cs]) (arr [c | (_, _, c) <- cs])
 where
  arr xs = listArray (0, length cs - 1) (map fromIntegral xs)

-- | The coefficients @(C2, C1, C0)@ of a segment
coefs2 :: Table2 -> Int -> (Int64, Int64, Int64)
coefs2 (Table2 c2 c1 c0) i = (unsafeAt c2 i, unsafeAt c1 i, unsafeAt c0 i)
{-# INLINE coefs2 #-}

asinHalfTable :: Table2
asinHalfTable = mkTable2 asinHalfCoefficients
{-# NOINLINE asinHalfTable #-}

asinLargeHalfTable :: Table1
asinLargeHalfTable = mkTable1 asinLargeHalfCoefficients
{-# NOINLINE asinLargeHalfTable #-}

-- | The IP's arcsine in half precision, on bit patterns, see
-- Note [Bit-exact half precision arcsine]
asinHalf :: Multipliers -> Word16 -> Word16
asinHalf mults w
  | e == 31 || e > 15 || (e == 15 && f /= 0) = nanHalf
  | e == 0 = sign
  -- pi / 2 rounded to nearest
  | e == 15 = sign .|. 0x3E48
  | e < 14 || f == 0 = sign .|. asinSmallHalf mults e f
  -- 2^11 (1 - |x|) = 2^10 - f
  | otherwise = sign .|. asinLargeHalf mults (bit 10 - f)
 where
  (neg, e, f) = fields w
  sign = if neg then bit 15 else 0

-- | @asin x@ for x in [2^-14, 1/2], given its biased exponent and fraction
asinSmallHalf :: Multipliers -> Int -> Int64 -> Word16
asinSmallHalf mults e f =
  fromIntegral (e + sh - 12) `shiftL` 10 .|. fromIntegral (q .&. 0x3FF)
 where
  m = bit 10 .|. f
  -- |x| at 2^-13
  xx | e >= 12 = m `shiftL` (e - 12)
     | otherwise = m `shiftR` (12 - e)
  (c2, c1, c0) = coefs2 asinHalfTable (fromIntegral (xx `shiftR` 9))
  y = xx .&. 0x1FF
  t2 = case mults of
    FullMultipliers ->
      let s1 = 2 * c1 + ((c2 * (y `shiftR` 2)) `shiftR` 6)
      in  (s1 * y) `shiftR` 8
    TruncatedMultipliers ->
      let s1 = 2 * c1 + ((c2 * (y `shiftR` 2) + bit 5) `shiftR` 6)
      in  (s1 * y + bit 7) `shiftR` 8
  -- asin x / x at 2^-12, in [2^12, 2^13)
  p = m * ((4 * c0 + 16 + t2) `shiftR` 5)
  sh0 = if p >= bit 23 then 13 else 12
  q0 = (p + bit (sh0 - 1)) `shiftR` sh0
  (q, sh) = if q0 == bit 11 then (q0 `shiftR` 1, sh0 + 1) else (q0, sh0)

-- | @asin x@ for x in (1/2, 1), given @k = 2^11 (1 - x)@
asinLargeHalf :: Multipliers -> Int64 -> Word16
asinLargeHalf mults k = case mults of
  FullMultipliers ->
    roundAt 15 (51471 - c0 - ((c1 * y) `shiftR` 5) - 2)
  TruncatedMultipliers ->
    roundAt 20 (1647099 - 32 * c0 - c1 * y - 64)
 where
  -- s = sqrt (2 (1 - x)) = sqrt (k * 2^-10), correctly rounded: sm * 2^se
  (sm, se) = crSqrtHalf k (-10)
  -- z = s / 2 at 2^-12
  zz | se + 11 >= 0 = sm `shiftL` (se + 11)
     | otherwise = sm `shiftR` negate (se + 11)
  (c1, c0) = coefs1 asinLargeHalfTable (fromIntegral (zz `shiftR` 5))
  y = zz .&. 0x1F

-- | The correctly rounded half precision square root of @v * 2^ev@ (@v > 0@)
-- as an 11 bit significand @m@ and an exponent @e@: @m * 2^e@
crSqrtHalf :: Int64 -> Int -> (Int64, Int)
crSqrtHalf v0 ev0 = (m, e)
 where
  -- Scale v by a power of four to 25 or 26 bits, so that its root has 13 bits
  k = (25 - (64 - countLeadingZeros v0)) `div` 2 + 1
  v = v0 `shiftL` (2 * k)
  r0 = floor (sqrt (fromIntegral v :: Double)) :: Int64
  r = until (\c -> c * c <= v) (subtract 1) (r0 + 1)
  drop1 = 64 - countLeadingZeros r - 11
  keep = r `shiftR` drop1
  rest = r .&. (bit drop1 - 1)
  half = bit (drop1 - 1)
  up = rest > half || (rest == half && (r * r /= v || odd keep))
  keep' = if up then keep + 1 else keep
  (m, e) | keep' == bit 11 = (keep' `shiftR` 1, (ev0 - 2 * k) `div` 2 + drop1 + 1)
         | otherwise = (keep', (ev0 - 2 * k) `div` 2 + drop1)

-- | The IP's arcsine divided by pi in half precision, on bit patterns:
-- 'asinHalf' times 1 / pi rounded to half precision (0x3518), rounded to
-- nearest, ties away from zero
asinPiHalf :: Multipliers -> Word16 -> Word16
asinPiHalf mults w
  | ea == 31 = a
  | ea == 0 || be <= 0 = sign
  | otherwise = sign .|. (fromIntegral be `shiftL` 10) .|. fromIntegral (q .&. 0x3FF)
 where
  a = asinHalf mults w
  sign = a .&. bit 15
  (_, ea, fa) = fields a
  p = (bit 10 .|. fa) * 0x518
  sh = if p >= bit 21 then 11 else 10
  q0 = (p + bit (sh - 1)) `shiftR` sh
  (q, carry) = if q0 == bit 11 then (q0 `shiftR` 1, 1) else (q0, 0)
  be = ea - 2 + (sh - 10) + carry
