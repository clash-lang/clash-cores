{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Bit-exact models of Intel's Floating Point Functions IP (@altera_fp_functions@)
in double precision: square root, reciprocal and the arithmetic, see Note
[Bit-exact double precision models].
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.BitExactDouble
  ( sqrtDouble
  , recipDouble
  , addDouble
  , subDouble
  , mulDouble
  , divDouble
  ) where

import Prelude

import Data.Array (Array, listArray, (!))
import Data.Bits
import Data.Ratio (denominator, numerator)
import Data.Word (Word64)

import Clash.Cores.Intel.Floating.Internal.BitExact (Multipliers(..))
import Clash.Cores.Intel.Floating.Internal.CoefficientsDouble (sqrtDoubleCoefficients)

{- Note [Bit-exact double precision models]
In double precision, the IP computes its functions with the algorithms of
single precision (see Note [Bit-exact models] in
"Clash.Cores.Intel.Floating.Internal.BitExact"), scaled up. The models in this
module give the IP's results bit for bit. As in single precision, their
structure was inferred by simulating the generated IP as a black box and from
Quartus's ordinary resource reports, and the coefficients are computed from
first principles (see "Clash.Cores.Intel.Floating.Internal.CoefficientsDouble").
The results depend on the device family, not on the latency or the target
frequency.

The reciprocal is correctly rounded, like in single precision: the IEEE 754
reciprocal rounded to nearest, ties to even, of the input with subnormal
inputs treated as zero and the exponent unbounded, with the quirk at the
bottom of the exponent range of the single precision one (a result whose
biased exponent would be 0 keeps its 52 fraction bits with an exponent field
of 0; results below that flush to a zero of the same sign).

The addition, subtraction, multiplication and division are correctly
rounded like in single precision: they are the IEEE 754 operations, rounded
to nearest with ties to even as if the exponent were unbounded, with
subnormal inputs treated as zero (keeping their sign) and results below the
smallest normal number flushed to a zero of the same sign. Unlike the
reciprocal, the division has no special case at the bottom of the exponent
range. NaN results are always the IP's NaN.

The square root uses a piecewise polynomial of degree five, evaluated in
Horner form, with a 256-entry table indexed by p * 128 + the top 7 fraction
bits (p the parity of the unbiased exponent) and y the low 45 fraction bits:

  S0 = C5
  Sk = C(5-k) + Tk,  Tk = Sk-1 * yk   (k = 1 .. 5)
  A  = C0 + T5

where yk is y without its low 28, 21, 12, 5 and 0 bits, and each product Tk is
truncated: on Cyclone V to 2^-28, 2^-36, 2^-42, 2^-49 and 2^-56 (rounded
down), with A at 2^-56 and a rounding constant of half a unit in the last
place (2^-53). The coefficients have their least significant bits at 2^-55,
2^-48, 2^-41, 2^-35, 2^-27 and 2^-20 in both parities (unlike in single
precision, where the odd parity's tables are one bit coarser).

The products are truncated multipliers, built from the device's multipliers,
that leave out the partial products S_i * y_j (or y_i * S_j) below a
staircase. The IP's generator can build such a multiplier on its own (its
fixed-point "truncated multiplier" component); generating those standalone
for the operand and output widths of each product, as found in the synthesis
report of the IP, and simulating them on random operands gave the exact
staircase of each product. Every partial product is kept or not as a whole
tile of a multiplier, so the staircases are made of rectangles:

  * Cyclone V (27x27 and 18x18 multipliers): T1 and T2 are exact products,
    rounded down. T3, T4 and T5 leave out the partial products listed in
    'sqrtCutsCycloneV', and are rounded down too.
  * MAX 10, Cyclone IV E and Cyclone 10 LP (18x18 multipliers): the operands
    are padded at the bottom to multiples of 17 bits, cut into 17-bit chunks
    and multiplied tile by tile; tiles of low weight are left out, except for
    a corner (an 8x8 or smaller multiplier, as in the synthesis report) at
    their most significant end, see 'sqrtCutsMax10'. T1 to T4 are rounded to
    nearest (half up) at the Cyclone V positions; T5 is rounded down at 2^-58,
    and the rounding constant in A is 2^-53 + 2^-58.

How the structure was inferred:

  * The RAM sizes in the synthesis report give the table sizes and widths; a
    synthesis with Intel's generator option for full multipliers (on MAX 10)
    gave the bits of y each product uses (from the multiplier sizes).
  * The constant term: for y < 32 only C0 and C1 matter, and the outputs
    determined C0 plus the rounding constant exactly in every segment, once the
    coefficients were on the grid of the first parity in both parities.
  * With full multipliers, a coordinate descent over the truncation of each
    product (and its rounding) gave a datapath that matched all outputs, except
    in one segment whose coefficients came out differently from the lattice
    method with Chebyshev nodes; the interpolation points of the minimax
    polynomial (the textbook choice of fpminimax) fixed it.
  * For the truncated multipliers, the multiplier sizes of the synthesis report
    (and the widths of the registers between them) identified the operand and
    output widths of each product, and the standalone multipliers of the
    generator their staircases. Inputs with few nonzero bits of y (only the
    top bits, or only the bottom bits) separated the products; the rounding
    of each product and the constant were scanned.

Zero and subnormal inputs give a zero with the same sign; negative numbers
(including -inf) and NaN give NaN; +inf gives +inf.

The IP generates the NaN 0x7FF0000000000001.
-}

-- | The NaN the IP generates
nanDouble :: Word64
nanDouble = 0x7FF0000000000001

-- | Positive infinity
infDouble :: Word64
infDouble = 0x7FF0000000000000

-- | Fields of a double precision number: sign bit, biased exponent, fraction
fields :: Word64 -> (Bool, Int, Integer)
fields w = (testBit w 63, fromIntegral ((w `shiftR` 52) .&. 0x7FF), toInteger (w .&. 0xFFFFFFFFFFFFF))
{-# INLINE fields #-}

-- | A truncated multiplier: @a * b@ without the partial products @a_i * b_j@
-- with @i < cuts !! j@, for an unsigned @b@ and a two's complement @a@ (whose
-- sign bit is a partial product row like any other: leaving out the bits of
-- @a@ below position @c@ is rounding @a@ down to a multiple of @2^c@)
truncMul :: [Int] -> Integer -> Integer -> Integer
truncMul cuts a b = sum [(a `shiftR` c) `shiftL` (c + j) | (j, c) <- zip [0 ..] cuts, testBit b j]
{-# INLINE truncMul #-}

-- | Cut lists from runs: @(n, c)@ is @n@ rows (bits of the second operand, from
-- the least significant one up) that keep the partial products with the bits
-- of the first operand from position @c@ up
runs :: [(Int, Int)] -> [Int]
runs = concatMap (uncurry replicate)

-- | Truncated products of the square root on MAX 10, Cyclone IV E and Cyclone
-- 10 LP, as cut lists for 'truncMul' (operand widths in brackets)
sqrtCutsMax10 :: ([Int], [Int], [Int], [Int], [Int])
sqrtCutsMax10 =
  ( -- T1 = (y >> 28) [17 bits] * C5 [16 bits]: a 17x17 multiplier and a 4x3
    -- corner that keeps the last bit of y only for the top two bits of C5
    runs [(14, 1), (2, 0)]
    -- T2 = S1 [26 bits] * (y >> 21) [24 bits]: 17x17, 17x18 and 18x17 tiles
  , runs [(8, 9), (16, 0)]
    -- T3 = (y >> 12) [33 bits] * S2 [34 bits, positive]: 17x17, 17x18 and
    -- 18x17 tiles and two 5x5 corners
  , runs [(1, 29), (12, 17), (5, 12), (16, 0)]
    -- T4 = S3 [42 bits] * (y >> 5) [40 bits]: six tiles of a 3x3 grid
  , runs [(7, 25), (17, 8), (16, 0)]
    -- T5 = S4 [51 bits] * y [45 bits]: six tiles of a 3x3 grid and two 8x8
    -- corners
  , runs [(4, 34), (8, 26), (9, 17), (8, 9), (16, 0)]
  )

-- | Truncated products T3, T4 and T5 of the square root on Cyclone V, as cut
-- lists for 'truncMul' (T1 and T2 are exact)
sqrtCutsCycloneV :: ([Int], [Int], [Int])
sqrtCutsCycloneV =
  ( runs [(17, 16), (17, 0)]
  , runs [(14, 24), (9, 15), (17, 0)]
  , runs [(17, 24), (2, 22), (26, 0)]
  )

-- | The table of 'sqrtDouble'
sqrtTable :: Array Int (Integer, Integer, Integer, Integer, Integer, Integer)
sqrtTable = listArray (0, 255) sqrtDoubleCoefficients
{-# NOINLINE sqrtTable #-}

-- * Square root

-- | The IP's square root in double precision, on bit patterns
sqrtDouble :: Multipliers -> Word64 -> Word64
sqrtDouble mults w
  | e == 2047 = if f /= 0 || neg then nanDouble else w
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 63
  | neg = nanDouble
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = (fromIntegral (k + 1023) `shiftL` 52) + fromInteger (o - bit 52)
 where
  (neg, e, f) = fields w
  k = (e - 1023) `shiftR` 1
  p = (e - 1023) .&. 1
  o = sqrtSig mults p f

-- | Significand of sqrt (m * 2^p) in units of 2^-52 (2^52 to 2^53), for the
-- fraction @f@ of m
sqrtSig :: Multipliers -> Int -> Integer -> Integer
sqrtSig mults p f = case mults of
  FullMultipliers ->
    let (cut3, cut4, cut5) = sqrtCutsCycloneV
        s1 = 2 * c4 + (c5 * y1) `shiftR` 16           -- 2^-28
        s2 = 2 * c3 + (s1 * y2) `shiftR` 23           -- 2^-36
        s3 = 2 * c2 + truncMul cut3 y3 s2 `shiftR` 34 -- 2^-42
        s4 = 2 * c1 + truncMul cut4 s3 y4 `shiftR` 40 -- 2^-49
        -- At 2^-56, with the rounding constant 2^-53
        a = 2 * c0 + 8 + truncMul cut5 s4 y `shiftR` 45
    in  a `shiftR` 4
  TruncatedMultipliers ->
    let (cut1, cut2, cut3, cut4, cut5) = sqrtCutsMax10
        -- Rounded half up, at the same positions as on Cyclone V
        s1 = 2 * c4 + (truncMul cut1 y1 c5 + bit 15) `shiftR` 16
        s2 = 2 * c3 + (truncMul cut2 s1 y2 + bit 22) `shiftR` 23
        s3 = 2 * c2 + (truncMul cut3 y3 s2 + bit 33) `shiftR` 34
        s4 = 2 * c1 + (truncMul cut4 s3 y4 + bit 39) `shiftR` 40
        -- At 2^-58, with the rounding constant 2^-53 + 2^-58
        a = 8 * c0 + 33 + truncMul cut5 s4 y `shiftR` 43
    in  a `shiftR` 6
 where
  (c0, c1, c2, c3, c4, c5) = sqrtTable ! (p * 128 + fromInteger (f `shiftR` 45))
  y = f .&. (bit 45 - 1)
  y1 = y `shiftR` 28
  y2 = y `shiftR` 21
  y3 = y `shiftR` 12
  y4 = y `shiftR` 5

-- * Reciprocal

-- | The IP's reciprocal in double precision, on bit patterns
recipDouble :: Word64 -> Word64
recipDouble w
  | e == 2047 = if f /= 0 then nanDouble else s
  -- Zeros and subnormal numbers: an infinity of the same sign
  | e == 0 = s .|. infDouble
  | h < -1023 = s
  | h > 1023 = s .|. infDouble
  -- h = -1023 gives an exponent field of 0 with the fraction bits of q
  | otherwise = s .|. (fromIntegral (h + 1023) `shiftL` 52) .|. fromInteger (q - bit 52)
 where
  (_, e, f) = fields w
  s = w .&. bit 63
  m = f .|. bit 52
  (q0, r) = bit 105 `quotRem` m
  -- 2^105 / m, rounded to nearest, ties to even
  q1 | 2 * r > m || (2 * r == m && odd q0) = q0 + 1
     | otherwise = q0
  -- The result is q * 2^(h - 52); only m = 2^52 gives q1 = 2^53
  (h, q) | q1 >= bit 53 = (1023 - e, q1 `shiftR` 1)
         | otherwise = (1022 - e, q1)

-- * Arithmetic

-- | A double precision number as the arithmetic of the IP sees it: subnormal
-- numbers are zeros
data Value
  = VNaN
  | VInf Bool
  -- ^ Infinity, with its sign
  | VNum Bool Rational
  -- ^ Sign and magnitude (zero for zeros and subnormal numbers)

value :: Word64 -> Value
value w
  | e == 2047 = if f /= 0 then VNaN else VInf s
  | e == 0 = VNum s 0
  | otherwise = VNum s (toRational (f .|. bit 52) * 2 ^^ (e - 1075))
 where
  (s, e, f) = fields w

-- | Round a magnitude to double precision: to nearest, ties to even, with an
-- unbounded exponent; results of 2^1024 or more overflow to infinity and
-- results below 2^-1022 are flushed to zero
roundDouble :: Bool -> Rational -> Word64
roundDouble s a
  | a == 0 = sgn
  | be >= 2047 = sgn .|. infDouble
  | be <= 0 = sgn
  | otherwise = sgn .|. (fromIntegral be `shiftL` 52) .|. fromInteger (q - bit 52)
 where
  sgn = if s then bit 63 else 0
  -- a in [2^e0, 2^(e0 + 1))
  l = bitLength (numerator a) - bitLength (denominator a)
  e0 = if a < 2 ^^ l then l - 1 else l
  -- The significand, in [2^52, 2^53]
  (q0, r) = properFraction (a * 2 ^^ (52 - e0)) :: (Integer, Rational)
  q1 | r > 1 / 2 || (r == 1 / 2 && odd q0) = q0 + 1
     | otherwise = q0
  (q, e) = if q1 == bit 53 then (q1 `shiftR` 1, e0 + 1) else (q1, e0)
  be = e + 1023

-- | The number of bits of a positive integer
bitLength :: Integer -> Int
bitLength = go 0
 where
  go k m = if m == 0 then k else go (k + 1) (m `shiftR` 1)

-- | The IP's addition in double precision, on bit patterns
addDouble :: Word64 -> Word64 -> Word64
addDouble x y = case (value x, value y) of
  (VNaN, _) -> nanDouble
  (_, VNaN) -> nanDouble
  (VInf s, VInf t) -> if s /= t then nanDouble else signed s infDouble
  (VInf s, _) -> signed s infDouble
  (_, VInf t) -> signed t infDouble
  (VNum s a, VNum t b)
    -- Zeros: negative only if both are
    | a == 0 && b == 0 -> signed (s && t) 0
    | v == 0 -> 0
    | otherwise -> roundDouble (v < 0) (abs v)
   where
    v = sgnd s a + sgnd t b
 where
  sgnd s a = if s then negate a else a

-- | The IP's subtraction in double precision, on bit patterns
subDouble :: Word64 -> Word64 -> Word64
subDouble x y = addDouble x (y `xor` bit 63)

-- | The IP's multiplication in double precision, on bit patterns
mulDouble :: Word64 -> Word64 -> Word64
mulDouble x y = case (value x, value y) of
  (VNaN, _) -> nanDouble
  (_, VNaN) -> nanDouble
  (VInf _, VNum _ 0) -> nanDouble
  (VNum _ 0, VInf _) -> nanDouble
  (VInf s, VInf t) -> signed (s /= t) infDouble
  (VInf s, VNum t _) -> signed (s /= t) infDouble
  (VNum s _, VInf t) -> signed (s /= t) infDouble
  (VNum s a, VNum t b) -> roundDouble (s /= t) (a * b)

-- | The IP's division in double precision, on bit patterns
divDouble :: Word64 -> Word64 -> Word64
divDouble x y = case (value x, value y) of
  (VNaN, _) -> nanDouble
  (_, VNaN) -> nanDouble
  (VInf _, VInf _) -> nanDouble
  (VNum _ 0, VNum _ 0) -> nanDouble
  (VInf s, VNum t _) -> signed (s /= t) infDouble
  (VNum s _, VInf t) -> signed (s /= t) 0
  (VNum s _, VNum t 0) -> signed (s /= t) infDouble
  (VNum s a, VNum t b) -> roundDouble (s /= t) (a / b)

-- | Set the sign bit of a bit pattern if the flag is set
signed :: Bool -> Word64 -> Word64
signed s w = if s then w .|. bit 63 else w
