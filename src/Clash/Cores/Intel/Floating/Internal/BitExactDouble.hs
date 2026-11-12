{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Bit-exact models of Intel's Floating Point Functions IP (@altera_fp_functions@)
in double precision: square root, reciprocal square root, reciprocal,
exponential, the hypotenuse and the arithmetic, see Note [Bit-exact double
precision models], Note [Bit-exact double precision hypotenuse],
Note [Bit-exact double precision reciprocal square root] and Note [Bit-exact
double precision exponential].
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.BitExactDouble
  ( sqrtDouble
  , recipDouble
  , expDouble
  , rsqrtDouble
  , hypotDouble
  , addDouble
  , subDouble
  , mulDouble
  , divDouble
  ) where

import Prelude

import Data.Array (Array, listArray, (!), (//))
import Data.Bits
import Data.Ratio (denominator, numerator)
import Data.Word (Word64)

import Clash.Cores.Intel.Floating.Internal.BitExact (Multipliers(..))
import Clash.Cores.Intel.Floating.Internal.CoefficientsDouble
  (expDoubleCoefficients, rsqrtDoubleCoefficients, sqrtDoubleCoefficients)

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
sqrtSig mults p f = sqrtA mults p f `shiftR` sqrtShift mults

-- | The position of the least significant bit of 'sqrtA': A is at 2^-(52 +
-- 'sqrtShift')
sqrtShift :: Multipliers -> Int
sqrtShift FullMultipliers = 4
sqrtShift TruncatedMultipliers = 6

-- | The square root's polynomial with its rounding constant, before the final
-- truncation (see 'sqrtSig')
sqrtA :: Multipliers -> Int -> Integer -> Integer
sqrtA mults p f = case mults of
  FullMultipliers ->
    let (cut3, cut4, cut5) = sqrtCutsCycloneV
        s1 = 2 * c4 + (c5 * y1) `shiftR` 16           -- 2^-28
        s2 = 2 * c3 + (s1 * y2) `shiftR` 23           -- 2^-36
        s3 = 2 * c2 + truncMul cut3 y3 s2 `shiftR` 34 -- 2^-42
        s4 = 2 * c1 + truncMul cut4 s3 y4 `shiftR` 40 -- 2^-49
        -- At 2^-56, with the rounding constant 2^-53
    in  2 * c0 + 8 + truncMul cut5 s4 y `shiftR` 45
  TruncatedMultipliers ->
    let (cut1, cut2, cut3, cut4, cut5) = sqrtCutsMax10
        -- Rounded half up, at the same positions as on Cyclone V
        s1 = 2 * c4 + (truncMul cut1 y1 c5 + bit 15) `shiftR` 16
        s2 = 2 * c3 + (truncMul cut2 s1 y2 + bit 22) `shiftR` 23
        s3 = 2 * c2 + (truncMul cut3 y3 s2 + bit 33) `shiftR` 34
        s4 = 2 * c1 + (truncMul cut4 s3 y4 + bit 39) `shiftR` 40
        -- At 2^-58, with the rounding constant 2^-53 + 2^-58
    in  8 * c0 + 33 + truncMul cut5 s4 y `shiftR` 43
 where
  (c0, c1, c2, c3, c4, c5) = sqrtTable ! (p * 128 + fromInteger (f `shiftR` 45))
  y = f .&. (bit 45 - 1)
  y1 = y `shiftR` 28
  y2 = y `shiftR` 21
  y3 = y `shiftR` 12
  y4 = y `shiftR` 5

{- Note [Bit-exact double precision exponential]
The exponential reduces its argument like the single precision one (see Note
[Bit-exact models] in "Clash.Cores.Intel.Floating.Internal.BitExact"):
e^x = 2^k * e^r with k = floor (x / ln 2) and r = x - k * ln 2 in [0, ln 2),
and evaluates e^r with a piecewise polynomial of degree five. In detail:

  * X = floor (x * 2^53), as a two's complement number.
  * The estimate of k is a table-based constant multiplier (KCM, as in
    FloPoCo and Intel's DSP Builder) of the significand by 1/ln 2: the
    significand m = 1.f (in units of 2^-52), negated for a negative x, is a
    54-bit two's complement number I. It is cut into digits of 4 bits (6 bits
    on Cyclone V), from the least significant end; the top digit is signed.
    Each digit d at position i looks up d * 2^i * C * 2^-52, rounded down to
    a multiple of 2^-60, in a table of its own, where C is 1/ln 2 rounded to
    nearest at 2^-57. The sum of the table outputs is rounded to nearest at
    2^-55 (sR), and k = floor (sR * 2^(e - 1023)), with e the biased
    exponent of x. This estimate is never below floor (x / ln 2), but it is
    one above it for the doubles within about 2/3 of 2^-56 (relative) below a
    multiple of ln 2: then r below is negative and clamped to zero, and the
    result is exactly 2^k.
  * K = k * ln 2 in units of 2^-55: ln 2 rounded to nearest at 2^-63, the
    product rounded down to 2^-62 and then up to 2^-55.
  * r = 4 X - K at 2^-55, clamped to 0 when negative; the table index j is
    r divided by 2^48 (0 to 88) and y the low 48 bits, t = y * 2^-48.
  * The polynomial, in Horner form with the coefficients C0 .. C5 of segment j
    (see "Clash.Cores.Intel.Floating.Internal.CoefficientsDouble"):

      S1 = 2 C4 + T1,  T1 = C5 * (y >> 32)   at 2^-56
      S2 = 2 C3 + T2,  T2 = S1 * (y >> 23)   at 2^-57
      S3 = 2 C2 + T3,  T3 = S2 * (y >> 13)   at 2^-56
      S4 = 2 C1 + T4,  T4 = S3 * (y >> 5)    at 2^-56
      A  = 8 C0 + T5 + rc,  T5 = y * S4      at 2^-58

    and the significand is A >> 6, at 2^-52, in [2^52, 2^53]. A significand
    of 2^53 carries into the exponent k + 1023.
  * Results with an exponent field of 2047 or more are +inf; 0 or less give
    +0. NaN gives the IP's NaN, +inf gives +inf and -inf gives +0, and
    |x| >= 1024 gives +inf or +0.

The products are truncated multipliers as in the square root (see Note
[Bit-exact double precision models]):

  * Cyclone V: T1 and T2 are exact products, rounded down. T3, T4 and T5 leave
    out the partial products of 'expCutsCycloneV' (staircases of the
    device's 27x27 and 18x18 multipliers), and are rounded down. rc = 2^-53.
  * MAX 10, Cyclone IV E and Cyclone 10 LP: the tiling of the square root
    (17-bit chunks, tiles of low weight left out except for a corner), see
    'expCutsMax10'. T1 to T4 are rounded to nearest (half up), T5 is rounded
    down, rc = 2^-53 + 2^-58.

How the structure was inferred:

  * The coefficients and the datapath with full multipliers (Intel's generator
    option) were found as for the square root: for k = 0, inputs just above
    j / 128 isolate C0, C1 and the rounding constant; a coordinate descent over
    the truncation of each product gave the rest. The table sizes and widths
    come from the synthesis report.
  * The truncated multipliers: the multiplier sizes and register widths of the
    synthesis report gave each product's operand and output widths. Generating
    the same products standalone with the IP's generator and simulating them
    gave their staircases: on Cyclone V with operands that have only one
    chunk of bits set (e.g. only the top 27 bits of both operands), which
    isolates the partial products of each multiplier of the synthesis report.
    Inside the IP, T5 keeps a few partial products less than the standalone
    product (one row shorter on Cyclone V; the first row of each step on
    MAX 10): inputs whose result lies exactly on a rounding boundary of the
    model told which ones.
  * The constant multiplier: the synthesis report names its digits and gives
    their widths (13 of 4 bits and one of 2 bits, or 9 of 6 bits on Cyclone V,
    and a 58-bit sum). Only the doubles closest to a
    multiple of ln 2 (one per multiple at most, all in the test set) show
    whether k is one too high; the table precision, the rounding of the
    tables, of the sum and of the constant were chosen among the usual
    choices for such a multiplier by matching those inputs.
-}

-- | Truncated products T3, T4 and T5 of the exponential on Cyclone V, as cut
-- lists for 'truncMul'
expCutsCycloneV :: ([Int], [Int], [Int])
expCutsCycloneV =
  ( -- T3 = S2 [37 bits] * (y >> 13) [35 bits]
    runs [(9, 19), (9, 10), (17, 0)]
    -- T4 = S3 [45 bits] * (y >> 5) [43 bits]: a 27x27 multiplier, a sum of
    -- two 18x18 multipliers, two small ones and a 4x4 corner
  , runs [(13, 27), (4, 23), (5, 18), (4, 14), (17, 0)]
    -- T5 = y [49 bits] * S4 [51 bits]
  , runs [(22, 22), (4, 18), (25, 0)]
  )

-- | Truncated products T2 to T5 of the exponential on MAX 10, Cyclone IV E
-- and Cyclone 10 LP, as cut lists for 'truncMul' (T1 is exact)
expCutsMax10 :: ([Int], [Int], [Int], [Int])
expCutsMax10 =
  ( -- T2 = S1 [27 bits] * (y >> 23) [25 bits]
    runs [(9, 10), (16, 0)]
    -- T3 = S2 [37 bits] * (y >> 13) [35 bits]
  , runs [(2, 29), (9, 20), (8, 12), (9, 3), (7, 0)]
    -- T4 = S3 [45 bits] * (y >> 5) [43 bits]
  , runs [(10, 28), (17, 11), (16, 0)]
    -- T5 = y [49 bits] * S4 [51 bits]: the tiling rule keeps the rows of S4
    -- in steps of 8 or 9 (cuts 41, 32, 24, 15, 7 and 0); the IP leaves out
    -- the partial products of the first row of each step with i + j < 41
  , runs [(1, 41), (1, 40), (8, 32), (1, 31), (7, 24), (1, 23), (8, 15), (1, 14), (7, 7), (1, 6), (15, 0)]
  )

-- | The table of 'expDouble'
expTable :: Array Int (Integer, Integer, Integer, Integer, Integer, Integer)
expTable = listArray (0, 88) expDoubleCoefficients
{-# NOINLINE expTable #-}

-- * Exponential

-- | The IP's exponential in double precision, on bit patterns
expDouble :: Multipliers -> Word64 -> Word64
expDouble mults w
  | e == 2047 = if f /= 0 then nanDouble else if neg then 0 else infDouble
  | e > 1032 = if neg then 0 else infDouble
  | be >= 2047 = infDouble
  | be <= 0 = 0
  | otherwise = (fromIntegral be `shiftL` 52) .|. fromInteger (o .&. (bit 52 - 1))
 where
  (neg, e, f) = fields w
  -- The significand (also for zeros and subnormal numbers) and x * 2^53,
  -- rounded down
  m = f .|. bit 52
  mx = if neg then negate m else m
  x53 = if e >= 1022 then mx `shiftL` (e - 1022) else mx `shiftR` (1022 - e)
  k = expK mults mx e
  -- k * ln 2 at 2^-55: ln 2 at 2^-63 (rounded to nearest), the product rounded
  -- down to 2^-62 and then up to 2^-55
  kln2 = negate (negate ((k * 0x58B90BFBE8E7BCD6) `shiftR` 1) `shiftR` 7)
  r = max 0 (4 * x53 - kln2)
  o0 = expSig mults (fromInteger (r `shiftR` 48)) (r .&. (bit 48 - 1))
  (o, be) = if o0 >= bit 53 then (o0 `shiftR` 1, fromInteger k + 1024) else (o0, fromInteger k + 1023 :: Int)

-- | The IP's estimate of floor (x / ln 2), for the significand @mx@ of x
-- (negated for a negative x) and its biased exponent @e@: a table-based
-- constant multiplier, see Note [Bit-exact double precision exponential]
expK :: Multipliers -> Integer -> Int -> Integer
expK mults mx e = sR `shiftR` (55 - (e - 1023))
 where
  -- 1/ln 2 at 2^-57, rounded to nearest
  c = 0x2E2A8ECA5705FC3
  -- The digit width: the size of the device's lookup tables
  dw = case mults of
    TruncatedMultipliers -> 4
    FullMultipliers -> 6
  nd = (54 + dw - 1) `quot` dw
  digit i
    | i == nd - 1 = mx `shiftR` (dw * i)
    | otherwise = (mx `shiftR` (dw * i)) .&. (bit dw - 1)
  -- d * 2^(dw i) * c * 2^-57 * 2^-52, rounded down at 2^-60
  s = sum [((digit i * c) `shiftL` (dw * i)) `shiftR` 49 | i <- [0 .. nd - 1]]
  sR = (s + 16) `shiftR` 5

-- | Significand of e^r in units of 2^-52 (2^52 to 2^53), for the segment @j@
-- and the 48 low bits @y@ of r
expSig :: Multipliers -> Int -> Integer -> Integer
expSig mults j y = case mults of
  FullMultipliers ->
    let (cut3, cut4, cut5) = expCutsCycloneV
        s1 = 2 * c4 + (c5 * y1) `shiftR` 15            -- 2^-56
        s2 = 2 * c3 + (s1 * y2) `shiftR` 24            -- 2^-57
        s3 = 2 * c2 + truncMul cut3 s2 y3 `shiftR` 36  -- 2^-56
        s4 = 2 * c1 + truncMul cut4 s3 y4 `shiftR` 43  -- 2^-56
        -- At 2^-58, with the rounding constant 2^-53
        a = 8 * c0 + 32 + truncMul cut5 y s4 `shiftR` 46
    in  a `shiftR` 6
  TruncatedMultipliers ->
    let (cut2, cut3, cut4, cut5) = expCutsMax10
        -- Rounded half up
        s1 = 2 * c4 + (c5 * y1 + bit 14) `shiftR` 15            -- 2^-56
        s2 = 2 * c3 + (truncMul cut2 s1 y2 + bit 23) `shiftR` 24  -- 2^-57
        s3 = 2 * c2 + (truncMul cut3 s2 y3 + bit 35) `shiftR` 36  -- 2^-56
        s4 = 2 * c1 + (truncMul cut4 s3 y4 + bit 42) `shiftR` 43  -- 2^-56
        -- At 2^-58, with the rounding constant 2^-53 + 2^-58
        a = 8 * c0 + 33 + truncMul cut5 y s4 `shiftR` 46
    in  a `shiftR` 6
 where
  (c0, c1, c2, c3, c4, c5) = expTable ! j
  y1 = y `shiftR` 32
  y2 = y `shiftR` 23
  y3 = y `shiftR` 13
  y4 = y `shiftR` 5

{- Note [Bit-exact double precision reciprocal square root]
The reciprocal square root uses the scheme of the square root (see Note
[Bit-exact double precision models]): 1 / sqrt x = 2^-k * 2 / sqrt (m * 2^p)
for x = m * 2^(2k + p), with the significand 2 / sqrt (m * 2^p) in (1, 2]
from a piecewise polynomial of degree five, evaluated in Horner form. Its
table has 512 entries, indexed by p * 256 + the top 8 fraction bits, with
p = 1 for an even biased exponent (else 0); y is the low 44 fraction bits.
The coefficients have their least significant bits at 2^-55, 2^-47, 2^-39,
2^-32, 2^-23 and 2^-15 in both parities (in units of y * 2^-52):

  S1 = 2 C4 + T1,  T1 = C5 * (y >> 29)   at 2^-24
  S2 = 2 C3 + T2,  T2 = S1 * (y >> 20)   at 2^-33
  S3 = 2 C2 + T3,  T3 = S2 * (y >> 11)   at 2^-40
  S4 = 2 C1 + T4,  T4 = S3 * (y >> 4)    at 2^-48
  A  = C0 + T5 + rc,  T5 = S4 * y

The significand is A at 2^-52, in [2^52, 2^53]; a significand of 2^53 (only
for x = 2^(2k + 1)) carries into the exponent field (3068 - e) >> 1, with e
the biased exponent of x.

  * Cyclone V: T1 and T2 are exact products, rounded down. T3, T4 and T5 leave
    out the partial products of 'rsqrtCutsCycloneV' and are rounded down; A is
    at 2^-56 and rc = 2^-53.
  * MAX 10, Cyclone IV E and Cyclone 10 LP: T1 is an exact product; T2 to T5
    are the 17-bit tilings of the square root, see 'rsqrtCutsMax10'. T1 to T4
    are rounded to nearest (half up) at the Cyclone V positions, T5 is rounded
    to nearest at 2^-57, and A is at 2^-57 with rc = 2^-53.

How the structure was inferred: the datapath with full multipliers (Intel's
generator option) as for the square root, the table sizes and widths from the
synthesis report. The register widths in the MAX 10 synthesis report (26, 35
and 50 bits for S1, S2 and S4) and its multiplier sizes (one 17x17 multiplier
for T1; three, four, six and eight multipliers for T2 to T5, with two 5x5
corners in T3 and in T5) gave each product's operand and output widths;
the tiling rule of the square root then gives the products, and a scan over
their widths, output positions and rounding matched all outputs. The Cyclone
V staircases were found by scanning one- and two-step staircases per product,
on inputs with small y for T5 first.

Zero and subnormal inputs give an infinity of the same sign; negative numbers
(including -inf) and NaN give NaN; +inf gives +0.
-}

{- Note [Observed double precision rsqrt coefficients]
The coefficients of the reciprocal square root are computed from first
principles (see "Clash.Cores.Intel.Floating.Internal.CoefficientsDouble"),
with one exception: in 2 of its 512 segments, 320 and 426, the IP's results
show other coefficients than any first-principles method we tried. In these
two segments the model uses the coefficients of 'rsqrtObservedCoefficients'
instead, which were observed from the IP's outputs (with Intel's generator
option for full multipliers): for inputs with small y only C0, C1 and the
rounding constant matter, and the outputs pin down C0; among the lattice
vectors near the generator's, the one with a single rounding step of Babai's
algorithm flipped gave the IP's results on all 200,000 inputs per segment,
and the model with it matches the IP on 685,000 more inputs in these two
segments on all four families.

No recipe reproduces them while keeping the other 510 segments: Chebyshev
nodes (6 to 24, first and second kind), the interpolation points or extrema
of the minimax polynomial, uniform nodes and overdetermined node sets;
absolute and relative error; LLL with delta 1/2, 3/4 or 0.99 in all 720
orders of the basis, fplll's LLL, Kannan's embedding and exact closest
vectors among the candidates; the interval [0, 2^-8] with its end moved;
coefficient grids one bit finer or coarser; nodes, targets or the constant
sqrt 2 rounded to precisions from 24 to 200 bits; and choosing among nearby
lattice vectors by the error of the polynomial or of the datapath. The two
segments are not the ones nearest to a tie in Babai's rounding (others with
a fraction closer to 1/2 match), and their errors are within the range of the
other segments.

The IP's coefficients minus the generator's, in units of their least
significant bits (C0 to C5):

  segment 320: [0, +1, -14, +105, -74, +35]
  segment 426: [+1, -20, +112, -510, +253, -91]

This is the only data observed from the IP in the models of this module.
-}

-- | Coefficients @(C0, .., C5)@ of the reciprocal square root that the IP
-- uses in segments 320 and 426, where they differ from the generated ones.
-- These were observed from the IP's outputs, see Note [Observed double
-- precision rsqrt coefficients].
rsqrtObservedCoefficients :: [(Int, (Integer, Integer, Integer, Integer, Integer, Integer))]
rsqrtObservedCoefficients =
  [ (320, (45573223974324343, -71208162459884, 166894130771, -869240252, 1188381, -3309))
  , (426, (39498439913699104, -46359671260220, 81619139596, -319323942, 328055, -729))
  ]

-- | Truncated products T2 to T5 of the reciprocal square root on MAX 10,
-- Cyclone IV E and Cyclone 10 LP, as cut lists for 'truncMul' (T1 is exact)
rsqrtCutsMax10 :: ([Int], [Int], [Int], [Int])
rsqrtCutsMax10 =
  ( -- T2 = S1 [26 bits] * (y >> 20) [24 bits]: 17x17, 17x18 and 18x17 tiles
    runs [(8, 9), (16, 0)]
    -- T3 = S2 [35 bits] * (y >> 11) [33 bits]: three tiles and two 5x5
    -- corners
  , runs [(12, 18), (5, 13), (12, 1), (4, 0)]
    -- T4 = S3 [42 bits] * (y >> 4) [40 bits]: six tiles of a 3x3 grid
  , runs [(7, 25), (17, 8), (16, 0)]
    -- T5 = S4 [50 bits] * y [44 bits]: six tiles of a 3x3 grid and two 5x5
    -- corners
  , runs [(6, 33), (5, 28), (12, 16), (5, 11), (16, 0)]
  )

-- | Truncated products T3, T4 and T5 of the reciprocal square root on Cyclone
-- V, as cut lists for 'truncMul' (T1 and T2 are exact)
rsqrtCutsCycloneV :: ([Int], [Int], [Int])
rsqrtCutsCycloneV =
  ( runs [(16, 17), (17, 0)]
  , runs [(14, 24), (9, 15), (17, 0)]
  , runs [(18, 23), (26, 0)]
  )

-- | The table of 'rsqrtDouble': the generated coefficients, with the observed
-- ones in segments 320 and 426
rsqrtTable :: Array Int (Integer, Integer, Integer, Integer, Integer, Integer)
rsqrtTable = listArray (0, 511) rsqrtDoubleCoefficients // rsqrtObservedCoefficients
{-# NOINLINE rsqrtTable #-}

-- * Reciprocal square root

-- | The IP's reciprocal square root in double precision, on bit patterns
rsqrtDouble :: Multipliers -> Word64 -> Word64
rsqrtDouble mults w
  | e == 2047 = if f /= 0 || neg then nanDouble else 0
  -- Zeros and subnormal numbers: an infinity of the same sign
  | e == 0 = (w .&. bit 63) .|. infDouble
  | neg = nanDouble
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = (fromIntegral ((3068 - e) `shiftR` 1) `shiftL` 52) + fromInteger (o - bit 52)
 where
  (neg, e, f) = fields w
  o = rsqrtSig mults (if even e then 1 else 0) f

-- | Significand of 2 / sqrt (m * 2^p) in units of 2^-52 (2^52 to 2^53), for
-- the fraction @f@ of m
rsqrtSig :: Multipliers -> Int -> Integer -> Integer
rsqrtSig mults p f = case mults of
  FullMultipliers ->
    let (cut3, cut4, cut5) = rsqrtCutsCycloneV
        s1 = 2 * c4 + (c5 * y1) `shiftR` 14           -- 2^-24
        s2 = 2 * c3 + (s1 * y2) `shiftR` 23           -- 2^-33
        s3 = 2 * c2 + truncMul cut3 s2 y3 `shiftR` 34 -- 2^-40
        s4 = 2 * c1 + truncMul cut4 s3 y4 `shiftR` 40 -- 2^-48
        -- At 2^-56, with the rounding constant 2^-53
        a = 2 * c0 + 8 + truncMul cut5 s4 y `shiftR` 44
    in  a `shiftR` 4
  TruncatedMultipliers ->
    let (cut2, cut3, cut4, cut5) = rsqrtCutsMax10
        -- Rounded half up
        s1 = 2 * c4 + (c5 * y1 + bit 13) `shiftR` 14                -- 2^-24
        s2 = 2 * c3 + (truncMul cut2 s1 y2 + bit 22) `shiftR` 23    -- 2^-33
        s3 = 2 * c2 + (truncMul cut3 s2 y3 + bit 33) `shiftR` 34    -- 2^-40
        s4 = 2 * c1 + (truncMul cut4 s3 y4 + bit 39) `shiftR` 40    -- 2^-48
        -- At 2^-57, with the rounding constant 2^-53
        a = 4 * c0 + 16 + (truncMul cut5 s4 y + bit 42) `shiftR` 43
    in  a `shiftR` 5
 where
  (c0, c1, c2, c3, c4, c5) = rsqrtTable ! (p * 256 + fromInteger (f `shiftR` 44))
  y = f .&. (bit 44 - 1)
  y1 = y `shiftR` 29
  y2 = y `shiftR` 20
  y3 = y `shiftR` 11
  y4 = y `shiftR` 4

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

{- Note [Bit-exact double precision hypotenuse]
The three-dimensional hypotenuse sqrt (x^2 + y^2 + z^2) works like the single
precision one (see Note [Bit-exact models] in
"Clash.Cores.Intel.Floating.Internal.BitExact"), scaled up, and uses the
square root's datapath (see Note [Bit-exact double precision models]):

  * Each square m^2 of a nonzero input's significand m = 1.f is truncated to 53
    bits: a significand M (2^52 <= M < 2^53) with exponent E = 2 (e - 1023) + h,
    h = 1 when m^2 >= 2. Zero and subnormal inputs contribute nothing.
  * With Emax the largest E, each square is aligned to 2^Emax, truncated at
    2^(Emax - 54); squares more than 53 binades below Emax are left out. The
    sum is exact.
  * The sum is rounded to nearest, ties away from zero, to 53 significant bits,
    and its square root is computed with the square root's polynomial ('sqrtA')
    plus an extra half unit in the last place: 2^-53 on top of the square
    root's own rounding constant.
  * Any NaN gives the IP's NaN, otherwise any infinity gives +inf, and three
    zeros give +0. Results with an exponent field of 2047 or more are +inf.
    Signs are ignored.

How it was found: the single precision recipe with every width scaled to
double precision matched all outputs at once, on MAX 10 and Cyclone V
(random inputs, special values, exponent differences up to 70, sums with few
bits, inputs near overflow and underflow).
-}

-- | The IP's three-dimensional hypotenuse in double precision, on bit patterns
hypotDouble :: Multipliers -> Word64 -> Word64 -> Word64 -> Word64
hypotDouble mults a b c
  | any (\(e, f) -> e == 2047 && f /= 0) xs = nanDouble
  | any (\(e, _) -> e == 2047) xs = infDouble
  | null sqs = 0
  | be >= 2047 = infDouble
  | otherwise = (fromIntegral be `shiftL` 52) .|. fromInteger (o .&. (bit 52 - 1))
 where
  xs = [(e, f) | w <- [a, b, c], let (_, e, f) = fields w]
  -- The squares, truncated to 53 bits: (exponent, significand)
  sqs =
    [ (2 * (e - 1023) + h, m2 `shiftR` (52 + h))
    | (e, f) <- xs, e /= 0
    , let m = f .|. bit 52
    , let m2 = m * m
    , let h = fromInteger (m2 `shiftR` 105)
    ]
  emax = maximum (map fst sqs)
  -- Aligned at 2^(emax - 54)
  s = sum [(m `shiftL` 2) `shiftR` d | (e, m) <- sqs, let d = emax - e, d <= 53]
  n = bitLength s
  q0 = (s + bit (n - 54)) `shiftR` (n - 53)
  (q, et) = if q0 >= bit 53 then (q0 `shiftR` 1, emax + n - 54) else (q0, emax + n - 55)
  p = et .&. 1
  k = et `shiftR` 1
  o0 = (sqrtA mults p (q - bit 52) + bit (sqrtShift mults - 1)) `shiftR` sqrtShift mults
  (o, be) = if o0 >= bit 53 then (o0 `shiftR` 1, k + 1024) else (o0, k + 1023)
