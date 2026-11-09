{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Bit-exact models of the square root, reciprocal square root, reciprocal,
exponential and natural logarithm of Intel's Floating Point Functions IP
(@altera_fp_functions@) in single precision, see Note [Bit-exact models].
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.BitExact
  ( Multipliers(..)
  , sqrtSingle
  , rsqrtSingle
  , recipSingle
  , expSingle
  , logSingle
  ) where

import Prelude

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
import Data.Int (Int64)
import Data.Word (Word32)

import Clash.Cores.Intel.Floating.Internal.Coefficients
  (expCoefficients, logCoefficients, rsqrtCoefficients, sqrtCoefficients)

{- Note [Bit-exact models]
The Floating Point Functions user guide documents the latency and the
accuracy of the IP's functions, but not how it computes them. The models in
this module give the IP's results bit for bit. Their structure (table sizes,
the position of every coefficient's least significant bit, the widths and
rounding of intermediate results and how the multipliers are cut) was
inferred by simulating the generated IP as a black box: choosing inputs and
observing outputs, and from Quartus's ordinary resource reports (RAM sizes and
multiplier sizes of a synthesis). The polynomial coefficients are computed from
first principles, see "Clash.Cores.Intel.Floating.Internal.Coefficients"; no
data was taken from the IP.

The square root, the reciprocal square root, the exponential and the logarithm
use a piecewise polynomial of degree two, evaluated in Horner form:

  T1 = C2 * y'      (y' = the top bits of y)
  S1 = C1 + T1
  A  = C0 + S1 * y

where the top bits of the reduced argument select the segment (the table
index) and y holds the bits below them. The IP has two datapaths for this,
selected by the device family ('Multipliers'):

  * 'FullMultipliers' (Cyclone V, with 27x27 multipliers): T1 is rounded down,
    S1 * y is exact or rounded down.
  * 'TruncatedMultipliers' (MAX 10, Cyclone IV E, Cyclone 10 LP, with 18x18
    multipliers): T1 is rounded half up, and S1 * y is a truncated multiplier
    that leaves out the partial products below a "staircase" of tiles,
    compensated by a constant. For the square root, the reciprocal square
    root and the logarithm the kept partial products S1_i * y_j are: all i for
    j >= j0 + 8, i >= 3 for the 3 rows below that, i >= 6 for the rest, with
    j0 = 1 for the square root and 0 otherwise (a 17x17 signed multiplier
    plus an 8x9 and a 3x3 one, as in the synthesis report). The exponential
    leaves out S1_i * y_j for i < 2, j < 12; 2 <= i < 7, j < 7 and
    7 <= i < 11, j < 3.

The results do not depend on the latency or the target frequency, only on the
device family. The full-multiplier datapath was solved first, on a MAX 10
build of the IP generated with full multipliers (an option of Intel's
generator), then the truncated one, and both were confirmed on the other
families.

How the structure was inferred:

  * The RAM sizes in the synthesis report give the table sizes and the widths
    of C0, C1 and C2; the report also gives the sizes of the multipliers.
  * For a fixed structure, the output on all inputs of a segment pins down its
    coefficients: every output gives an interval for A, which gives linear
    constraints on (C2, C1, C0). A grid search over the formats (the LSBs of
    the coefficients, the bits of y used for T1, the LSB of S1) left exactly
    one format with a feasible, unique set of coefficients for every segment.
  * With the full multipliers solved, the difference between the outputs of
    the two datapaths for the same coefficients gave the rounding of T1 (a
    sawtooth in C2 * y' mod the LSB of T1 when rounded down) and the truncated
    multiplier: a fit over the partial products S1_i * y_j gave a crisp 0/1
    pattern, refined by coordinate descent over per-row cuts, and the
    constant was scanned.
  * The coefficients the fits produced were then reproduced exactly by the
    lattice method described in
    "Clash.Cores.Intel.Floating.Internal.Coefficients", which the models now
    use.

Square root (exponent e = E - 127, p = e mod 2, x = m * 2^e with m in
[1, 2)): the result is sqrt (m * 2^p) * 2^floor(e/2). The table index is
p * 128 + the top 7 fraction bits, y the low 16 fraction bits. The
significand O comes out in units of 2^-23 in [1, 2]; O = 2 (from rounding)
carries into the exponent, which the model gets by adding O - 1 to the result's
exponent field rather than or-ing. C0 gets a rounding constant of 4 units: a
quarter unit in the last place for p = 0 and a half for p = 1. Zero and
subnormal inputs give a zero with the same sign; negative numbers (including
-inf) and NaN give NaN; +inf gives +inf.

Reciprocal square root: index p * 256 + top 8 fraction bits (p = 1 for an even
biased exponent E), y the low 15 fraction bits; the result is
f(m) / 2 * 2^(-(E - 127 - p) / 2) with f(m) = 2 / sqrt (m * 2^p) in (1, 2], so
its biased exponent is floor ((380 - E) / 2), plus the carry of O = 2.
Rounding constant: 8 units of 2^-27 for p = 0, 4 for p = 1. Zero and
subnormal inputs give an infinity of the same sign; negative numbers and NaN
give NaN; +inf gives +0.

Exponential, following de Dinechin and Pasca ("Floating-point exponential
functions for DSP-enabled FPGAs", FPT 2010):

  1. X = floor (x * 2^24) in two's complement; zero and subnormal inputs keep
     their hidden bit (so e^-0 is 1 - 2^-24, 0x3F7FFFFF). |x| >= 512 overflows
     (+inf) or underflows (+0).
  2. k = floor (x / ln 2), estimated from the significand times 1/ln 2
     (floor (2^34 / ln 2) / 2^34), truncated to 27 bits relative to the
     significand. For negative x just below k * ln 2, the estimate can be one
     too high; then r below is negative and is clamped to 0.
  3. r = x - k * ln 2 in units of 2^-26, with k * ln 2 computed from
     ln 2 rounded down to 34 fraction bits, rounded down to 33 fraction bits
     and then up to 26.
  4. e^r with an 89-entry table indexed by the top 7 bits of r (r * 128 is
     below 89), y the 19 bits below them; the result's significand is
     truncated to 2^-23, with a rounding constant of 2^-24 in C0.
  5. A significand of 2 or more is normalised; a biased exponent of 255 or
     more gives +inf, of 0 or less +0 (no subnormal results). NaN gives NaN,
     +inf gives +inf and -inf gives +0.

Natural logarithm: for x = m * 2^E in [0.5, 2) (E is -1 or 0) the IP
computes ln x = z * P(x) with z = x - 1 exact and P approximating
ln(x) / (x - 1), with four regions of 128 segments each (see 'logCoefficients').
P is truncated to 24 bits (Aq). Elsewhere it computes E * ln 2 + z * Aq, with
ln 2 rounded to 21 fraction bits and z = m - 1 (the tables of [1, 2)). The
result is computed exactly and rounded once to nearest, ties away from zero.
C0 gets a rounding constant of 8 units (2^-27 for P < 1, 2^-26 for P >= 1).
Zero and subnormal inputs give -inf; negative numbers, -inf and NaN give NaN;
+inf gives +inf and 1 gives +0.

The reciprocal is correctly rounded (round to nearest, ties to even) and
flushes subnormal inputs to zero, with a quirk at the bottom of the exponent
range: the result is computed with an unbounded exponent, and a result whose
biased exponent would be 0 keeps its 23 fraction bits with an exponent field
of 0. Note that this is not the subnormal number the bit pattern would encode:
the result has the significand of a normal number. Results below that flush to
a zero of the same sign. 1/+-0 is +-inf, 1/+-inf is +-0 and NaN gives NaN.

The IP generates the NaN 0x7F800001.
-}

-- | Which datapath the IP uses for its polynomials, determined by the device
-- family, see Note [Bit-exact models]
data Multipliers
  = TruncatedMultipliers
  -- ^ MAX 10, Cyclone IV E and Cyclone 10 LP (18x18 multipliers)
  | FullMultipliers
  -- ^ Cyclone V (27x27 multipliers)
  deriving (Eq, Show, Enum, Bounded)

-- | The NaN the IP generates
nanSingle :: Word32
nanSingle = 0x7F800001

-- | Positive infinity
infSingle :: Word32
infSingle = 0x7F800000

-- | A coefficient table as three arrays: C2, C1, C0
data Table = Table !(UArray Int Int64) !(UArray Int Int64) !(UArray Int Int64)

mkTable :: [(Int, Int, Int)] -> Table
mkTable cs =
  Table (arr [c | (c, _, _) <- cs]) (arr [c | (_, c, _) <- cs]) (arr [c | (_, _, c) <- cs])
 where
  arr xs = listArray (0, length cs - 1) (map fromIntegral xs)

-- | Look up the coefficients of a segment. Callers guarantee that the index is
-- in range.
coefs :: Table -> Int -> (Int64, Int64, Int64)
coefs (Table c2 c1 c0) i = (unsafeAt c2 i, unsafeAt c1 i, unsafeAt c0 i)
{-# INLINE coefs #-}

sqrtTable, rsqrtTable, expTable, logTable :: Table
sqrtTable = mkTable sqrtCoefficients
{-# NOINLINE sqrtTable #-}
rsqrtTable = mkTable rsqrtCoefficients
{-# NOINLINE rsqrtTable #-}
expTable = mkTable expCoefficients
{-# NOINLINE expTable #-}
logTable = mkTable logCoefficients
{-# NOINLINE logTable #-}

-- | Fields of a single precision number: sign bit, biased exponent, fraction
fields :: Word32 -> (Bool, Int, Int64)
fields w =
  (testBit w 31, fromIntegral ((w `shiftR` 23) .&. 0xFF), fromIntegral (w .&. 0x7FFFFF))
{-# INLINE fields #-}

-- | The truncated multiplier of the square root, the reciprocal square root
-- and the logarithm: S1 * y without the partial products S1_i * y_j with
-- (i < 3, j < j0 + 8) or (i < 6, j < j0 + 5), where y has j0 + 15 bits and the
-- low j0 bits of y are ignored by the two small tiles
staircase :: Int -> Int64 -> Int64 -> Int64
staircase j0 s1 y =
    ((s1 `shiftR` 6) * y) `shiftL` 6
  + ((s1 .&. 63) * (y `shiftR` (j0 + 8))) `shiftL` (j0 + 8)
  + (((s1 `shiftR` 3) .&. 7) * ((y `shiftR` (j0 + 5)) .&. 7)) `shiftL` (j0 + 8)
{-# INLINE staircase #-}

-- * Square root

-- | The IP's square root in single precision, on bit patterns
sqrtSingle :: Multipliers -> Word32 -> Word32
sqrtSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else w
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 31
  | neg = nanSingle
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise = (fromIntegral (k + 127) `shiftL` 23) + fromIntegral (o - bit 23)
 where
  (neg, e, f) = fields w
  k = (e - 127) `shiftR` 1
  p = (e - 127) .&. 1
  o = sqrtSig mults p f

-- | Significand of sqrt (m * 2^p) in units of 2^-23 (2^23 to 2^24), for the
-- fraction @f@ of m. The coefficients have their LSBs at 2^-(13 - p), 2^-(20 -
-- p) and 2^-(27 - p); S1 at 2^-21; A at 2^-44.
sqrtSig :: Multipliers -> Int -> Int64 -> Int64
sqrtSig mults p f = a `shiftR` 21
 where
  (c2, c1, c0) = coefs sqrtTable (p * 128 + fromIntegral (f `shiftR` 16))
  y = f .&. 0xFFFF
  -- C2 times the top 12 bits of y, to 2^-21
  t1 = case mults of
    FullMultipliers -> (c2 * (y `shiftR` 4)) `shiftR` (11 - p)
    TruncatedMultipliers -> (c2 * (y `shiftR` 4) + bit (10 - p)) `shiftR` (11 - p)
  s1 = (c1 `shiftL` (1 + p)) + t1
  -- C0 plus the rounding constant of 4 LSBs, at 2^-44
  a0 = (c0 + 4) `shiftL` (17 + p)
  a = case mults of
    FullMultipliers -> a0 + s1 * y
    -- Compensation constant 2^-30
    TruncatedMultipliers -> a0 + staircase 1 s1 y + bit 14

-- * Reciprocal square root

-- | The IP's reciprocal square root in single precision, on bit patterns
rsqrtSingle :: Multipliers -> Word32 -> Word32
rsqrtSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else 0
  -- Zeros and subnormal numbers: an infinity of the same sign
  | e == 0 = (w .&. bit 31) .|. infSingle
  | neg = nanSingle
  -- The carry of a significand of 2 propagates into the exponent
  | otherwise =
      (fromIntegral ((380 - e) `shiftR` 1) `shiftL` 23) + fromIntegral (o - bit 23)
 where
  (neg, e, f) = fields w
  o = rsqrtSig mults (if even e then 1 else 0) f

-- | Significand of 2 / sqrt (m * 2^p) in units of 2^-23 (2^23 to 2^24), for
-- the fraction @f@ of m. Coefficients at 2^-11, 2^-19, 2^-27; S1 at 2^-20.
rsqrtSig :: Multipliers -> Int -> Int64 -> Int64
rsqrtSig mults p f = case mults of
  FullMultipliers ->
    let t1 = (c2 * (y `shiftR` 3)) `shiftR` 11
        s1 = 2 * c1 + t1
    in  (c0' + ((s1 * y) `shiftR` 16)) `shiftR` 4
  TruncatedMultipliers ->
    let t1 = (c2 * (y `shiftR` 3) + bit 10) `shiftR` 11
        s1 = 2 * c1 + t1
    -- At 2^-37, with the compensation constant 2^-30
    in  ((c0' `shiftL` 10) + bit 7 + (staircase 0 s1 y `shiftR` 6)) `shiftR` 14
 where
  (c2, c1, c0) = coefs rsqrtTable (p * 256 + fromIntegral (f `shiftR` 15))
  y = f .&. 0x7FFF
  -- Rounding constant: half an output LSB for p = 0, a quarter for p = 1
  c0' = c0 + (if p == 1 then 4 else 8)

-- * Reciprocal

-- | The IP's reciprocal in single precision, on bit patterns
recipSingle :: Word32 -> Word32
recipSingle w
  | e == 255 = if f /= 0 then nanSingle else s
  -- Zeros and subnormal numbers: an infinity of the same sign
  | e == 0 = s .|. infSingle
  | h < -127 = s
  | h > 127 = s .|. infSingle
  -- h = -127 gives an exponent field of 0 with the fraction bits of q
  | otherwise = s .|. (fromIntegral (h + 127) `shiftL` 23) .|. fromIntegral (q - bit 23)
 where
  (_, e, f) = fields w
  s = w .&. bit 31
  m = f .|. bit 23
  (q0, r) = bit 47 `quotRem` m
  -- 2^47 / m, rounded to nearest, ties to even
  q1 | 2 * r > m || (2 * r == m && odd q0) = q0 + 1
     | otherwise = q0
  -- The result is q * 2^(h - 23); only m = 2^23 gives q1 = 2^24
  (h, q) | q1 >= bit 24 = (127 - e, q1 `shiftR` 1)
         | otherwise = (126 - e, q1)

-- * Exponential

-- | The IP's exponential in single precision, on bit patterns
expSingle :: Multipliers -> Word32 -> Word32
expSingle mults w
  | e == 255 = if f /= 0 then nanSingle else if neg then 0 else infSingle
  -- |x| >= 512
  | sh > 8 = if neg then 0 else infSingle
  | be >= 255 = infSingle
  | be <= 0 = 0
  | otherwise = (fromIntegral be `shiftL` 23) .|. fromIntegral (o .&. 0x7FFFFF)
 where
  (neg, e, f) = fields w
  -- The hidden bit is set even for zeros and subnormal numbers
  m = f .|. bit 23
  -- |x| = m * 2^sh in units of 2^-24
  sh = e - 126
  -- |x| at 2^-24, rounded down for positive x and up for negative x (below
  -- 2^-24 the significand is shifted out completely)
  xAbs | sh >= 0 = m `shiftL` sh
       | otherwise =
           let d = min 24 (negate sh)
           in  (m `shiftR` d) + (if neg && m .&. (bit d - 1) /= 0 then 1 else 0)
  -- x = floor (x * 2^24)
  x = if neg then negate xAbs else xAbs
  -- m / ln 2 to 27 fraction bits (relative to m), with 1/ln 2 rounded down to
  -- 34 fraction bits
  q = (if neg then negate else id) ((m * 24785312074) `shiftR` 30)
  -- k = floor (x / ln 2), estimated
  k = q `shiftR` min 63 (154 - e)
  -- k * ln 2 with ln 2 rounded down to 34 fraction bits, rounded down to 33
  -- fraction bits and then up to 26
  kLn2 = negate (negate ((k * 11908177887) `shiftR` 1) `shiftR` 7)
  -- r = x - k * ln 2 in units of 2^-26; when k is one too high, r = 0
  r = max 0 (4 * x - kLn2)
  (c2, c1, c0) = coefs expTable (fromIntegral (r `shiftR` 19))
  -- The low 19 bits of r, at 2^-19 of a segment (weights of 2^-27 units)
  y = r .&. (bit 19 - 1)
  t1 = case mults of
    FullMultipliers -> (c2 * (y `shiftR` 5)) `shiftR` 14
    TruncatedMultipliers -> (c2 * (y `shiftR` 5) + bit 13) `shiftR` 14
  s1 = c1 + t1
  -- C0 with its rounding constant of 2^-24, at 2^-54
  a0 = (c0 + 8) `shiftL` 27
  a = case mults of
    FullMultipliers -> a0 + (s1 * y) `shiftL` 8
    TruncatedMultipliers ->
      let drop' = (s1 .&. 0x3) * (y .&. 0xFFF)
                + (s1 .&. 0x7C) * (y .&. 0x7F)
                + (s1 .&. 0x780) * (y .&. 0x7)
      -- Compensation constant 2^-29
      in  a0 + (s1 * y - drop' + bit 17) `shiftL` 8
  -- The significand in units of 2^-23: [1, 4)
  o0 = a `shiftR` 31
  (o, be) | o0 >= bit 24 = (o0 `shiftR` 1, fromIntegral k + 128 :: Int)
          | otherwise = (o0, fromIntegral k + 127)

-- * Natural logarithm

-- | The IP's natural logarithm in single precision, on bit patterns
logSingle :: Multipliers -> Word32 -> Word32
logSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers
  | e == 0 = 0xFF800000
  | neg = nanSingle
  | e == 127 && f == 0 = 0
  | otherwise = roundLog s
 where
  (neg, e, f) = fields w
  j = fromIntegral ((f `shiftR` 15) .&. 127)
  hi = testBit f 22
  y = f .&. 0x7FFF
  -- The result at 2^-47
  s | e == 126 =
        -- x in [0.5, 1): z = x - 1 at 2^-24, P at 2^-23
        (f - bit 23) * logPoly mults ((if hi then 384 else 256) + j) y
    | otherwise =
        -- z at 2^-23, P at 2^-24, (e - 127) * ln 2 with ln 2 rounded to 21
        -- fraction bits
        f * logPoly mults ((if hi then 128 else 0) + j) y
          + (fromIntegral (e - 127) * 1453635) `shiftL` 26

-- | P = ln (1 + z) / z truncated to 24 bits (units of 2^-24 or 2^-23)
logPoly :: Multipliers -> Int -> Int64 -> Int64
logPoly mults i y = (c0 + 8 + t2) `shiftR` 6
 where
  (c2, c1, c0) = coefs logTable i
  t2 = case mults of
    FullMultipliers ->
      let s1 = c1 + ((c2 * (y `shiftR` 2)) `shiftR` 13)
      in  (s1 * y) `shiftR` 15
    TruncatedMultipliers ->
      let s1 = c1 + ((c2 * (y `shiftR` 2) + bit 12) `shiftR` 13)
      -- Compensation constant 2^13 (2^-32 for the regions with P < 1)
      in  (staircase 0 s1 y + bit 13) `shiftR` 15

-- | Round a non-zero value in units of 2^-47 to single precision: to nearest,
-- ties away from zero
roundLog :: Int64 -> Word32
roundLog s =
      (if s < 0 then bit 31 else 0)
  .|. (fromIntegral be `shiftL` 23)
  .|. fromIntegral (o .&. 0x7FFFFF)
 where
  a = abs s
  -- The number of bits to drop
  d = 64 - countLeadingZeros a - 24
  o0 | d <= 0 = a `shiftL` negate d
     | otherwise = (a + bit (d - 1)) `shiftR` d
  -- The biased exponent: 2^-47 * 2^d * 2^23 * 2^127
  (o, be) | o0 >= bit 24 = (o0 `shiftR` 1, d + 104)
          | otherwise = (o0, d + 103)
