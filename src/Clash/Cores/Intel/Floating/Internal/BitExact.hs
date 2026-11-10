{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Bit-exact models of the square root, reciprocal square root, reciprocal,
exponentials and logarithms (natural, base 2, base 10 and @log (1 + x)@) of
Intel's Floating Point Functions IP (@altera_fp_functions@) in single
precision, see Note [Bit-exact models].
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal.BitExact
  ( Multipliers(..)
  , sqrtSingle
  , rsqrtSingle
  , recipSingle
  , expSingle
  , exp2Single
  , exp10Single
  , logSingle
  , log2Single
  , log10Single
  , log1pSingle
  ) where

import Prelude

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
import Data.Int (Int64)
import Data.Word (Word32)

import Clash.Cores.Intel.Floating.Internal.Coefficients
  ( exp10Coefficients, exp2Coefficients, expCoefficients, log10Coefficients
  , log2Coefficients, logCoefficients, rsqrtCoefficients, sqrtCoefficients )

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

The square root, the reciprocal square root, the exponentials and the
logarithms use a piecewise polynomial of degree two, evaluated in Horner form:

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
    root, the base-2 exponential and the natural and base 10 logarithms the
    kept partial products S1_i * y_j are: all i for j >= j0 + 8, i >= 3 for
    the 3 rows below that, i >= 6 for the rest, with j0 = 1 for the square
    root and the base-2 exponential and 0 otherwise (a 17x17 signed
    multiplier plus an 8x9 and a 3x3 one, as in the synthesis report). The base 2 logarithm keeps all i for j >= 8, i >= 3 for j = 6
    and 7 and i >= 5 for the rest (17x17, 8x9 and 2x2). The exponential
    leaves out S1_i * y_j for i < 2, j < 12; 2 <= i < 7, j < 7 and
    7 <= i < 11, j < 3, the base-10 exponential for i < 2, j < 11;
    2 <= i < 6, j < 7 and 6 <= i < 11, j < 2.

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

Base-2 exponential: the exponential's structure without the range reduction.

  1. X = floor (x * 2^23) in two's complement, with the hidden bit kept for
     zeros and subnormal numbers (so 2^-0 is 0x3F7FFFFF too). |x| >= 512
     overflows (+inf) or underflows (+0).
  2. k = X >> 23 is the result's exponent; 2^f for the 23 fraction bits f of
     X uses a 128-entry table indexed by the top 7 bits of f, y the 16 bits
     below them, with the multipliers of the square root (T1 from the top 12
     bits of y; on 18x18 families the same staircase with j0 = 1, compensation
     constant 2^-29). The result's significand is truncated to 2^-23 with a
     rounding constant of 2^-24 in C0.
  3. Overflow, underflow and special inputs as for the exponential.

Base-10 exponential: 10^x = 2^k * 10^r with r = x - k * log10 2.

  1. X = floor (x * 2^24) as for the exponential.
  2. k = floor (x * log2 10), estimated from the significand times log2 10
     rounded to nearest at 26 fraction bits, truncated to 26 bits relative to
     the significand. The estimate can be one too high for inputs (of either
     sign) just below k * log10 2; r is then clamped to 0.
  3. r = x - k * log10 2 in units of 2^-27, with k * log10 2 computed from
     log10 2 rounded to nearest at 34 fraction bits, rounded up to 27.
  4. 10^r with a 155-entry table (in a 256-entry memory) indexed by the top 8
     bits of r (r * 512 is below 155), y the 18 bits below them. T1 is C2
     times the top 13 bits of y at 2^-27; on 18x18 families the truncated
     multiplier leaves out S1_i * y_j for i < 2, j < 11; 2 <= i < 6, j < 7 and
     6 <= i < 11, j < 2 (a 17x17, an 8x9, a 9x8 and two 4x4 multipliers, as in
     the synthesis report), compensated by 2^-29. The rounding constant in C0
     is a whole unit in the last place, 2^-23, so 10^0 is 1 + 2^-23
     (0x3F800001) and 10^-0 is 0x3F7FFFFE.
  5. Overflow, underflow and special inputs as for the exponential.

The structure of the base-10 exponential was found as follows: the outputs
depend on floor (x * 2^24) only (aligned groups of inputs give equal outputs);
the mean error is periodic in x - k * log10 2 with a period of 1/512; on the
inputs with k = 0 the grid search found T1's input width and LSB, with
unique coefficients for all 155 segments, identical to the ones generated
from first principles; per k, exactly one value of k * log10 2 at 2^-27
made all outputs match, which pins log10 2 down to a small interval (rounding
it up to 33 to 38 fraction bits gives the same results); the inputs where the
estimate of k is one too high select the estimator among the ones giving the
same results on all inputs; and the truncated multiplier was fitted by
coordinate descent over per-row cuts of the partial products, starting from
the exponential's.

Natural logarithm: for x = m * 2^E in [0.5, 2) (E is -1 or 0) the IP
computes ln x = z * P(x) with z = x - 1 exact and P approximating
ln(x) / (x - 1), with four regions of 128 segments each (see 'logCoefficients').
P is truncated to 24 bits (Aq). Elsewhere it computes E * ln 2 + z * Aq, with
ln 2 rounded to 21 fraction bits and z = m - 1 (the tables of [1, 2)). The
result is computed exactly and rounded once to nearest, ties away from zero.
C0 gets a rounding constant of 8 units (2^-27 for P < 1, 2^-26 for P >= 1).
Zero and subnormal inputs give -inf; negative numbers, -inf and NaN give NaN;
+inf gives +inf and 1 gives +0.

The base 2 and base 10 logarithms have the same structure as the natural
logarithm (the same regions and segments, and their own tables of
log_b(1 + z) / z, see 'log2Coefficients' and 'log10Coefficients'), with
these differences, found by the same format search on all inputs in
[0.5, 2) and by checking the constants on all inputs in [0.25, 0.5) and
[2, 4):

  * Base 2: P is in [1, 2] in all four regions. The coefficients are on the
    grid 2^-27 (one bit coarser than the natural logarithm's for P < 1, as the
    narrower tables in the synthesis report suggest), T1 uses the top 12 bits
    of y, the rounding constant is 2^-25 and P is truncated to 2^-24 (25 bits).
    Outside [0.5, 1) the result is E + z * P, computed exactly: powers of two
    give exact results.
  * Base 10: P is in [0.30, 0.61], the coefficients are on the grid 2^-29 with
    a rounding constant of 2^-27. For x in [0.5, 1) P is truncated to 2^-25;
    elsewhere (P < 0.5) the polynomial is evaluated one bit finer (as if the
    coefficients were doubled), and P truncated to 2^-26. Elsewhere the
    result is E * log10 2 + z * P, with log10 2 rounded to 30 fraction bits.

In both, the sum is computed exactly and rounded once to nearest, ties away
from zero, and the special cases are those of the natural logarithm.

ln (1 + x) uses the tables of the natural logarithm, with P truncated to 25
bits instead of 24 (a rounding constant of 8 units, as there). With
v = 1 + x:

  * For v in [0.5, 2) (x in (-0.5, 1)) the result is x * P(v), with x exact:
    the segment and y come from v rounded down to 2^-23 (v >= 1) or 2^-24
    (v < 1), the regions of the natural logarithm of v. So there is no
    cancellation for small x, and for |x| < 2^-24 the result is x.
  * Otherwise v is truncated to its 24 leading bits, v = (1 + g * 2^-23) * 2^E,
    and the result is E * ln 2 + g * 2^-23 * P(v) as for the natural
    logarithm.

The result is rounded once to nearest, ties away from zero. Zero and subnormal
inputs give a zero of the same sign, -1 gives -inf, x < -1, -inf and NaN give
NaN and +inf gives +inf. This was found by testing the hypothesis "the natural
logarithm's datapath applied to 1 + x" against the IP, and varying where P is
truncated and how v is rounded: only this combination matched.

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

sqrtTable, rsqrtTable, expTable, exp2Table, exp10Table, logTable, log2Table, log10Table :: Table
sqrtTable = mkTable sqrtCoefficients
{-# NOINLINE sqrtTable #-}
rsqrtTable = mkTable rsqrtCoefficients
{-# NOINLINE rsqrtTable #-}
expTable = mkTable expCoefficients
{-# NOINLINE expTable #-}
exp2Table = mkTable exp2Coefficients
{-# NOINLINE exp2Table #-}
exp10Table = mkTable exp10Coefficients
{-# NOINLINE exp10Table #-}
logTable = mkTable logCoefficients
{-# NOINLINE logTable #-}
log2Table = mkTable log2Coefficients
{-# NOINLINE log2Table #-}
log10Table = mkTable log10Coefficients
{-# NOINLINE log10Table #-}

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

-- * Exponential, base 2

-- | The IP's base-2 exponential in single precision, on bit patterns
exp2Single :: Multipliers -> Word32 -> Word32
exp2Single mults w
  | e == 255 = if f /= 0 then nanSingle else if neg then 0 else infSingle
  -- |x| >= 512
  | sh > 8 = if neg then 0 else infSingle
  | otherwise = expResult k (a `shiftR` 20)
 where
  (neg, e, f) = fields w
  -- The hidden bit is set even for zeros and subnormal numbers
  m = f .|. bit 23
  -- |x| = m * 2^sh in units of 2^-23
  sh = e - 127
  -- x = floor (x * 2^23) = k * 2^23 + the fraction bits of x
  x = expFixed neg sh m
  k = x `shiftR` 23
  (c2, c1, c0) = coefs exp2Table (fromIntegral ((x `shiftR` 16) .&. 127))
  -- The low 16 fraction bits
  y = x .&. 0xFFFF
  -- C2 times the top 12 bits of y, to 2^-27
  t1 = case mults of
    FullMultipliers -> (c2 * (y `shiftR` 4)) `shiftR` 12
    TruncatedMultipliers -> (c2 * (y `shiftR` 4) + bit 11) `shiftR` 12
  s1 = c1 + t1
  -- C0 with its rounding constant of 2^-24, at 2^-43
  a0 = (c0 + 8) `shiftL` 16
  a = case mults of
    FullMultipliers -> a0 + s1 * y
    -- Compensation constant 2^-29
    TruncatedMultipliers -> a0 + staircase 1 s1 y + bit 14

-- * Exponential, base 10

-- | The IP's base-10 exponential in single precision, on bit patterns
exp10Single :: Multipliers -> Word32 -> Word32
exp10Single mults w
  | e == 255 = if f /= 0 then nanSingle else if neg then 0 else infSingle
  -- |x| >= 256
  | sh > 8 = if neg then 0 else infSingle
  | otherwise = expResult k (a `shiftR` 22)
 where
  (neg, e, f) = fields w
  -- The hidden bit is set even for zeros and subnormal numbers
  m = f .|. bit 23
  -- |x| = m * 2^sh in units of 2^-24
  sh = e - 126
  -- x = floor (x * 2^24)
  x = expFixed neg sh m
  -- m * log2 10 to 26 fraction bits (relative to m), with log2 10 rounded to
  -- nearest at 26 fraction bits
  q = (if neg then negate else id) ((m * 222930821) `shiftR` 23)
  -- k = floor (x * log2 10), estimated
  k = q `shiftR` min 63 (153 - e)
  -- k * log10 2 with log10 2 rounded to nearest at 34 fraction bits, rounded
  -- up to 27 fraction bits
  kLog = negate (negate (k * 5171655946) `shiftR` 7)
  -- r = x - k * log10 2 in units of 2^-27; when k is one too high, r = 0
  r = max 0 (8 * x - kLog)
  (c2, c1, c0) = coefs exp10Table (fromIntegral (r `shiftR` 18))
  -- The low 18 bits of r
  y = r .&. (bit 18 - 1)
  -- C2 times the top 13 bits of y, to 2^-27
  t1 = case mults of
    FullMultipliers -> (c2 * (y `shiftR` 5)) `shiftR` 13
    TruncatedMultipliers -> (c2 * (y `shiftR` 5) + bit 12) `shiftR` 13
  s1 = c1 + t1
  -- C0 with its rounding constant of 2^-23, at 2^-45
  a0 = (c0 + 16) `shiftL` 18
  a = case mults of
    FullMultipliers -> a0 + s1 * y
    TruncatedMultipliers ->
      let drop' = (s1 .&. 0x3) * (y .&. 0x7FF)
                + (s1 .&. 0x3C) * (y .&. 0x7F)
                + (s1 .&. 0x7C0) * (y .&. 0x3)
      -- Compensation constant 2^-29
      in  a0 + s1 * y - drop' + bit 16

-- | @floor (x * 2^n)@ in two's complement for @|x| = m * 2^(sh - n)@ with
-- @m < 2^24@ (below 2^-n the significand is shifted out completely)
expFixed :: Bool -> Int -> Int64 -> Int64
expFixed neg sh m = if neg then negate xAbs else xAbs
 where
  -- |x| rounded down for positive x and up for negative x
  xAbs | sh >= 0 = m `shiftL` sh
       | otherwise =
           let d = min 24 (negate sh)
           in  (m `shiftR` d) + (if neg && m .&. (bit d - 1) /= 0 then 1 else 0)

-- | The result of 'exp2Single' and 'exp10Single' from the unbiased exponent
-- @k@ and the significand @o0@ in units of 2^-23 (in [1, 4)): normalised,
-- with overflow to +inf and underflow to +0 (no subnormal results)
expResult :: Int64 -> Int64 -> Word32
expResult k o0
  | be >= 255 = infSingle
  | be <= 0 = 0
  | otherwise = (fromIntegral be `shiftL` 23) .|. fromIntegral (o .&. 0x7FFFFF)
 where
  (o, be) | o0 >= bit 24 = (o0 `shiftR` 1, k + 128)
          | otherwise = (o0, k + 127)

-- * Natural logarithm

-- | The IP's natural logarithm in single precision, on bit patterns
logSingle :: Multipliers -> Word32 -> Word32
logSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers
  | e == 0 = 0xFF800000
  | neg = nanSingle
  | e == 127 && f == 0 = 0
  | otherwise = roundAt 47 s
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
logPoly mults i y = (c0 + 8 + lnT2 mults c2 c1 y) `shiftR` 6
 where
  (c2, c1, c0) = coefs logTable i

-- | The part @C2 * y^2 + C1 * y@ of the polynomials of the natural and base
-- 10 logarithms and of @log (1 + x)@, in the units of the table
lnT2 :: Multipliers -> Int64 -> Int64 -> Int64 -> Int64
lnT2 mults c2 c1 y = case mults of
  FullMultipliers ->
    let s1 = c1 + ((c2 * (y `shiftR` 2)) `shiftR` 13)
    in  (s1 * y) `shiftR` 15
  TruncatedMultipliers ->
    let s1 = c1 + ((c2 * (y `shiftR` 2) + bit 12) `shiftR` 13)
    -- Compensation constant 2^13 (2^-32 for the regions with P < 1 of the
    -- natural logarithm)
    in  (staircase 0 s1 y + bit 13) `shiftR` 15

-- | Round a non-zero value in units of 2^-g to single precision: to nearest,
-- ties away from zero. The result must be a normal number.
roundAt :: Int -> Int64 -> Word32
roundAt g s =
      (if s < 0 then bit 31 else 0)
  .|. (fromIntegral be `shiftL` 23)
  .|. fromIntegral (o .&. 0x7FFFFF)
 where
  a = abs s
  -- The number of bits to drop
  d = 64 - countLeadingZeros a - 24
  o0 | d <= 0 = a `shiftL` negate d
     | otherwise = (a + bit (d - 1)) `shiftR` d
  -- The biased exponent: 2^-g * 2^d * 2^23 * 2^127
  (o, be) | o0 >= bit 24 = (o0 `shiftR` 1, d - g + 151)
          | otherwise = (o0, d - g + 150)

-- * Base 2 logarithm

-- | The IP's base 2 logarithm in single precision, on bit patterns
log2Single :: Multipliers -> Word32 -> Word32
log2Single mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers
  | e == 0 = 0xFF800000
  | neg = nanSingle
  | e == 127 && f == 0 = 0
  -- x in [0.5, 1): z = x - 1 at 2^-24, P at 2^-24
  | e == 126 = roundAt 48 ((f - bit 23) * log2Poly mults ((if hi then 384 else 256) + j) y)
  -- z at 2^-23, P at 2^-24, the exponent (e - 127) exactly
  | otherwise =
      roundAt 47
        (f * log2Poly mults ((if hi then 128 else 0) + j) y
          + fromIntegral (e - 127) `shiftL` 47)
 where
  (neg, e, f) = fields w
  j = fromIntegral ((f `shiftR` 15) .&. 127)
  hi = testBit f 22
  y = f .&. 0x7FFF

-- | P = log2 (1 + z) / z, in [1, 2], truncated to 2^-24
log2Poly :: Multipliers -> Int -> Int64 -> Int64
log2Poly mults i y = (c0 + 8 + t2) `shiftR` 4
 where
  (c2, c1, c0) = coefs log2Table i
  t2 = case mults of
    FullMultipliers ->
      let s1 = c1 + ((c2 * (y `shiftR` 3)) `shiftR` 12)
      in  (s1 * y) `shiftR` 15
    TruncatedMultipliers ->
      let s1 = c1 + ((c2 * (y `shiftR` 3) + bit 11) `shiftR` 12)
          -- Leaves out S1_i * y_j for (i < 3, j < 8) and (i < 5, j < 6)
          p = ((s1 `shiftR` 5) * y) `shiftL` 5
            + ((s1 .&. 31) * (y `shiftR` 8)) `shiftL` 8
            + (((s1 `shiftR` 3) .&. 3) * ((y `shiftR` 6) .&. 3)) `shiftL` 9
      -- Compensation constant 2^13 (2^-30)
      in  (p + bit 13) `shiftR` 15

-- * Base 10 logarithm

-- | The IP's base 10 logarithm in single precision, on bit patterns
log10Single :: Multipliers -> Word32 -> Word32
log10Single mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers
  | e == 0 = 0xFF800000
  | neg = nanSingle
  | e == 127 && f == 0 = 0
  -- The result at 2^-49. x in [0.5, 1): z = x - 1 at 2^-24, P at 2^-25
  | e == 126 =
      roundAt 49 ((f - bit 23) * log10Poly mults 0 ((if hi then 384 else 256) + j) y)
  -- z at 2^-23, P at 2^-26, (e - 127) * log10 2 with log10 2 rounded to 30
  -- fraction bits (20201781 * 2^-26)
  | otherwise =
      roundAt 49
        (f * log10Poly mults 1 ((if hi then 128 else 0) + j) y
          + (fromIntegral (e - 127) * 20201781) `shiftL` 23)
 where
  (neg, e, f) = fields w
  j = fromIntegral ((f `shiftR` 15) .&. 127)
  hi = testBit f 22
  y = f .&. 0x7FFF

-- | P = log10 (1 + z) / z truncated to 2^-(25 + k). For k = 1 (x in [1, 2),
-- where P < 0.5), the polynomial is evaluated with the coefficients doubled,
-- one bit finer.
log10Poly :: Multipliers -> Int -> Int -> Int64 -> Int64
log10Poly mults k i y =
  (((c0 + 8) `shiftL` k) + lnT2 mults (c2 `shiftL` k) (c1 `shiftL` k) y) `shiftR` 5
 where
  (c2, c1, c0) = coefs log10Table i

-- * Natural logarithm of 1 + x

-- | The IP's @log (1 + x)@ in single precision, on bit patterns
log1pSingle :: Multipliers -> Word32 -> Word32
log1pSingle mults w
  | e == 255 = if f /= 0 || neg then nanSingle else infSingle
  -- Zeros and subnormal numbers: a zero of the same sign
  | e == 0 = w .&. bit 31
  -- x <= -1
  | neg && e >= 127 = if e == 127 && f == 0 then 0xFF800000 else nanSingle
  -- 1 + x in [0.5, 2): x * P (1 + x), at 2^-(e' + 25), with x = m * 2^-e' at
  -- 2^-e' = 2^(e - 150) and P at 2^-25 or 2^-24
  | small =
      if v >= bit 24
        then
          -- 1 + x in [1, 2), at 2^-23
          let v' = (v - bit 24) `shiftR` 1
          in  roundAt (175 - e) (mx * log1pPoly mults ((if testBit v' 22 then 128 else 0) + jv v') v')
        else
          -- 1 + x in [0.5, 1), at 2^-24
          let v' = v - bit 23
          in  roundAt (174 - e) (mx * log1pPoly mults ((if testBit v' 22 then 384 else 256) + jv v') v')
  -- Otherwise 1 + x = (1 + g * 2^-23) * 2^ex truncated to 24 bits:
  -- ex * ln 2 + g * P at 2^-48, ln 2 rounded to 21 fraction bits
  | otherwise =
      roundAt 48
        (g * log1pPoly mults ((if testBit g 22 then 128 else 0) + jv g) g
          + (fromIntegral ex * 1453635) `shiftL` 27)
 where
  (neg, e, f) = fields w
  m = f .|. bit 23
  mx = if neg then negate m else m
  -- 1 + x in [0.5, 2): x in (-0.5, 1)
  small = if neg then e <= 125 else e <= 126
  -- 1 + x at 2^-24, rounded down (x = +-m * 2^-(sh + 24))
  sh = 126 - e
  v | neg = bit 24 + (negate m `shiftR` sh)
    | otherwise = bit 24 + (m `shiftR` sh)
  jv u = fromIntegral ((u `shiftR` 15) .&. 127)
  -- 1 + x with its 24 leading bits, as 1 + g * 2^-23 times 2^ex
  (ex, g)
    -- 1 is below the last bit of x
    | e > 150 = (e - 127, f)
    | otherwise =
        let -- 1 + x at 2^-(150 - e), x is positive or in (-1, -0.5]
            u = bit (150 - e) + mx
            nb = 64 - countLeadingZeros u
            u' | nb >= 24 = u `shiftR` (nb - 24)
               | otherwise = u `shiftL` (24 - nb)
        in  (nb - 1 - (150 - e), u' - bit 23)

-- | P = ln (1 + z) / z truncated to 25 bits (units of 2^-25 or 2^-24): the
-- 'logPoly' of the natural logarithm with one more bit
log1pPoly :: Multipliers -> Int -> Int64 -> Int64
log1pPoly mults i u = (c0 + 8 + lnT2 mults c2 c1 (u .&. 0x7FFF)) `shiftR` 5
 where
  (c2, c1, c0) = coefs logTable i
