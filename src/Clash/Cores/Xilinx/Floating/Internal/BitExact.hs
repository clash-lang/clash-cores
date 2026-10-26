{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Bit-exact models of the reciprocal, reciprocal square root, exponential and
logarithm of the Xilinx Floating-Point IP, see Note [Bit-exact models].
-}

{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Floating.Internal.BitExact
  ( expSingle
  , recipSingle
  , rsqrtSingle
  , recipDouble
  , rsqrtDouble
  , expDouble
  , expHalf
  , logSingle
  , logHalf
  , logDouble
  , bitLength
  , roundShiftR
  ) where

import Prelude

import Data.Array (Array)
import Data.Ratio ((%))
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits
import Data.Int (Int64)
import Data.Word (Word16, Word32, Word64)
import GHC.Num.Integer (integerLog2)

import Clash.Cores.Xilinx.Floating.Internal.ExpCoefficients (expDoubleCoefficients)

{- Note [Bit-exact models]
The IP's datasheet (PG060) does not document how it computes the reciprocal,
the reciprocal square root, the exponential and the logarithm, only that the
results are accurate to one unit in the last place. The models in this module
give the IP's results bit for bit. They implement published algorithms; their
parameters (table sizes, widths and rounding of the intermediate results) were
found by comparing the results with those of the IP's bit-accurate C model,
treating the C model as a black box. All tables are computed from formulas,
except the polynomial coefficients of the exponential in double precision
("Clash.Cores.Xilinx.Floating.Internal.ExpCoefficients"); the few entries where
the IP's tables deviate from their formula are noted below. The test suite
compares the models with the C model, and the models were compared with it on
every single and half precision input.

The reciprocal and the reciprocal square root use the method of Ercegovac,
Lang, Muller and Tisserand: "Reciprocation, square root, inverse square root,
and some elementary functions using small multipliers", IEEE Transactions on
Computers 49(7), 2000. For a significand Y in [1, 2), whose top k fraction
bits form the integer j:

  1. R = r / 2^(k+1) approximates 1 / Y, with r = floor (2^(2k+1) / (2^k + j))
     limited to k + 1 bits.
  2. A = Y * R - 1, rounded half-up to 4k fraction bits, is small: |A| < 2^-k.
     A2 and A3 are its next two chunks of k bits, A2 signed.
  3. B = f(1 + A) is evaluated as a short series in A2 and A3, with the cube
     reusing the top bits of the square, and rounded half-up once to 4k
     fraction bits. For f(t) = 1/t the coefficients are 1, -1, 1, -1; for
     f(t) = 1/sqrt(t) they are 1, -1/2, 3/8, -5/16.
  4. The result is M * B, rounded half-up to the precision, where M = R for the
     reciprocal and M = sqrt (R / 2^p) rounded to nearest at 4k fraction bits
     for the reciprocal square root (p is the parity of the exponent).

Single precision uses k = 7. Double precision uses k = 15 for the reciprocal
and k = 14 for the reciprocal square root. The single precision table of r has
one entry that differs from the formula: for j = 89 it holds 150 instead of
151. The model copies that.

In double precision, the reciprocal square root does not take M from a table:
it computes M = R * z3, where z3 approximates 1 / sqrt R. It starts from a
table of 512 values z0 = floor (2^16 / sqrt R), one for every 32 consecutive
values of r and taken at the end of their range, and refines that with three
Newton-Raphson steps z' = z/2 * (3 - R * z^2), with specific widths and
roundings ('rsqrtTableDouble'). For an odd exponent, M is that times
sqrt (1/2), rounded.

The exponential uses the method of de Dinechin and Pasca: "Floating-point
exponential functions for DSP-enabled FPGAs", International Conference on
Field-Programmable Technology (FPT) 2010. In single precision:

  1. |x| is converted to fixed point with 26 fraction bits, truncated.
  2. K = round (|x| / ln 2) is computed from |x| truncated to 3 fraction bits,
     with a constant multiplier made of two tables of 32 entries, multiplying by
     1/ln 2 rounded to 13 fraction bits.
  3. Y = x - K * ln 2, with 26 fraction bits and ln 2 rounded to 29 fraction
     bits. For positive x, Y comes out one unit lower.
  4. e^Y = e^A * (1 + Z + (e^Z - Z - 1)), where A are the top 10 bits of Y and
     Z the rest: e^A comes from a table of 1024 entries, e^Z - Z - 1 from a
     table of 64 entries indexed by the top 6 bits of Z. Both tables are
     rounded up to 26 fraction bits, and the product is rounded down.
  5. The result 2^K * e^Y is rounded to nearest, ties to even, from 3 extra
     bits.

Double precision works the same way, with 56 fraction bits and 4 extra bits:

  1. K = round (|x| / ln 2) is computed from |x| truncated to 4 fraction bits,
     with three tables multiplying by 1/ln 2 rounded to 14 fraction bits.
  2. K * ln 2 is computed with ln 2 truncated to 66 fraction bits, in two parts
     (the top bits of K and the 6 bits below them), one of which is rounded.
  3. A are the top 9 bits of Y, and e^A comes from a table of 512 entries,
     rounded up to 56 fraction bits.
  4. e^Z - Z - 1 is approximated by 512 polynomials of degree two, on the top
     38 bits of Z, with specific truncations. Their coefficients are minimax
     approximations generated with Sollya, see
     "Clash.Cores.Xilinx.Floating.Internal.ExpCoefficients". The IP's first
     polynomial deviates from that in one coefficient.

In half precision, the exponential does not reduce by multiples of ln 2. It
splits x into n/2 + lo, with n = floor (2x) and lo in [0, 1/2), with lo
truncated to 13 fraction bits:

  1. e^lo = e^A * (1 + Z), with A the top 6 bits of lo and Z the other 6: e^A
     from a table rounded to 13 fraction bits, and the product rounded half-up
     to 13 fraction bits.
  2. e^(n/2) from a table of 64 entries, each rounded to 14 significant bits.
  3. The product of the two, rounded half-up to 11 significant bits.

The logarithm is described in Note [Logarithm, single precision],
Note [Logarithm, half precision] and Note [Logarithm, double precision].
-}

-- * Reciprocal and reciprocal square root

-- | Table of r for single precision, indexed by j (see
-- Note [Bit-exact models]): floor (2^15 / (128 + j)), limited to 8 bits
recipTable :: UArray Int Int64
recipTable = listArray (0, 127) (map entry [0 .. 127])
 where
  entry :: Int -> Int64
  entry 0 = 255
  -- The IP's table deviates from the formula here
  entry 89 = 150
  entry j = bit 15 `div` (128 + fromIntegral j)

-- | Post-multiplier of the reciprocal square root in single precision, indexed
-- by p * 128 + j: sqrt (r / 2^8 / 2^p), rounded to nearest at 28 fraction bits
rsqrtTable :: UArray Int Int64
rsqrtTable = listArray (0, 255) [entry p j | p <- [0, 1], j <- [0 .. 127]]
 where
  entry :: Int -> Int -> Int64
  entry p j = fromInteger (if n > s * s + s then s + 1 else s)
   where
    -- sqrt (r / 2^8 / 2^p) * 2^28 = sqrt (r * 2^(48 - p))
    n = toInteger (recipTable ! j) `shiftL` (48 - p)
    s = isqrt n

-- | Integer square root, rounded down
isqrt :: Integer -> Integer
isqrt 0 = 0
isqrt n = go n
 where
  go x = let y = (x + n `div` x) `shiftR` 1 in if y >= x then x else go y

-- | Reduction of a single precision significand @m@ (2^23 <= m < 2^24): j, r,
-- A in units of 2^-28, A2 and A3
reduceSingle :: Int64 -> (Int, Int64, Int64, Int64, Int64)
reduceSingle m = (j, r, a, a2, a3)
 where
  j = fromIntegral ((m `shiftR` 16) .&. 127)
  r = recipTable ! j
  -- Y * R - 1 in units of 2^-31, rounded half-up to units of 2^-28
  a = (m * r - bit 31 + 4) `shiftR` 3
  a2 = a `shiftR` 14
  a3 = (a `shiftR` 7) .&. 127

-- | Significand of 1 / Y for a single precision significand (2^23 <= m <
-- 2^24), in units of 2^-24: from 2^23 to 2^24
recipSigSingle :: Int64 -> Int64
recipSigSingle m = (r * b + bit 11) `shiftR` 12
 where
  (_, r, a, a2, a3) = reduceSingle m
  t = (a2 * a2) `shiftR` 7
  -- The terms below 2^-28, in units of 2^-42
  s = 2 * a2 * a3 * 128 - a2 * t * 128
  b = bit 28 - a + a2 * a2 + ((s + bit 13) `shiftR` 14)

-- | Significand of 1 / sqrt (Y * 2^p) for a single precision significand
-- (2^23 <= m < 2^24), in units of 2^-24
rsqrtSigSingle :: Int -> Int64 -> Int64
rsqrtSigSingle p m = (rsqrtTable ! (p * 128 + j) * b + bit 31) `shiftR` 32
 where
  (j, _, a, a2, a3) = reduceSingle m
  t = (a2 * a2) `shiftR` 7
  -- -A/2 + 3/8 A2^2 z^4 + 3/4 A2 A3 z^5 - 5/16 A2 T z^6, with z = 2^-7 and
  -- T = A2^2 rounded down to a multiple of 2^7, in units of 2^-39
  s = negate (a `shiftL` 10) + 3 * a2 * a2 * 256 + 3 * a2 * a3 * 4 - 5 * a2 * t
  b = bit 28 + ((s + bit 10) `shiftR` 11)

-- | A single precision number with sign @neg@ and value @q * 2^(ex - 23)@,
-- where @q@ is a significand from 2^23 to 2^24: flushed to zero below the
-- smallest normal number, infinity beyond the largest finite number
packSingle :: Bool -> Int -> Int64 -> Word32
packSingle neg ex q
  | e < -126 = s
  | e > 127 = s .|. 0x7F800000
  | otherwise = s .|. (fromIntegral (e + 127) `shiftL` 23) .|. fromIntegral f
 where
  (e, f) = if q == bit 24 then (ex + 1, 0) else (ex, q - bit 23)
  s = if neg then bit 31 else 0

-- | The IP's reciprocal in single precision, on bit patterns
recipSingle :: Word32 -> Word32
recipSingle w
  | e == 255 = if f /= 0 then nanSingle else s
  -- Zeros and subnormal numbers
  | e == 0 = s .|. 0x7F800000
  | otherwise = packSingle (s /= 0) (126 - e) (recipSigSingle (fromIntegral f .|. bit 23))
 where
  s = w .&. bit 31
  e = fromIntegral ((w `shiftR` 23) .&. 0xFF) :: Int
  f = w .&. 0x7FFFFF

-- | The IP's reciprocal square root in single precision, on bit patterns
rsqrtSingle :: Word32 -> Word32
rsqrtSingle w
  | e == 255 = if f /= 0 || s /= 0 then nanSingle else 0
  -- Zeros and subnormal numbers, of either sign
  | e == 0 = s .|. 0x7F800000
  | s /= 0 = nanSingle
  | otherwise = packSingle False (-k - 1) (rsqrtSigSingle p (fromIntegral f .|. bit 23))
 where
  s = w .&. bit 31
  e = fromIntegral ((w `shiftR` 23) .&. 0xFF) :: Int
  f = w .&. 0x7FFFFF
  k = (e - 127) `div` 2
  p = e - 127 - 2 * k

-- | The NaN the IP generates in single precision
nanSingle :: Word32
nanSingle = 0x7FC00000

-- | The NaN the IP generates in double precision
nanDouble :: Word64
nanDouble = 0x7FF8000000000000

-- | The NaN the IP generates in half precision
nanHalf :: Word16
nanHalf = 0x7E00

-- | The IP's reciprocal in double precision, on bit patterns
recipDouble :: Word64 -> Word64
recipDouble w
  | e == 2047 = if f /= 0 then nanDouble else s
  -- Zeros and subnormal numbers
  | e == 0 = s .|. 0x7FF0000000000000
  | re < -1022 = s
  | re > 1023 = s .|. 0x7FF0000000000000
  | otherwise = s .|. (fromIntegral (re + 1023) `shiftL` 52) .|. rf
 where
  s = w .&. bit 63
  e = fromIntegral ((w `shiftR` 52) .&. 0x7FF) :: Int
  f = w .&. 0xFFFFFFFFFFFFF
  q = recipSigDouble (toInteger f .|. bit 52)
  -- The result's exponent if its significand is below 1
  ex = 1022 - e
  (re, rf) = if q == bit 53 then (ex + 1, 0) else (ex, fromInteger q - bit 52)

-- | Significand of 1 / Y for a double precision significand (2^52 <= m <
-- 2^53), in units of 2^-53: from 2^52 to 2^53
recipSigDouble :: Integer -> Integer
recipSigDouble m = (r * b + bit 22) `shiftR` 23
 where
  j = (m `shiftR` 37) .&. 0x7FFF
  r = min (bit 31 `div` (bit 15 + j)) (bit 16 - 1)
  -- Y * R - 1 in units of 2^-68, rounded half-up to units of 2^-60
  a = (m * r - bit 68 + bit 7) `shiftR` 8
  a2 = a `shiftR` 30
  a3 = (a `shiftR` 15) .&. 0x7FFF
  t = (a2 * a2) `shiftR` 15
  -- The terms below 2^-60, in units of 2^-90
  s = 2 * a2 * a3 * bit 15 - a2 * t * bit 15
  b = bit 60 - a + a2 * a2 + ((s + bit 29) `shiftR` 30)

-- | The IP's reciprocal square root in double precision, on bit patterns
rsqrtDouble :: Word64 -> Word64
rsqrtDouble w
  | e == 2047 = if f /= 0 || s /= 0 then nanDouble else 0
  -- Zeros and subnormal numbers, of either sign
  | e == 0 = s .|. 0x7FF0000000000000
  | s /= 0 = nanDouble
  -- The result is always a normal number
  | otherwise = (fromIntegral (re + 1023) `shiftL` 52) .|. (fromInteger rq .&. 0xFFFFFFFFFFFFF)
 where
  s = w .&. bit 63
  e = fromIntegral ((w `shiftR` 52) .&. 0x7FF) :: Int
  f = w .&. 0xFFFFFFFFFFFFF
  k = (e - 1023) `div` 2
  p = e - 1023 - 2 * k
  q = rsqrtSigDouble p (toInteger f .|. bit 52)
  (re, rq) = if q >= bit 53 then (-k, q `shiftR` 1) else (-k - 1, q)

-- | Significand of 1 / sqrt (Y * 2^p) for a double precision significand
-- (2^52 <= m < 2^53), in units of 2^-53
rsqrtSigDouble :: Int -> Integer -> Integer
rsqrtSigDouble p m = (toInteger (rsqrtTableDouble ! (p * bit 14 + j)) * b + bit 58) `shiftR` 59
 where
  j = fromInteger ((m `shiftR` 38) .&. 0x3FFF)
  r = rsqrtRDouble (toInteger j)
  -- Y * R - 1 in units of 2^-67, rounded half-up to units of 2^-56
  a = (m * r - bit 67 + bit 10) `shiftR` 11
  a2 = a `shiftR` 28
  a3 = (a `shiftR` 14) .&. 0x3FFF
  t = (a2 * a2) `shiftR` 14
  -- -A/2 + 3/8 A2^2 z^4 + 3/4 A2 A3 z^5 - 5/16 A2 T z^6, with z = 2^-14 and
  -- T = A2^2 rounded down to a multiple of 2^14, in units of 2^-88
  s = negate (a `shiftL` 31) + 3 * a2 * a2 * bit 29 + 3 * a2 * a3 * bit 16 - 5 * a2 * t * bit 14
  -- 1 plus those, rounded half-up once to units of 2^-56
  b = bit 56 + ((s + bit 31) `shiftR` 32)

-- | r of the reciprocal square root in double precision, for j: floor (2^29 /
-- (2^14 + j)), limited to 15 bits
rsqrtRDouble :: Integer -> Integer
rsqrtRDouble j = min (bit 29 `div` (bit 14 + j)) (bit 15 - 1)

-- | M of the reciprocal square root in double precision, indexed by p * 2^14 +
-- j, in units of 2^-56: sqrt (R / 2^p), computed like the IP does, see
-- Note [Bit-exact models]
rsqrtTableDouble :: UArray Int Word64
rsqrtTableDouble =
  listArray (0, 2 * bit 14 - 1)
    (  [fromInteger (mEven (rsqrtRDouble j)) | j <- [0 .. bit 14 - 1]]
    ++ [fromInteger (mOdd (rsqrtRDouble j)) | j <- [0 .. bit 14 - 1]] )
 where
  -- Round x in units of 2^-from to units of 2^-to: down or half-up
  down x from to = x `shiftR` (from - to)
  halfUp x from to = (x + bit (from - to - 1)) `shiftR` (from - to)

  -- One Newton-Raphson step z' = z/2 * (3 - R * z^2) for R = r / 2^15, with z in
  -- units of 2^-fz, z^2 rounded half-up to units of 2^-fs (exact if fs is
  -- 2 * fz), 3 - R * z^2 to units of 2^-fe, and the result rounded with
  -- @rnd@ to units of 2^-fo
  step r fz fs fe fo rnd z = rnd (z * f) (fz + fe + 1) fo
   where
    sq = if fs == 2 * fz then z * z else halfUp (z * z) (2 * fz) fs
    f = halfUp (3 `shiftL` (fs + 15) - r * sq) (fs + 15) fe

  -- floor (2^16 / sqrt R) at the end of the range of 32 values of r around r
  seed r = isqrt (bit 42 `div` (513 + ((r `shiftR` 5) .&. 511)))

  mEven r = halfUp (r * z3) 72 56
   where
    z1 = step r 16 22 22 33 down (seed r)
    z2 = step r 33 66 70 57 down z1
    z3 = step r 57 71 56 57 halfUp z2

  -- sqrt (1/2) rounded to nearest at 56 fraction bits
  invSqrt2 = let n = bit 111; sq = isqrt n in if n > sq * sq + sq then sq + 1 else sq
  mOdd r = halfUp (mEven r * invSqrt2) 112 56

-- * Exponential

-- | The IP's exponential in single precision, on bit patterns
expSingle :: Word32 -> Word32
expSingle w
  | e == 255 = if m /= 0 then nanSingle else if s then 0 else 0x7F800000
  -- Below 2^-26, including zeros and subnormal numbers
  | e < 101 = 0x3F800000
  | e >= 134 = if s then 0 else 0x7F800000
  | r >= 255 `shiftL` 23 = 0x7F800000
  | r < bit 23 = 0
  | otherwise = fromIntegral r
 where
  s = testBit w 31
  e = fromIntegral ((w `shiftR` 23) .&. 0xFF) :: Int
  m = fromIntegral (w .&. 0x7FFFFF) :: Int

  -- |x| in fixed point with 26 fraction bits, truncated
  mm = m .|. bit 23
  sv = e - 101
  fix = if sv >= 23 then mm `shiftL` (sv - 23) else mm `shiftR` (23 - sv)

  -- K = round (|x| / ln 2), from |x| truncated to 3 fraction bits
  xt = (fix `shiftR` 23) .&. 1023
  absK = (kTable0 ! (xt .&. 31) + kTable1 ! (xt `shiftR` 5) + 4) `shiftR` 3
  k = if s then negate absK else absK

  -- Y = x - K * ln 2 and its parts A and Z
  y
    | s = ((absK * ln2 + 5) `shiftR` 3) - fix
    | otherwise = fix - ((absK * ln2 + 10) `shiftR` 3)
  yu = y .&. (bit 26 - 1)
  ea = expATable ! (yu `shiftR` 16)
  z = yu .&. 0xFFFF

  -- e^Y with 26 fraction bits
  i = ea + (((ea `shiftR` 10) * (z + expZTable ! (z `shiftR` 10))) `shiftR` 16)

  -- Round to 24 significant bits, to nearest, ties to even
  (guard, biasedE) = if testBit i 26 then (3, 127 + k) else (2, 126 + k)
  r = (biasedE `shiftL` 23) + roundShiftR guard i - bit 23

-- | ln 2 rounded to nearest at 29 fraction bits
ln2 :: Int
ln2 = 372130559

-- | The tables of the constant multiplier for K: n * (1 / ln 2) rounded to 13
-- fraction bits (11819 / 2^13), rounded to nearest at 3 fraction bits
kTable0, kTable1 :: UArray Int Int
kTable0 = listArray (0, 31) [mulInvLn2 l | l <- [0 .. 31]]
kTable1 = listArray (0, 31) [mulInvLn2 (32 * h) | h <- [0 .. 31]]

mulInvLn2 :: Int -> Int
mulInvLn2 n = (n * 11819 + 4096) `shiftR` 13

-- | e^A, rounded up to 26 fraction bits, for the top 10 bits of Y (a signed
-- multiple of 2^-10)
expATable :: UArray Int Int
expATable = listArray (0, 1023) (map entry [0 .. 1023])
 where
  entry a = fromInteger (expCeil 26 False (if a >= 512 then a - 1024 else a) 1024)

-- | e^Z - Z - 1, rounded up to 26 fraction bits, for the top 6 bits of Z (a
-- multiple of 2^-16)
expZTable :: UArray Int Int
expZTable = listArray (0, 63) [fromInteger (expCeil 26 True z 65536) | z <- [0 .. 63]]

-- | @expCeil n minus p q@: e^(p/q) (or e^(p/q) - p/q - 1 if @minus@) rounded up
-- to @n@ fraction bits. The series is evaluated with @2n + 48@ fraction bits;
-- none of the values is close enough to a multiple of 2^-n for its error to
-- matter, which the function checks.
expCeil :: Int -> Bool -> Int -> Int -> Integer
expCeil n minus p q
  | p == 0 = if minus then 0 else bit n
  | otherwise = agree "expCeil" [ceilShift (v - err), ceilShift (v + err)]
 where
  prec = 2 * n + 48
  -- The terms (p/q)^j / j! in fixed point, from j = 0, or j = 2 if @minus@
  terms = takeWhile (/= 0) [term j | j <- [if minus then 2 else 0 ..]]
  term :: Integer -> Integer
  term j = (toInteger p ^ j * bit prec) `quot` (toInteger q ^ j * product [1 .. j])
  -- Each term is truncated towards zero, so the sum is within the number of
  -- terms (plus two for the terms left out, which are smaller than one unit
  -- and decrease at least geometrically by half) of the exact value
  v = sum terms
  err = toInteger (length terms) + 2
  ceilShift x = negate (negate x `shiftR` (prec - n))

-- | The IP's exponential in double precision, on bit patterns
expDouble :: Word64 -> Word64
expDouble w
  | e == 2047 = if m /= 0 then nanDouble else if s then 0 else 0x7FF0000000000000
  -- Below 2^-56, including zeros and subnormal numbers
  | e < 967 = 0x3FF0000000000000
  -- 1024 or more
  | e >= 1033 = if s then 0 else 0x7FF0000000000000
  | r >= 0x7FF0000000000000 = 0x7FF0000000000000
  | r < bit 52 = 0
  | otherwise = fromInteger r
 where
  s = testBit w 63
  e = fromIntegral ((w `shiftR` 52) .&. 0x7FF) :: Int
  m = w .&. 0xFFFFFFFFFFFFF

  -- |x| in fixed point with 56 fraction bits, truncated
  mm = toInteger m .|. bit 52
  fix = if e >= 1019 then mm `shiftL` (e - 1019) else mm `shiftR` (1019 - e)

  -- K = round (|x| / ln 2), from |x| truncated to 4 fraction bits
  xt = fix `shiftR` 52
  kPart sh d = roundShiftR 14 (d * bit sh * invLn2Double)
  absK =
    (kPart 0 (xt .&. 31) + kPart 5 ((xt `shiftR` 5) .&. 31) + kPart 10 (xt `shiftR` 10) + 8)
      `shiftR` 4
  k = if s then negate absK else absK

  -- Y = x - K * ln 2 and its parts A and Z
  kLn2 = roundShiftR 7 ((absK `shiftR` 6) * 64 * ln2Double) * 128 + (absK .&. 63) * ln2Double
  y | s = ((kLn2 + 576) `shiftR` 10) - fix
    | otherwise = fix - ((kLn2 + 1472) `shiftR` 10)
  yu = y .&. (bit 56 - 1)
  ea = toInteger (expATableDouble ! fromInteger (yu `shiftR` 47))
  z = yu .&. (bit 47 - 1)

  -- e^Z - Z - 1, from the top 38 bits of Z
  zh = z `shiftR` 9
  (c0, c1, c2) = expPolyDouble ! fromInteger (zh `shiftR` 29)
  t = zh .&. (bit 29 - 1)
  s1 = c1 + (c2 * (t `shiftR` 4)) `shiftR` 25
  ez = (c0 * bit 30 + t * s1) `shiftR` 31

  -- e^Y with 56 fraction bits
  i = ea + ((ea `shiftR` 9) * (z + ez)) `shiftR` 47

  -- Round to 53 significant bits, to nearest, ties to even
  (guard, biasedE) = if testBit i 56 then (4, 1023 + k) else (3, 1022 + k)
  r = biasedE `shiftL` 52 + roundShiftR guard i - bit 52

-- | ln 2 truncated to 66 fraction bits
ln2Double :: Integer
ln2Double = ln2Floor 66

-- | 1 / ln 2 rounded to nearest at 14 fraction bits
invLn2Double :: Integer
invLn2Double = (2 * bit (14 + 80) + d) `quot` (2 * d)
 where
  -- Truncating ln 2 to 80 bits cannot change the result at 14 bits
  d = ln2Floor 80

-- | floor (ln 2 * 2^p), from ln 2 = sum (k >= 1) of 1 / (k * 2^k)
ln2Floor :: Int -> Integer
ln2Floor p = agree "ln2Floor" [v `shiftR` g, (v + toInteger (p + g + 4)) `shiftR` g]
 where
  g = 64
  -- Each term is truncated, and the terms left out sum to less than one unit
  v = sum [bit (p + g) `quot` (toInteger k * bit k) | k <- [1 .. p + g + 2 :: Int]]

-- | e^A, rounded up to 56 fraction bits, for the top 9 bits of Y (a signed
-- multiple of 2^-9)
expATableDouble :: UArray Int Word64
expATableDouble = listArray (0, 511) (map entry [0 .. 511])
 where
  entry a = fromInteger (expCeil 56 False (if a >= 256 then a - 512 else a) 512)

-- | The polynomials approximating e^Z - Z - 1, see
-- "Clash.Cores.Xilinx.Floating.Internal.ExpCoefficients"
expPolyDouble :: Array Int (Integer, Integer, Integer)
expPolyDouble = listArray (0, 511) (zipWith fixFirst [0 :: Int ..] expDoubleCoefficients)
 where
  -- The IP's first polynomial has no linear term
  fixFirst 0 (c0, _, c2) = (c0, 0, c2)
  fixFirst _ c = c

-- * Logarithm in single precision

{- Note [Logarithm, single precision]
The IP computes the logarithm with the iterative multiplicative range reduction
of Detrey and de Dinechin, as in FloPoCo's FPLog (J. Detrey, F. de Dinechin,
"A parameterized floating-point logarithm operator for FPGAs", 2005;
F. de Dinechin, "A flexible floating-point logarithm for reconfigurable
computers", 2010). For x = 2^e * m, m = 1.f, with all fixed-point values
having 39 fraction bits unless stated otherwise:

  1. FB is the leading fraction bit. The result is Ep * ln 2 + ln Y0 with
     Ep = e + FB and Y0 = m / 2^FB in [0.75, 1.5).
  2. Range reduction. j = the 6 leading fraction bits of m. The reciprocal
     table gives r = ceil (2^(11+FB) / (64 + j)), so that R = r / 2^5
     approximates 1 / Y0 with 5 fraction bits. Z1 = R * Y0 - 1 is exact with
     29 fraction bits, 0 <= Z1 < 2^-4.
  3. Iteration 1: A1 = Z1's bits 2^-5..2^-8, B1 = the rest. E1 = 2^-8 if A1 >= 8,
     else 2^-9. Z2 = B1 + E1 * (1 + Z1) - A1 * 2^-8 * Z1 with 37 fraction bits,
     E1 * (1 + Z1) truncated to 37 fraction bits. 0 <= Z2 < 2^-7.
  4. Iteration 2: A2 = Z2's bits 2^-8..2^-13, B2 = the rest, E2 = 2^-14.
     Z3 = B2 + E2 * (1 + Z2) - P2, truncated, where P2 = A2 * 2^-13 * Z2' is
     truncated separately and Z2' is Z2 truncated to 28 fraction bits.
     0 <= Z3 < 2^-12.
  5. ln Y0 = L0[j] + L1[A1] + L2[A2] + Z3 - Z3'^2 / 2, where Z3' is Z3 truncated
     to 24 fraction bits and Z3'^2 / 2 is truncated. The tables hold,
     truncated,
       L0[j]  = ln 2 * rho - 2^-8 - 2^-14, with rho = log2 (1/R) rounded to
                nearest at 39 fraction bits (so this table goes through log2)
       L1[A1] = -ln (1 + E1 - A1 * 2^-8) + 2^-8
       L2[A2] = -ln (1 + 2^-14 - A2 * 2^-13) + 2^-14
  6. V = Ep * ln2_29 + ln Y0, with ln2_29 = 372130559 * 2^-29 (ln 2 rounded
     to nearest at 29 fraction bits).
  7. M = |V| + 8 * 2^-39, minus 2^-39 if V < 0, plus 2^-39 if Ep * ln2_29 and
     ln Y0 have opposite signs. (These look like one's-complement negations in
     the sign handling; the model just reproduces the effect.) M is truncated
     to 24 significant bits.

Near 1, |x - 1| < 2^-12, the IP uses z = x - 1 (exact) instead:
A = z - floor (z^2 * 2^44) * 2^-45, truncated to 24 significant bits.

The constant 8 * 2^-39 in step 7 is fitted. The rest was found by comparing
with the C model; the widths agree with Vivado's per-block utilization report
(e.g. 25-bit Z1, 30-bit Z2, 41-bit table adders for 2 + 39 bits).
-}

-- | The reciprocal table of the logarithm in every precision, indexed by the 6
-- leading fraction bits j: r = ceil (2^(11+FB) / (64 + j)), R = r / 2^5, where
-- FB is the leading fraction bit
logRecip :: Num r => Int -> r
logRecip j = fromIntegral ((bit (11 + j `shiftR` 5) + 63 + j) `quot` (64 + j))

-- | ln (n / d) * 2^sc, approximately (every term of the series is truncated),
-- for 1/2 < n/d < 2, from ln t = 2 atanh ((t-1) / (t+1)).
lnScaled :: Integer -> Integer -> Int -> Integer
lnScaled n d sc = 2 * go t 0 0
 where
  one = bit sc :: Integer
  t = ((n - d) * one) `quot` (n + d)
  t2 = (t * t) `quot` one
  go !pw !k !acc
    | pw == 0 = acc
    | otherwise = go ((pw * t2) `quot` one) (k + 1) (acc + pw `quot` (2 * k + 1))

-- | floor (2^p * (ln (n / d) + 2^-cs)), checked against the approximation
-- error
floorLn :: Integer -> Integer -> Int -> Int -> Integer
floorLn n d cs p
  | n == d = bit p `shiftR` cs                       -- ln 1 = 0: exact
  | otherwise = agree "floorLn" [(v - err) `shiftR` g, (v + err) `shiftR` g]
 where
  g = 80
  sc = p + g
  v = lnScaled n d sc + bit (sc - cs)
  err = 64

-- | L0 table, 39 fraction bits, indexed by the 6 leading fraction bits j.
-- L0[j] = floor (ln 2 * rho - 2^-8 - 2^-14) with rho = log2 (1/R), rounded to
-- nearest at 39 fraction bits, where R = r / 2^5.
logL0Single :: UArray Int Int
logL0Single = listArray (0, 63) (map entry [0 .. 63])
 where
  entry j =
    let rho = roundLog2 32 (logRecip j) 39                         -- log2 (32 / r) * 2^39
    in fromInteger (floorMulLn2 rho - bit 31 - bit 25)

-- | round-half-up (2^p * log2 (n / d)), checked
roundLog2 :: Integer -> Integer -> Int -> Integer
roundLog2 n d p
  | n == d = 0
  | otherwise = agree "roundLog2" cands
 where
  sc = p + 100
  a = lnScaled n d sc
  b = ln2Floor sc
  e = 256
  -- floor ((a / b) * 2^p + 1/2) for the extreme values of a and b
  cands = [ (2 * a' * bit p + b') `div` (2 * b') | a' <- [a - e, a + e], b' <- [b - e, b + e] ]

-- | floor (rho * ln 2) for an integer rho, checked
floorMulLn2 :: Integer -> Integer
floorMulLn2 rho
  | rho == 0 = 0
  | otherwise = agree "floorMulLn2" [(rho * b - e) `shiftR` sc, (rho * b + e) `shiftR` sc]
 where
  sc = 100
  b = ln2Floor sc
  e = 256 * (abs rho + 1)

-- | Entry A1 of the table L1 of the logarithm, with p fraction bits:
-- -ln (1 + E1 - A1 * 2^-8) + 2^-8, truncated, where E1 = 2^-8 if A1 >= th,
-- else 2^-9
logL1 :: Int -> Int -> Int -> Integer
logL1 th p a = floorLn 512 (512 + (if a >= th then 2 else 1) - 2 * toInteger a) 8 p

-- | Entry A of the table of a later iteration of the logarithm, with p fraction
-- bits: -ln (1 + 2^-2pp - A * 2^-qq) + 2^-2pp, truncated
logLi :: Int -> Int -> Int -> Int -> Integer
logLi pp qq p a =
  floorLn (bit (2 * pp)) (bit (2 * pp) + 1 - toInteger a * bit (2 * pp - qq)) (2 * pp) p

-- | L1 table, 39 fraction bits, indexed by A1 (4 bits)
logL1Single :: UArray Int Int
logL1Single = listArray (0, 15) (map (fromInteger . logL1 8 39) [0 .. 15])

-- | L2 table, 39 fraction bits, indexed by A2 (6 bits)
logL2Single :: UArray Int Int
logL2Single = listArray (0, 63) (map (fromInteger . logLi 7 13 39) [0 .. 63])

-- | Model of the IP's natural logarithm in single precision, see
-- Note [Logarithm, single precision].
logSingle :: Word32 -> Word32
logSingle w
  | ex == 0xFF = if f /= 0 || s then nanSingle else 0x7F800000
  | ex == 0 = 0xFF800000
  | s = nanSingle
  | w == 0x3F800000 = 0
  | ex == 127 && f < bit 11 = hp (2 * f)
  | ex == 126 && bit 23 - f < bit 12 = hp (f - bit 23)
  | otherwise = packSingleTruncated (v < 0) mag 39
 where
  s = testBit w 31
  ex = fromIntegral ((w `shiftR` 23) .&. 0xFF) :: Int
  f = fromIntegral (w .&. 0x7FFFFF) :: Int

  -- near 1: z = zz * 2^-24, A = z - floor (z^2 * 2^44) * 2^-45
  hp zz =
    let a = zz `shiftL` 21 - (zz * zz) `shiftR` 4
    in packSingleTruncated (a < 0) (abs a) 45

  fb = f `shiftR` 22
  ep = ex - 127 + fb
  j = f `shiftR` 17
  r = logRecip j                                                -- R * 2^5
  z1 = (r * (bit 23 .|. f)) `shiftL` (1 - fb) - bit 29          -- 2^-29
  -- iteration 1
  a1 = z1 `shiftR` 21
  b1 = z1 - a1 `shiftL` 21
  ey1 = (z1 + bit 29) `shiftR` (if a1 >= 8 then 0 else 1)       -- 2^-37
  z2 = b1 `shiftL` 8 + ey1 - a1 * z1                            -- 2^-37
  -- iteration 2
  a2 = z2 `shiftR` 24
  b2 = z2 - a2 `shiftL` 24
  ey2 = (z2 + bit 37) `shiftR` 12                               -- 2^-39
  p2 = (a2 * (z2 `shiftR` 9)) `shiftR` 2                        -- 2^-39
  z3 = b2 `shiftL` 2 + ey2 - p2                                 -- 2^-39
  -- ln (1 + Z3) ~ Z3 - Z3^2 / 2
  sq = z3 `shiftR` 15                                           -- 2^-24
  z2o2 = (sq * sq) `shiftR` 10                                  -- 2^-39
  logY = logL0Single ! j + logL1Single ! a1 + logL2Single ! a2 + z3 - z2o2
  el = ep * ln2 `shiftL` 10                                     -- 2^-39
  v = el + logY
  opposite = (el > 0 && logY < 0) || (el < 0 && logY > 0)
  mag = abs v + (if v < 0 then 7 else 8) + (if opposite then 1 else 0)

-- | Truncate m * 2^-lsb (m > 0) to a single precision number
packSingleTruncated :: Bool -> Int -> Int -> Word32
packSingleTruncated neg m lsb =
  (if neg then bit 31 else 0) .|. fromIntegral ((e + 127) `shiftL` 23) .|. (mant .&. 0x7FFFFF)
 where
  bl = finiteBitSize m - 1 - countLeadingZeros m
  e = bl - lsb
  mant = fromIntegral (if bl >= 23 then m `shiftR` (bl - 23) else m `shiftL` (23 - bl)) :: Word32

-- * Logarithm in half precision

{- Note [Logarithm, half precision]
The IP computes the logarithm in half precision with the same iterative
multiplicative range reduction as in single precision (see
Note [Logarithm, single precision]), with a single iteration. For
x = 2^e * m, m = 1.f, with all fixed-point values having 18 fraction bits
unless stated otherwise:

  1. FB is the leading fraction bit. The result is Ep * ln 2 + ln Y0 with
     Ep = e + FB and Y0 = m / 2^FB in [0.75, 1.5) (11 fraction bits).
  2. Range reduction. j = the 6 leading fraction bits of m. The reciprocal
     table gives r = ceil (2^(11+FB) / (64 + j)), so that R = r / 2^5
     approximates 1 / Y0 with 5 fraction bits (the same table as in single
     precision). Z1 = R * Y0 - 1 is exact with 16 fraction bits,
     0 <= Z1 < 2^-4.
  3. Iteration: A1 = Z1's bits 2^-5..2^-8, B1 = the rest (8 bits). E1 = 2^-8
     if A1 >= 8, else 2^-9. Z2 = B1 + E1 * (1 + Z1) - P, where E1 * (1 + Z1)
     is truncated, and P = A1 * 2^-8 * Z1' is truncated separately, with Z1'
     Z1 truncated to 12 fraction bits. 0 <= Z2 < 2^-7.
  4. ln Y0 = L0[j] + L1[A1] + Z2 - Z2'^2 / 2, where Z2' is Z2 truncated to
     11 fraction bits (4 bits) and Z2'^2 / 2 is truncated. The tables hold
       L0[j]  = -ln R - 2^-8, where -ln R is rounded to nearest at 19
                fraction bits and then truncated to 18 fraction bits, in
                sign-magnitude (so its magnitude is rounded down)
       L1[A1] = -ln (1 + E1 - A1 * 2^-8) + 2^-8, truncated
     with one exception: for r = 39 (j = 42 and 43), L0 is one unit
     (2^-18) larger than this formula gives.
  5. V = Ep * ln2_16 + ln Y0, with ln2_16 = 45426 * 2^-16 (ln 2 rounded to
     nearest at 16 fraction bits). |V| is rounded to 11 significant bits,
     ties away from zero, and the result gets the sign of V.

Steps 4 and 5 are equivalent to the sign handling of single precision (step 7
there, without the constant 8 * 2^-39), with the negative entries of L0 stored
as one's complements.

Near 1, Ep = 0 and |Y0 - 1| < 2^-7, the IP uses z = Y0 - 1 (exact, 11
fraction bits) instead: A = z - floor (z^2 * 2^19) * 2^-20 (20 fraction bits),
rounded like V.

The widths agree with Vivado's per-block utilization report of a synthesis
without DSPs: an 8-bit B1, a 12-bit B1 + E1 * (1 + Z1), an 11-bit Z2, a 4-bit
squarer input, 20-bit table adders (2 + 18 bits), a 20-bit Ep * ln 2 and a
23-bit final sum. The rest was found by comparing with the C model, whose
outputs pin down the datapath and most table entries; the exception in step 4
has no known formula.
-}

-- | L0 table of the logarithm in half precision, 18 fraction bits, indexed by
-- the 6 leading fraction bits j
logL0Half :: UArray Int Int
logL0Half = listArray (0, 63) (map entry [0 .. 63])
 where
  entry j =
    let r = logRecip j                                             -- R * 2^5
        -- |ln R| rounded to nearest at 19 fraction bits
        m19 = if r <= 32 then floorLn 32 r 20 19 else floorLn r 32 20 19
        -- truncated to 18 fraction bits, with the sign of -ln R
        m18 = (if r <= 32 then id else negate) (m19 `shiftR` 1)
        -- The IP's table deviates from the formula here
        quirk = if r == 39 then 1 else 0
    in fromInteger (m18 - bit 10 + quirk)

-- | L1 table of the logarithm in half precision, 18 fraction bits, indexed by
-- A1 (4 bits)
logL1Half :: UArray Int Int
logL1Half = listArray (0, 15) (map (fromInteger . logL1 8 18) [0 .. 15])

-- | ln 2 rounded to nearest at 16 fraction bits
ln2Half :: Int
ln2Half = 45426

-- | Model of the IP's natural logarithm in half precision, on bit patterns,
-- see Note [Logarithm, half precision].
logHalf :: Word16 -> Word16
logHalf w
  | ex == 31 = if f /= 0 || s then nanHalf else 0x7C00
  -- Zeros and subnormal numbers, of either sign
  | ex == 0 = 0xFC00
  | s = nanHalf
  | w == 0x3C00 = 0
  -- near 1: z = zz * 2^-11, A = z - floor (z^2 * 2^19) * 2^-20
  | ep == 0 && abs zz < 16 = packHalfRounded (a < 0) (abs a) 20
  | otherwise = packHalfRounded (v < 0) (abs v) 18
 where
  s = testBit w 15
  ex = fromIntegral ((w `shiftR` 10) .&. 31) :: Int
  f = fromIntegral (w .&. 1023) :: Int

  fb = f `shiftR` 9
  ep = ex - 15 + fb
  y0 = (bit 10 .|. f) `shiftL` (1 - fb)                         -- 2^-11
  zz = y0 - bit 11
  a = zz `shiftL` 9 - (zz * zz) `shiftR` 3                      -- 2^-20

  j = f `shiftR` 4
  r = logRecip j                                                -- R * 2^5
  z1 = r * y0 - bit 16                                          -- 2^-16
  -- iteration
  a1 = z1 `shiftR` 8
  b1 = z1 - a1 `shiftL` 8
  ey = (z1 + bit 16) `shiftR` (if a1 >= 8 then 6 else 7)        -- 2^-18
  p = (a1 * (z1 `shiftR` 4)) `shiftR` 2                         -- 2^-18
  z2 = b1 `shiftL` 2 + ey - p                                   -- 2^-18
  -- ln (1 + Z2) ~ Z2 - Z2^2 / 2
  sq = z2 `shiftR` 7                                            -- 2^-11
  z2o2 = (sq * sq) `shiftR` 5                                   -- 2^-18
  logY = logL0Half ! j + logL1Half ! a1 + z2 - z2o2
  v = ep * ln2Half `shiftL` 2 + logY                            -- 2^-18

-- | Round m * 2^-lsb (m > 0) to a half precision number, to nearest with ties
-- away from zero. The results of the logarithm stay in the normal range.
packHalfRounded :: Bool -> Int -> Int -> Word16
packHalfRounded neg m lsb =
  (if neg then bit 15 else 0) .|. fromIntegral ((e' + 15) `shiftL` 10) .|. (fromIntegral q' .&. 1023)
 where
  bl = finiteBitSize m - 1 - countLeadingZeros m
  e = bl - lsb
  q
    | bl >= 11 = ((m `shiftR` (bl - 11)) + 1) `shiftR` 1
    | otherwise = m `shiftL` (10 - bl)
  (q', e') = if q == bit 11 then (q `shiftR` 1, e + 1) else (q, e)

-- * Logarithm in double precision

{- Note [Logarithm, double precision]
The IP uses the same algorithm as in single precision (see
Note [Logarithm, single precision]), with five iterations of the range
reduction instead of two. For x = 2^e * m, m = 1.f, with all fixed-point values
having 85 fraction bits unless stated otherwise:

  1. FB is the leading fraction bit. The result is Ep * ln 2 + ln Y0 with
     Ep = e + FB and Y0 = m / 2^FB in [0.75, 1.5).
  2. Range reduction. j = the 6 leading fraction bits of m, r = ceil (2^(11+FB)
     / (64 + j)) and R = r / 2^5 approximates 1 / Y0. Z1 = R * Y0 - 1 is exact
     with 58 fraction bits, 0 <= Z1 < 2^-4.
  3. Iteration i = 1 .. 5, with p_i = 4, 7, 12, 17, 22 and q_i = p_i + 6
     (q_1 = 8): A_i = the bits of Z_i from 2^-(p_i+1) to 2^-q_i, B_i = the
     rest, E_i = 2^-2p_i, except E1 = 2^-9 if A1 < 7, and
       Z_{i+1} = B_i + E_i * (1 + Z_i) - A_i * 2^-q_i * Z_i'.
     Iterations 1 and 2 are exact (Z2 has 66 fraction bits, Z3 80). In
     iterations 3 .. 5, Z_i' is Z_i truncated to 85 - p_i - 1 fraction bits
     (72, 67 and 62, so that the product has 90 fraction bits), and the two
     terms E_i * (1 + Z_i) and A_i * 2^-q_i * Z_i' are truncated separately.
     0 <= Z6 < 2^-27.
  4. ln Y0 = L0[j] + L1[A1] + ... + L5[A5] + Z6 - Z6'^2 / 2, where Z6' is Z6
     truncated to 51 fraction bits (its leading 24 bits) and Z6'^2 / 2 is
     truncated. The tables hold, truncated,
       L0[j]    = ln2_85 * log2 (1/R) - (2^-8 + 2^-14 + 2^-24 + 2^-34 + 2^-44),
                  with ln2_85 = ln 2 rounded up at 85 fraction bits (so this
                  table goes through log2, like in single precision)
       L1[A1]   = -ln (1 + E1 - A1 * 2^-8) + 2^-8
       L_i[A_i] = -ln (1 + 2^-2p_i - A_i * 2^-q_i) + 2^-2p_i,  i = 2 .. 5
  5. V = Ep * ln2_62 + ln Y0, with ln2_62 = ln 2 rounded to nearest at 62
     fraction bits.
  6. M = |V|, minus 2^-85 if V < 0, plus 2^-85 if Ep * ln2_62 and ln Y0 have
     opposite signs (as in single precision). M is rounded to 53 significant
     bits, to nearest with ties away from zero.

Near 1, |x - 1| < 2^-27, the IP uses z = x - 1 (exact) instead:
A = z - floor (z^2 * 2^102) * 2^-103, rounded to nearest, ties away from zero.

All of this was found by comparing with the C model. Away from 1, the result
depends on the datapath only down to about 2^-64. The low-order details show
for inputs just outside the path near 1 (2^-27 <= |x - 1| < 2^-11, where the
result's unit in the last place goes down to 2^-79) and for inputs that put
|V| within 2^-84 of a rounding boundary; tests need both kinds. With ln 2
truncated instead of rounded up, three of the 22 distinct entries of L0 would
be one unit off.

The widths agree with Vivado's per-block utilization report: 68-, 68-, 63- and
58-bit adders for Z3 .. Z6, a 26-bit squarer input shared with the path near
1, 87-bit table adders, a 73-bit Ep * ln 2 product and a 96-bit final adder
(11 integer bits and 85 fraction bits). The model matched the C model on 14.9
million inputs, including both kinds above.
-}

-- | Tables of the logarithm in double precision, 85 fraction bits, truncated
logTablesDouble :: Array Int (Array Int Integer)
logTablesDouble = listArray (0, 5) [t0, t1, ti 7 13, ti 12 18, ti 17 23, ti 22 28]
 where
  -- ln2Up85 * log2 (1 / R) - sum 2^-2p_i, with sum 2^-2p_i =
  -- (2^36 + 2^30 + 2^20 + 2^10 + 1) * 2^-44
  t0 = listArray (0, 63)
    [ floorLog2MulLn2Up 32 (logRecip j) - (bit 77 + bit 71 + bit 61 + bit 51 + bit 41) | j <- [0 .. 63] ]
  -- E1 = 2^-8 if A1 >= 7, else 2^-9
  t1 = listArray (0, 15) (map (logL1 7 85) [0 .. 15])
  ti pp qq = listArray (0, 63) (map (logLi pp qq 85) [0 .. 63])

-- | ln 2 rounded to nearest at 62 fraction bits
ln2Double62 :: Integer
ln2Double62 = (ln2Floor 63 + 1) `shiftR` 1

-- | ln 2 rounded up at 85 fraction bits
ln2Up85 :: Integer
ln2Up85 = ln2Floor 85 + 1

-- | floor (log2 (n / d) * ln2Up85), checked against the approximation error
floorLog2MulLn2Up :: Integer -> Integer -> Integer
floorLog2MulLn2Up n d
  | n == d = 0
  | otherwise = agree "floorLog2MulLn2Up" cands
 where
  sc = 200
  a = lnScaled n d sc
  b = ln2Floor sc
  e = 256
  cands = [ (a' * ln2Up85) `div` b' | a' <- [a - e, a + e], b' <- [b - e, b + e] ]

-- | Model of the IP's natural logarithm in double precision, see
-- Note [Logarithm, double precision].
logDouble :: Word64 -> Word64
logDouble w
  | ex == 0x7FF = if f /= 0 || s then nanDouble else 0x7FF0000000000000
  -- Zeros and subnormal numbers, of either sign
  | ex == 0 = 0xFFF0000000000000
  | s = nanDouble
  | w == 0x3FF0000000000000 = 0
  | ex == 1023 && f < bit 25 = nearOne (2 * toInteger f)
  | ex == 1022 && bit 52 - f < bit 26 = nearOne (toInteger f - bit 52)
  | otherwise = roundDouble (v < 0) mag 85
 where
  s = testBit w 63
  ex = fromIntegral ((w `shiftR` 52) .&. 0x7FF) :: Int
  f = w .&. (bit 52 - 1)
  -- near 1: z = zz * 2^-53, A = z - floor (z^2 * 2^102) * 2^-103
  nearOne zz =
    let a = zz `shiftL` 50 - (zz * zz) `shiftR` 4
    in roundDouble (a < 0) (abs a) 103
  fi = toInteger f
  fb = fromInteger (fi `shiftR` 51) :: Int
  ep = ex - 1023 + fb
  j = fromInteger (fi `shiftR` 46) :: Int
  z1 = (logRecip j * (bit 52 .|. fi)) `shiftL` (1 - fb) - bit 58        -- 2^-58
  tab i a = logTablesDouble ! i ! fromInteger a
  -- iteration 1: E1 = 2^-8 or 2^-9, exact
  a1 = z1 `shiftR` 50
  b1 = z1 - a1 `shiftL` 50
  ey1 = (z1 + bit 58) `shiftR` (if a1 >= 7 then 0 else 1)               -- 2^-66
  z2 = b1 `shiftL` 8 + ey1 - a1 * z1                                    -- 2^-66
  -- iteration 2: E2 = 2^-14, exact
  a2 = z2 `shiftR` 53
  b2 = z2 - a2 `shiftL` 53
  z3 = b2 `shiftL` 14 + (z2 + bit 66) - (a2 * z2) `shiftL` 1            -- 2^-80
  -- iterations 3 .. 5
  z4 = step 80 18 19 8 z3
  z5 = step 85 23 34 18 z4
  z6 = step 85 28 44 23 z5
  -- Z_{i+1} = B_i + E_i * (1 + Z_i) - A_i * 2^-q_i * Z_i', with both terms
  -- truncated to 85 fraction bits. The arguments: the lsb of Z_i, q_i, the
  -- shift of E_i * (1 + Z_i) to 2^-85 and the bits dropped for Z_i'.
  step :: Int -> Int -> Int -> Int -> Integer -> Integer
  step lsb q esh msh z =
    let a = z `shiftR` (lsb - q)
        b = (z - a `shiftL` (lsb - q)) `shiftL` (85 - lsb)
        ey = (z + bit lsb) `shiftR` esh
        pr = (a * (z `shiftR` msh)) `shiftR` 5                          -- 2^-90 -> 2^-85
    in b + ey - pr
  a3 = z3 `shiftR` 62
  a4 = z4 `shiftR` 62
  a5 = z5 `shiftR` 57
  sumL = logTablesDouble ! 0 ! j + tab 1 a1 + tab 2 a2 + tab 3 a3 + tab 4 a4 + tab 5 a5
  -- ln (1 + Z6) ~ Z6 - Z6'^2 / 2, Z6' = Z6 truncated to 51 fraction bits
  sq = z6 `shiftR` 34                                                   -- 2^-51
  z2o2 = (sq * sq) `shiftR` 18                                          -- 2^-85
  -- |V|, minus 2^-85 if V < 0, plus 2^-85 if Ep * ln 2 and ln Y0 have
  -- opposite signs
  el = toInteger ep * ln2Double62 `shiftL` 23
  logY = sumL + z6 - z2o2
  v = el + logY
  opposite = (el > 0 && logY < 0) || (el < 0 && logY > 0)
  mag = (if v < 0 then negate v - 1 else v) + (if opposite then 1 else 0)

-- | Round m * 2^-lsb (m > 0) to a double, to nearest with ties away from zero.
-- The results of the logarithm stay in the normal range.
roundDouble :: Bool -> Integer -> Int -> Word64
roundDouble neg m lsb =
  (if neg then bit 63 else 0) .|. fromIntegral ((e' + 1023) `shiftL` 52) .|. (fromInteger mant .&. (bit 52 - 1))
 where
  bl = bitLength m - 1
  e = bl - lsb
  drop_ = bl - 52
  (mant, e')
    | drop_ <= 0 = (m `shiftL` negate drop_, e)
    | otherwise =
        let q = (m `shiftR` (drop_ - 1) + 1) `shiftR` 1
        in if q == bit 53 then (q `shiftR` 1, e + 1) else (q, e)

-- * Exponential in half precision

-- | The IP's exponential in half precision, on bit patterns
expHalf :: Word16 -> Word16
expHalf w
  | e == 31 = if m /= 0 then nanHalf else if s then 0 else 0x7C00
  -- 16 or more
  | e >= 19 = if s then 0 else 0x7C00
  | biasedE >= 31 = 0x7C00
  | biasedE <= 0 = 0
  | otherwise = fromIntegral ((biasedE `shiftL` 10) + frac - 1024)
 where
  s = testBit w 15
  e = fromIntegral ((w `shiftR` 10) .&. 31) :: Int
  m = fromIntegral (w .&. 1023) :: Int

  -- x in fixed point with 13 fraction bits, |x| truncated; zeros and
  -- subnormal numbers give zero, and so a result of one
  fix
    | e == 0 = 0
    | e >= 12 = (1024 + m) `shiftL` (e - 12)
    | otherwise = (1024 + m) `shiftR` (12 - e)
  x = if s then negate fix else fix
  -- x = n/2 + lo, with lo in [0, 1/2)
  n = x `shiftR` 12
  lo = x - n `shiftL` 12

  -- e^lo with 13 fraction bits
  ea = expHalfTable ! (lo `shiftR` 6)
  b = ea + (ea * (lo .&. 63) + 4096) `shiftR` 13

  -- e^(n/2) * e^lo, rounded half-up to 11 significant bits
  p = expHalfMantissas ! n * b
  big = p >= bit 27
  sh = if big then 17 else 16
  frac0 = p `shiftR` sh + (if testBit p (sh - 1) then 1 else 0)
  biasedE0 = 15 + expHalfExponents ! n + (if big then 1 else 0)
  (biasedE, frac) = if frac0 == 2048 then (biasedE0 + 1, 1024) else (biasedE0, frac0)

-- | e^(a/128), rounded to 13 fraction bits, for a from 0 to 63
expHalfTable :: UArray Int Int
expHalfTable = listArray (0, 63) [fromInteger (roundExp (a % 128) 13) | a <- [0 .. 63]]

-- | e^(n/2) as m * 2^(k - 13) with m of 14 bits, rounded to nearest, for n
-- from -32 to 31: the exponents k and the significands m
expHalfExponents, expHalfMantissas :: UArray Int Int
expHalfExponents = listArray (-32, 31) (map (fst . expHalfEntry) [-32 .. 31])
expHalfMantissas = listArray (-32, 31) (map (snd . expHalfEntry) [-32 .. 31])

expHalfEntry :: Integer -> (Int, Int)
expHalfEntry n
  | mant == bit 14 = (k + 1, bit 13)
  | otherwise = (k, fromInteger mant)
 where
  v = expRational (n % 2)
  -- 2^k <= e^(n/2) < 2^(k+1)
  k = findExponent (-24)
  findExponent i
    | 2 ^^ (i + 1) > v = i
    | otherwise = findExponent (i + 1)
  mant = roundExact (v * 2 ^^ (13 - k))

-- | round (e^q * 2^k) for |q| < 1
roundExp :: Rational -> Int -> Integer
roundExp q k = roundExact (expRational q * 2 ^^ k)

-- | e^q for |q| <= 16, to within a relative error far below 2^-100: 150 terms
-- of the series, of 1/e^-q for negative q
expRational :: Rational -> Rational
expRational q
  | q < 0 = 1 / expRational (negate q)
  | otherwise = sum (take 150 (scanl (\t i -> t * q / fromInteger i) 1 [1 ..]))

-- | Round to nearest. None of the table entries is close enough to a tie for
-- the error of 'expRational' to matter, which this checks.
roundExact :: Rational -> Integer
roundExact v = agree "roundExact" [floor (v + 1 % 2 - eps), floor (v + 1 % 2 + eps)]
 where
  eps = 1 % 2 ^ (64 :: Int)

-- * Integer arithmetic

-- | Number of bits of a positive integer
bitLength :: Integer -> Int
bitLength i = fromIntegral (integerLog2 i) + 1

-- | Shift a non-negative integer to the right by the given number of bits,
-- rounding to nearest, ties to even.
roundShiftR :: (Integral a, Bits a) => Int -> a -> a
roundShiftR s m
  | s <= 0 = m
  | rest > half || (rest == half && odd q) = q + 1
  | otherwise = q
 where
  q = m `shiftR` s
  rest = m .&. (bit s - 1)
  half = bit (s - 1)
{-# INLINABLE roundShiftR #-}

-- | The value all candidates agree on: the candidates round the ends of an
-- interval that contains the exact value, so if they agree, the exact value
-- rounds to the same. The tables are far enough from rounding boundaries for
-- their candidates to agree.
agree :: String -> [Integer] -> Integer
agree _ (c : cs) | all (== c) cs = c
agree name _ = error (name <> ": too close to a rounding boundary")
