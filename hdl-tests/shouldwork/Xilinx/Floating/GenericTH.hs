{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Samples for the floating point HDL tests in half and double precision
(@FloatingHalf@ and @FloatingDouble@), generic in the precision. Most expected
results come from the simulation models in
"Clash.Cores.Xilinx.Floating.Internal". The hand-written cases have their
expected results written out, and are checked against the models at compile
time.
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Floating.GenericTH where

import Prelude

import Clash.Prelude (BitPack(..), BitSize, BitVector, Signed, Unsigned, natToNum)
import Data.Bits (bit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Word (Word64)
import Numeric.Half (Half)

import qualified Clash.Cores.Xilinx.Floating.Double as D
import qualified Clash.Cores.Xilinx.Floating.Half as H
import Clash.Cores.Xilinx.Floating.Internal
  (ipNaN, xilinxBinaryExact, xilinxCompare)
import qualified Clash.Cores.Xilinx.Floating.Internal as F

-- * Precisions

-- | Precisions under test, with the simulation models of their operations
class (BitPack a, RealFloat a) => TestFloat a where
  -- | Model of addition, subtraction, multiplication or division
  xilinxBinary :: (forall b. Fractional b => b -> b -> b) -> a -> a -> a

instance TestFloat Half where
  xilinxBinary = xilinxBinaryExact

instance TestFloat Double where
  xilinxBinary = xilinxBinaryExact

-- * Samples per test

-- | Latency of the @addLowLatency@ test: the maximum for 'H.LatencyArch'
type HalfAddLowLatencyDelay = 7

-- | Latency of the @mulMaxDsp@ test: the maximum for 'D.MaxDspUsage' on
-- UltraScale(+) devices, and below the maximum on 7-series devices
type DoubleMulMaxDspDelay = 13

halfAddSamples :: [(Half, Half, BitVector 16)]
halfAddSamples = romSamples (natToNum @H.AddDefDelay) addSamples

halfAddLowLatencySamples :: [(Half, Half, BitVector 16)]
halfAddLowLatencySamples =
  romSamples (natToNum @HalfAddLowLatencyDelay) addSamples

halfSubSamples :: [(Half, Half, BitVector 16)]
halfSubSamples = romSamples (natToNum @H.SubDefDelay) subSamples

halfMulSamples :: [(Half, Half, BitVector 16)]
halfMulSamples = romSamples (natToNum @H.MulDefDelay) mulSamples

halfDivSamples :: [(Half, Half, BitVector 16)]
halfDivSamples = romSamples (natToNum @H.DivDefDelay) divSamples

halfCompareSamples :: [(Half, Half, F.Ordering)]
halfCompareSamples =
  delayOutput (natToNum @H.CompareDefDelay) F.NaN compareSamples

doubleAddSamples :: [(Double, Double, BitVector 64)]
doubleAddSamples = romSamples (natToNum @D.AddDefDelay) addSamples

doubleSubSamples :: [(Double, Double, BitVector 64)]
doubleSubSamples = romSamples (natToNum @D.SubDefDelay) subSamples

doubleMulSamples :: [(Double, Double, BitVector 64)]
doubleMulSamples = romSamples (natToNum @D.MulDefDelay) mulSamples

doubleMulMaxDspSamples :: [(Double, Double, BitVector 64)]
doubleMulMaxDspSamples =
  romSamples (natToNum @DoubleMulMaxDspDelay) mulSamples

doubleDivSamples :: [(Double, Double, BitVector 64)]
doubleDivSamples = romSamples (natToNum @D.DivDefDelay) divSamples

doubleCompareSamples :: [(Double, Double, F.Ordering)]
doubleCompareSamples =
  delayOutput (natToNum @D.CompareDefDelay) F.NaN compareSamples

doubleFromU32Samples :: [(Unsigned 32, BitVector 64)]
doubleFromU32Samples =
  delayOutput1 (natToNum @D.FromU32DefDelay) 0 $
    map (\x -> (x, pack (fromIntegral x :: Double))) fromU32Inputs

doubleFromS32Samples :: [(Signed 32, BitVector 64)]
doubleFromS32Samples =
  delayOutput1 (natToNum @D.FromS32DefDelay) 0 $
    map (\x -> (x, pack (fromIntegral x :: Double))) fromS32Inputs

-- | Delay the expected results by the latency of the function under test, and
-- compare them by their bits: 'BitVector's are equal exactly when the floating
-- point numbers have the same bits, so zeros of different signs differ and
-- NaNs are equal.
romSamples ::
  TestFloat a => Int -> [(a, a, a)] -> [(a, a, BitVector (BitSize a))]
romSamples d = delayOutput d 0 . map (\(x, y, z) -> (x, y, pack z))

-- | Prepend @d@ default values to the expected outputs and repeat the last
-- inputs, so that the inputs of each row produce the output @d@ rows later.
delayOutput :: Int -> c -> [(a, b, c)] -> [(a, b, c)]
delayOutput d cDef es = zip3 xs ys rs
 where
  (xs0, ys0, rs0) = unzip3 es
  xs = xs0 ++ repeat (last xs0)
  ys = ys0 ++ repeat (last ys0)
  rs = replicate d cDef ++ rs0

-- | 'delayOutput' for functions with one input.
delayOutput1 :: Int -> b -> [(a, b)] -> [(a, b)]
delayOutput1 d bDef es = zip xs rs
 where
  (xs0, rs0) = unzip es
  xs = xs0 ++ repeat (last xs0)
  rs = replicate d bDef ++ rs0

-- * Samples per operation

addSamples :: forall a. (TestFloat a, Show a) => [(a, a, a)]
addSamples =
     checked "add" model (addSubCases ++ addCases)
  ++ modelled model (interesting `cartesianProduct` interesting)
  ++ nanSamples
  ++ modelled model (nearOverflowAdd ++ randomSamples 1)
 where
  model = xilinxBinary (+)

subSamples :: forall a. (TestFloat a, Show a) => [(a, a, a)]
subSamples =
     checked "sub" model
       (map (\(x, y, z) -> (x, negate y, z)) addSubCases ++ subCases)
  ++ modelled model (interesting `cartesianProduct` interesting)
  ++ nanSamples
  ++ modelled model
       (map (\(x, y) -> (x, negate y)) nearOverflowAdd ++ randomSamples 2)
 where
  model = xilinxBinary (-)

mulSamples :: forall a. (TestFloat a, Show a) => [(a, a, a)]
mulSamples =
     checked "mul" model mulCases
  ++ modelled model (interesting `cartesianProduct` interesting)
  ++ nanSamples
  ++ modelled model
       (  nearMinNormalMul
       ++ nearOverflowMul
       ++ take 150 (productsNear (eMin @a - 1) 3)
       ++ take 100 (productsNear (eMax @a - 1) 4)
       ++ randomSamples 5 )
 where
  model = xilinxBinary (*)

divSamples :: forall a. (TestFloat a, Show a) => [(a, a, a)]
divSamples =
     checked "div" model divCases
  ++ modelled model (interesting `cartesianProduct` interesting)
  ++ nanSamples
  ++ modelled model
       (  nearMinNormalDiv
       ++ nearOverflowDiv
       ++ take 150 (quotientsNear (eMin @a - 1) 6)
       ++ take 100 (quotientsNear (eMax @a - 1) 7)
       ++ randomSamples 8 )
 where
  model = xilinxBinary (/)

compareSamples :: forall a. (TestFloat a, Show a) => [(a, a, F.Ordering)]
compareSamples =
     checked "compare" xilinxCompare compareCases
  ++ modelled xilinxCompare (interesting `cartesianProduct` interesting)
  ++ [ r | nan <- nans, r <- [(nan, 1, F.NaN), (1, nan, F.NaN), (nan, nan, F.NaN)] ]
  ++ modelled xilinxCompare
       (  [(x, x) | x <- take 50 (randomBits 9)]
       ++ [(x, negate x) | x <- take 50 (randomBits 10)]
       ++ randomSamples 11 )

-- | Inputs of @fromU32@: the same as for single precision. All of them are
-- exactly representable as a 'Double'.
fromU32Inputs :: [Unsigned 32]
fromU32Inputs =
  [ 0
  , 1
  , maxBound
  , 0b0010_1010_1010_1010_1010_1010_1010_1010
  , 0b0101_0101_0101_0101_0101_0101_0101_0101
  , 0b1010_1010_1010_1010_1010_1010_1010_1010
  , 0b1101_0101_0101_0101_0101_0101_0101_0101
  , 0b0000_0000_1111_1111_1111_1111_1111_1111
  , 0b0000_0001_0000_0000_0000_0000_0000_0001
  , 0b0000_0001_0000_0000_0000_0000_0000_0011
  , 0b0000_0010_0000_0000_0000_0000_0000_0001
  ] ++ map fromIntegral (take 50 (randoms 12))

-- | Inputs of @fromS32@: the same as for single precision, and their
-- negations. All of them are exactly representable as a 'Double'.
fromS32Inputs :: [Signed 32]
fromS32Inputs =
     specials
  ++ map negate specials
  ++ map fromIntegral (take 50 (randoms 13))
 where
  specials =
    [ 0
    , 1
    , minBound
    , maxBound
    , 0b0010_1010_1010_1010_1010_1010_1010_1010
    , 0b0101_0101_0101_0101_0101_0101_0101_0101
    , 0b1010_1010_1010_1010_1010_1010_1010_1010
    , 0b1101_0101_0101_0101_0101_0101_0101_0101
    , 0b0000_0000_1111_1111_1111_1111_1111_1111
    , 0b0000_0001_0000_0000_0000_0000_0000_0001
    , 0b0000_0001_0000_0000_0000_0000_0000_0011
    , 0b0000_0010_0000_0000_0000_0000_0000_0001
    ]

-- * Hand-written cases

-- | Cases for both addition and subtraction, written as additions. For
-- subtraction, the second input is negated.
addSubCases :: forall a. TestFloat a => [(a, a, a)]
addSubCases =
  [ -- Subnormal positive results are flushed to plus zero
    --
    -- The unconditioned result is the subnormal of largest magnitude
    (-minNormal, minNormal + maxDenormal, 0)
    -- The unconditioned result is the subnormal of smallest magnitude
  , (-minNormal, minNormal + minDenormal, 0)
    -- Subnormal negative results are flushed to minus zero
  , (minNormal, -minNormal - maxDenormal, -0)
  , (minNormal, -minNormal - minDenormal, -0)
    -- Subnormal inputs are flushed to zero
    --
    -- The result would be the smallest normal number, but both inputs are
    -- flushed to zero.
  , (maxDenormal, minDenormal, 0)
    -- The result would be almost twice the smallest normal number
  , (maxDenormal, maxDenormal, 0)
    -- The result would be exact, but one input is flushed to zero
  , (minNormal, minDenormal, minNormal)
  , (maxDenormal, minNormal, minNormal)
    -- Negative versions
  , (-maxDenormal, -minDenormal, -0)
  , (-maxDenormal, -maxDenormal, -0)
  , (-minNormal, -minDenormal, -minNormal)
  , (-maxDenormal, -minNormal, -minNormal)
    -- Round to nearest
    --
    -- For a datatype with 4 bits of precision, the significands align as:
    -- 1000
    --     1001
    -- -------- +
    -- 1001
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ (digits - 1) + 1) (-digits)
    , 2 ^ (digits - 1) + 1
    )
    -- 1000
    --     01111
    -- --------- +
    -- 1000
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ digits - 1) (-digits - 1)
    , 2 ^ (digits - 1)
    )
    -- Ties to even
    --
    -- 1000
    --     1000
    -- -------- +
    -- 1000
  , ( 2 ^ (digits - 1)
    , encodeFloat (2 ^ (digits - 1)) (-digits)
    , 2 ^ (digits - 1)
    )
    -- 1001
    --     1000
    -- -------- +
    -- 1010
  , ( 2 ^ (digits - 1) + 1
    , encodeFloat (2 ^ (digits - 1)) (-digits)
    , 2 ^ (digits - 1) + 2
    )
    -- Rounding at the maximum exponent
    --
    -- 1111
    --     1000
    -- -------- +
    -- infinity
  , ( maxFinite
    , encodeFloat (2 ^ (digits - 1)) (eMax @a - 2 * digits)
    , infinity
    )
    -- 1111
    --     01111
    -- --------- +
    -- 1111
  , ( maxFinite
    , encodeFloat (2 ^ digits - 1) (eMax @a - 2 * digits - 1)
    , maxFinite
    )
  , (maxFinite, maxFinite, infinity)
  , (-maxFinite, -maxFinite, -infinity)
    -- Infinities
  , (infinity, -maxFinite, infinity)
  , (-infinity, maxFinite, -infinity)
  , (infinity, -infinity, ipNaN)
  ]
 where
  digits = digitsOf @a

-- | Cases for addition only
addCases :: forall a. TestFloat a => [(a, a, a)]
addCases =
  [ (1, 4, 5)
  , (2, 5, 7)
  , (3, 6, 9)
  , (0, -0, 0)
  , (-0, -0, -0)
  , (1, -1, 0)
  , (-1, 1, 0)
  ]

-- | Cases for subtraction only
subCases :: forall a. TestFloat a => [(a, a, a)]
subCases =
  [ (1, 6, -5)
  , (2, 5, -3)
  , (3, 4, -1)
  , (0, 0, 0)
  , (-0, 0, -0)
  , (-0, -0, 0)
  , (1, 1, 0)
  , (-1, -1, 0)
  , (infinity, infinity, ipNaN)
  ]

mulCases :: forall a. TestFloat a => [(a, a, a)]
mulCases =
  [ (1, 4, 4)
  , (2, 5, 10)
  , (3, 6, 18)
    -- The exact result is the smallest normal number times (1 - 2^-digits),
    -- just below the smallest normal number. The IP rounds it to 'digits'
    -- significant bits, which leaves it unchanged and still below the smallest
    -- normal number, so the IP flushes it to zero. IEEE rounding, which rounds
    -- to the subnormal grid, would give the smallest normal number instead.
  , (pred1, minNormal, 0)
  , (pred1, -minNormal, -0)
    -- Subnormal results are flushed to zero
  , (minNormal, 0.5, 0)
  , (-minNormal, 0.5, -0)
  , (encodeFloat 1 (1 - digits), minNormal, 0)
    -- Subnormal inputs are flushed to zero
  , (maxDenormal, maxFinite, 0)
  , (-maxDenormal, maxFinite, -0)
  , (encodeFloat 1 (digits - 1), minDenormal, 0)
  , (encodeFloat 1 (digits - 1), -minDenormal, -0)
    -- Overflow
  , (maxFinite, 2, infinity)
  , (maxFinite, -2, -infinity)
  , (maxFinite, 1 + epsilon, infinity)
    -- The exact result is the largest finite number minus one unit in the last
    -- place, plus 2^-digits units in the last place.
  , (maxFinite, pred1, encodeFloat (2 ^ digits - 2) (eMax @a - digits))
    -- Rounding: multiplying by five gives a significand with 'digits' + 2
    -- bits, so the two bits below the rounding position follow from the
    -- significand of the second input modulo four.
    --
    -- 01: round down
  , (5, m 1, fromInteger (5 * mi 1 - 1))
    -- 11: round up
  , (5, m 3, fromInteger (5 * mi 3 + 1))
    -- 10: a tie, rounded down to even
  , (5, m 2, fromInteger (5 * mi 2 - 2))
    -- 10: a tie, rounded up to even
  , (5, m 6, fromInteger (5 * mi 6 + 2))
    -- Infinities
  , (infinity, minNormal, infinity)
  , (-infinity, minNormal, -infinity)
  , (infinity, -minNormal, -infinity)
  , (-infinity, -minNormal, infinity)
  , (infinity, 0, ipNaN)
  , (-infinity, 0, ipNaN)
  , (infinity, -0, ipNaN)
  , (-infinity, -0, ipNaN)
  ]
 where
  digits = digitsOf @a
  mi k = 2 ^ (digits - 1) + k
  m k = fromInteger (mi k)

divCases :: forall a. TestFloat a => [(a, a, a)]
divCases =
  [ (1, 2, 0.5)
  , (3, 4, 0.75)
  , (7, 8, 0.875)
    -- The exact result is the smallest normal number times (1 - 2^-digits).
    -- As for multiplication, the IP flushes it to zero instead of rounding it
    -- up to the smallest normal number.
  , (encodeFloat (2 ^ digits - 1) (1 - digits), encodeFloat 1 (eMax @a - 1), 0)
    -- Subnormal results are flushed to zero
  , (minNormal, 2, 0)
  , (-minNormal, 2, -0)
  , (minNormal, maxFinite, 0)
    -- Subnormal inputs are flushed to zero
  , (maxDenormal, minNormal, 0)
  , (-maxDenormal, minNormal, -0)
  , (encodeFloat 2 (1 - digits), minDenormal, infinity)
  , (encodeFloat 2 (1 - digits), -minDenormal, -infinity)
    -- Results near the limits
  , (1, minNormal, encodeFloat 1 (1 - eMin @a))
  , (maxFinite, 0.5, infinity)
  , (maxFinite, -0.5, -infinity)
  , (maxFinite, minNormal, infinity)
    -- Division by zero
  , (1, 0, infinity)
  , (-1, 0, -infinity)
  , (1, -0, -infinity)
  , (-1, -0, infinity)
  , (0, 0, ipNaN)
    -- Infinities
  , (infinity, maxFinite, infinity)
  , (-infinity, maxFinite, -infinity)
  , (infinity, -maxFinite, -infinity)
  , (-infinity, -maxFinite, infinity)
  , (1, infinity, 0)
  , (1, -infinity, -0)
  , (infinity, infinity, ipNaN)
  , (-infinity, infinity, ipNaN)
  , (infinity, -infinity, ipNaN)
  , (-infinity, -infinity, ipNaN)
  ]
 where
  digits = digitsOf @a

compareCases :: forall a. TestFloat a => [(a, a, F.Ordering)]
compareCases =
  [ (1, 2, F.LT)
  , (2, 1, F.GT)
  , (1, 1, F.EQ)
  , (ipNaN, 1, F.NaN)
  , (1, ipNaN, F.NaN)
  , (0, -0, F.EQ)
    -- Subnormal inputs are flushed to zero
  , (minDenormal, 0, F.EQ)
  , (minDenormal, -minDenormal, F.EQ)
  , (maxDenormal, minDenormal, F.EQ)
  , (maxDenormal, minNormal, F.LT)
  , (-infinity, infinity, F.LT)
  , (infinity, maxFinite, F.GT)
  , (infinity, infinity, F.EQ)
  , (-infinity, -infinity, F.EQ)
  , (1, 1 + epsilon, F.LT)
  , (-1, -pred1, F.LT)
  ]

-- * Generated inputs

-- | Inputs whose products are close to the smallest normal number, where the
-- IP rounds differently from IEEE: the first inputs are a little below one,
-- the second ones a little above the smallest normal number.
nearMinNormalMul :: forall a. TestFloat a => [(a, a)]
nearMinNormalMul = [(s * x, y) | s <- [1, -1], x <- xs, y <- ys]
 where
  digits = digitsOf @a
  xs = [encodeFloat (2 ^ digits - k) (-digits) | k <- [1 .. 4]]
  ys = [encodeFloat (2 ^ (digits - 1) + j) (eMin @a - digits) | j <- [0 .. 3]]

-- | Inputs whose quotients are close to the smallest normal number
nearMinNormalDiv :: forall a. TestFloat a => [(a, a)]
nearMinNormalDiv =
     [(s * x, y) | s <- [1, -1], x <- xs, y <- ys]
  ++ [(s * x, encodeFloat 1 (eMax @a - 1)) | s <- [1, -1], x <- belowTwo]
 where
  digits = digitsOf @a
  xs = [encodeFloat (2 ^ (digits - 1) + j) (eMin @a - digits) | j <- [0 .. 3]]
  ys =
       [encodeFloat (2 ^ (digits - 1) + k) (1 - digits) | k <- [0 .. 3]]
    ++ [encodeFloat (2 ^ digits - k) (-digits) | k <- [1 .. 3]]
  belowTwo = [encodeFloat (2 ^ digits - k) (1 - digits) | k <- [1 .. 4]]

-- | Inputs whose sums are close to the largest finite number
nearOverflowAdd :: forall a. TestFloat a => [(a, a)]
nearOverflowAdd = [(s * x, s * y) | s <- [1, -1], x <- xs, y <- ys]
 where
  digits = digitsOf @a
  xs = [encodeFloat (2 ^ digits - k) (eMax @a - digits) | k <- [1 .. 3]]
  -- Multiples of a quarter unit in the last place of the largest finite number
  ys = [encodeFloat j (eMax @a - digits - 2) | j <- [1 .. 6]]

-- | Inputs whose products are close to the largest finite number
nearOverflowMul :: forall a. TestFloat a => [(a, a)]
nearOverflowMul = [(s * x, y) | s <- [1, -1], x <- xs, y <- ys]
 where
  digits = digitsOf @a
  xs = [encodeFloat (2 ^ digits - k) (eMax @a - digits) | k <- [1 .. 3]]
  ys =
       [encodeFloat (2 ^ (digits - 1) + j) (1 - digits) | j <- [0 .. 2]]
    ++ [encodeFloat (2 ^ digits - k) (-digits) | k <- [1 .. 2]]

-- | Inputs whose quotients are close to the largest finite number
nearOverflowDiv :: forall a. TestFloat a => [(a, a)]
nearOverflowDiv = [(s * x, y) | s <- [1, -1], x <- xs, y <- ys]
 where
  digits = digitsOf @a
  xs = [encodeFloat (2 ^ digits - k) (eMax @a - digits) | k <- [1 .. 3]]
  ys =
       [encodeFloat (2 ^ (digits - 1) + j) (1 - digits) | j <- [0 .. 2]]
    ++ [encodeFloat (2 ^ digits - k) (-digits) | k <- [1 .. 3]]

-- | Pseudo-random inputs, from the given seed: arbitrary bit patterns, and
-- numbers of similar magnitude, which exercise the alignment of significands
-- and cancellation in additions and subtractions.
randomSamples :: forall a. TestFloat a => Word64 -> [(a, a)]
randomSamples seed =
     take 150 (pairs (randomBits (seed * 1000 + 1)))
  ++ take 250 closePairs
 where
  digits = digitsOf @a
  closePairs =
    [ (normalWith w1 e1, normalWith w2 e2)
    | (w1, w2, w3) <- triples (randoms (seed * 1000 + 2))
    , let e1 = fromIntegral (w3 `mod` 9) - 4
          delta = fromIntegral ((w3 `shiftR` 8) `mod` fromIntegral (2 * digits + 7))
                - (digits + 3)
          e2 = max (eMin @a - 1) (min (eMax @a - 1) (e1 + delta))
    ]

-- | Pseudo-random pairs of normal numbers whose products have the given
-- exponent, give or take one.
productsNear :: forall a. TestFloat a => Int -> Word64 -> [(a, a)]
productsNear e seed =
  [ (normalWith w1 e1, normalWith w2 e2)
  | (w1, w2, w3) <- triples (randoms (seed * 1000 + 3))
  , let e1 = eLo + fromIntegral (w3 `mod` fromIntegral (eHi - eLo + 1))
        e2 = e - e1 + fromIntegral ((w3 `shiftR` 32) `mod` 3) - 1
  , e2 >= eLo && e2 <= eHi
  ]
 where
  (eLo, eHi) = (eMin @a - 1, eMax @a - 1)

-- | Pseudo-random pairs of normal numbers whose quotients have the given
-- exponent, give or take one.
quotientsNear :: forall a. TestFloat a => Int -> Word64 -> [(a, a)]
quotientsNear e seed =
  [ (normalWith w1 e1, normalWith w2 e2)
  | (w1, w2, w3) <- triples (randoms (seed * 1000 + 4))
  , let e2 = eLo + fromIntegral (w3 `mod` fromIntegral (eHi - eLo + 1))
        e1 = e + e2 + fromIntegral ((w3 `shiftR` 32) `mod` 3) - 1
  , e1 >= eLo && e1 <= eHi
  ]
 where
  (eLo, eHi) = (eMin @a - 1, eMax @a - 1)

-- | A normal number with exponent @e@ (so its magnitude is at least @2^e@ and
-- less than @2^(e+1)@), with its significand and sign taken from the given
-- random number.
normalWith :: forall a. RealFloat a => Word64 -> Int -> a
normalWith w e =
  (if testBit w 63 then negate else id) $
    encodeFloat
      (2 ^ (digits - 1) + toInteger w `mod` 2 ^ (digits - 1))
      (e - digits + 1)
 where
  digits = digitsOf @a

-- | Pseudo-random bit patterns
randomBits :: forall a. TestFloat a => Word64 -> [a]
randomBits = map (unpack . fromIntegral) . randoms

-- | Pseudo-random numbers from the given seed (SplitMix64)
randoms :: Word64 -> [Word64]
randoms = drop 1 . map mix . iterate (+ 0x9e3779b97f4a7c15)
 where
  mix z0 =
    let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
    in z2 `xor` (z2 `shiftR` 31)

pairs :: [b] -> [(b, b)]
pairs (x:y:zs) = (x, y) : pairs zs
pairs _ = []

triples :: [b] -> [(b, b, b)]
triples (x:y:z:ws) = (x, y, z) : triples ws
triples _ = []

-- * Special values

interesting :: forall a. TestFloat a => [a]
interesting =
  [ 0
  , -0
  , 1
  , -1
  , 2
  , 3
  , 42
  , 0.1
  , 1 + epsilon
  , pred1
  , minDenormal
  , -minDenormal
  , maxDenormal
  , -maxDenormal
  , minNormal
  , -minNormal
  , minNormal * (1 + epsilon)
  , maxFinite
  , -maxFinite
  , infinity
  , -infinity
  , ipNaN
  ]

-- | All kinds of NaNs: quiet and signaling, both signs, and several payloads
nans :: forall a. TestFloat a => [a]
nans =
  [ mkNaN negative quiet payload
  | negative <- [False, True]
  , (quiet, payload) <-
      [ (True, 0)
      , (True, 1)
      , (False, 1)
      , (True, msbPayload)
      , (False, msbPayload)
      , (True, maxPayload)
      , (False, maxPayload)
      , (True, 0x5a3c96e1b2d4f087 .&. maxPayload)
      , (False, 0x5a3c96e1b2d4f087 .&. maxPayload)
      ]
  ]
 where
  payloadWidth = digitsOf @a - 2
  msbPayload = bit (payloadWidth - 1)
  maxPayload = bit payloadWidth - 1

-- | Every NaN in 'nans' as either input gives 'ipNaN'
nanSamples :: forall a. TestFloat a => [(a, a, a)]
nanSamples =
  [ r
  | nan <- nans
  , r <- [(nan, 1, ipNaN), (1, nan, ipNaN), (nan, nan, ipNaN)]
  ]

-- | A NaN with the given sign, quiet bit and payload. The payload of a
-- signaling NaN must not be zero, or it is an infinity.
mkNaN :: forall a. TestFloat a => Bool -> Bool -> Integer -> a
mkNaN negative quiet payload = unpack (fromInteger bits)
 where
  -- Width of the exponent
  e = natToNum @(BitSize a) - digitsOf @a
  -- Stored bits of the significand
  m = digitsOf @a - 1
  bits =
        (if negative then bit (e + m) else 0)
    .|. ((bit e - 1) `shiftL` m)
    .|. (if quiet then bit (m - 1) else 0)
    .|. payload

-- | Number of significant bits, including the hidden bit
digitsOf :: forall a. RealFloat a => Int
digitsOf = floatDigits (undefined :: a)

-- | Exponent range, see 'floatRange'
eMin, eMax :: forall a. RealFloat a => Int
eMin = fst (floatRange (undefined :: a))
eMax = snd (floatRange (undefined :: a))

minNormal :: forall a. RealFloat a => a
minNormal = encodeFloat 1 (eMin @a - 1)

minDenormal :: forall a. RealFloat a => a
minDenormal = encodeFloat 1 (eMin @a - digitsOf @a)

maxDenormal :: forall a. RealFloat a => a
maxDenormal = encodeFloat (2 ^ (digitsOf @a - 1) - 1) (eMin @a - digitsOf @a)

maxFinite :: forall a. RealFloat a => a
maxFinite = encodeFloat (2 ^ digitsOf @a - 1) (eMax @a - digitsOf @a)

-- | Difference between one and the next larger number
epsilon :: forall a. RealFloat a => a
epsilon = encodeFloat 1 (1 - digitsOf @a)

-- | The largest number below one
pred1 :: forall a. RealFloat a => a
pred1 = encodeFloat (2 ^ digitsOf @a - 1) (negate (digitsOf @a))

infinity :: RealFloat a => a
infinity = 1 / 0

-- * Helpers

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct as bs = [(a, b) | a <- as, b <- bs]

-- | Add the expected results according to a model
modelled :: (a -> b -> c) -> [(a, b)] -> [(a, b, c)]
modelled model = map (\(x, y) -> (x, y, model x y))

-- | Check hand-written cases against a model, comparing the bits of the
-- results. Fails at compile time if they disagree.
checked ::
  (Show a, Show b, Show c, BitPack c) =>
  String -> (a -> b -> c) -> [(a, b, c)] -> [(a, b, c)]
checked name model = map check
 where
  check s@(x, y, expected)
    | pack actual == pack expected = s
    | otherwise = error $
        name <> " " <> show x <> " " <> show y <> ": expected "
          <> show expected <> ", but the model gives " <> show actual
   where
    actual = model x y
