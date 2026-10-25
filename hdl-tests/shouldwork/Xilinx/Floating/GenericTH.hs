{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Samples for the floating point HDL tests (@Floating@, @FloatingHalf@ and
@FloatingDouble@), generic in the precision. Most expected results come from
the simulation models in
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
import Data.Proxy (Proxy(..))
import Data.Word (Word64)
import Numeric.Half (Half)

import qualified Clash.Cores.Xilinx.Floating as S
import qualified Clash.Cores.Xilinx.Floating.Double as D
import qualified Clash.Cores.Xilinx.Floating.Half as H
import Clash.Cores.Xilinx.Floating.Internal
  ( XilinxFloat(..), xilinxAbs, xilinxCompare, xilinxConvert, xilinxFromInteger
  , xilinxSqrt, xilinxToS32 )
import qualified Clash.Cores.Xilinx.Floating.Internal as F

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
  romSamples1 (natToNum @D.FromU32DefDelay) (fromIntegerSamples @Double fromU32Inputs)

doubleFromS32Samples :: [(Signed 32, BitVector 64)]
doubleFromS32Samples =
  romSamples1 (natToNum @D.FromS32DefDelay) (fromIntegerSamples @Double fromS32Inputs)

-- | Latency of the @sqrtBram@ test: the maximum for 'H.FullBMemUsage'
type HalfSqrtBramDelay = 4

halfSqrtSamples :: [(Half, BitVector 16)]
halfSqrtSamples = romSamples1 (natToNum @H.SqrtDefDelay) sqrtSamples

halfSqrtBramSamples :: [(Half, BitVector 16)]
halfSqrtBramSamples = romSamples1 (natToNum @HalfSqrtBramDelay) sqrtSamples

halfAbsSamples :: [(Half, BitVector 16)]
halfAbsSamples = romSamples1 0 absSamples

halfConvertSamples :: [(Half, BitVector 16)]
halfConvertSamples =
  romSamples1 (natToNum @(H.ConvertDefDelay Half)) (convertSamples @Half @Half)

halfToFloatSamples :: [(Half, BitVector 32)]
halfToFloatSamples =
  romSamples1 (natToNum @H.ToFloatDefDelay) (convertSamples @Half @Float)

halfToDoubleSamples :: [(Half, BitVector 64)]
halfToDoubleSamples =
  romSamples1 (natToNum @H.ToDoubleDefDelay) (convertSamples @Half @Double)

singleSqrtSamples :: [(Float, BitVector 32)]
singleSqrtSamples = romSamples1 (natToNum @S.SqrtDefDelay) sqrtSamples

singleAbsSamples :: [(Float, BitVector 32)]
singleAbsSamples = romSamples1 0 absSamples

singleConvertSamples :: [(Float, BitVector 32)]
singleConvertSamples =
  romSamples1 (natToNum @(S.ConvertDefDelay Float)) (convertSamples @Float @Float)

singleToHalfSamples :: [(Float, BitVector 16)]
singleToHalfSamples =
  romSamples1 (natToNum @S.ToHalfDefDelay) (convertSamples @Float @Half)

singleToDoubleSamples :: [(Float, BitVector 64)]
singleToDoubleSamples =
  romSamples1 (natToNum @S.ToDoubleDefDelay) (convertSamples @Float @Double)

singleFromU64Samples :: [(Unsigned 64, BitVector 32)]
singleFromU64Samples =
  romSamples1 (natToNum @S.FromU64DefDelay) (fromIntegerSamples @Float fromU64Inputs)

singleFromS64Samples :: [(Signed 64, BitVector 32)]
singleFromS64Samples =
  romSamples1 (natToNum @S.FromS64DefDelay) (fromIntegerSamples @Float fromS64Inputs)

singleToS32Samples :: [(Float, Signed 32)]
singleToS32Samples = delayOutput1 (natToNum @S.ToS32DefDelay) 0 toS32Samples

doubleSqrtSamples :: [(Double, BitVector 64)]
doubleSqrtSamples = romSamples1 (natToNum @D.SqrtDefDelay) sqrtSamples

doubleAbsSamples :: [(Double, BitVector 64)]
doubleAbsSamples = romSamples1 0 absSamples

doubleConvertSamples :: [(Double, BitVector 64)]
doubleConvertSamples =
  romSamples1 (natToNum @(D.ConvertDefDelay Double)) (convertSamples @Double @Double)

doubleToHalfSamples :: [(Double, BitVector 16)]
doubleToHalfSamples =
  romSamples1 (natToNum @D.ToHalfDefDelay) (convertSamples @Double @Half)

doubleToFloatSamples :: [(Double, BitVector 32)]
doubleToFloatSamples =
  romSamples1 (natToNum @D.ToFloatDefDelay) (convertSamples @Double @Float)

doubleFromU64Samples :: [(Unsigned 64, BitVector 64)]
doubleFromU64Samples =
  romSamples1 (natToNum @D.FromU64DefDelay) (fromIntegerSamples @Double fromU64Inputs)

doubleFromS64Samples :: [(Signed 64, BitVector 64)]
doubleFromS64Samples =
  romSamples1 (natToNum @D.FromS64DefDelay) (fromIntegerSamples @Double fromS64Inputs)

doubleToS32Samples :: [(Double, Signed 32)]
doubleToS32Samples = delayOutput1 (natToNum @D.ToS32DefDelay) 0 toS32Samples

-- | Like 'romSamples', for functions with one input.
romSamples1 :: BitPack b => Int -> [(a, b)] -> [(a, BitVector (BitSize b))]
romSamples1 d = delayOutput1 d 0 . map (\(x, y) -> (x, pack y))

-- | Delay the expected results by the latency of the function under test, and
-- compare them by their bits: 'BitVector's are equal exactly when the floating
-- point numbers have the same bits, so zeros of different signs differ and
-- NaNs are equal.
romSamples ::
  XilinxFloat a => Int -> [(a, a, a)] -> [(a, a, BitVector (BitSize a))]
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

addSamples :: forall a. (XilinxFloat a, Show a) => [(a, a, a)]
addSamples =
     checked "add" model (addSubCases ++ addCases)
  ++ modelled model (interesting `cartesianProduct` interesting)
  ++ nanSamples
  ++ modelled model (nearOverflowAdd ++ randomSamples 1)
 where
  model = xilinxBinary (+)

subSamples :: forall a. (XilinxFloat a, Show a) => [(a, a, a)]
subSamples =
     checked "sub" model
       (map (\(x, y, z) -> (x, negate y, z)) addSubCases ++ subCases)
  ++ modelled model (interesting `cartesianProduct` interesting)
  ++ nanSamples
  ++ modelled model
       (map (\(x, y) -> (x, negate y)) nearOverflowAdd ++ randomSamples 2)
 where
  model = xilinxBinary (-)

mulSamples :: forall a. (XilinxFloat a, Show a) => [(a, a, a)]
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

divSamples :: forall a. (XilinxFloat a, Show a) => [(a, a, a)]
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

compareSamples :: forall a. (XilinxFloat a, Show a) => [(a, a, F.Ordering)]
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

-- | Inputs and results of the square root: the hand-written cases, special
-- values, NaNs, exact squares, numbers whose square root is very close to
-- halfway between two numbers, and pseudo-random numbers.
sqrtSamples :: forall a. (XilinxFloat a, Show a) => [(a, a)]
sqrtSamples =
     checked1 "sqrt" xilinxSqrt sqrtCases
  ++ modelled1 xilinxSqrt
       (  interesting
       ++ nans
       ++ sqrtNearHalfway
       ++ [ encodeFloat (m * m) (2 * k)
          | m <- [3, 5, 7, 2 ^ ((digitsOf @a - 1) `div` 2) - 1]
          , k <- [-3, 0, 3]
          ]
       ++ take 100 (randomBits 20)
       ++ take 100 (map abs (randomNormals 21)) )

-- | Inputs and results of the absolute value: special values, NaNs and
-- pseudo-random numbers, all of both signs.
absSamples :: forall a. (XilinxFloat a, Show a) => [(a, a)]
absSamples =
     checked1 "abs" xilinxAbs absCases
  ++ modelled1 xilinxAbs
       (interesting ++ nans ++ take 100 (randomBits 22))

-- | Inputs and results of the conversion from precision @a@ to precision @b@.
-- Besides special values, NaNs and pseudo-random numbers, conversions to a
-- narrower precision get numbers close to the smallest normal number and the
-- largest finite number of precision @b@, and close to halfway between two
-- numbers of precision @b@.
convertSamples ::
  forall a b. (XilinxFloat a, XilinxFloat b, Show a, Show b) => [(a, b)]
convertSamples =
     checked1 "convert" xilinxConvert (convertCases @a @b)
  ++ modelled1 xilinxConvert
       (  interesting
       ++ nans
       ++ take 100 (randomBits 23)
       ++ (if narrowing then narrowingInputs else []) )
 where
  pa = digitsOf @a
  pb = digitsOf @b
  narrowing = pb < pa
  -- The smallest normal number of precision b is @2^e@
  e = eMin @b - 1
  narrowingInputs =
       concatMap (\x -> neighbours x ++ neighbours (negate x))
         [ -- Below the smallest normal number of b, but representable with
           -- its precision: flushed to zero
           encodeFloat (2 ^ pb - 1) (e - pb)
           -- Halfway between that and the smallest normal number: rounds up
         , encodeFloat (2 ^ (pb + 1) - 1) (e - pb - 1)
         , encodeFloat 1 e
           -- The largest finite number of b
         , encodeFloat (2 ^ pb - 1) (eMax @b - pb)
           -- Halfway between that and 2^eMax: rounds to infinity
         , encodeFloat (2 ^ (pb + 1) - 1) (eMax @b - pb - 1)
         ]
    ++ take 100 nearTies
  -- Numbers within an ulp of halfway between two numbers of precision b
  nearTies =
    [ nudge ((w `shiftR` 32) `mod` 3) (signedWith w (encodeFloat (2 * m + 1) (ex - pb)))
    | w <- randoms 24
    , let m = 2 ^ (pb - 1) + toInteger (w `mod` (2 ^ (pb - 1)))
          ex = e - pb - 1 + fromIntegral ((w `shiftR` 40) `mod` fromIntegral (eMax @b - e + pb + 2))
    ]
  signedWith :: Word64 -> a -> a
  signedWith w x = if testBit w 63 then negate x else x
  -- Move one unit in the last place down (0), not at all (1) or up (2)
  nudge :: Word64 -> a -> a
  nudge k x = unpack (pack x + fromIntegral k - 1)

-- | The number and the numbers one and two units in the last place away from
-- it
neighbours :: XilinxFloat a => a -> [a]
neighbours x = [unpack (pack x + fromInteger d) | d <- [-2, -1, 0, 1, 2]]

-- | Inputs and results of the conversion to @Signed 32@: special values, NaNs,
-- numbers (close to) halfway between two integers, numbers close to the
-- bounds of @Signed 32@, and pseudo-random numbers.
toS32Samples :: forall a. (XilinxFloat a, Show a) => [(a, Signed 32)]
toS32Samples =
     checked1 "toS32" xilinxToS32 toS32Cases
  ++ modelled1 xilinxToS32
       (  interesting
       ++ nans
       ++ concatMap neighbours
            [ fromRational (s * (fromInteger k + 1 / 2))
            | s <- [1, -1], k <- [0, 1, 2, 3, 1000, 2 ^ (min 30 (digitsOf @a - 2)) + 1] ]
       ++ concatMap neighbours
            [ s * t
            | s <- [1, -1]
            , t <- [2 ^ (31 :: Int), 2 ^ (31 :: Int) - 1, fromRational (2 ^ (31 :: Int) - 1 / 2)] ]
       ++ take 100 (randomBits 25)
       ++ take 100 [x | x <- randomNormals 26, abs x < 2 ^ (33 :: Int)] )

-- | Inputs and results of conversions of integers
fromIntegerSamples :: forall a i. (XilinxFloat a, Integral i) => [i] -> [(i, a)]
fromIntegerSamples = map (\x -> (x, xilinxFromInteger (toInteger x)))

-- | Inputs of @fromU64@: special values, rounding cases (just below, at and
-- just above halfway between two numbers) in single and double precision, and
-- pseudo-random numbers.
fromU64Inputs :: [Unsigned 64]
fromU64Inputs =
     [0, 1, 2 ^ (24 :: Int) + 1, 2 ^ (53 :: Int) + 1, 2 ^ (63 :: Int), maxBound, maxBound - 1]
  ++ [ (m `shiftL` k) + r
     | (p, m) <- [(24, 0xC00001), (53, 0x1C000000000001)]
     , k <- [1, 2, 64 - p]
     , let half = 2 ^ (k - 1)
     , r <- [half - 1, half, half + 1, 2 * half - 1]
     ]
  ++ [ (m `shiftL` k) + r
     | (p, m) <- [(24, 0xC00002), (53, 0x1C000000000002)]
     , k <- [1, 2, 64 - p]
     , let half = 2 ^ (k - 1)
     , r <- [half]
     ]
  ++ map fromIntegral (take 50 (randoms 27))

-- | Inputs of @fromS64@: the inputs of @fromU64@ as signed numbers, their
-- halves and the negations of those, and the extremes.
fromS64Inputs :: [Signed 64]
fromS64Inputs =
     [minBound, maxBound, minBound + 1, -1]
  ++ [ f (fromIntegral w)
     | w <- fromU64Inputs
     , f <- [id, (`shiftR` 1), negate . (`shiftR` 1)]
     ]

-- * Hand-written cases

-- | Cases for both addition and subtraction, written as additions. For
-- subtraction, the second input is negated.
addSubCases :: forall a. XilinxFloat a => [(a, a, a)]
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
addCases :: forall a. XilinxFloat a => [(a, a, a)]
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
subCases :: forall a. XilinxFloat a => [(a, a, a)]
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

mulCases :: forall a. XilinxFloat a => [(a, a, a)]
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

divCases :: forall a. XilinxFloat a => [(a, a, a)]
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

compareCases :: forall a. XilinxFloat a => [(a, a, F.Ordering)]
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

sqrtCases :: forall a. XilinxFloat a => [(a, a)]
sqrtCases =
  [ (4, 2)
  , (1, 1)
  , (0.25, 0.5)
  , (0, 0)
    -- The square root of a negative zero is a negative zero
  , (-0, -0)
    -- Subnormal inputs are flushed to zero, keeping their sign
  , (minDenormal, 0)
  , (maxDenormal, 0)
  , (-minDenormal, -0)
  , (-maxDenormal, -0)
    -- The exponent of the smallest normal number is even
  , (minNormal, encodeFloat 1 ((eMin @a - 1) `div` 2))
  , (infinity, infinity)
    -- Other negative numbers give NaN
  , (-1, ipNaN)
  , (-minNormal, ipNaN)
  , (-maxFinite, ipNaN)
  , (-infinity, ipNaN)
  ]

absCases :: forall a. XilinxFloat a => [(a, a)]
absCases =
  [ (-1, 1)
  , (1, 1)
  , (-0, 0)
  , (0, 0)
  , (-infinity, infinity)
    -- Subnormal numbers are not flushed to zero
  , (-minDenormal, minDenormal)
  , (-maxDenormal, maxDenormal)
  , (maxDenormal, maxDenormal)
    -- NaNs keep their payload: only the sign bit is cleared
  , (mkNaN True False 1, mkNaN False False 1)
  , (mkNaN True True 5, mkNaN False True 5)
  , (mkNaN False False 5, mkNaN False False 5)
  ]

convertCases :: forall a b. (XilinxFloat a, XilinxFloat b) => [(a, b)]
convertCases =
  [ (1, 1)
  , (-2, -2)
  , (0.5, 0.5)
  , (0, 0)
  , (-0, -0)
    -- Subnormal inputs are flushed to zero, keeping their sign
  , (minDenormal, 0)
  , (-maxDenormal, -0)
  , (infinity, infinity)
  , (-infinity, -infinity)
    -- Every NaN becomes the NaN the IP generates
  , (ipNaN, ipNaN)
  , (mkNaN True False 1, ipNaN)
  , (mkNaN False True 5, ipNaN)
  ] ++
  if pb < pa
    then
      [ -- Overflow
        (maxFinite, infinity)
      , (-maxFinite, -infinity)
        -- Results below the smallest normal number are flushed to zero
      , (minNormal, 0)
      , (-minNormal, -0)
        -- Round to nearest
      , (1 + epsilon, 1)
        -- Ties to even: 1 + 2^-pb is halfway between 1 and the next number,
        -- 1 + 3 * 2^-pb halfway between that and the one after it
      , (encodeFloat (2 ^ pb + 1) (negate pb), 1)
      , (encodeFloat (2 ^ pb + 3) (negate pb), encodeFloat (2 ^ (pb - 1) + 2) (1 - pb))
      ]
    else
      [ -- Exact
        (maxFinite, encodeFloat (2 ^ pa - 1) (eMax @a - pa))
      , (minNormal, encodeFloat 1 (eMin @a - 1))
      , (1 + epsilon, encodeFloat (2 ^ (pa - 1) + 1) (1 - pa))
      ]
 where
  pa = digitsOf @a
  pb = digitsOf @b

toS32Cases :: forall a. XilinxFloat a => [(a, Signed 32)]
toS32Cases =
  [ (0, 0)
  , (-0, 0)
  , (1, 1)
  , (-1, -1)
  , (0.75, 1)
  , (0.25, 0)
    -- Ties to even
  , (0.5, 0)
  , (1.5, 2)
  , (2.5, 2)
  , (3.5, 4)
  , (-0.5, 0)
  , (-1.5, -2)
  , (-2.5, -2)
    -- Subnormal numbers become zero
  , (minDenormal, 0)
  , (-maxDenormal, 0)
    -- Saturation
  , (2 ^ (31 :: Int), maxBound)
  , (-(2 ^ (31 :: Int)), minBound)
  , (maxFinite, maxBound)
  , (-maxFinite, minBound)
  , (infinity, maxBound)
  , (-infinity, minBound)
    -- Every NaN becomes the smallest number
  , (ipNaN, minBound)
  , (mkNaN True False 1, minBound)
  ]

-- * Generated inputs

-- | Inputs whose products are close to the smallest normal number, where the
-- IP rounds differently from IEEE: the first inputs are a little below one,
-- the second ones a little above the smallest normal number.
nearMinNormalMul :: forall a. XilinxFloat a => [(a, a)]
nearMinNormalMul = [(s * x, y) | s <- [1, -1], x <- xs, y <- ys]
 where
  digits = digitsOf @a
  xs = [encodeFloat (2 ^ digits - k) (-digits) | k <- [1 .. 4]]
  ys = [encodeFloat (2 ^ (digits - 1) + j) (eMin @a - digits) | j <- [0 .. 3]]

-- | Inputs whose quotients are close to the smallest normal number
nearMinNormalDiv :: forall a. XilinxFloat a => [(a, a)]
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
nearOverflowAdd :: forall a. XilinxFloat a => [(a, a)]
nearOverflowAdd = [(s * x, s * y) | s <- [1, -1], x <- xs, y <- ys]
 where
  digits = digitsOf @a
  xs = [encodeFloat (2 ^ digits - k) (eMax @a - digits) | k <- [1 .. 3]]
  -- Multiples of a quarter unit in the last place of the largest finite number
  ys = [encodeFloat j (eMax @a - digits - 2) | j <- [1 .. 6]]

-- | Inputs whose products are close to the largest finite number
nearOverflowMul :: forall a. XilinxFloat a => [(a, a)]
nearOverflowMul = [(s * x, y) | s <- [1, -1], x <- xs, y <- ys]
 where
  digits = digitsOf @a
  xs = [encodeFloat (2 ^ digits - k) (eMax @a - digits) | k <- [1 .. 3]]
  ys =
       [encodeFloat (2 ^ (digits - 1) + j) (1 - digits) | j <- [0 .. 2]]
    ++ [encodeFloat (2 ^ digits - k) (-digits) | k <- [1 .. 2]]

-- | Inputs whose quotients are close to the largest finite number
nearOverflowDiv :: forall a. XilinxFloat a => [(a, a)]
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
randomSamples :: forall a. XilinxFloat a => Word64 -> [(a, a)]
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
productsNear :: forall a. XilinxFloat a => Int -> Word64 -> [(a, a)]
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
quotientsNear :: forall a. XilinxFloat a => Int -> Word64 -> [(a, a)]
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

-- | Pseudo-random normal numbers of both signs, with exponents anywhere in the
-- normal range
randomNormals :: forall a. RealFloat a => Word64 -> [a]
randomNormals seed =
  [ normalWith w1 (eMin @a - 1 + fromIntegral (w2 `mod` fromIntegral (eMax @a - eMin @a + 1)))
  | (w1, w2) <- pairs (randoms seed)
  ]

-- | Numbers whose square root is very close to halfway between two numbers of
-- the precision, @p@ bits. Their significands are @s^2 - c@, for a small
-- @c = 1 (mod 8)@ and an odd @s@ of @p+1@ bits with @s^2 = c (mod 2^t)@, where
-- @t@ is the number of bits of @s^2@ beyond @p@: then @s^2 - c@ has at most
-- @p@ significant bits and its square root is within about @c / (2s)@ of @s@.
sqrtNearHalfway :: forall a. RealFloat a => [a]
sqrtNearHalfway =
  [ encodeFloat (s * s - c) (2 * k)
  | c <- [-15, -7, 1, 9, 17]
  , t <- [p + 1, p + 2]
  , s <- sqrtsMod2 c t
  , s >= 2 ^ p && s < 2 ^ (p + 1)
  , bitLength (s * s - c) <= p + t
    -- Results near one, near the smallest normal number and near the largest
    -- finite number
  , k <- [negate p - 1, (eMin @a - 2 * p) `div` 2, (eMax @a - 2 * p - 2) `div` 2]
  ]
 where
  p = digitsOf @a
  bitLength :: Integer -> Int
  bitLength = length . takeWhile (> 0) . iterate (`shiftR` 1)

-- | All square roots of @c@ modulo @2^t@, for @c = 1 (mod 8)@ and @t >= 3@
sqrtsMod2 :: Integer -> Int -> [Integer]
sqrtsMod2 c t = [x `mod` 2 ^ t | x <- [r, negate r, r + half, half - r]]
 where
  half = 2 ^ (t - 1)
  -- If r^2 = c (mod 2^k), then r or r + 2^(k-1) is a square root modulo
  -- 2^(k+1)
  r = foldl lift 1 [3 .. t - 1]
  lift x k
    | (x * x - c) `mod` (2 ^ (k + 1)) == 0 = x
    | otherwise = x + 2 ^ (k - 1)

-- | Pseudo-random bit patterns
randomBits :: forall a. XilinxFloat a => Word64 -> [a]
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

interesting :: forall a. XilinxFloat a => [a]
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
nans :: forall a. XilinxFloat a => [a]
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
  payloadWidth = fromInteger (fractionWidth (Proxy @a)) - 2
  msbPayload = bit (payloadWidth - 1)
  maxPayload = bit payloadWidth - 1

-- | Every NaN in 'nans' as either input gives 'ipNaN'
nanSamples :: forall a. XilinxFloat a => [(a, a, a)]
nanSamples =
  [ r
  | nan <- nans
  , r <- [(nan, 1, ipNaN), (1, nan, ipNaN), (nan, nan, ipNaN)]
  ]

-- | A NaN with the given sign, quiet bit and payload. The payload of a
-- signaling NaN must not be zero, or it is an infinity.
mkNaN :: forall a. XilinxFloat a => Bool -> Bool -> Integer -> a
mkNaN negative quiet payload = unpack (fromInteger bits)
 where
  e = fromInteger (exponentWidth (Proxy @a))
  -- Stored bits of the significand
  m = fromInteger (fractionWidth (Proxy @a)) - 1
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

-- | 'modelled' for functions with one input
modelled1 :: (a -> b) -> [a] -> [(a, b)]
modelled1 model = map (\x -> (x, model x))

-- | 'checked' for functions with one input
checked1 ::
  (Show a, Show b, BitPack b) => String -> (a -> b) -> [(a, b)] -> [(a, b)]
checked1 name model = map check
 where
  check s@(x, expected)
    | pack actual == pack expected = s
    | otherwise = error $
        name <> " " <> show x <> ": expected " <> show expected
          <> ", but the model gives " <> show actual
   where
    actual = model x

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
