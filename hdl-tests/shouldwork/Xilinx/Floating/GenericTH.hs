{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Samples for the floating point HDL tests (@Floating@, @FloatingHalf@ and
@FloatingDouble@), generic in the precision. Most expected results come from
the simulation models in
"Clash.Cores.Xilinx.Floating.Internal". The hand-written cases have their
expected results written out, and are checked against the models at compile
time. The reciprocal, reciprocal square root, exponential and logarithm only
use inputs for which the models agree with the IP, see
Note [Samples of the reciprocal, reciprocal square root, exponential and logarithm].
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
  ( XilinxFloat(..), xilinxAbs, xilinxCompare, xilinxConvert, xilinxExp
  , xilinxFromInteger, xilinxLog, xilinxRSqrt, xilinxRecip, xilinxSqrt
  , xilinxToS32 )
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

-- | Latency of the @rsqrtMinLatency@ test: the minimum in half precision
type HalfRSqrtMinLatencyDelay = 1

-- | Latency of the @recipNoDsp@ test: the maximum for 'S.NoDspUsage'
type SingleRecipNoDspDelay = 36

-- | Latency of the @expBram@ test in single precision: the maximum for
-- 'S.MediumDspUsage' and 'S.FullBMemUsage'
type SingleExpBramDelay = 21

-- | Latency of the @expBram@ test in double precision: the maximum for
-- 'D.FullDspUsage' and 'D.FullBMemUsage'
type DoubleExpBramDelay = 57

-- | Latency of the @logNoDsp@ test: the maximum for 'D.NoDspUsage'
type DoubleLogNoDspDelay = 37

halfRecipSamples :: [(Half, BitVector 16)]
halfRecipSamples = romSamples1 (natToNum @H.RecipDefDelay) recipSamples

halfRSqrtSamples :: [(Half, BitVector 16)]
halfRSqrtSamples = romSamples1 (natToNum @H.RSqrtDefDelay) rsqrtSamples

halfRSqrtMinLatencySamples :: [(Half, BitVector 16)]
halfRSqrtMinLatencySamples =
  romSamples1 (natToNum @HalfRSqrtMinLatencyDelay) rsqrtSamples

halfExpSamples :: [(Half, BitVector 16)]
halfExpSamples = romSamples1 (natToNum @H.ExpDefDelay) expSamples

halfLogSamples :: [(Half, BitVector 16)]
halfLogSamples = romSamples1 (natToNum @H.LogDefDelay) logSamples

singleRecipSamples :: [(Float, BitVector 32)]
singleRecipSamples = romSamples1 (natToNum @S.RecipDefDelay) recipSamples

singleRecipNoDspSamples :: [(Float, BitVector 32)]
singleRecipNoDspSamples =
  romSamples1 (natToNum @SingleRecipNoDspDelay) recipSamples

singleRSqrtSamples :: [(Float, BitVector 32)]
singleRSqrtSamples = romSamples1 (natToNum @S.RSqrtDefDelay) rsqrtSamples

singleExpSamples :: [(Float, BitVector 32)]
singleExpSamples = romSamples1 (natToNum @S.ExpDefDelay) expSamples

singleExpBramSamples :: [(Float, BitVector 32)]
singleExpBramSamples = romSamples1 (natToNum @SingleExpBramDelay) expSamples

singleLogSamples :: [(Float, BitVector 32)]
singleLogSamples = romSamples1 (natToNum @S.LogDefDelay) logSamples

doubleRecipSamples :: [(Double, BitVector 64)]
doubleRecipSamples = romSamples1 (natToNum @D.RecipDefDelay) recipSamples

doubleRSqrtSamples :: [(Double, BitVector 64)]
doubleRSqrtSamples = romSamples1 (natToNum @D.RSqrtDefDelay) rsqrtSamples

doubleExpSamples :: [(Double, BitVector 64)]
doubleExpSamples = romSamples1 (natToNum @D.ExpDefDelay) expSamples

doubleExpBramSamples :: [(Double, BitVector 64)]
doubleExpBramSamples = romSamples1 (natToNum @DoubleExpBramDelay) expSamples

doubleLogSamples :: [(Double, BitVector 64)]
doubleLogSamples = romSamples1 (natToNum @D.LogDefDelay) logSamples

doubleLogNoDspSamples :: [(Double, BitVector 64)]
doubleLogNoDspSamples = romSamples1 (natToNum @DoubleLogNoDspDelay) logSamples

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

{- Note [Samples of the reciprocal, reciprocal square root, exponential and logarithm]
Clash simulation gives the correctly rounded results of these operations,
while the IP is only accurate to one unit in the last place (except for the
reciprocal and reciprocal square root in half precision). So their samples only
use inputs for which both give the same result: special cases, exact results,
NaNs, and pseudo-random inputs that were checked offline against the IP's C
model ('checkedFor'), which matches the IP bit for bit. In half precision, the
reciprocal and reciprocal square root also get the other inputs used elsewhere.
-}

-- | Inputs and results of the reciprocal, see
-- Note [Samples of the reciprocal, reciprocal square root, exponential and logarithm]
recipSamples :: forall a. (XilinxFloat a, Show a) => [(a, a)]
recipSamples =
     checked1 "recip" xilinxRecip recipCases
  ++ modelled1 xilinxRecip
       (  nans
       ++ checkedFor @a halfRecipChecked singleRecipChecked doubleRecipChecked
       ++ exactInHalf @a )

-- | Inputs and results of the reciprocal square root, see
-- Note [Samples of the reciprocal, reciprocal square root, exponential and logarithm]
rsqrtSamples :: forall a. (XilinxFloat a, Show a) => [(a, a)]
rsqrtSamples =
     checked1 "rsqrt" xilinxRSqrt rsqrtCases
  ++ modelled1 xilinxRSqrt
       (  nans
       ++ checkedFor @a halfRSqrtChecked singleRSqrtChecked doubleRSqrtChecked
       ++ exactInHalf @a )

-- | Inputs and results of the exponential, see
-- Note [Samples of the reciprocal, reciprocal square root, exponential and logarithm]
expSamples :: forall a. (XilinxFloat a, Show a) => [(a, a)]
expSamples =
     checked1 "exp" xilinxExp expCases
  ++ modelled1 xilinxExp
       (nans ++ checkedFor @a halfExpChecked singleExpChecked doubleExpChecked)

-- | Inputs and results of the logarithm, see
-- Note [Samples of the reciprocal, reciprocal square root, exponential and logarithm]
logSamples :: forall a. (XilinxFloat a, Show a) => [(a, a)]
logSamples =
     checked1 "log" xilinxLog logCases
  ++ modelled1 xilinxLog
       (nans ++ checkedFor @a halfLogChecked singleLogChecked doubleLogChecked)

-- | More inputs for the operations that the IP rounds correctly in half
-- precision: special values and pseudo-random numbers. None in other
-- precisions.
exactInHalf :: forall a. XilinxFloat a => [a]
exactInHalf
  | digitsOf @a == 11 = interesting ++ take 100 (randomBits 30)
  | otherwise = []

-- | The inputs of the given precision, from their bit patterns
checkedFor :: forall a. XilinxFloat a => [Word64] -> [Word64] -> [Word64] -> [a]
checkedFor h s d = map (unpack . fromIntegral) $ case digitsOf @a of
  11 -> h
  24 -> s
  _ -> d

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

recipCases :: forall a. XilinxFloat a => [(a, a)]
recipCases =
  [ (1, 1)
  , (-1, -1)
  , (2, 0.5)
  , (-4, -0.25)
  , (0.5, 2)
  , (minNormal, encodeFloat 1 (1 - eMin @a))
    -- The reciprocal of a zero is an infinity of the same sign
  , (0, infinity)
  , (-0, -infinity)
    -- Subnormal inputs are flushed to zero, keeping their sign
  , (minDenormal, infinity)
  , (-maxDenormal, -infinity)
  , (infinity, 0)
  , (-infinity, -0)
    -- Results below the smallest normal number are flushed to zero
  , (maxFinite, 0)
  , (-maxFinite, -0)
  , (encodeFloat 1 (eMax @a - 1), 0)
  ]

rsqrtCases :: forall a. XilinxFloat a => [(a, a)]
rsqrtCases =
  [ (1, 1)
  , (4, 0.5)
  , (0.25, 2)
  , (16, 0.25)
    -- The exponent of the smallest normal number is even
  , (minNormal, encodeFloat 1 ((1 - eMin @a) `div` 2))
    -- The reciprocal square root of a zero is an infinity of the same sign
  , (0, infinity)
  , (-0, -infinity)
    -- Subnormal inputs are flushed to zero, keeping their sign
  , (minDenormal, infinity)
  , (-maxDenormal, -infinity)
  , (infinity, 0)
    -- Other negative numbers give NaN
  , (-1, ipNaN)
  , (-minNormal, ipNaN)
  , (-infinity, ipNaN)
  ]

expCases :: forall a. XilinxFloat a => [(a, a)]
expCases =
  [ (0, 1)
  , (-0, 1)
    -- Subnormal inputs are flushed to zero
  , (minDenormal, 1)
  , (-maxDenormal, 1)
    -- e^x rounds to one for |x| < 2^-(digits+2)
  , (minNormal, 1)
  , (-minNormal, 1)
  , (encodeFloat 1 (negate (digitsOf @a) - 3), 1)
  , (encodeFloat (-1) (negate (digitsOf @a) - 3), 1)
  , (infinity, infinity)
  , (-infinity, 0)
    -- Overflow, and results below the smallest normal number
  , (maxFinite, infinity)
  , (-maxFinite, 0)
  , (fromIntegral (eMax @a), infinity)
  , (fromIntegral (eMin @a - 3), 0)
  ]

logCases :: forall a. XilinxFloat a => [(a, a)]
logCases =
  [ (1, 0)
    -- The logarithm of a zero of either sign is negative infinity
  , (0, -infinity)
  , (-0, -infinity)
    -- Subnormal inputs are flushed to zero
  , (minDenormal, -infinity)
  , (-maxDenormal, -infinity)
  , (infinity, infinity)
    -- Other negative numbers give NaN
  , (-1, ipNaN)
  , (-minNormal, ipNaN)
  , (-infinity, ipNaN)
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

-- * Inputs checked against the C model

-- These inputs were checked offline: for each of them, the model and the IP's
-- C model (Vivado 2022.1) give the same, finite, non-zero result. They are
-- pseudo-random numbers in the ranges where the results are finite and
-- non-zero, and for the logarithm also numbers close to one.

halfRecipChecked :: [Word64]
halfRecipChecked =
  [ 0x1f0b, 0x91c6, 0x1781, 0x2754, 0x301b, 0xb4f8, 0x4b9f, 0xb7af, 0x8d5a
  , 0x4e97, 0x4dc3, 0x9a6e, 0x9601, 0xbdbc, 0x60f8, 0x2449, 0x2b22, 0x72d0
  , 0xdb11, 0x04cf, 0x2669, 0xbf06, 0x5cf0, 0x4582, 0xaf6c, 0xe89a, 0x2bf5
  , 0xb3aa, 0x65f0, 0x5397, 0xae34, 0x36e6, 0x2f35, 0x8d79, 0xc153, 0x8cb3
  , 0x3454, 0xd380, 0x6d76, 0x501a
  ]

singleRecipChecked :: [Word64]
singleRecipChecked =
  [ 0x1355a4fd, 0x4e1f71dd, 0x4f7fa906, 0x65f27807, 0x9e6e5e4e, 0xc405976e
  , 0x0b2ce6a8, 0xb3ae254c, 0x6f29c346, 0xa3860464, 0x5b73dafa, 0x87eb9c0d
  , 0x1eb3382a, 0x45c32dee, 0x100cc187, 0xd053ed71, 0x8b99367f, 0xd6374c16
  , 0x5ca5e701, 0x29f9c402, 0xba543acf, 0xb15fa4b5, 0x0e318bf0, 0x63a79961
  , 0x881269e2, 0x7b81ae4a, 0x1ea7446c, 0x25b7835e, 0x924afc87, 0x47793b1a
  , 0x07561659, 0xd6456d56, 0xc8873d1a, 0xbf95914b, 0x12768874, 0xd8963cb6
  , 0x5f12a2e3, 0x87aede00, 0x6cb8ec58, 0x040b3963
  ]

doubleRecipChecked :: [Word64]
doubleRecipChecked =
  [ 0x85fabc42ecc90c06, 0xf8046cfa63a59c90, 0x19b583d2364b3c43
  , 0x9d968f738dd1e155, 0x03b427e4879970b2, 0xcdfa693e1ffb629c
  , 0x9594c04096019d01, 0x59610c4a45e5b71d, 0xc63ac61f9f01cd2f
  , 0x86824600e4d64ba4, 0x70107f75eb9c3363, 0x189251f4f7a8b1a0
  , 0x5c02b5159a481b4d, 0xfc5386106b236261, 0xb29c67f9c673594f
  , 0x6420665934834ca8, 0x021de73851adbb16, 0x9be9626b2a1742b4
  , 0x5089fb193d4790b0, 0xb702d4c59709fcda, 0x896159e007be5169
  , 0xb3fbf2efdb587509, 0x30177384d82f6ef6, 0x8682b5a2603f0de1
  , 0x6d8b9a3641773470, 0xdc90088e083ed81d, 0x0a0cccb0ad285813
  , 0xe72a259351dafba3, 0x9821499aeca9bbdb, 0xcddb6ccdf9ad1d85
  , 0x9e274aa3caab68c2, 0xd5c91552d9dd8743, 0x5d82ae96154f23bd
  , 0x618093143d4de8f7, 0x92deb5fac73f1838, 0x14ab1a697a83fa5c
  , 0xf161c930d5446954, 0x357e0eee25eafd6b, 0x66808461478ad855
  , 0xe1f41709f990f053
  ]

halfRSqrtChecked :: [Word64]
halfRSqrtChecked =
  [ 0x25f4, 0x09ff, 0x4cf2, 0x0925, 0x4a5c, 0x60fd, 0x5a52, 0x344b, 0x4c1a
  , 0x5e05, 0x1106, 0x376a, 0x7bb4, 0x6c85, 0x1f0b, 0x4000, 0x42a2, 0x04b1
  , 0x5dea, 0x43a8, 0x5b4b, 0x4c05, 0x4a6b, 0x720d, 0x13c8, 0x5721, 0x0f65
  , 0x7156, 0x787b, 0x589a, 0x7bc0, 0x3f3d, 0x17e0, 0x0b07, 0x5019, 0x05d8
  , 0x1f59, 0x0975, 0x409e, 0x5f05
  ]

singleRSqrtChecked :: [Word64]
singleRSqrtChecked =
  [ 0x610ef2ac, 0x3fe2de03, 0x1aaf13a7, 0x51d722d5, 0x514b49c0, 0x756c48ba
  , 0x36449547, 0x42984224, 0x3839d442, 0x60928ca0, 0x121e69d2, 0x783b9988
  , 0x635d97ec, 0x365edf6a, 0x6bbd0fda, 0x459938ca, 0x618ff237, 0x1498931d
  , 0x33949efe, 0x67453666, 0x02de7b32, 0x3463e9ea, 0x3ebd96b0, 0x0fb628da
  , 0x781ba7ae, 0x7f349561, 0x320922bc, 0x19b99339, 0x42aa72fd, 0x042ab4a3
  , 0x5a81c73b, 0x20a75e15, 0x707943e7, 0x3d0a5822, 0x34c9c379, 0x38724a40
  , 0x3eeb4703, 0x07121cff, 0x0d97b451, 0x1c04c126
  ]

doubleRSqrtChecked :: [Word64]
doubleRSqrtChecked =
  [ 0x54f24580265d2759, 0x65f66e1c510c78c4, 0x58911095b5b4981f
  , 0x3ce699700ce5966f, 0x180f74f0a64330ea, 0x5672ecad56b2f64b
  , 0x0b3e2b5b55f6d8fb, 0x507f5d5dfee66c14, 0x04d53620522774ca
  , 0x724d342c0aeb90b1, 0x358789a1db311446, 0x5daa783aa523024c
  , 0x3c06d0f1b798c8c6, 0x538060a78b0e0d8e, 0x661f3dabb88b8bc1
  , 0x76965432107ccf64, 0x7356b6cc634f066d, 0x6fa42ba1590a0b4d
  , 0x00ddcd0a78d46cdc, 0x77b2beb5e07f8389, 0x185c6015d24eaf06
  , 0x3d222fe32c0b5ec9, 0x304be8948f52e8fe, 0x7f9ade1056f20aa1
  , 0x2a19f002d3207757, 0x7e55cecc739c3eae, 0x36ce11907d3e1fa0
  , 0x3581b6e5433ac4b6, 0x39554e800b2f1d2d, 0x71fc6719f152edbc
  , 0x41409f7b1b4ba653, 0x0f51fc996d4db949, 0x6698ff2277afba70
  , 0x6bfba34b93c5ebc4, 0x3a090f078c04cb14, 0x7454c4fdbd6ccabf
  , 0x32b741d8e48ad83e, 0x6f2b4a9f06df902f, 0x2da2a6dfa25943aa
  , 0x1661e2e0ad719af3
  ]

halfExpChecked :: [Word64]
halfExpChecked =
  [ 0x492f, 0xc20f, 0x483b, 0x45cd, 0xb40f, 0x48a3, 0x33cb, 0x44c1, 0xc595
  , 0x43ba, 0xabd2, 0xc4f9, 0x44a3, 0xc4a3, 0xc8aa, 0x4821, 0x4659, 0x456a
  , 0xc835, 0xc682, 0x4908, 0x4974, 0xc1d7, 0xc4ba, 0x4559, 0xbcbc, 0xc4ca
  , 0xc76d, 0x47b4, 0xc66f, 0xc824, 0xbe00, 0x47ee, 0xb69f, 0x3e6b, 0xc5e5
  , 0x4928, 0x4858, 0xc4cd, 0xbeec
  ]

singleExpChecked :: [Word64]
singleExpChecked =
  [ 0xc2290695, 0x42a71f54, 0xc2883499, 0xc29ce851, 0x42a6a07f, 0x42533dae
  , 0x425d3bb2, 0x40b39ca7, 0x415fe4b4, 0x420ecae5, 0x423fc432, 0x42443efe
  , 0x40f98ee4, 0x428e5b71, 0x4160f2f2, 0xc210b7f8, 0xc20cf68b, 0x41c28ee6
  , 0x429c8650, 0xc243a831, 0x422fe8d9, 0x426a0248, 0xc2a0554b, 0x411bfd7f
  , 0x42a65a95, 0x4207c0ed, 0xc1a3b5d1, 0x4166f1c2, 0xc13d8b42, 0x42177c5a
  , 0xc28594f1, 0xc292d22f, 0xc2997ebb, 0x42937a57, 0xc18fcfd6, 0xc2203623
  , 0xc25b4d24, 0x429fc4f8, 0xc19bd051, 0xc22b258c
  ]

doubleExpChecked :: [Word64]
doubleExpChecked =
  [ 0x40749b4aa310a180, 0x40780ae2d66c8444, 0xc0366a3c6afe7d40
  , 0x4079b5cb79c2e030, 0x4074110d0925a04c, 0x4075e402b67a684c
  , 0x40738aa86c7782c0, 0xc07eab1ac26a0924, 0x4070be371bc7cf1a
  , 0xc055309451649a08, 0x4071e4ba97f060ae, 0x4085c3d614e07b0e
  , 0x407919c9b32ee610, 0xc0820a4c3398d2ad, 0xc0830770a239760e
  , 0x407a14f400195fdc, 0xc07d3ba3374e23c6, 0x407805d1790c8ef8
  , 0x4080340b427dc2f4, 0xc056ee07ab248a08, 0x4073ab2290fa42b0
  , 0xc07208fc32de46cb, 0x4072467f2145f806, 0x408472f3ffff7650
  , 0xc068ccd9adf707ca, 0xc06786d22c86967c, 0xc07c19219d4689ca
  , 0x4080e455c280d31c, 0x407599f0be53de80, 0x407f675174ac43e8
  , 0x4080421af488ee82, 0x4077a27b7ddf26a0, 0xc079368ae8fc4aef
  , 0x408355413998fb54, 0xc0758de6d33b245c, 0x40664d2ba3e5f5a8
  , 0xc07182c33f369567, 0x4059516cb6d87168, 0x408155c6871f15f8
  , 0x405828fed58f2e08
  ]

halfLogChecked :: [Word64]
halfLogChecked =
  [ 0x65a6, 0x1b41, 0x37e7, 0x2f33, 0x5eb2, 0x3d1a, 0x4d0c, 0x6ef2, 0x6170
  , 0x4e85, 0x3133, 0x70f0, 0x4b0e, 0x1fd0, 0x5284, 0x530c, 0x04b8, 0x6811
  , 0x4c25, 0x4d44, 0x2546, 0x362e, 0x3c66, 0x2161, 0x73c6, 0x5140, 0x0517
  , 0x6701, 0x06d4, 0x18b6, 0x2193, 0x43a4, 0x213e, 0x7095, 0x15ea, 0x38fd
  , 0x45e0, 0x2a37, 0x265a, 0x338d
  ]

singleLogChecked :: [Word64]
singleLogChecked =
  [ 0x2fb9ed8a, 0x20e3ad0f, 0x599c2c53, 0x4f93eca6, 0x01e319d4, 0x46f5d0fe
  , 0x0b8f7751, 0x658db860, 0x42ebac95, 0x1e338655, 0x25245331, 0x2a99e7ec
  , 0x48cafcb3, 0x4ac9a098, 0x555e26a5, 0x2a8a779f, 0x3a8b3d7a, 0x62c45652
  , 0x51f8ce8e, 0x2ad5943b, 0x6c5e1352, 0x4b423918, 0x1a5d6d66, 0x3dad8ac3
  , 0x1edaf1be, 0x6b9a03ba, 0x2886fb3b, 0x55f61665, 0x1133e616, 0x5fc66d9e
  , 0x3f800d86, 0x3f61263e, 0x3f7ff801, 0x3f800063, 0x3f798d53, 0x3f7d778d
  , 0x3f416e06, 0x3f80b2bb, 0x3f9d2f7e, 0x3f800002
  ]

doubleLogChecked :: [Word64]
doubleLogChecked =
  [ 0x53225af425093e1c, 0x583074fe2394fac4, 0x4317a6fe19239d5c
  , 0x301413cebfcfa405, 0x2def9298f2b2eccc, 0x52dbf767d32757a7
  , 0x0cdaab35de9a65be, 0x3e07fa04cb67d186, 0x6e2bfd7d0801d969
  , 0x0cfd70698ad6323c, 0x6312530ec38cc44f, 0x2b57d15711fb10ce
  , 0x6de47e572444f45f, 0x3d49e34b0fd573de, 0x68f41a25bc5a358f
  , 0x5e6c099a3b8dd320, 0x5852ea650ec7ef6e, 0x5a79e01487558727
  , 0x05c56970f0e81ba6, 0x0d8c22c51c69e853, 0x1a71d22da0232f90
  , 0x0388c05bce53f555, 0x110a36aa0bf760fc, 0x1485d51cae0923a4
  , 0x3430eae242a85706, 0x17e2bb3fa181eb57, 0x017237470c8222c1
  , 0x13d388d10cdc481e, 0x35d88883e2c6f45b, 0x46473abab9b84e44
  , 0x3ff00012b76b4a43, 0x3fefffffffc9a96d, 0x3ff000003a66b7b8
  , 0x3ff0000006740c1e, 0x3fed6eca36703b91, 0x3ff01a2f2d0b0fab
  , 0x3feffffffffff202, 0x3feffffffff5c1d4, 0x3ff0000000037205
  , 0x3fefffffffffa5ae
  ]
