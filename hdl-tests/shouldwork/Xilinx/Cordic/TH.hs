{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Configurations and test samples for the HDL tests of
"Clash.Cores.Xilinx.Cordic". The expected outputs are computed with the
simulation model in "Clash.Cores.Xilinx.Cordic.Internal".
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cordic.TH where

import Prelude

import Data.Bits (shiftL, shiftR, xor)
import Data.List (unfoldr)
import Data.Word (Word64)

import Clash.Explicit.Prelude
  ( KnownNat, SFixed, UFixed, Unsigned, SNat (..), System, clockGen, hasUndefined, sampleN
  , sf, toEnable, uf, fromList )
import qualified Clash.Signal.Delayed as D

import qualified Clash.Cores.Xilinx.Cordic.Explicit as E
import Clash.Annotations.TopEntity (PortName (..), TopEntity (..))

import Clash.Cores.Xilinx.Cordic.Internal

-- | Annotation for a top entity with a clock and one other input
unaryTopAnn :: String -> TopEntity
unaryTopAnn name =
  Synthesize
    { t_name = name
    , t_inputs = [PortName "clk", PortName "x"]
    , t_output = PortName "result"
    }

------------------------------------------------------------------------------
-- Configurations under test
------------------------------------------------------------------------------

-- | Rotation with compensation scaling and rounding, 16 bit inputs and 12 bit
-- outputs
rotateConfig :: Config
rotateConfig = defConfig{compensationScaling = LutBased, roundMode = RoundNearestEven}

-- | Translation in scaled radians, with block RAM compensation scaling,
-- 12 bit inputs and 20 bit outputs
translateConfig :: Config
translateConfig =
  defConfig
    { compensationScaling = BramBased
    , phaseFormat = ScaledRadians
    , roundMode = RoundPosNegInf
    }

-- | Arctangent without coarse rotation, 20 bit inputs and 16 bit outputs
arcTanConfig :: Config
arcTanConfig = defConfig{coarseRotation = False, roundMode = RoundPosInf}

-- | Hyperbolic arctangent with an explicit number of iterations and precision
arcTanhConfig :: Config
arcTanhConfig = defConfig{iterations = Just 20, precision = Just 30}

-- | Square root, rounding to nearest even
sqrtConfig :: Config
sqrtConfig = defConfig{roundMode = RoundNearestEven}

------------------------------------------------------------------------------
-- Inputs
------------------------------------------------------------------------------

-- | Pseudo random numbers (xorshift64)
randoms :: Word64 -> [Integer]
randoms = unfoldr (\s -> let s' = step s in Just (toInteger s', s'))
 where
  step x0 =
    let x1 = x0 `xor` (x0 `shiftL` 13)
        x2 = x1 `xor` (x1 `shiftR` 7)
    in x2 `xor` (x2 `shiftL` 17)

-- | Pseudo random integers in @[lo, hi]@
randomRange :: Word64 -> Integer -> Integer -> [Integer]
randomRange seed lo hi = [ lo + r `mod` (hi - lo + 1) | r <- randoms seed ]

-- | Number of random samples per test
nRandom :: Int
nRandom = 300

-- | Phases in @[-pi, pi]@ (or @[-1, 1]@ for scaled radians), with @f@
-- fractional bits: edge cases and random values
phases :: Bool -> Bool -> Int -> Word64 -> [Integer]
phases scaledRadians full f seed = edges <> take nRandom (randomRange seed (negate hi) hi)
 where
  unit = if scaledRadians then 1 else pi :: Double
  hi = floor ((if full then unit else unit / 4) * 2 ^^ f)
  q c = floor (c * unit / 4 * 2 ^^ f)
  edges =
    [0, 1, -1, hi, negate hi]
      <> concat [ [q c + d, negate (q c) + d] | c <- [1, 2, 3], full || c < 2, d <- [-1, 0, 1] ]

-- | Vectors with X and Y in @[-1, 1]@, with @f@ fractional bits. Includes
-- vectors whose rotation overflows the output.
vectors :: Int -> Word64 -> [(Integer, Integer)]
vectors f seed = edges <> take nRandom (zip (randomRange seed (-one) one) (randomRange (seed + 1) (-one) one))
 where
  one = 1 `shiftL` f
  edges =
    [ (x, y) | x <- [0, one, negate one, one `div` 2], y <- [0, one, negate one, 1] ]
      <> [ (a, a) | a <- [one, negate one, 3] ] <> [ (a, negate a) | a <- [one, 5] ]

-- | Vectors with an angle in @[-pi/4, pi/4]@
firstQuadrant :: Int -> Word64 -> [(Integer, Integer)]
firstQuadrant f seed =
  [ (x, y) | (x0, y0) <- vectors f seed, let x = abs x0, let y = max (negate x) (min x y0) ]

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

toSF :: forall int frac. (KnownNat int, KnownNat frac) => Integer -> SFixed int frac
toSF = sf (SNat @frac) . fromInteger

toUF :: forall int frac. (KnownNat int, KnownNat frac) => Integer -> UFixed int frac
toUF = uf (SNat @frac) . fromInteger

-- | Delay the expected outputs by @d@ cycles, replacing the first @d@ by a
-- default value. The inputs are extended by repeating the last one.
delayOutput :: Int -> b -> [(a, b)] -> [(a, b)]
delayOutput d bDef es = zip (as <> replicate d (last as)) (replicate d bDef <> bs)
 where
  (as, bs) = unzip es

------------------------------------------------------------------------------
-- Samples
------------------------------------------------------------------------------

type XY f = (SFixed 2 f, SFixed 2 f)

sinCosSamples :: [(SFixed 3 13, XY 14)]
sinCosSamples =
  delayOutput (latency params) (0, 0)
    [ (toSF p, (toSF c, toSF s))
    | p <- phases False True 13 1
    , let (c, s) = modelSinCos params p
    ]
 where
  params = mkParams SinCos UnsignedFraction defConfig 16 16

-- | Input samples for the enable test: 0.1, 0.2, ...
sinCosEnableInputs :: [SFixed 3 13]
sinCosEnableInputs = [ toSF (k * 819) | k <- [1 .. 30] ]

rotateSamples :: [((XY 14, SFixed 3 13), XY 10)]
rotateSamples =
  delayOutput (latency params) (0, 0)
    [ ((xy, toSF p), (toSF x', toSF y'))
    | ((x, y), p) <- zip (vectors 14 2) (phases False True 13 3)
    , let xy = (toSF x, toSF y)
    , let (x', y') = modelRotate params (x, y) p
    ]
 where
  params = mkParams Rotate UnsignedFraction rotateConfig 16 12

-- | Rotation with the default configuration. The CORDIC scale factor makes
-- the outputs of many of these overflow.
rotateBasicSamples :: [((XY 14, SFixed 3 13), XY 14)]
rotateBasicSamples =
  delayOutput (latency params) (0, 0)
    [ ((xy, toSF p), (toSF x', toSF y'))
    | ((x, y), p) <- zip (vectors 14 10) (phases False True 13 11)
    , let xy = (toSF x, toSF y)
    , let (x', y') = modelRotate params (x, y) p
    ]
 where
  params = mkParams Rotate UnsignedFraction defConfig 16 16

translateSamples :: [(XY 10, (SFixed 2 18, SFixed 3 17))]
translateSamples =
  delayOutput (latency params) (0, 0)
    [ ((toSF x, toSF y), (toSF m, toSF p))
    | (x, y) <- vectors 10 4
    , let (m, p) = modelTranslate params (x, y)
    ]
 where
  params = mkParams Translate UnsignedFraction translateConfig 12 20

arcTanSamples :: [(XY 18, SFixed 3 13)]
arcTanSamples =
  delayOutput (latency params) 0
    [ ((toSF x, toSF y), toSF (modelArcTan params (x, y)))
    | (x, y) <- firstQuadrant 18 5
    ]
 where
  params = mkParams ArcTan UnsignedFraction arcTanConfig 20 16

sinhCoshSamples :: [(SFixed 3 15, XY 22)]
sinhCoshSamples =
  delayOutput (latency params) (0, 0)
    [ (toSF p, (toSF c, toSF s))
    | p <- phases False False 15 6
    , let (c, s) = modelSinhCosh params p
    ]
 where
  params = mkParams SinhCosh UnsignedFraction defConfig 18 24

arcTanhSamples :: [(XY 14, SFixed 3 13)]
arcTanhSamples =
  delayOutput (latency params) 0
    [ ((toSF x, toSF y), toSF (modelArcTanh params (x, y)))
    | (x0, y0) <- vectors 14 7
    , let x = max 1 (2 * abs x0 - 1)
    , let y = max (negate ((x * 4) `div` 5)) (min ((x * 4) `div` 5) y0)
    ]
 where
  params = mkParams ArcTanh UnsignedFraction arcTanhConfig 16 16

-- | Square roots, including those of numbers of 1.0 and more, for which
-- Xilinx's C model gives wrong results.
sqrtSamples :: [(UFixed 1 15, UFixed 1 13)]
sqrtSamples =
  delayOutput (latency params) 0
    [ (toUF x, toUF (modelSqrt params x))
    | x <- [0 .. 40] <> [ (1 `shiftL` 15) + d | d <- [-3 .. 3] ] <> [ (1 `shiftL` 16) - d | d <- [1 .. 4] ]
           <> take nRandom (randomRange 8 0 ((1 `shiftL` 16) - 1))
    ]
 where
  params = mkParams SquareRoot UnsignedFraction sqrtConfig 16 14

-- | Square roots of 17 bit integers, including those of @2^16@ and more, for
-- which Xilinx's C model gives wrong results.
sqrtUnsignedSamples :: [(Unsigned 17, Unsigned 9)]
sqrtUnsignedSamples =
  delayOutput (latency params) 0
    [ (fromInteger x, fromInteger (modelSqrt params x))
    | x <- [0 .. 40] <> [ (1 `shiftL` 16) + d | d <- [-3 .. 3] ] <> [ (1 `shiftL` 17) - d | d <- [1 .. 4] ]
           <> [ k * k + d | k <- [255, 256, 300, 362], d <- [-1, 0, 1] ]
           <> take nRandom (randomRange 9 0 ((1 `shiftL` 17) - 1))
    ]
 where
  params = mkParams SquareRoot UnsignedInteger defConfig 17 9

-- | Enable signal for the enable test
sinCosEnablePattern :: [Bool]
sinCosEnablePattern = replicate 22 True <> take 40 (cycle [True, False, False, True, True, False]) <> repeat True

-- | Inputs and expected outputs of the enable test, obtained by simulating
-- 'E.sinCos'. Outputs that are still undefined are marked invalid.
sinCosEnableSamples :: [((Bool, SFixed 3 13), (Bool, XY 14))]
sinCosEnableSamples = zip (zip ens ins) (map check outs)
 where
  n = 100
  ens = take n sinCosEnablePattern
  ins = take n (sinCosEnableInputs <> repeat (last sinCosEnableInputs))
  outs =
    sampleN n $ D.toSignal $
      E.sinCos @System @0 @13 @14 clockGen (toEnable (fromList (ens <> repeat True)))
        (D.fromSignal (fromList (ins <> repeat (last ins))))
  check o
    | hasUndefined o = (False, (0, 0))
    | otherwise = (True, o)
