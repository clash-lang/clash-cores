{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.,
                  2022     , Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Floating.TH where

import Clash.Prelude (natToNum, Signed, Unsigned)

import Prelude
import Numeric.IEEE
  (epsilon, infinity, maxFinite, minDenormal, minNormal)

import Clash.Cores.Xilinx.Floating as F
import Clash.Cores.Xilinx.Floating.Internal as F

import Floating.GenericTH (delayOutput, delayOutput1)

-- | For @compareEnableTB@. Obtained by running an RNG.
compareFloatsEnableInputA :: [Float]
compareFloatsEnableInputA =
  [ 63, 73, 85, 68, 14, 36, 52, 38, 97, 60, 80, 10, 94, 58, 47, 59, 70, 9, 64
  , 79, 5, 49, 88, 93, 43, 90, 99, 56, 98, 12, 11, 20, 100, 57, 37, 33, 74, 83
  , 19, 84, 95, 53, 34, 89, 75, 55, 76, 44, 18, 28
  ]

-- | For @compareEnableTB@. Obtained by running an RNG.
compareFloatsEnableInputB :: [Float]
compareFloatsEnableInputB =
  [ 74, 62, 11, 12, 41, 10, 76, 90, 26, 93, 43, 29, 33, 79, 77, 80, 57, 70, 22
  , 19, 14, 8, 37, 2, 85, 89, 36, 86, 91, 17, 53, 4, 25, 97, 72, 24, 50, 9, 99
  , 95, 65, 30, 63, 7, 92, 15, 28, 82, 87, 84
  ]

-- | For @compareEnableTB@.
compareFloatsEnableInput :: [Bool]
compareFloatsEnableInput = map (<= 50) compareFloatsEnableInputA

-- | For @compareEnableTB@. Obtained by sampling Haskell model.
compareFloatsEnableExpected :: [F.Ordering]
compareFloatsEnableExpected =
  [ F.EQ, F.EQ, F.EQ, F.EQ, F.EQ, F.EQ -- First samples undefined, replaced by EQ in TB
  , F.LT, F.LT, F.GT, F.GT, F.GT, F.GT, F.LT, F.LT, F.LT, F.LT, F.LT, F.LT, F.LT
  , F.LT, F.LT, F.LT, F.LT, F.LT, F.LT, F.GT, F.GT, F.GT, F.GT, F.GT, F.LT, F.LT
  , F.LT, F.LT, F.LT, F.GT, F.LT, F.LT, F.LT, F.GT, F.GT, F.GT, F.GT, F.LT, F.LT
  , F.LT, F.LT, F.LT, F.LT, F.LT
  ]

compareBasicSamples :: [(Float, Float, F.Ordering)]
compareBasicSamples =
    delayOutput (natToNum @F.CompareDefDelay) F.NaN
  $ [ (1.0,         2.0,         F.LT)
    , (2.0,         1.0,         F.GT)
    , (1.0,         1.0,         F.EQ)
    , (F.xilinxNaN, 1.0,         F.NaN)
    , (1.0,         F.xilinxNaN, F.NaN)
    ] ++ cartesianProductTest xilinxCompare interesting interesting

cartesianProductTest
  :: (a -> b -> c)
  -> [a]
  -> [b]
  -> [(a,b,c)]
cartesianProductTest f as bs =
  map (\(a,b) -> (a, b, f a b)) $ cartesianProduct as bs

cartesianProduct
  :: [a]
  -> [b]
  -> [(a,b)]
cartesianProduct as bs =
  concatMap (\a -> map (\b -> (a,b)) bs) as

interesting :: [Float]
interesting =
  [ infinity
  , minDenormal
  , maxDenormal
  , minNormal
  , maxFinite
  , epsilon
  , F.xilinxNaN
    -- Some basic numbers
  , 0
  , 1
  , 2
  , 4
  , 42
  ]

fromUBasicSamples :: [(Unsigned 32, Float)]
fromUBasicSamples = delayOutput1 (natToNum @F.FromU32DefDelay) 0 $
  map (\x -> (x, fromIntegral x))
    [ 0
    , 1
    , maxBound

      -- Patterns 0xaa and 0x55, but treating the "sign bit" separately. Floats
      -- are stored in sign/magnitude form so signed and unsigned numbers are
      -- not interchangeable like with two's complement. All these numbers
      -- should be unsigned, verify that they are treated as such.
    , 0b0010_1010_1010_1010_1010_1010_1010_1010
    , 0b0101_0101_0101_0101_0101_0101_0101_0101
    , 0b1010_1010_1010_1010_1010_1010_1010_1010
    , 0b1101_0101_0101_0101_0101_0101_0101_0101

    , -- Longest exactly representable
      0b0000_0000_1111_1111_1111_1111_1111_1111

    , -- Smallest with rounding
      0b0000_0001_0000_0000_0000_0000_0000_0001

      -- More rounding tests
    , 0b0000_0001_0000_0000_0000_0000_0000_0011
    , 0b0000_0010_0000_0000_0000_0000_0000_0001
    ]

fromSBasicSamples :: [(Signed 32, Float)]
fromSBasicSamples = delayOutput1 (natToNum @F.FromS32DefDelay) 0 $
  map (\x -> (x, fromIntegral x)) (specials ++ map (* (-1)) specials)

 where
  specials =
    [ 0
    , 1
    , minBound
    , maxBound

      -- Patterns 0xaa and 0x55, but treating the "sign bit" separately. Floats
      -- are stored in sign/magnitude form so signed and unsigned numbers are
      -- not interchangeable like with two's complement.
    , 0b0010_1010_1010_1010_1010_1010_1010_1010
    , 0b0101_0101_0101_0101_0101_0101_0101_0101
    , 0b1010_1010_1010_1010_1010_1010_1010_1010
    , 0b1101_0101_0101_0101_0101_0101_0101_0101

    , -- Longest exactly representable
      0b0000_0000_1111_1111_1111_1111_1111_1111

    , -- Smallest with rounding
      0b0000_0001_0000_0000_0000_0000_0000_0001

      -- More rounding tests
    , 0b0000_0001_0000_0000_0000_0000_0000_0011
    , 0b0000_0010_0000_0000_0000_0000_0000_0001
    ]

-- Maximum subnormal value
maxDenormal :: Float
maxDenormal = minNormal - minDenormal
