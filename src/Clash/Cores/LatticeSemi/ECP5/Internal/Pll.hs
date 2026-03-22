{-|
  Copyright   :  (C) 2026, Felix Klein
  License     :  CERN-OHL-P-2.0
  Maintainer  :  felix@qbaylogic.com

The parameter limits and a configuration calculator for the tunable
PLL parameters of the ECP5 FPGAs.

The following relations are given according to the LatticeECP/EC and
LatticeXP sysCLOCK PLL Design and Usage Guide:

* output frequency = input frequency * (feedback divider / input divider)
* voltage controlled oscillator frequency = output frequency * output divider
* phase frequency detector frequency = input frequency / input divider
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.LatticeSemi.ECP5.Internal.Pll where

import Prelude

import Data.Ratio ((%))

-- | The limits according to the following Lattice data sheets:
--
--  * FPGA-DS-02012-3.4 ECP5 and ECP-5G Family
--  * FPGA Libraries Reference Guide
--
data ECP5Limits = ECP5Limits
  { -- | input frequency (MHz)
    inputFreq :: Rational
  , -- | output frequency (MHz)
    outputFreq :: Rational
  , -- | phase frequency detector frequency (Mhz)
    pfdFreq :: Rational
  , -- | voltage controlled oscillator frequency (MHz)
    vcoFreq :: Rational
  , -- | input divider
    inputDiv :: Integer
  , -- | output divider
    outputDiv :: Integer
  , -- | feedback divider
    feedbackDiv :: Integer
  }

instance Bounded ECP5Limits where
  minBound = ECP5Limits
    { inputFreq   = 8
    , outputFreq  = 10
    , pfdFreq     = 25 % 8
    , vcoFreq     = 400
    , inputDiv    = 1
    , outputDiv   = 1
    , feedbackDiv = 1
    }
  maxBound = ECP5Limits
    { inputFreq   = 400
    , outputFreq  = 400
    , pfdFreq     = 400
    , vcoFreq     = 800
    , inputDiv    = 128
    , outputDiv   = 128
    , feedbackDiv = 80
    }

-- | The tunable PLL parameters.
data PllParams a = PllParams
  { -- | input divider
    inputDiv :: Integer
  , -- | feedback divider
    feedbackDiv :: Integer
  , -- | output divider
    outputDiv :: Integer
  , -- | voltage controlled oscillator frequency (MHz)
    vcoFreq :: a
  , -- | output frequency (MHz)
    outputFreq :: a
  } deriving (Show, Eq)

-- | Calculates optimized PLL parameters from the given input and
-- output frequencies. The calculation is inspired by
-- https://github.com/YosysHQ/prjtrellis/blob/main/libtrellis/tools/ecppll.cpp,
-- but has been slightly improved through the usage of the error-free
-- 'Rational' type instead of 'Float'.
calcPllParams :: Rational -> Rational -> Either String (PllParams Rational)
calcPllParams iFreq oFreq = do
  guard (iFreq >= minima.inputFreq)
    "The given input frequency exceeds the supported minimum."
  guard (iFreq <= maxima.inputFreq)
    "The given input frequency exceeds the supported maximum."
  guard (oFreq >= minima.outputFreq)
    "The targeted output frequency exceeds the supported minimum."
  guard (oFreq <= maxima.outputFreq)
    "The targeted output frequency exceeds the supported maximum."
  snd <$> foldl optimize (Left "Could not find a suitable PLL parameter set.")
    [ PllParams{..}
    | inputDiv <- [minima.inputDiv .. maxInputDiv]
    , let fpfd = iFreq / fromInteger inputDiv
    , fpfd >= minima.pfdFreq
    , fpfd <= maxima.pfdFreq
    , feedbackDiv <- [minima.feedbackDiv .. maxFbDiv fpfd]
    , outputDiv <- [minima.outputDiv .. maxOutputDiv fpfd]
    , let vcoFreq = fpfd * fromInteger (feedbackDiv * outputDiv)
    , vcoFreq >= minima.vcoFreq
    , vcoFreq <= maxima.vcoFreq
    , let outputFreq = vcoFreq / fromInteger outputDiv
    ]
 where
  minima = minBound :: ECP5Limits
  maxima = maxBound :: ECP5Limits

  maxInputDiv = min maxima.inputDiv (floor (iFreq / minima.pfdFreq))
  maxFbDiv fpfd = min maxima.feedbackDiv (floor (maxima.vcoFreq / fpfd))
  maxOutputDiv fpfd = min maxima.outputDiv (floor (maxima.vcoFreq / fpfd))

  optimize ma newParams = do
    let newErr = abs $ newParams.outputFreq - oFreq
    return $ case ma of
      Right (oldErr, oldParams)
        | newErr > oldErr                 -> (oldErr, oldParams)
        | newErr < oldErr                 -> (newErr, newParams)
        | vcod newParams < vcod oldParams -> (newErr, newParams)
        | otherwise                       -> (oldErr, oldParams)
      Left _                              -> (newErr, newParams)

  guard cond errMsg = if cond then Right () else Left errMsg

-- | Calculates the distance to the targeted VCO frequency.
vcod :: Fractional a => PllParams a -> a
vcod x = abs $ x.vcoFreq - fromRational targetVcoFreq
 where
  targetVcoFreq = minima.vcoFreq + (maxima.vcoFreq - minima.vcoFreq) / 2
  minima = minBound :: ECP5Limits
  maxima = maxBound :: ECP5Limits
