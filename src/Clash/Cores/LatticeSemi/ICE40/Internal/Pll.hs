{-|
  Copyright   :  (C) 2026, Felix Klein
  License     :  CERN-OHL-P-2.0
  Maintainer  :  felix@qbaylogic.com

The parameter limits and a configuration calculator for the tunable
PLL parameters of the ICE40 FPGAs. This module currently only supports
the FEEDBACK_PATH = SIMPLE option.

The following relations are given according to the iCE40 sysCLOCK PLL
Design and User Guide:

* output frequency = (input frequency * (feedback divider + 1)
    / (2 ^ (VCO divider) * (reference clock divider + 1))
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.LatticeSemi.ICE40.Internal.Pll where

import Prelude

-- | The limits according to the following Lattice data sheets:
--
--  * FPGA-DS-02027-1.9 iCE40 UltraLite Family Data Sheet
--  * FPGA-TN-02052-1.4 iCE40 sysCLOCK PLL Design and User Guide
--
data ICE40Limits = ICE40Limits
  { -- | input frequency (MHz)
    inputFreq :: Rational
  , -- | output frequency (MHz)
    outputFreq :: Rational
  , -- | phase frequency detector frequency (MHz)
    pfdFreq :: Rational
  , -- | voltage controlled oscillator frequency (MHz)
    vcoFreq :: Rational
  , -- | reference clock divider
    refClkDiv :: Integer
  , -- | feedback divider
    feedbackDiv :: Integer
  , -- | voltage controlled oscillator divider
    vcoDiv :: Integer
  }

instance Bounded ICE40Limits where
  minBound = ICE40Limits
    { inputFreq   = 10
    , outputFreq  = 16
    , pfdFreq     = 10
    , vcoFreq     = 533
    , refClkDiv   = 0
    , feedbackDiv = 0
    , vcoDiv      = 0
    }
  maxBound = ICE40Limits
    { inputFreq   = 133
    , outputFreq  = 275
    , pfdFreq     = 133
    , vcoFreq     = 1066
    , refClkDiv   = 15
    , feedbackDiv = 127
    , vcoDiv      = 7
    }

-- | The tunable PLL parameters.
data PllParams a = PllParams
  { -- | reference clock divider
    refClkDiv :: Integer
  , -- | feedback divider
    feedbackDiv :: Integer
  , -- | voltage controlled oscillator divider
    vcoDiv :: Integer
  , -- | PLL filter range
    filterRange :: Integer
  , -- | voltage controlled oscillator frequency (MHz)
    vcoFreq :: a
  , -- | output frequency (MHz)
    outputFreq :: a
  } deriving (Show, Eq)

-- | Calculates optimized PLL parameters from the given input and
-- output frequencies. The calculation is inspired by
-- https://github.com/YosysHQ/icestorm/blob/main/icepll/icepll.cc, but
-- has been slightly improved through the usage of the error-free
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
    | refClkDiv <- [minima.refClkDiv .. maxRefClkDiv]
    , let fpfd = iFreq / fromInteger (refClkDiv + 1)
    , fpfd >= minima.pfdFreq
    , fpfd <= maxima.pfdFreq
    , feedbackDiv <- [minima.feedbackDiv .. maxFbDiv fpfd]
    , let vcoFreq = fpfd * fromInteger (feedbackDiv + 1)
    , vcoFreq >= minima.vcoFreq
    , vcoFreq <= maxima.vcoFreq
    , vcoDiv <- [minima.vcoDiv .. maxima.vcoDiv]
    , let outputFreq = vcoFreq / (2 ^ vcoDiv)
    , -- the same calculation as used by icepll
      let filterRange
            | fpfd < 17  = 1
            | fpfd < 26  = 2
            | fpfd < 44  = 3
            | fpfd < 66  = 4
            | fpfd < 101 = 5
            | otherwise  = 6
    ]
 where
  minima = minBound :: ICE40Limits
  maxima = maxBound :: ICE40Limits

  maxRefClkDiv :: Integer
  maxRefClkDiv = min maxima.refClkDiv (floor (iFreq / minima.pfdFreq) - 1)
  maxFbDiv fpfd = min maxima.feedbackDiv (floor (maxima.vcoFreq / fpfd) - 1)

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
  minima = minBound :: ICE40Limits
  maxima = maxBound :: ICE40Limits
