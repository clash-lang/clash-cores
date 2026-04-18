{-|
  Copyright   :  (C) 2026, Felix Klein
  License     :  CERN-OHL-P-2.0
  Maintainer  :  felix@qbaylogic.com

Property based tests for Clash.Cores.LatticeSemi.ICE40.Pll. Note that
this test suite requires the icepll tool from project icestorm to be in
the PATH.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.LatticeSemi.ICE40.Pll (tests) where

import Prelude

import Control.Monad.IO.Class (liftIO)
import Data.Ratio ((%))
import Hedgehog ((===), assert, forAll, property)
import Hedgehog.Gen (int)
import Hedgehog.Range (linear)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Text.Read (readMaybe)
import System.Exit (ExitCode(..))
import System.IO (hGetContents)
import System.Process (runInteractiveProcess, waitForProcess)

import Clash.Cores.LatticeSemi.ICE40.Internal.Pll

admissibleFloatErr :: Float
admissibleFloatErr = 0.001

tests :: TestTree
tests = testProperty "Test.Cores.LatticeSemi.ICE40.Pll" $ property $ do
  let forAllFreq x y = forAll $ int $ linear (fromEnum x) (fromEnum y)
  iFreq <- forAllFreq minima.inputFreq maxima.inputFreq
  oFreq <- forAllFreq minima.outputFreq maxima.outputFreq

  Just golden <- liftIO $ getGoldenParams (toEnum iFreq) (toEnum oFreq)
  Right params <- return $ calcPllParams (toRational iFreq) (toRational oFreq)

  -- the output frequency is as good as the golden reference
  let errParams = abs (toRational oFreq - params.outputFreq)
      errGolden = abs (toEnum oFreq - golden.outputFreq)
  assert $ fromRational errParams <= errGolden + admissibleFloatErr

  -- the VCO frequency is at least as close to the targeted frequency
  -- as the golden reference
  assert $ fromRational (vcod params) <= vcod golden + admissibleFloatErr

  -- the PFD frequency is within range
  let fpfd = toRational iFreq / toRational (params.refClkDiv + 1)
  assert $ fpfd >= minima.pfdFreq
  assert $ fpfd <= maxima.pfdFreq

  -- the VCO frequency is within range
  assert $ params.vcoFreq >= minima.vcoFreq
  assert $ params.vcoFreq <= maxima.vcoFreq

  -- output frequency = (input frequency * (feedback divider + 1)
  --   / (2 ^ (VCO divider) * (reference clock divider + 1))
  params.outputFreq
    === (toInteger iFreq * (params.feedbackDiv + 1))
          % ((params.refClkDiv + 1) * 2 ^ params.vcoDiv)
  let oFreqCalc = (toInteger iFreq * (golden.feedbackDiv + 1))
                    % ((golden.refClkDiv + 1) * 2 ^ golden.vcoDiv)
  assert $ abs (golden.outputFreq - fromRational oFreqCalc) < admissibleFloatErr
 where
  minima = minBound :: ICE40Limits
  maxima = maxBound :: ICE40Limits

getGoldenParams :: Float -> Float -> IO (Maybe (PllParams Float))
getGoldenParams iFreq oFreq = do
  (_, out, _, pid) <- runInteractiveProcess
    "icepll" [ "-i", show iFreq, "-o", show oFreq ] Nothing Nothing
  waitForProcess pid >>= \case
    ExitFailure{} -> return Nothing
    ExitSuccess   -> hGetContents out >>= \str -> return $ do
      [_, _, l0, _, _, l1, l2, l3, l4, l5] <- return
        $ filter (not . null) $ lines str
      [_, w0, _, _] <- return $ words l0
      [_, w1, _] <- return $ words l1
      [_, w2, _] <- return $ words l2
      [_, w3, _] <- return $ words l3
      [_, w4, _] <- return $ words l4
      [_, w5, _] <- return $ words l5
      outputFreq <- readMaybe w0
      vcoFreq <- readMaybe w1
      refClkDiv <- readMaybe w2
      feedbackDiv <- readMaybe w3
      vcoDiv <- readMaybe w4
      filterRange <- readMaybe w5
      return PllParams{..}
