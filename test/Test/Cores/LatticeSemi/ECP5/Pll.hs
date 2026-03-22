{-|
  Copyright   :  (C) 2026, Felix Klein
  License     :  CERN-OHL-P-2.0
  Maintainer  :  felix@qbaylogic.com

Property based tests for Clash.Cores.LatticeSemi.ECP5.Pll. Note that
this test suite requires the ecppll tool from project Trellis to be in
the PATH.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.LatticeSemi.ECP5.Pll (tests) where

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

import Clash.Cores.LatticeSemi.ECP5.Internal.Pll

admissibleFloatErr :: Float
admissibleFloatErr = 0.001

tests :: TestTree
tests = testProperty "Test.Cores.LatticeSemi.ECP5.Pll" $ property $ do
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
  let fpfd = toRational iFreq / toRational params.inputDiv
  assert $ fpfd >= minima.pfdFreq
  assert $ fpfd <= maxima.pfdFreq

  -- the VCO frequency is within range
  assert $ params.vcoFreq >= minima.vcoFreq
  assert $ params.vcoFreq <= maxima.vcoFreq

  -- output frequency = input frequency * (feedback divider / input divider)
  params.outputFreq
    === (toInteger iFreq * params.feedbackDiv) % params.inputDiv
  let oFreqCalc = (toInteger iFreq * golden.feedbackDiv) % golden.inputDiv
  assert $ abs (golden.outputFreq - fromRational oFreqCalc) < admissibleFloatErr

  -- VCO frequency = output frequency * output divider
  params.vcoFreq === params.outputFreq * toRational params.outputDiv
  let vcoFreqCalc = golden.outputFreq * fromInteger golden.outputDiv
  assert $ abs (golden.vcoFreq - vcoFreqCalc) < 10 * admissibleFloatErr
 where
  minima = minBound :: ECP5Limits
  maxima = maxBound :: ECP5Limits

getGoldenParams :: Float -> Float -> IO (Maybe (PllParams Float))
getGoldenParams iFreq oFreq = do
  (_, out, _, pid) <- runInteractiveProcess
    "ecppll" [ "-i", show iFreq, "-o", show oFreq ] Nothing Nothing
  waitForProcess pid >>= \case
    ExitFailure{} -> return Nothing
    ExitSuccess   -> hGetContents out >>= \str -> return $ do
      [_, l0, l1, l2, l3, l4] <- return $ lines str
      [_, _, w0] <- return $ words l0
      [_, _, w1] <- return $ words l1
      [_, _, w2] <- return $ words l2
      [_, _, w3, _] <- return $ words l3
      [_, _, w4] <- return $ words l4
      inputDiv <- readMaybe w0
      feedbackDiv <- readMaybe w1
      outputDiv <- readMaybe w2
      outputFreq <- readMaybe w3
      vcoFreq <- readMaybe w4
      return PllParams{..}
