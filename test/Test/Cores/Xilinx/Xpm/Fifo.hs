{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Properties of the simulation models of the XPM FIFOs. Cycle accuracy with
-- respect to the XPMs is tested by the HDL tests; these tests check that the
-- models behave like a FIFO, for many random stimuli.
module Test.Cores.Xilinx.Xpm.Fifo (tests) where

import Clash.Explicit.Prelude

import qualified Prelude as P

import Data.Maybe (isJust)

import Clash.Cores.Xilinx.Xpm.Fifo
import Clash.Cores.Xilinx.Xpm.Fifo.Internal (advFeaturesParam)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)

createDomain vXilinxSystem{vName="F7", vPeriod=7_000}
createDomain vXilinxSystem{vName="F10", vPeriod=10_000}
createDomain vXilinxSystem{vName="F20", vPeriod=20_000}
createDomain vXilinxSystem{vName="F23", vPeriod=23_000}

type Dat = Unsigned 8

-- | Random requests, starting after the FIFO's power-on reset. Writes before
-- and during the power-on reset are lost, which is tested by the HDL tests.
genRequests :: Gen a -> Gen [Maybe a]
genRequests genA = do
  n <- Gen.int (Range.linear 0 1500)
  p <- Gen.int (Range.linear 1 10)
  reqs <- Gen.list (Range.singleton n) $
    Gen.frequency [(p, Just <$> genA), (10 - p, pure Nothing)]
  pure (P.replicate 100 Nothing <> reqs)

genReadMode :: Gen ReadMode
genReadMode = Gen.element [Std 1, Std 2, Std 3, Fwft]

-- | Words written (accepted by the FIFO) and words read. Requests stop after
-- the generated ones, after which the read side reads until the FIFO is
-- empty.
data Transfers = Transfers
  { written :: [Dat]
  , readOut :: [Dat]
  } deriving (Show, Eq)

-- | Words accepted by a FIFO, according to its outputs
acceptedWrites :: [Maybe Dat] -> [Bool] -> [Bool] -> [Dat]
acceptedWrites wrs fulls busys =
  [ d | (Just d, full, busy) <- P.zip3 wrs fulls busys, not full, not busy ]

-- | Words read from a FIFO, according to its outputs
readWords :: ReadMode -> [Bool] -> [Bool] -> [Dat] -> [Dat]
readWords rm rds valids douts = case rm of
  Std _ -> [ d | (True, d) <- P.zip valids douts ]
  Fwft -> [ d | (True, True, d) <- P.zip3 rds valids douts ]

prop_syncFifo :: Property
prop_syncFifo = property $ do
  rm <- forAll genReadMode
  wrs <- forAll (genRequests (Gen.integral Range.linearBounded))
  rds <- forAll (genRequests (pure ()))
  let
    cfg = (defXpmFifoConfig @4){readMode = rm, advFeatures = allAdvFeatures}
    n = P.length wrs `P.max` P.length rds + 200
    wrIn = P.take n (wrs <> P.repeat Nothing)
    rdIn = P.take n (fmap isJust rds <> P.repeat True)
    clk = clockGen @F10
    XpmFifoOut{..} =
      xpmFifoSyncWith cfg clk noReset (fromList wrIn) (fromList rdIn)
    fulls = sampleN n isFull
    busys = sampleN n writeResetBusy
    valids = sampleN n isDataValid
    douts = sampleN n fifoData
    ws = acceptedWrites wrIn fulls busys
  H.cover 50 "words transferred" (not (P.null ws))
  -- All words written are read, in order
  readWords rm rdIn valids douts === ws
  -- The FIFO ends up empty
  P.last (sampleN n isEmpty) === True

-- | Writes are rejected while the FIFO is full, and while it resets
prop_syncCapacity :: Property
prop_syncCapacity = property $ do
  rm <- forAll genReadMode
  let
    cfg = (defXpmFifoConfig @4){readMode = rm, advFeatures = allAdvFeatures}
    clk = clockGen @F10
    XpmFifoOut{..} =
      xpmFifoSyncWith cfg clk noReset (pure (Just (0 :: Dat))) (pure False)
    counts = sampleN 100 writeCount
    capacity = case rm of
      Std _ -> 16
      Fwft -> 18
  P.last counts === capacity
  P.last (sampleN 100 isFull) === True

tests :: TestTree
tests = testGroup "XPM FIFO"
  [ testPropertyNamed "xpmFifoSync is a FIFO" "prop_syncFifo" prop_syncFifo
  , testPropertyNamed "xpmFifoSync capacity" "prop_syncCapacity" prop_syncCapacity
  , testCase "advFeaturesParam" $ do
      advFeaturesParam defAdvFeatures @?= "0707"
      advFeaturesParam allAdvFeatures @?= "1f1f"
      advFeaturesParam noAdvFeatures{enDataValid = True, enOverflow = True} @?= "1001"
  ]
