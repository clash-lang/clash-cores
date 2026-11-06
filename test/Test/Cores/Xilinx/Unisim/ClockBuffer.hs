{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}

{- | Properties of the simulation models of the clock buffers. The models are
tested against the UNISIM models in the HDL tests; these tests check them
against each other, with dynamic input clocks.
-}
module Test.Cores.Xilinx.Unisim.ClockBuffer (tests) where

import Clash.Explicit.Prelude

import Clash.Signal.Internal (Clock (..), Femtoseconds (..))
import Data.Int (Int64)
import Hedgehog (Gen, Property, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Cores.Xilinx.Unisim.ClockBuffer

createDomain vSystem{vName = "In0", vPeriod = 10_000}
createDomain vSystem{vName = "In1", vPeriod = 7_000}
createDomain vSystem{vName = "Out", vPeriod = 10_000}

-- | Number of output periods compared
numPeriods :: Int
numPeriods = 200

-- | Random clock periods (in femtoseconds): a clock with a varying period
genPeriods :: Gen [Int64]
genPeriods = Gen.list (Range.singleton 1000) (Gen.int64 (Range.linear 2_000_000 20_000_000))

dynamicClock :: (KnownDomain dom) => [Int64] -> Clock dom
dynamicClock ps = Clock SSymbol (Just (fromList (Femtoseconds <$> (ps <> L.repeat 10_000_000))))

periodsOf :: Clock dom -> [Int64]
periodsOf (Clock _ (Just ps)) = (\(Femtoseconds fs) -> fs) <$> sampleN numPeriods ps
periodsOf (Clock _ Nothing) = error "periodsOf: static clock"

genBools :: Gen [Bool]
genBools = Gen.list (Range.singleton 1000) (Gen.frequency [(3, pure True), (1, pure False)])

ctrl :: Bool -> Bool -> BufgctrlControl
ctrl s ce = BufgctrlControl{bufgctrlS = s, bufgctrlCe = ce, bufgctrlIgnore = False}

{- | With @I1@ never selected and the enable of @I0@ synchronous to @I0@,
'bufgctrl' is a 'bufgce'
-}
prop_bufgctrlIsBufgce :: Property
prop_bufgctrlIsBufgce = property $ do
  ps <- forAll genPeriods
  ces <- forAll genBools
  let
    clk0 = dynamicClock @In0 ps
    ce = fromList (ces <> L.repeat True)
    muxed =
      bufgctrl @In0 @In0 @In1 @In1 @Out
        bufgctrlDefaultConfig{bufgctrlPreselect = Just BufgctrlI0}
        clk0 clk0 (ctrl True <$> ce)
        clockGen clockGen (pure (ctrl False True))
    gated = bufgce @In0 @In0 clk0 (toEnable ce)
  periodsOf muxed === periodsOf gated

{- | 'bufgmuxCtrl' with a constant select is the selected input, whatever the
periods of the inputs
-}
prop_bufgmuxCtrlConstant :: Property
prop_bufgmuxCtrlConstant = property $ do
  ps0 <- forAll genPeriods
  ps1 <- forAll genPeriods
  sel <- forAll Gen.bool
  let
    clk0 = dynamicClock @In0 ps0
    clk1 = dynamicClock @In1 ps1
    out = bufgmuxCtrl @In0 @In1 @In0 @Out clk0 clk1 clk0 (pure sel)
  -- With I1 selected, I0 is selected after configuration and gets deselected
  -- at its first falling edge; I1 gets selected at its first falling edge
  -- after that. The first output edge is replaced by the one at t = 0.
  let
    edges0 = L.scanl (+) 0 ps0
    edges1 = L.scanl (+) 0 ps1
    fall0 = L.sum (L.take 1 ps0) `quot` 2
    fall1 = L.sum (L.take 1 (L.filter (> fall0) (L.zipWith (\t p -> t + p `quot` 2) edges1 ps1)))
    expected
      | sel = L.filter (> fall1) edges1
      | otherwise = L.drop 1 edges0
  periodsOf out === L.take numPeriods (L.zipWith (-) expected (0 : expected))

tests :: TestTree
tests =
  testGroup
    "Xilinx UNISIM clock buffers"
    [ testPropertyNamed "bufgctrl with one input is bufgce" "prop_bufgctrlIsBufgce" prop_bufgctrlIsBufgce
    , testPropertyNamed "bufgmuxCtrl with a constant select" "prop_bufgmuxCtrlConstant" prop_bufgmuxCtrlConstant
    ]
