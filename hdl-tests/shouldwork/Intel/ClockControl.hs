{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell model of 'Clash.Cores.Intel.Clock.altclkctrl' against
Intel's simulation models of the clock control atoms, by counting the edges of
the output clock.
-}
module ClockControl where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Clock

import ClockControlTypes
import CycloneIveSim
import FiftyfivenmSim
import IntelTestCommon

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbCycloneIVE :: Signal System Bool
tbCycloneIVE =
  withCycloneIveSim $
    tb (dut CycloneIVE) $(blobs (dut CycloneIVE) (genStimuli 1))
{-# ANN tbCycloneIVE (TestBench 'topEntity) #-}

tbCycloneIVE2 :: Signal System Bool
tbCycloneIVE2 =
  withCycloneIveSim $
    tb (dut CycloneIVE) $(blobs (dut CycloneIVE) (genStimuli 2))
{-# ANN tbCycloneIVE2 (TestBench 'topEntity) #-}

tbMax10 :: Signal System Bool
tbMax10 =
  withFiftyfivenmSim $
    tb (dut Max10) $(blobs (dut Max10) (genStimuli 3))
{-# ANN tbMax10 (TestBench 'topEntity) #-}
