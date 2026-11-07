{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell model of 'Clash.Cores.Intel.Cdc.avalonStClockCrosser'
against the simulation model of the Avalon-ST Handshake Clock Crosser that
Platform Designer generates, with an output clock that is faster, slower and
the same as the input clock, for several synchronizer depths, with and without
an output pipeline stage.
-}
module ClockCrosser where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Cdc

import ClockCrosserTypes
import IntelTestCommon

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbDefaultFast :: Signal System Bool
tbDefaultFast =
  tb2 (dut @O7 defClockCrosserConfig)
    $(uncurry (blobs2 (dut @O7 defClockCrosserConfig)) (genStimuli 7 1))
{-# ANN tbDefaultFast (TestBench 'topEntity) #-}

tbDefaultSlow :: Signal System Bool
tbDefaultSlow =
  tb2 (dut @O25 defClockCrosserConfig)
    $(uncurry (blobs2 (dut @O25 defClockCrosserConfig)) (genStimuli 25 2))
{-# ANN tbDefaultSlow (TestBench 'topEntity) #-}

tbNoPipelineSame :: Signal System Bool
tbNoPipelineSame =
  tb2 (dut @System (ClockCrosserConfig 2 3 False))
    $(uncurry (blobs2 (dut @System (ClockCrosserConfig 2 3 False))) (genStimuli 10 3))
{-# ANN tbNoPipelineSame (TestBench 'topEntity) #-}

tbNoPipelineFast :: Signal System Bool
tbNoPipelineFast =
  tb2 (dut @O7 (ClockCrosserConfig 4 2 False))
    $(uncurry (blobs2 (dut @O7 (ClockCrosserConfig 4 2 False))) (genStimuli 7 4))
{-# ANN tbNoPipelineFast (TestBench 'topEntity) #-}

tbDeepSlow :: Signal System Bool
tbDeepSlow =
  tb2 (dut @O25 (ClockCrosserConfig 3 4 True))
    $(uncurry (blobs2 (dut @O25 (ClockCrosserConfig 3 4 True))) (genStimuli 25 5))
{-# ANN tbDeepSlow (TestBench 'topEntity) #-}
