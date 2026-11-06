{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell model of 'Clash.Cores.Intel.Memory.altshiftTaps'
against Intel's simulation model (@altera_mf@), for several numbers of taps
and tap distances.
-}
module ShiftTaps where

import Clash.Explicit.Prelude

import IntelTestCommon
import ShiftTapsTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbThreeTaps :: Signal System Bool
tbThreeTaps =
  tb (dut (SSymbol @"Cyclone V") d3 d4)
    $(blobs (dut @3 @4 @System (SSymbol @"Cyclone V") d3 d4) (genStimuli 1))
{-# ANN tbThreeTaps (TestBench 'topEntity) #-}

tbTwoTaps :: Signal System Bool
tbTwoTaps =
  tb (dut (SSymbol @"Cyclone IV E") d2 d5)
    $(blobs (dut @2 @5 @System (SSymbol @"Cyclone IV E") d2 d5) (genStimuli 2))
{-# ANN tbTwoTaps (TestBench 'topEntity) #-}

tbSingleTap :: Signal System Bool
tbSingleTap =
  tb (dut (SSymbol @"Cyclone IV E") d1 d3)
    $(blobs (dut @1 @3 @System (SSymbol @"Cyclone IV E") d1 d3) (genStimuli 3))
{-# ANN tbSingleTap (TestBench 'topEntity) #-}
