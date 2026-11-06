{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell model of 'Clash.Cores.Intel.DDR.altddioBidir'
against Intel's simulation model (@altera_mf@), with two registers sharing a
wire. Cyclone IV E and MAX 10 don't support synchronous resets, so we only
test those on Cyclone V.
-}
module DdioBidir where

import Clash.Explicit.Prelude

import DdioBidirTypes
import IntelTestCommon

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbCycloneVAsync :: Signal System Bool
tbCycloneVAsync =
  tb (dut (SSymbol @"Cyclone V"))
    $(blobs (dut @System (SSymbol @"Cyclone V")) (genStimuli 1))
{-# ANN tbCycloneVAsync (TestBench 'topEntity) #-}

tbCycloneVSync :: Signal DomSync Bool
tbCycloneVSync =
  tb (dut (SSymbol @"Cyclone V"))
    $(blobs (dut @DomSync (SSymbol @"Cyclone V")) (genStimuli 2))
{-# ANN tbCycloneVSync (TestBench 'topEntity) #-}

tbCycloneIVEAsync :: Signal System Bool
tbCycloneIVEAsync =
  tb (dut (SSymbol @"Cyclone IV E"))
    $(blobs (dut @System (SSymbol @"Cyclone IV E")) (genStimuli 3))
{-# ANN tbCycloneIVEAsync (TestBench 'topEntity) #-}

tbMax10Async :: Signal System Bool
tbMax10Async =
  tb (dut (SSymbol @"MAX 10"))
    $(blobs (dut @System (SSymbol @"MAX 10")) (genStimuli 4))
{-# ANN tbMax10Async (TestBench 'topEntity) #-}
