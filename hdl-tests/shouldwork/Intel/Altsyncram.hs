{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell models of the RAMs in "Clash.Cores.Intel.Memory"
against Intel's simulation model (@altera_mf@), for all read-during-write
modes, with and without output registers, with byte enables and an
initialization file, on Cyclone V (M10K) and Cyclone IV E (M9K).
-}
module Altsyncram where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Memory

import AltsyncramTypes
import IntelTestCommon

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbSpCycloneV :: Signal System Bool
tbSpCycloneV =
  tb (dutSp (SSymbol @"Cyclone V") withInit (port False NewData))
    $(blobs (dutSp (SSymbol @"Cyclone V") withInit (port False NewData)) (genStimuli 1))
{-# ANN tbSpCycloneV (TestBench 'topEntity) #-}

tbSpCycloneVReg :: Signal System Bool
tbSpCycloneVReg =
  tb (dutSp (SSymbol @"Cyclone V") defAltsyncramConfig (port True DontCare))
    $(blobs (dutSp (SSymbol @"Cyclone V") defAltsyncramConfig (port True DontCare)) (genStimuli 2))
{-# ANN tbSpCycloneVReg (TestBench 'topEntity) #-}

tbSpCycloneIVOld :: Signal System Bool
tbSpCycloneIVOld =
  tb (dutSp (SSymbol @"Cyclone IV E") withInit (port True OldData))
    $(blobs (dutSp (SSymbol @"Cyclone IV E") withInit (port True OldData)) (genStimuli 3))
{-# ANN tbSpCycloneIVOld (TestBench 'topEntity) #-}

tbSpCycloneIVNbe :: Signal System Bool
tbSpCycloneIVNbe =
  tb (dutSp (SSymbol @"Cyclone IV E") defAltsyncramConfig (port False NewDataOldMaskedBytes))
    $(blobs (dutSp (SSymbol @"Cyclone IV E") defAltsyncramConfig (port False NewDataOldMaskedBytes)) (genStimuli 4))
{-# ANN tbSpCycloneIVNbe (TestBench 'topEntity) #-}

tbSdpCycloneVOld :: Signal System Bool
tbSdpCycloneVOld =
  tb (dutSdp (SSymbol @"Cyclone V") withInit{mixedReadDuringWrite = MixedOldData} (port True NewData))
    $(blobs (dutSdp (SSymbol @"Cyclone V") withInit{mixedReadDuringWrite = MixedOldData} (port True NewData)) (genStimuli 5))
{-# ANN tbSdpCycloneVOld (TestBench 'topEntity) #-}

tbSdpCycloneVDontCare :: Signal System Bool
tbSdpCycloneVDontCare =
  tb (dutSdp (SSymbol @"Cyclone V") defAltsyncramConfig (port False NewData))
    $(blobs (dutSdp (SSymbol @"Cyclone V") defAltsyncramConfig (port False NewData)) (genStimuli 6))
{-# ANN tbSdpCycloneVDontCare (TestBench 'topEntity) #-}

-- | Intel's simulation model warns that it treats 'MixedDontCare' as
-- 'MixedOldData' for this configuration on Cyclone IV E, and the test harness
-- doesn't accept warnings, so we only test 'MixedOldData'.
tbSdpCycloneIVOld :: Signal System Bool
tbSdpCycloneIVOld =
  tb (dutSdp (SSymbol @"Cyclone IV E") defAltsyncramConfig{mixedReadDuringWrite = MixedOldData} (port False NewData))
    $(blobs (dutSdp (SSymbol @"Cyclone IV E") defAltsyncramConfig{mixedReadDuringWrite = MixedOldData} (port False NewData)) (genStimuli 9))
{-# ANN tbSdpCycloneIVOld (TestBench 'topEntity) #-}

tbTdpCycloneV :: Signal System Bool
tbTdpCycloneV =
  tb (dutTdp (SSymbol @"Cyclone V") withInit (port False NewData) (port True NewData))
    $(blobs (dutTdp (SSymbol @"Cyclone V") withInit (port False NewData) (port True NewData)) (genStimuli 7))
{-# ANN tbTdpCycloneV (TestBench 'topEntity) #-}

tbTdpCycloneIV :: Signal System Bool
tbTdpCycloneIV =
  tb (dutTdp (SSymbol @"Cyclone IV E") defAltsyncramConfig{mixedReadDuringWrite = MixedOldData} (port True OldData) (port False NewDataOldMaskedBytes))
    $(blobs (dutTdp (SSymbol @"Cyclone IV E") defAltsyncramConfig{mixedReadDuringWrite = MixedOldData} (port True OldData) (port False NewDataOldMaskedBytes)) (genStimuli 8))
{-# ANN tbTdpCycloneIV (TestBench 'topEntity) #-}
