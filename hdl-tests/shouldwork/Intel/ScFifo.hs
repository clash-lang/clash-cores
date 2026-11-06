{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell model of 'Clash.Cores.Intel.Fifo.scfifo' against
Intel's simulation model (@altera_mf@), for all read modes, with and without
output register, with and without overflow and underflow checking, and with
asynchronous and synchronous resets.
-}
module ScFifo where

import Clash.Explicit.Prelude

import IntelTestCommon
import ScFifoTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbNormalArea :: Signal System Bool
tbNormalArea = tb (dut normalArea d8) $(blobs (dut @8 @System normalArea d8) (genStimuli 1))
{-# ANN tbNormalArea (TestBench 'topEntity) #-}

tbNormalSpeed :: Signal DomSync Bool
tbNormalSpeed = tb (dut normalSpeed d8) $(blobs (dut @8 @DomSync normalSpeed d8) (genStimuli 2))
{-# ANN tbNormalSpeed (TestBench 'topEntity) #-}

tbShowAheadArea :: Signal System Bool
tbShowAheadArea = tb (dut showAheadArea d6) $(blobs (dut @6 @System showAheadArea d6) (genStimuli 3))
{-# ANN tbShowAheadArea (TestBench 'topEntity) #-}

tbShowAheadSpeed :: Signal System Bool
tbShowAheadSpeed = tb (dut showAheadSpeed d8) $(blobs (dut @8 @System showAheadSpeed d8) (genStimuli 4))
{-# ANN tbShowAheadSpeed (TestBench 'topEntity) #-}

tbShowAheadSpeedSync :: Signal DomSync Bool
tbShowAheadSpeedSync =
  tb (dut showAheadSpeed d5) $(blobs (dut @5 @DomSync showAheadSpeed d5) (genStimuli 5))
{-# ANN tbShowAheadSpeedSync (TestBench 'topEntity) #-}

tbUncheckedNormal :: Signal System Bool
tbUncheckedNormal =
  tb (dutUnchecked (unchecked normalSpeed) d8)
    $(blobs (dutUnchecked @8 @System (unchecked normalSpeed) d8) (genStimuli 6))
{-# ANN tbUncheckedNormal (TestBench 'topEntity) #-}

tbUncheckedShowAhead :: Signal System Bool
tbUncheckedShowAhead =
  tb (dutUnchecked (unchecked showAheadArea) d16)
    $(blobs (dutUnchecked @16 @System (unchecked showAheadArea) d16) (genStimuli 7))
{-# ANN tbUncheckedShowAhead (TestBench 'topEntity) #-}
