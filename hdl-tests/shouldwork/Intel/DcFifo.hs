{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell model of 'Clash.Cores.Intel.Fifo.dcfifo' against
Intel's simulation model (@altera_mf@), for both read modes, several numbers
of synchronizer registers, with and without overflow and underflow checking,
and read clocks faster than, slower than, and equal to the write clock.
-}
module DcFifo where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Fifo

import DcFifoTypes
import IntelTestCommon

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbNormalFast :: Signal System Bool
tbNormalFast =
  tb2 (dut @4 @R7 defDcFifoConfig d4)
    $(uncurry (blobs2 (dut @4 @R7 defDcFifoConfig d4)) (genStimuli 10000 7 1))
{-# ANN tbNormalFast (TestBench 'topEntity) #-}

tbShowAheadSlow :: Signal System Bool
tbShowAheadSlow =
  tb2 (dut @3 @R25 defDcFifoConfig{dcReadMode = ShowAhead, dcReadSyncStages = 3, dcWriteSyncStages = 1} d3)
    $(uncurry
        (blobs2 (dut @3 @R25 defDcFifoConfig{dcReadMode = ShowAhead, dcReadSyncStages = 3, dcWriteSyncStages = 1} d3))
        (genStimuli 10000 25 2))
{-# ANN tbShowAheadSlow (TestBench 'topEntity) #-}

tbShowAheadSame :: Signal System Bool
tbShowAheadSame =
  tb2 (dut @5 @System defDcFifoConfig{dcReadMode = ShowAhead, dcReadSyncStages = 1, dcWriteSyncStages = 4} d5)
    $(uncurry
        (blobs2 (dut @5 @System defDcFifoConfig{dcReadMode = ShowAhead, dcReadSyncStages = 1, dcWriteSyncStages = 4} d5))
        (genStimuli 10000 10 3))
{-# ANN tbShowAheadSame (TestBench 'topEntity) #-}

tbUncheckedNormal :: Signal System Bool
tbUncheckedNormal =
  tb2 (dutUnchecked @2 @R7 defDcFifoConfig{dcOverflowChecking = False, dcUnderflowChecking = False} d2)
    $(uncurry
        (blobs2 (dutUnchecked @2 @R7 defDcFifoConfig{dcOverflowChecking = False, dcUnderflowChecking = False} d2))
        (genStimuli 10000 7 4))
{-# ANN tbUncheckedNormal (TestBench 'topEntity) #-}

tbUncheckedShowAhead :: Signal System Bool
tbUncheckedShowAhead =
  tb2 (dutUnchecked @4 @R25 defDcFifoConfig{dcReadMode = ShowAhead, dcOverflowChecking = False, dcUnderflowChecking = False} d4)
    $(uncurry
        (blobs2 (dutUnchecked @4 @R25 defDcFifoConfig{dcReadMode = ShowAhead, dcOverflowChecking = False, dcUnderflowChecking = False} d4))
        (genStimuli 10000 25 5))
{-# ANN tbUncheckedShowAhead (TestBench 'topEntity) #-}
