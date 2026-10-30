module XpmFifoAxif where

import Clash.Explicit.Prelude

import Data.Proxy

import Clash.Cores.Xilinx.Xpm.Fifo.Axif
import XpmFifoTestCommon

import qualified XpmFifoAxifTypes as T

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

-- | Common clock, small FIFOs
tb0 = done
 where
  done =               T.tbSync       @T.Conf @16 @16 (Proxy @D10) stim cfg expected
  expected = $(T.expectedSync @T.Conf @16 @16 (Proxy @D10) defStimuli ((xpmFifoAxifConfig d16 d16){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16}))
  stim = defStimuli
  cfg = (xpmFifoAxifConfig d16 d16){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16}
{-# ANN tb0 (TestBench 'topEntity) #-}
{-# NOINLINE tb0 #-}

-- | Independent clocks, small FIFOs
tb1 = done
 where
  done =                T.tbAsync       @T.Conf @16 @16 (Proxy @D10) (Proxy @D5) stim cfg expected
  expected = $(T.expectedAsync @T.Conf @16 @16 (Proxy @D10) (Proxy @D5) defStimuli ((xpmFifoAxifConfig d16 d16){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16}))
  stim = defStimuli
  cfg = (xpmFifoAxifConfig d16 d16){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16}
{-# ANN tb1 (TestBench 'topEntity) #-}
{-# NOINLINE tb1 #-}

-- | Common clock, status outputs, no reset synchronizer, various memory types
tb2 = done
 where
  done =               T.tbSync       @T.Conf @32 @64 (Proxy @D5) stim cfg expected
  expected = $(T.expectedSync @T.Conf @32 @64 (Proxy @D5) defStimuli ((xpmFifoAxifConfig d32 d64){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16, axifWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 12, statusProgEmptyThresh = Just 6, statusWriteDataCount = True, statusReadDataCount = True}, axifReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 40, statusProgEmptyThresh = Just 20, statusWriteDataCount = True, statusReadDataCount = True}, axifResetSynchronizer = False, axifWriteAddressMemoryType = Block, axifWriteDataMemoryType = Distributed, axifReadDataMemoryType = Auto}))
  stim = defStimuli
  cfg = (xpmFifoAxifConfig d32 d64){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16, axifWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 12, statusProgEmptyThresh = Just 6, statusWriteDataCount = True, statusReadDataCount = True}, axifReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 40, statusProgEmptyThresh = Just 20, statusWriteDataCount = True, statusReadDataCount = True}, axifResetSynchronizer = False, axifWriteAddressMemoryType = Block, axifWriteDataMemoryType = Distributed, axifReadDataMemoryType = Auto}
{-# ANN tb2 (TestBench 'topEntity) #-}
{-# NOINLINE tb2 #-}

-- | Independent clocks (slow to fast), status outputs, three CDC stages
tb3 = done
 where
  done =                T.tbAsync       @T.Conf @32 @16 (Proxy @D5) (Proxy @D11) stim cfg expected
  expected = $(T.expectedAsync @T.Conf @32 @16 (Proxy @D5) (Proxy @D11) defStimuli ((xpmFifoAxifConfig d32 d16){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16, axifWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 20, statusProgEmptyThresh = Just 9, statusWriteDataCount = True, statusReadDataCount = True}, axifReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 9, statusProgEmptyThresh = Just 5, statusWriteDataCount = True, statusReadDataCount = True}, axifCdcSyncStages = 3, axifReadDataMemoryType = Distributed}))
  stim = defStimuli
  cfg = (xpmFifoAxifConfig d32 d16){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16, axifWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 20, statusProgEmptyThresh = Just 9, statusWriteDataCount = True, statusReadDataCount = True}, axifReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 9, statusProgEmptyThresh = Just 5, statusWriteDataCount = True, statusReadDataCount = True}, axifCdcSyncStages = 3, axifReadDataMemoryType = Distributed}
{-# ANN tb3 (TestBench 'topEntity) #-}
{-# NOINLINE tb3 #-}

-- | Independent clocks (fast to slow), status outputs, four CDC stages, zero width IDs and user signals
tb4 = done
 where
  done =                T.tbAsync       @T.ConfNarrow @64 @32 (Proxy @D11) (Proxy @D3) stim cfg expected
  expected = $(T.expectedAsync @T.ConfNarrow @64 @32 (Proxy @D11) (Proxy @D3) defStimuli ((xpmFifoAxifConfig d64 d32){axifWriteAddressDepth = 32, axifWriteResponseDepth = 32, axifReadAddressDepth = 32, axifWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 30, statusProgEmptyThresh = Just 10, statusWriteDataCount = True, statusReadDataCount = True}, axifReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 15, statusProgEmptyThresh = Just 7, statusWriteDataCount = True, statusReadDataCount = True}, axifCdcSyncStages = 4}))
  stim = defStimuli
  cfg = (xpmFifoAxifConfig d64 d32){axifWriteAddressDepth = 32, axifWriteResponseDepth = 32, axifReadAddressDepth = 32, axifWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 30, statusProgEmptyThresh = Just 10, statusWriteDataCount = True, statusReadDataCount = True}, axifReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 15, statusProgEmptyThresh = Just 7, statusWriteDataCount = True, statusReadDataCount = True}, axifCdcSyncStages = 4}
{-# ANN tb4 (TestBench 'topEntity) #-}
{-# NOINLINE tb4 #-}

-- | Common clock, UltraRAM, zero width IDs and user signals
tb5 = done
 where
  done =               T.tbSync       @T.ConfNarrow @16 @32 (Proxy @D11) stim cfg expected
  expected = $(T.expectedSync @T.ConfNarrow @16 @32 (Proxy @D11) defStimuli ((xpmFifoAxifConfig d16 d32){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16, axifWriteDataMemoryType = Ultra, axifReadDataMemoryType = Ultra, axifWriteResponseMemoryType = Ultra}))
  stim = defStimuli
  cfg = (xpmFifoAxifConfig d16 d32){axifWriteAddressDepth = 16, axifWriteResponseDepth = 16, axifReadAddressDepth = 16, axifWriteDataMemoryType = Ultra, axifReadDataMemoryType = Ultra, axifWriteResponseMemoryType = Ultra}
{-# ANN tb5 (TestBench 'topEntity) #-}
{-# NOINLINE tb5 #-}
