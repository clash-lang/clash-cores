module XpmFifoAxil where

import Clash.Explicit.Prelude

import Data.Proxy

import Clash.Cores.Xilinx.Xpm.Fifo.Axil
import XpmFifoTestCommon

import qualified XpmFifoAxilTypes as T

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

-- | Common clock, small FIFOs
tb0 = done
 where
  done =               T.tbSync       @16 @16 (Proxy @D10) stim cfg expected
  expected = $(T.expectedSync @16 @16 (Proxy @D10) defStimuli ((xpmFifoAxilConfig d16 d16){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16}))
  stim = defStimuli
  cfg = (xpmFifoAxilConfig d16 d16){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16}
{-# ANN tb0 (TestBench 'topEntity) #-}
{-# NOINLINE tb0 #-}

-- | Independent clocks, small FIFOs
tb1 = done
 where
  done =                T.tbAsync       @16 @16 (Proxy @D10) (Proxy @D5) stim cfg expected
  expected = $(T.expectedAsync @16 @16 (Proxy @D10) (Proxy @D5) defStimuli ((xpmFifoAxilConfig d16 d16){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16}))
  stim = defStimuli
  cfg = (xpmFifoAxilConfig d16 d16){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16}
{-# ANN tb1 (TestBench 'topEntity) #-}
{-# NOINLINE tb1 #-}

-- | Common clock, status outputs, no reset synchronizer, various memory types
tb2 = done
 where
  done =               T.tbSync       @32 @64 (Proxy @D5) stim cfg expected
  expected = $(T.expectedSync @32 @64 (Proxy @D5) defStimuli ((xpmFifoAxilConfig d32 d64){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16, axilWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 12, statusProgEmptyThresh = Just 6, statusWriteDataCount = True, statusReadDataCount = True}, axilReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 40, statusProgEmptyThresh = Just 20, statusWriteDataCount = True, statusReadDataCount = True}, axilResetSynchronizer = False, axilWriteAddressMemoryType = Block, axilWriteDataMemoryType = Distributed, axilReadDataMemoryType = Auto}))
  stim = defStimuli
  cfg = (xpmFifoAxilConfig d32 d64){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16, axilWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 12, statusProgEmptyThresh = Just 6, statusWriteDataCount = True, statusReadDataCount = True}, axilReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 40, statusProgEmptyThresh = Just 20, statusWriteDataCount = True, statusReadDataCount = True}, axilResetSynchronizer = False, axilWriteAddressMemoryType = Block, axilWriteDataMemoryType = Distributed, axilReadDataMemoryType = Auto}
{-# ANN tb2 (TestBench 'topEntity) #-}
{-# NOINLINE tb2 #-}

-- | Independent clocks (slow to fast), status outputs, three CDC stages
tb3 = done
 where
  done =                T.tbAsync       @32 @16 (Proxy @D5) (Proxy @D11) stim cfg expected
  expected = $(T.expectedAsync @32 @16 (Proxy @D5) (Proxy @D11) defStimuli ((xpmFifoAxilConfig d32 d16){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16, axilWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 20, statusProgEmptyThresh = Just 9, statusWriteDataCount = True, statusReadDataCount = True}, axilReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 9, statusProgEmptyThresh = Just 5, statusWriteDataCount = True, statusReadDataCount = True}, axilCdcSyncStages = 3, axilReadDataMemoryType = Distributed}))
  stim = defStimuli
  cfg = (xpmFifoAxilConfig d32 d16){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16, axilWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 20, statusProgEmptyThresh = Just 9, statusWriteDataCount = True, statusReadDataCount = True}, axilReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 9, statusProgEmptyThresh = Just 5, statusWriteDataCount = True, statusReadDataCount = True}, axilCdcSyncStages = 3, axilReadDataMemoryType = Distributed}
{-# ANN tb3 (TestBench 'topEntity) #-}
{-# NOINLINE tb3 #-}

-- | Independent clocks (fast to slow), status outputs, four CDC stages
tb4 = done
 where
  done =                T.tbAsync       @64 @32 (Proxy @D11) (Proxy @D3) stim cfg expected
  expected = $(T.expectedAsync @64 @32 (Proxy @D11) (Proxy @D3) defStimuli ((xpmFifoAxilConfig d64 d32){axilWriteAddressDepth = 32, axilWriteResponseDepth = 32, axilReadAddressDepth = 32, axilWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 30, statusProgEmptyThresh = Just 10, statusWriteDataCount = True, statusReadDataCount = True}, axilReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 15, statusProgEmptyThresh = Just 7, statusWriteDataCount = True, statusReadDataCount = True}, axilCdcSyncStages = 4}))
  stim = defStimuli
  cfg = (xpmFifoAxilConfig d64 d32){axilWriteAddressDepth = 32, axilWriteResponseDepth = 32, axilReadAddressDepth = 32, axilWriteDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 30, statusProgEmptyThresh = Just 10, statusWriteDataCount = True, statusReadDataCount = True}, axilReadDataStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 15, statusProgEmptyThresh = Just 7, statusWriteDataCount = True, statusReadDataCount = True}, axilCdcSyncStages = 4}
{-# ANN tb4 (TestBench 'topEntity) #-}
{-# NOINLINE tb4 #-}

-- | Common clock, UltraRAM
tb5 = done
 where
  done =               T.tbSync       @16 @32 (Proxy @D11) stim cfg expected
  expected = $(T.expectedSync @16 @32 (Proxy @D11) defStimuli ((xpmFifoAxilConfig d16 d32){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16, axilWriteDataMemoryType = Ultra, axilReadDataMemoryType = Ultra, axilWriteResponseMemoryType = Ultra}))
  stim = defStimuli
  cfg = (xpmFifoAxilConfig d16 d32){axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16, axilWriteDataMemoryType = Ultra, axilReadDataMemoryType = Ultra, axilWriteResponseMemoryType = Ultra}
{-# ANN tb5 (TestBench 'topEntity) #-}
{-# NOINLINE tb5 #-}
