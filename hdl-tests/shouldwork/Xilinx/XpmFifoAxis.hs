module XpmFifoAxis where

import Clash.Explicit.Prelude

import Data.Proxy

import Clash.Cores.Xilinx.Xpm.Fifo.Axis
import Clash.Cores.Xilinx.Xpm.Fifo.Internal (FifoMemoryType(..))
import XpmFifoTestCommon

import qualified XpmFifoAxisTypes as T

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

-- | Common clock, default configuration
tb0 = done
 where
  done =               T.tbSync       @16 @T.Conf @T.User (Proxy @D10) stim cfg expected
  expected = $(T.expectedSync @16 @T.Conf @T.User (Proxy @D10) defStimuli (xpmFifoAxisConfig d16))
  stim = defStimuli
  cfg = xpmFifoAxisConfig d16
{-# ANN tb0 (TestBench 'topEntity) #-}
{-# NOINLINE tb0 #-}

-- | Independent clocks, default configuration
tb1 = done
 where
  done =                T.tbAsync       @16 @T.Conf @T.User (Proxy @D10) (Proxy @D5) stim cfg expected
  expected = $(T.expectedAsync @16 @T.Conf @T.User (Proxy @D10) (Proxy @D5) defStimuli (xpmFifoAxisConfig d16))
  stim = defStimuli
  cfg = xpmFifoAxisConfig d16
{-# ANN tb1 (TestBench 'topEntity) #-}
{-# NOINLINE tb1 #-}

-- | Common clock, all status outputs, block RAM
tb2 = done
 where
  done =               T.tbSync       @32 @T.Conf @T.User (Proxy @D10) stim cfg expected
  expected = $(T.expectedSync @32 @T.Conf @T.User (Proxy @D10) defStimuli ((xpmFifoAxisConfig d32){axisStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 12, statusProgEmptyThresh = Just 6, statusWriteDataCount = True, statusReadDataCount = True}, axisAlmostFull = True, axisAlmostEmpty = True, axisMemoryType = Block}))
  stim = defStimuli
  cfg = (xpmFifoAxisConfig d32){axisStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 12, statusProgEmptyThresh = Just 6, statusWriteDataCount = True, statusReadDataCount = True}, axisAlmostFull = True, axisAlmostEmpty = True, axisMemoryType = Block}
{-# ANN tb2 (TestBench 'topEntity) #-}
{-# NOINLINE tb2 #-}

-- | Independent clocks (slow to fast), all status outputs, distributed RAM
tb3 = done
 where
  done =                T.tbAsync       @32 @T.Conf @T.User (Proxy @D5) (Proxy @D11) stim cfg expected
  expected = $(T.expectedAsync @32 @T.Conf @T.User (Proxy @D5) (Proxy @D11) defStimuli ((xpmFifoAxisConfig d32){axisStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 20, statusProgEmptyThresh = Just 9, statusWriteDataCount = True, statusReadDataCount = True}, axisAlmostFull = True, axisAlmostEmpty = True, axisMemoryType = Distributed, axisCdcSyncStages = 3}))
  stim = defStimuli
  cfg = (xpmFifoAxisConfig d32){axisStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 20, statusProgEmptyThresh = Just 9, statusWriteDataCount = True, statusReadDataCount = True}, axisAlmostFull = True, axisAlmostEmpty = True, axisMemoryType = Distributed, axisCdcSyncStages = 3}
{-# ANN tb3 (TestBench 'topEntity) #-}
{-# NOINLINE tb3 #-}

-- | Independent clocks (fast to slow), all status outputs, four CDC stages
tb4 = done
 where
  done =                T.tbAsync       @64 @T.Conf @T.User (Proxy @D11) (Proxy @D3) stim cfg expected
  expected = $(T.expectedAsync @64 @T.Conf @T.User (Proxy @D11) (Proxy @D3) defStimuli ((xpmFifoAxisConfig d64){axisStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 30, statusProgEmptyThresh = Just 10, statusWriteDataCount = True, statusReadDataCount = True}, axisAlmostFull = True, axisAlmostEmpty = True, axisCdcSyncStages = 4}))
  stim = defStimuli
  cfg = (xpmFifoAxisConfig d64){axisStatus = XpmFifoStatusConfig{statusProgFullThresh = Just 30, statusProgEmptyThresh = Just 10, statusWriteDataCount = True, statusReadDataCount = True}, axisAlmostFull = True, axisAlmostEmpty = True, axisCdcSyncStages = 4}
{-# ANN tb4 (TestBench 'topEntity) #-}
{-# NOINLINE tb4 #-}

-- | Common clock, packet mode
tb5 = done
 where
  done =               T.tbSync       @32 @T.Conf @T.User (Proxy @D5) stim cfg expected
  expected = $(T.expectedSync @32 @T.Conf @T.User (Proxy @D5) defStimuli ((xpmFifoAxisConfig d32){axisPacketFifo = True, axisStatus = noStatus{statusWriteDataCount = True, statusReadDataCount = True}}))
  stim = defStimuli
  cfg = (xpmFifoAxisConfig d32){axisPacketFifo = True, axisStatus = noStatus{statusWriteDataCount = True, statusReadDataCount = True}}
{-# ANN tb5 (TestBench 'topEntity) #-}
{-# NOINLINE tb5 #-}

-- | Independent clocks, packet mode
tb6 = done
 where
  done =                T.tbAsync       @32 @T.Conf @T.User (Proxy @D10) (Proxy @D3) stim cfg expected
  expected = $(T.expectedAsync @32 @T.Conf @T.User (Proxy @D10) (Proxy @D3) defStimuli ((xpmFifoAxisConfig d32){axisPacketFifo = True, axisStatus = noStatus{statusWriteDataCount = True, statusReadDataCount = True}}))
  stim = defStimuli
  cfg = (xpmFifoAxisConfig d32){axisPacketFifo = True, axisStatus = noStatus{statusWriteDataCount = True, statusReadDataCount = True}}
{-# ANN tb6 (TestBench 'topEntity) #-}
{-# NOINLINE tb6 #-}

-- | Independent clocks, packet mode, packets smaller than 8 words
tb7 = done
 where
  done =                T.tbAsync       @16 @T.Conf @T.User (Proxy @D3) (Proxy @D10) stim cfg expected
  expected = $(T.expectedAsync @16 @T.Conf @T.User (Proxy @D3) (Proxy @D10) defStimuli ((xpmFifoAxisConfig d16){axisPacketFifo = True, axisPacketSizeLessThan8 = True}))
  stim = defStimuli
  cfg = (xpmFifoAxisConfig d16){axisPacketFifo = True, axisPacketSizeLessThan8 = True}
{-# ANN tb7 (TestBench 'topEntity) #-}
{-# NOINLINE tb7 #-}

-- | Common clock, zero width ID, destination, and user fields, UltraRAM
tb8 = done
 where
  done =               T.tbSync       @16 @T.ConfNarrow @T.UserNarrow (Proxy @D11) stim cfg expected
  expected = $(T.expectedSync @16 @T.ConfNarrow @T.UserNarrow (Proxy @D11) defStimuli ((xpmFifoAxisConfig d16){axisMemoryType = Ultra}))
  stim = defStimuli
  cfg = (xpmFifoAxisConfig d16){axisMemoryType = Ultra}
{-# ANN tb8 (TestBench 'topEntity) #-}
{-# NOINLINE tb8 #-}
