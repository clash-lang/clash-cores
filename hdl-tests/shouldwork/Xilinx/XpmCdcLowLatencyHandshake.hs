module XpmCdcLowLatencyHandshake where

import Clash.Cores.Xilinx.Xpm.Cdc.LowLatencyHandshake
import Clash.Explicit.Prelude
import Data.Proxy

import XpmTestCommon (D3, D5, D10, D11)

import qualified XpmCdcLowLatencyHandshakeTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

tb0 = done
 where
  --                          src dst srcStages dstStages samples                                                         init
  done =             Types.tb @D3 @D5 @2        @2        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D3 @D5 @2        @2        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) SNat)
{-# ANN tb0 (TestBench 'topEntity) #-}
{-# NOINLINE tb0 #-}

tb1 = done
 where
  --                          src dst srcStages dstStages samples                                                         init
  done =             Types.tb @D5 @D3 @2        @2        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D5 @D3 @2        @2        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) SNat)
{-# ANN tb1 (TestBench 'topEntity) #-}
{-# NOINLINE tb1 #-}

tb2 = done
 where
  --                          src dst  srcStages dstStages samples                                                         init
  done =             Types.tb @D5 @D10 @4        @4        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D5 @D10 @4        @4        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) SNat)
{-# ANN tb2 (TestBench 'topEntity) #-}
{-# NOINLINE tb2 #-}

tb3 = done
 where
  --                          src  dst srcStages dstStages samples                                                         init
  done =             Types.tb @D10 @D5 @4        @4        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D10 @D5 @4        @4        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) SNat)
{-# ANN tb3 (TestBench 'topEntity) #-}
{-# NOINLINE tb3 #-}

tb4 = done
 where
  --                          src dst  srcStages dstStages samples                                                         init
  done =             Types.tb @D5 @D11 @2        @10       @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D5 @D11 @2        @10       @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) SNat)
{-# ANN tb4 (TestBench 'topEntity) #-}
{-# NOINLINE tb4 #-}

tb5 = done
 where
  --                          src  dst srcStages dstStages samples                                                         init
  done =             Types.tb @D11 @D5 @10       @2        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) expected
  expected = $(Types.expected @D11 @D5 @10       @2        @200     Proxy Proxy (XpmCdcLowLatencyHandshakeConfig SNat SNat True) SNat)
{-# ANN tb5 (TestBench 'topEntity) #-}
{-# NOINLINE tb5 #-}
