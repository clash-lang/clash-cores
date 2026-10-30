module XpmFifoAsync where

import Clash.Explicit.Prelude

import XpmTestCommon (D3, D5, D10, D11)

import qualified XpmFifoTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

-- | Write domain D3, read domain D5
tb0 :: Signal D5 Bool
tb0 =
  Types.asyncTb @D3 @D5 Types.asyncCfg0
    $(Types.asyncExpected Types.asyncCfg0 (clockGen @D3) (clockGen @D5))
{-# ANN tb0 (TestBench 'topEntity) #-}
{-# NOINLINE tb0 #-}

-- | Write domain D5, read domain D3
tb1 :: Signal D3 Bool
tb1 =
  Types.asyncTb @D5 @D3 Types.asyncCfg1
    $(Types.asyncExpected Types.asyncCfg1 (clockGen @D5) (clockGen @D3))
{-# ANN tb1 (TestBench 'topEntity) #-}
{-# NOINLINE tb1 #-}

-- | Write domain D10, read domain D11
tb2 :: Signal D11 Bool
tb2 =
  Types.asyncTb @D10 @D11 Types.asyncCfg2
    $(Types.asyncExpected Types.asyncCfg2 (clockGen @D10) (clockGen @D11))
{-# ANN tb2 (TestBench 'topEntity) #-}
{-# NOINLINE tb2 #-}

-- | Write domain D11, read domain D10
tb3 :: Signal D10 Bool
tb3 =
  Types.asyncTb @D11 @D10 Types.asyncCfg3
    $(Types.asyncExpected Types.asyncCfg3 (clockGen @D11) (clockGen @D10))
{-# ANN tb3 (TestBench 'topEntity) #-}
{-# NOINLINE tb3 #-}

-- | Write domain D5, read domain D10
tb4 :: Signal D10 Bool
tb4 =
  Types.asyncTb @D5 @D10 Types.asyncCfg4
    $(Types.asyncExpected Types.asyncCfg4 (clockGen @D5) (clockGen @D10))
{-# ANN tb4 (TestBench 'topEntity) #-}
{-# NOINLINE tb4 #-}

-- | Write domain D10, read domain D5
tb5 :: Signal D5 Bool
tb5 =
  Types.asyncTb @D10 @D5 Types.asyncCfg5
    $(Types.asyncExpected Types.asyncCfg5 (clockGen @D10) (clockGen @D5))
{-# ANN tb5 (TestBench 'topEntity) #-}
{-# NOINLINE tb5 #-}

-- | Write domain D5, read domain D11
tb6 :: Signal D11 Bool
tb6 =
  Types.asyncTb @D5 @D11 Types.asyncCfg6
    $(Types.asyncExpected Types.asyncCfg6 (clockGen @D5) (clockGen @D11))
{-# ANN tb6 (TestBench 'topEntity) #-}
{-# NOINLINE tb6 #-}

-- | Write domain D11, read domain D5
tb7 :: Signal D5 Bool
tb7 =
  Types.asyncTb @D11 @D5 Types.asyncCfg7
    $(Types.asyncExpected Types.asyncCfg7 (clockGen @D11) (clockGen @D5))
{-# ANN tb7 (TestBench 'topEntity) #-}
{-# NOINLINE tb7 #-}
