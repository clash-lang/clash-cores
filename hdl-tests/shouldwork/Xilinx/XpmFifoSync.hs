module XpmFifoSync where

import Clash.Explicit.Prelude

import XpmTestCommon (D10)

import qualified XpmFifoTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

tb0 :: Signal D10 Bool
tb0 = Types.syncTb Types.syncCfg0 $(Types.syncExpected Types.syncCfg0 (clockGen @D10) 800)
{-# ANN tb0 (TestBench 'topEntity) #-}
{-# NOINLINE tb0 #-}

tb1 :: Signal D10 Bool
tb1 = Types.syncTb Types.syncCfg1 $(Types.syncExpected Types.syncCfg1 (clockGen @D10) 800)
{-# ANN tb1 (TestBench 'topEntity) #-}
{-# NOINLINE tb1 #-}

tb2 :: Signal D10 Bool
tb2 = Types.syncTb Types.syncCfg2 $(Types.syncExpected Types.syncCfg2 (clockGen @D10) 800)
{-# ANN tb2 (TestBench 'topEntity) #-}
{-# NOINLINE tb2 #-}

tb3 :: Signal D10 Bool
tb3 = Types.syncTb Types.syncCfg3 $(Types.syncExpected Types.syncCfg3 (clockGen @D10) 800)
{-# ANN tb3 (TestBench 'topEntity) #-}
{-# NOINLINE tb3 #-}

tb4 :: Signal D10 Bool
tb4 = Types.syncTb Types.syncCfg4 $(Types.syncExpected Types.syncCfg4 (clockGen @D10) 800)
{-# ANN tb4 (TestBench 'topEntity) #-}
{-# NOINLINE tb4 #-}

tb5 :: Signal D10 Bool
tb5 = Types.syncTb Types.syncCfg5 $(Types.syncExpected Types.syncCfg5 (clockGen @D10) 800)
{-# ANN tb5 (TestBench 'topEntity) #-}
{-# NOINLINE tb5 #-}

tb6 :: Signal D10 Bool
tb6 = Types.syncTb Types.syncCfg6 $(Types.syncExpected Types.syncCfg6 (clockGen @D10) 800)
{-# ANN tb6 (TestBench 'topEntity) #-}
{-# NOINLINE tb6 #-}
