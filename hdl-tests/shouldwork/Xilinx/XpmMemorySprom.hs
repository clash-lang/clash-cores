module XpmMemorySprom where

import Clash.Explicit.Prelude

import XpmMemoryCommon
import qualified XpmMemorySpromTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0
{-# NOINLINE topEntity #-}

tb0 = tb Types.dut0 $(expected Types.dut0)
{-# ANN tb0 (TestBench 'topEntity) #-}
{-# NOINLINE tb0 #-}

tb1 = tb Types.dut1 $(expected Types.dut1)
{-# ANN tb1 (TestBench 'topEntity) #-}
{-# NOINLINE tb1 #-}

tb2 = tb Types.dut2 $(expected Types.dut2)
{-# ANN tb2 (TestBench 'topEntity) #-}
{-# NOINLINE tb2 #-}

tb3 = tb Types.dut3 $(expected Types.dut3)
{-# ANN tb3 (TestBench 'topEntity) #-}
{-# NOINLINE tb3 #-}
