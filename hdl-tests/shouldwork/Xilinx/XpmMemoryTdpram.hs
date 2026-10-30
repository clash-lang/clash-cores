module XpmMemoryTdpram where

import Clash.Explicit.Prelude

import XpmMemoryCommon
import qualified XpmMemoryTdpramTypes as Types

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

tb4 = tb Types.dut4 $(expected Types.dut4)
{-# ANN tb4 (TestBench 'topEntity) #-}
{-# NOINLINE tb4 #-}

tb5 = tb Types.dut5 $(expected Types.dut5)
{-# ANN tb5 (TestBench 'topEntity) #-}
{-# NOINLINE tb5 #-}

tb6 = tb Types.dut6 $(expected Types.dut6)
{-# ANN tb6 (TestBench 'topEntity) #-}
{-# NOINLINE tb6 #-}

tb7 = tb Types.dut7 $(expected Types.dut7)
{-# ANN tb7 (TestBench 'topEntity) #-}
{-# NOINLINE tb7 #-}

tb8 = tb Types.dut8 $(expected Types.dut8)
{-# ANN tb8 (TestBench 'topEntity) #-}
{-# NOINLINE tb8 #-}
