{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Ddr where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Clash.Cores.Xilinx.Unisim.Ddr
import DdrTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- | Test bench with a DUT that has its output in the slow domain
tbSlow ::
  forall n a.
  (KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock Slow -> Clock Fast -> Signal Slow a) ->
  Vec n a ->
  Signal Slow Bool
tbSlow dut expected = done
 where
  done = outputVerifier' clkSlow noReset expected (dut clkSlow clkFast)
  clkSlow = tbClockGen (not <$> done)
  clkFast = tbClockGen (not <$> unsafeSynchronizer clkSlow clkFast done)

-- | Test bench with a DUT that has its output in the fast domain
tbFast ::
  forall n a.
  (KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock Slow -> Clock Fast -> Signal Fast a) ->
  Vec n a ->
  Signal Fast Bool
tbFast dut expected = done
 where
  done = outputVerifier' clkFast noReset expected (dut clkSlow clkFast)
  clkSlow = tbClockGen (not <$> unsafeSynchronizer clkFast clkSlow done)
  clkFast = tbClockGen (not <$> done)

tbOddre1 = tbFast (dutOddre1 oddre1DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutOddre1 oddre1DefaultConfig clockGen clockGen)))
{-# ANN tbOddre1 (TestBench 'topEntity) #-}

tbOddre1Plus = tbFast (dutOddre1 config) expected
 where
  config = Oddre1Config{oddre1SimDevice=UltraScalePlus, oddre1ResetValue=1}
  expected = $(listToVecTH (sampleN 200 (dutOddre1 (Oddre1Config UltraScalePlus 1) clockGen clockGen)))
{-# ANN tbOddre1Plus (TestBench 'topEntity) #-}

tbIddre1OppositeEdge = tbSlow (dutIddre1 OppositeEdge) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIddre1 OppositeEdge clockGen clockGen)))
{-# ANN tbIddre1OppositeEdge (TestBench 'topEntity) #-}

tbIddre1SameEdge = tbSlow (dutIddre1 SameEdge) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIddre1 SameEdge clockGen clockGen)))
{-# ANN tbIddre1SameEdge (TestBench 'topEntity) #-}

tbIddre1SameEdgePipelined = tbSlow (dutIddre1 SameEdgePipelined) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIddre1 SameEdgePipelined clockGen clockGen)))
{-# ANN tbIddre1SameEdgePipelined (TestBench 'topEntity) #-}

tbLoopbackOppositeEdge = tbSlow (dutLoopback OppositeEdge) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutLoopback OppositeEdge clockGen clockGen)))
{-# ANN tbLoopbackOppositeEdge (TestBench 'topEntity) #-}

tbLoopbackSameEdge = tbSlow (dutLoopback SameEdge) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutLoopback SameEdge clockGen clockGen)))
{-# ANN tbLoopbackSameEdge (TestBench 'topEntity) #-}

tbLoopbackSameEdgePipelined = tbSlow (dutLoopback SameEdgePipelined) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutLoopback SameEdgePipelined clockGen clockGen)))
{-# ANN tbLoopbackSameEdgePipelined (TestBench 'topEntity) #-}
