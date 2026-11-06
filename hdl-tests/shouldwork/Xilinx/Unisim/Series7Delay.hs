{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Series7Delay where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Clash.Cores.Xilinx.Unisim.Series7.Delay
import Series7DelayTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbWith ::
  forall n a.
  (KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock Dom -> Signal Dom a) ->
  Vec n a ->
  Signal Dom Bool
tbWith dut expected = done
 where
  done = outputVerifier' clk noReset expected (dut clk)
  clk = tbClockGen (not <$> done)

-- | Loadable delay, from an input buffer
cfg0 = DelayConfig DelayVarLoad 0 True PatternData
-- | Variable delay starting at 13 taps, from the fabric
cfg1 = DelayConfig DelayVariable 13 False PatternData
-- | Fixed delay of 20 taps, for a clock
cfg2 = DelayConfig DelayFixed 20 False PatternClock

tbIdelay0 = tbWith (dutDelay True FromInputBuffer cfg0) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay True FromInputBuffer (DelayConfig DelayVarLoad 0 True PatternData) clockGen)))
{-# ANN tbIdelay0 (TestBench 'topEntity) #-}

tbIdelay1 = tbWith (dutDelay True FromFabric cfg1) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay True FromFabric (DelayConfig DelayVariable 13 False PatternData) clockGen)))
{-# ANN tbIdelay1 (TestBench 'topEntity) #-}

tbIdelay2 = tbWith (dutDelay True FromInputBuffer cfg2) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay True FromInputBuffer (DelayConfig DelayFixed 20 False PatternClock) clockGen)))
{-# ANN tbIdelay2 (TestBench 'topEntity) #-}

tbOdelay0 = tbWith (dutDelay False FromFabric cfg0) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay False FromFabric (DelayConfig DelayVarLoad 0 True PatternData) clockGen)))
{-# ANN tbOdelay0 (TestBench 'topEntity) #-}

tbOdelay1 = tbWith (dutDelay False FromFabric cfg1) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay False FromFabric (DelayConfig DelayVariable 13 False PatternData) clockGen)))
{-# ANN tbOdelay1 (TestBench 'topEntity) #-}
