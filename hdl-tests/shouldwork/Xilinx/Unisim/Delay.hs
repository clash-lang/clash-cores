{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Delay where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Clash.Cores.Xilinx.Unisim.Delay
import DelayTypes

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

-- | DelayVariable delay in taps, from an input buffer
cfg0 = DelayConfig DelayCount DelayVarLoad 100 UpdateAsync UltraScale
-- | DelayVariable delay in taps, from the fabric
cfg1 = DelayConfig DelayCount DelayVariable 37 UpdateSync UltraScalePlus
-- | DelayFixed delay in picoseconds
cfg2 = DelayConfig DelayTime DelayFixed 500 UpdateAsync UltraScale
-- | DelayVariable delay in picoseconds
cfg3 = DelayConfig DelayTime DelayVarLoad 1000 UpdateAsync UltraScalePlus

tbIdelay0 = tbWith (dutDelay True FromInputBuffer cfg0) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay True FromInputBuffer (DelayConfig DelayCount DelayVarLoad 100 UpdateAsync UltraScale) clockGen)))
{-# ANN tbIdelay0 (TestBench 'topEntity) #-}

tbIdelay1 = tbWith (dutDelay True FromFabric cfg1) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay True FromFabric (DelayConfig DelayCount DelayVariable 37 UpdateSync UltraScalePlus) clockGen)))
{-# ANN tbIdelay1 (TestBench 'topEntity) #-}

tbIdelay2 = tbWith (dutDelay True FromInputBuffer cfg2) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay True FromInputBuffer (DelayConfig DelayTime DelayFixed 500 UpdateAsync UltraScale) clockGen)))
{-# ANN tbIdelay2 (TestBench 'topEntity) #-}

tbIdelay3 = tbWith (dutDelay True FromInputBuffer cfg3) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay True FromInputBuffer (DelayConfig DelayTime DelayVarLoad 1000 UpdateAsync UltraScalePlus) clockGen)))
{-# ANN tbIdelay3 (TestBench 'topEntity) #-}

tbOdelay0 = tbWith (dutDelay False FromFabric cfg0) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay False FromFabric (DelayConfig DelayCount DelayVarLoad 100 UpdateAsync UltraScale) clockGen)))
{-# ANN tbOdelay0 (TestBench 'topEntity) #-}

tbOdelay3 = tbWith (dutDelay False FromFabric cfg3) expected
 where
  expected = $(listToVecTH (sampleN 200 (dutDelay False FromFabric (DelayConfig DelayTime DelayVarLoad 1000 UpdateAsync UltraScalePlus) clockGen)))
{-# ANN tbOdelay3 (TestBench 'topEntity) #-}

tbIdelayctrl = tbWith dutIdelayctrl expected
 where
  expected = $(listToVecTH (sampleN 200 (dutIdelayctrl clockGen)))
{-# ANN tbIdelayctrl (TestBench 'topEntity) #-}
