{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Config where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import ConfigTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbWith ::
  forall n a.
  (KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock A -> Signal A a) ->
  Vec n a ->
  Signal A Bool
tbWith dut expected = done
 where
  done = outputVerifier' clk noReset expected (dut clk)
  clk = tbClockGen (not <$> done)

tbStartupE3 = tbWith dutStartupE3 expected
 where
  expected = $(listToVecTH (sampleN 100 (dutStartupE3 clockGen)))
{-# ANN tbStartupE3 (TestBench 'topEntity) #-}

tbIcapE3 = tbWith dutIcapE3 expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIcapE3 clockGen)))
{-# ANN tbIcapE3 (TestBench 'topEntity) #-}
