{-# OPTIONS_GHC -Wno-missing-signatures #-}
module IoBuffer where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import IoBufferTypes

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
  done = outputVerifier' clkA rstA expected (dut clkA)
  clkA = tbClockGen (not <$> done)
  rstA = noReset

tbObufds = tbWith dutObufds expected
 where
  expected = $(listToVecTH (sampleN 100 (dutObufds clockGen)))
{-# ANN tbObufds (TestBench 'topEntity) #-}

tbIbufdsGte4Div1 = tbWith (dutIbufdsGte4 @1 @A d1) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIbufdsGte4 @1 @A d1 clockGen)))
{-# ANN tbIbufdsGte4Div1 (TestBench 'topEntity) #-}

tbIbufdsGte4Div2 = tbWith (dutIbufdsGte4 @2 @Div2 d2) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIbufdsGte4 @2 @Div2 d2 clockGen)))
{-# ANN tbIbufdsGte4Div2 (TestBench 'topEntity) #-}
