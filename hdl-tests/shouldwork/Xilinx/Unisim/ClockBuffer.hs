{-# OPTIONS_GHC -Wno-missing-signatures #-}
module ClockBuffer where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import ClockBufferTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbWith ::
  forall n.
  (KnownNat n, 1 <= n) =>
  (Clock A -> Signal A (Unsigned 8)) ->
  Vec n (Unsigned 8) ->
  Signal A Bool
tbWith dut expected = done
 where
  done = outputVerifier' clkA rstA expected (dut clkA)
  clkA = tbClockGen (not <$> done)
  rstA = noReset

tbBufg = tbWith dutBufg expected
 where
  expected = $(listToVecTH (sampleN 300 (dutBufg clockGen)))
{-# ANN tbBufg (TestBench 'topEntity) #-}

tbBufgce = tbWith dutBufgce expected
 where
  expected = $(listToVecTH (sampleN 300 (dutBufgce clockGen)))
{-# ANN tbBufgce (TestBench 'topEntity) #-}

tbBufgceDiv1 = tbWith (dutBufgceDiv @1 @G1 d1 12) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutBufgceDiv @1 @G1 d1 12 clockGen)))
{-# ANN tbBufgceDiv1 (TestBench 'topEntity) #-}

tbBufgceDiv2 = tbWith (dutBufgceDiv @2 @G2 d2 12) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutBufgceDiv @2 @G2 d2 12 clockGen)))
{-# ANN tbBufgceDiv2 (TestBench 'topEntity) #-}

tbBufgceDiv3 = tbWith (dutBufgceDiv @3 @G3 d3 12) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutBufgceDiv @3 @G3 d3 12 clockGen)))
{-# ANN tbBufgceDiv3 (TestBench 'topEntity) #-}

-- | Divider without gating: always enabled
tbBufgceDiv8 = tbWith (dutBufgceDiv @8 @G8 d8 16) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutBufgceDiv @8 @G8 d8 16 clockGen)))
{-# ANN tbBufgceDiv8 (TestBench 'topEntity) #-}
