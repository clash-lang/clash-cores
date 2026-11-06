{-# OPTIONS_GHC -Wno-missing-signatures #-}
module IoBuffer where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Clash.Cores.Xilinx.Unisim.IoBuffer
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

tbIbufdsGte4Div1 = tbWith (dutIbufdsGte4 @1 @A False d1) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIbufdsGte4 @1 @A False d1 clockGen)))
{-# ANN tbIbufdsGte4Div1 (TestBench 'topEntity) #-}

tbIbufdsGte4Div2 = tbWith (dutIbufdsGte4 @2 @Div2 False d2) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIbufdsGte4 @2 @Div2 False d2 clockGen)))
{-# ANN tbIbufdsGte4Div2 (TestBench 'topEntity) #-}

tbIbufdsGte3Div2 = tbWith (dutIbufdsGte4 @2 @Div2 True d2) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIbufdsGte4 @2 @Div2 True d2 clockGen)))
{-# ANN tbIbufdsGte3Div2 (TestBench 'topEntity) #-}

tbIobufFloating = tbWith (dutIobuf @'Floating False iobufConfig) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIobuf @'Floating False iobufConfig clockGen)))
{-# ANN tbIobufFloating (TestBench 'topEntity) #-}

tbIobufPullUp = tbWith (dutIobuf @'PullUp True iobufDefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIobuf @'PullUp True iobufDefaultConfig clockGen)))
{-# ANN tbIobufPullUp (TestBench 'topEntity) #-}

tbIobufPullDown = tbWith (dutIobuf @'PullDown True iobufConfig) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIobuf @'PullDown True iobufConfig clockGen)))
{-# ANN tbIobufPullDown (TestBench 'topEntity) #-}

tbObuftFloating = tbWith (dutObuft @'Floating False obuftConfig) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutObuft @'Floating False obuftConfig clockGen)))
{-# ANN tbObuftFloating (TestBench 'topEntity) #-}

tbObuftPullUp = tbWith (dutObuft @'PullUp True obuftDefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutObuft @'PullUp True obuftDefaultConfig clockGen)))
{-# ANN tbObuftPullUp (TestBench 'topEntity) #-}

tbObuftPullDown = tbWith (dutObuft @'PullDown True obuftConfig) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutObuft @'PullDown True obuftConfig clockGen)))
{-# ANN tbObuftPullDown (TestBench 'topEntity) #-}

tbIobufds = tbWith dutIobufds expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIobufds clockGen)))
{-# ANN tbIobufds (TestBench 'topEntity) #-}

tbObuftds = tbWith dutObuftds expected
 where
  expected = $(listToVecTH (sampleN 100 (dutObuftds clockGen)))
{-# ANN tbObuftds (TestBench 'topEntity) #-}
