{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Series7Serdes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Clash.Cores.Xilinx.Unisim.Series7.Serdes
import Series7SerdesTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- | Test bench for a deserializer: its output is in the divided clock domain
tbIserdes ::
  forall clk div n a.
  (KnownDomain clk, KnownDomain div, KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock Line -> Clock Clk2 -> Clock clk -> Clock div -> Signal div a) ->
  Vec n a ->
  Signal div Bool
tbIserdes dut expected = done
 where
  done = outputVerifier' clkDiv noReset expected (dut clkLine clkOddr clk clkDiv)
  clkDiv = tbClockGen (not <$> done)
  clk = tbClockGen (not <$> unsafeSynchronizer clkDiv clk done)
  clkOddr = tbClockGen (not <$> unsafeSynchronizer clkDiv clkOddr done)
  clkLine = tbClockGen (not <$> unsafeSynchronizer clkDiv clkLine done)

-- | Test bench for a DUT with its output in the divided clock domain
tbDiv ::
  forall clk div n a.
  (KnownDomain clk, KnownDomain div, KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock Line -> Clock clk -> Clock div -> Signal div a) ->
  Vec n a ->
  Signal div Bool
tbDiv dut expected = done
 where
  done = outputVerifier' clkDiv noReset expected (dut clkLine clk clkDiv)
  clkDiv = tbClockGen (not <$> done)
  clk = tbClockGen (not <$> unsafeSynchronizer clkDiv clk done)
  clkLine = tbClockGen (not <$> unsafeSynchronizer clkDiv clkLine done)

-- | Test bench for a DUT with its output in the line domain
tbLine ::
  forall clk div n a.
  (KnownDomain clk, KnownDomain div, KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock Line -> Clock clk -> Clock div -> Signal Line a) ->
  Vec n a ->
  Signal Line Bool
tbLine dut expected = done
 where
  done = outputVerifier' clkLine noReset expected (dut clkLine clk clkDiv)
  clkLine = tbClockGen (not <$> done)
  clk = tbClockGen (not <$> unsafeSynchronizer clkLine clk done)
  clkDiv = tbClockGen @div (not <$> unsafeSynchronizer clkLine clkDiv done)

-- Deserializers, with random resets and bitslips

tbIserdesSdr2 = tbIserdes (dutIserdes @SDR @2 @Div2 iserdese2DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 400 (dutIserdes @SDR @2 @Div2 iserdese2DefaultConfig clockGen clockGen clockGen clockGen)))
{-# ANN tbIserdesSdr2 (TestBench 'topEntity) #-}

tbIserdesSdr3 = tbIserdes (dutIserdes @SDR @3 @Div3 iserdese2DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutIserdes @SDR @3 @Div3 iserdese2DefaultConfig clockGen clockGen clockGen clockGen)))
{-# ANN tbIserdesSdr3 (TestBench 'topEntity) #-}

tbIserdesSdr5 = tbIserdes (dutIserdes @SDR @5 @Div5 (Iserdese2Config InputDdly)) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutIserdes @SDR @5 @Div5 (Iserdese2Config InputDdly) clockGen clockGen clockGen clockGen)))
{-# ANN tbIserdesSdr5 (TestBench 'topEntity) #-}

tbIserdesSdr8 = tbIserdes (dutIserdes @SDR @8 @Div8 iserdese2DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutIserdes @SDR @8 @Div8 iserdese2DefaultConfig clockGen clockGen clockGen clockGen)))
{-# ANN tbIserdesSdr8 (TestBench 'topEntity) #-}

tbIserdesDdr4 = tbIserdes (dutIserdes @DDR @4 @Div4 iserdese2DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 400 (dutIserdes @DDR @4 @Div4 iserdese2DefaultConfig clockGen clockGen clockGen clockGen)))
{-# ANN tbIserdesDdr4 (TestBench 'topEntity) #-}

tbIserdesDdr6 = tbIserdes (dutIserdes @DDR @6 @Div6 (Iserdese2Config InputDdly)) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutIserdes @DDR @6 @Div6 (Iserdese2Config InputDdly) clockGen clockGen clockGen clockGen)))
{-# ANN tbIserdesDdr6 (TestBench 'topEntity) #-}

tbIserdesDdr8 = tbIserdes (dutIserdes @DDR @8 @Div8 iserdese2DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutIserdes @DDR @8 @Div8 iserdese2DefaultConfig clockGen clockGen clockGen clockGen)))
{-# ANN tbIserdesDdr8 (TestBench 'topEntity) #-}

-- Serializers, with random resets

tbOserdesSdr2 = tbLine (dutOserdes @SDR @2 @Div2 oserdese2DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 800 (dutOserdes @SDR @2 @Div2 oserdese2DefaultConfig clockGen clockGen clockGen)))
{-# ANN tbOserdesSdr2 (TestBench 'topEntity) #-}

tbOserdesSdr5 = tbLine (dutOserdes @SDR @5 @Div5 (Oserdese2Config 1 OutputOq)) expected
 where
  expected = $(listToVecTH (sampleN 1000 (dutOserdes @SDR @5 @Div5 (Oserdese2Config 1 OutputOq) clockGen clockGen clockGen)))
{-# ANN tbOserdesSdr5 (TestBench 'topEntity) #-}

tbOserdesSdr8 = tbLine (dutOserdes @SDR @8 @Div8 (Oserdese2Config 0 OutputOfb)) expected
 where
  expected = $(listToVecTH (sampleN 1600 (dutOserdes @SDR @8 @Div8 (Oserdese2Config 0 OutputOfb) clockGen clockGen clockGen)))
{-# ANN tbOserdesSdr8 (TestBench 'topEntity) #-}

tbOserdesDdr4 = tbLine (dutOserdes @DDR @4 @Div4 (Oserdese2Config 1 OutputOq)) expected
 where
  expected = $(listToVecTH (sampleN 1200 (dutOserdes @DDR @4 @Div4 (Oserdese2Config 1 OutputOq) clockGen clockGen clockGen)))
{-# ANN tbOserdesDdr4 (TestBench 'topEntity) #-}

tbOserdesDdr6 = tbLine (dutOserdes @DDR @6 @Div6 oserdese2DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 1200 (dutOserdes @DDR @6 @Div6 oserdese2DefaultConfig clockGen clockGen clockGen)))
{-# ANN tbOserdesDdr6 (TestBench 'topEntity) #-}

tbOserdesDdr8 = tbLine (dutOserdes @DDR @8 @Div8 (Oserdese2Config 1 OutputOfb)) expected
 where
  expected = $(listToVecTH (sampleN 1600 (dutOserdes @DDR @8 @Div8 (Oserdese2Config 1 OutputOfb) clockGen clockGen clockGen)))
{-# ANN tbOserdesDdr8 (TestBench 'topEntity) #-}

-- Serializer to deserializer

tbLoopbackSdr4 = tbDiv (dutLoopback @SDR @4 @Div4) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutLoopback @SDR @4 @Div4 clockGen clockGen clockGen)))
{-# ANN tbLoopbackSdr4 (TestBench 'topEntity) #-}

tbLoopbackDdr8 = tbDiv (dutLoopback @DDR @8 @Div8) expected
 where
  expected = $(listToVecTH (sampleN 300 (dutLoopback @DDR @8 @Div8 clockGen clockGen clockGen)))
{-# ANN tbLoopbackDdr8 (TestBench 'topEntity) #-}

-- Through delay lines

tbIserdesDelayed = tbIserdes dutIserdesDelayed expected
 where
  expected = $(listToVecTH (sampleN 300 (dutIserdesDelayed clockGen clockGen clockGen clockGen)))
{-# ANN tbIserdesDelayed (TestBench 'topEntity) #-}

tbOserdesDelayed = tbLine dutOserdesDelayed expected
 where
  expected = $(listToVecTH (sampleN 1200 (dutOserdesDelayed clockGen clockGen clockGen)))
{-# ANN tbOserdesDelayed (TestBench 'topEntity) #-}
