{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Serdes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Clash.Cores.Xilinx.Unisim.Serdes
import SerdesTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- | Test bench for a DUT with its output in the divided clock domain
tbDiv ::
  forall div n a.
  (KnownDomain div, KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock Line -> Clock Clk -> Clock div -> Signal div a) ->
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
  forall div n a.
  (KnownDomain div, KnownNat n, 1 <= n, ShowX a, Eq a) =>
  (Clock Line -> Clock Clk -> Clock div -> Signal Line a) ->
  Vec n a ->
  Signal Line Bool
tbLine dut expected = done
 where
  done = outputVerifier' clkLine noReset expected (dut clkLine clk clkDiv)
  clkLine = tbClockGen (not <$> done)
  clk = tbClockGen (not <$> unsafeSynchronizer clkLine clk done)
  clkDiv = tbClockGen @div (not <$> unsafeSynchronizer clkLine clkDiv done)

tbIserdes4 = tbDiv (dutIserdes @4 @Div4 UltraScale) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIserdes @4 @Div4 UltraScale clockGen clockGen clockGen)))
{-# ANN tbIserdes4 (TestBench 'topEntity) #-}

tbIserdes8 = tbDiv (dutIserdes @8 @Div8 UltraScalePlus) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIserdes @8 @Div8 UltraScalePlus clockGen clockGen clockGen)))
{-# ANN tbIserdes8 (TestBench 'topEntity) #-}

tbOserdes4 = tbLine (dutOserdes @4 @Div4 oserdese3DefaultConfig) expected
 where
  expected = $(listToVecTH (sampleN 400 (dutOserdes @4 @Div4 oserdese3DefaultConfig clockGen clockGen clockGen)))
{-# ANN tbOserdes4 (TestBench 'topEntity) #-}

tbOserdes8 = tbLine (dutOserdes @8 @Div8 (Oserdese3Config UltraScalePlus 1)) expected
 where
  expected = $(listToVecTH (sampleN 800 (dutOserdes @8 @Div8 (Oserdese3Config UltraScalePlus 1) clockGen clockGen clockGen)))
{-# ANN tbOserdes8 (TestBench 'topEntity) #-}

tbLoopback4 = tbDiv (dutLoopback @4 @Div4) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutLoopback @4 @Div4 clockGen clockGen clockGen)))
{-# ANN tbLoopback4 (TestBench 'topEntity) #-}

tbLoopback8 = tbDiv (dutLoopback @8 @Div8) expected
 where
  expected = $(listToVecTH (sampleN 100 (dutLoopback @8 @Div8 clockGen clockGen clockGen)))
{-# ANN tbLoopback8 (TestBench 'topEntity) #-}
