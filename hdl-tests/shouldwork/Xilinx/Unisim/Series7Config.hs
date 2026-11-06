{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Series7Config where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Series7ConfigTypes
import Series7TestCommon

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbWith ::
  forall n a dom.
  (KnownNat n, 1 <= n, ShowX a, Eq a, KnownDomain dom) =>
  (Clock dom -> Signal dom a) ->
  Vec n a ->
  Signal dom Bool
tbWith dut expected = done
 where
  -- The UNISIM models of STARTUPE2 and ICAPE2 contain free running
  -- oscillators: end the simulation explicitly
  done = endSimulation (outputVerifier' clk noReset expected (dut clk))
  clk = tbClockGen (not <$> done)

tbStartupE2 = tbWith dutStartupE2 expected
 where
  expected = $(listToVecTH (sampleN 100 (dutStartupE2 clockGen)))
{-# ANN tbStartupE2 (TestBench 'topEntity) #-}

tbIcapE2 = tbWith dutIcapE2 expected
 where
  expected = $(listToVecTH (sampleN 100 (dutIcapE2 clockGen)))
{-# ANN tbIcapE2 (TestBench 'topEntity) #-}

tbReadDnaPort = tbWith dutReadDnaPort expected
 where
  expected = $(listToVecTH (sampleN 100 (dutReadDnaPort clockGen)))
{-# ANN tbReadDnaPort (TestBench 'topEntity) #-}

tbDnaPort = tbWith dutDnaPort expected
 where
  expected = $(listToVecTH (sampleN 300 (dutDnaPort clockGen)))
{-# ANN tbDnaPort (TestBench 'topEntity) #-}
