{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Series7ClockBuffer where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Series7ClockBufferTypes

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

-- | Like 'tbWith', for a device under test that also uses 'clockF'
tbWithC ::
  forall n.
  (KnownNat n, 1 <= n) =>
  (Clock A -> Clock F -> Signal A (Unsigned 8)) ->
  Vec n (Unsigned 8) ->
  Signal A Bool
tbWithC dut expected = done
 where
  done = outputVerifier' clkA noReset expected (dut clkA (clockF clkA))
  clkA = tbClockGen (not <$> done)

tbBufio = tbWith dutBufio expected
 where
  expected = $(listToVecTH (sampleN 100 (dutBufio clockGen)))
{-# ANN tbBufio (TestBench 'topEntity) #-}

tbBufr = tbWith dutBufr expected
 where
  expected = $(listToVecTH (sampleN 100 (dutBufr clockGen)))
{-# ANN tbBufr (TestBench 'topEntity) #-}

tbBufh = tbWith dutBufh expected
 where
  expected = $(listToVecTH (sampleN 100 (dutBufh clockGen)))
{-# ANN tbBufh (TestBench 'topEntity) #-}

tbBufmr = tbWith dutBufmr expected
 where
  expected = $(listToVecTH (sampleN 100 (dutBufmr clockGen)))
{-# ANN tbBufmr (TestBench 'topEntity) #-}

tbBufhce = tbWith dutBufhce expected
 where
  expected = $(listToVecTH (sampleN 300 (dutBufhce clockGen)))
{-# ANN tbBufhce (TestBench 'topEntity) #-}

tbBufmrce = tbWith dutBufmrce expected
 where
  expected = $(listToVecTH (sampleN 300 (dutBufmrce clockGen)))
{-# ANN tbBufmrce (TestBench 'topEntity) #-}

tbBufrDiv1 = tbWithC (dutBufrDiv @1 @G1 d1 12 1) expected
 where
  expected =
    $(listToVecTH (sampleN 300 (dutBufrDiv @1 @G1 d1 12 1 clockGen (clockF clockGen))))
{-# ANN tbBufrDiv1 (TestBench 'topEntity) #-}

tbBufrDiv2 = tbWithC (dutBufrDiv @2 @G2 d2 12 1) expected
 where
  expected =
    $(listToVecTH (sampleN 300 (dutBufrDiv @2 @G2 d2 12 1 clockGen (clockF clockGen))))
{-# ANN tbBufrDiv2 (TestBench 'topEntity) #-}

tbBufrDiv3 = tbWithC (dutBufrDiv @3 @G3 d3 12 2) expected
 where
  expected =
    $(listToVecTH (sampleN 300 (dutBufrDiv @3 @G3 d3 12 2 clockGen (clockF clockGen))))
{-# ANN tbBufrDiv3 (TestBench 'topEntity) #-}

tbBufrDiv4 = tbWithC (dutBufrDiv @4 @G4 d4 14 1) expected
 where
  expected =
    $(listToVecTH (sampleN 300 (dutBufrDiv @4 @G4 d4 14 1 clockGen (clockF clockGen))))
{-# ANN tbBufrDiv4 (TestBench 'topEntity) #-}

tbBufrDiv5 = tbWithC (dutBufrDiv @5 @G5 d5 14 2) expected
 where
  expected =
    $(listToVecTH (sampleN 300 (dutBufrDiv @5 @G5 d5 14 2 clockGen (clockF clockGen))))
{-# ANN tbBufrDiv5 (TestBench 'topEntity) #-}

-- | Divider without gating or clearing
tbBufrDiv8 = tbWithC (dutBufrDiv @8 @G8 d8 16 0) expected
 where
  expected =
    $(listToVecTH (sampleN 300 (dutBufrDiv @8 @G8 d8 16 0 clockGen (clockF clockGen))))
{-# ANN tbBufrDiv8 (TestBench 'topEntity) #-}

tbBufmrceBufrDiv = tbWithC dutBufmrceBufrDiv expected
 where
  expected =
    $(listToVecTH (sampleN 300 (dutBufmrceBufrDiv clockGen (clockF clockGen))))
{-# ANN tbBufmrceBufrDiv (TestBench 'topEntity) #-}
