{-# OPTIONS_GHC -Wno-missing-signatures #-}
module ClockMux where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Clash.Cores.Xilinx.Unisim.ClockBuffer
import ClockMuxTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- | Checks the observations of the output clock from both input domains
tbWith ::
  forall n0 n1.
  (KnownNat n0, 1 <= n0, KnownNat n1, 1 <= n1) =>
  Dut ->
  Vec n0 (Unsigned 4) ->
  Vec n1 (Unsigned 4) ->
  Signal M0 Bool
tbWith dut expected0 expected1 = done
 where
  (o0, o1) = dut clk0 clk1 clkC
  done0 = outputVerifier' clk0 noReset expected0 (hold clk0 expected0 o0)
  done1 = outputVerifier' clk1 noReset expected1 (hold clk1 expected1 o1)
  done = done0 .&&. unsafeSynchronizer clk1 clk0 done1
  clk0 = tbClockGen (not <$> done)
  clk1 = tbClockGen (not <$> unsafeSynchronizer clk0 clk1 done)
  clkC = tbClockGen (not <$> unsafeSynchronizer clk0 clkC done)

-- | After all expected values have been checked, keep presenting the last one
-- to 'outputVerifier\'': its assertion stays active in HDL, while the other
-- domain's verifier might still be running.
hold ::
  forall dom n a.
  (KnownDomain dom, KnownNat n, 1 <= n) =>
  Clock dom ->
  Vec n a ->
  Signal dom a ->
  Signal dom a
hold clk expected o = mux (cnt .==. pure maxBound) (pure (expected !! (natToNum @n - 1 :: Int))) o
 where
  cnt = delay clk enableGen (0 :: Index (n + 1)) (satSucc SatBound <$> cnt)

tbBufgctrl = tbWith dut expected0 expected1
 where
  dut = dutBufgctrl bufgctrlDefaultConfig
  expected0 = $(listToVecTH (sampleN 300 (fst (dutBufgctrl bufgctrlDefaultConfig clockGen clockGen clockGen))))
  expected1 = $(listToVecTH (sampleN 320 (snd (dutBufgctrl bufgctrlDefaultConfig clockGen clockGen clockGen))))
{-# ANN tbBufgctrl (TestBench 'topEntity) #-}

-- | Preselected I1, INIT_OUT = 1: switches at rising edges
tbBufgctrlInitOut1 = tbWith dut expected0 expected1
 where
  dut = dutBufgctrl cfg
  cfg = BufgctrlConfig{bufgctrlPreselect = Just BufgctrlI1, bufgctrlInitOut = 1}
  expected0 = $(listToVecTH (sampleN 300 (fst (dutBufgctrl (BufgctrlConfig (Just BufgctrlI1) 1) clockGen clockGen clockGen))))
  expected1 = $(listToVecTH (sampleN 320 (snd (dutBufgctrl (BufgctrlConfig (Just BufgctrlI1) 1) clockGen clockGen clockGen))))
{-# ANN tbBufgctrlInitOut1 (TestBench 'topEntity) #-}

-- | Both IGNOREs asserted: switching without waiting for the input clocks
tbBufgctrlIgnore = tbWith dutBufgctrlIgnore expected0 expected1
 where
  expected0 = $(listToVecTH (sampleN 300 (fst (dutBufgctrlIgnore clockGen clockGen clockGen))))
  expected1 = $(listToVecTH (sampleN 320 (snd (dutBufgctrlIgnore clockGen clockGen clockGen))))
{-# ANN tbBufgctrlIgnore (TestBench 'topEntity) #-}

-- | Control signals synchronous to the input they control
tbBufgctrlSync = tbWith dutBufgctrlSync expected0 expected1
 where
  expected0 = $(listToVecTH (sampleN 300 (fst (dutBufgctrlSync clockGen clockGen clockGen))))
  expected1 = $(listToVecTH (sampleN 320 (snd (dutBufgctrlSync clockGen clockGen clockGen))))
{-# ANN tbBufgctrlSync (TestBench 'topEntity) #-}

tbBufgmuxCtrl = tbWith dutBufgmuxCtrl expected0 expected1
 where
  expected0 = $(listToVecTH (sampleN 300 (fst (dutBufgmuxCtrl clockGen clockGen clockGen))))
  expected1 = $(listToVecTH (sampleN 320 (snd (dutBufgmuxCtrl clockGen clockGen clockGen))))
{-# ANN tbBufgmuxCtrl (TestBench 'topEntity) #-}

tbBufgmux = tbWith dutBufgmux expected0 expected1
 where
  expected0 = $(listToVecTH (sampleN 300 (fst (dutBufgmux clockGen clockGen clockGen))))
  expected1 = $(listToVecTH (sampleN 320 (snd (dutBufgmux clockGen clockGen clockGen))))
{-# ANN tbBufgmux (TestBench 'topEntity) #-}
