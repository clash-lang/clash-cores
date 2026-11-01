{-# LANGUAGE AllowAmbiguousTypes #-}
module IoBufferTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench (clockToDiffClock)

import Clash.Cores.Xilinx.Unisim.IoBuffer
import UnisimTestCommon

-- | Domain of the test bench
type A = XilinxSystem

createDomain vXilinxSystem{vName="Div2", vPeriod=2 * hzToPeriod 100e6}

-- | Random data through an 'obufds', and back through an 'ibufds'. Returns
-- the positive and negative output of the 'obufds', and the output of the
-- 'ibufds'.
dutObufds :: Clock A -> Signal A (BitVector 3)
dutObufds clk = pack <$> bundle (p, n, ibufds p n)
 where
  (p, n) = obufds (genTestData randomSeed clk)

-- | A counter clocked by a clock, observed from domain 'A'. See
-- "ClockBufferTypes".
observe ::
  (KnownDomain dom) =>
  Clock A ->
  Clock dom ->
  Signal A (Unsigned 8)
observe clkA clkB =
  delay clkA enableGen 0 (unsafeSynchronizer clkB clkA cnt)
 where
  cnt = delay clkB enableGen 0 (cnt + 1)

-- | Counters clocked by both outputs of an 'ibufdsGte4' or 'ibufdsGte3',
-- observed from 'A'
dutIbufdsGte4 ::
  forall div domDiv.
  ( KnownDomain domDiv
  , 1 <= div
  , div <= 2
  , DomainPeriod domDiv ~ div * DomainPeriod A
  ) =>
  -- | Use 'ibufdsGte3' instead of 'ibufdsGte4'
  Bool ->
  SNat div ->
  Clock A ->
  Signal A (Unsigned 8, Unsigned 8)
dutIbufdsGte4 gte3 divider clkA = bundle (observe clkA o, observe clkA odiv2)
 where
  (o, odiv2)
    | gte3 = ibufdsGte3 @div @A @domDiv divider (clockToDiffClock clkA)
    | otherwise = ibufdsGte4 @div @A @domDiv divider (clockToDiffClock clkA)
