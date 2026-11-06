{-# LANGUAGE AllowAmbiguousTypes #-}
module Series7ClockBufferTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Series7.ClockBuffer
import Series7TestCommon
import UnisimTestCommon

-- | Input domain of the clock buffers
type A = XilinxSystem

-- | Domain of the asynchronous controls of 'bufrDiv': the clock of 'A', but
-- clocked by its falling edge, see 'clockF'
createDomain vXilinxSystem{vName="F", vActiveEdge=Falling}

-- Output domains of the clock buffers: divided by 1, 2, 3, 4, 5 and 8
createDomain vXilinxSystem{vName="G1", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="G2", vPeriod=2 * hzToPeriod 100e6}
createDomain vXilinxSystem{vName="G3", vPeriod=3 * hzToPeriod 100e6}
createDomain vXilinxSystem{vName="G4", vPeriod=4 * hzToPeriod 100e6}
createDomain vXilinxSystem{vName="G5", vPeriod=5 * hzToPeriod 100e6}
createDomain vXilinxSystem{vName="G8", vPeriod=8 * hzToPeriod 100e6}

-- | Clock of 'F': the clock of 'A'. The clear and enable of a 'bufrDiv' are
-- asynchronous and only switch cleanly while its input clock is low: drive
-- them from registers clocked by the falling edge of the input clock.
clockF :: Clock A -> Clock F
clockF = asOtherEdgeClock

-- | Number of cycles in which the clock buffers are disabled at the start of
-- the simulation, to make sure the UNISIM models are out of their global
-- set/reset (asserted for the first 100 ns).
startupCycles :: Index 32
startupCycles = 16

-- | Input clock edge at which the buffered clocks produce their first edge,
-- except for the one of 'dutBufmrceBufrDiv'
firstEdge :: Index 32
firstEdge = startupCycles + 1

-- | The number of edges of a buffered clock in every cycle of the input
-- domain: a counter clocked by the buffered clock, observed from the input
-- domain, minus its value in the cycle before. The observed sequence shows at
-- which input clock edges the buffered clock ticked. The counter is sampled
-- from the input domain, not the other way around: the buffered clock's edges
-- arrive a few delta cycles after those of the input clock in VHDL
-- simulation, so the other way around would sample the input domain's
-- registers after they've been updated.
--
-- The observations are masked up to and including the edge after the first
-- edge of the buffered clock: Clash's model of the buffered clock has its first
-- edge at the start of the simulation instead, see
-- "Clash.Cores.Xilinx.Unisim.ClockBuffer". Because only differences are
-- observed, edges before that (such as the glitch that the Verilog UNISIM model
-- of @BUFGCTRL@, which @BUFHCE@ and @BUFMRCE@ use, produces when the global
-- set/reset ends) don't affect the observations.
observe ::
  (KnownDomain dom) =>
  -- | Input clock edge at which the buffered clock produces its first edge
  Index 32 ->
  Clock A ->
  Clock dom ->
  Signal A (Unsigned 8)
observe first clkA clkB =
  delay clkA enableGen 0 $
    mux (cycleCount clkA .<=. pure (first + 1)) 0 (cntA - delay clkA enableGen 0 cntA)
 where
  cntA = unsafeSynchronizer clkB clkA cnt
  cnt = delay clkB enableGen 0 (cnt + 1)

-- | Random enable, deasserted before 'firstEdge' and asserted at 'firstEdge'
-- and the four edges after it. After that, the enable is asserted in
-- /threshold/ out of 16 cycles, i.e., always for a threshold of 16. The enable
-- is driven by a register, so its value at clock edge /k/ depends on the cycle
-- count at edge /k - 1/.
randomEnable ::
  (KnownDomain dom) => Int -> Unsigned 5 -> Clock dom -> Signal dom Bool
randomEnable seed threshold clk =
  delay clk enableGen False $
    (\n r ->
       (n >= startupCycles && n <= startupCycles + 4)
         || (n > startupCycles && resize (r :: Unsigned 4) < threshold))
      <$> cycleCount clk
      <*> genTestData seed clk

-- | Random clear, asserted before edge /release/ and deasserted at edge
-- /release/. After that, the clear is asserted in /threshold/ out of 32
-- cycles.
randomClear ::
  (KnownDomain dom) => Index 32 -> Int -> Unsigned 6 -> Clock dom -> Signal dom Bool
randomClear release seed threshold clk =
  delay clk enableGen True $
    (\n r -> n < release - 1 || (n >= release && resize (r :: Unsigned 5) < threshold))
      <$> cycleCount clk
      <*> genTestData seed clk

dutBufio :: Clock A -> Signal A (Unsigned 8)
dutBufio clkA = observe firstEdge clkA (bufio clkA)

dutBufr :: Clock A -> Signal A (Unsigned 8)
dutBufr clkA = observe firstEdge clkA (bufr clkA)

dutBufh :: Clock A -> Signal A (Unsigned 8)
dutBufh clkA = observe firstEdge clkA (bufh clkA)

dutBufmr :: Clock A -> Signal A (Unsigned 8)
dutBufmr clkA = observe firstEdge clkA (bufmr clkA)

dutBufhce :: Clock A -> Signal A (Unsigned 8)
dutBufhce clkA =
  observe firstEdge clkA (bufhce @A @G1 clkA (toEnable (randomEnable randomSeed 10 clkA)))

dutBufmrce :: Clock A -> Signal A (Unsigned 8)
dutBufmrce clkA =
  observe firstEdge clkA (bufmrce @A @G1 clkA (toEnable (randomEnable randomSeed 10 clkA)))

-- | A 'bufrDiv' with a random enable and clear, both driven from 'F'
dutBufrDiv ::
  forall n dom.
  ( KnownDomain dom
  , 1 <= n, n <= 8
  , DomainPeriod dom ~ n * DomainPeriod A ) =>
  SNat n ->
  -- | Enable threshold (out of 16)
  Unsigned 5 ->
  -- | Clear threshold (out of 32)
  Unsigned 6 ->
  Clock A ->
  Clock F ->
  Signal A (Unsigned 8)
dutBufrDiv n enaThreshold clrThreshold clkA clkF =
  observe firstEdge clkA (bufrDiv @n @A @F @dom n clkA clkF clr ena)
 where
  ena = toEnable (unsafeSynchronizer clkF clkA (randomEnable randomSeed enaThreshold clkF))
  clr = unsafeFromActiveHigh (randomClear firstEdge (randomSeed + 1) clrThreshold clkF)

-- | A 'bufmrce' driving a 'bufrDiv', as in UG472's use model for aligning
-- several 'bufrDiv's: the clear of the 'bufrDiv' (driven from 'F') is often
-- asserted and deasserted while the 'bufmrce' is stopped (by its enable,
-- driven from 'A').
--
-- The clear is released four edges after the 'bufmrce' starts: Clash's model of
-- the 'bufmrce' output has its first edge at the start of the simulation, so
-- the clear must be asserted at that edge.
dutBufmrceBufrDiv :: Clock A -> Clock F -> Signal A (Unsigned 8)
dutBufmrceBufrDiv clkA clkF =
  observe release clkA (bufrDiv @3 @G1 @F @G3 d3 clkMr clkF clr enableGen)
 where
  release = firstEdge + 4
  clkMr = bufmrce @A @G1 clkA (toEnable (randomEnable randomSeed 6 clkA))
  clr = unsafeFromActiveHigh (randomClear release (randomSeed + 1) 8 clkF)
