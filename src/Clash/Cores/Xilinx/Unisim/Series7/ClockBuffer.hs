{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Regional, horizontal and multi-region clock buffers of 7 series devices
(Artix-7, Kintex-7, Virtex-7, Zynq-7000). For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/Clock-Components)
and the /7 Series FPGAs Clocking Resources/ user guide (UG472).

Which buffer can drive what is restricted by the clock architecture, see UG472.
In short:

  * 'bufio' drives the I/O clock network of one bank, which only reaches the
    I/O logic (for example the high-speed clock of an @ISERDESE2@), not the
    fabric.

  * 'bufr' and 'bufrDiv' drive the regional clock network of one clock region,
    which reaches the I/O logic and the fabric of that region.

  * 'bufh' and 'bufhce' drive the horizontal clock network of one clock
    region.

  * 'bufmr' and 'bufmrce' drive the 'bufio's and 'bufr's of their own clock
    region and of the regions above and below it. They can only be driven by a
    multi-region clock-capable (MRCC) input pin.

Global clock buffers ('Clash.Cores.Xilinx.Unisim.ClockBuffer.bufg') exist on 7
series devices too.

= Gated and divided clocks in Clash simulation

'bufhce', 'bufmrce' and 'bufrDiv' can stop their output clock, and 'bufrDiv'
divides it. They are modelled like the UltraScale buffers
'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgce' and
'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgceDiv', with a clock whose period
varies: see "Clash.Cores.Xilinx.Unisim.ClockBuffer" for what that means and for
its limitations.

The UNISIM simulation models of 'bufhce', 'bufmrce' and 'bufrDiv' produce no
clock edges while the global set/reset (GSR) is asserted, which is the first 100
ns of an HDL simulation. The Verilog models of 'bufhce' and 'bufmrce' output a
short pulse at the first input edge after that, even if the enable is
deasserted, if that edge comes less than a picosecond after the end of the GSR,
as the first edge of a Clash test bench clock in Verilog does.
-}
{-# LANGUAGE BangPatterns #-}

module Clash.Cores.Xilinx.Unisim.Series7.ClockBuffer
  ( -- * Regional and I/O clock buffers
    bufio
  , bufr
  , bufrDiv
    -- * Horizontal clock buffers
  , bufh
  , bufhce
    -- * Multi-region clock buffers
  , bufmr
  , bufmrce
  ) where

import Clash.Explicit.Prelude

import Clash.Signal.Internal
  (Clock (..), ClockAB (..), Femtoseconds (..), Signal ((:-)), clockTicks)
import Data.Int (Int64)

import Clash.Cores.Xilinx.Unisim.Internal (clockPeriodsFs, gatedClockModel, unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (ClockPort (..), Param (..), Port (..), ResetPort (..), inst, unPort)

{- | I/O clock buffer (@BUFIO@). Drives a clock onto the I/O clock network of a
bank, which only reaches the I/O logic of that bank. Simulation: the input
clock.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/BUFIO).
-}
bufio :: forall dom. (KnownDomain dom) => Clock dom -> Clock dom
bufio clk
  | clashSimulation = clk
  | otherwise = unPort go
 where
  go :: ClockPort "O" dom
  go = inst (unisim "BUFIO") (ClockPort @"I" clk)

{- | Regional clock buffer (@BUFR@) without a divider (@BUFR_DIVIDE =
\"BYPASS\"@). Drives a clock onto the regional clock network of a clock
region. Simulation: the input clock.

In this mode the buffer's clock enable and clear can't be used, so they are
tied off. Use @'bufrDiv' d1@ for an undivided clock that can be stopped. Note
that the propagation delay of the buffer is slightly smaller in this mode than
with a divider, even a divider of 1, see UG472.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/BUFR).
-}
bufr :: forall dom. (KnownDomain dom) => Clock dom -> Clock dom
bufr clk
  | clashSimulation = clk
  | otherwise = unPort go
 where
  go :: ClockPort "O" dom
  go =
    inst
      (unisim "BUFR")
      (Param @"BUFR_DIVIDE" @String "BYPASS")
      (ClockPort @"I" clk)
      (Port @"CE" (pure 1 :: Signal dom Bit))
      (Port @"CLR" (pure 0 :: Signal dom Bit))

{- | Regional clock buffer with a clock enable and a divider (@BUFR@ with
@BUFR_DIVIDE@ set to @n@). Drives a clock onto the regional clock network of a
clock region. The output clock runs at the input frequency divided by @n@,
where @n@ is between 1 and 8. Its edges coincide with rising edges of the input
clock: the first enabled input edge after configuration (or after a clear)
produces a rising output edge, and from then on every @n@th enabled input edge
does. For odd @n@, the output is low for one input period longer than it is
high.

The clock enable (@CE@) gates the input clock. An input edge is counted if the
enable is asserted at that edge; edges that are not counted do not advance the
divider, so while the enable is deasserted the output clock stops (possibly
halfway a period). Unlike the enable of
'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgce', the enable of a @BUFR@ is
asynchronous: it is ANDed with the input clock, and Vivado does not time it.
UG472 says it is not intended to be used as an actively switching signal. It
only switches cleanly if it changes while the input clock is low:

  * If it is deasserted while the input clock is high, the high phase of the
    gated clock is cut short. If that happens right after a rising edge, as it
    does when the enable comes from a register clocked by the rising edge of
    the input clock, the buffer sees a very short pulse, which may or may not
    be counted and which can make the output clock glitch.

  * If it is asserted while the input clock is high, the gated clock rises
    right away: the buffer counts an edge that has a smaller delay than the
    input edge before it, and a shortened high phase.

The simulation model assumes the enable changes while the input clock is low.
To use the enable at all, it is best to only change it while the divider is
cleared, or to drive it from a register clocked by the falling edge of the
input clock.

The clear (@CLR@) is an asynchronous, active-high reset of the divider. While
it is asserted, the output clock is low and no input edges are counted. After
it is deasserted, the first counted input edge produces an output edge.
Asserting the clear halfway a high phase of the output shortens that phase.
Pass 'noReset' to never clear the divider. If the input clock stops, UG472
requires to clear the divider after it is back.

Because the clear is asynchronous, it can take effect while the input clock is
stopped, for example by a 'bufmrce' driving the @BUFR@. A Clash signal in the
input domain has no samples while its clock is stopped, so the clear comes with
the clock of its own domain, which is only used by the simulation model. In
Clash terms: an input edge is not counted, and restarts the divider, if the
clear is asserted at that edge. If the clear has been asserted since the
previous input edge, but no longer is at this edge, the edge is counted and
restarts the divider: it produces an output edge. If the clear is driven from
the input domain, pass the input clock as the clear's clock; the clear then
works like the one of 'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgceDiv'.

The VHDL UNISIM model of @BUFR@ deviates from the hardware (and from the Verilog
model) when the clear is deasserted while both the input clock and the enable
are high: it then produces an output edge right away, instead of waiting for
the next input edge.

The output clock is modelled as a clock with a varying period; see the module
documentation for how to use it. If the enable is always asserted and the
clear never is, the output clock is periodic and its domain can be used like
any other domain.

To phase-align the outputs of several @BUFR@s driven by one 'bufmrce', UG472
recommends to stop the 'bufmrce', assert and deassert the clears of the
@BUFR@s, and then enable the 'bufmrce' again. The simulation model supports
that sequence, as long as the clears' clock keeps running.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/BUFR).
The primitive's @SIM_DEVICE@ attribute is left at its default, @\"7SERIES\"@.
-}
bufrDiv ::
  forall n domIn domClr domOut.
  ( KnownDomain domIn
  , KnownDomain domClr
  , KnownDomain domOut
  , 1 <= n
  , n <= 8
  , DomainPeriod domOut ~ n * DomainPeriod domIn
  ) =>
  -- | Divider (@BUFR_DIVIDE@)
  SNat n ->
  -- | Input clock (@I@)
  Clock domIn ->
  -- | Simulation only: the clock of the clear's domain
  Clock domClr ->
  -- | Asynchronous clear (@CLR@)
  Reset domClr ->
  -- | Asynchronous clock enable (@CE@)
  Enable domIn ->
  Clock domOut
bufrDiv divider@SNat clk clkClr clr ena
  | clashSimulation =
      bufrDivModel
        (snatToNum divider)
        clk
        (asyncClear clk clkClr (unsafeToActiveHigh clr))
        (fromEnable ena)
  | otherwise = unPort go
 where
  go :: ClockPort "O" domOut
  go =
    inst
      (unisim "BUFR")
      (Param @"BUFR_DIVIDE" @String (divideString (snatToInteger divider)))
      (ClockPort @"I" clk)
      (Port @"CE" (boolToBit <$> fromEnable ena))
      (ResetPort @"CLR" @'ActiveHigh clr)

{- | Horizontal clock buffer (@BUFH@). Drives a clock onto the horizontal clock
network of a clock region. Simulation: the input clock.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/BUFH).
-}
bufh :: forall dom. (KnownDomain dom) => Clock dom -> Clock dom
bufh clk
  | clashSimulation = clk
  | otherwise = unPort go
 where
  go :: ClockPort "O" dom
  go = inst (unisim "BUFH") (ClockPort @"I" clk)

{- | Horizontal clock buffer with a clock enable (@BUFHCE@). Drives a clock
onto the horizontal clock network of a clock region. While the enable is
deasserted, the output clock stays low.

The enable is synchronous to the input clock and works like the one of
'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgce': an input edge passes if the
enable is asserted /at/ that edge, i.e., in the Clash sample belonging to that
edge. For an enable driven by a register, that is the value the register held
before that edge. The buffer never glitches, as long as the enable meets its
setup and hold times (which Vivado times).

The output clock is modelled as a clock with a varying period; see the module
documentation for how to use it. The output domain must have the same period
as the input domain: the output clock runs at the input frequency while it is
enabled.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/BUFHCE).
The primitive's attributes are left at their defaults (@CE_TYPE = \"SYNC\"@,
@INIT_OUT = 0@, no inversions).
-}
bufhce ::
  forall domIn domOut.
  ( KnownDomain domIn
  , KnownDomain domOut
  , DomainPeriod domOut ~ DomainPeriod domIn
  ) =>
  Clock domIn ->
  -- | Clock enable (@CE@)
  Enable domIn ->
  Clock domOut
bufhce clk ena
  | clashSimulation = gatedClockModel 1 clk (pure False) (fromEnable ena)
  | otherwise = unPort go
 where
  go :: ClockPort "O" domOut
  go =
    inst
      (unisim "BUFHCE")
      (ClockPort @"I" clk)
      (Port @"CE" (boolToBit <$> fromEnable ena))

{- | Multi-region clock buffer (@BUFMR@). Drives the 'bufio's and 'bufr's of
its own clock region and of the regions above and below it. Simulation: the
input clock.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/BUFMR).
-}
bufmr :: forall dom. (KnownDomain dom) => Clock dom -> Clock dom
bufmr clk
  | clashSimulation = clk
  | otherwise = unPort go
 where
  go :: ClockPort "O" dom
  go = inst (unisim "BUFMR") (ClockPort @"I" clk)

{- | Multi-region clock buffer with a clock enable (@BUFMRCE@). Drives the
'bufio's and 'bufr's of its own clock region and of the regions above and
below it. While the enable is deasserted, the output clock stays low.

The enable is synchronous to the input clock and works like the one of
'bufhce'. Use it to stop the clock while clearing the dividers of the 'bufrDiv's
the buffer drives, see 'bufrDiv'.

The output clock is modelled as a clock with a varying period; see the module
documentation for how to use it. The output domain must have the same period
as the input domain.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/BUFMRCE).
The primitive's attributes are left at their defaults (@CE_TYPE = \"SYNC\"@,
@INIT_OUT = 0@, no inversions).
-}
bufmrce ::
  forall domIn domOut.
  ( KnownDomain domIn
  , KnownDomain domOut
  , DomainPeriod domOut ~ DomainPeriod domIn
  ) =>
  Clock domIn ->
  -- | Clock enable (@CE@)
  Enable domIn ->
  Clock domOut
bufmrce clk ena
  | clashSimulation = gatedClockModel 1 clk (pure False) (fromEnable ena)
  | otherwise = unPort go
 where
  go :: ClockPort "O" domOut
  go =
    inst
      (unisim "BUFMRCE")
      (ClockPort @"I" clk)
      (Port @"CE" (boolToBit <$> fromEnable ena))

-- | Value of the @BUFR_DIVIDE@ attribute for a divider. Clash can't evaluate
-- 'show' for an 'Integer' at compile time.
divideString :: Integer -> String
divideString n = case n of
  1 -> "1"
  2 -> "2"
  3 -> "3"
  4 -> "4"
  5 -> "5"
  6 -> "6"
  7 -> "7"
  8 -> "8"
  _ -> error ("bufrDiv: invalid divider " <> show n)

{- | For every edge of the input clock: whether the clear is asserted at that
edge, and whether it has been asserted at any time since the previous input
edge, up to and including this edge. Like 'unsafeSynchronizer', at an input
edge the clear has the value it had before any coinciding edge of its own
clock.
-}
asyncClear ::
  forall domIn domClr.
  (KnownDomain domIn, KnownDomain domClr) =>
  Clock domIn ->
  Clock domClr ->
  Signal domClr Bool ->
  Signal domIn (Bool, Bool)
asyncClear clkIn clkClr clr0@(~(c0 :- _)) = go (clockTicks clkIn clkClr) clr0 c0
 where
  -- @acc@: whether the clear has been asserted since the previous input edge,
  -- including its current value
  go :: [ClockAB] -> Signal domClr Bool -> Bool -> Signal domIn (Bool, Bool)
  go [] _ _ = error "asyncClear: clockTicks should be infinite"
  go (tick : ticks) cs@(~(c :- cs1@(~(c1 :- _)))) !acc =
    case tick of
      ClockA -> (c, acc) :- go ticks cs c
      ClockB -> go ticks cs1 (acc || c1)
      -- The clear changes right after the input edge, so its old value is not
      -- present after the input edge.
      ClockAB -> (c, acc) :- go ticks cs1 c1

{- | Simulation model of 'bufrDiv'. Like
'Clash.Cores.Xilinx.Unisim.Internal.gatedClockModel', but with a clear that can
be asserted between two input edges (see 'asyncClear'): if it is, the divider
restarts, and the input edge is counted if the clear is no longer asserted.
-}
bufrDivModel ::
  forall domIn domOut.
  (KnownDomain domIn, KnownDomain domOut) =>
  -- | Divider, at least 1
  Int ->
  Clock domIn ->
  -- | Clear: asserted at the edge, asserted since the previous edge
  Signal domIn (Bool, Bool) ->
  -- | Enable
  Signal domIn Bool ->
  Clock domOut
bufrDivModel divider clkIn clr0 ena0 =
  Clock SSymbol (Just (Femtoseconds <$> goFirst 0 0 (clockPeriodsFs clkIn) clr0 ena0))
 where
  -- Whether input edge produces an output edge, and the next divider phase.
  -- The phase is the number of counted edges since the last output edge,
  -- modulo the divider.
  step :: Int -> (Bool, Bool) -> Bool -> (Bool, Int)
  step phase (clrAt, clrSince) ena = (counted && phase0 == 0, phase1)
   where
    phase0 = if clrSince then 0 else phase
    counted = ena && not clrAt
    phase1
      | counted = (phase0 + 1) `mod` divider
      | otherwise = phase0

  -- Before the first output edge. @t@ is the time of the current input edge.
  -- The first output edge of the model is at time 0, so the first period ends
  -- at the time of the second real output edge.
  goFirst ::
    Int -> Int64 ->
    Signal domIn Int64 -> Signal domIn (Bool, Bool) -> Signal domIn Bool ->
    Signal domOut Int64
  goFirst !phase !t (p :- ps) (c :- cs) (e :- es)
    | edge = go phase1 (t + p) ps cs es
    | otherwise = goFirst phase1 (t + p) ps cs es
   where
    (edge, phase1) = step phase c e

  -- After the first output edge. @acc@ is the time between the previous output
  -- edge and the current input edge.
  go ::
    Int -> Int64 ->
    Signal domIn Int64 -> Signal domIn (Bool, Bool) -> Signal domIn Bool ->
    Signal domOut Int64
  go !phase !acc (p :- ps) (c :- cs) (e :- es)
    | edge = acc :- go phase1 p ps cs es
    | otherwise = go phase1 (acc + p) ps cs es
   where
    (edge, phase1) = step phase c e
