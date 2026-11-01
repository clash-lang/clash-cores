{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Global clock buffers of UltraScale and UltraScale+ devices. For more
information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/Clock-Components)
and the /UltraScale Architecture Clocking Resources/ user guide (UG572).

= Gated clocks in Clash simulation

'bufgce' and 'bufgceDiv' can stop their output clock. Clash models this with a
clock whose period varies: every period lasts until the next edge the hardware
produces. Signals in the output domain therefore have one sample per output
edge, just like registers in hardware only change on those edges. Because the
output clock is not periodic, it should get its own domain whenever its enable
can be deasserted; using the input domain for it makes Clash simulation
disagree with the hardware. Use 'unsafeSynchronizer' (or a proper clock domain
crossing) to move signals between the domains: in hardware the output edges
coincide with input edges, and 'unsafeSynchronizer' follows the clock edges of
both domains in simulation.

Clash simulation has two limitations:

  * Every clock in Clash has an active edge at the start of the simulation, so
    the output clock does too. This edge stands in for the first edge the
    buffer produces. If the buffer is disabled at the start of the simulation,
    the edges of the input clock before the first real output edge see the
    output domain as if that first edge already happened. From the first real
    output edge on, simulation is exact.

  * A clock that stops forever makes simulation hang as soon as a computation
    needs its next edge, for example when 'unsafeSynchronizer' moves a signal
    from the stopped domain to another one.

The UNISIM simulation models of 'bufgce' and 'bufgceDiv' produce no clock edges
while the global set/reset (GSR) is asserted, which is the first 100 ns of an
HDL simulation.
-}
module Clash.Cores.Xilinx.Unisim.ClockBuffer
  ( bufg
  , bufgce
  , bufgceDiv
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Internal (gatedClockModel, unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (ClockPort (..), Param (..), Port (..), ResetPort (..), inst, unPort)

{- | General clock buffer (@BUFG@). Drives a clock onto the global clock network.
Simulation: the input clock.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFG).
-}
bufg :: forall dom. (KnownDomain dom) => Clock dom -> Clock dom
bufg clk
  | clashSimulation = clk
  | otherwise = unPort go
 where
  go :: ClockPort "O" dom
  go = inst (unisim "BUFG") (ClockPort @"I" clk)

{- | General clock buffer with a clock enable (@BUFGCE@). While the enable is
deasserted, the output clock stays low. The enable is synchronous to the input
clock: an input edge passes if the enable is asserted /at/ that edge, i.e., in
the Clash sample belonging to that edge. For an enable driven by a register,
that is the value the register held before that edge. In hardware, the buffer
latches the enable while the input clock is low, so the output clock never
glitches.

The output clock is modelled as a clock with a varying period; see the module
documentation for how to use it. The output domain must have the same period
as the input domain: the output clock runs at the input frequency while it is
enabled.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFGCE).
The primitive's attributes are left at their defaults (@CE_TYPE = \"SYNC\"@,
no inversions).
-}
bufgce ::
  forall domIn domOut.
  ( KnownDomain domIn
  , KnownDomain domOut
  , DomainPeriod domOut ~ DomainPeriod domIn
  ) =>
  Clock domIn ->
  -- | Clock enable (@CE@)
  Enable domIn ->
  Clock domOut
bufgce clk ena
  | clashSimulation = gatedClockModel 1 clk (pure False) (fromEnable ena)
  | otherwise = unPort go
 where
  go :: ClockPort "O" domOut
  go =
    inst
      (unisim "BUFGCE")
      (ClockPort @"I" clk)
      (Port @"CE" (boolToBit <$> fromEnable ena))

{- | General clock buffer with a clock enable and a divider (@BUFGCE_DIV@). The
output clock runs at the input frequency divided by @n@, where @n@ is between 1
and 8. Its rising edges coincide with rising edges of the input clock: the
first enabled input edge after configuration (or after a clear) produces an
output edge, and from then on every @n@th enabled input edge does.

The clock enable works like the one of 'bufgce': an input edge is counted if the
enable is asserted at that edge. Edges that are not counted do not advance the
divider, so while the enable is deasserted the output clock stops (possibly
halfway a period).

The clear (@CLR@) is an asynchronous, active-high reset of the divider. While
it is asserted, the output clock is low and no input edges are counted. After
it is deasserted, the first counted input edge produces an output edge. In
Clash terms: an input edge at which the clear is asserted is not counted, and
restarts the divider.
Asserting the clear halfway a high phase of the output shortens that phase;
assert it while the enable is deasserted to prevent a glitch. Pass 'noReset' to
never clear the divider.

The output clock is modelled as a clock with a varying period; see the module
documentation for how to use it. If the enable is always asserted and the
clear never is, the output clock is periodic and its domain can be used like
any other domain.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFGCE_DIV).
The primitive's other attributes are left at their defaults (@CE_TYPE =
\"SYNC\"@, @HARDSYNC_CLR = \"FALSE\"@, no inversions).
-}
bufgceDiv ::
  forall n domIn domOut.
  ( KnownDomain domIn
  , KnownDomain domOut
  , 1 <= n
  , n <= 8
  , DomainPeriod domOut ~ n * DomainPeriod domIn
  ) =>
  -- | Divider (@BUFGCE_DIVIDE@)
  SNat n ->
  Clock domIn ->
  -- | Asynchronous clear (@CLR@)
  Reset domIn ->
  -- | Clock enable (@CE@)
  Enable domIn ->
  Clock domOut
bufgceDiv divider@SNat clk clr ena
  | clashSimulation =
      gatedClockModel (snatToNum divider) clk (unsafeToActiveHigh clr) (fromEnable ena)
  | otherwise = unPort go
 where
  go :: ClockPort "O" domOut
  go =
    inst
      (unisim "BUFGCE_DIV")
      (Param @"BUFGCE_DIVIDE" @Integer (snatToInteger divider))
      (ClockPort @"I" clk)
      (Port @"CE" (boolToBit <$> fromEnable ena))
      (ResetPort @"CLR" @'ActiveHigh clr)
