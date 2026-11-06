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

The UNISIM simulation models of 'bufgce', 'bufgceDiv' and 'bufgGt' produce no
clock edges while the global set/reset (GSR) is asserted, which is the first
100 ns of an HDL simulation.

= Clock multiplexers in Clash simulation

'bufgctrl' switches its output between two clocks, which need not be related.
'bufgmuxCtrl' and 'bufgmux' are 'bufgctrl' with fixed settings. The output is
again a clock with a varying period. Clash simulation computes the time of
every output edge from the edges of both input clocks and of the clocks of the
control signals, by simulating the four latches of the primitive the way the
UNISIM simulation model does. This is exact, for unrelated clocks too, as long as
the following holds:

  * Input clocks have a duty cycle of 50%. Clash clocks have no duty cycle, but
    the multiplexer switches at falling edges (or rising edges, see
    'bufgctrlInitOut'), so the model puts the falling edges halfway the
    periods. The clocks that Clash generates in HDL test benches have this
    duty cycle.

  * Control signals change right after an active edge of their clock, like
    the output of a register. The model, like the UNISIM model, has no setup
    and hold times. In hardware, a select that changes close to an edge of the
    input clock it controls may take effect one cycle of that input later than
    it does in simulation.

  * No edge of an input clock coincides with an edge of the other input
    clock, or with a change of a control signal of the other input. The model
    handles these deterministically: changes of control signals come after
    clock edges at the same time, and a latch that closes at the same time the
    other input gets deselected does not see the deselection yet. In hardware
    such coincidences violate setup and hold times. A control signal driven
    from the domain of the input it controls does change at the same time as
    an edge of that input, which is fine: it changes right after that edge, in
    hardware as well as in simulation.

Some things the model cannot do:

  * Glitches. When switching with 'bufgctrlIgnore' asserted, or when a select
    and an enable of the same input change at the same time, the output can
    produce pulses of a few picoseconds. The model produces their rising edges
    at the times the UNISIM model does (its latches have a delay of 1 ps), but
    Clash clocks only consist of rising edges, so the width of a pulse is
    lost. Whether hardware produces such a pulse depends on its timing.

  * Feedback from the output domain. Clash needs the time of the next output
    edge to move a signal from the output domain to another domain, so if the
    control signals depend on the output domain, simulation hangs.

  * The output domain's period. Its period ('DomainPeriod') is not related to
    the periods of the input clocks. In simulation only the dynamic periods
    matter, but functions that use the period of a domain, such as
    'Clash.Explicit.Prelude.periodToHz', give meaningless results for the
    output domain. Using the period of the fastest input is a sensible choice.

Signals in the output domain have one sample per output edge. Use
'unsafeSynchronizer' (or a proper clock domain crossing) to move signals from
and to the output domain.
-}
module Clash.Cores.Xilinx.Unisim.ClockBuffer
  ( -- * Global clock buffers
    bufg
  , bufgce
  , bufgceDiv
    -- * Clock multiplexers
  , bufgctrl
  , BufgctrlConfig (..)
  , bufgctrlDefaultConfig
  , BufgctrlInput (..)
  , BufgctrlControl (..)
  , bufgmuxCtrl
  , bufgmux
    -- * Transceiver clocks
  , bufgGt
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal ((:-)))

import Clash.Cores.Xilinx.Unisim.ClockBuffer.Internal
  ( BufgctrlConfig (..)
  , BufgctrlControl (..)
  , BufgctrlInput (..)
  , bufgctrlModel
  )
import Clash.Cores.Xilinx.Unisim.Internal (gatedClockModel, unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( BoolStringParam (..)
  , ClockPort (..)
  , Param (..)
  , Port (..)
  , ResetPort (..)
  , inst
  , unPort
  )

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

{- | Default configuration of 'bufgctrl', with the defaults of the primitive: no
input preselected, and a low output while no input is selected
-}
bufgctrlDefaultConfig :: BufgctrlConfig
bufgctrlDefaultConfig =
  BufgctrlConfig
    { bufgctrlPreselect = Nothing
    , bufgctrlInitOut = 0
    }

{- | General clock control buffer (@BUFGCTRL@): a glitch-free multiplexer of
two clocks, with a clock enable for each.

Every input has three control signals (t'BufgctrlControl'): a select (@S@), a
clock enable (@CE@) and @IGNORE@. An input gets selected when both its select
and its enable are asserted, and the other input is not selected; it gets
deselected when its select or its enable is deasserted. These changes take
effect at the next falling edge of the input concerned (the next rising edge if
'bufgctrlInitOut' is 1), at which the output is at its idle level ('bufgctrlInitOut')
anyway, so the output never glitches. To switch from @I0@ to @I1@:

  1. Deassert @S0@ and assert @S1@ (or @CE0@ and @CE1@).

  2. At the next falling edge of @I0@, @I0@ gets deselected. The output stays
     low.

  3. At the next falling edge of @I1@ after that, @I1@ gets selected. The output
     follows @I1@ from then on: its first rising edge is the first rising edge
     of @I1@ after that falling edge.

If both inputs are selected and enabled, the one that got selected first stays
selected. If neither is, the output stays at its idle level. An input clock
that stopped never reaches its next falling edge, so switching away from it
requires its @IGNORE@ to be asserted: the input then gets deselected as soon as
its select or enable is deasserted, which makes the output glitch.

UG974 requires the enables to meet setup and hold times with respect to their
input clocks, or the output may glitch. The selects have setup and hold times
too, but violating them does not make the output glitch; the switch may just
happen a cycle later. So the selects may be asynchronous. Each input's control
signals come from a domain of their own, which can be the domain of that input.

If both inputs get selected at the same moment, the hardware's behaviour is
undefined. The UNISIM model then either keeps the output low, or (when ignoring
the input clocks) oscillates; Clash simulation does the former, or raises an
error for the latter. This cannot happen while the selects are complementary,
as in 'bufgmuxCtrl', or while the falling edges of the inputs do not
coincide. See the module documentation for how Clash simulates the
multiplexer, and its limitations.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFGCTRL)
and UG572. The attributes t'BufgctrlConfig' does not cover are left at their
defaults: no inversions, @CE_TYPE_CE0 = CE_TYPE_CE1 = \"SYNC\"@ and
@STARTUP_SYNC = \"FALSE\"@ (@HARDSYNC@ and @STARTUP_SYNC@ are only supported on
Versal devices).
-}
bufgctrl ::
  forall dom0 domCtrl0 dom1 domCtrl1 domOut.
  ( KnownDomain dom0
  , KnownDomain domCtrl0
  , KnownDomain dom1
  , KnownDomain domCtrl1
  , KnownDomain domOut
  ) =>
  BufgctrlConfig ->
  -- | Clock input @I0@
  Clock dom0 ->
  -- | Clock of the control signals of @I0@
  Clock domCtrl0 ->
  -- | Control signals of @I0@: @S0@, @CE0@ and @IGNORE0@
  Signal domCtrl0 BufgctrlControl ->
  -- | Clock input @I1@
  Clock dom1 ->
  -- | Clock of the control signals of @I1@
  Clock domCtrl1 ->
  -- | Control signals of @I1@: @S1@, @CE1@ and @IGNORE1@
  Signal domCtrl1 BufgctrlControl ->
  Clock domOut
bufgctrl cfg clk0 clkCtrl0 ctrl0 clk1 clkCtrl1 ctrl1
  | clashSimulation = bufgctrlModel cfg clk0 clkCtrl0 ctrl0 clk1 clkCtrl1 ctrl1
  | otherwise = unPort go
 where
  pin :: forall dom. (BufgctrlControl -> Bool) -> Signal dom BufgctrlControl -> Signal dom Bit
  pin f = fmap (boolToBit . f)

  go :: ClockPort "O" domOut
  go =
    inst
      (unisim "BUFGCTRL")
      (Param @"INIT_OUT" @Integer (if bufgctrlInitOut cfg == 1 then 1 else 0))
      (BoolStringParam @"PRESELECT_I0" (bufgctrlPreselect cfg == Just BufgctrlI0))
      (BoolStringParam @"PRESELECT_I1" (bufgctrlPreselect cfg == Just BufgctrlI1))
      (ClockPort @"I0" clk0)
      (ClockPort @"I1" clk1)
      (Port @"S0" (pin bufgctrlS ctrl0))
      (Port @"S1" (pin bufgctrlS ctrl1))
      (Port @"CE0" (pin bufgctrlCe ctrl0))
      (Port @"CE1" (pin bufgctrlCe ctrl1))
      (Port @"IGNORE0" (pin bufgctrlIgnore ctrl0))
      (Port @"IGNORE1" (pin bufgctrlIgnore ctrl1))

{- | Glitch-free clock multiplexer (@BUFGMUX_CTRL@). Outputs @I0@ while the
select is deasserted, and @I1@ while it is asserted. @I0@ is selected right
after configuration.

When the select changes, the output switches at the next falling edge of the
currently selected input, stays low until the next falling edge of the newly
selected input, and follows that input from then on; see 'bufgctrl'. The select
may be asynchronous to both inputs. Vivado implements @BUFGMUX_CTRL@ as a
'bufgctrl' with @S0@ driven by the inverted select, @S1@ by the select, both
enables asserted, and 'bufgctrlPreselect' set to 'BufgctrlI0'; that is what
this function instantiates. The input that is not selected must keep running
until the switch completes: to switch away from a stopped clock, use 'bufgctrl'
with 'bufgctrlIgnore'.

See the module documentation for how Clash simulates the multiplexer, and its
limitations. For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFGMUX_CTRL).
-}
bufgmuxCtrl ::
  forall dom0 dom1 domSel domOut.
  ( KnownDomain dom0
  , KnownDomain dom1
  , KnownDomain domSel
  , KnownDomain domOut
  ) =>
  -- | Clock input @I0@
  Clock dom0 ->
  -- | Clock input @I1@
  Clock dom1 ->
  -- | Clock of the select
  Clock domSel ->
  -- | Select (@S@): 'False' selects @I0@, 'True' selects @I1@
  Signal domSel Bool ->
  Clock domOut
bufgmuxCtrl clk0 clk1 clkSel sel =
  bufgctrl
    bufgctrlDefaultConfig{bufgctrlPreselect = Just BufgctrlI0}
    clk0
    clkSel
    ((\s -> BufgctrlControl{bufgctrlS = not s, bufgctrlCe = True, bufgctrlIgnore = False}) <$> sel)
    clk1
    clkSel
    ((\s -> BufgctrlControl{bufgctrlS = s, bufgctrlCe = True, bufgctrlIgnore = False}) <$> sel)

{- | Clock multiplexer (@BUFGMUX@) that switches with the clock enables of a
'bufgctrl' instead of its selects. Outputs @I0@ while the select is deasserted,
and @I1@ while it is asserted. @I0@ is selected right after configuration.

Vivado implements @BUFGMUX@ as a 'bufgctrl' with @CE0@ driven by the inverted
select, @CE1@ by the select, both selects asserted, and 'bufgctrlPreselect' set
to 'BufgctrlI0'; that is what this function instantiates. Its switching
behaviour is the same as that of 'bufgmuxCtrl', except that a deasserted enable
takes effect right away while the input is low, while a deasserted select
only takes effect at the next falling edge. Because the enables are used, the
select must meet the setup and hold times of @CE0@ and @CE1@ with respect to
both input clocks; use 'bufgmuxCtrl' if it does not. The @CLK_SEL_TYPE =
\"ASYNC\"@ variant is not provided: use 'bufgctrl' with 'bufgctrlIgnore' for
that.

See the module documentation for how Clash simulates the multiplexer, and its
limitations. For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFGMUX).
-}
bufgmux ::
  forall dom0 dom1 domSel domOut.
  ( KnownDomain dom0
  , KnownDomain dom1
  , KnownDomain domSel
  , KnownDomain domOut
  ) =>
  -- | Clock input @I0@
  Clock dom0 ->
  -- | Clock input @I1@
  Clock dom1 ->
  -- | Clock of the select
  Clock domSel ->
  -- | Select (@S@): 'False' selects @I0@, 'True' selects @I1@
  Signal domSel Bool ->
  Clock domOut
bufgmux clk0 clk1 clkSel sel =
  bufgctrl
    bufgctrlDefaultConfig{bufgctrlPreselect = Just BufgctrlI0}
    clk0
    clkSel
    ((\s -> BufgctrlControl{bufgctrlS = True, bufgctrlCe = not s, bufgctrlIgnore = False}) <$> sel)
    clk1
    clkSel
    ((\s -> BufgctrlControl{bufgctrlS = True, bufgctrlCe = s, bufgctrlIgnore = False}) <$> sel)

{- | Clock buffer for the clocks of GTH and GTY transceivers (@BUFG_GT@), such
as @TXOUTCLK@ and @RXOUTCLK@, and for the @ODIV2@ output of
'Clash.Cores.Xilinx.Unisim.IoBuffer.ibufdsGte4'. The output clock runs at the
input frequency divided by @n@, where @n@ is between 1 and 8. Like the one of
'bufgceDiv', the divider counts the input edges at which it is enabled, and
every @n@th counted edge, starting with the first one, produces an output edge.

The clock enable (@CE@) and the clear (@CLR@) pass through a synchronizer of
two flip-flops clocked by the input (the @BUFG_GT_SYNC@ that Vivado inserts
automatically), so they take effect later than those of 'bufgceDiv'. In Clash
terms, input edge /k/ is counted if:

  * The clear mask (@CLRMASK@) is asserted at edge /k/, or the clear is not
    asserted at any of the edges /k - 2/, /k - 1/ and /k/. The clear is
    asserted asynchronously, but deasserted through the synchronizer.

  * And: the enable mask (@CEMASK@) is asserted at edge /k/, or the enable is
    asserted at edge /k - 2/.

Input edges at which the clear is in effect (the first condition does not
hold) restart the divider: the next counted edge produces an output edge. Right
after configuration, the clear and the enable count as asserted and deasserted
at the two edges before the first one, respectively.

The clear takes effect asynchronously, so if it takes effect at edge /k + 1/
(for example because it gets asserted right after edge /k/, or its mask gets
deasserted), it cuts short the output pulse that edge /k/ started. The UNISIM
model then produces no output edge for edge /k/ at all, and neither does Clash
simulation. In hardware, this produces a runt pulse: assert the clear while the
output is low, or while the enable is deasserted, to prevent that.

The output clock is modelled as a clock with a varying period; see the module
documentation for how to use it. If the enable is always asserted (or masked)
and the clear never is (or is masked), the output clock is periodic and its
domain can be used like any other domain.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/BUFG_GT)
and UG572. The divider (@DIV@, which is @n - 1@) is fixed; changing it
dynamically is not supported. @SIM_DEVICE@ and @STARTUP_SYNC@ are left at their
defaults, which behave the same on UltraScale and UltraScale+ devices.
-}
bufgGt ::
  forall n domIn domOut.
  ( KnownDomain domIn
  , KnownDomain domOut
  , 1 <= n
  , n <= 8
  , DomainPeriod domOut ~ n * DomainPeriod domIn
  ) =>
  -- | Divider (@DIV + 1@)
  SNat n ->
  Clock domIn ->
  -- | Clear (@CLR@)
  Reset domIn ->
  -- | Clear mask (@CLRMASK@)
  Signal domIn Bool ->
  -- | Clock enable (@CE@)
  Enable domIn ->
  -- | Clock enable mask (@CEMASK@)
  Signal domIn Bool ->
  Clock domOut
bufgGt divider@SNat clk clr clrMask ena ceMask
  | clashSimulation = gatedClockModel (snatToNum divider) clk clrEffective enaSync
  | otherwise = unPort go
 where
  clr0 = unsafeToActiveHigh clr
  clr1 = delay clk enableGen True clr0
  clr2 = delay clk enableGen True clr1
  clrActive = (not <$> clrMask) .&&. (clr0 .||. clr1 .||. clr2)
  -- A clear that takes effect at edge k + 1 also suppresses the output edge of
  -- edge k. Treating edge k as cleared does exactly that: the divider restarts
  -- at edge k + 1 anyway.
  clrEffective = clrActive .||. lookahead clrActive
  lookahead ~(_ :- xs) = xs
  ce2 = delay clk enableGen False (delay clk enableGen False (fromEnable ena))
  enaSync = ce2 .||. ceMask

  go :: ClockPort "O" domOut
  go =
    inst
      (unisim "BUFG_GT")
      (ClockPort @"I" clk)
      (Port @"CE" (boolToBit <$> fromEnable ena))
      (Port @"CEMASK" (boolToBit <$> ceMask))
      (ResetPort @"CLR" @'ActiveHigh clr)
      (Port @"CLRMASK" (boolToBit <$> clrMask))
      (Port @"DIV" (pure (fromInteger (snatToInteger divider - 1)) :: Signal domIn (BitVector 3)))
