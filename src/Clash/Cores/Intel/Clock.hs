{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Clock control block of Intel FPGAs (@ALTCLKCTRL@): a global clock buffer with
an enable. For more information see the /Clock Control Block (ALTCLKCTRL) IP
Core User Guide/
(<https://www.intel.com/content/www/us/en/docs/programmable/683598/current/clock-control-block-altclkctrl-ip-core.html>).

The ALTCLKCTRL IP core is not a megafunction that can be instantiated directly:
the IP core instantiates the clock control atom of the device family, which is
what 'altclkctrl' does. Only device families of which Quartus ships a plain
(not encrypted) simulation model of that atom are supported: Cyclone IV E
(@cycloneive_clkctrl@), Cyclone 10 LP (@cyclone10lp_clkctrl@) and MAX 10
(@fiftyfivenm_clkctrl@). The atoms of Cyclone V (@cyclonev_clkena@ and
@cyclonev_clkselect@) only have encrypted simulation models.

Selecting between several clocks (@clkselect@) is not supported: its output
glitches when switching, which the model of a Clash clock can't express. Nor
are the enable register modes @\"none\"@, which glitches, and
@\"double register\"@, which Quartus doesn't allow for global clocks with an
enable.
-}

module Clash.Cores.Intel.Clock
  ( altclkctrl
  , ClockControlFamily (..)
  ) where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (HDL (..), Primitive (..), hasBlackBox)
import Clash.Signal.Internal (Clock (..), Femtoseconds (..), Signal ((:-)))
import Data.Int (Int64)
import GHC.Stack (HasCallStack)

import Clash.Cores.Intel.Clock.Internal (verilogTemplate, vhdlTemplate)

import qualified Prelude as P

-- | Device family, which determines the clock control atom 'altclkctrl'
-- instantiates
data ClockControlFamily
  = -- | Cyclone IV E (@cycloneive_clkctrl@)
    CycloneIVE
  | -- | Cyclone 10 LP (@cyclone10lp_clkctrl@)
    Cyclone10LP
  | -- | MAX 10 (@fiftyfivenm_clkctrl@)
    Max10
  deriving (Show, Eq, Generic, NFDataX)

{- | Global clock buffer with an enable (@ALTCLKCTRL@ with a single input
clock). The enable is registered on the falling edge of the input clock
(@ena_register_mode = \"falling edge\"@), so the output clock doesn't glitch:
it skips the rising edges for which the registered enable is deasserted, and
is low in between. The register powers up enabled.

An enable driven by a register clocked by the input clock changes right after
a rising edge, so the falling edge register sees it before the next rising
edge. In Clash terms: the output clock has edge /k/ of the input clock if
enable sample /k/ is asserted, as if the enable gated the clock
combinationally.

Use the enable to stop a clock domain, or to divide its clock: an enable that
is asserted in every /n/th cycle makes the output clock tick at every /n/th
edge of the input clock. The output clock has its own domain, with the same
nominal period as the input clock. Note that Quartus' timing analysis treats
the output as a clock with the frequency of the input clock.

Simulation: a clock with the edges of the input clock that pass. Like every
Clash clock, it has an edge at the start of the simulation, which corresponds
to the first edge of the input clock (which passes, as the registers power up
enabled).
-}
altclkctrl ::
  forall domIn domOut.
  ( HasCallStack
  , KnownDomain domIn
  , KnownDomain domOut
  , DomainPeriod domOut ~ DomainPeriod domIn
  ) =>
  ClockControlFamily ->
  -- | Input clock (@inclk[0]@)
  Clock domIn ->
  -- | Enable (@ena@)
  Signal domIn Bool ->
  -- | Output clock (@outclk@)
  Clock domOut
altclkctrl family clk ena = case family of
  CycloneIVE -> cycloneiveClkctrl clk ena
  Cyclone10LP -> cyclone10lpClkctrl clk ena
  Max10 -> fiftyfivenmClkctrl clk ena

{- | Simulation model of 'altclkctrl'. Input edge 0 passes, as the enable
register powers up enabled. Input edge /k/ passes if enable sample /k/ is
asserted.
-}
gatedClockModel ::
  forall domIn domOut.
  (KnownDomain domIn, KnownDomain domOut) =>
  Clock domIn ->
  Signal domIn Bool ->
  Clock domOut
gatedClockModel clk ena =
  Clock SSymbol (Just (Femtoseconds <$> periods passes (inPeriods clk)))
 where
  -- The enable register samples on the falling edge between input edge /k-1/
  -- and /k/, which sees enable sample /k/ (the value present before edge /k/)
  passes = True : P.drop 1 (sampleList ena)

  sampleList (x :- xs) = x : sampleList xs

  inPeriods :: Clock domIn -> [Int64]
  inPeriods (Clock _ (Just ps)) = (\(Femtoseconds fs) -> fs) <$> sampleList ps
  inPeriods (Clock _ Nothing) = P.repeat (1000 * snatToNum (clockPeriod @domIn))

  -- Periods between the edges that pass. The first edge always passes.
  periods :: [Bool] -> [Int64] -> Signal domOut Int64
  periods (_ : ps) (t : ts) = go t ps ts
  periods _ _ = error "altclkctrl: unreachable"

  -- Time since the last edge that passed, whether the next edges pass, and
  -- the periods of the input clock after the next edge
  go acc (p : ps) (t : ts)
    | p = acc :- go t ps ts
    | otherwise = go (acc + t) ps ts
  go _ _ _ = error "altclkctrl: unreachable"

-- | Black box of 'altclkctrl' for Cyclone IV E
cycloneiveClkctrl ::
  forall domIn domOut.
  (KnownDomain domIn, KnownDomain domOut) =>
  Clock domIn ->
  Signal domIn Bool ->
  Clock domOut
cycloneiveClkctrl = gatedClockModel
{-# OPAQUE cycloneiveClkctrl #-}
{-# ANN cycloneiveClkctrl hasBlackBox #-}
{-# ANN cycloneiveClkctrl (InlineYamlPrimitive [Verilog, SystemVerilog] (verilogTemplate "Clash.Cores.Intel.Clock.cycloneiveClkctrl" "cycloneive_clkctrl")) #-}
{-# ANN cycloneiveClkctrl (InlineYamlPrimitive [VHDL] (vhdlTemplate "Clash.Cores.Intel.Clock.cycloneiveClkctrl" "cycloneive")) #-}

-- | Black box of 'altclkctrl' for Cyclone 10 LP
cyclone10lpClkctrl ::
  forall domIn domOut.
  (KnownDomain domIn, KnownDomain domOut) =>
  Clock domIn ->
  Signal domIn Bool ->
  Clock domOut
cyclone10lpClkctrl = gatedClockModel
{-# OPAQUE cyclone10lpClkctrl #-}
{-# ANN cyclone10lpClkctrl hasBlackBox #-}
{-# ANN cyclone10lpClkctrl (InlineYamlPrimitive [Verilog, SystemVerilog] (verilogTemplate "Clash.Cores.Intel.Clock.cyclone10lpClkctrl" "cyclone10lp_clkctrl")) #-}
{-# ANN cyclone10lpClkctrl (InlineYamlPrimitive [VHDL] (vhdlTemplate "Clash.Cores.Intel.Clock.cyclone10lpClkctrl" "cyclone10lp")) #-}

-- | Black box of 'altclkctrl' for MAX 10
fiftyfivenmClkctrl ::
  forall domIn domOut.
  (KnownDomain domIn, KnownDomain domOut) =>
  Clock domIn ->
  Signal domIn Bool ->
  Clock domOut
fiftyfivenmClkctrl = gatedClockModel
{-# OPAQUE fiftyfivenmClkctrl #-}
{-# ANN fiftyfivenmClkctrl hasBlackBox #-}
{-# ANN fiftyfivenmClkctrl (InlineYamlPrimitive [Verilog, SystemVerilog] (verilogTemplate "Clash.Cores.Intel.Clock.fiftyfivenmClkctrl" "fiftyfivenm_clkctrl")) #-}
{-# ANN fiftyfivenmClkctrl (InlineYamlPrimitive [VHDL] (vhdlTemplate "Clash.Cores.Intel.Clock.fiftyfivenmClkctrl" "fiftyfivenm")) #-}
