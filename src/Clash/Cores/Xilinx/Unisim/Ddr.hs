{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

DDR input and output registers of UltraScale and UltraScale+ devices. For more
information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/I/O-Components)
and the /UltraScale Architecture SelectIO Resources/ user guide (UG571).

These functions follow the conventions of "Clash.Xilinx.DDR" and
"Clash.Explicit.DDR": the DDR signal lives in a domain @domDDR@ with half the
period of the domain @dom@ of the clock. A sample of a signal is the value it
has at an active edge of its clock, so for every cycle of @dom@ there are two
samples of the DDR signal: one at the rising edge and one at the falling edge
of the clock. Pairs are ordered in time: the first element is the bit that is
on the DDR signal first.
-}
module Clash.Cores.Xilinx.Unisim.Ddr
  ( -- * Input
    iddre1
  , IddrMode (..)
    -- * Output
  , oddre1
  , Oddre1Config (..)
  , oddre1DefaultConfig
  , SimDevice (..)
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal ((:-)), asyncRegister#)

import Clash.Cores.Xilinx.Unisim.Internal (SimDevice (..), simDeviceString, unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (ClockPort (..), Param (..), Port (..), ResetPort (..), inst, unPort)

{- | Value of the @DDR_CLK_EDGE@ attribute of 'iddre1': how the two bits
captured in a clock cycle are presented to the fabric. Let @d0, d1, d2, ...@
be the samples of the DDR input, where @d(2k)@ is captured at rising edge /k/
of the clock and @d(2k+1)@ at the falling edge after it. Then sample /k/ of
the output of 'iddre1' is:

+-----------------------+--------------------------+-----------+-----------+
| Mode                  | Output at sample /k/     | First bit | Second bit|
+=======================+==========================+===========+===========+
| 'OppositeEdge'        | @(d(2k-2), d(2k-1))@     | @Q1@      | @Q2@      |
+-----------------------+--------------------------+-----------+-----------+
| 'SameEdge'            | @(d(2k-3), d(2k-2))@     | @Q2@      | @Q1@      |
+-----------------------+--------------------------+-----------+-----------+
| 'SameEdgePipelined'   | @(d(2k-4), d(2k-3))@     | @Q1@      | @Q2@      |
+-----------------------+--------------------------+-----------+-----------+

In 'OppositeEdge' mode, the primitive's @Q2@ output changes at the falling
edge of the clock, giving logic in the fabric only half a clock cycle to use
it. 'SameEdge' corresponds to what "Clash.Xilinx.DDR" uses for 7-series
devices.
-}
data IddrMode
  = OppositeEdge
  | SameEdge
  | SameEdgePipelined
  deriving (Show, Eq, Generic, NFDataX)

-- | Value of the @DDR_CLK_EDGE@ attribute
iddrModeString :: IddrMode -> String
iddrModeString OppositeEdge = "OPPOSITE_EDGE"
iddrModeString SameEdge = "SAME_EDGE"
iddrModeString SameEdgePipelined = "SAME_EDGE_PIPELINED"

{- | Dedicated input DDR register (@IDDRE1@). Captures the DDR input at both
edges of the clock and returns the bits as a pair, ordered in time. See
'IddrMode' for the latency of the different modes.

The reset (@R@) is asynchronous and active-high. While it is asserted, both
outputs are 0. The primitive's clock inputs @C@ and @CB@ are both connected to
the clock, with @CB@ inverted inside the primitive (@IS_CB_INVERTED = 1@).

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IDDRE1).
-}
iddre1 ::
  forall dom domDDR.
  ( KnownDomain dom
  , KnownDomain domDDR
  , DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  , DomainActiveEdge dom ~ 'Rising
  ) =>
  IddrMode ->
  Clock dom ->
  -- | Asynchronous reset (@R@)
  Reset dom ->
  -- | DDR input (@D@)
  Signal domDDR Bit ->
  -- | Bits captured in a clock cycle, ordered in time
  Signal dom (Bit, Bit)
iddre1 mode clk rst d
  | clashSimulation = sim
  | otherwise = bundle (first, second)
 where
  sim = case mode of
    OppositeEdge -> bundle (q1, q2)
    SameEdge -> bundle (q2Same, q1)
    SameEdgePipelined -> bundle (q1Pipelined, q2Same)

  -- Bits captured at the rising and falling edges of cycle /k/, as samples
  -- of @dom@
  (risingBits, fallingBits) = unbundle (pairs d)

  rstBool = unsafeToActiveHigh rst
  asyncReg = asyncRegister# clk rst enableGen 0 0

  q1 = asyncReg risingBits
  q1Pipelined = asyncReg q1
  -- Registered at the falling edge, so at the next rising edge it holds the
  -- bit captured in the previous cycle, unless the reset is asserted by then.
  q2 = mux rstBool 0 (delay clk enableGen 0 fallingBits)
  q2Same = asyncReg q2

  (first, second) = case mode of
    SameEdge -> (q2Out, q1Out)
    _ -> (q1Out, q2Out)

  (q1Out, q2Out) = (unPort q1Port, unPort q2Port)
  (q1Port, q2Port) = go
  go :: (Port "Q1" dom Bit, Port "Q2" dom Bit)
  go =
    inst
      (unisim "IDDRE1")
      (Param @"DDR_CLK_EDGE" @String (iddrModeString mode))
      (Param @"IS_CB_INVERTED" @Bit 1)
      (Param @"IS_C_INVERTED" @Bit 0)
      (ClockPort @"C" clk)
      (ClockPort @"CB" clk)
      (Port @"D" d)
      (ResetPort @"R" @'ActiveHigh rst)

-- | Pairs of consecutive samples of a DDR signal, one pair per cycle of @dom@
pairs ::
  forall dom domDDR a.
  ( KnownDomain dom
  , KnownDomain domDDR
  , DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  ) =>
  Signal domDDR a ->
  Signal dom (a, a)
pairs (a :- ~(b :- abs0)) = (a, b) :- pairs abs0

-- | Configuration of 'oddre1'
data Oddre1Config = Oddre1Config
  { oddre1SimDevice :: SimDevice
  -- ^ Device family (@SIM_DEVICE@). The UltraScale and UltraScale+ primitives
  -- behave the same.
  , oddre1ResetValue :: Bit
  -- ^ Value of the output while the reset is asserted, and at the start of
  -- the simulation (@SRVAL@).
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'oddre1': an UltraScale device, and a reset
-- value of 0
oddre1DefaultConfig :: Oddre1Config
oddre1DefaultConfig =
  Oddre1Config
    { oddre1SimDevice = UltraScale
    , oddre1ResetValue = 0
    }

{- | Dedicated output DDR register (@ODDRE1@). Of the input pair, the first bit
is output during the first half of the next clock cycle, and the second bit
during the second half. Put in terms of samples: let @(a(k), b(k))@ be sample
/k/ of the input, then the output is @r, a(0), b(0), a(1), b(1), ...@, where
@r@ is the reset value. This matches 'Clash.Xilinx.DDR.oddr'.

The reset (@SR@) asserts asynchronously and deasserts synchronously: the
primitive keeps the output at its reset value until three rising clock edges
after the reset is deasserted. Put in terms of samples: if the reset is
asserted at sample /k/, the outputs belonging to input samples /k - 1/ up to
and including /k + 3/ are replaced by the reset value.

To forward a clock, output @(1, 0)@ every cycle.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/ODDRE1).
-}
oddre1 ::
  forall dom domDDR.
  ( KnownDomain dom
  , KnownDomain domDDR
  , DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  , DomainActiveEdge dom ~ 'Rising
  ) =>
  Oddre1Config ->
  Clock dom ->
  -- | Reset (@SR@)
  Reset dom ->
  -- | Bits to output, ordered in time (@D1@, @D2@)
  Signal dom (Bit, Bit) ->
  -- | DDR output (@Q@)
  Signal domDDR Bit
oddre1 config clk rst d
  | clashSimulation = rstVal :- go (False, False, False) (unsafeToActiveHigh rst) d
  | otherwise = unPort q
 where
  rstVal = oddre1ResetValue config

  -- @r1@, @r2@ and @r3@ are the reset values at the previous three samples.
  -- At sample /k/, the reset holds the output belonging to input sample /k/ if
  -- it is asserted at sample /k/ up to and including /k + 4/.
  go (r1, r2, r3) (r0 :- rs@(~(rNext :- _))) ((a, b) :- ds) =
    let held = r0 || r1 || r2 || r3 || rNext
     in (if held then rstVal else a)
          :- (if held then rstVal else b)
          :- go (r0, r1, r2) rs ds
  -- Note that @held@ above also depends on the reset at sample /k + 1/: the
  -- reset asserts asynchronously, during the clock cycle in which the
  -- output belonging to input sample /k/ is on the DDR output.

  (dFirst, dSecond) = unbundle d
  q :: Port "Q" domDDR Bit
  q =
    inst
      (unisim "ODDRE1")
      (Param @"IS_C_INVERTED" @Bit 0)
      (Param @"IS_D1_INVERTED" @Bit 0)
      (Param @"IS_D2_INVERTED" @Bit 0)
      (Param @"SIM_DEVICE" @String (simDeviceString (oddre1SimDevice config)))
      (Param @"SRVAL" @Bit rstVal)
      (ClockPort @"C" clk)
      (Port @"D1" dFirst)
      (Port @"D2" dSecond)
      (ResetPort @"SR" @'ActiveHigh rst)
