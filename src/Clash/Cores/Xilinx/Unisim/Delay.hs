{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Input and output delay lines of UltraScale and UltraScale+ devices, and their
controller. For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/I/O-Components)
and the /UltraScale Architecture SelectIO Resources/ user guide (UG571).

= Simulation

A delay line delays a signal by a fraction of a clock cycle: at most 512 taps
of a few picoseconds each. Clash simulation can't express such delays, so the
simulation models pass the data through unchanged (zero delay). What they do
model is the tap counter: the tap value reported by the primitive
(@CNTVALUEOUT@) after loads, increments, decrements and resets, cycle for cycle
as the UNISIM Verilog simulation models do.

The UNISIM simulation models have some quirks that the Clash models don't
reproduce:

  * The VHDL models report an undefined tap value while @EN_VTC@ is asserted.

  * The Verilog models report an undefined tap value after a reset (or the
    global set/reset at the start of a simulation) when simulated in Vivado's
    simulator, until a tap value is loaded. The Clash models report
    'delayValue', which is what the hardware is reset to (UG571).

  * The VHDL models start accepting commands one clock cycle earlier after a
    reset than the Verilog models.

= Delay formats

In 'DelayTime' format, the delay is specified in picoseconds and the primitive
calibrates its taps using the reference clock of an 'idelayctrl' in the same
I/O bank. The tap count reported by the UNISIM models, and hence by the Clash
models, is the delay divided by the nominal tap delay; the hardware reports
the calibrated value. In 'DelayCount' format, the delay is specified in taps and no
'idelayctrl' is needed.

The primitives' @REFCLK_FREQUENCY@ attribute, which must match the frequency of
the 'idelayctrl' reference clock in 'DelayTime' format, is a real number. Real-valued
attributes can't be passed through the instantiation mechanism these wrappers
use, so it is left at its default of 300 MHz. To use another reference clock
frequency, set the attribute in your constraints file, for example:

> set_property REFCLK_FREQUENCY 400.0 [get_cells -hierarchical -filter {REF_NAME == IDELAYE3}]
-}
{-# LANGUAGE LambdaCase #-}

module Clash.Cores.Xilinx.Unisim.Delay
  ( -- * Delay lines
    idelaye3
  , odelaye3
  , DelayConfig (..)
  , delayDefaultConfig
  , DelayFormat (..)
  , DelayType (..)
  , UpdateMode (..)
  , DelaySource (..)
  , DelayCommand (..)
    -- * Delay controller
  , idelayctrl
    -- * Common
  , SimDevice (..)
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Internal (SimDevice (..), simDeviceString, unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (ClockPort (..), Param (..), Port (..), ResetPort (..), inst, unPort)

-- | Unit of 'delayValue' (@DELAY_FORMAT@)
data DelayFormat
  = -- | Picoseconds. Needs an 'idelayctrl'.
    DelayTime
  | -- | Taps
    DelayCount
  deriving (Show, Eq, Generic, NFDataX)

-- | Whether and how the delay can be changed at run time (@DELAY_TYPE@)
data DelayType
  = -- | The delay is fixed to 'delayValue'. Commands are ignored.
    DelayFixed
  | -- | The delay can be loaded ('DelayLoad'), incremented and decremented
    DelayVarLoad
  | -- | The delay can be incremented and decremented. 'DelayLoad' is not
    -- allowed.
    DelayVariable
  deriving (Show, Eq, Generic, NFDataX)

-- | When a new tap value takes effect on the data path (@UPDATE_MODE@). This
-- only affects the data path, which Clash simulation doesn't delay.
data UpdateMode
  = -- | Immediately
    UpdateAsync
  | -- | After the data has toggled a few times
    UpdateSync
  deriving (Show, Eq, Generic, NFDataX)

-- | Input of an 'idelaye3' (@DELAY_SRC@)
data DelaySource
  = -- | An input buffer (@IDATAIN@)
    FromInputBuffer
  | -- | The fabric (@DATAIN@)
    FromFabric
  deriving (Show, Eq, Generic, NFDataX)

-- | Configuration of 'idelaye3' and 'odelaye3'
data DelayConfig = DelayConfig
  { delayFormat :: DelayFormat
  -- ^ Unit of 'delayValue' (@DELAY_FORMAT@)
  , delayType :: DelayType
  -- ^ Whether the delay can be changed (@DELAY_TYPE@)
  , delayValue :: Integer
  -- ^ Initial delay (@DELAY_VALUE@): picoseconds in 'DelayTime' format (at most
  -- 1100 on UltraScale and 1250 on UltraScale+ devices), taps in 'DelayCount'
  -- format (at most 511). The tap value is reset to this value.
  , delayUpdateMode :: UpdateMode
  -- ^ When a new tap value takes effect (@UPDATE_MODE@)
  , delaySimDevice :: SimDevice
  -- ^ Device family (@SIM_DEVICE@)
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration: a variable delay in taps, starting at 0, on an
-- UltraScale device. This needs no 'idelayctrl'.
delayDefaultConfig :: DelayConfig
delayDefaultConfig =
  DelayConfig
    { delayFormat = DelayCount
    , delayType = DelayVarLoad
    , delayValue = 0
    , delayUpdateMode = UpdateAsync
    , delaySimDevice = UltraScale
    }

-- | Command for the tap counter of a delay line, issued at a clock edge
data DelayCommand
  = -- | Keep the tap value
    DelayHold
  | -- | Add one tap, wrapping around from 511 to 0 (@CE = 1, INC = 1@)
    DelayIncrement
  | -- | Remove one tap, wrapping around from 0 to 511 (@CE = 1, INC = 0@)
    DelayDecrement
  | -- | Load a tap value (@LOAD = 1@, @CNTVALUEIN@). Only for 'DelayVarLoad'.
    DelayLoad (Unsigned 9)
  deriving (Show, Eq, Generic, NFDataX, BitPack)

-- | Ports @LOAD@, @CE@, @INC@ and @CNTVALUEIN@ for a 'DelayCommand'
commandPorts :: DelayCommand -> (Bit, Bit, Bit, BitVector 9)
commandPorts = \case
  DelayHold -> (0, 0, 0, 0)
  DelayIncrement -> (0, 1, 1, 0)
  DelayDecrement -> (0, 1, 0, 0)
  DelayLoad n -> (1, 0, 0, pack n)

-- | Tap value after a reset. In 'DelayTime' format, the UNISIM models divide the
-- delay by the nominal tap delay.
initialTaps ::
  -- | Nominal tap delay in picoseconds
  Integer ->
  DelayConfig ->
  Unsigned 9
initialTaps tapDelay cfg = case delayFormat cfg of
  DelayCount -> fromInteger (delayValue cfg)
  DelayTime -> fromInteger (delayValue cfg `quot` tapDelay)

{- | Model of the tap counter of @IDELAYE3@ and @ODELAYE3@. The reset is
asynchronous, and the counter ignores commands while the reset is asserted
and during the three clock cycles after it is deasserted.
-}
tapCounter ::
  forall dom.
  (KnownDomain dom) =>
  DelayType ->
  -- | Tap value after a reset
  Unsigned 9 ->
  Clock dom ->
  Reset dom ->
  -- | @EN_VTC@
  Signal dom Bool ->
  Signal dom DelayCommand ->
  Signal dom (Unsigned 9)
tapCounter typ taps0 clk rst enVtc cmd = taps
 where
  rstBool = unsafeToActiveHigh rst
  -- Reset at the previous three clock edges, as seen by the reset synchronizer
  rstHistory :: Signal dom (Vec 3 Bool)
  rstHistory = delay clk enableGen (repeat False) ((+>>) <$> rstBool <*> rstHistory)
  enabled = (\r h -> not (r || or h)) <$> rstBool <*> rstHistory

  taps = mux rstBool (pure taps0) tapsReg
  tapsReg = delay clk enableGen taps0 (update <$> enabled <*> enVtc <*> cmd <*> taps)

  update en vtc c n
    | not en || vtc = n
    | otherwise = case (typ, c) of
        (DelayFixed, _) -> n
        (_, DelayHold) -> n
        (_, DelayIncrement) -> n + 1
        (_, DelayDecrement) -> n - 1
        (DelayVarLoad, DelayLoad m) -> m
        (DelayVariable, DelayLoad _) -> n

delayTypeString :: DelayType -> String
delayTypeString = \case
  DelayFixed -> "FIXED"
  DelayVarLoad -> "VAR_LOAD"
  DelayVariable -> "VARIABLE"

delayFormatString :: DelayFormat -> String
delayFormatString = \case
  DelayTime -> "TIME"
  DelayCount -> "COUNT"

updateModeString :: UpdateMode -> String
updateModeString = \case
  UpdateAsync -> "ASYNC"
  UpdateSync -> "SYNC"

{- | Input delay line (@IDELAYE3@). Returns the delayed data, which Clash
simulation doesn't delay, and the tap value (@CNTVALUEOUT@).

Commands are executed at the rising edges of the clock, while @EN_VTC@ is
deasserted. @EN_VTC@ enables the voltage and temperature compensation of a
delay in 'DelayTime' format; it must be deasserted to change the tap value. The
reset (@RST@) is active-high and asynchronous; it sets the tap value back to
'delayValue'. Commands are ignored during the three clock cycles after the
reset is deasserted.

The data input and output may be in any domain. In 'DelayTime' format, an
'idelayctrl' in the same I/O bank must be instantiated as well. Cascading
(@CASCADE@) and loopback are not supported, nor is the @MANUAL@ update mode.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IDELAYE3).
-}
idelaye3 ::
  forall domCtrl domData.
  (KnownDomain domCtrl, KnownDomain domData) =>
  -- | Where the data comes from (@DELAY_SRC@)
  DelaySource ->
  DelayConfig ->
  -- | Clock (@CLK@)
  Clock domCtrl ->
  -- | Reset (@RST@)
  Reset domCtrl ->
  -- | Enable voltage and temperature compensation (@EN_VTC@)
  Signal domCtrl Bool ->
  -- | Tap counter command (@LOAD@, @CE@, @INC@, @CNTVALUEIN@)
  Signal domCtrl DelayCommand ->
  -- | Data (@IDATAIN@ or @DATAIN@)
  Signal domData Bit ->
  -- | Delayed data (@DATAOUT@) and tap value (@CNTVALUEOUT@)
  (Signal domData Bit, Signal domCtrl (Unsigned 9))
idelaye3 src cfg clk rst enVtc cmd dataIn
  | clashSimulation = (dataIn, tapCounter (delayType cfg) taps0 clk rst enVtc cmd)
  | otherwise = (unPort dataOut, unpack <$> unPort cntValueOut)
 where
  taps0 = initialTaps (if delaySimDevice cfg == UltraScale then 5 else 4) cfg
  (load, ce, inc, cntValueIn) = unbundle (commandPorts <$> cmd)
  zero = pure 0 :: Signal domData Bit
  (idataIn, datain) = case src of
    FromInputBuffer -> (dataIn, zero)
    FromFabric -> (zero, dataIn)

  (_cascOut, cntValueOut, dataOut) = go
  go ::
    ( Port "CASC_OUT" domData Bit
    , Port "CNTVALUEOUT" domCtrl (BitVector 9)
    , Port "DATAOUT" domData Bit
    )
  go =
    inst
      (unisim "IDELAYE3")
      (Param @"CASCADE" @String "NONE")
      (Param @"DELAY_FORMAT" @String (delayFormatString (delayFormat cfg)))
      (Param @"DELAY_SRC" @String (case src of FromInputBuffer -> "IDATAIN"; FromFabric -> "DATAIN"))
      (Param @"DELAY_TYPE" @String (delayTypeString (delayType cfg)))
      (Param @"DELAY_VALUE" @Integer (delayValue cfg))
      (Param @"IS_CLK_INVERTED" @Bit 0)
      (Param @"IS_RST_INVERTED" @Bit 0)
      (Param @"LOOPBACK" @String "FALSE")
      (Param @"SIM_DEVICE" @String (simDeviceString (delaySimDevice cfg)))
      (Param @"UPDATE_MODE" @String (updateModeString (delayUpdateMode cfg)))
      (Port @"CASC_IN" zero)
      (Port @"CASC_RETURN" zero)
      (Port @"CE" ce)
      (ClockPort @"CLK" clk)
      (Port @"CNTVALUEIN" cntValueIn)
      (Port @"DATAIN" datain)
      (Port @"EN_VTC" (boolToBit <$> enVtc))
      (Port @"IDATAIN" idataIn)
      (Port @"INC" inc)
      (Port @"LOAD" load)
      (ResetPort @"RST" @'ActiveHigh rst)

{- | Output delay line (@ODELAYE3@). Like 'idelaye3', but for a signal going to
an output buffer (@ODATAIN@ to @DATAOUT@). In 'DelayTime' format, the UNISIM model
of @ODELAYE3@ uses a nominal tap delay of 5 ps on all devices.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/ODELAYE3).
-}
odelaye3 ::
  forall domCtrl domData.
  (KnownDomain domCtrl, KnownDomain domData) =>
  DelayConfig ->
  -- | Clock (@CLK@)
  Clock domCtrl ->
  -- | Reset (@RST@)
  Reset domCtrl ->
  -- | Enable voltage and temperature compensation (@EN_VTC@)
  Signal domCtrl Bool ->
  -- | Tap counter command (@LOAD@, @CE@, @INC@, @CNTVALUEIN@)
  Signal domCtrl DelayCommand ->
  -- | Data (@ODATAIN@)
  Signal domData Bit ->
  -- | Delayed data (@DATAOUT@) and tap value (@CNTVALUEOUT@)
  (Signal domData Bit, Signal domCtrl (Unsigned 9))
odelaye3 cfg clk rst enVtc cmd dataIn
  | clashSimulation = (dataIn, tapCounter (delayType cfg) taps0 clk rst enVtc cmd)
  | otherwise = (unPort dataOut, unpack <$> unPort cntValueOut)
 where
  taps0 = initialTaps 5 cfg
  (load, ce, inc, cntValueIn) = unbundle (commandPorts <$> cmd)
  zero = pure 0 :: Signal domData Bit

  (_cascOut, cntValueOut, dataOut) = go
  go ::
    ( Port "CASC_OUT" domData Bit
    , Port "CNTVALUEOUT" domCtrl (BitVector 9)
    , Port "DATAOUT" domData Bit
    )
  go =
    inst
      (unisim "ODELAYE3")
      (Param @"CASCADE" @String "NONE")
      (Param @"DELAY_FORMAT" @String (delayFormatString (delayFormat cfg)))
      (Param @"DELAY_TYPE" @String (delayTypeString (delayType cfg)))
      (Param @"DELAY_VALUE" @Integer (delayValue cfg))
      (Param @"IS_CLK_INVERTED" @Bit 0)
      (Param @"IS_RST_INVERTED" @Bit 0)
      (Param @"SIM_DEVICE" @String (simDeviceString (delaySimDevice cfg)))
      (Param @"UPDATE_MODE" @String (updateModeString (delayUpdateMode cfg)))
      (Port @"CASC_IN" zero)
      (Port @"CASC_RETURN" zero)
      (Port @"CE" ce)
      (ClockPort @"CLK" clk)
      (Port @"CNTVALUEIN" cntValueIn)
      (Port @"EN_VTC" (boolToBit <$> enVtc))
      (Port @"INC" inc)
      (Port @"LOAD" load)
      (Port @"ODATAIN" dataIn)
      (ResetPort @"RST" @'ActiveHigh rst)

{- | Delay controller (@IDELAYCTRL@). Calibrates the delay lines in its I/O bank
that use the 'DelayTime' format, using a reference clock (@REFCLK@). Returns
whether the delay lines are calibrated (@RDY@).

The reset (@RST@) is active-high and asynchronous. UG571 prescribes a reset
after the reference clock is stable. Simulation: the controller is ready from
the third clock edge at which the reset is deasserted on, as long as the reset
stays deasserted. Like the UNISIM model, the Clash model doesn't model losing
the reference clock.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IDELAYCTRL).
-}
idelayctrl ::
  forall dom.
  (KnownDomain dom) =>
  -- | Reference clock (@REFCLK@)
  Clock dom ->
  -- | Reset (@RST@)
  Reset dom ->
  -- | Ready (@RDY@)
  Signal dom Bool
idelayctrl clk rst
  | clashSimulation = (&&) <$> (not <$> rstBool) <*> measured
  | otherwise = bitToBool <$> unPort rdy
 where
  rstBool = unsafeToActiveHigh rst
  -- Whether the reference clock had two rising edges at which the reset was
  -- deasserted, which the controller needs to measure its period
  edges :: Signal dom (Index 3)
  edges = delay clk enableGen 0 (next <$> rstBool <*> edges)
  next r n = if r then n else satSucc SatBound n
  measured = (== 2) <$> edges

  rdy :: Port "RDY" dom Bit
  rdy =
    inst
      (unisim "IDELAYCTRL")
      (Param @"SIM_DEVICE" @String "ULTRASCALE")
      (ClockPort @"REFCLK" clk)
      (ResetPort @"RST" @'ActiveHigh rst)
