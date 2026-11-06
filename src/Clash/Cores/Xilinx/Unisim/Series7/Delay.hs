{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Input and output delay lines of 7 series devices (Artix-7, Kintex-7,
Virtex-7, Spartan-7 and Zynq-7000), and their controller. For more information
see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/IDELAYE2)
and the /7 Series FPGAs SelectIO Resources/ user guide (UG471).

For the UltraScale counterparts see "Clash.Cores.Xilinx.Unisim.Delay". The
types of this module have the same names as the ones there, but are different
types: the 7 series delay lines have 32 taps and no reset, and their modes
differ slightly.

= Simulation

A delay line delays a signal by a fraction of a clock cycle: at most 31 taps of
78, 52 or 39 ps each (for a reference clock of 200, 300 or 400 MHz). Clash
simulation can't express such delays, so the simulation models pass the data
through unchanged (zero delay). What they do model is the tap counter: the tap
value reported by the primitive (@CNTVALUEOUT@) after loads, increments and
decrements, cycle for cycle as the UNISIM simulation models do.

At the start of a simulation, the UNISIM models ignore commands during the
global set/reset (the first 100 ns). The Clash models don't; issue commands
after that. The VHDL models also take the tap value presented on @CNTVALUEIN@
during the global set/reset as the starting point of the first command after
it, in 'DelayVarLoad' mode; the wrappers present 0 unless a 'DelayLoad' is
issued.

= Delay controller

The tap delay of the 7 series delay lines is always calibrated by an
@IDELAYCTRL@ in the same I/O bank, so one must be instantiated whenever a delay
line is used. 'idelayctrl', shared with the UltraScale wrappers, is that
controller: its @SIM_DEVICE@ attribute (@\"ULTRASCALE\"@) only changes an
informational message of the UNISIM simulation models and is ignored by
implementation, so it works for 7 series devices as is.

The primitives' @REFCLK_FREQUENCY@ attribute, which must match the frequency of
the 'idelayctrl' reference clock, is a real number. Real-valued attributes can't
be passed through the instantiation mechanism these wrappers use, so it is left
at its default of 200 MHz. To use another reference clock frequency (190 to 210,
290 to 310 or 390 to 410 MHz, depending on the speed grade), set the attribute
in your constraints file, for example:

> set_property REFCLK_FREQUENCY 300.0 [get_cells -hierarchical -filter {REF_NAME == IDELAYE2}]

= Not supported

The @VAR_LOAD_PIPE@ mode, in which a tap value is first written to a pipeline
register (@LDPIPEEN@, @REGRST@) and loaded later, is not supported. It is
meant for loading many delay lines from a shared bus at the same time; a
fabric register in front of 'DelayLoad' in 'DelayVarLoad' mode does the same.
Neither is dynamic inversion of the clock (@CINVCTRL@): UG471 restricts it to
applications where glitches don't matter. The @ODELAYE2@ clock input
(@CLKIN@, for delaying a clock) isn't supported either. Note that @ODELAYE2@ is
only available in high-performance (HP) I/O banks, so not in Artix-7 and
Spartan-7 devices.
-}
{-# LANGUAGE LambdaCase #-}

module Clash.Cores.Xilinx.Unisim.Series7.Delay
  ( -- * Delay lines
    idelaye2
  , odelaye2
  , DelayConfig (..)
  , delayDefaultConfig
  , DelayType (..)
  , SignalPattern (..)
  , DelaySource (..)
  , DelayCommand (..)
    -- * Delay controller
  , idelayctrl
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Delay (DelaySource (..), idelayctrl)
import Clash.Cores.Xilinx.Unisim.Internal (unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (ClockPort (..), Param (..), Port (..), inst, unPort)

-- | Whether and how the delay can be changed at run time (@IDELAY_TYPE@,
-- @ODELAY_TYPE@)
data DelayType
  = -- | The delay is fixed to 'delayValue'. Commands are ignored.
    DelayFixed
  | -- | The delay starts at 'delayValue' and can be incremented and
    -- decremented. 'DelayLoad' sets it back to 'delayValue'.
    DelayVariable
  | -- | The delay starts at 0 and can be loaded ('DelayLoad'), incremented and
    -- decremented. 'delayValue' is not used.
    DelayVarLoad
  deriving (Show, Eq, Generic, NFDataX)

-- | Kind of signal that is delayed (@SIGNAL_PATTERN@). Only affects timing
-- analysis, which accounts for more jitter on data.
data SignalPattern
  = -- | Data
    PatternData
  | -- | A clock
    PatternClock
  deriving (Show, Eq, Generic, NFDataX)

-- | Configuration of 'idelaye2' and 'odelaye2'
data DelayConfig = DelayConfig
  { delayType :: DelayType
  -- ^ Whether the delay can be changed (@IDELAY_TYPE@, @ODELAY_TYPE@)
  , delayValue :: Unsigned 5
  -- ^ Initial delay in taps (@IDELAY_VALUE@, @ODELAY_VALUE@), see 'DelayType'
  , delayHighPerformance :: Bool
  -- ^ Reduce jitter at the cost of power (@HIGH_PERFORMANCE_MODE@)
  , delaySignalPattern :: SignalPattern
  -- ^ Kind of signal that is delayed (@SIGNAL_PATTERN@)
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration: a variable delay starting at 0 taps, delaying data,
-- in high-performance mode
delayDefaultConfig :: DelayConfig
delayDefaultConfig =
  DelayConfig
    { delayType = DelayVarLoad
    , delayValue = 0
    , delayHighPerformance = True
    , delaySignalPattern = PatternData
    }

-- | Command for the tap counter of a delay line, issued at a clock edge
data DelayCommand
  = -- | Keep the tap value
    DelayHold
  | -- | Add one tap, wrapping around from 31 to 0 (@CE = 1, INC = 1@)
    DelayIncrement
  | -- | Remove one tap, wrapping around from 0 to 31 (@CE = 1, INC = 0@)
    DelayDecrement
  | -- | Load a tap value (@LD = 1@, @CNTVALUEIN@). In 'DelayVariable' mode
    -- this sets the tap value back to 'delayValue' instead, ignoring the
    -- argument.
    DelayLoad (Unsigned 5)
  deriving (Show, Eq, Generic, NFDataX, BitPack)

-- | Ports @LD@, @CE@, @INC@ and @CNTVALUEIN@ for a 'DelayCommand'. Only
-- 'DelayVarLoad' uses @CNTVALUEIN@; Vivado warns when it is driven otherwise.
commandPorts :: DelayType -> DelayCommand -> (Bit, Bit, Bit, BitVector 5)
commandPorts typ = \case
  DelayHold -> (0, 0, 0, 0)
  DelayIncrement -> (0, 1, 1, 0)
  DelayDecrement -> (0, 1, 0, 0)
  DelayLoad n -> (1, 0, 0, if typ == DelayVarLoad then pack n else 0)

{- | Model of the tap counter of @IDELAYE2@ and @ODELAYE2@: a register updated
at every rising clock edge. The counter has no reset.
-}
tapCounter ::
  forall dom.
  (KnownDomain dom) =>
  DelayConfig ->
  Clock dom ->
  Signal dom DelayCommand ->
  Signal dom (Unsigned 5)
tapCounter cfg clk cmd = case delayType cfg of
  DelayFixed -> pure (delayValue cfg)
  DelayVariable -> taps (delayValue cfg)
  DelayVarLoad -> taps 0
 where
  taps taps0 = n
   where
    n = delay clk enableGen taps0 (update <$> cmd <*> n)

  update c n = case c of
    DelayHold -> n
    DelayIncrement -> n + 1
    DelayDecrement -> n - 1
    DelayLoad m
      | delayType cfg == DelayVarLoad -> m
      | otherwise -> delayValue cfg

delayTypeString :: DelayType -> String
delayTypeString = \case
  DelayFixed -> "FIXED"
  DelayVariable -> "VARIABLE"
  DelayVarLoad -> "VAR_LOAD"

signalPatternString :: SignalPattern -> String
signalPatternString = \case
  PatternData -> "DATA"
  PatternClock -> "CLOCK"

{- | Input delay line (@IDELAYE2@). Returns the delayed data, which Clash
simulation doesn't delay, and the tap value (@CNTVALUEOUT@).

Commands are executed at the rising edges of the clock (@C@). In 'DelayFixed'
mode the clock is not used, and the tap value is 'delayValue'. The delay line
has no reset.

The data input and output may be in any domain. An 'idelayctrl' in the same
I/O bank must be instantiated as well. To feed the delayed data into an
'Clash.Cores.Xilinx.Unisim.Series7.Serdes.iserdese2', configure the latter with
'Clash.Cores.Xilinx.Unisim.Series7.Serdes.InputDdly'.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/IDELAYE2)
and UG471.
-}
idelaye2 ::
  forall domCtrl domData.
  (KnownDomain domCtrl, KnownDomain domData) =>
  -- | Where the data comes from (@DELAY_SRC@)
  DelaySource ->
  DelayConfig ->
  -- | Clock (@C@)
  Clock domCtrl ->
  -- | Tap counter command (@LD@, @CE@, @INC@, @CNTVALUEIN@)
  Signal domCtrl DelayCommand ->
  -- | Data (@IDATAIN@ or @DATAIN@)
  Signal domData Bit ->
  -- | Delayed data (@DATAOUT@) and tap value (@CNTVALUEOUT@)
  (Signal domData Bit, Signal domCtrl (Unsigned 5))
idelaye2 src cfg clk cmd dataIn
  | clashSimulation = (dataIn, tapCounter cfg clk cmd)
  | otherwise = (unPort dataOut, unpack <$> unPort cntValueOut)
 where
  (ld, ce, inc, cntValueIn) = unbundle (commandPorts (delayType cfg) <$> cmd)
  zero = pure 0 :: Signal domData Bit
  zeroCtrl = pure 0 :: Signal domCtrl Bit
  (idataIn, datain) = case src of
    FromInputBuffer -> (dataIn, zero)
    FromFabric -> (zero, dataIn)

  (cntValueOut, dataOut) = go
  go ::
    ( Port "CNTVALUEOUT" domCtrl (BitVector 5)
    , Port "DATAOUT" domData Bit
    )
  go =
    inst
      (unisim "IDELAYE2")
      (Param @"CINVCTRL_SEL" @String "FALSE")
      (Param @"DELAY_SRC" @String (case src of FromInputBuffer -> "IDATAIN"; FromFabric -> "DATAIN"))
      (Param @"HIGH_PERFORMANCE_MODE" @String (boolString (delayHighPerformance cfg)))
      (Param @"IDELAY_TYPE" @String (delayTypeString (delayType cfg)))
      (Param @"IDELAY_VALUE" @Integer (toInteger (delayValue cfg)))
      (Param @"IS_C_INVERTED" @Bit 0)
      (Param @"IS_DATAIN_INVERTED" @Bit 0)
      (Param @"IS_IDATAIN_INVERTED" @Bit 0)
      (Param @"PIPE_SEL" @String "FALSE")
      (Param @"SIGNAL_PATTERN" @String (signalPatternString (delaySignalPattern cfg)))
      (ClockPort @"C" clk)
      (Port @"CE" ce)
      (Port @"CINVCTRL" zeroCtrl)
      (Port @"CNTVALUEIN" cntValueIn)
      (Port @"DATAIN" datain)
      (Port @"IDATAIN" idataIn)
      (Port @"INC" inc)
      (Port @"LD" ld)
      (Port @"LDPIPEEN" zeroCtrl)
      (Port @"REGRST" zeroCtrl)

{- | Output delay line (@ODELAYE2@). Like 'idelaye2', but for a signal going to
an output buffer (@ODATAIN@ to @DATAOUT@). To delay the output of an
'Clash.Cores.Xilinx.Unisim.Series7.Serdes.oserdese2', configure the latter with
'Clash.Cores.Xilinx.Unisim.Series7.Serdes.OutputOfb'. Only available in HP I/O
banks.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/ODELAYE2)
and UG471.
-}
odelaye2 ::
  forall domCtrl domData.
  (KnownDomain domCtrl, KnownDomain domData) =>
  DelayConfig ->
  -- | Clock (@C@)
  Clock domCtrl ->
  -- | Tap counter command (@LD@, @CE@, @INC@, @CNTVALUEIN@)
  Signal domCtrl DelayCommand ->
  -- | Data (@ODATAIN@)
  Signal domData Bit ->
  -- | Delayed data (@DATAOUT@) and tap value (@CNTVALUEOUT@)
  (Signal domData Bit, Signal domCtrl (Unsigned 5))
odelaye2 cfg clk cmd dataIn
  | clashSimulation = (dataIn, tapCounter cfg clk cmd)
  | otherwise = (unPort dataOut, unpack <$> unPort cntValueOut)
 where
  (ld, ce, inc, cntValueIn) = unbundle (commandPorts (delayType cfg) <$> cmd)
  zeroCtrl = pure 0 :: Signal domCtrl Bit

  (cntValueOut, dataOut) = go
  go ::
    ( Port "CNTVALUEOUT" domCtrl (BitVector 5)
    , Port "DATAOUT" domData Bit
    )
  go =
    inst
      (unisim "ODELAYE2")
      (Param @"CINVCTRL_SEL" @String "FALSE")
      (Param @"DELAY_SRC" @String "ODATAIN")
      (Param @"HIGH_PERFORMANCE_MODE" @String (boolString (delayHighPerformance cfg)))
      (Param @"IS_C_INVERTED" @Bit 0)
      (Param @"IS_ODATAIN_INVERTED" @Bit 0)
      (Param @"ODELAY_TYPE" @String (delayTypeString (delayType cfg)))
      (Param @"ODELAY_VALUE" @Integer (toInteger (delayValue cfg)))
      (Param @"PIPE_SEL" @String "FALSE")
      (Param @"SIGNAL_PATTERN" @String (signalPatternString (delaySignalPattern cfg)))
      (ClockPort @"C" clk)
      (Port @"CE" ce)
      (Port @"CINVCTRL" zeroCtrl)
      (Port @"CLKIN" (pure 0 :: Signal domData Bit))
      (Port @"CNTVALUEIN" cntValueIn)
      (Port @"INC" inc)
      (Port @"LD" ld)
      (Port @"LDPIPEEN" zeroCtrl)
      (Port @"ODATAIN" dataIn)
      (Port @"REGRST" zeroCtrl)

boolString :: Bool -> String
boolString b = if b then "TRUE" else "FALSE"
