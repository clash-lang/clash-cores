{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

I/O buffers of UltraScale and UltraScale+ devices. For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/I/O-Components).
The tristate and bidirectional buffers ('obuft', 'iobuf', 'obuftds' and
'iobufds') exist on 7 series devices as well, with the same attributes, see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/I/O-Components).

The input and output buffers 'ibufds' and 'obufds' don't set any attributes:
set I/O standards and other placement-related attributes (@IOSTANDARD@,
@DIFF_TERM@, ...) in your constraints file instead. The tristate and
bidirectional buffers expose their attributes in their configuration, but
leave the I/O standard to the constraints file by default.

= Tristate and bidirectional pads

#pads#
The tristate and bidirectional buffers connect to their pads through a
'BiSignalIn' and 'BiSignalOut' pair, see "Clash.Signal.BiSignal". Pass the
'BiSignalIn' of a top entity argument and return the 'BiSignalOut' from the top
entity. Clash renders a 1-bit 'BiSignalIn' as a vector of one bit, so the pad is
called @name[0]@ in your constraints file.

The value to drive onto a pad is a 'Maybe': 'Just' drives the bit, 'Nothing'
puts the output in high impedance. The buffer's @T@ input is high for 'Nothing'.

The 'BiSignalDefault' of a single-ended pad determines its pull resistor: for
'PullUp' and 'PullDown' pads, 'iobuf' and 'obuft' instantiate a @PULLUP@ or
@PULLDOWN@ primitive on the pad, enabling the weak pull resistor of the I/O
block. This makes the hardware read a released pad the way Clash simulates it.
A v'Floating' pad gets no pull resistor from these functions, though you can still
set one with the @PULLTYPE@ property in your constraints file. The weak pull
resistor is no replacement for an external one where a bus specification
requires that, such as on most I2C buses; adding both does no harm.

=== Example: an I2C bus

I2C lines are open drain: devices only ever pull a line low or release it, and
a resistor pulls a released line high. The following function connects the
"Clash.Cores.I2c" controller to its pads. Pass the pads from the arguments of
the top entity, feed the 'Clash.Cores.I2c.I2cIn' it returns to
'Clash.Cores.I2c.i2c', and return the 'BiSignalOut's from the top entity.

@
import Clash.Prelude
import Clash.Cores.I2c (I2cIn, I2cOut)
import Clash.Cores.Xilinx.Unisim.IoBuffer (iobuf, iobufDefaultConfig)

i2cPads ::
  (KnownDomain dom) =>
  \"SCL\" ::: BiSignalIn 'PullUp dom 1 ->
  \"SDA\" ::: BiSignalIn 'PullUp dom 1 ->
  Signal dom I2cOut ->
  ( \"SCL\" ::: BiSignalOut 'PullUp dom 1
  , \"SDA\" ::: BiSignalOut 'PullUp dom 1
  , Signal dom I2cIn
  )
i2cPads sclPad sdaPad i2cOut = (sclPadOut, sdaPadOut, bundle (scl, sda))
 where
  (sclO, sclOen, sdaO, sdaOen) = unbundle i2cOut
  (sclPadOut, scl) = iobuf iobufDefaultConfig sclPad (openDrain \<$\> sclO \<*\> sclOen)
  (sdaPadOut, sda) = iobuf iobufDefaultConfig sdaPad (openDrain \<$\> sdaO \<*\> sdaOen)

  -- The output enables of the I2C core are active low: True releases the line
  openDrain o oen = if oen then Nothing else Just o
@

The pads read high in simulation when no device pulls them low, and the
@PULLUP@ primitives 'iobuf' instantiates for these 'PullUp' pads make sure
they do in hardware too.
-}
module Clash.Cores.Xilinx.Unisim.IoBuffer
  ( -- * Differential input buffers
    ibufds
  , ibufdsClock
  , ibufdsGte3
  , ibufdsGte4
    -- * Differential output buffers
  , obufds
    -- * Tristate output buffers
  , obuft
  , ObuftConfig (..)
  , obuftDefaultConfig
  , obuftds
  , ObuftdsConfig (..)
  , obuftdsDefaultConfig
    -- * Bidirectional buffers
  , iobuf
  , IobufConfig (..)
  , iobufDefaultConfig
  , iobufds
  , IobufdsConfig (..)
  , iobufdsDefaultConfig
    -- * Attributes
  , Slew (..)
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.BiSignal (HasBiSignalDefault (..), SBiSignalDefault (..))
import Clash.Signal.Internal (DiffClock (..))
import Data.Maybe (isNothing)

import Clash.Cores.Xilinx.Ibufds (ibufdsClock)
import Clash.Cores.Xilinx.Unisim.Internal (gatedClockModel, unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( BiSignalOutPort, BitBiSignalInPort (..), BoolStringParam (..), ClockPort (..)
  , InstConfig (..), NamedDiffClockPort (..), Param (..), Port (..), inst, unPort
  )

{- | Differential input buffer (@IBUFDS@) for a data signal. Use 'ibufdsClock'
for clocks. Simulation: the positive input.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS).
-}
ibufds ::
  forall dom.
  (KnownDomain dom) =>
  -- | Positive input (@I@)
  Signal dom Bit ->
  -- | Negative input (@IB@)
  Signal dom Bit ->
  Signal dom Bit
ibufds p n
  | clashSimulation = p
  | otherwise = unPort go
 where
  go :: Port "O" dom Bit
  go = inst (unisim "IBUFDS") (Port @"I" p) (Port @"IB" n)

{- | Differential output buffer (@OBUFDS@) for a data signal. Returns the
positive and negative output. Simulation: the input and its complement.

To forward a clock off-chip, drive the buffer with an
'Clash.Cores.Xilinx.Unisim.Ddr.oddre1'
that outputs @(1, 0)@ every cycle instead of connecting the clock to it.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/OBUFDS).
-}
obufds ::
  forall dom.
  (KnownDomain dom) =>
  Signal dom Bit ->
  -- | Positive (@O@) and negative (@OB@) output
  (Signal dom Bit, Signal dom Bit)
obufds i
  | clashSimulation = (i, complement <$> i)
  | otherwise = (unPort o, unPort ob)
 where
  (o, ob) = go
  go :: (Port "O" dom Bit, Port "OB" dom Bit)
  go = inst (unisim "OBUFDS") (Port @"I" i)

{- | Differential input buffer for the reference clock of a GTH or GTY
transceiver (@IBUFDS_GTE4@) on UltraScale+ devices. Returns:

  1. The clock for the transceivers' reference clock inputs (@O@). This output
     can only drive transceiver clock inputs, not the fabric.

  2. The clock for the fabric (@ODIV2@), divided by @div@ (1 or 2). It can only
     drive a @BUFG_GT@. When the divider is 2, the first rising edge of the
     input clock produces a rising edge of this output.

The clock enable (@CEB@) is tied to always enabled.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS_GTE4)
and the /UltraScale Architecture GTH\/GTY Transceivers/ user guides (UG576,
UG578).
-}
ibufdsGte4 ::
  forall div dom domDiv.
  ( KnownDomain dom
  , KnownDomain domDiv
  , 1 <= div
  , div <= 2
  , DomainPeriod domDiv ~ div * DomainPeriod dom
  ) =>
  -- | Divider of @ODIV2@ (@REFCLK_HROW_CK_SEL@)
  SNat div ->
  DiffClock dom ->
  -- | @O@ and @ODIV2@
  (Clock dom, Clock domDiv)
ibufdsGte4 divider@SNat clkIn
  | clashSimulation
  , DiffClock clkP _ <- clkIn =
      (clkP, gatedClockModel (snatToNum divider) clkP (pure False) (pure True))
  | otherwise = (unPort o, unPort odiv2)
 where
  (o, odiv2) = go
  go :: (ClockPort "O" dom, ClockPort "ODIV2" domDiv)
  go =
    inst
      (unisim "IBUFDS_GTE4")
      (Param @"REFCLK_EN_TX_PATH" @Bit 0)
      (Param @"REFCLK_HROW_CK_SEL" @(BitVector 2) (if snatToInteger divider == 1 then 0b00 else 0b01))
      (Param @"REFCLK_ICNTL_RX" @(BitVector 2) 0b00)
      (NamedDiffClockPort @"I" @"IB" clkIn)
      (Port @"CEB" (pure 0 :: Signal dom Bit))

{- | Differential input buffer for the reference clock of a GTH or GTY
transceiver (@IBUFDS_GTE3@) on UltraScale devices. Like 'ibufdsGte4', which is
its UltraScale+ counterpart.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS_GTE3)
and the /UltraScale Architecture GTH\/GTY Transceivers/ user guides (UG576,
UG578).
-}
ibufdsGte3 ::
  forall div dom domDiv.
  ( KnownDomain dom
  , KnownDomain domDiv
  , 1 <= div
  , div <= 2
  , DomainPeriod domDiv ~ div * DomainPeriod dom
  ) =>
  -- | Divider of @ODIV2@ (@REFCLK_HROW_CK_SEL@)
  SNat div ->
  DiffClock dom ->
  -- | @O@ and @ODIV2@
  (Clock dom, Clock domDiv)
ibufdsGte3 divider@SNat clkIn
  | clashSimulation
  , DiffClock clkP _ <- clkIn =
      (clkP, gatedClockModel (snatToNum divider) clkP (pure False) (pure True))
  | otherwise = (unPort o, unPort odiv2)
 where
  (o, odiv2) = go
  go :: (ClockPort "O" dom, ClockPort "ODIV2" domDiv)
  go =
    inst
      (unisim "IBUFDS_GTE3")
      (Param @"REFCLK_EN_TX_PATH" @Bit 0)
      (Param @"REFCLK_HROW_CK_SEL" @(BitVector 2) (if snatToInteger divider == 1 then 0b00 else 0b01))
      (Param @"REFCLK_ICNTL_RX" @(BitVector 2) 0b00)
      (NamedDiffClockPort @"I" @"IB" clkIn)
      (Port @"CEB" (pure 0 :: Signal dom Bit))

{- | Slew rate of an output buffer (@SLEW@). 'Medium' is only available on
UltraScale and UltraScale+ devices.
-}
data Slew
  = Slow
  | Medium
  | Fast
  deriving (Show, Eq, Generic, NFDataX)

-- | Value of the @SLEW@ attribute
slewString :: Slew -> String
slewString Slow = "SLOW"
slewString Medium = "MEDIUM"
slewString Fast = "FAST"

-- | Configuration of 'obuft'
data ObuftConfig = ObuftConfig
  { obuftDrive :: Natural
  -- ^ Output drive strength in mA (@DRIVE@). Which values are allowed depends
  -- on the device, the I/O bank and the I/O standard.
  , obuftSlew :: Slew
  -- ^ Slew rate (@SLEW@)
  , obuftIoStandard :: String
  -- ^ I/O standard (@IOSTANDARD@), e.g. @\"LVCMOS33\"@. @\"DEFAULT\"@ leaves
  -- it to the constraints file.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'obuft': the defaults of the primitive, i.e. a
-- drive strength of 12 mA, 'Slow' slew rate, and the I/O standard from the
-- constraints file
obuftDefaultConfig :: ObuftConfig
obuftDefaultConfig =
  ObuftConfig
    { obuftDrive = 12
    , obuftSlew = Slow
    , obuftIoStandard = "DEFAULT"
    }

-- | Configuration of 'iobuf'
data IobufConfig = IobufConfig
  { iobufDrive :: Natural
  -- ^ Output drive strength in mA (@DRIVE@). Which values are allowed depends
  -- on the device, the I/O bank and the I/O standard.
  , iobufSlew :: Slew
  -- ^ Slew rate (@SLEW@)
  , iobufIoStandard :: String
  -- ^ I/O standard (@IOSTANDARD@), e.g. @\"LVCMOS33\"@. @\"DEFAULT\"@ leaves
  -- it to the constraints file.
  , iobufLowPower :: Bool
  -- ^ Use the low power mode of the input buffer (@IBUF_LOW_PWR@), at the
  -- expense of performance. Only affects I/O standards with a reference
  -- voltage or a differential input.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'iobuf': the defaults of the primitive, i.e. a
-- drive strength of 12 mA, 'Slow' slew rate, the I/O standard from the
-- constraints file, and a low power input buffer
iobufDefaultConfig :: IobufConfig
iobufDefaultConfig =
  IobufConfig
    { iobufDrive = 12
    , iobufSlew = Slow
    , iobufIoStandard = "DEFAULT"
    , iobufLowPower = True
    }

-- | Configuration of 'obuftds'
data ObuftdsConfig = ObuftdsConfig
  { obuftdsSlew :: Slew
  -- ^ Slew rate (@SLEW@). Only affects some I/O standards, such as
  -- @DIFF_SSTL@ and @DIFF_HSTL@.
  , obuftdsIoStandard :: String
  -- ^ I/O standard (@IOSTANDARD@), e.g. @\"BLVDS_25\"@. @\"DEFAULT\"@ leaves
  -- it to the constraints file.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'obuftds': the defaults of the primitive, i.e.
-- 'Slow' slew rate and the I/O standard from the constraints file
obuftdsDefaultConfig :: ObuftdsConfig
obuftdsDefaultConfig =
  ObuftdsConfig
    { obuftdsSlew = Slow
    , obuftdsIoStandard = "DEFAULT"
    }

-- | Configuration of 'iobufds'
data IobufdsConfig = IobufdsConfig
  { iobufdsSlew :: Slew
  -- ^ Slew rate (@SLEW@). Only affects some I/O standards, such as
  -- @DIFF_SSTL@ and @DIFF_HSTL@.
  , iobufdsIoStandard :: String
  -- ^ I/O standard (@IOSTANDARD@), e.g. @\"BLVDS_25\"@. @\"DEFAULT\"@ leaves
  -- it to the constraints file.
  , iobufdsLowPower :: Bool
  -- ^ Use the low power mode of the input buffer (@IBUF_LOW_PWR@), at the
  -- expense of performance
  , iobufdsDiffTerm :: Bool
  -- ^ Enable the internal differential termination (@DIFF_TERM@). Only
  -- available for some I/O standards and banks.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'iobufds': the defaults of the primitive, i.e.
-- 'Slow' slew rate, the I/O standard from the constraints file, a low power
-- input buffer, and no differential termination
iobufdsDefaultConfig :: IobufdsConfig
iobufdsDefaultConfig =
  IobufdsConfig
    { iobufdsSlew = Slow
    , iobufdsIoStandard = "DEFAULT"
    , iobufdsLowPower = True
    , iobufdsDiffTerm = False
    }

-- | Input @I@ of a tristate buffer
tristateI :: Signal dom (Maybe Bit) -> Signal dom Bit
tristateI = fmap fromJustX

-- | Input @T@ of a tristate buffer: high for high impedance
tristateT :: Signal dom (Maybe Bit) -> Signal dom Bit
tristateT = fmap (boolToBit . isNothing)

-- | Instantiates a @PULLUP@ or @PULLDOWN@ primitive on a 'PullUp' or
-- 'PullDown' pad, and merges it with the given output. Does nothing for a
-- v'Floating' pad.
withPullResistor ::
  forall ds dom.
  (HasBiSignalDefault ds, KnownDomain dom) =>
  BiSignalIn ds dom 1 ->
  BiSignalOut ds dom 1 ->
  BiSignalOut ds dom 1
withPullResistor pad out = case pullUpMode pad of
  SFloating -> out
  SPullUp -> mergeBiSignalOuts (out :> unPort (pull "PULLUP") :> Nil)
  SPullDown -> mergeBiSignalOuts (out :> unPort (pull "PULLDOWN") :> Nil)
 where
  pull :: String -> BiSignalOutPort "O" ds dom 1
  pull name = inst (unisim name){renderVoid = True} (BitBiSignalInPort @"O" pad)

{- | Tristate output buffer (@OBUFT@). Drives the pad with the bit of a 'Just',
and puts it in high impedance for 'Nothing'. Instantiates a pull resistor for
'PullUp' and 'PullDown' pads, see [Tristate and bidirectional
pads]("Clash.Cores.Xilinx.Unisim.IoBuffer#pads").

Simulation: 'writeToBiSignal'.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/OBUFT).
-}
obuft ::
  forall ds dom.
  (HasBiSignalDefault ds, KnownDomain dom) =>
  ObuftConfig ->
  -- | Pad (@O@)
  BiSignalIn ds dom 1 ->
  -- | Bit to drive onto the pad, or 'Nothing' for high impedance (@I@ and @T@)
  Signal dom (Maybe Bit) ->
  -- | Pad (@O@)
  BiSignalOut ds dom 1
obuft cfg pad out
  | clashSimulation = writeToBiSignal pad out
  | otherwise = withPullResistor pad (unPort go)
 where
  go :: BiSignalOutPort "O" ds dom 1
  go =
    inst
      (unisim "OBUFT"){renderVoid = True}
      (Param @"DRIVE" @Integer (toInteger (obuftDrive cfg)))
      (Param @"IOSTANDARD" @String (obuftIoStandard cfg))
      (Param @"SLEW" @String (slewString (obuftSlew cfg)))
      (BitBiSignalInPort @"O" pad)
      (Port @"I" (tristateI out))
      (Port @"T" (tristateT out))

{- | Bidirectional buffer (@IOBUF@). Drives the pad with the bit of a 'Just',
and puts it in high impedance for 'Nothing'. Returns the level of the pad,
whether the buffer drives it or not. Instantiates a pull resistor for
'PullUp' and 'PullDown' pads, see [Tristate and bidirectional
pads]("Clash.Cores.Xilinx.Unisim.IoBuffer#pads").

Simulation: 'writeToBiSignal' and 'readFromBiSignal'. Like reading a
'BiSignalIn', the returned level is undefined when no one drives a v'Floating'
pad.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IOBUF).
-}
iobuf ::
  forall ds dom.
  (HasBiSignalDefault ds, KnownDomain dom) =>
  IobufConfig ->
  -- | Pad (@IO@)
  BiSignalIn ds dom 1 ->
  -- | Bit to drive onto the pad, or 'Nothing' for high impedance (@I@ and @T@)
  Signal dom (Maybe Bit) ->
  -- | Pad (@IO@) and the level of the pad (@O@)
  (BiSignalOut ds dom 1, Signal dom Bit)
iobuf cfg pad out
  | clashSimulation = (writeToBiSignal pad out, readFromBiSignal pad)
  | otherwise = (withPullResistor pad (unPort ioOut), unPort o)
 where
  (ioOut, o) = go
  go :: (BiSignalOutPort "IO" ds dom 1, Port "O" dom Bit)
  go =
    inst
      (unisim "IOBUF"){renderVoid = True}
      (Param @"DRIVE" @Integer (toInteger (iobufDrive cfg)))
      (BoolStringParam @"IBUF_LOW_PWR" (iobufLowPower cfg))
      (Param @"IOSTANDARD" @String (iobufIoStandard cfg))
      (Param @"SLEW" @String (slewString (iobufSlew cfg)))
      (BitBiSignalInPort @"IO" pad)
      (Port @"I" (tristateI out))
      (Port @"T" (tristateT out))

{- | Differential tristate output buffer (@OBUFTDS@). Drives the positive pad
with the bit of a 'Just' and the negative pad with its complement, and puts
both in high impedance for 'Nothing'.

The pads are v'Floating': a pull resistor on a differential pair would pull
both pads to the same level, which the receiving end can't interpret.

Simulation: 'writeToBiSignal'.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/OBUFTDS).
-}
obuftds ::
  forall dom.
  (KnownDomain dom) =>
  ObuftdsConfig ->
  -- | Positive pad (@O@)
  BiSignalIn 'Floating dom 1 ->
  -- | Negative pad (@OB@)
  BiSignalIn 'Floating dom 1 ->
  -- | Bit to drive onto the pads, or 'Nothing' for high impedance (@I@ and
  -- @T@)
  Signal dom (Maybe Bit) ->
  -- | Positive (@O@) and negative (@OB@) pad
  (BiSignalOut 'Floating dom 1, BiSignalOut 'Floating dom 1)
obuftds cfg padP padN out
  | clashSimulation =
      (writeToBiSignal padP out, writeToBiSignal padN (fmap complement <$> out))
  | otherwise = (unPort oOut, unPort obOut)
 where
  (oOut, obOut) = go
  go :: (BiSignalOutPort "O" 'Floating dom 1, BiSignalOutPort "OB" 'Floating dom 1)
  go =
    inst
      (unisim "OBUFTDS"){renderVoid = True}
      (Param @"IOSTANDARD" @String (obuftdsIoStandard cfg))
      (Param @"SLEW" @String (slewString (obuftdsSlew cfg)))
      (BitBiSignalInPort @"O" padP)
      (BitBiSignalInPort @"OB" padN)
      (Port @"I" (tristateI out))
      (Port @"T" (tristateT out))

{- | Differential bidirectional buffer (@IOBUFDS@). Drives the positive pad
with the bit of a 'Just' and the negative pad with its complement, and puts
both in high impedance for 'Nothing'. Returns the level of the differential
pair, whether the buffer drives it or not: the level of the positive pad if the
pads have different levels, undefined otherwise.

The pads are v'Floating': a pull resistor on a differential pair would pull
both pads to the same level, which the receiver can't interpret. The @DQS_BIAS@
attribute, which makes the receiver output a 0 for a floating pair, is not
supported.

Simulation: 'writeToBiSignal' and 'readFromBiSignal'.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IOBUFDS).
-}
iobufds ::
  forall dom.
  (KnownDomain dom) =>
  IobufdsConfig ->
  -- | Positive pad (@IO@)
  BiSignalIn 'Floating dom 1 ->
  -- | Negative pad (@IOB@)
  BiSignalIn 'Floating dom 1 ->
  -- | Bit to drive onto the pads, or 'Nothing' for high impedance (@I@ and
  -- @T@)
  Signal dom (Maybe Bit) ->
  -- | Positive (@IO@) and negative (@IOB@) pad, and the level of the
  -- differential pair (@O@)
  (BiSignalOut 'Floating dom 1, BiSignalOut 'Floating dom 1, Signal dom Bit)
iobufds cfg padP padN out
  | clashSimulation =
      ( writeToBiSignal padP out
      , writeToBiSignal padN (fmap complement <$> out)
      , receiver <$> readFromBiSignal padP <*> readFromBiSignal padN
      )
  | otherwise = (unPort ioOut, unPort iobOut, unPort o)
 where
  receiver :: Bit -> Bit -> Bit
  receiver p n
    | p /= n = p
    | otherwise = errorX "iobufds: both pads have the same level"

  (ioOut, iobOut, o) = go
  go ::
    ( BiSignalOutPort "IO" 'Floating dom 1
    , BiSignalOutPort "IOB" 'Floating dom 1
    , Port "O" dom Bit
    )
  go =
    inst
      (unisim "IOBUFDS"){renderVoid = True}
      (BoolStringParam @"DIFF_TERM" (iobufdsDiffTerm cfg))
      (Param @"DQS_BIAS" @String "FALSE")
      (BoolStringParam @"IBUF_LOW_PWR" (iobufdsLowPower cfg))
      (Param @"IOSTANDARD" @String (iobufdsIoStandard cfg))
      (Param @"SLEW" @String (slewString (iobufdsSlew cfg)))
      (BitBiSignalInPort @"IO" padP)
      (BitBiSignalInPort @"IOB" padN)
      (Port @"I" (tristateI out))
      (Port @"T" (tristateT out))
