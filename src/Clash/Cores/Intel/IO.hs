{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

I/O buffer primitives of Intel FPGAs: @ALT_OUTBUF_TRI@, @ALT_IOBUF@,
@ALT_INBUF_DIFF@, @ALT_OUTBUF_DIFF@, @ALT_IOBUF_DIFF@ and @OPNDRN@. Quartus
infers I/O buffers for the ports of a design by itself; instantiating them is
only needed to set I/O assignments, such as the I/O standard or the weak
pull-up resistor, from HDL instead of the Quartus Settings File (@.qsf@).

Quartus requires the pad side of these buffers to be connected directly to a
pin of the design: pass the pads to and from the top entity, without logic in
between.

For more information see the Quartus Prime Help, section /Primitives/ →
/Buffer Primitives/
(<https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/prim/prim_file_alt_iobuf.htm>),
and the /Designing with Low-Level Primitives User Guide/
(<https://cdrdv2-public.intel.com/654838/ug_low_level.pdf>).

= Device support

Not every device family supports every buffer. With Quartus Prime Lite 21.1,
we found:

+--------------------+--------------+-----------+---------------+--------+
| Buffer             | Cyclone IV E | Cyclone V | Cyclone 10 LP | MAX 10 |
+====================+==============+===========+===============+========+
| 'outbufTri'        | yes          | yes       | yes           | no     |
+--------------------+--------------+-----------+---------------+--------+
| 'iobuf'            | yes          | yes       | yes           | no     |
+--------------------+--------------+-----------+---------------+--------+
| 'opndrn'           | yes          | yes       | yes           | yes    |
+--------------------+--------------+-----------+---------------+--------+
| 'inbufDiff'        | yes          | yes       | yes           | no     |
+--------------------+--------------+-----------+---------------+--------+
| 'outbufDiff'       | yes          | see below | yes           | no     |
+--------------------+--------------+-----------+---------------+--------+
| 'iobufDiff'        | see below    | no        | see below     | no     |
+--------------------+--------------+-----------+---------------+--------+

  * Cyclone IV E and Cyclone 10 LP accept 'iobufDiff' in synthesis, but their
    differential I/O standards can't be used on bidirectional pins, so the
    design doesn't fit. 'iobufDiff' is meant for the high-end families.

  * On Cyclone V, an 'outbufDiff' with I/O standard @\"LVDS\"@ and no pin
    assignments didn't fit: the negative output got the default 2.5 V I/O
    standard.

  * Cyclone V doesn't support the @slew_rate@ parameter ('ioSlewRate').

  * The @termination@ parameter of 'inbufDiff' ('ioTermination') crashes
    synthesis for Cyclone V, and Cyclone IV E and Cyclone 10 LP don't support
    differential input termination.

= Tristate and bidirectional pads

#pads#
The bidirectional buffers and 'opndrn' connect to their pads through a
'BiSignalIn' and 'BiSignalOut' pair, see "Clash.Signal.BiSignal". Pass the
'BiSignalIn' of a top entity argument and return the 'BiSignalOut' from the top
entity. Clash renders a 1-bit 'BiSignalIn' as a vector of one bit, so the pad is
called @name[0]@ in your assignments. The tristate output buffer 'outbufTri'
drives an output pin instead.

The value to drive onto a pad is a 'Maybe': 'Just' drives the bit, 'Nothing'
puts the output in high impedance. The output enable (@oe@) of the buffer is
high for 'Just'.

The 'BiSignalDefault' of a single-ended pad determines its weak pull-up
resistor: for 'PullUp' pads, 'iobuf' sets @weak_pull_up_resistor@ to
@\"ON\"@, which makes the hardware read a released pad the way Clash simulates
it. For v'Floating' pads, it leaves it to the Quartus settings. Intel devices
have no weak pull-down resistor, so 'PullDown' pads are rejected at compile
time: use a v'Floating' pad and an external resistor instead. The weak pull-up
resistor is no replacement for an external one where a bus specification
requires that, such as on most I2C buses. Intel's simulation models ignore
@weak_pull_up_resistor@, so a released pad reads as high impedance in HDL
simulation.

=== Example: an I2C bus

I2C lines are open drain: devices only ever pull a line low or release it, and
a resistor pulls a released line high. The following function connects the
"Clash.Cores.I2c" controller to its pads. Pass the pads from the arguments of
the top entity, feed the 'Clash.Cores.I2c.I2cIn' it returns to
'Clash.Cores.I2c.i2c', and return the 'BiSignalOut's from the top entity.

@
import Clash.Prelude
import Clash.Cores.I2c (I2cIn, I2cOut)
import Clash.Cores.Intel.IO (iobuf, defaultIoBufferConfig)

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
  (sclPadOut, scl) = iobuf defaultIoBufferConfig sclPad (openDrain \<$\> sclO \<*\> sclOen)
  (sdaPadOut, sda) = iobuf defaultIoBufferConfig sdaPad (openDrain \<$\> sdaO \<*\> sdaOen)

  -- The output enables of the I2C core are active low: True releases the line
  openDrain o oen = if oen then Nothing else Just o
@
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Intel.IO
  ( -- * Configuration
    IoBufferConfig (..)
  , defaultIoBufferConfig
  , IntelPull
    -- * Single-ended buffers
  , outbufTri
  , iobuf
  , opndrn
    -- * Differential buffers
  , inbufDiff
  , outbufDiff
  , iobufDiff
  ) where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (HDL (..), Primitive (..), hasBlackBox)
import Clash.Signal.BiSignal (HasBiSignalDefault (..), SBiSignalDefault (..))
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (__i)

import Clash.Cores.Intel.Primitive.Internal (alteraPrimitive, noneString, onOffString)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( BiSignalOutPort, BitBiSignalInPort (..), InstConfig (..), Param (..), Port (..)
  , inst, unPort
  )

{- | I/O assignments of a buffer, set through the parameters of the primitive.
'Nothing' leaves an assignment to the Quartus settings (the parameter is set to
@\"NONE\"@). The values are those of the corresponding assignments in the
Quartus Settings File; which ones are legal depends on the device and the I/O
bank. Not every buffer has every parameter: each buffer documents the ones it
uses.
-}
data IoBufferConfig = IoBufferConfig
  { ioStandard :: Maybe String
  -- ^ I/O standard (@io_standard@), e.g. @\"3.3-V LVTTL\"@ or @\"LVDS\"@
  , ioCurrentStrength :: Maybe String
  -- ^ Output current strength (@current_strength_new@), e.g. @\"8MA\"@ or
  -- @\"MAXIMUM CURRENT\"@
  , ioSlewRate :: Maybe Natural
  -- ^ Slew rate of the output (@slew_rate@), where 0 is the slowest. The legal
  -- values depend on the device family.
  , ioBusHold :: Maybe Bool
  -- ^ Enable the bus-hold circuitry (@enable_bus_hold@). Can't be combined
  -- with the weak pull-up resistor of a 'PullUp' pad.
  , ioTermination :: Maybe String
  -- ^ On-chip termination (@termination@), e.g. @\"SERIES 50 OHMS\"@ or
  -- @\"DIFFERENTIAL\"@
  , ioLocation :: Maybe String
  -- ^ Pin location (@location@), e.g. @\"PIN_AB12\"@
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Configuration that leaves all I/O assignments to the Quartus settings
defaultIoBufferConfig :: IoBufferConfig
defaultIoBufferConfig =
  IoBufferConfig
    { ioStandard = Nothing
    , ioCurrentStrength = Nothing
    , ioSlewRate = Nothing
    , ioBusHold = Nothing
    , ioTermination = Nothing
    , ioLocation = Nothing
    }

{- | Single-ended pads of Intel devices can be v'Floating', or have a weak
'PullUp' resistor. There is no weak pull-down resistor.
-}
type family IntelPull (ds :: BiSignalDefault) :: Constraint where
  IntelPull 'PullDown =
    TypeError
      ( 'Text "Intel devices have no weak pull-down resistor."
          ':$$: 'Text "Use a 'Floating pad and an external pull-down resistor instead."
      )
  IntelPull _ = ()

-- | Value of @weak_pull_up_resistor@ for a pad
weakPullUp :: (HasBiSignalDefault ds) => BiSignalIn ds dom 1 -> String
weakPullUp pad = case pullUpMode pad of
  SPullUp -> "ON"
  _ -> "NONE"

-- | Input @i@ of a tristate buffer
tristateI :: Signal dom (Maybe Bit) -> Signal dom Bit
tristateI = fmap fromJustX

-- | Input @oe@ of a tristate buffer: high to drive the pad
tristateOe :: Signal dom (Maybe Bit) -> Signal dom Bit
tristateOe = fmap (boolToBit . isJust)

-- | Output of a differential receiver: the level of the positive input, or
-- undefined if both inputs have the same level
receiver :: String -> Bit -> Bit -> Bit
receiver nm p n
  | p /= n = p
  | otherwise = errorX (nm <> ": both inputs have the same level")

{- | Tristate output buffer (@ALT_OUTBUF_TRI@). Drives the output with the
bit of a 'Just', and puts it in high impedance for 'Nothing'.

Quartus requires the output of the buffer to be connected directly to an
output pin of the design, not to a bidirectional one, so the output is a
'Signal' rather than a 'BiSignalOut': return it from the top entity. Clash
can't represent high impedance, so it simulates the output as undefined for
'Nothing'. Use 'iobuf' for a pin that other devices drive too.

Uses all fields of 'IoBufferConfig'. The primitive can enable the weak pull-up
resistor of the pin, but 'outbufTri' leaves that to the Quartus settings
(@WEAK_PULL_UP_RESISTOR@).

Simulation: the bit of a 'Just', undefined for 'Nothing'.
-}
outbufTri ::
  forall dom.
  (KnownDomain dom) =>
  IoBufferConfig ->
  -- | Bit to drive onto the output, or 'Nothing' for high impedance (@i@ and
  -- @oe@)
  Signal dom (Maybe Bit) ->
  -- | Output pin (@o@)
  Signal dom Bit
outbufTri cfg out
  | clashSimulation = fromMaybe (errorX "outbufTri: high impedance") <$> out
  | otherwise = unPort go
 where
  go :: Port "o" dom Bit
  go =
    inst
      (alteraPrimitive "alt_outbuf_tri")
      (Param @"io_standard" @String (noneString (ioStandard cfg)))
      (Param @"current_strength_new" @String (noneString (ioCurrentStrength cfg)))
      (Param @"slew_rate" @Integer (maybe (-1) toInteger (ioSlewRate cfg)))
      (Param @"location" @String (noneString (ioLocation cfg)))
      (Param @"enable_bus_hold" @String (onOffString (ioBusHold cfg)))
      (Param @"termination" @String (noneString (ioTermination cfg)))
      (Port @"i" (tristateI out))
      (Port @"oe" (tristateOe out))

{- | Bidirectional buffer (@ALT_IOBUF@). Drives the pad with the bit of a
'Just', and puts it in high impedance for 'Nothing'. Returns the level of the
pad, whether the buffer drives it or not. Sets the weak pull-up resistor of
'PullUp' pads, see [Tristate and bidirectional
pads]("Clash.Cores.Intel.IO#pads").

Uses all fields of 'IoBufferConfig'.

Simulation: 'writeToBiSignal' and 'readFromBiSignal'. Like reading a
'BiSignalIn', the returned level is undefined when no one drives a v'Floating'
pad.
-}
iobuf ::
  forall ds dom.
  (HasBiSignalDefault ds, IntelPull ds, KnownDomain dom) =>
  IoBufferConfig ->
  -- | Pad (@io@)
  BiSignalIn ds dom 1 ->
  -- | Bit to drive onto the pad, or 'Nothing' for high impedance (@i@ and @oe@)
  Signal dom (Maybe Bit) ->
  -- | Pad (@io@) and the level of the pad (@o@)
  (BiSignalOut ds dom 1, Signal dom Bit)
iobuf cfg pad out
  | clashSimulation = (writeToBiSignal pad out, readFromBiSignal pad)
  | otherwise = (unPort ioOut, unPort o)
 where
  (ioOut, o) = go
  go :: (BiSignalOutPort "io" ds dom 1, Port "o" dom Bit)
  go =
    inst
      (alteraPrimitive "alt_iobuf"){renderVoid = True}
      (Param @"io_standard" @String (noneString (ioStandard cfg)))
      (Param @"current_strength_new" @String (noneString (ioCurrentStrength cfg)))
      (Param @"slew_rate" @Integer (maybe (-1) toInteger (ioSlewRate cfg)))
      (Param @"location" @String (noneString (ioLocation cfg)))
      (Param @"enable_bus_hold" @String (onOffString (ioBusHold cfg)))
      (Param @"weak_pull_up_resistor" @String (weakPullUp pad))
      (Param @"termination" @String (noneString (ioTermination cfg)))
      (BitBiSignalInPort @"io" pad)
      (Port @"i" (tristateI out))
      (Port @"oe" (tristateOe out))

{- | Open-drain output (@OPNDRN@). Drives the pad low for a 0, and puts it in
high impedance for a 1. The primitive has no parameters, so it can't set the
weak pull-up resistor of a 'PullUp' pad: set @WEAK_PULL_UP_RESISTOR@ in the
Quartus settings, or use 'iobuf' with @Just 0@ and 'Nothing' instead, which
also reads back the level of the pad.

Simulation: 'writeToBiSignal', with @Just 0@ for a 0 and 'Nothing' for a 1.
-}
opndrn ::
  forall ds dom.
  (HasBiSignalDefault ds, IntelPull ds, KnownDomain dom) =>
  -- | Pad (@out@ in Verilog, @a_out@ in VHDL)
  BiSignalIn ds dom 1 ->
  -- | Input (@in@ in Verilog, @a_in@ in VHDL)
  Signal dom Bit ->
  -- | Pad
  BiSignalOut ds dom 1
opndrn = opndrnPrim

-- | Black box of 'opndrn'. The ports of @OPNDRN@ have different names in
-- Verilog and VHDL, which 'inst' doesn't support.
opndrnPrim :: BiSignalIn ds dom 1 -> Signal dom Bit -> BiSignalOut ds dom 1
opndrnPrim pad i = writeToBiSignal pad (openDrain <$> i)
 where
  openDrain :: Bit -> Maybe Bit
  openDrain 0 = Just 0
  openDrain _ = Nothing
{-# OPAQUE opndrnPrim #-}
{-# ANN opndrnPrim hasBlackBox #-}
{-# ANN opndrnPrim (InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
  BlackBox:
    name: Clash.Cores.Intel.IO.opndrnPrim
    kind: Declaration
    renderVoid: RenderVoid
    template: |-
      opndrn ~GENSYM[opndrn_inst][0] (.in(~ARG[1]), .out(~ARG[0]));
  |]) #-}
{-# ANN opndrnPrim (InlineYamlPrimitive [VHDL] [__i|
  BlackBox:
    name: Clash.Cores.Intel.IO.opndrnPrim
    kind: Declaration
    renderVoid: RenderVoid
    libraries: [altera]
    imports: [altera.altera_primitives_components.all]
    template: |-
      ~GENSYM[opndrn_inst][0] : opndrn port map (a_in => ~VAR[a_in][1], a_out => ~ARG[0](0));
  |]) #-}

{- | Differential input buffer (@ALT_INBUF_DIFF@). Returns the level of the
positive input if the inputs have different levels, and is undefined
otherwise, like Intel's simulation model.

Uses 'ioStandard', 'ioBusHold', 'ioTermination' (e.g. @\"DIFFERENTIAL\"@) and
'ioLocation'.
-}
inbufDiff ::
  forall dom.
  (KnownDomain dom) =>
  IoBufferConfig ->
  -- | Positive input (@i@)
  Signal dom Bit ->
  -- | Negative input (@ibar@)
  Signal dom Bit ->
  -- | Output (@o@)
  Signal dom Bit
inbufDiff cfg p n
  | clashSimulation = receiver "inbufDiff" <$> p <*> n
  | otherwise = unPort go
 where
  go :: Port "o" dom Bit
  go =
    inst
      (alteraPrimitive "alt_inbuf_diff")
      (Param @"io_standard" @String (noneString (ioStandard cfg)))
      (Param @"location" @String (noneString (ioLocation cfg)))
      (Param @"enable_bus_hold" @String (onOffString (ioBusHold cfg)))
      (Param @"termination" @String (noneString (ioTermination cfg)))
      (Port @"i" p)
      (Port @"ibar" n)

{- | Differential output buffer (@ALT_OUTBUF_DIFF@). Returns the positive and
negative output: the input and its complement.

Uses all fields of 'IoBufferConfig'.
-}
outbufDiff ::
  forall dom.
  (KnownDomain dom) =>
  IoBufferConfig ->
  -- | Input (@i@)
  Signal dom Bit ->
  -- | Positive (@o@) and negative (@obar@) output
  (Signal dom Bit, Signal dom Bit)
outbufDiff cfg i
  | clashSimulation = (i, complement <$> i)
  | otherwise = (unPort o, unPort obar)
 where
  (o, obar) = go
  go :: (Port "o" dom Bit, Port "obar" dom Bit)
  go =
    inst
      (alteraPrimitive "alt_outbuf_diff")
      (Param @"io_standard" @String (noneString (ioStandard cfg)))
      (Param @"current_strength_new" @String (noneString (ioCurrentStrength cfg)))
      (Param @"slew_rate" @Integer (maybe (-1) toInteger (ioSlewRate cfg)))
      (Param @"location" @String (noneString (ioLocation cfg)))
      (Param @"enable_bus_hold" @String (onOffString (ioBusHold cfg)))
      (Param @"termination" @String (noneString (ioTermination cfg)))
      (Port @"i" i)

{- | Differential bidirectional buffer (@ALT_IOBUF_DIFF@). Drives the positive
pad with the bit of a 'Just' and the negative pad with its complement, and puts
both in high impedance for 'Nothing'. Returns the level of the differential
pair, whether the buffer drives it or not: the level of the positive pad if the
pads have different levels, undefined otherwise.

The pads are v'Floating': a pull resistor on a differential pair would pull
both pads to the same level, which the receiver can't interpret.

Uses all fields of 'IoBufferConfig'.

Simulation: 'writeToBiSignal' and 'readFromBiSignal'.
-}
iobufDiff ::
  forall dom.
  (KnownDomain dom) =>
  IoBufferConfig ->
  -- | Positive pad (@io@)
  BiSignalIn 'Floating dom 1 ->
  -- | Negative pad (@iobar@)
  BiSignalIn 'Floating dom 1 ->
  -- | Bit to drive onto the pads, or 'Nothing' for high impedance (@i@ and
  -- @oe@)
  Signal dom (Maybe Bit) ->
  -- | Positive (@io@) and negative (@iobar@) pad, and the level of the
  -- differential pair (@o@)
  (BiSignalOut 'Floating dom 1, BiSignalOut 'Floating dom 1, Signal dom Bit)
iobufDiff cfg padP padN out
  | clashSimulation =
      ( writeToBiSignal padP out
      , writeToBiSignal padN (fmap complement <$> out)
      , receiver "iobufDiff" <$> readFromBiSignal padP <*> readFromBiSignal padN
      )
  | otherwise = (unPort ioOut, unPort iobarOut, unPort o)
 where
  (ioOut, iobarOut, o) = go
  go ::
    ( BiSignalOutPort "io" 'Floating dom 1
    , BiSignalOutPort "iobar" 'Floating dom 1
    , Port "o" dom Bit
    )
  go =
    inst
      (alteraPrimitive "alt_iobuf_diff"){renderVoid = True}
      (Param @"io_standard" @String (noneString (ioStandard cfg)))
      (Param @"current_strength_new" @String (noneString (ioCurrentStrength cfg)))
      (Param @"slew_rate" @Integer (maybe (-1) toInteger (ioSlewRate cfg)))
      (Param @"location" @String (noneString (ioLocation cfg)))
      (Param @"enable_bus_hold" @String (onOffString (ioBusHold cfg)))
      (Param @"termination" @String (noneString (ioTermination cfg)))
      (BitBiSignalInPort @"io" padP)
      (BitBiSignalInPort @"iobar" padN)
      (Port @"i" (tristateI out))
      (Port @"oe" (tristateOe out))
