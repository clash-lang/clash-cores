{-|
  Copyright   :  (C) 2020, Foamspace corp & Christoph Mayer
                     2022-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  hcab14@gmail.com

Lattice ECP5 I/O buffer primitives. They are documented in Lattice's
[ECP5 and ECP5-5G sysIO Usage Guide (FPGA-TN-02032)](https://www.latticesemi.com/view_document?document_id=50464)
and the "FPGA Libraries Reference Guide" of Lattice Diamond.

#yosys#
The simulation models are tested against the community simulation models that
ship with Yosys (@share\/yosys\/ecp5\/cells_io.vh@, validated against Yosys
0.62, which the Nix flake pins, and 0.70). Those models describe the buffers'
logic, not their electrical behaviour: they don't model pull resistors or the
negative legs of differential outputs. Their output buffers (@OLVDS@, @OBCO@)
don't work in event-driven simulators such as IVerilog and Vivado either:
@TRELLIS_IO@ ties an unconnected @T@ input low in an @always \@(*)@ block,
which never executes if @T@ doesn't change, so the output stays undefined. The
documentation of each function says where this matters.

= VHDL

In VHDL, the instances use the component declarations of package
@ecp5u.components@, like designs for Lattice Diamond do. Lattice Diamond
provides the @ecp5u@ library. For Yosys-based flows, the
[GHDL Yosys plugin](https://github.com/ghdl/ghdl-yosys-plugin) provides it in
@library\/ecp5u\/components.vhdl@.

= Unsupported buffers

There are no wrappers for the single-ended input and output buffers @IB@,
@IBPU@, @IBPD@ and @OB@: they don't add anything over plain top entity ports.
Both Lattice Diamond and Yosys insert them automatically, and their attributes
(@IO_TYPE@, @PULLMODE@, @DRIVE@, ...) are set in the LPF constraints file,
for example:

> IOBUF PORT "button" IO_TYPE=LVCMOS33 PULLMODE=UP;

The tristate output buffers @OBZ@, @OBZPU@ and @OBZPD@ don't have wrappers
either: 'bidirectionalBuffer' does the same, while not using its input path is
free. Clash represents a tristate pad as a 'BiSignalIn' and 'BiSignalOut' pair,
which renders as an @inout@ port, while the output of @OBZ@ is an @out@ port.
VHDL doesn't allow connecting those.
-}
{-# LANGUAGE GADTs #-}

module Clash.Cores.LatticeSemi.Ecp5.IO
  ( -- * Bidirectional buffers
    bidirectionalBuffer
    -- * Differential buffers
  , ilvds
  , olvds
  , obco
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.BiSignal (HasBiSignalDefault (..), SBiSignalDefault (..))
import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( BiSignalOutPort, BitBiSignalInPort (..), InstConfig (..), Port (..), inst
  , instConfig, unPort
  )

-- | Instance of an ECP5 primitive, see the module documentation on VHDL.
ecp5 :: String -> InstConfig
ecp5 name =
  (instConfig name)
    { library = Just "ecp5u"
    , libraryImport = Just "ecp5u.components.all"
    }

{- | Bidirectional buffer (@BB@, @BBPU@ or @BBPD@). Drives the pad with the
output bit if the output is enabled, and puts it in high impedance otherwise.
Returns the level of the pad, whether the buffer drives it or not.

The 'BiSignalDefault' of the pad selects the buffer: @BBPU@ (pull-up) for
'PullUp', @BBPD@ (pull-down) for 'PullDown' and @BB@ (no pull resistor) for
v'Floating'. The buffer's @T@ input is high when the output is not enabled.

Pass the 'BiSignalIn' of a top entity argument and return the 'BiSignalOut'
from the top entity. Clash renders a 1-bit 'BiSignalIn' as a vector of one bit,
so the pad is called @name[0]@ in your constraints file.

__NB__: Yosys's simulation models of @BBPU@ and @BBPD@ don't model the pull
resistor: when nothing drives the pad, they read @z@ (an undefined value),
while the Clash simulation reads the level of the pull resistor, like the
hardware does.

For more information see
[FPGA-TN-02032](https://www.latticesemi.com/view_document?document_id=50464).
-}
bidirectionalBuffer ::
  forall ds dom.
  ( HasCallStack
  , HasBiSignalDefault ds
  , KnownDomain dom
  ) =>
  -- | Output enable
  Enable dom ->
  -- | Pad (@B@)
  BiSignalIn ds dom 1 ->
  -- | Output bit (@I@)
  Signal dom Bit ->
  -- | Pad (@B@) and the level of the pad (@O@)
  (BiSignalOut ds dom 1, Signal dom Bit)
bidirectionalBuffer en pad output
  | clashSimulation =
      ( writeToBiSignal pad (mux (fromEnable en) (Just <$> output) (pure Nothing))
      , readFromBiSignal pad
      )
  | otherwise = (unPort padOut, unPort o)
 where
  name = case pullUpMode pad of
    SFloating -> "BB"
    SPullUp -> "BBPU"
    SPullDown -> "BBPD"

  (padOut, o) = go
  go :: (BiSignalOutPort "B" ds dom 1, Port "O" dom Bit)
  go =
    inst
      (ecp5 name){renderVoid = True}
      (BitBiSignalInPort @"B" pad)
      (Port @"I" output)
      (Port @"T" (boolToBit . not <$> fromEnable en))

{- | LVDS input buffer (@ILVDS@). Takes the positive (@A@) and negative (@AN@)
pad of a differential pair.

Simulation: the positive pad, like Yosys's simulation model. The hardware's
output is undefined when both pads have the same level.

Diamond needs the pads to be top entity ports. Yosys and nextpnr don't need
this buffer: they only use the positive pad, and setting @IO_TYPE=LVDS@ on its
port in the LPF constraints file suffices.

For more information see
[FPGA-TN-02032](https://www.latticesemi.com/view_document?document_id=50464).
-}
ilvds ::
  forall dom.
  KnownDomain dom =>
  -- | Positive pad (@A@)
  Signal dom Bit ->
  -- | Negative pad (@AN@)
  Signal dom Bit ->
  -- | Output (@Z@)
  Signal dom Bit
ilvds a an
  | clashSimulation = a
  | otherwise = unPort go
 where
  go :: Port "Z" dom Bit
  go = inst (ecp5 "ILVDS") (Port @"A" a) (Port @"AN" an)

{- | LVDS output buffer (@OLVDS@). Returns the positive (@Z@) and negative
(@ZN@) pad of a differential pair, which should be top entity ports.

Simulation: the input and its complement.

__NB__: The simulation is not tested against Yosys's model of @OLVDS@, as it
can't be: the model leaves the negative pad undriven, and its positive pad stays
undefined in event-driven simulators, see "Clash.Cores.LatticeSemi.Ecp5.IO#yosys".
The test only checks that the instance connects to the model. With Yosys and
nextpnr, setting
@IO_TYPE=LVDS@ on the positive pad's port in the LPF constraints file suffices;
nextpnr then uses the other pin of the pair as the negative pad.

For more information see
[FPGA-TN-02032](https://www.latticesemi.com/view_document?document_id=50464).
-}
olvds ::
  forall dom.
  KnownDomain dom =>
  -- | Input (@A@)
  Signal dom Bit ->
  -- | Positive (@Z@) and negative (@ZN@) pad
  (Signal dom Bit, Signal dom Bit)
olvds a
  | clashSimulation = (a, complement <$> a)
  | otherwise = (unPort z, unPort zn)
 where
  (z, zn) = go
  go :: (Port "Z" dom Bit, Port "ZN" dom Bit)
  go = inst (ecp5 "OLVDS") (Port @"A" a)

{- | Output buffer with complementary outputs (@OBCO@), for differential output
standards that the I/O implements with two complementary single-ended outputs,
such as @LVCMOS33D@. Returns the true (@OT@) and complementary (@OC@) output,
which should be top entity ports.

Simulation: the input and its complement.

__NB__: The simulation is not tested against Yosys's model of @OBCO@, as it
can't be: the model is an @OLVDS@, see 'olvds'. The test only checks that the
instance connects to the model.

For more information see
[FPGA-TN-02032](https://www.latticesemi.com/view_document?document_id=50464).
-}
obco ::
  forall dom.
  KnownDomain dom =>
  -- | Input (@I@)
  Signal dom Bit ->
  -- | True (@OT@) and complementary (@OC@) output
  (Signal dom Bit, Signal dom Bit)
obco i
  | clashSimulation = (i, complement <$> i)
  | otherwise = (unPort ot, unPort oc)
 where
  (ot, oc) = go
  go :: (Port "OT" dom Bit, Port "OC" dom Bit)
  go = inst (ecp5 "OBCO") (Port @"I" i)
