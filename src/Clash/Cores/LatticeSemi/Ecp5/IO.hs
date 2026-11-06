{-|
  Copyright   :  (C) 2020, Foamspace corp & Christoph Mayer
                     2022-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  hcab14@gmail.com

Lattice ECP5 I/O buffer primitives. They are documented in Lattice's
[ECP5 and ECP5-5G sysIO Usage Guide (FPGA-TN-02032)](https://www.latticesemi.com/view_document?document_id=50464)
and the "FPGA Libraries Reference Guide" of Lattice Diamond.

The simulation models are tested against the community simulation models that
ship with Yosys (@share\/yosys\/ecp5\/cells_io.vh@, validated against Yosys
0.70). Those models describe the buffers' logic, not their electrical
behaviour: they don't model pull resistors. The documentation of each function
says where this matters.

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

For more information see section \"Programmable PULLUP/PULLDOWN\" and the
\"Bidirectional Buffer\" primitives in
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
