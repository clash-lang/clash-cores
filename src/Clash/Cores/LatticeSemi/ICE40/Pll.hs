{-|
  Copyright   :  (C) 2026, Felix Klein
  License     :  CERN-OHL-P-2.0
  Maintainer  :  felix@qbaylogic.com

This module contains functions for instantiating clock generators on
Lattice ICE40 FPGA's.

We suggest you use a clock generator even if your oscillator runs at
the frequency you want to run your circuit at.

A clock generator generates a stable clock signal for your design at a
configurable frequency. A clock generator in an FPGA is frequently
referred to as a PLL (Phase-Locked Loop). Lattice also refers to them
as PLL's in general but because this is not consistently the case
among FPGA vendors, we choose the more generic term clock generator.
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.LatticeSemi.ICE40.Pll
  ( ice40pllCore
  , ice40pllPad
  ) where

import Clash.Prelude

import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Cores.LatticeSemi.ICE40.Blackboxes.Pll
  (ice40pllCoreTF, ice40pllPadTF)
import Clash.Clocks (Clocks(..))
import Data.String.Interpolate (__i)

-- | Instantiates a Lattice clock generator using the SB_PLL40_CORE
-- primitive. This primitive shall be used exclusively for internal
-- reference clocks.
ice40pllCore ::
  forall domIn domOut.
  (HasAsynchronousReset domIn, KnownDomain domOut) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal
  -- oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  (Clock domOut, Signal domOut Bool)
ice40pllCore = clocks
{-# OPAQUE ice40pllCore #-}
{-# ANN ice40pllCore hasBlackBox #-}
{-# ANN ice40pllCore
  let
    primName = show 'ice40pllCore
    tfName = show 'ice40pllCoreTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}

-- | Instantiates a Lattice clock generator using the SB_PLL40_PAD
-- primitive. This primitive shall be used exclusively for external
-- reference clocks.
ice40pllPad ::
  forall domIn domOut.
  (HasAsynchronousReset domIn, KnownDomain domOut) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal
  -- oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  (Clock domOut, Signal domOut Bool)
ice40pllPad = clocks
{-# OPAQUE ice40pllPad #-}
{-# ANN ice40pllPad hasBlackBox #-}
{-# ANN ice40pllPad
  let
    primName = show 'ice40pllPad
    tfName = show 'ice40pllPadTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}
