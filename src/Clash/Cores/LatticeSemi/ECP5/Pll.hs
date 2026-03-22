{-|
  Copyright   :  (C) 2026, Felix Klein
  License     :  CERN-OHL-P-2.0
  Maintainer  :  felix@qbaylogic.com

This module contains functions for instantiating clock generators on
Lattice ECP5 FPGA's.

We suggest you use a clock generator even if your oscillator runs at
the frequency you want to run your circuit at.

A clock generator generates a stable clock signal for your design at a
configurable frequency. A clock generator in an FPGA is frequently
referred to as a PLL (Phase-Locked Loop). Lattice also refers to them
as PLL's in general but because this is not consistently the case
among FPGA vendors, we choose the more generic term clock generator.
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.LatticeSemi.ECP5.Pll (ecp5pll) where

import Clash.Prelude

import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Cores.LatticeSemi.ECP5.Blackboxes.Pll (ecp5pllTF)
import Clash.Clocks (Clocks(..))
import Data.String.Interpolate (__i)

-- | Instantiates a Lattice clock generator using the EHXPLLL
-- primitive supported by the Lattice ECP5 FPGAs.
ecp5pll ::
  forall domIn domOut.
  (HasAsynchronousReset domIn, KnownDomain domOut) =>
  -- | Free running clock (e.g. a clock pin connected to a crystal
  -- oscillator)
  Clock domIn ->
  -- | Reset for the clock generator
  Reset domIn ->
  (Clock domOut, Signal domOut Bool)
ecp5pll = clocks
{-# OPAQUE ecp5pll #-}
{-# ANN ecp5pll hasBlackBox #-}
{-# ANN ecp5pll
  let
    primName = show 'ecp5pll
    tfName = show 'ecp5pllTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}
