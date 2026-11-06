{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Shared functionality of the iCE40 primitives.
-}
{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.LatticeSemi.Ice40.Internal (ice40Prim) where

import Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.Internal (InstConfig (..), instConfig)

{- | 'InstConfig' of an iCE40 primitive. In VHDL, the instantiating architecture
declares the component: no VHDL library declares iCE40 primitives for every
tool. Lattice's iCEcube2 and Radiant ship VHDL libraries, each with their own
name, Yosys (with GHDL) and Vivado's simulator (with Yosys' Verilog simulation
models) don't.
-}
ice40Prim :: String -> InstConfig
ice40Prim nm = (instConfig nm){componentDeclaration = True}
