{- | Makes Intel's simulation libraries available to the HDL simulators of the
test suite, without changes to the test harness (@clash-testsuite@).

Quartus ships plain Verilog and VHDL simulation models of its primitives,
megafunctions and device atoms in @quartus\/eda\/sim_lib@. The test benches
compare the Haskell simulation models of @clash-cores@ against them, and
against the simulation models of Intel IP that "Clash.Cores.Intel.Internal"
generates when @CLASH_INTEL_SIM_FAMILY@ is set (see @hdl-tests/Main.hs@).

The functions in this module are top entities named after the VHDL libraries
Quartus puts the models in (@altera_mf@, @altera@, @lpm@, ...). They behave
like 'id', but use 'simLibraryFiles' to add the library's files to the HDL
output. Vivado compiles the VHDL files of a top entity into a library named
after it, which is the library the wrappers in @clash-cores@ and the generated
IP import (e.g. @library altera_mf; use altera_mf.altera_mf_components.all;@).
Icarus Verilog compiles all Verilog files in the HDL directories. A test bench
passes its @done@ signal through 'withIntelSim' (or one of its variants) to
make the libraries part of its HDL.

GHDL is not supported: it needs @-fsynopsys@ and @-fexplicit@ to compile
@altera_mf.vhd@, which the test harness doesn't pass, and in strict VHDL-93
mode it doesn't bind the component instantiations in Intel's generated VHDL
(they rely on the default binding to entities in the work library). So VHDL
is tested with Vivado only.

The IP tests generate IP for MAX 10 devices, which uses the VHDL models of
@altsyncram@ (in @altera_mf@) and @lpm_mult@ (in @lpm@). The IP for Cyclone V
uses @altera_syncram@ instead, whose SystemVerilog model (@altera_lnsim.sv@)
doesn't simulate correctly in Vivado's simulator when the design is VHDL. So
'alteraLnsim' only provides its component declarations.

The files are read from @$QUARTUS_ROOTDIR\/eda\/sim_lib@ when Clash generates
HDL; @hdl-tests/Main.hs@ sets @QUARTUS_ROOTDIR@.
-}
module IntelSimLib
  ( -- * Making libraries part of a test bench
    withIntelSim
  , withCycloneIveSim
  , withFiftyfivenmSim
  , simLibraries
    -- * Libraries
  , alteraMf
  , altera
  , lpm
  , alteraLnsim
  , cycloneive
  , fiftyfivenm
  ) where

import Clash.Prelude

import Clash.Cores.Intel.Internal (simLibraryFiles)

-- | Megafunctions such as @altsyncram@ and @dcfifo@
alteraMf :: Bool -> Bool
alteraMf =
  simLibraryFiles ("altera_mf_components.vhd" :> "altera_mf.vhd" :> "altera_mf.v" :> Nil)
{-# OPAQUE alteraMf #-}
{-# ANN alteraMf (Synthesize "altera_mf" [PortName "i"] (PortName "o")) #-}

-- | Primitives such as @alt_iobuf@ and @opndrn@
altera :: Bool -> Bool
altera =
  simLibraryFiles
    ( "altera_primitives_components.vhd" :> "altera_primitives.vhd"
   :> "altera_primitives.v" :> Nil )
{-# OPAQUE altera #-}
{-# ANN altera (Synthesize "altera" [PortName "i"] (PortName "o")) #-}

-- | Library of parameterized modules, such as @lpm_mult@ and @lpm_divide@
lpm :: Bool -> Bool
lpm = simLibraryFiles ("220pack.vhd" :> "220model.vhd" :> "220model.v" :> Nil)
{-# OPAQUE lpm #-}
{-# ANN lpm (Synthesize "lpm" [PortName "i"] (PortName "o")) #-}

-- | Component declarations of @altera_lnsim@, see the module documentation
alteraLnsim :: Bool -> Bool
alteraLnsim = simLibraryFiles ("altera_lnsim_components.vhd" :> Nil)
{-# OPAQUE alteraLnsim #-}
{-# ANN alteraLnsim (Synthesize "altera_lnsim" [PortName "i"] (PortName "o")) #-}

-- | Atoms of Cyclone IV E
cycloneive :: Bool -> Bool
cycloneive =
  simLibraryFiles
    ( "cycloneive_atoms.v" :> "cycloneive_atoms.vhd" :> "cycloneive_components.vhd"
   :> Nil )
{-# OPAQUE cycloneive #-}
{-# ANN cycloneive (Synthesize "cycloneive" [PortName "i"] (PortName "o")) #-}

-- | Atoms of MAX 10
fiftyfivenm :: Bool -> Bool
fiftyfivenm =
  simLibraryFiles
    ( "fiftyfivenm_atoms.v" :> "fiftyfivenm_atoms.vhd" :> "fiftyfivenm_components.vhd"
   :> Nil )
{-# OPAQUE fiftyfivenm #-}
{-# ANN fiftyfivenm (Synthesize "fiftyfivenm" [PortName "i"] (PortName "o")) #-}

-- | Make the libraries 'alteraMf', 'altera', 'lpm' and 'alteraLnsim' part of a
-- design
simLibraries :: Bool -> Bool
simLibraries = alteraMf . altera . lpm . alteraLnsim

{- | Returns its argument, but makes the simulation libraries of 'simLibraries'
part of the HDL of the test bench using it. Apply it to the @done@ signal of a
test bench.
-}
withIntelSim :: Signal dom Bool -> Signal dom Bool
withIntelSim = fmap simLibraries

-- | Like 'withIntelSim', but also makes the Cyclone IV E atoms part of the HDL
withCycloneIveSim :: Signal dom Bool -> Signal dom Bool
withCycloneIveSim = withIntelSim . fmap cycloneive

-- | Like 'withIntelSim', but also makes the MAX 10 atoms part of the HDL
withFiftyfivenmSim :: Signal dom Bool -> Signal dom Bool
withFiftyfivenmSim = withIntelSim . fmap fiftyfivenm
