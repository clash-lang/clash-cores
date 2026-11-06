{- | Makes Intel's simulation libraries available to the HDL simulators of the
test suite, so that they can simulate the models of Intel IP that
"Clash.Cores.Intel.Internal" adds to the HDL when @CLASH_INTEL_SIM_FAMILY@ is
set, and megafunctions like @lpm_divide@ that Clash instantiates directly. This
needs no changes to the test harness (@clash-testsuite@).

The models refer to libraries such as @altera_mf@, @lpm@ and @altera_lnsim@.
The functions in this module are top entities named after these libraries.
They behave like 'id', but add files from Quartus's @eda\/sim_lib@ directory to
the HDL output (see 'simLibraryFiles'). Vivado compiles the VHDL files of a top
entity into a library named after it, which creates the libraries the models
need. Icarus Verilog compiles all Verilog files in the HDL directories. A test
bench passes a signal through 'simLibraries' to make them part of its HDL.

GHDL is not supported: the test harness analyzes in strict VHDL-93 mode, in
which GHDL doesn't bind the component instantiations in Intel's generated VHDL
(they rely on the default binding to entities in the work library). The
libraries also need @-fsynopsys@.

The tests generate IP for MAX 10 devices, which uses the VHDL models of
@altsyncram@ (in @altera_mf@) and @lpm_mult@ (in @lpm@). The IP for Cyclone V
uses @altera_syncram@ instead, whose SystemVerilog model (@altera_lnsim.sv@)
doesn't simulate correctly in Vivado's simulator when the design is VHDL. So
@altera_lnsim@ only needs its component declarations.
-}
module IntelIpSim (simLibraries) where

import Clash.Prelude

import Clash.Cores.Intel.Internal (simLibraryFiles)

alteraMf :: Bool -> Bool
alteraMf =
  simLibraryFiles ("altera_mf_components.vhd" :> "altera_mf.vhd" :> "altera_mf.v" :> Nil)
{-# OPAQUE alteraMf #-}
{-# ANN alteraMf (Synthesize "altera_mf" [PortName "i"] (PortName "o")) #-}

lpm :: Bool -> Bool
lpm = simLibraryFiles ("220pack.vhd" :> "220model.vhd" :> "220model.v" :> Nil)
{-# OPAQUE lpm #-}
{-# ANN lpm (Synthesize "lpm" [PortName "i"] (PortName "o")) #-}

alteraLnsim :: Bool -> Bool
alteraLnsim = simLibraryFiles ("altera_lnsim_components.vhd" :> Nil)
{-# OPAQUE alteraLnsim #-}
{-# ANN alteraLnsim (Synthesize "altera_lnsim" [PortName "i"] (PortName "o")) #-}

-- | Make the simulation libraries part of a design
simLibraries :: Bool -> Bool
simLibraries = alteraMf . lpm . alteraLnsim
