{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Test of "Clash.Cores.Intel.Debug.VirtualJtag" without scans: the
simulation models only reset the TAP controller and load the @USER1@
instruction. Only for Verilog: Intel's VHDL model can't be elaborated without
scans (Vivado reports an index into a null range). See "VirtualJtag".
-}
module VirtualJtagNoScans where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Debug.VirtualJtag

import VirtualJtagDuts

virtualJtagNoScansTB :: Signal Slow Bool
virtualJtagNoScansTB =
  virtualJtagTB @3
    (Nil :: Vec 0 VirtualJtagScan)
    $(lift (timeoutCycles 3 []))
    $(memBlobTH (Just 0) (expectedTrace @3 []))
{-# ANN virtualJtagNoScansTB (Synthesize "virtualJtagNoScansTB" [] (PortName "done")) #-}
