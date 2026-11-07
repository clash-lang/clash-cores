{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Tests of "Clash.Cores.Intel.Debug.VirtualJtag" against the simulation
models of @sld_virtual_jtag@ in Intel's @altera_mf@ library, which simulate a
JTAG host performing the given scans. The test benches compare the outputs of
the models with the Clash model, cycle by cycle of TCK, for several widths of
the virtual instruction register and several lists of scans. See
"VirtualJtagDuts".
-}
module VirtualJtag where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Debug.VirtualJtag

import VirtualJtagDuts

virtualJtag4TB :: Signal Slow Bool
virtualJtag4TB =
  virtualJtagTB @4
    $(listToVecTH [IrScan 5, DrScan 8 0xa5, Idle 1, DrScan 12 0x123, IrScan 0xa, DrScan 3 5, IrScan 0xf])
    $(lift (timeoutCycles 4 [Idle 1]))
    $(memBlobTH (Just 0) (expectedTrace @4 [IrScan 5, DrScan 8 0xa5, Idle 1, DrScan 12 0x123, IrScan 0xa, DrScan 3 5, IrScan 0xf]))
{-# ANN virtualJtag4TB (Synthesize "virtualJtag4TB" [] (PortName "done")) #-}

virtualJtag1TB :: Signal Slow Bool
virtualJtag1TB =
  virtualJtagTB @1
    $(listToVecTH (randomScans 1 20 1))
    $(lift (timeoutCycles 1 (randomScans 1 20 1)))
    $(memBlobTH (Just 0) (expectedTrace @1 (randomScans 1 20 1)))
{-# ANN virtualJtag1TB (Synthesize "virtualJtag1TB" [] (PortName "done")) #-}

virtualJtag7TB :: Signal Slow Bool
virtualJtag7TB =
  virtualJtagTB @7
    $(listToVecTH (randomScans 7 40 2))
    $(lift (timeoutCycles 7 (randomScans 7 40 2)))
    $(memBlobTH (Just 0) (expectedTrace @7 (randomScans 7 40 2)))
{-# ANN virtualJtag7TB (Synthesize "virtualJtag7TB" [] (PortName "done")) #-}

virtualJtag24TB :: Signal Slow Bool
virtualJtag24TB =
  virtualJtagTB @24
    $(listToVecTH (randomScans 24 12 3))
    $(lift (timeoutCycles 24 (randomScans 24 12 3)))
    $(memBlobTH (Just 0) (expectedTrace @24 (randomScans 24 12 3)))
{-# ANN virtualJtag24TB (Synthesize "virtualJtag24TB" [] (PortName "done")) #-}
