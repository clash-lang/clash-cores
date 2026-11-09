{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Tests of "Clash.Cores.Intel.Cordic" against the simulation models of the
CORDIC IP that Quartus generates. See "IpTestCommon" for how the test benches
work, and "IntelSimLib" for how the IP's simulation models end up in the HDL.
-}
module Cordic where

import Clash.Explicit.Prelude

import IpTestCommon
import CordicDuts
import IntelSimLib (simLibraries)

sinCos10TB :: Signal Dom Bool
sinCos10TB = romTB simLibraries sinCos10Dut $(memBlobTH (Just 0) (trace sinCos10Dut (cycles (angles 10 1500 21))))
{-# ANN sinCos10TB (Synthesize "sinCos10TB" [] (PortName "done")) #-}

sinCos16x8TB :: Signal Dom Bool
sinCos16x8TB = romTB simLibraries sinCos16x8Dut $(memBlobTH (Just 0) (trace sinCos16x8Dut (cycles (angles 8 1500 22))))
{-# ANN sinCos16x8TB (Synthesize "sinCos16x8TB" [] (PortName "done")) #-}

sinCos20x16TB :: Signal Dom Bool
sinCos20x16TB = romTB simLibraries sinCos20x16Dut $(memBlobTH (Just 0) (trace sinCos20x16Dut (cycles (angles 16 1500 23))))
{-# ANN sinCos20x16TB (Synthesize "sinCos20x16TB" [] (PortName "done")) #-}

sinCos4TB :: Signal Dom Bool
sinCos4TB = romTB simLibraries sinCos4Dut $(memBlobTH (Just 0) (trace sinCos4Dut (cycles (angles 4 300 24))))
{-# ANN sinCos4TB (Synthesize "sinCos4TB" [] (PortName "done")) #-}

sinCosUnsignedTB :: Signal Dom Bool
sinCosUnsignedTB = romTB simLibraries sinCosUnsignedDut $(memBlobTH (Just 0) (trace sinCosUnsignedDut (cycles (anglesUnsigned 12 1500 25))))
{-# ANN sinCosUnsignedTB (Synthesize "sinCosUnsignedTB" [] (PortName "done")) #-}

sinCos12TB :: Signal Dom Bool
sinCos12TB = romTB simLibraries sinCos12Dut $(memBlobTH (Just 0) (trace sinCos12Dut (cycles (angles 12 1500 26))))
{-# ANN sinCos12TB (Synthesize "sinCos12TB" [] (PortName "done")) #-}
