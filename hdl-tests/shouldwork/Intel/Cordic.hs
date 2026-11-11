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
import Clash.Cores.Intel.Cordic.Internal (AtanFunction(..))

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

atan2W10TB :: Signal Dom Bool
atan2W10TB = romTB simLibraries atan2W10Dut $(memBlobTH (Just 0) (trace atan2W10Dut (cycles (vectorsS Atan2Function 8 1500 31))))
{-# ANN atan2W10TB (Synthesize "atan2W10TB" [] (PortName "done")) #-}

atan2W8F14TB :: Signal Dom Bool
atan2W8F14TB = romTB simLibraries atan2W8F14Dut $(memBlobTH (Just 0) (trace atan2W8F14Dut (cycles (vectorsS Atan2Function 14 1500 32))))
{-# ANN atan2W8F14TB (Synthesize "atan2W8F14TB" [] (PortName "done")) #-}

atan2W6F20TB :: Signal Dom Bool
atan2W6F20TB = romTB simLibraries atan2W6F20Dut $(memBlobTH (Just 0) (trace atan2W6F20Dut (cycles (vectorsS Atan2Function 20 600 33))))
{-# ANN atan2W6F20TB (Synthesize "atan2W6F20TB" [] (PortName "done")) #-}

atan2W12F20TB :: Signal Dom Bool
atan2W12F20TB = romTB simLibraries atan2W12F20Dut $(memBlobTH (Just 0) (trace atan2W12F20Dut (cycles (vectorsS Atan2Function 20 1500 34))))
{-# ANN atan2W12F20TB (Synthesize "atan2W12F20TB" [] (PortName "done")) #-}

atan2W32TB :: Signal Dom Bool
atan2W32TB = romTB simLibraries atan2W32Dut $(memBlobTH (Just 0) (trace atan2W32Dut (cycles (vectorsS Atan2Function 28 1500 35))))
{-# ANN atan2W32TB (Synthesize "atan2W32TB" [] (PortName "done")) #-}

atan2UnsignedTB :: Signal Dom Bool
atan2UnsignedTB = romTB simLibraries atan2UnsignedDut $(memBlobTH (Just 0) (trace atan2UnsignedDut (cycles (vectorsU Atan2Function 12 1500 36))))
{-# ANN atan2UnsignedTB (Synthesize "atan2UnsignedTB" [] (PortName "done")) #-}

atan2LutTB :: Signal Dom Bool
atan2LutTB = romTB simLibraries atan2LutDut $(memBlobTH (Just 0) (trace atan2LutDut (cycles (vectorsS Atan2LutFunction 6 1500 44))))
{-# ANN atan2LutTB (Synthesize "atan2LutTB" [] (PortName "done")) #-}

atan2LutW16TB :: Signal Dom Bool
atan2LutW16TB = romTB simLibraries atan2LutW16Dut $(memBlobTH (Just 0) (trace atan2LutW16Dut (cycles (vectorsS Atan2LutFunction 14 1500 45))))
{-# ANN atan2LutW16TB (Synthesize "atan2LutW16TB" [] (PortName "done")) #-}

atan2UnsignedLutTB :: Signal Dom Bool
atan2UnsignedLutTB = romTB simLibraries atan2UnsignedLutDut $(memBlobTH (Just 0) (trace atan2UnsignedLutDut (cycles (vectorsU Atan2LutFunction 16 1500 46))))
{-# ANN atan2UnsignedLutTB (Synthesize "atan2UnsignedLutTB" [] (PortName "done")) #-}

vecTranslateTB :: Signal Dom Bool
vecTranslateTB = romTB simLibraries vecTranslateDut $(memBlobTH (Just 0) (trace vecTranslateDut (cycles (vectorsS VecTranslateFunction 12 1500 37))))
{-# ANN vecTranslateTB (Synthesize "vecTranslateTB" [] (PortName "done")) #-}

vecTranslateCompTB :: Signal Dom Bool
vecTranslateCompTB = romTB simLibraries vecTranslateCompDut $(memBlobTH (Just 0) (trace vecTranslateCompDut (cycles (vectorsS VecTranslateFunction 8 1500 38))))
{-# ANN vecTranslateCompTB (Synthesize "vecTranslateCompTB" [] (PortName "done")) #-}

vecTranslateUnsignedCompTB :: Signal Dom Bool
vecTranslateUnsignedCompTB = romTB simLibraries vecTranslateUnsignedCompDut $(memBlobTH (Just 0) (trace vecTranslateUnsignedCompDut (cycles (vectorsU VecTranslateFunction 10 1500 39))))
{-# ANN vecTranslateUnsignedCompTB (Synthesize "vecTranslateUnsignedCompTB" [] (PortName "done")) #-}

vecRotate4TB :: Signal Dom Bool
vecRotate4TB = romTB simLibraries vecRotate4Dut $(memBlobTH (Just 0) (trace vecRotate4Dut (cycles (rotations 4 1000 41))))
{-# ANN vecRotate4TB (Synthesize "vecRotate4TB" [] (PortName "done")) #-}

vecRotate8TB :: Signal Dom Bool
vecRotate8TB = romTB simLibraries vecRotate8Dut $(memBlobTH (Just 0) (trace vecRotate8Dut (cycles (rotations 8 1500 42))))
{-# ANN vecRotate8TB (Synthesize "vecRotate8TB" [] (PortName "done")) #-}

vecRotate16x20TB :: Signal Dom Bool
vecRotate16x20TB = romTB simLibraries vecRotate16x20Dut $(memBlobTH (Just 0) (trace vecRotate16x20Dut (cycles (rotations 20 1500 43))))
{-# ANN vecRotate16x20TB (Synthesize "vecRotate16x20TB" [] (PortName "done")) #-}
