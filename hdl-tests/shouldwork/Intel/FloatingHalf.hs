{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Tests of "Clash.Cores.Intel.Floating.Half" against the simulation models
of the Floating Point Functions IP that Quartus generates, in half precision.
See "IpTestCommon" for how the test benches work, and "IntelSimLib" for how
the IP's simulation models end up in the HDL.

The models were verified on all 2^16 inputs outside of these tests; the tests
feed every 17th input and all inputs of two or three binades, see 'everyNth'
and 'binades'.
-}
module FloatingHalf where

import Clash.Explicit.Prelude

import IpTestCommon
import FloatingHalfDuts
import IntelSimLib (simLibraries)

sqrtTB :: Signal Dom Bool
sqrtTB = romTB simLibraries sqrtDut $(memBlobTH (Just 0) (trace sqrtDut (cycles (halves 17 0 False 15 2))))
{-# ANN sqrtTB (Synthesize "sqrtTB" [] (PortName "done")) #-}

recipTB :: Signal Dom Bool
recipTB = romTB simLibraries recipDut $(memBlobTH (Just 0) (trace recipDut (cycles (halves 17 1 True 28 3))))
{-# ANN recipTB (Synthesize "recipTB" [] (PortName "done")) #-}

rsqrtTB :: Signal Dom Bool
rsqrtTB = romTB simLibraries rsqrtDut $(memBlobTH (Just 0) (trace rsqrtDut (cycles (halves 17 2 False 15 2))))
{-# ANN rsqrtTB (Synthesize "rsqrtTB" [] (PortName "done")) #-}

expTB :: Signal Dom Bool
expTB = romTB simLibraries expDut $(memBlobTH (Just 0) (trace expDut (cycles (everyNth 17 3 <> binades False 14 1 <> binades True 15 1))))
{-# ANN expTB (Synthesize "expTB" [] (PortName "done")) #-}

exp2TB :: Signal Dom Bool
exp2TB = romTB simLibraries exp2Dut $(memBlobTH (Just 0) (trace exp2Dut (cycles (everyNth 17 4 <> binades False 14 1 <> binades True 14 1))))
{-# ANN exp2TB (Synthesize "exp2TB" [] (PortName "done")) #-}

exp10TB :: Signal Dom Bool
exp10TB = romTB simLibraries exp10Dut $(memBlobTH (Just 0) (trace exp10Dut (cycles (everyNth 17 5 <> binades False 13 1 <> binades True 13 1))))
{-# ANN exp10TB (Synthesize "exp10TB" [] (PortName "done")) #-}

logTB :: Signal Dom Bool
logTB = romTB simLibraries logDut $(memBlobTH (Just 0) (trace logDut (cycles (everyNth 17 6 <> binades False 14 2))))
{-# ANN logTB (Synthesize "logTB" [] (PortName "done")) #-}

log2TB :: Signal Dom Bool
log2TB = romTB simLibraries log2Dut $(memBlobTH (Just 0) (trace log2Dut (cycles (everyNth 17 7 <> binades False 14 2))))
{-# ANN log2TB (Synthesize "log2TB" [] (PortName "done")) #-}

log10TB :: Signal Dom Bool
log10TB = romTB simLibraries log10Dut $(memBlobTH (Just 0) (trace log10Dut (cycles (everyNth 17 8 <> binades False 14 2))))
{-# ANN log10TB (Synthesize "log10TB" [] (PortName "done")) #-}

log1pTB :: Signal Dom Bool
log1pTB = romTB simLibraries log1pDut $(memBlobTH (Just 0) (trace log1pDut (cycles (everyNth 17 9 <> binades False 13 2 <> binades True 13 1))))
{-# ANN log1pTB (Synthesize "log1pTB" [] (PortName "done")) #-}
