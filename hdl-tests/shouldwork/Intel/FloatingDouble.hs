{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Tests of "Clash.Cores.Intel.Floating.Double" against the simulation models
of the Floating Point Functions IP that Quartus generates, in double
precision. See "IpTestCommon" for how the test benches work, and
"IntelSimLib" for how the IP's simulation models end up in the HDL.

The models were verified on millions of inputs outside of these tests; the
tests feed special values and random inputs, see 'doublesIn' and
'doublePairs'.
-}
module FloatingDouble where

import Clash.Explicit.Prelude

import IpTestCommon
import FloatingDoubleDuts
import IntelSimLib (simLibraries)

addTB :: Signal Dom Bool
addTB = romTB simLibraries addDut $(memBlobTH (Just 0) (trace addDut (cycles (doublePairs 2000 80))))
{-# ANN addTB (Synthesize "addTB" [] (PortName "done")) #-}

subTB :: Signal Dom Bool
subTB = romTB simLibraries subDut $(memBlobTH (Just 0) (trace subDut (cycles (doublePairs 2000 81))))
{-# ANN subTB (Synthesize "subTB" [] (PortName "done")) #-}

mulTB :: Signal Dom Bool
mulTB = romTB simLibraries mulDut $(memBlobTH (Just 0) (trace mulDut (cycles (doublePairs 2000 82))))
{-# ANN mulTB (Synthesize "mulTB" [] (PortName "done")) #-}

divTB :: Signal Dom Bool
divTB = romTB simLibraries divDut $(memBlobTH (Just 0) (trace divDut (cycles (doublePairs 2000 83))))
{-# ANN divTB (Synthesize "divTB" [] (PortName "done")) #-}

sqrtTB :: Signal Dom Bool
sqrtTB = romTB simLibraries sqrtDut $(memBlobTH (Just 0) (trace sqrtDut (cycles (doublesIn 1 2046 2000 70))))
{-# ANN sqrtTB (Synthesize "sqrtTB" [] (PortName "done")) #-}

rsqrtTB :: Signal Dom Bool
rsqrtTB = romTB simLibraries rsqrtDut $(memBlobTH (Just 0) (trace rsqrtDut (cycles (rsqrtDoublesIn 2000 73))))
{-# ANN rsqrtTB (Synthesize "rsqrtTB" [] (PortName "done")) #-}

recipTB :: Signal Dom Bool
recipTB = romTB simLibraries recipDut $(memBlobTH (Just 0) (trace recipDut (cycles (doublesIn 1 2046 2000 71))))
{-# ANN recipTB (Synthesize "recipTB" [] (PortName "done")) #-}

expTB :: Signal Dom Bool
expTB = romTB simLibraries expDut $(memBlobTH (Just 0) (trace expDut (cycles (expDoublesIn 2000 72))))
{-# ANN expTB (Synthesize "expTB" [] (PortName "done")) #-}

hypotTB :: Signal Dom Bool
hypotTB = romTB simLibraries hypotDut $(memBlobTH (Just 0) (trace hypotDut (cycles (hypotDoublesIn 1000 75))))
{-# ANN hypotTB (Synthesize "hypotTB" [] (PortName "done")) #-}

