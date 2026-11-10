{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Tests of "Clash.Cores.Intel.Floating.Half" against the simulation models
of the Floating Point Functions IP that Quartus generates, in half precision.
See "IpTestCommon" for how the test benches work, and "IntelSimLib" for how
the IP's simulation models end up in the HDL.

The models were verified on all 2^16 inputs outside of these tests; the tests
feed every 17th input and all inputs of two binades, see 'halves'.
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
