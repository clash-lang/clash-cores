{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Tests of "Clash.Cores.Intel.Divide" and "Clash.Cores.Intel.Sqrt" against
the simulation models of @lpm_divide@ and @altsqrt@ in Intel's simulation
libraries. See "IpTestCommon" for how the test benches work.
-}
module Integer where

import Clash.Explicit.Prelude

import IntegerDuts
import IntelSimLib (simLibraries)
import IpTestCommon

divideUnsignedTB :: Signal Dom Bool
divideUnsignedTB =
  romTB simLibraries divideUnsignedDut
    $(memBlobTH (Just 0) (trace divideUnsignedDut (cycles (numerDenoms 3000 1))))
{-# ANN divideUnsignedTB (Synthesize "divideUnsignedTB" [] (PortName "done")) #-}

divideSignedTB :: Signal Dom Bool
divideSignedTB =
  romTB simLibraries divideSignedDut
    $(memBlobTH (Just 0) (trace divideSignedDut (cycles (numerDenoms 3000 2))))
{-# ANN divideSignedTB (Synthesize "divideSignedTB" [] (PortName "done")) #-}

sqrtTB :: Signal Dom Bool
sqrtTB =
  romTB simLibraries sqrtDut
    $(memBlobTH (Just 0) (trace sqrtDut (cycles (radicals 3000 3))))
{-# ANN sqrtTB (Synthesize "sqrtTB" [] (PortName "done")) #-}
