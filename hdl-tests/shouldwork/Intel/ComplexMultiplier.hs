{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Tests of "Clash.Cores.Intel.ComplexMultiplier" against the netlists
Quartus generates for @altmult_complex@, see "ComplexMultiplierSim". See
"IpTestCommon" for how the test benches work.
-}
module ComplexMultiplier where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Internal (simLibraryFiles)

import ComplexMultiplierDuts
import ComplexMultiplierSim (referenceFiles)
import IpTestCommon

-- | The reference netlists of all configurations in 'refConfigs', the atoms
-- they use, and the component declaration of @altmult_complex@, in a library
-- named @altera_mf@, see "ComplexMultiplierSim"
alteraMf :: Bool -> Bool
alteraMf = simLibraryFiles $(referenceFiles refConfigs)
{-# OPAQUE alteraMf #-}
{-# ANN alteraMf (Synthesize "altera_mf" [PortName "i"] (PortName "o")) #-}

signedTB :: Signal Dom Bool
signedTB =
  romTB alteraMf signedDut
    $(memBlobTH (Just 0) (trace signedDut (cycles (operands 1500 1))))
{-# ANN signedTB (Synthesize "signedTB" [] (PortName "done")) #-}

signedNarrowTB :: Signal Dom Bool
signedNarrowTB =
  romTB alteraMf signedNarrowDut
    $(memBlobTH (Just 0) (trace signedNarrowDut (cycles (operands 1500 2))))
{-# ANN signedNarrowTB (Synthesize "signedNarrowTB" [] (PortName "done")) #-}

signedWideTB :: Signal Dom Bool
signedWideTB =
  romTB alteraMf signedWideDut
    $(memBlobTH (Just 0) (trace signedWideDut (cycles (operands 1500 3))))
{-# ANN signedWideTB (Synthesize "signedWideTB" [] (PortName "done")) #-}

unsignedTB :: Signal Dom Bool
unsignedTB =
  romTB alteraMf unsignedDut
    $(memBlobTH (Just 0) (trace unsignedDut (cycles (operands 1500 4))))
{-# ANN unsignedTB (Synthesize "unsignedTB" [] (PortName "done")) #-}

unsignedNarrowTB :: Signal Dom Bool
unsignedNarrowTB =
  romTB alteraMf unsignedNarrowDut
    $(memBlobTH (Just 0) (trace unsignedNarrowDut (cycles (operands 1500 5))))
{-# ANN unsignedNarrowTB (Synthesize "unsignedNarrowTB" [] (PortName "done")) #-}

mixedTB :: Signal Dom Bool
mixedTB =
  romTB alteraMf mixedDut
    $(memBlobTH (Just 0) (trace mixedDut (cycles (operands 1500 6))))
{-# ANN mixedTB (Synthesize "mixedTB" [] (PortName "done")) #-}

tinyTB :: Signal Dom Bool
tinyTB =
  romTB alteraMf tinyDut
    $(memBlobTH (Just 0) (trace tinyDut (cycles (operands 200 7))))
{-# ANN tinyTB (Synthesize "tinyTB" [] (PortName "done")) #-}
