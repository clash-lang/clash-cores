{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Tests of "Clash.Cores.Intel.Floating" against the simulation models of
the Floating Point Functions IP that Quartus generates. See "IpTestCommon"
for how the test benches work, and "IntelSimLib" for how the IP's simulation
models end up in the HDL.
-}
module Floating where

import Clash.Explicit.Prelude
import qualified Prelude as P


import IpTestCommon
import FloatingDuts
import IntelSimLib (simLibraries)

addTB :: Signal Dom Bool
addTB = romTB simLibraries addDut $(memBlobTH (Just 0) (trace addDut (cycles (floatPairs 2500 1))))
{-# ANN addTB (Synthesize "addTB" [] (PortName "done")) #-}

subTB :: Signal Dom Bool
subTB = romTB simLibraries subDut $(memBlobTH (Just 0) (trace subDut (cycles (floatPairs 2500 2))))
{-# ANN subTB (Synthesize "subTB" [] (PortName "done")) #-}

mulTB :: Signal Dom Bool
mulTB = romTB simLibraries mulDut $(memBlobTH (Just 0) (trace mulDut (cycles (floatPairs 2500 3))))
{-# ANN mulTB (Synthesize "mulTB" [] (PortName "done")) #-}

divTB :: Signal Dom Bool
divTB = romTB simLibraries divDut $(memBlobTH (Just 0) (trace divDut (cycles (floatPairs 2500 4))))
{-# ANN divTB (Synthesize "divTB" [] (PortName "done")) #-}

mulAddTB :: Signal Dom Bool
mulAddTB = romTB simLibraries mulAddDut $(memBlobTH (Just 0) (trace mulAddDut (cycles (floatTriples 2500 5))))
{-# ANN mulAddTB (Synthesize "mulAddTB" [] (PortName "done")) #-}

compareTB :: Signal Dom Bool
compareTB = romTB simLibraries compareDut $(memBlobTH (Just 0) (trace compareDut (cycles (floatPairs 2500 6))))
{-# ANN compareTB (Synthesize "compareTB" [] (PortName "done")) #-}

predicateTB :: Signal Dom Bool
predicateTB = romTB simLibraries predicateDut $(memBlobTH (Just 0) (trace predicateDut (cycles (floatPairs 2500 7))))
{-# ANN predicateTB (Synthesize "predicateTB" [] (PortName "done")) #-}

fromS32TB :: Signal Dom Bool
fromS32TB = romTB simLibraries fromS32Dut $(memBlobTH (Just 0) (trace fromS32Dut (cycles (P.map unpack (words32 2000 8)))))
{-# ANN fromS32TB (Synthesize "fromS32TB" [] (PortName "done")) #-}

fromU32TB :: Signal Dom Bool
fromU32TB = romTB simLibraries fromU32Dut $(memBlobTH (Just 0) (trace fromU32Dut (cycles (P.map unpack (words32 2000 9)))))
{-# ANN fromU32TB (Synthesize "fromU32TB" [] (PortName "done")) #-}

toS32TB :: Signal Dom Bool
toS32TB = romTB simLibraries toS32Dut $(memBlobTH (Just 0) (trace toS32Dut (cycles (floats 3000 10))))
{-# ANN toS32TB (Synthesize "toS32TB" [] (PortName "done")) #-}

toU32TB :: Signal Dom Bool
toU32TB = romTB simLibraries toU32Dut $(memBlobTH (Just 0) (trace toU32Dut (cycles (floats 3000 11))))
{-# ANN toU32TB (Synthesize "toU32TB" [] (PortName "done")) #-}

toDoubleTB :: Signal Dom Bool
toDoubleTB = romTB simLibraries toDoubleDut $(memBlobTH (Just 0) (trace toDoubleDut (cycles (floats 3000 12))))
{-# ANN toDoubleTB (Synthesize "toDoubleTB" [] (PortName "done")) #-}

fromDoubleTB :: Signal Dom Bool
fromDoubleTB = romTB simLibraries fromDoubleDut $(memBlobTH (Just 0) (trace fromDoubleDut (cycles (doubles 3000 13))))
{-# ANN fromDoubleTB (Synthesize "fromDoubleTB" [] (PortName "done")) #-}
