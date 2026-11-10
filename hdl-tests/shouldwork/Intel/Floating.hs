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

sqrtTB :: Signal Dom Bool
sqrtTB = romTB simLibraries sqrtDut $(memBlobTH (Just 0) (trace sqrtDut (cycles (floats 1000 14 <> floatsIn 1 254 3000 15))))
{-# ANN sqrtTB (Synthesize "sqrtTB" [] (PortName "done")) #-}

cbrtTB :: Signal Dom Bool
cbrtTB = romTB simLibraries cbrtDut $(memBlobTH (Just 0) (trace cbrtDut (cycles (floats 1000 60 <> signedFloatsIn 1 254 3000 61))))
{-# ANN cbrtTB (Synthesize "cbrtTB" [] (PortName "done")) #-}

hypotTB :: Signal Dom Bool
hypotTB = romTB simLibraries hypotDut $(memBlobTH (Just 0) (trace hypotDut (cycles (floatTriples 1000 62 <> closeTriples 20 200 2500 63))))
{-# ANN hypotTB (Synthesize "hypotTB" [] (PortName "done")) #-}

sinPiTB :: Signal Dom Bool
sinPiTB = romTB simLibraries sinPiDut $(memBlobTH (Just 0) (trace sinPiDut (cycles (floats 1000 70 <> signedFloatsIn 100 60 3000 71))))
{-# ANN sinPiTB (Synthesize "sinPiTB" [] (PortName "done")) #-}

cosPiTB :: Signal Dom Bool
cosPiTB = romTB simLibraries cosPiDut $(memBlobTH (Just 0) (trace cosPiDut (cycles (floats 1000 72 <> signedFloatsIn 100 60 3000 73))))
{-# ANN cosPiTB (Synthesize "cosPiTB" [] (PortName "done")) #-}

recipTB :: Signal Dom Bool
recipTB = romTB simLibraries recipDut $(memBlobTH (Just 0) (trace recipDut (cycles (floats 3000 16 <> floatsIn 251 4 500 17))))
{-# ANN recipTB (Synthesize "recipTB" [] (PortName "done")) #-}

rsqrtTB :: Signal Dom Bool
rsqrtTB = romTB simLibraries rsqrtDut $(memBlobTH (Just 0) (trace rsqrtDut (cycles (floats 1000 18 <> floatsIn 1 254 3000 19))))
{-# ANN rsqrtTB (Synthesize "rsqrtTB" [] (PortName "done")) #-}

expTB :: Signal Dom Bool
expTB = romTB simLibraries expDut $(memBlobTH (Just 0) (trace expDut (cycles (floats 500 20 <> expInputs 3500 21))))
{-# ANN expTB (Synthesize "expTB" [] (PortName "done")) #-}

logTB :: Signal Dom Bool
logTB = romTB simLibraries logDut $(memBlobTH (Just 0) (trace logDut (cycles (floats 1000 22 <> floatsIn 1 254 1500 23 <> floatsIn 126 2 1500 24))))
{-# ANN logTB (Synthesize "logTB" [] (PortName "done")) #-}

log2TB :: Signal Dom Bool
log2TB = romTB simLibraries log2Dut $(memBlobTH (Just 0) (trace log2Dut (cycles (floats 1000 40 <> floatsIn 1 254 1500 41 <> floatsIn 126 2 1500 42))))
{-# ANN log2TB (Synthesize "log2TB" [] (PortName "done")) #-}

log10TB :: Signal Dom Bool
log10TB = romTB simLibraries log10Dut $(memBlobTH (Just 0) (trace log10Dut (cycles (floats 1000 43 <> floatsIn 1 254 1500 44 <> floatsIn 126 2 1500 45))))
{-# ANN log10TB (Synthesize "log10TB" [] (PortName "done")) #-}

log1pTB :: Signal Dom Bool
log1pTB = romTB simLibraries log1pDut $(memBlobTH (Just 0) (trace log1pDut (cycles (floats 1000 50 <> log1pInputs 3000 51))))
{-# ANN log1pTB (Synthesize "log1pTB" [] (PortName "done")) #-}

exp2TB :: Signal Dom Bool
exp2TB = romTB simLibraries exp2Dut $(memBlobTH (Just 0) (trace exp2Dut (cycles (floats 500 50 <> signedFloatsIn 100 35 3500 51))))
{-# ANN exp2TB (Synthesize "exp2TB" [] (PortName "done")) #-}

exp10TB :: Signal Dom Bool
exp10TB = romTB simLibraries exp10Dut $(memBlobTH (Just 0) (trace exp10Dut (cycles (floats 500 52 <> signedFloatsIn 100 34 3500 53))))
{-# ANN exp10TB (Synthesize "exp10TB" [] (PortName "done")) #-}
