{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tests of "Clash.Cores.Xilinx.Floating.Double". See "Floating.GenericTH" for the
samples.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module FloatingDouble where

import Clash.Prelude

import qualified Clash.Cores.Xilinx.Floating.Double as D

import Floating.Annotations
import Floating.GenericTH
import Floating.RomTB

addBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.AddDefDelay Double
addBasic clk x y = withClock clk $ withEnable enableGen $ D.add x y
{-# OPAQUE addBasic #-}
{-# ANN addBasic (binaryTopAnn "addBasic") #-}

addBasicTB :: Signal XilinxSystem Bool
addBasicTB =
  basicRomTB
    (\clk a b -> pack <$> addBasic clk a b)
    0
    $(memBlobTH Nothing doubleAddSamples)
{-# ANN addBasicTB (TestBench 'addBasic) #-}

subBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.SubDefDelay Double
subBasic clk x y = withClock clk $ withEnable enableGen $ D.sub x y
{-# OPAQUE subBasic #-}
{-# ANN subBasic (binaryTopAnn "subBasic") #-}

subBasicTB :: Signal XilinxSystem Bool
subBasicTB =
  basicRomTB
    (\clk a b -> pack <$> subBasic clk a b)
    0
    $(memBlobTH Nothing doubleSubSamples)
{-# ANN subBasicTB (TestBench 'subBasic) #-}

mulBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.MulDefDelay Double
mulBasic clk x y = withClock clk $ withEnable enableGen $ D.mul x y
{-# OPAQUE mulBasic #-}
{-# ANN mulBasic (binaryTopAnn "mulBasic") #-}

mulBasicTB :: Signal XilinxSystem Bool
mulBasicTB =
  basicRomTB
    (\clk a b -> pack <$> mulBasic clk a b)
    0
    $(memBlobTH Nothing doubleMulSamples)
{-# ANN mulBasicTB (TestBench 'mulBasic) #-}

-- | Multiplication with 'D.MaxDspUsage'
mulMaxDsp
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem DoubleMulMaxDspDelay Double
mulMaxDsp clk x y =
  withClock clk $ withEnable enableGen $
    D.mulWith D.defConfig{D.dspUsage = D.MaxDspUsage} x y
{-# OPAQUE mulMaxDsp #-}
{-# ANN mulMaxDsp (binaryTopAnn "mulMaxDsp") #-}

mulMaxDspTB :: Signal XilinxSystem Bool
mulMaxDspTB =
  basicRomTB
    (\clk a b -> pack <$> mulMaxDsp clk a b)
    0
    $(memBlobTH Nothing doubleMulMaxDspSamples)
{-# ANN mulMaxDspTB (TestBench 'mulMaxDsp) #-}

divBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.DivDefDelay Double
divBasic clk x y = withClock clk $ withEnable enableGen $ D.div x y
{-# OPAQUE divBasic #-}
{-# ANN divBasic (binaryTopAnn "divBasic") #-}

divBasicTB :: Signal XilinxSystem Bool
divBasicTB =
  basicRomTB
    (\clk a b -> pack <$> divBasic clk a b)
    0
    $(memBlobTH Nothing doubleDivSamples)
{-# ANN divBasicTB (TestBench 'divBasic) #-}

compareBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.CompareDefDelay D.Ordering
compareBasic clk x y =
  withClock clk $ withEnable enableGen $ D.compare x y
{-# OPAQUE compareBasic #-}
{-# ANN compareBasic (binaryTopAnn "compareBasic") #-}

compareBasicTB :: Signal XilinxSystem Bool
compareBasicTB =
  basicRomTB compareBasic D.NaN $(memBlobTH Nothing doubleCompareSamples)
{-# ANN compareBasicTB (TestBench 'compareBasic) #-}

fromUBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 (Unsigned 32)
  -> DSignal XilinxSystem D.FromU32DefDelay Double
fromUBasic clk x = withClock clk $ withEnable enableGen $ D.fromU32 x
{-# OPAQUE fromUBasic #-}
{-# ANN fromUBasic (unaryTopAnn "fromUBasic") #-}

fromUBasicTB :: Signal XilinxSystem Bool
fromUBasicTB =
  unaryRomTB
    (\clk a -> pack <$> fromUBasic clk a)
    0
    $(memBlobTH Nothing doubleFromU32Samples)
{-# ANN fromUBasicTB (TestBench 'fromUBasic) #-}

fromSBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 (Signed 32)
  -> DSignal XilinxSystem D.FromS32DefDelay Double
fromSBasic clk x = withClock clk $ withEnable enableGen $ D.fromS32 x
{-# OPAQUE fromSBasic #-}
{-# ANN fromSBasic (unaryTopAnn "fromSBasic") #-}

fromSBasicTB :: Signal XilinxSystem Bool
fromSBasicTB =
  unaryRomTB
    (\clk a -> pack <$> fromSBasic clk a)
    0
    $(memBlobTH Nothing doubleFromS32Samples)
{-# ANN fromSBasicTB (TestBench 'fromSBasic) #-}
