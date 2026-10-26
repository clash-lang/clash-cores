{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tests of "Clash.Cores.Xilinx.Floating.Half". See "Floating.GenericTH" for the
samples.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module FloatingHalf where

import Clash.Prelude

import Numeric.Half (Half)

import qualified Clash.Cores.Xilinx.Floating.Half as H

import Floating.Annotations
import Floating.GenericTH
import Floating.RomTB

addBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.AddDefDelay Half
addBasic clk x y = withClock clk $ withEnable enableGen $ H.add x y
{-# OPAQUE addBasic #-}
{-# ANN addBasic (binaryTopAnn "addBasic") #-}

addBasicTB :: Signal XilinxSystem Bool
addBasicTB =
  basicRomTB
    (\clk a b -> pack <$> addBasic clk a b)
    0
    $(memBlobTH Nothing halfAddSamples)
{-# ANN addBasicTB (TestBench 'addBasic) #-}

-- | Addition with the low-latency architecture, at its maximum latency
addLowLatency
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem HalfAddLowLatencyDelay Half
addLowLatency clk x y =
  withClock clk $ withEnable enableGen $
    H.addWith H.defConfig{H.archOpt = H.LatencyArch} x y
{-# OPAQUE addLowLatency #-}
{-# ANN addLowLatency (binaryTopAnn "addLowLatency") #-}

addLowLatencyTB :: Signal XilinxSystem Bool
addLowLatencyTB =
  basicRomTB
    (\clk a b -> pack <$> addLowLatency clk a b)
    0
    $(memBlobTH Nothing halfAddLowLatencySamples)
{-# ANN addLowLatencyTB (TestBench 'addLowLatency) #-}

subBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.SubDefDelay Half
subBasic clk x y = withClock clk $ withEnable enableGen $ H.sub x y
{-# OPAQUE subBasic #-}
{-# ANN subBasic (binaryTopAnn "subBasic") #-}

subBasicTB :: Signal XilinxSystem Bool
subBasicTB =
  basicRomTB
    (\clk a b -> pack <$> subBasic clk a b)
    0
    $(memBlobTH Nothing halfSubSamples)
{-# ANN subBasicTB (TestBench 'subBasic) #-}

mulBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.MulDefDelay Half
mulBasic clk x y = withClock clk $ withEnable enableGen $ H.mul x y
{-# OPAQUE mulBasic #-}
{-# ANN mulBasic (binaryTopAnn "mulBasic") #-}

mulBasicTB :: Signal XilinxSystem Bool
mulBasicTB =
  basicRomTB
    (\clk a b -> pack <$> mulBasic clk a b)
    0
    $(memBlobTH Nothing halfMulSamples)
{-# ANN mulBasicTB (TestBench 'mulBasic) #-}

divBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.DivDefDelay Half
divBasic clk x y = withClock clk $ withEnable enableGen $ H.div x y
{-# OPAQUE divBasic #-}
{-# ANN divBasic (binaryTopAnn "divBasic") #-}

divBasicTB :: Signal XilinxSystem Bool
divBasicTB =
  basicRomTB
    (\clk a b -> pack <$> divBasic clk a b)
    0
    $(memBlobTH Nothing halfDivSamples)
{-# ANN divBasicTB (TestBench 'divBasic) #-}

compareBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.CompareDefDelay H.Ordering
compareBasic clk x y =
  withClock clk $ withEnable enableGen $ H.compare x y
{-# OPAQUE compareBasic #-}
{-# ANN compareBasic (binaryTopAnn "compareBasic") #-}

compareBasicTB :: Signal XilinxSystem Bool
compareBasicTB =
  basicRomTB compareBasic H.NaN $(memBlobTH Nothing halfCompareSamples)
{-# ANN compareBasicTB (TestBench 'compareBasic) #-}

sqrtBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.SqrtDefDelay Half
sqrtBasic clk x = withClock clk $ withEnable enableGen $ H.sqrt x
{-# OPAQUE sqrtBasic #-}
{-# ANN sqrtBasic (unaryTopAnn "sqrtBasic") #-}

sqrtBasicTB :: Signal XilinxSystem Bool
sqrtBasicTB =
  unaryRomTB
    (\clk a -> pack <$> sqrtBasic clk a)
    0
    $(memBlobTH Nothing halfSqrtSamples)
{-# ANN sqrtBasicTB (TestBench 'sqrtBasic) #-}

-- | Square root using block memory
sqrtBram
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem HalfSqrtBramDelay Half
sqrtBram clk x =
  withClock clk $ withEnable enableGen $
    H.sqrtWith H.defConfig{H.bMemUsage = H.FullBMemUsage} x
{-# OPAQUE sqrtBram #-}
{-# ANN sqrtBram (unaryTopAnn "sqrtBram") #-}

sqrtBramTB :: Signal XilinxSystem Bool
sqrtBramTB =
  unaryRomTB
    (\clk a -> pack <$> sqrtBram clk a)
    0
    $(memBlobTH Nothing halfSqrtBramSamples)
{-# ANN sqrtBramTB (TestBench 'sqrtBram) #-}

-- | The IP is combinational, so this has no clock
absBasic
  :: DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem 0 Half
absBasic = H.abs
{-# OPAQUE absBasic #-}
{-# ANN absBasic (unaryCombTopAnn "absBasic") #-}

absBasicTB :: Signal XilinxSystem Bool
absBasicTB =
  unaryRomTB
    (\_clk a -> pack <$> absBasic a)
    0
    $(memBlobTH Nothing halfAbsSamples)
{-# ANN absBasicTB (TestBench 'absBasic) #-}

toFloatBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.ToFloatDefDelay Float
toFloatBasic clk x = withClock clk $ withEnable enableGen $ H.toFloat x
{-# OPAQUE toFloatBasic #-}
{-# ANN toFloatBasic (unaryTopAnn "toFloatBasic") #-}

toFloatBasicTB :: Signal XilinxSystem Bool
toFloatBasicTB =
  unaryRomTB
    (\clk a -> pack <$> toFloatBasic clk a)
    0
    $(memBlobTH Nothing halfToFloatSamples)
{-# ANN toFloatBasicTB (TestBench 'toFloatBasic) #-}

toDoubleBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.ToDoubleDefDelay Double
toDoubleBasic clk x = withClock clk $ withEnable enableGen $ H.toDouble x
{-# OPAQUE toDoubleBasic #-}
{-# ANN toDoubleBasic (unaryTopAnn "toDoubleBasic") #-}

toDoubleBasicTB :: Signal XilinxSystem Bool
toDoubleBasicTB =
  unaryRomTB
    (\clk a -> pack <$> toDoubleBasic clk a)
    0
    $(memBlobTH Nothing halfToDoubleSamples)
{-# ANN toDoubleBasicTB (TestBench 'toDoubleBasic) #-}

recipBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.RecipDefDelay Half
recipBasic clk x =
  withClock clk $ withEnable enableGen $
    H.recip x
{-# OPAQUE recipBasic #-}
{-# ANN recipBasic (unaryTopAnn "recipBasic") #-}

recipBasicTB :: Signal XilinxSystem Bool
recipBasicTB =
  unaryRomTB
    (\clk a -> pack <$> recipBasic clk a)
    0
    $(memBlobTH Nothing halfRecipSamples)
{-# ANN recipBasicTB (TestBench 'recipBasic) #-}

rsqrtBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.RSqrtDefDelay Half
rsqrtBasic clk x =
  withClock clk $ withEnable enableGen $
    H.rsqrt x
{-# OPAQUE rsqrtBasic #-}
{-# ANN rsqrtBasic (unaryTopAnn "rsqrtBasic") #-}

rsqrtBasicTB :: Signal XilinxSystem Bool
rsqrtBasicTB =
  unaryRomTB
    (\clk a -> pack <$> rsqrtBasic clk a)
    0
    $(memBlobTH Nothing halfRSqrtSamples)
{-# ANN rsqrtBasicTB (TestBench 'rsqrtBasic) #-}

-- | Reciprocal square root with the minimum latency
rsqrtMinLatency
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem HalfRSqrtMinLatencyDelay Half
rsqrtMinLatency clk x =
  withClock clk $ withEnable enableGen $
    H.rsqrtWith H.defConfig x
{-# OPAQUE rsqrtMinLatency #-}
{-# ANN rsqrtMinLatency (unaryTopAnn "rsqrtMinLatency") #-}

rsqrtMinLatencyTB :: Signal XilinxSystem Bool
rsqrtMinLatencyTB =
  unaryRomTB
    (\clk a -> pack <$> rsqrtMinLatency clk a)
    0
    $(memBlobTH Nothing halfRSqrtMinLatencySamples)
{-# ANN rsqrtMinLatencyTB (TestBench 'rsqrtMinLatency) #-}

expBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.ExpDefDelay Half
expBasic clk x =
  withClock clk $ withEnable enableGen $
    H.exp x
{-# OPAQUE expBasic #-}
{-# ANN expBasic (unaryTopAnn "expBasic") #-}

expBasicTB :: Signal XilinxSystem Bool
expBasicTB =
  unaryRomTB
    (\clk a -> pack <$> expBasic clk a)
    0
    $(memBlobTH Nothing halfExpSamples)
{-# ANN expBasicTB (TestBench 'expBasic) #-}

-- | In half precision, the IP rejects 'H.FullDspUsage' (the default) for the
-- logarithm, so 'H.log' uses 'H.MediumDspUsage' instead
logBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Half
  -> DSignal XilinxSystem H.LogDefDelay Half
logBasic clk x =
  withClock clk $ withEnable enableGen $
    H.log x
{-# OPAQUE logBasic #-}
{-# ANN logBasic (unaryTopAnn "logBasic") #-}

logBasicTB :: Signal XilinxSystem Bool
logBasicTB =
  unaryRomTB
    (\clk a -> pack <$> logBasic clk a)
    0
    $(memBlobTH Nothing halfLogSamples)
{-# ANN logBasicTB (TestBench 'logBasic) #-}
