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

import Numeric.Half (Half)

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

sqrtBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.SqrtDefDelay Double
sqrtBasic clk x = withClock clk $ withEnable enableGen $ D.sqrt x
{-# OPAQUE sqrtBasic #-}
{-# ANN sqrtBasic (unaryTopAnn "sqrtBasic") #-}

sqrtBasicTB :: Signal XilinxSystem Bool
sqrtBasicTB =
  unaryRomTB
    (\clk a -> pack <$> sqrtBasic clk a)
    0
    $(memBlobTH Nothing doubleSqrtSamples)
{-# ANN sqrtBasicTB (TestBench 'sqrtBasic) #-}

-- | The IP is combinational, so this has no clock
absBasic
  :: DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
absBasic = D.abs
{-# OPAQUE absBasic #-}
{-# ANN absBasic (unaryCombTopAnn "absBasic") #-}

absBasicTB :: Signal XilinxSystem Bool
absBasicTB =
  unaryRomTB
    (\_clk a -> pack <$> absBasic a)
    0
    $(memBlobTH Nothing doubleAbsSamples)
{-# ANN absBasicTB (TestBench 'absBasic) #-}

toHalfBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.ToHalfDefDelay Half
toHalfBasic clk x = withClock clk $ withEnable enableGen $ D.toHalf x
{-# OPAQUE toHalfBasic #-}
{-# ANN toHalfBasic (unaryTopAnn "toHalfBasic") #-}

toHalfBasicTB :: Signal XilinxSystem Bool
toHalfBasicTB =
  unaryRomTB
    (\clk a -> pack <$> toHalfBasic clk a)
    0
    $(memBlobTH Nothing doubleToHalfSamples)
{-# ANN toHalfBasicTB (TestBench 'toHalfBasic) #-}

toFloatBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.ToFloatDefDelay Float
toFloatBasic clk x = withClock clk $ withEnable enableGen $ D.toFloat x
{-# OPAQUE toFloatBasic #-}
{-# ANN toFloatBasic (unaryTopAnn "toFloatBasic") #-}

toFloatBasicTB :: Signal XilinxSystem Bool
toFloatBasicTB =
  unaryRomTB
    (\clk a -> pack <$> toFloatBasic clk a)
    0
    $(memBlobTH Nothing doubleToFloatSamples)
{-# ANN toFloatBasicTB (TestBench 'toFloatBasic) #-}

fromU64Basic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 (Unsigned 64)
  -> DSignal XilinxSystem D.FromU64DefDelay Double
fromU64Basic clk x = withClock clk $ withEnable enableGen $ D.fromU64 x
{-# OPAQUE fromU64Basic #-}
{-# ANN fromU64Basic (unaryTopAnn "fromU64Basic") #-}

fromU64BasicTB :: Signal XilinxSystem Bool
fromU64BasicTB =
  unaryRomTB
    (\clk a -> pack <$> fromU64Basic clk a)
    0
    $(memBlobTH Nothing doubleFromU64Samples)
{-# ANN fromU64BasicTB (TestBench 'fromU64Basic) #-}

fromS64Basic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 (Signed 64)
  -> DSignal XilinxSystem D.FromS64DefDelay Double
fromS64Basic clk x = withClock clk $ withEnable enableGen $ D.fromS64 x
{-# OPAQUE fromS64Basic #-}
{-# ANN fromS64Basic (unaryTopAnn "fromS64Basic") #-}

fromS64BasicTB :: Signal XilinxSystem Bool
fromS64BasicTB =
  unaryRomTB
    (\clk a -> pack <$> fromS64Basic clk a)
    0
    $(memBlobTH Nothing doubleFromS64Samples)
{-# ANN fromS64BasicTB (TestBench 'fromS64Basic) #-}

toS32Basic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.ToS32DefDelay (Signed 32)
toS32Basic clk x = withClock clk $ withEnable enableGen $ D.toS32 x
{-# OPAQUE toS32Basic #-}
{-# ANN toS32Basic (unaryTopAnn "toS32Basic") #-}

toS32BasicTB :: Signal XilinxSystem Bool
toS32BasicTB =
  unaryRomTB
    toS32Basic
    0
    $(memBlobTH Nothing doubleToS32Samples)
{-# ANN toS32BasicTB (TestBench 'toS32Basic) #-}

recipBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.RecipDefDelay Double
recipBasic clk x =
  withClock clk $ withEnable enableGen $
    D.recip x
{-# OPAQUE recipBasic #-}
{-# ANN recipBasic (unaryTopAnn "recipBasic") #-}

recipBasicTB :: Signal XilinxSystem Bool
recipBasicTB =
  unaryRomTB
    (\clk a -> pack <$> recipBasic clk a)
    0
    $(memBlobTH Nothing doubleRecipSamples)
{-# ANN recipBasicTB (TestBench 'recipBasic) #-}

rsqrtBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.RSqrtDefDelay Double
rsqrtBasic clk x =
  withClock clk $ withEnable enableGen $
    D.rsqrt x
{-# OPAQUE rsqrtBasic #-}
{-# ANN rsqrtBasic (unaryTopAnn "rsqrtBasic") #-}

rsqrtBasicTB :: Signal XilinxSystem Bool
rsqrtBasicTB =
  unaryRomTB
    (\clk a -> pack <$> rsqrtBasic clk a)
    0
    $(memBlobTH Nothing doubleRSqrtSamples)
{-# ANN rsqrtBasicTB (TestBench 'rsqrtBasic) #-}

expBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.ExpDefDelay Double
expBasic clk x =
  withClock clk $ withEnable enableGen $
    D.exp x
{-# OPAQUE expBasic #-}
{-# ANN expBasic (unaryTopAnn "expBasic") #-}

expBasicTB :: Signal XilinxSystem Bool
expBasicTB =
  unaryRomTB
    (\clk a -> pack <$> expBasic clk a)
    0
    $(memBlobTH Nothing doubleExpSamples)
{-# ANN expBasicTB (TestBench 'expBasic) #-}

-- | Exponential with block memory
expBram
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem DoubleExpBramDelay Double
expBram clk x =
  withClock clk $ withEnable enableGen $
    D.expWith D.defConfig{D.bMemUsage = D.FullBMemUsage} x
{-# OPAQUE expBram #-}
{-# ANN expBram (unaryTopAnn "expBram") #-}

expBramTB :: Signal XilinxSystem Bool
expBramTB =
  unaryRomTB
    (\clk a -> pack <$> expBram clk a)
    0
    $(memBlobTH Nothing doubleExpBramSamples)
{-# ANN expBramTB (TestBench 'expBram) #-}

logBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.LogDefDelay Double
logBasic clk x =
  withClock clk $ withEnable enableGen $
    D.log x
{-# OPAQUE logBasic #-}
{-# ANN logBasic (unaryTopAnn "logBasic") #-}

logBasicTB :: Signal XilinxSystem Bool
logBasicTB =
  unaryRomTB
    (\clk a -> pack <$> logBasic clk a)
    0
    $(memBlobTH Nothing doubleLogSamples)
{-# ANN logBasicTB (TestBench 'logBasic) #-}

-- | Logarithm without DSP slices
logNoDsp
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem DoubleLogNoDspDelay Double
logNoDsp clk x =
  withClock clk $ withEnable enableGen $
    D.logWith D.defConfig{D.dspUsage = D.NoDspUsage} x
{-# OPAQUE logNoDsp #-}
{-# ANN logNoDsp (unaryTopAnn "logNoDsp") #-}

logNoDspTB :: Signal XilinxSystem Bool
logNoDspTB =
  unaryRomTB
    (\clk a -> pack <$> logNoDsp clk a)
    0
    $(memBlobTH Nothing doubleLogNoDspSamples)
{-# ANN logNoDspTB (TestBench 'logNoDsp) #-}

fmaBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.FmaDefDelay Double
fmaBasic clk x y z = withClock clk $ withEnable enableGen $ D.fma x y z
{-# OPAQUE fmaBasic #-}
{-# ANN fmaBasic (ternaryTopAnn "fmaBasic") #-}

fmaBasicTB :: Signal XilinxSystem Bool
fmaBasicTB =
  ternaryRomTB
    (\clk a b c -> pack <$> fmaBasic clk a b c)
    0
    $(memBlobTH Nothing doubleFmaSamples)
{-# ANN fmaBasicTB (TestBench 'fmaBasic) #-}

-- | Fused multiply-add with medium DSP usage, at its maximum latency
fmaMedium
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem DoubleFmaMediumDelay Double
fmaMedium clk x y z =
  withClock clk $ withEnable enableGen $
    D.fmaWith D.defConfig{D.dspUsage = D.MediumDspUsage} x y z
{-# OPAQUE fmaMedium #-}
{-# ANN fmaMedium (ternaryTopAnn "fmaMedium") #-}

fmaMediumTB :: Signal XilinxSystem Bool
fmaMediumTB =
  ternaryRomTB
    (\clk a b c -> pack <$> fmaMedium clk a b c)
    0
    $(memBlobTH Nothing doubleFmaMediumSamples)
{-# ANN fmaMediumTB (TestBench 'fmaMedium) #-}

-- | Accumulator with the default settings, without flags
accumBasic
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 Bool
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem D.AccumDefDelay Double
accumBasic clk en lst x = withClock clk $ withEnable en $ D.accum lst x
{-# OPAQUE accumBasic #-}
{-# ANN accumBasic (accumTopAnn "accumBasic") #-}

accumBasicTB :: Signal XilinxSystem Bool
accumBasicTB =
  accumRomTB
    (\clk en l a -> pack <$> accumBasic clk en l a)
    $(memBlobTH Nothing doubleAccumBasicRom)
{-# ANN accumBasicTB (TestBench 'accumBasic) #-}

-- | Accumulator with narrow bounds and flags, below its maximum latency
accumFlags
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 Bool
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem AccumCustomDelay (Double, D.AccumFlags)
accumFlags clk en lst x =
  withClock clk $ withEnable en $
    D.accumWithFlags (accumNarrowConfig D.MediumDspUsage) lst x
{-# OPAQUE accumFlags #-}
{-# ANN accumFlags (accumTopAnn "accumFlags") #-}

accumFlagsTB :: Signal XilinxSystem Bool
accumFlagsTB =
  accumRomTB
    (\clk en l a -> pack <$> accumFlags clk en l a)
    $(memBlobTH Nothing doubleAccumFlagsRom)
{-# ANN accumFlagsTB (TestBench 'accumFlags) #-}

-- | Accumulator with sums below the smallest normal number
accumTiny
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 Bool
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem AccumCustomDelay (Double, D.AccumFlags)
accumTiny clk en lst x =
  withClock clk $ withEnable en $
    D.accumWithFlags (accumTinyConfig @Double) lst x
{-# OPAQUE accumTiny #-}
{-# ANN accumTiny (accumTopAnn "accumTiny") #-}

accumTinyTB :: Signal XilinxSystem Bool
accumTinyTB =
  accumRomTB
    (\clk en l a -> pack <$> accumTiny clk en l a)
    $(memBlobTH Nothing doubleAccumTinyRom)
{-# ANN accumTinyTB (TestBench 'accumTiny) #-}

-- | Accumulator with sums beyond the largest finite number
accumHuge
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 Bool
  -> DSignal XilinxSystem 0 Double
  -> DSignal XilinxSystem AccumCustomDelay (Double, D.AccumFlags)
accumHuge clk en lst x =
  withClock clk $ withEnable en $
    D.accumWithFlags (accumHugeConfig @Double) lst x
{-# OPAQUE accumHuge #-}
{-# ANN accumHuge (accumTopAnn "accumHuge") #-}

accumHugeTB :: Signal XilinxSystem Bool
accumHugeTB =
  accumRomTB
    (\clk en l a -> pack <$> accumHuge clk en l a)
    $(memBlobTH Nothing doubleAccumHugeRom)
{-# ANN accumHugeTB (TestBench 'accumHuge) #-}
