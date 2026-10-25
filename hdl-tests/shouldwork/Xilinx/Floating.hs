{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.,
                  2022     , Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module Floating where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import qualified Clash.Signal.Delayed as D
import Clash.Explicit.Testbench

import qualified Prelude as P
import Numeric (showHex)

import qualified Clash.Cores.Xilinx.Floating as F

import Floating.Annotations
import Floating.RomTB
import Floating.TH

newtype FloatVerifier = FloatVerifier Float
  deriving (Generic)
  deriving anyclass BitPack

instance Eq FloatVerifier where
  (FloatVerifier x) == (FloatVerifier y) = pack x == pack y

instance ShowX FloatVerifier where
  showsPrecX = showsPrecXWith showsPrec

instance Show FloatVerifier where
  showsPrec = floatVerifierShowsPrec

floatVerifierShowsPrec
  :: Int
  -> FloatVerifier
  -> ShowS
floatVerifierShowsPrec _ (FloatVerifier x)
  | isNaN x = nanSign . nanString . showHex payload . (')':)
  | otherwise = shows x
 where
  nanSign | msb (pack x) == 0 = ('+':)
          | otherwise         = ('-':)
  nanString
    | testBit (pack x) 22 = ("qNaN(0x" P.++)
    | otherwise           = ("sNaN(0x" P.++)
  payload = truncateB $ pack x :: BitVector 22

addBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem F.AddDefDelay Float
addBasic clk x y = withClock clk $ withEnable enableGen $ F.add x y
{-# OPAQUE addBasic #-}
{-# ANN addBasic (binaryTopAnn "addBasic") #-}

addBasicTB :: Signal XilinxSystem Bool
addBasicTB =
  basicRomTB
    (\clk a b -> FloatVerifier <$> addBasic clk a b)
    (FloatVerifier 0.0)
    $(memBlobTH Nothing addBasicSamples)
{-# ANN addBasicTB (TestBench 'addBasic) #-}

addEnable
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 11 Float
addEnable clk en x y = withClock clk $ withEnable en $ F.add x y
{-# OPAQUE addEnable #-}
{-# ANN addEnable (binaryEnTopAnn "addEnable") #-}

addEnableTB :: Signal XilinxSystem Bool
addEnableTB = done
 where
  testInput =
    fromSignal $ stimuliGenerator clk rst $(listToVecTH [1 :: Float .. 25])
  en =
    toEnable $ stimuliGenerator clk rst
        (   (replicate d11 True ++ replicate d4 True ++ replicate d4 False)
         :< True)
  expectedOutput =
       replicate d11 0
    ++ $(listToVecTH . P.map (\i -> i + i) $
                [1 :: Float .. 4]
           -- Stall for four cycles
           P.++ P.replicate 4 5
           -- Still in the pipeline (11 deep) from before the stall.
           P.++ P.take 11 [5 .. 25]
           -- We "lose" four samples of what remains due to not being enabled
           -- for those inputs.
           P.++ P.drop 4 (P.drop 11 [5 .. 25])
        )
  expectOutput =
    outputVerifier' clk rst expectedOutput
  done =
      expectOutput . ignoreFor clk rst enableGen d11 0
    . toSignal $ addEnable clk en testInput testInput
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# ANN addEnableTB (TestBench 'addEnable) #-}

addShortPL
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem AddShortPLDelay Float
addShortPL clk x y =
  withClock clk $ withEnable enableGen $ F.addWith F.defConfig x y
{-# OPAQUE addShortPL #-}
{-# ANN addShortPL (binaryTopAnn "addShortPL") #-}

addShortPLTB :: Signal XilinxSystem Bool
addShortPLTB =
  basicRomTB
    (\clk a b -> FloatVerifier <$> addShortPL clk a b)
    (FloatVerifier 0.0)
    $(memBlobTH Nothing addShortPLSamples)
{-# ANN addShortPLTB (TestBench 'addShortPL) #-}

subBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem F.SubDefDelay Float
subBasic clk x y = withClock clk $ withEnable enableGen $ F.sub x y
{-# OPAQUE subBasic #-}
{-# ANN subBasic (binaryTopAnn "subBasic") #-}

subBasicTB :: Signal XilinxSystem Bool
subBasicTB =
  basicRomTB
    (\clk a b -> FloatVerifier <$> subBasic clk a b)
    (FloatVerifier 0.0)
    $(memBlobTH Nothing subBasicSamples)
{-# ANN subBasicTB (TestBench 'subBasic) #-}

mulBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem F.MulDefDelay Float
mulBasic clk x y = withClock clk $ withEnable enableGen $ F.mul x y
{-# OPAQUE mulBasic #-}
{-# ANN mulBasic (binaryTopAnn "mulBasic") #-}

mulBasicTB :: Signal XilinxSystem Bool
mulBasicTB =
  basicRomTB
    (\clk a b -> FloatVerifier <$> mulBasic clk a b)
    (FloatVerifier 0.0)
    $(memBlobTH Nothing mulBasicSamples)
{-# ANN mulBasicTB (TestBench 'mulBasic) #-}

divBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem F.DivDefDelay Float
divBasic clk x y = withClock clk $ withEnable enableGen $ F.div x y
{-# OPAQUE divBasic #-}
{-# ANN divBasic (binaryTopAnn "divBasic") #-}

divBasicTB :: Signal XilinxSystem Bool
divBasicTB =
  basicRomTB
    (\clk a b -> FloatVerifier <$> divBasic clk a b)
    (FloatVerifier 0.0)
    $(memBlobTH Nothing divBasicSamples)
{-# ANN divBasicTB (TestBench 'divBasic) #-}

compareBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem F.CompareDefDelay F.Ordering
compareBasic clk x y =
  withClock clk $ withEnable enableGen $ F.compare x y
{-# OPAQUE compareBasic #-}
{-# ANN compareBasic (binaryTopAnn "compareBasic") #-}

compareBasicTB :: Signal XilinxSystem Bool
compareBasicTB =
  basicRomTB compareBasic F.NaN $(memBlobTH Nothing compareBasicSamples)
{-# ANN compareBasicTB (TestBench 'compareBasic) #-}

compareEnable
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem 0 Float
  -> DSignal XilinxSystem F.CompareDefDelay F.Ordering
compareEnable clk en x y = withClock clk $ withEnable en $ F.compare x y
{-# OPAQUE compareEnable #-}
{-# ANN compareEnable (binaryEnTopAnn "compareEnable") #-}

compareEnableTB :: Signal XilinxSystem Bool
compareEnableTB = done
 where
  done = outputVerifier' clk rst $(listToVecTH compareFloatsEnableExpected) actual1

  actual1 = ignoreFor clk rst enableGen d6 F.EQ (D.toSignal actual0)
  actual0 = compareEnable clk ena (D.fromSignal testInputA) (D.fromSignal testInputB)

  clk = tbClockGen (not <$> done)
  rst = resetGen
  ena = toEnable $ CEP.stimuliGenerator clk rst $(listToVecTH compareFloatsEnableInput)

  testInputA = CEP.stimuliGenerator clk rst $(listToVecTH compareFloatsEnableInputA)
  testInputB = CEP.stimuliGenerator clk rst $(listToVecTH compareFloatsEnableInputB)
{-# ANN compareEnableTB (TestBench 'compareEnable) #-}

fromUBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 (Unsigned 32)
  -> DSignal XilinxSystem F.FromU32DefDelay Float
fromUBasic clk x = withClock clk $ withEnable enableGen $ F.fromU32 x
{-# OPAQUE fromUBasic #-}
{-# ANN fromUBasic (unaryTopAnn "fromUBasic") #-}

fromUBasicTB :: Signal XilinxSystem Bool
fromUBasicTB = unaryRomTB fromUBasic 0 $(memBlobTH Nothing fromUBasicSamples)
{-# ANN fromUBasicTB (TestBench 'fromUBasic) #-}

fromUEnable
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 (Unsigned 32)
  -> DSignal XilinxSystem 5 Float
fromUEnable clk en x = withClock clk $ withEnable en $ F.fromU32 x
{-# OPAQUE fromUEnable #-}
{-# ANN fromUEnable (unaryEnTopAnn "fromUEnable") #-}

fromUEnableTB :: Signal XilinxSystem Bool
fromUEnableTB = done
 where
  testInput = fromSignal $
    stimuliGenerator clk rst $(listToVecTH [1 :: Unsigned 32 .. 20])
  en = toEnable $ stimuliGenerator clk rst
        ((replicate d5 True ++ replicate d4 True ++ replicate d4 False) :< True)
  expectedOutput = replicate d5 0 ++
    $(listToVecTH $
         [1 :: Float .. 4]
      -- Stall for four cycles
      <> P.replicate 4 5
      -- Still in the pipeline (5 deep) from before the stall.
      <> P.take 5 [5 .. 20]
      -- We "lose" four samples of what remains due to not being enabled
      -- for those inputs.
      <> P.drop 4 (P.drop 5 [5 .. 20])
    )
  expectOutput = outputVerifier' clk rst expectedOutput
  done = expectOutput . ignoreFor clk rst enableGen d5 0 . toSignal $
    fromUEnable clk en testInput
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# ANN fromUEnableTB (TestBench 'fromUEnable) #-}

fromSBasic
  :: Clock XilinxSystem
  -> DSignal XilinxSystem 0 (Signed 32)
  -> DSignal XilinxSystem F.FromS32DefDelay Float
fromSBasic clk x = withClock clk $ withEnable enableGen $ F.fromS32 x
{-# OPAQUE fromSBasic #-}
{-# ANN fromSBasic (unaryTopAnn "fromSBasic") #-}

fromSBasicTB :: Signal XilinxSystem Bool
fromSBasicTB = unaryRomTB fromSBasic 0 $(memBlobTH Nothing fromSBasicSamples)
{-# ANN fromSBasicTB (TestBench 'fromSBasic) #-}

fromSEnable
  :: Clock XilinxSystem
  -> Enable XilinxSystem
  -> DSignal XilinxSystem 0 (Signed 32)
  -> DSignal XilinxSystem 6 Float
fromSEnable clk en x = withClock clk $ withEnable en $ F.fromS32 x
{-# OPAQUE fromSEnable #-}
{-# ANN fromSEnable (unaryEnTopAnn "fromSEnable") #-}

fromSEnableTB :: Signal XilinxSystem Bool
fromSEnableTB = done
 where
  testInput = fromSignal $
    stimuliGenerator clk rst $(listToVecTH [1 :: Signed 32 .. 21])
  en = toEnable $ stimuliGenerator clk rst
        ((replicate d6 True ++ replicate d4 True ++ replicate d4 False) :< True)
  expectedOutput = replicate d6 0 ++
    $(listToVecTH $
         [1 :: Float .. 4]
      -- Stall for four cycles
      <> P.replicate 4 5
      -- Still in the pipeline (6 deep) from before the stall.
      <> P.take 6 [5 .. 21]
      -- We "lose" four samples of what remains due to not being enabled
      -- for those inputs.
      <> P.drop 4 (P.drop 6 [5 .. 21])
    )
  expectOutput = outputVerifier' clk rst expectedOutput
  done = expectOutput . ignoreFor clk rst enableGen d6 0 . toSignal $
    fromSEnable clk en testInput
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# ANN fromSEnableTB (TestBench 'fromSEnable) #-}
