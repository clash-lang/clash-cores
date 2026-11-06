-- | Circuits and stimuli for "SbIo"
module SbIoTypes where

import Clash.Explicit.Prelude
import Clash.Signal (withClock, withEnable)

import Clash.Cores.LatticeSemi.Ice40.IO
import Ice40TestCommon

import Language.Haskell.TH (ExpQ)

import qualified Prelude as P

-- | Number of cycles of each test bench
type Cycles = 400

-- | Inputs of 'sbio' and the test bench's driver of the pad
data SbioIn = SbioIn
  { padDrive :: Maybe Bit
  -- ^ Value the test bench drives onto the pad
  , latchInput :: Bit
  , dOut0 :: Bit
  , dOut1 :: Bit
  , outputEnable :: Bool
  , clockEnable :: Bool
  }
  deriving (Show, Generic, NFDataX, BitPack)

-- | 'sbio', with its pad connected to a driver in the test bench. Outputs the
-- level of the pad, @D_IN_0@ and @D_IN_1@.
sbioCircuit :: BitVector 6 -> Clock Dom -> Signal Dom SbioIn -> Signal Dom (Bit, Bit, Bit)
sbioCircuit pinType clk i = bundle (readFromBiSignal pad, dIn0, dIn1)
 where
  pad = veryUnsafeToBiSignalIn (mergeBiSignalOuts (sbioOut :> tbOut :> Nil))
  tbOut = writeToBiSignal pad (padDrive <$> i)
  (sbioOut, dIn0, dIn1) =
    withClock clk $
      withEnable (toEnable (clockEnable <$> i)) $
        sbio
          pinType
          pad
          (latchInput <$> i)
          (dOut0 <$> i)
          (dOut1 <$> i)
          (outputEnable <$> i)
{-# INLINE sbioCircuit #-}

-- | Whether the pin type drives the pad
drivesPad :: BitVector 6 -> Bool
drivesPad pinType = slice d5 d4 pinType /= 0

-- | Whether the pin type has a latch
hasLatch :: BitVector 6 -> Bool
hasLatch pinType = testBit pinType 1

-- | Random stimuli for a pin type. The test bench only drives the pad if the
-- cell doesn't.
genSbioIn :: BitVector 6 -> G SbioIn
genSbioIn pinType = do
  drive <- genBit
  release <- chance 10
  latch <- chance (if hasLatch pinType then 20 else 0)
  d0 <- genBit
  d1 <- genBit
  oe <- genBool
  ce <- chance 80
  pure
    SbioIn
      { padDrive = if drivesPad pinType || release then Nothing else Just drive
      , latchInput = boolToBit latch
      , dOut0 = d0
      , dOut1 = d1
      , outputEnable = oe
      , clockEnable = ce
      }

sbioStimuli :: BitVector 6 -> Int -> [SbioIn]
sbioStimuli pinType = evalG (sequence (P.replicate (natToNum @Cycles) (genSbioIn pinType)))

-- | Inputs of 'sbioDdr' in the slow domain
data SbioDdrIn = SbioDdrIn
  { ddrLatchInput :: Bit
  , ddrDOut0 :: Bit
  , ddrDOut1 :: Bit
  , ddrOutputEnable :: Bool
  , ddrClockEnable :: Bool
  }
  deriving (Show, Generic, NFDataX, BitPack)

-- | 'sbioDdr', with its pad connected to a driver in the test bench, in the
-- fast domain. Outputs @D_IN_0@ and @D_IN_1@, and the level of the pad.
sbioDdrCircuit :: BitVector 6 -> DdrCircuit SbioDdrIn (Maybe Bit) (Bit, Bit) Bit
sbioDdrCircuit pinType clk _clkFast i padDriveFast = (bundle (dIn0, dIn1), readFromBiSignal pad)
 where
  pad = veryUnsafeToBiSignalIn (mergeBiSignalOuts (sbioOut :> tbOut :> Nil))
  tbOut = writeToBiSignal pad padDriveFast
  (sbioOut, dIn0, dIn1) =
    sbioDdr
      clk
      (toEnable (ddrClockEnable <$> i))
      pinType
      pad
      (ddrLatchInput <$> i)
      (ddrDOut0 <$> i)
      (ddrDOut1 <$> i)
      (ddrOutputEnable <$> i)
{-# INLINE sbioDdrCircuit #-}

genSbioDdrIn :: G SbioDdrIn
genSbioDdrIn = do
  d0 <- genBit
  d1 <- genBit
  oe <- genBool
  ce <- chance 80
  pure
    SbioDdrIn
      { ddrLatchInput = 0
      , ddrDOut0 = d0
      , ddrDOut1 = d1
      , ddrOutputEnable = oe
      , ddrClockEnable = ce
      }

-- | Value the test bench drives onto the pad, at twice the clock rate
genPadDrive :: BitVector 6 -> G (Maybe Bit)
genPadDrive pinType = do
  drive <- genBit
  release <- chance 10
  pure (if drivesPad pinType || release then Nothing else Just drive)

-- | Stimuli for both domains
sbioDdrStimuli :: BitVector 6 -> Int -> ([SbioDdrIn], [Maybe Bit])
sbioDdrStimuli pinType =
  evalG $
    (,)
      <$> sequence (P.replicate (natToNum @Cycles) genSbioDdrIn)
      <*> sequence (P.replicate (2 * natToNum @Cycles) (genPadDrive pinType))

-- | Stimuli, expected values and masks for 'tb'
sbioBlobs :: BitVector 6 -> Int -> ExpQ
sbioBlobs pinType seed = blobs (sbioCircuit pinType) (sbioStimuli pinType seed)

-- | Stimuli, expected values and masks for 'tbDdr'
sbioDdrBlobs :: BitVector 6 -> Int -> ExpQ
sbioDdrBlobs pinType seed = blobsDdr (sbioDdrCircuit pinType) iSlow iFast
 where
  (iSlow, iFast) = sbioDdrStimuli pinType seed
