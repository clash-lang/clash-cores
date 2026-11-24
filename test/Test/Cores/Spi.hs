module Test.Cores.Spi where

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           Clash.Prelude hiding (interleave)
import           Clash.Cores.Spi

import           Test.Cores.Internal.SampleSpi
import           Test.Cores.Internal.Signals

data Device
  = Master | Slave
  deriving (Eq, Show)

instance Arbitrary Device where
  arbitrary = QC.elements [Master, Slave]

-- | Test that sending a simple input repeats that simple input
-- without changing it. The lawfulness of a device is irrelevant here.
--
simpleRepeats
  :: (1 <= halfPeriod, 1 <= waitTime)
  => SimpleTest halfPeriod waitTime 8 8 (Device -> Bool)
simpleRepeats divHalf wait mVal sVal mode latch duration dev =
  case dev of
    Master -> checkWith ssMasterOut sVal
    Slave  -> checkWith ssSlaveOut  mVal
 where
  sampleSpi = sampleSimple masterLawfulSignal slaveLawfulSignal

  checkWith outs val =
    let spis = sampleSpi divHalf wait mVal sVal mode latch duration
     in all (== val) . catMaybes $ outs spis

-- | Test that sending muliple words repeats those words in a cycle
-- without changing the message. This only works for lawful devices.
--
cyclingRepeats
  :: (1 <= halfPeriod, 1 <= waitTime)
  => CyclingTest halfPeriod waitTime 8 8 (Device -> Bool)
cyclingRepeats divHalf wait mVals sVals mode latch duration dev =
  case dev of
    Master -> checkWith ssMasterOut sVals
    Slave  -> checkWith ssSlaveOut  mVals
 where
  sampleSpi = sampleCycling masterLawfulSignal slaveLawfulSignal

  checkWith outs vals =
    let spis = sampleSpi divHalf wait mVals sVals mode latch duration
     in NE.isPrefixOf (catMaybes $ outs spis) (NE.cycle vals)

-- | Test that acknowledgement signals and done signals are interleaved.
-- As acknowledgement signals a transfer has started and done signals a
-- transfer has finished, there should never be two of either in a row.
-- The lawfulness of a device is irrelevant here.
--
ackDoneInterleaved
  :: (1 <= halfPeriod, 1 <= waitTime)
  => SimpleTest halfPeriod waitTime 8 8 (Device -> Bool)
ackDoneInterleaved divHalf wait mVal sVal mode latch duration dev =
  case dev of
    Master -> checkWith ssMasterOut ssMasterAck
    Slave  -> checkWith ssSlaveOut  ssSlaveAck
 where
  sampleSpi = sampleSimple masterLawfulSignal slaveLawfulSignal

  interleave []     ys = ys
  interleave (x:xs) ys = x : interleave ys xs

  checkWith outs acks =
    let spis   = sampleSpi divHalf wait mVal sVal mode latch duration
        outIxs = List.findIndices isJust $ outs spis
        ackIxs = List.elemIndices True $ acks spis
     in interleave ackIxs outIxs == List.sort (ackIxs <> outIxs)

-- | Simulate an SPI slave driven by a master that keeps slave select asserted
-- for all words of a single transfer. Unlike 'spiMaster', which de-asserts
-- slave select after every word, this exercises loading new words while slave
-- select stays low.
--
-- Returns the words the master read from MISO and the words the slave
-- received from MOSI. Words containing undefined bits are 'Nothing'.
--
sampleHeldSelect
  :: SpiMode
  -> Bool
  -- ^ Whether the SPI slave should latch signals
  -> [BitVector 8]
  -- ^ Values master sends to slave
  -> NonEmpty (BitVector 8)
  -- ^ Values slave sends to master
  -> ([Maybe (BitVector 8)], [Maybe (BitVector 8)])
sampleHeldSelect mode latch mWords sWords = (masterGot, slaveGot)
 where
  -- Half an SCK period in clock cycles
  halfPeriod = 4
  idleSck = mode == SpiMode2 || mode == SpiMode3
  -- CPHA = 0: sample on the leading edge, CPHA = 1: on the trailing edge
  cpha0 = mode == SpiMode0 || mode == SpiMode2

  -- (SS, SCK, MOSI) per clock cycle. Each MOSI bit is held for a full SCK
  -- period, which is arranged such that its sample edge falls in the middle.
  stimulus =
       List.replicate 8 (True, idleSck, 0)
    <> List.replicate 4 (False, idleSck, 0)
    <> List.concatMap oneBit (List.concatMap msbFirst mWords)
    <> List.replicate 4 (False, idleSck, 0)
    <> List.replicate 8 (True, idleSck, 0)

  oneBit b
    | cpha0     = half idleSck <> half (not idleSck)
    | otherwise = half (not idleSck) <> half idleSck
   where
    half sck = List.replicate halfPeriod (False, sck, b)

  msbFirst :: BitVector 8 -> [Bit]
  msbFirst w = [w ! i | i <- [7, 6 .. 0 :: Int]]

  nCycles = List.length stimulus
  stimSs   = [ss | (ss, _, _) <- stimulus]
  stimSck  = [sck | (_, sck, _) <- stimulus]
  stimMosi = [m | (_, _, m) <- stimulus]

  clk = systemClockGen
  rst = systemResetGen

  ssS   = fromList (stimSs <> List.repeat True)
  sckS  = fromList (stimSck <> List.repeat idleSck)
  mosiS = fromList (stimMosi <> List.repeat 0)

  config :: SpiSlaveConfig 'Floating System
  config = SpiSlaveConfig mode latch buffer
  buffer bin oe out = writeToBiSignal bin (mux oe (Just <$> out) (pure Nothing))

  (misoOut, ack, dout) =
    withClockResetEnable clk rst enableGen
      (spiSlave config sckS mosiS misoIn ssS din)
  misoIn = veryUnsafeToBiSignalIn misoOut
  din = slaveLawfulSignal clk rst sWords ack

  misoSamples = sampleN nCycles (readFromBiSignal misoIn :: Signal System Bit)
  doutSamples = sampleN nCycles dout

  -- MISO as the master sees it on each of its sample edges
  masterBits =
    [ m
    | (sckPrev, sckNow, ssNow, m) <-
        List.zip4 stimSck (List.drop 1 stimSck) (List.drop 1 stimSs)
          (List.drop 1 misoSamples)
    , not ssNow
    , sckPrev /= sckNow
    , if cpha0 then sckPrev == idleSck else sckNow == idleSck
    ]

  masterGot = fmap toWord (chunksOf8 masterBits)
  slaveGot  = fmap defined (catMaybes doutSamples)

  toWord bits
    | List.any hasUndefined bits = Nothing
    | otherwise = Just (List.foldl (\acc b -> shiftL acc 1 .|. zeroExtend (pack b)) 0 bits)

  defined w = if hasUndefined w then Nothing else Just w

  chunksOf8 [] = []
  chunksOf8 xs = List.take 8 xs : chunksOf8 (List.drop 8 xs)

-- | Test that a slave sends and receives every word correctly when the master
-- keeps slave select asserted for multiple words.
--
heldSelectTransfers
  :: SpiMode
  -> Bool
  -> QC.NonEmptyList (BitVector 8, BitVector 8)
  -> QC.Property
heldSelectTransfers mode latch (QC.NonEmpty pairs0) =
  sampleHeldSelect mode latch mWords (NE.fromList sWords)
    === (fmap Just sWords, fmap Just mWords)
 where
  pairs = List.take 5 pairs0
  (mWords, sWords) = List.unzip pairs

tests :: TestTree
tests = testGroup "SPI"
  [ testGroup "Repeating messages"
    [ QC.testProperty "Single word messages repeat indefinitely"
        $ simpleRepeats d4 d1 mSimple sSimple

    , QC.testProperty "Multiple word messages repeat indefinitely"
        $ cyclingRepeats d4 d1 mCycling sCycling
    ]

  , testGroup "Acknowledgement signals"
    [ QC.testProperty "Acknowledgement interleaves with Done"
        $ ackDoneInterleaved d4 d1 mSimple sSimple
    ]

  , testGroup "Slave select held across words"
    [ QC.testProperty "Slave sends and receives every word"
        heldSelectTransfers
    ]
  ]
 where
  mSimple  = 0b10101010
  mCycling = 0b10101010 :| [0b11110000, 0b10010110]

  sSimple  = 0b00010011
  sCycling = 0b11111111 :| [0b00100010]
