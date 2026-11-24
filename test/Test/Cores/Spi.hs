module Test.Cores.Spi where

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

import           Clash.Prelude hiding (interleave)

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
cyclingRepeats =
  cyclingRepeatsWith 0 (sampleCycling masterLawfulSignal slaveLawfulSignal)

-- | Same as 'cyclingRepeats', with a different sample function. The test
-- fails when the sample duration contains fewer words than the given minimum.
--
cyclingRepeatsWith
  :: KnownNat n
  => Int
  -- ^ Minimum number of words in the sample duration
  -> CyclingTest halfPeriod waitTime n n (SpiSamples n n)
  -> CyclingTest halfPeriod waitTime n n (Device -> Bool)
cyclingRepeatsWith minWords sampleSpi divHalf wait mVals sVals mode latch
                   duration dev =
  case dev of
    Master -> checkWith ssMasterOut sVals
    Slave  -> checkWith ssSlaveOut  mVals
 where
  checkWith outs vals =
    let spis  = sampleSpi divHalf wait mVals sVals mode latch duration
        outs' = catMaybes $ outs spis
     in List.length outs' >= minWords && NE.isPrefixOf outs' (NE.cycle vals)

-- | Test that acknowledgement signals and done signals are interleaved.
-- As acknowledgement signals a transfer has started and done signals a
-- transfer has finished, there should never be two of either in a row.
-- The lawfulness of a device is irrelevant here.
--
ackDoneInterleaved
  :: (1 <= halfPeriod, 1 <= waitTime, KnownNat n, 1 <= n)
  => SimpleTest halfPeriod waitTime n n (Device -> Bool)
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

-- | Test that sending multiple words repeats those words in a cycle when
-- slave select of the slave stays asserted between words. Most SPI masters
-- operate in this manner. This only works for lawful devices.
--
-- The slave loads a new word only from the second word. Thus, the test fails
-- when the sample duration contains fewer than two words.
--
heldSelectRepeats
  :: (1 <= halfPeriod, 1 <= waitTime)
  => CyclingTest halfPeriod waitTime 8 8 (Device -> Bool)
heldSelectRepeats =
  cyclingRepeatsWith 2
    (sampleCyclingHeldSelect masterLawfulSignal slaveLawfulSignal)

-- | Test that sending multiple words repeats those words in a cycle when
-- slave select of the slave stays asserted for bursts of three words. This
-- only works for lawful devices.
--
-- The test doubles the sample duration, and fails when the sample duration
-- contains fewer than four words. Thus, the test always checks a word after
-- the first burst.
--
burstSelectRepeats
  :: (1 <= halfPeriod, 1 <= waitTime)
  => CyclingTest halfPeriod waitTime 8 8 (Device -> Bool)
burstSelectRepeats divHalf wait mVals sVals mode latch duration =
  cyclingRepeatsWith 4
    (sampleCyclingBurstSelect d3 masterLawfulSignal slaveLawfulSignal)
    divHalf wait mVals sVals mode latch (2 * duration)

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

  , testGroup "Slave select held between words"
    -- With a half period of 2, the minimum sample duration contains two words.
    [ QC.testProperty "Multiple word messages repeat indefinitely"
        $ heldSelectRepeats d2 d1 mCycling sCycling

    , QC.testProperty "Multiple word messages repeat in bursts of three words"
        $ burstSelectRepeats d2 d1 mCycling sCycling
    ]

  , testGroup "Words of 1 bit"
    [ QC.testProperty "Multiple word messages repeat indefinitely"
        $ cyclingRepeatsWith 2
            (sampleCycling masterLawfulSignal slaveLawfulSignal)
            d4 d1 mCycling1 sCycling1

    , QC.testProperty "Slave select held between words"
        $ cyclingRepeatsWith 2
            (sampleCyclingHeldSelect masterLawfulSignal slaveLawfulSignal)
            d4 d1 mCycling1 sCycling1

    , QC.testProperty "Slave select held for bursts of three words"
        $ cyclingRepeatsWith 4
            (sampleCyclingBurstSelect d3 masterLawfulSignal slaveLawfulSignal)
            d4 d1 mCycling1 sCycling1

    , QC.testProperty "Acknowledgement interleaves with Done"
        $ ackDoneInterleaved d4 d1 mSimple1 sSimple1
    ]
  ]
 where
  mSimple  = 0b10101010
  mCycling = 0b10101010 :| [0b11110000, 0b10010110]

  sSimple  = 0b00010011
  sCycling = 0b11111111 :| [0b00100010]

  -- With words of 1 bit, a word that the slave sends two times changes the
  -- sequence. Thus, the sequences contain the two values in an irregular
  -- order.
  mSimple1, sSimple1 :: BitVector 1
  mSimple1 = 0b1
  sSimple1 = 0b0

  mCycling1, sCycling1 :: NonEmpty (BitVector 1)
  mCycling1 = 0b1 :| [0b0, 0b0]
  sCycling1 = 0b0 :| [0b1, 0b1]

