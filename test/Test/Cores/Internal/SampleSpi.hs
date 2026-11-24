{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cores.Internal.SampleSpi
  ( SpiSamples(..)
  , Duration(..)
  , CyclingTest
  , SimpleTest
  , sampleCycling
  , sampleCyclingHeldSelect
  , sampleCyclingBurstSelect
  , sampleSimple
  ) where

import qualified Data.List as List (unzip4)
import           Data.List.NonEmpty (NonEmpty)
import           Test.QuickCheck (Arbitrary(..), suchThat)

import qualified Clash.Explicit.Prelude as E
import           Clash.Prelude
import           Clash.Cores.Spi

import           Test.Cores.Internal.Signals

-- Generally speaking, test units will be able to determine passing or failing
-- by looking at the values of the master and slave, and the acknowledgement
-- signals sent over time.
--
data SpiSamples master slave = SpiSamples
  { ssMasterOut :: [Maybe (BitVector master)]
  , ssMasterAck :: [Bool]
  , ssSlaveOut  :: [Maybe (BitVector slave)]
  , ssSlaveAck  :: [Bool]
  } deriving (Eq, Show)

newtype Duration = Duration { getDuration :: Int }
  deriving stock   (Eq, Ord, Show)
  deriving newtype (Num)

instance Arbitrary Duration where
  arbitrary = Duration <$> arbitrary `suchThat` (>= 100)

-- | A cycling test is one where the data sent by SPI masters and slaves
-- changes over time, cycling through a list of values to send for each.
--
type CyclingTest halfPeriod waitTime master slave a =
     SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ Core clock cycles between de-assertion of slave-select
  -- and the start of the SPI clock
  -> NonEmpty (BitVector master)
  -- ^ Values master sends to slave
  -> NonEmpty (BitVector slave)
  -- ^ Values slave sends to master
  -> SpiMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch signals
  -> Duration
  -- ^ Sample duration
  -> a
  -- ^ Output of test function

sampleCycling
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     )
  => GenMaster master
  -> GenSlave slave
  -> CyclingTest halfPeriod waitTime master slave (SpiSamples master slave)
sampleCycling = sampleCyclingWith (\_ _ ss -> ss)

-- | Same as 'sampleCycling', but slave select of the SPI slave stays asserted
-- after the first word. The SPI master de-asserts slave select after each
-- word. Thus, this test uses a different slave select signal for the slave.
-- This signal goes low with slave select of the master, and then stays low.
--
sampleCyclingHeldSelect
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     )
  => GenMaster master
  -> GenSlave slave
  -> CyclingTest halfPeriod waitTime master slave (SpiSamples master slave)
sampleCyclingHeldSelect = sampleCyclingWith holdSelect
 where
  holdSelect clk rst ss = ssHeld
   where
    ssHeld = ss .&&. beforeFirst
    beforeFirst = E.register clk rst enableGen True ssHeld

-- | Same as 'sampleCycling', but slave select of the SPI slave stays asserted
-- for a burst of words. After each burst, slave select of the slave goes high
-- with slave select of the master. Then, the next burst starts.
--
sampleCyclingBurstSelect
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     , KnownNat burst, 1 <= burst
     )
  => SNat burst
  -- ^ Number of words in a burst
  -> GenMaster master
  -> GenSlave slave
  -> CyclingTest halfPeriod waitTime master slave (SpiSamples master slave)
sampleCyclingBurstSelect burst = sampleCyclingWith burstSelect
 where
  burstSelect clk rst ss =
    E.mealy clk rst enableGen go (True, 0 `asIndexOf` burst) ss

  -- Count the words of the master. A word ends when slave select of the
  -- master goes high.
  go (ssOld, cnt) ssNow = ((ssNow, cntNext), ssNow && cntNext == 0)
   where
    cntNext
      | ssNow && not ssOld = if cnt == maxBound then 0 else cnt + 1
      | otherwise          = cnt

  asIndexOf :: Index n -> SNat n -> Index n
  asIndexOf i SNat = i

sampleCyclingWith
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     )
  => (  Clock System
     -> Reset System
     -> Signal System Bool
     -> Signal System Bool )
  -- ^ Function to make slave select of the SPI slave from slave select of
  -- the SPI master
  -> GenMaster master
  -> GenSlave slave
  -> CyclingTest halfPeriod waitTime master slave (SpiSamples master slave)
sampleCyclingWith slaveSelect genM genS divHalf wait mVals sVals mode latch
                  duration =
  (\(mO, mA, sO, sA) -> SpiSamples mO mA sO sA) $ List.unzip4 samples
 where
  samples = sampleN (getDuration duration) $ bundle (mOut, mAck, sOut, sAck)
  slaveIn = genS clk rst sVals sAck

  (misoZ, sAck, sOut) =
    withClockResetEnable clk rst enableGen
      (spiSlaveLatticeSBIO mode latch sclk mosi miso ssSlave slaveIn)

  ssSlave = slaveSelect clk rst ss

  miso = veryUnsafeToBiSignalIn misoZ
  masterIn = genM clk rst mVals mAck bp

  (sclk, mosi, ss, bp, mAck, mOut) =
    withClockResetEnable clk rst enableGen
      (spiMaster mode divHalf wait masterIn (readFromBiSignal miso))

  clk = systemClockGen
  rst = systemResetGen

type SimpleTest halfPeriod waitTime master slave a =
     SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ Core clock cycles between de-assertion of slave-select
  -- and the start of the SPI clock
  -> BitVector master
  -- ^ Value master sends to slave
  -> BitVector slave
  -- ^ Value slave sends to master
  -> SpiMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch signals
  -> Duration
  -- ^ Sample duration
  -> a
  -- ^ Output of test function (depends on test)

sampleSimple
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     )
  => GenMaster master
  -> GenSlave slave
  -> SimpleTest halfPeriod waitTime master slave (SpiSamples master slave)
sampleSimple genM genS divHalf wait mVal sVal =
  sampleCycling genM genS divHalf wait (pure mVal) (pure sVal)
