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
  , sampleCyclingTransaction
  , sampleEchoTransaction
  , sampleSimple
  ) where

import qualified Data.List as List (unzip5)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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
  , ssSlaveSelect :: [Bool]
  -- ^ Slave select at the SPI slave (active low)
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
sampleCycling genM genS =
  sampleCyclingWith (\_ _ ss -> ss) (pure False) genM (fromGenSlave genS)

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
sampleCyclingHeldSelect genM genS =
  sampleCyclingWith holdSelect (pure False) genM (fromGenSlave genS)
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
sampleCyclingBurstSelect burst genM genS =
  sampleCyclingWith burstSelect (pure False) genM (fromGenSlave genS)
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

-- | Same as 'sampleCycling', but the SPI master keeps slave select asserted
-- for all words. Thus, all words are in one transaction.
--
sampleCyclingTransaction
  :: ( KnownNat master, KnownNat slave
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= master, 1 <= slave
     )
  => GenMaster master
  -> GenSlave slave
  -> CyclingTest halfPeriod waitTime master slave (SpiSamples master slave)
sampleCyclingTransaction genM genS =
  sampleCyclingWith (\_ _ ss -> ss) (pure True) genM (fromGenSlave genS)

-- | Same as 'sampleCyclingTransaction', but the SPI slave sends each
-- received word back in the next word. The first word of the slave is the
-- first value of the slave values. This sampler shows a read that depends on
-- the previous write in the same transaction.
--
sampleEchoTransaction
  :: ( KnownNat n
     , 1 <= halfPeriod, 1 <= waitTime, 1 <= n
     )
  => GenMaster n
  -> CyclingTest halfPeriod waitTime n n (SpiSamples n n)
sampleEchoTransaction genM =
  sampleCyclingWith (\_ _ ss -> ss) (pure True) genM echo
 where
  echo clk rst sVals _ received =
    E.regMaybe clk rst enableGen (NE.head sVals) received

-- | Function to make the data input of the SPI slave. The arguments are the
-- slave values, the acknowledgement of the slave and the received words of
-- the slave.
--
type SlaveData slave =
     Clock System
  -> Reset System
  -> NonEmpty (BitVector slave)
  -> Signal System Bool
  -> Signal System (Maybe (BitVector slave))
  -> Signal System (BitVector slave)

fromGenSlave :: KnownNat slave => GenSlave slave -> SlaveData slave
fromGenSlave genS clk rst sVals ack _ = genS clk rst sVals ack

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
  -> Signal System Bool
  -- ^ Hold input of the SPI master
  -> GenMaster master
  -> SlaveData slave
  -> CyclingTest halfPeriod waitTime master slave (SpiSamples master slave)
sampleCyclingWith slaveSelect hold genM slaveData divHalf wait mVals sVals
                  mode latch duration =
  (\(mO, mA, sO, sA, sS) -> SpiSamples mO mA sO sA sS) $ List.unzip5 samples
 where
  samples =
    sampleN (getDuration duration) $ bundle (mOut, mAck, sOut, sAck, ssSlave)
  slaveIn = slaveData clk rst sVals sAck sOut

  (misoZ, sAck, sOut) =
    withClockResetEnable clk rst enableGen
      (spiSlaveLatticeSBIO mode latch sclk mosi miso ssSlave slaveIn)

  ssSlave = slaveSelect clk rst ss

  miso = veryUnsafeToBiSignalIn misoZ
  masterIn = genM clk rst mVals mAck bp

  (sclk, mosi, ss, bp, mAck, mOut) =
    withClockResetEnable clk rst enableGen
      (spiMasterTransaction mode divHalf wait hold masterIn
        (readFromBiSignal miso))

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
