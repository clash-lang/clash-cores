{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for SPI master and slave cores
-}
module Test.Cores.SPI where

import qualified Prelude as P (length)
import qualified Data.List as L (group, nub, sort, unzip4)
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import           Clash.Cores.SPI

import           Test.Cores.Base

slaveAddressRotate
  :: forall n dom
   . (KnownDomain dom, KnownNat n, 1 <= n)
  => Clock dom
  -> Reset dom
  -> (Signal dom Bool, Signal dom Bool)
  -> Vec n (Signal dom Bool)
slaveAddressRotate clk rst =
  E.mealyB clk rst enableGen
    (\(cntQ, bQ) (ss, b) ->
        let bF = bQ && not b
            cntD | bF = if cntQ == maxBound then 0 else cntQ + 1
                 | otherwise = cntQ

            oH = (ss ||) <$> (unpack . complement $ bin2onehot cntQ)
        in  ((cntD, b), oH))
    (0 :: Index n, False)
 where
  bin2onehot = setBit 0 . fromEnum

-- Values from SPI master / slave.
-- This is a pair of distinct readings and the number of readings.
--
type Value n = ([BitVector n], Int)

-- General type of test functions for SPI.
--
type TestFn halfPeriod waitTime master slave a =
     SPIMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether the SPI slave should latch signals
  -> SNat halfPeriod
  -- ^ Half-period of the clock divider for the SPI master
  -> SNat waitTime
  -- ^ Core clock cycles between de-assertion of slave-select
  -- and the start of the SPI clock
  -> BitVector master
  -- ^ Value master sends to slave
  -> BitVector slave
  -- ^ Value slave sends to master
  -> Int
  -- ^ Sample duration
  -> a
  -- ^ Output of test function (depends on test)

testMasterSlave
  :: (1 <= halfPeriod, 1 <= waitTime)
  => TestFn halfPeriod waitTime 10 10 (Value 10, Value 10)
  -- ^ Outputs (slave0, master)
testMasterSlave mode latch halfPeriod wait mVal sVal duration =
  let spis = sampleSPI mode latch halfPeriod wait mVal sVal duration
   in (samplesToValue (ssSlaveOut spis), samplesToValue (ssMasterOut spis))
 where
  samplesToValue x =
    let x' = catMaybes x in (L.nub x', P.length x')

testMasterSlaveMultiWord
  :: (1 <= halfPeriod, 1 <= waitTime)
  => TestFn halfPeriod waitTime 16 8 ([Value 8], Value 16)
  -- ^ Outputs ([slave0 values], master)
testMasterSlaveMultiWord mode latch halfPeriod wait mVal sVal duration =
  let spis = sampleSPI mode latch halfPeriod wait mVal sVal duration
   in (samplesToValues (ssSlaveOut spis), samplesToValue (ssMasterOut spis))
 where
  samplesToValue x =
    let x' = catMaybes x in (L.nub x', P.length x')

  samplesToValues xs =
    let xs' = L.group . L.sort $ catMaybes xs
     in fmap (\x -> (L.nub x, P.length x)) xs'

testMasterMultiSlave
  :: (1 <= halfPeriod, 1 <= waitTime)
  => TestFn halfPeriod waitTime 10 10 (Value 10, Value 10, Value 10, Value 10)
  -- ^ Outputs (slave0, slave1, slave2, master)
testMasterMultiSlave spiMode latchSPI divHalf wait mVal sVal duration =
   let s = sampleN duration (bundle (slaveOut0,slaveOut1,slaveOut2,masterOut))
       (slaveOut0MS,slaveOut1MS,slaveOut2MS,masterOutMS) = L.unzip4 s
       slaveOut0S = catMaybes slaveOut0MS
       slaveOut1S = catMaybes slaveOut1MS
       slaveOut2S = catMaybes slaveOut2MS
       masterOutS = catMaybes masterOutMS
   in  ((L.nub slaveOut0S,P.length slaveOut0S)
       ,(L.nub slaveOut1S,P.length slaveOut1S)
       ,(L.nub slaveOut2S,P.length slaveOut2S)
       ,(L.nub masterOutS,P.length masterOutS))
 where
  slaveIn = pure sVal
  (misoZ0, _, slaveOut0) =
    exposeSpecificClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen spiMode latchSPI sclk mosi miso ss0 slaveIn

  (misoZ1, _, slaveOut1) =
    exposeSpecificClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen spiMode latchSPI sclk mosi miso ss1 slaveIn

  (misoZ2, _, slaveOut2) =
    exposeSpecificClockResetEnable spiSlaveLatticeSBIO
      clk rst enableGen spiMode latchSPI sclk mosi miso ss2 slaveIn

  miso = veryUnsafeToBiSignalIn
         (mergeBiSignalOuts (misoZ2 :> misoZ1 :> misoZ0 :> Nil))

  masterIn = masterInBP clk rst mVal bp

  (ss2 :> ss1 :> ss0 :> Nil) = slaveAddressRotate @3 clk rst (ss,bp)

  (sclk, mosi, ss, bp, _, masterOut) =
    exposeSpecificClockResetEnable spiMaster
      clk rst enableGen spiMode divHalf wait masterIn (readFromBiSignal miso)

  clk = systemClockGen
  rst = systemResetGen

tests :: TestTree
tests =
  testGroup "SPI" $
    [ oneSlaveLatch
    , oneSlaveNoLatch
    , threeSlaveLatch
    , threeSlaveNoLatch
    , oneSlaveManyWordsLatch
    , oneSlaveManyWordsNoLatch
    , oneSlaveDelayLatch
    , oneSlaveDelayNoLatch
    , threeSlavesDelayLatch
    , threeSlavesDelayNoLatch
    , oneSlaveManyWordsDelayLatch
    , oneSlaveManyWordsDelayNoLatch
    ] <*> [SPIMode0, SPIMode1, SPIMode2, SPIMode3]
 where
  oneSlaveLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterSlave spi True d4 d1 0b0110011101 0b0110010101 (3 * 84)
        @?= (([0b0110011101], 3), ([0b0110010101], 3))

  oneSlaveNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterSlave spi False d1 d1 0b0110011101 0b0110010101 (3 * 25)
        @?= (([0b0110011101], 3), ([0b0110010101], 3))

  threeSlaveLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterMultiSlave spi True d4 d1 0b0110011101 0b0110010101 (3 * 84)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  threeSlaveNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterMultiSlave spi False d1 d1 0b0110011101 0b0110010101 (3 * 25)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  oneSlaveManyWordsLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterSlaveMultiWord spi True d4 d1 0b0101010100001111 0b10010101 (5 * 84)
        @?= ([([0b00001111], 3), ([0b01010101], 3)], ([0b01001010110010101], 3))

  oneSlaveManyWordsNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterSlaveMultiWord spi False d1 d1 0b0101010100001111 0b0110010101 (5 * 25)
        @?= ([([0b00001111],3),([0b01010101],3)],([0b01001010110010101],3))

  oneSlaveDelayLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterSlave spi True d4 d3 0b0110011101 0b0110010101 (3 * 87)
        @?= (([0b0110011101],3),([0b0110010101],3))

  oneSlaveDelayNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterSlave spi False d1 d3 0b0110011101 0b0110010101 (3 * 27)
        @?= (([0b0110011101],3),([0b0110010101],3))

  threeSlavesDelayLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterMultiSlave spi True d4 d3 0b0110011101 0b0110010101 (3 * 87)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  threeSlavesDelayNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterMultiSlave spi False d1 d3 0b0110011101 0b0110010101 (3 * 27)
        @?= (([0b0110011101],1),([0b0110011101],1),([0b0110011101],1),([0b0110010101],3))

  oneSlaveManyWordsDelayLatch spi =
    testCase (show spi <> ", Divider 8, Slave Latch") $
      testMasterSlaveMultiWord spi True d4 d3 0b0101010100001111 0b10010101 (5 * 87)
        @?= ([([0b00001111],3),([0b01010101],3)],([0b01001010110010101],3))

  oneSlaveManyWordsDelayNoLatch spi =
    testCase (show spi <> ", Divider 2, No Slave Latch") $
      testMasterSlaveMultiWord spi False d1 d3 0b0101010100001111 0b10010101 (5 * 27)
        @?= ([([0b00001111],3),([0b01010101],3)],([0b01001010110010101],3))

