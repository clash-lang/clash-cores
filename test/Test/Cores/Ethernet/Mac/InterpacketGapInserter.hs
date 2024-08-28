{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.Mac.InterpacketGapInserter where

import qualified Data.List as L
import Prelude

import Clash.Prelude hiding ( repeat )
import qualified Clash.Prelude as C

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

import Protocols
import Protocols.PacketStream
import Protocols.Hedgehog

import Clash.Cores.Ethernet.Mac.InterpacketGapInserter


genVec :: (KnownNat n, 1 <= n) => Gen a -> Gen (Vec n a)
genVec gen = sequence (C.repeat gen)

-- | If we do not consider the timing information of the component,
--   all it should do is forward its inputs. Hence, this id test.
prop_interpacket_gap_inserter_id :: Property
prop_interpacket_gap_inserter_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets)
    (exposeClockResetEnable id)
    (exposeClockResetEnable @System (interpacketGapInserterC d12))
    (===)
    where
      -- This is used to generate
      genPackets =
        PacketStreamM2S <$>
        genVec Gen.enumBounded <*>
        Gen.maybe Gen.enumBounded <*>
        Gen.enumBounded <*>
        Gen.enumBounded

fwdIn :: [Maybe (PacketStreamM2S 1 ())]
fwdIn = [
  Just (PacketStreamM2S (0xAB :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0xCD :> Nil) (Just 0) () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)] L.++ L.repeat Nothing

bwdIn :: [PacketStreamS2M]
bwdIn = fmap PacketStreamS2M (L.repeat True)

expectedFwdOut :: [Maybe (PacketStreamM2S 1 ())]
expectedFwdOut = [
    Just (PacketStreamM2S (0xAB :> Nil) Nothing () False)
  , Just (PacketStreamM2S (0xCD :> Nil) (Just 0) () False)]
  L.++ L.replicate 12 Nothing
  L.++ [Just (PacketStreamM2S (0x01 :> Nil) Nothing () False)]

expectedBwdOut :: [PacketStreamS2M]
expectedBwdOut = fmap PacketStreamS2M ([True, True] L.++ L.replicate 12 False L.++ [True])

clk :: Clock System
clk = systemClockGen

rst :: Reset System
rst = systemResetGen

en :: Enable System
en = enableGen

fwdOut :: Signal System (Maybe (PacketStreamM2S 1 ()))
bwdOut :: Signal System PacketStreamS2M
(bwdOut, fwdOut) = toSignals ckt (fromList fwdIn, fromList bwdIn)
  where ckt = exposeClockResetEnable (interpacketGapInserterC d12) clk rst en

prop_12_cycles_no_data_after_last :: Property
prop_12_cycles_no_data_after_last = property $
  do L.map fst (sampleN 15 $ bundle (fwdOut, bwdOut)) === expectedFwdOut

prop_12_cycles_backpressure_after_last :: Property
prop_12_cycles_backpressure_after_last = property $
  do L.map snd (sampleN 15 $ bundle (fwdOut, bwdOut)) === expectedBwdOut

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
