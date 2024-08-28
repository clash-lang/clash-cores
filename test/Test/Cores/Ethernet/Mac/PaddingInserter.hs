{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}

module Test.Cores.Ethernet.Mac.PaddingInserter where

import Prelude

import Clash.Prelude ( type (<=) )
import qualified Clash.Prelude as C

import Hedgehog
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

import Protocols
import Protocols.PacketStream
import Protocols.Hedgehog
import Protocols.PacketStream.Hedgehog

import Clash.Cores.Ethernet.Mac.PaddingInserter


model
  :: forall (dataWidth :: C.Nat)
   . C.KnownNat dataWidth
  => 1 <= dataWidth
  => Int
  -> [PacketStreamM2S dataWidth ()]
  -> [PacketStreamM2S dataWidth ()]
model padBytes fragments = concatMap (upConvert . setLasts . insertPadding) $ chunkByPacket $ downConvert fragments
  where
    insertPadding pkts = pkts ++ replicate (paddingNeeded pkts) padding
    paddingNeeded pkts = max 0 (padBytes - length pkts)
    padding = PacketStreamM2S {_data = C.repeat 0, _last = Nothing, _meta = (), _abort = False}
    setLasts pkts = map (\pkt -> pkt{_last = Nothing}) (init pkts) ++ [(last pkts){_last = Just 0}]

-- | Test the padding inserter.
paddingInserterTest
  :: forall dataWidth padBytes
   . C.KnownNat padBytes
  => 1 <= dataWidth
  => 1 <= padBytes
  => C.SNat dataWidth
  -> C.SNat padBytes
  -> Property
paddingInserterTest C.SNat padBytes =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (genValidPackets (Range.linear 1 10) (Range.linear 0 10) Abort)
    (C.exposeClockResetEnable (model $ C.natToNum @padBytes))
    (C.exposeClockResetEnable @C.System ckt)
    (===)
  where
    ckt :: forall (dom :: C.Domain)
      . C.HiddenClockResetEnable dom
      => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
    ckt = paddingInserterC padBytes

-- We test the edge case dataWidth = padBytes = 1,
-- a case where dataWidth divides padBytes,
-- a case where dataWidth does not divide padBytes, and
-- a case where dataWidth is more than padBytes.
prop_paddinginserter_d1, prop_paddinginserter_d2, prop_paddinginserter_d5, prop_paddinginserter_d50 :: Property
prop_paddinginserter_d1  = paddingInserterTest C.d1  C.d1
prop_paddinginserter_d2  = paddingInserterTest C.d2  C.d46
prop_paddinginserter_d5  = paddingInserterTest C.d5  C.d46
prop_paddinginserter_d50 = paddingInserterTest C.d50 C.d46

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
