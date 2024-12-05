{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Test.Cores.Ethernet.Mac.FrameCheckSequence (
  tests,
) where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Cores.Ethernet.Mac.FrameCheckSequence

import Clash.Prelude

import qualified Data.List as L

import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Protocols.Hedgehog

import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

$(deriveHardwareCrc Crc32_ethernet d8 d1)
$(deriveHardwareCrc Crc32_ethernet d8 d2)
$(deriveHardwareCrc Crc32_ethernet d8 d3)
$(deriveHardwareCrc Crc32_ethernet d8 d4)
$(deriveHardwareCrc Crc32_ethernet d8 d7)
$(deriveHardwareCrc Crc32_ethernet d8 d8)

packetToCrcInp ::
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  [PacketStreamM2S dataWidth ()] ->
  [BitVector 8]
packetToCrcInp packet = head . _data <$> (chopPacket =<< packet)

insertCrc ::
  forall (dataWidth :: Nat).
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  [PacketStreamM2S dataWidth ()] ->
  [PacketStreamM2S dataWidth ()]
insertCrc = upConvert . go . downConvert
 where
  go :: [PacketStreamM2S 1 ()] -> [PacketStreamM2S 1 ()]
  go pkt = pkt''
   where
    crcInp = head . _data <$> pkt
    softwareCrc = mkSoftwareCrc Crc32_ethernet d8
    crc = digest $ L.foldl' feed softwareCrc crcInp
    crc' = singleton . v2bv <$> (toList . reverse . unconcat d8 . bv2v $ crc)
    lastfmnt = L.last pkt
    pkt' =
      L.init pkt
        L.++ [lastfmnt{_last = Nothing}]
        L.++ fmap (\dat -> lastfmnt{_data = dat, _last = Nothing}) crc'
    pkt'' = L.init pkt' L.++ [(L.last pkt'){_last = Just 0}]


validateCrc ::
  forall (dataWidth :: Nat).
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  [PacketStreamM2S dataWidth ()] ->
  [PacketStreamM2S dataWidth ()]
validateCrc packet = L.init packet L.++ [lastPacketSetAbort]
 where
  lastFragment = L.last packet
  softwareCrc = mkSoftwareCrc Crc32_ethernet d8
  crcBytes = digest $ L.foldl' feed softwareCrc $ packetToCrcInp packet
  valid = complement crcBytes == residue Crc32_ethernet

  lastPacketSetAbort =
    lastFragment
      { _abort = not valid || _abort lastFragment
      }

-- | Test the FCS inserter
fcsinserterTest ::
  forall dataWidth.
  (1 <= dataWidth) =>
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
  SNat dataWidth ->
  Property
fcsinserterTest SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets (Range.linear 1 4) Abort (genValidPacket (pure ()) (Range.linear 0 20)))
    (exposeClockResetEnable modelInsert)
    (exposeClockResetEnable (fcsInserterC @dataWidth))
 where
  modelInsert packets = L.concatMap insertCrc (chunkByPacket packets)

-- | Test the FCS validator
fcsvalidatorTest ::
  forall dataWidth.
  (1 <= dataWidth) =>
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
  SNat dataWidth ->
  Property
fcsvalidatorTest SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets (Range.linear 1 4) Abort genPkt)
    (exposeClockResetEnable modelValidate)
    (exposeClockResetEnable (fcsValidatorC @dataWidth))
 where
  genPkt am =
    Gen.choice
      [ -- Random packet
        genValidPacket (pure ()) (Range.linear 0 20) am
      , -- Packet with valid CRC
        insertCrc <$> genValidPacket (pure ()) (Range.linear 0 20) am
      ]

  modelValidate packets = validateCrc =<< chunkByPacket packets

prop_fcsinserter_d1 :: Property
prop_fcsinserter_d1 = fcsinserterTest d1

prop_fcsinserter_d2 :: Property
prop_fcsinserter_d2 = fcsinserterTest d2

prop_fcsinserter_d4 :: Property
prop_fcsinserter_d4 = fcsinserterTest d4

prop_fcsinserter_d8 :: Property
prop_fcsinserter_d8 = fcsinserterTest d8

prop_fcsvalidator_d1 :: Property
prop_fcsvalidator_d1 = fcsvalidatorTest d1

prop_fcsvalidator_d3 :: Property
prop_fcsvalidator_d3 = fcsvalidatorTest d3

prop_fcsvalidator_d4 :: Property
prop_fcsvalidator_d4 = fcsvalidatorTest d4

prop_fcsvalidator_d7 :: Property
prop_fcsvalidator_d7 = fcsvalidatorTest d7

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
