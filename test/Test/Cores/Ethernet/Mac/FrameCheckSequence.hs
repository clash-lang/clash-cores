{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
$(deriveHardwareCrc Crc32_ethernet d8 d3)
$(deriveHardwareCrc Crc32_ethernet d8 d4)
$(deriveHardwareCrc Crc32_ethernet d8 d7)

packetToCrcInp ::
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  [PacketStreamM2S dataWidth ()] ->
  [BitVector 8]
packetToCrcInp packet = go (chopPacket =<< packet)
 where
  go = L.concatMap (\p -> [head (_data p) | _last p /= Just 0])

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
    lastfmnt = L.last pkt
    trailingZero = _last lastfmnt == Just 0

    crcInp' = head . _data <$> pkt
    crcInp = if trailingZero then L.init crcInp' else crcInp'
    softwareCrc = mkSoftwareCrc Crc32_ethernet d8
    crc = digest $ L.foldl' feed softwareCrc crcInp
    crc' = singleton . v2bv <$> (toList . reverse . unconcat d8 . bv2v $ crc)

    pkt' =
      L.init pkt
        L.++ ([lastfmnt{_last = Nothing} | not trailingZero])
        L.++ fmap (\dat -> lastfmnt{_data = dat, _last = Nothing}) crc'
    pkt'' = L.init pkt' L.++ [(L.last pkt'){_last = Just 1}]

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
fcsInserterTest ::
  forall dataWidth.
  (1 <= dataWidth) =>
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
  SNat dataWidth ->
  Property
fcsInserterTest SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets 1 8 genPkt)
    (exposeClockResetEnable modelInsert)
    (exposeClockResetEnable (fcsInserterC @dataWidth))
 where
  pktOpts = defPacketOptions{poAllowEmptyPackets = False}
  genPkt = genValidPacket pktOpts (pure ()) (Range.linear 1 20)
  modelInsert packets = L.concatMap insertCrc (chunkByPacket packets)

-- | Test the FCS validator
fcsValidatorTest ::
  forall dataWidth.
  (1 <= dataWidth) =>
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
  SNat dataWidth ->
  Property
fcsValidatorTest SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets 1 8 genPkt)
    (exposeClockResetEnable modelValidate)
    (exposeClockResetEnable (fcsValidatorC @dataWidth))
 where
  pktOpts = defPacketOptions{poAllowEmptyPackets = False}
  genPkt =
    Gen.choice
      [ -- Random packet
        genValidPacket pktOpts (pure ()) (Range.linear 0 20)
      , -- Packet with valid CRC
        insertCrc <$> genValidPacket pktOpts (pure ()) (Range.linear 0 20)
      ]

  modelValidate packets = validateCrc =<< chunkByPacket packets

prop_fcs_inserter_d1 :: Property
prop_fcs_inserter_d1 = fcsInserterTest d1

prop_fcs_inserter_d3 :: Property
prop_fcs_inserter_d3 = fcsInserterTest d3

prop_fcs_inserter_d4 :: Property
prop_fcs_inserter_d4 = fcsInserterTest d4

prop_fcs_inserter_d7 :: Property
prop_fcs_inserter_d7 = fcsInserterTest d7

prop_fcs_validator_d1 :: Property
prop_fcs_validator_d1 = fcsValidatorTest d1

prop_fcs_validator_d3 :: Property
prop_fcs_validator_d3 = fcsValidatorTest d3

prop_fcs_validator_d4 :: Property
prop_fcs_validator_d4 = fcsValidatorTest d4

prop_fcs_validator_d7 :: Property
prop_fcs_validator_d7 = fcsValidatorTest d7

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
