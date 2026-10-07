{-# LANGUAGE NumericUnderscores #-}

module Test.Cores.Ethernet.IP.IPPacketizers (
  tests,
) where

import Clash.Cores.Ethernet.IP.IPPacketizers
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes (EthernetHeader)

import Clash.Prelude

import qualified Data.List as L
import Data.Maybe (isJust)

import Hedgehog (Gen, Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Protocols.Experimental.Hedgehog
import Protocols.Experimental.PacketStream ()
import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

import Test.Cores.Ethernet.Base
import Test.Cores.Ethernet.InternetChecksum (pureInternetChecksum)

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

testIPPacketizer ::
  forall (dataWidth :: Nat).
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Property
testIPPacketizer SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 400, eoStopAfterEmpty = Just 400}
    (genPackets 1 4 (genValidPacket defPacketOptions genIPv4Header (Range.linear 0 30)))
    (exposeClockResetEnable (packetizerModel _ipv4Destination id . setChecksums))
    (exposeClockResetEnable (ipPacketizerC @_ @dataWidth))
 where
  setChecksums ps = L.concatMap setChecksum (chunkByPacket ps)
  setChecksum [] =
    -- 'chunkBy' filters empty lists
    error "Unreachable code"
  setChecksum xs@(x0:_) =
    L.map (\x1 -> x1{_meta = (_meta x1){_ipv4Checksum = checksum}}) xs
   where
    checksum = (pureInternetChecksum @(Vec 10) . bitCoerce . _meta) x0

testIPDepacketizer ::
  forall (dataWidth :: Nat).
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Property
testIPDepacketizer SNat =
  testIPDepacketizerWith (SNat @dataWidth) (genPackets 1 10 genPkt)
 where
  validPkt = genValidPacket defPacketOptions genEthernetHeader (Range.linear 0 10)
  genPkt =
    Gen.choice
      [ -- Random packet: extremely high chance to get aborted.
        validPkt
      , -- Packet with valid header: should not get aborted.
        do
          hdr <- genIPv4Header
          packetizerModel
            id
            (const hdr{_ipv4Checksum = pureInternetChecksum (bitCoerce hdr :: Vec 10 (BitVector 16))})
            <$> validPkt
      , -- Packet with valid header apart from (most likely) the checksum.
        do
          hdr <- genIPv4Header
          packetizerModel id (const hdr{_ipv4Checksum = 0xABCD}) <$> validPkt
      ]

testIPDepacketizerWith ::
  forall dataWidth.
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Gen [PacketStreamM2S dataWidth EthernetHeader] ->
  Property
testIPDepacketizerWith SNat gen =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoStopAfterEmpty = Just 400}
    gen
    (exposeClockResetEnable model)
    (exposeClockResetEnable (ipDepacketizerC @_ @dataWidth))
 where
  model fragments = L.concat $ L.zipWith setAbort packets aborts
   where
    setAbort [] _ = []
    setAbort packet@(p:_) abort =
      (\f -> f{_abort = _abort f || abort || (isJust (_last f) && endAbort)}) <$> trimmed
     where
      payload = downConvert packet
      actual = L.length (L.filter ((/= Just 0) . _last) payload)
      expected = fromIntegral (satSub SatBound (_ipv4Length (_meta p)) 20)
      endAbort = L.any _abort packet || actual < expected
      trimmed
        | actual <= expected = packet
        | otherwise = upConvert $
            L.take expected payload L.++ [p{_data = singleton 0, _last = Just 0, _abort = endAbort}]
    getMeta [] =
      -- 'chunkBy' filters empty lists
      error "Unreachable code"
    getMeta (p:_) = _meta p
    validateHeader hdr =
      pureInternetChecksum (bitCoerce hdr :: Vec 10 (BitVector 16)) /= 0
        || _ipv4Ihl hdr /= 5
        || _ipv4Version hdr /= 4
        || _ipv4FlagReserved hdr
        || _ipv4FlagMF hdr
        || _ipv4FragmentOffset hdr /= 0
        || _ipv4Length hdr < 20
    packets = chunkByPacket $ depacketizerModel const fragments
    aborts = validateHeader . getMeta <$> packets

-- | 20 % dataWidth ~ 0
prop_ip_ip_packetizer_d1 :: Property
prop_ip_ip_packetizer_d1 = testIPPacketizer d1

-- | dataWidth < 20
prop_ip_ip_packetizer_d7 :: Property
prop_ip_ip_packetizer_d7 = testIPPacketizer d7

-- | dataWidth ~ 20
prop_ip_ip_packetizer_d20 :: Property
prop_ip_ip_packetizer_d20 = testIPPacketizer d20

-- | dataWidth > 20
prop_ip_ip_packetizer_d23 :: Property
prop_ip_ip_packetizer_d23 = testIPPacketizer d23

-- | 20 % dataWidth ~ 0
prop_ip_depacketizer_d1 :: Property
prop_ip_depacketizer_d1 = testIPDepacketizer d1

-- | dataWidth < 20
prop_ip_depacketizer_d7 :: Property
prop_ip_depacketizer_d7 = testIPDepacketizer d7

-- | dataWidth ~ 20
prop_ip_depacketizer_d20 :: Property
prop_ip_depacketizer_d20 = testIPDepacketizer d20

-- | dataWidth > 20
prop_ip_depacketizer_d23 :: Property
prop_ip_depacketizer_d23 = testIPDepacketizer d23

ipPacketWithEnding :: Unsigned 16 -> Int -> Bool -> [PacketStreamM2S 7 EthernetHeader]
ipPacketWithEnding len payloadSize aborted =
  (\f -> f{_abort = aborted && isJust (_last f)}) <$> packet
 where
  header = (unpack 0){_ipv4Version = 4, _ipv4Ihl = 5, _ipv4Length = len}
  checksum = pureInternetChecksum (bitCoerce header :: Vec 10 (BitVector 16))
  payload = fullPackets $ L.replicate payloadSize $
    PacketStreamM2S (singleton 0) Nothing (unpack 0) False
  packet = upConvert $ packetizerModel id (const header{_ipv4Checksum = checksum}) payload

prop_ip_depacketizer_truncated_d7 :: Property
prop_ip_depacketizer_truncated_d7 =
  testIPDepacketizerWith d7 (pure $ ipPacketWithEnding 72 8 False)

prop_ip_depacketizer_late_abort_d7 :: Property
prop_ip_depacketizer_late_abort_d7 =
  testIPDepacketizerWith d7 (pure $ ipPacketWithEnding 35 15 True)

prop_ip_depacketizer_padding_abort_d7 :: Property
prop_ip_depacketizer_padding_abort_d7 =
  testIPDepacketizerWith d7 (pure $ ipPacketWithEnding 28 22 True)

tests :: TestTree
tests =
  localOption (mkTimeout 180_000_000) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
