{-# LANGUAGE NumericUnderscores #-}

module Test.Cores.Ethernet.IP.IPPacketizers (
  tests,
) where

import Clash.Cores.Ethernet.IP.IPPacketizers
import Clash.Cores.Ethernet.IP.IPv4Types

import Clash.Prelude

import qualified Data.List as L

import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Protocols.Hedgehog
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
    defExpectOptions{eoSampleMax = 400, eoStopAfterEmpty = 400}
    (genPackets 1 4 (genValidPacket defPacketOptions genIPv4Header (Range.linear 0 30)))
    (exposeClockResetEnable (packetizerModel _ipv4Destination id . setChecksums))
    (exposeClockResetEnable (ipPacketizerC @_ @dataWidth))
 where
  setChecksums ps = L.concatMap setChecksum (chunkByPacket ps)
  setChecksum xs = L.map (\x -> x{_meta = (_meta x){_ipv4Checksum = checksum}}) xs
   where
    checksum = (pureInternetChecksum @(Vec 10) . bitCoerce . _meta) (L.head xs)

testIPDepacketizer ::
  forall (dataWidth :: Nat).
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Property
testIPDepacketizer SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoStopAfterEmpty = 400}
    (genPackets 1 10 genPkt)
    (exposeClockResetEnable model)
    (exposeClockResetEnable (ipDepacketizerC @_ @dataWidth))
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

  model fragments = L.concat $ L.zipWith setAbort packets aborts
   where
    setAbort packet abort = (\f -> f{_abort = _abort f || abort}) <$> packet
    validateHeader hdr =
      pureInternetChecksum (bitCoerce hdr :: Vec 10 (BitVector 16)) /= 0
        || _ipv4Ihl hdr /= 5
        || _ipv4Version hdr /= 4
        || _ipv4FlagReserved hdr
        || _ipv4FlagMF hdr
    packets = chunkByPacket $ depacketizerModel const fragments
    aborts = validateHeader . _meta . L.head <$> packets

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

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
