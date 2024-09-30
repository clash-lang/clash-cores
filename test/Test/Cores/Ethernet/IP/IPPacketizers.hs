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
    (genPackets (Range.linear 1 4) Abort (genValidPacket genIPv4Header (Range.linear 0 30)))
    (exposeClockResetEnable (packetizerModel _ipv4Destination id . setChecksums))
    (exposeClockResetEnable (ipPacketizerC @_ @dataWidth))
 where
  setChecksums ps = L.concatMap setChecksum (chunkByPacket ps)
  setChecksum xs = L.map (\x -> x{_meta = (_meta x){_ipv4Checksum = checksum}}) xs
   where
    checksum = (pureInternetChecksum @(Vec 10) . bitCoerce . _meta) (L.head xs)

testVerifyIPChecksum ::
  forall dataWidth.
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Property
testVerifyIPChecksum SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoStopAfterEmpty = 1000, eoDriveEarly=False, eoResetCycles=0}
    (genPackets (Range.linear 1 1) Abort genPkt)
    (exposeClockResetEnable model)
    (exposeClockResetEnable (verifyChecksumC @dataWidth))
 where
  --x = natToNum @(20 `DivRU` dataWidth + 1)
  validPkt = genValidPacket (pure ()) (Range.linear 3 3)
  genPkt am =
    Gen.choice
      [ -- Random packet: extremely high chance to get aborted.
        validPkt am
        -- Packet with valid header: should not get aborted.
      , do
          hdr <- genIPv4Header
          packetizerModel
            id
            (const hdr{_ipv4Checksum = pureInternetChecksum (bitCoerce hdr :: Vec 10 (BitVector 16))})
              <$> validPkt am
        -- Packet with valid header apart from (most likely) the checksum.
      , do
          hdr <- genIPv4Header
          packetizerModel id (const hdr{_ipv4Checksum = 0x0001}) <$> validPkt am
      ]

  model fragments = L.concatMap go packets
   where
    packets = chunkByPacket fragments

    go packet
      | dropPacket = []
      | otherwise = packet
     where
      asIpv4hdr :: [PacketStreamM2S dataWidth IPv4Header]
      asIpv4hdr = depacketizerModel const packet

      hdr = _meta (L.head asIpv4hdr)
      dropPacket = pureInternetChecksum (bitCoerce hdr :: Vec 10 (BitVector 16)) /= 0

prop_checksum_verif_d1 :: Property
prop_checksum_verif_d1 = testVerifyIPChecksum d1

prop_checksum_verif_d2 :: Property
prop_checksum_verif_d2 = testVerifyIPChecksum d2

prop_checksum_verif_d3 :: Property
prop_checksum_verif_d3 = testVerifyIPChecksum d3

prop_checksum_verif_d4 :: Property
prop_checksum_verif_d4 = testVerifyIPChecksum d4

prop_checksum_verif_d5 :: Property
prop_checksum_verif_d5 = testVerifyIPChecksum d5

prop_checksum_verif_d6 :: Property
prop_checksum_verif_d6 = testVerifyIPChecksum d6

prop_checksum_verif_d7 :: Property
prop_checksum_verif_d7 = testVerifyIPChecksum d7

prop_checksum_verif_d9 :: Property
prop_checksum_verif_d9 = testVerifyIPChecksum d9

-- Fails!
prop_checksum_verif_d11 :: Property
prop_checksum_verif_d11 = testVerifyIPChecksum d11

prop_checksum_verif_d20 :: Property
prop_checksum_verif_d20 = testVerifyIPChecksum d20

prop_checksum_verif_d25 :: Property
prop_checksum_verif_d25 = testVerifyIPChecksum d25

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

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
