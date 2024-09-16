{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cores.Ethernet.Icmp (
  tests,
) where

import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Icmp

import Clash.Prelude

import qualified Data.Bifunctor as B
import qualified Data.List as L

import Hedgehog (Property)
import qualified Hedgehog.Range as Range

import Protocols.Hedgehog
import Protocols.PacketStream (PacketStreamM2S(_meta))
import Protocols.PacketStream.Hedgehog

import Test.Cores.Ethernet.Base (genIPv4HeaderLite)
import Test.Cores.Ethernet.InternetChecksum (calculateChecksum)

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

ourIPv4 :: IPv4Address
ourIPv4 = IPv4Address (repeat @4 0x3)

icmpResponderPropertyGenerator ::
  forall dataWidth.
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Property
icmpResponderPropertyGenerator SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets (Range.linear 1 5) Abort genValidIcmpRequestPacket)
    (exposeClockResetEnable (L.concatMap model . chunkByPacket))
    (exposeClockResetEnable (icmpEchoResponderC $ pure ourIPv4))
 where
  genValidIcmpRequestPacket am = do
    dat <- genValidPacket (genIPv4HeaderLite ourIPv4) (Range.linear 0 10) am
    let checksum = calculateChecksum (packetizerModel id (const (IcmpHeader 8 0 0)) dat)
    pure $ packetizerModel id (const $ IcmpHeader 8 0 checksum) dat

  model ::
    [PacketStreamM2S dataWidth IPv4HeaderLite] ->
    [PacketStreamM2S dataWidth IPv4HeaderLite]
  model fragments = res
   where
    filtered =
      L.map (fmap (B.second toIcmpLite)) $
        L.filter ((\hdr -> _code hdr == 0 && _type hdr == 8) . snd . _meta) $
          depacketizerModel (\icmpHdr ipHdr -> (ipHdr, icmpHdr)) fragments

    withIcmpHeader =
      packetizerModel fst (fromIcmpLite . snd) $ fmap (B.bimap swapIPs (updateChecksum 0))
        <$> filtered

    res =
      packetizerModel fst (fromIcmpLite . snd) $
        L.map (fmap (B.bimap swapIPs (updateChecksum newChecksum))) filtered

    -- To prevent the tests from failing, we set the checksum to 0x0000 if
    -- it is 0xFFFF. This is a limitation of manually adjusting the checksum,
    -- but the case will almost never happen in practice. See the comments in
    -- Clash.Cores.Ethernet.Icmp for more information.
    newChecksum =
      let c = calculateChecksum withIcmpHeader
       in if c == 0xFFFF then 0x0000 else c

    swapIPs ipHdr =
      ipHdr
        { _ipv4lSource = ourIPv4
        , _ipv4lDestination = _ipv4lSource ipHdr
        }
    updateChecksum chk icmpHdr = icmpHdr{_checksumL = chk}

prop_icmp_responder_d1 :: Property
prop_icmp_responder_d1 = icmpResponderPropertyGenerator d1

prop_icmp_responder_d3 :: Property
prop_icmp_responder_d3 = icmpResponderPropertyGenerator d3

prop_icmp_responder_d4 :: Property
prop_icmp_responder_d4 = icmpResponderPropertyGenerator d4

prop_icmp_responder_d7 :: Property
prop_icmp_responder_d7 = icmpResponderPropertyGenerator d7

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
