{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Test.Cores.Ethernet.IP.EthernetStream (
  tests,
) where

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.EthernetStream
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Clash.Prelude

import Hedgehog (Property)
import qualified Hedgehog.Range as Range

import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

import Test.Cores.Ethernet.Base (genIPv4Addr)

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

myMac :: MacAddress
myMac = MacAddress (6 :> 6 :> 6 :> 6 :> 6 :> 6 :> Nil)

someMac :: MacAddress
someMac = MacAddress (7 :> 7 :> 0 :> 7 :> 7 :> 6 :> Nil)

{- | drive the bwd of the arp lookup constantly with
a given response.
-}
arpConstC ::
  forall (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownDomain dom) =>
  ArpResponse ->
  Circuit (ArpLookup dom) ()
arpConstC response = fromSignals ckt
 where
  ckt (_, _) = (pure $ Just response, ())

{- | toEthernetStream, but with the arp lookup given
by arpConstC
-}
testCircuit ::
  forall (dom :: Domain) (dataWidth :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownDomain dom) =>
  (KnownNat dataWidth) =>
  ArpResponse ->
  Circuit
    (PacketStream dom dataWidth IPv4Address)
    (PacketStream dom dataWidth EthernetHeader)
testCircuit response = circuit $ \packet -> do
  (packetOut, lookup) <- toEthernetStreamC $ pure myMac -< packet
  () <- arpConstC response -< lookup
  idC -< packetOut

-- model of testCircuit: inserts the given macadress when the
-- arp response is an ArpEntryFound mac,
-- drops the entire packet if the arp response is ArpEntryNotFound.
model ::
  ArpResponse ->
  [PacketStreamM2S dataWidth IPv4Address] ->
  [PacketStreamM2S dataWidth EthernetHeader]
model response = case response of
  ArpEntryNotFound -> const []
  ArpEntryFound ma -> fmap (hdr <$)
   where
    hdr = EthernetHeader ma myMac 0x0800

ethernetStreamTest ::
  forall (dataWidth :: Nat).
  (1 <= dataWidth) =>
  SNat dataWidth ->
  ArpResponse ->
  Property
ethernetStreamTest SNat arpResponse =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets 1 10 (genValidPacket defPacketOptions genIPv4Addr (Range.linear 0 10)))
    (exposeClockResetEnable (model arpResponse))
    (exposeClockResetEnable (testCircuit @_ @dataWidth arpResponse))

{-
We test whether the circuit succesfully inserts the given MAC address when
the ARP lookup service constantly gives an @ArpEntryFound@, and whether the
circuit succesfully drops the entire packet if the ARP lookup service
constantly gives an @ArpEntryNotFound@.
-}

-- dataWidth ~ 1
prop_ethernetstream_d1_noresp :: Property
prop_ethernetstream_d1_noresp = ethernetStreamTest d1 ArpEntryNotFound

prop_ethernetstream_d1_resp :: Property
prop_ethernetstream_d1_resp = ethernetStreamTest d21 (ArpEntryFound someMac)

-- dataWidth large
prop_ethernetstream_d21_resp :: Property
prop_ethernetstream_d21_resp = ethernetStreamTest d21 (ArpEntryFound someMac)

prop_ethernetstream_d21_noresp :: Property
prop_ethernetstream_d21_noresp = ethernetStreamTest d21 ArpEntryNotFound

-- dataWidth extra large
prop_ethernetstream_d28_resp :: Property
prop_ethernetstream_d28_resp = ethernetStreamTest d28 (ArpEntryFound someMac)

prop_ethernetstream_d28_noresp :: Property
prop_ethernetstream_d28_noresp = ethernetStreamTest d28 ArpEntryNotFound

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
