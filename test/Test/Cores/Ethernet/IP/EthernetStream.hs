{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Test.Cores.Ethernet.IP.EthernetStream where

import Clash.Prelude

-- hedgehog
import Hedgehog
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-protocols
import Protocols
import Protocols.PacketStream
import Protocols.Hedgehog

-- IP
import Clash.Cores.Ethernet.IP.EthernetStream
import Clash.Cores.Ethernet.IP.IPv4Types

-- MAC
import Clash.Cores.Ethernet.Mac.EthernetTypes

-- ARP
import Clash.Cores.Ethernet.Arp.ArpTypes
import Protocols.PacketStream.Hedgehog

myMac :: MacAddress
myMac = MacAddress (6 :> 6:> 6:> 6:> 6:> 6:> Nil)

someMac :: MacAddress
someMac = MacAddress (7 :> 7:> 0:> 7 :> 7:> 6:> Nil)

-- | drive the bwd of the arp lookup constantly with
-- a given response.
arpConstC
  :: forall (dom :: Domain)
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => ArpResponse
  -> Circuit (ArpLookup dom) ()
arpConstC response = fromSignals ckt
  where
    ckt (_,_) = (pure $ Just response, ())

-- | toEthernetStream, but with the arp lookup given
-- by arpConstC
testCircuit
  :: forall (dom :: Domain) (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => KnownNat dataWidth
  => ArpResponse
  -> Circuit (PacketStream dom dataWidth IPv4Address) (PacketStream dom dataWidth EthernetHeader)
testCircuit response = circuit $ \packet -> do
  (packetOut, lookup) <- toEthernetStreamC $ pure myMac -< packet
  () <- arpConstC response -< lookup
  idC -< packetOut

-- model of testCircuit: inserts the given macadress when the
-- arp response is an ArpEntryFound mac,
-- drops the entire packet if the arp response is ArpEntryNotFound.
model
  :: ArpResponse
  -> [PacketStreamM2S dataWidth IPv4Address]
  -> [PacketStreamM2S dataWidth EthernetHeader]
model response = case response of
  ArpEntryNotFound -> const []
  ArpEntryFound ma -> fmap (hdr <$)
    where
      hdr = EthernetHeader ma myMac 0x0800

ethernetStreamTest
  :: forall (dataWidth :: Nat)
    . 1 <= dataWidth
  => KnownNat dataWidth
  => 1 <= dataWidth
  => SNat dataWidth
  -> ArpResponse
  -> Property
ethernetStreamTest SNat arpResponse =
  propWithModelSingleDomain
    @System
    defExpectOptions
    (genValidPackets (Range.linear 1 10) (Range.linear 0 10) Abort)
    (exposeClockResetEnable (model arpResponse))
    (exposeClockResetEnable @System ckt)
    (===)
  where
    ckt :: forall (dom :: Domain)
      . HiddenClockResetEnable dom
      => Circuit (PacketStream dom dataWidth IPv4Address) (PacketStream dom dataWidth EthernetHeader)
    ckt = testCircuit arpResponse

-- | We test whether the circuit succesfully inserts the given macadress when the
-- arp lookup service constantly gives an ArpEntryFound mac,
-- and whether the circuit succesfully drops the entire packet if the arp lookup
-- service constantly gives an ArpEntryNotFound.
prop_ethernetstreamer_d1_noresp, prop_ethernetstreamer_d21_noresp, prop_ethernetstreamer_d28_noresp :: Property
prop_ethernetstreamer_d1_resp, prop_ethernetstreamer_d21_resp, prop_ethernetstreamer_d28_resp :: Property

-- dataWidth ~ 1
prop_ethernetstreamer_d1_noresp  = ethernetStreamTest d1 ArpEntryNotFound
prop_ethernetstreamer_d1_resp = ethernetStreamTest d21 (ArpEntryFound someMac)

-- dataWidth large
prop_ethernetstreamer_d21_resp = ethernetStreamTest d21 (ArpEntryFound someMac)
prop_ethernetstreamer_d21_noresp  = ethernetStreamTest d21 ArpEntryNotFound

-- dataWidth extra large
prop_ethernetstreamer_d28_resp = ethernetStreamTest d28 (ArpEntryFound someMac)
prop_ethernetstreamer_d28_noresp  = ethernetStreamTest d28 ArpEntryNotFound


tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
