{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cores.Ethernet.Arp.ArpManager where

-- base
import qualified Data.List as L
import Prelude

-- clash-prelude
import Clash.Prelude hiding ( repeat )
import Clash.Prelude as C

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-protocols
import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream
import Protocols.Hedgehog

-- ethernet
import Clash.Cores.Ethernet.Arp.ArpManager
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

-- tests
import Protocols.PacketStream.Hedgehog
import Clash.Hedgehog.Sized.Vector (genVec)


createDomain vSystem
  { vName="TestDom10Hz"
  , vPeriod=100_000_000_000
  , vActiveEdge=Rising
  , vResetKind=Asynchronous
  , vInitBehavior=Unknown
  , vResetPolarity=ActiveHigh
  }

ip1, ip2 :: IPv4Address
ip1 = IPv4Address $ 0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil
ip2 = IPv4Address $ 0xDE :> 0xAD :> 0xBE :> 0xEE :> Nil

mac1, mac2 :: MacAddress
mac1 = MacAddress (C.repeat 0x01)
mac2 = MacAddress (C.repeat 0x02)

-- | Test proper outgoing requests, relaying and timeouts of the ARP manager.
--   Manual test, because circuits containing ArpLookup cannot be automatically tested.
prop_arp_manager :: Property
prop_arp_manager = property $
  do L.zip expectedBwdOut expectedFwdOut === sampleN 32 (bundle (bwdOut, bundle fwdOut))
    where
      fwdIn :: [Maybe IPv4Address]
      fwdIn = [ Nothing
              , Nothing
              , Just ip1
              , Just ip1
              , Nothing
              , Just ip1
              , Just ip1
              , Just ip1
              , Just ip1
              ] L.++ L.repeat (Just ip2)

      bwdIn :: [(Maybe ArpResponse, Ack)]
      bwdIn = L.map (, Ack True) $
              [ Nothing
              , Nothing
              , Nothing
              , Just (ArpEntryFound mac1)
              , Nothing
              , Nothing
              , Just ArpEntryNotFound
              , Nothing
              , Just (ArpEntryFound mac1)
              , Nothing
              , Just ArpEntryNotFound]
              L.++ L.replicate 20 Nothing
              L.++ [Just ArpEntryNotFound]

      bwdOut :: Signal TestDom10Hz (Maybe ArpResponse)
      fwdOut :: (Signal TestDom10Hz (Maybe IPv4Address), Signal TestDom10Hz (Df.Data ArpLite))
      (bwdOut, fwdOut) = toSignals ckt inp
        where
          -- We wait 1-2 seconds for ARP replies
          ckt = exposeClockResetEnable (arpManagerC @TestDom10Hz d2) clockGen resetGen enableGen
          inp = (unbundle $ fromList fwdIn, unbundle $ fromList bwdIn)

      expectedBwdOut :: [Maybe ArpResponse]
      expectedBwdOut
        = [ Nothing
          , Nothing
          , Nothing
          , Just (ArpEntryFound mac1) -- Relaying to client circuit from ARP table
          , Nothing
          , Nothing
          , Nothing
          , Nothing
          , Just (ArpEntryFound mac1)] -- Relaying to client circuit from ARP reply
          L.++ L.replicate 22 Nothing
          L.++
          [Just ArpEntryNotFound] -- We were waiting for an ARP reply, but we timed out.

      expectedFwdOut :: [(Maybe IPv4Address, Df.Data ArpLite)]
      expectedFwdOut
        = [ (Nothing,  Df.NoData)
          , (Nothing,  Df.NoData)
          , (Just ip1, Df.NoData) -- Relay IP lookup to ARP table
          , (Just ip1, Df.NoData)
          , (Nothing,  Df.NoData)
          , (Just ip1, Df.NoData)
          , (Just ip1, Df.Data (ArpLite broadcastMac ip1 True)) -- We received ArpEntryNotFound from the ARP table, so send request
          , (Just ip1, Df.NoData)
          , (Just ip1, Df.NoData) -- Received ArpEntryFound
          , (Just ip2, Df.NoData)
          , (Just ip2, Df.Data (ArpLite broadcastMac ip2 True))] -- We received ArpEntryNotFound from the ARP table, so send request
          L.++ L.replicate 21 (Just ip2, Df.NoData) -- We were waiting for an ARP reply, but we timed out.

ourMac :: MacAddress
ourMac = MacAddress (0xDE C.:> 0xAD C.:> 0xBE C.:> 0xEF C.:> 0x01 C.:> 0x02 C.:> C.Nil)

ourIP :: IPv4Address
ourIP = IPv4Address (0x33 C.:> 0x44 C.:> 0x55 C.:> 0x66 C.:> C.Nil)

genArpLite :: Gen ArpLite
genArpLite = ArpLite <$>
          (MacAddress <$> genVec Gen.enumBounded) <*>
          (IPv4Address <$> genVec Gen.enumBounded) <*>
          Gen.enumBounded

arpTransmitterPropertyGenerator
  :: forall (dataWidth :: C.Nat)
   . 1 C.<= dataWidth
  => C.SNat dataWidth
  -> Property
arpTransmitterPropertyGenerator C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions{eoSampleMax = 2000}
    (Gen.list (Range.linear 1 10) genArpLite)
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System (arpTransmitterC (pure ourMac) (pure ourIP)))
    (===)
    where
      model :: [ArpLite] -> [PacketStreamM2S dataWidth EthernetHeader]
      model = packetizeFromDfModel toEthernetHdr toArpPkt

      toEthernetHdr arpLite = EthernetHeader {
        _macDst = _targetMac arpLite,
        _macSrc = ourMac,
        _etherType = arpEtherType
      }

      toArpPkt arpLite = newArpPacket ourMac ourIP (_targetMac arpLite) (_targetIPv4 arpLite) (_isRequest arpLite)

-- | headerBytes mod dataWidth ~ 0
prop_arp_transmitter_d1 :: Property
prop_arp_transmitter_d1 = arpTransmitterPropertyGenerator C.d1

-- | dataWidth < headerBytes
prop_arp_transmitter_d15 :: Property
prop_arp_transmitter_d15 = arpTransmitterPropertyGenerator C.d11

-- | dataWidth ~ headerBytes
prop_arp_transmitter_d28 :: Property
prop_arp_transmitter_d28 = arpTransmitterPropertyGenerator C.d28

-- | dataWidth > headerBytes
prop_arp_transmitter_d29 :: Property
prop_arp_transmitter_d29 = arpTransmitterPropertyGenerator C.d29

-- | generates 1/4 well-formed ARP requests intended for us,
-- 1/4 well-formed ARP requests intended for us but aborted,
-- 1/2 non-ARP request packet streams (with varying length and content).
-- All generated streams consist of full packets (i.e. end with a fragment with _last == True).
genPackets :: forall
    (dataWidth :: C.Nat)
  .  C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => Gen [PacketStreamM2S dataWidth EthernetHeader]
genPackets = do
  arpPkt <- genArpPacket
  abort <- Gen.bool
  pickRandomPacket <- Gen.bool
  if pickRandomPacket
  then
    C.fmap fullPackets (Gen.list (Range.linear 1 100) genPackets')
  else
    let stream = packetizeFromDfModel toEthernetHdr C.id [arpPkt] :: [PacketStreamM2S dataWidth EthernetHeader]
    in C.pure C.$ L.init stream L.++ [(L.last stream){_abort = abort}]

  where
    genArpPacket =
      newArpPacket C.<$>
      (MacAddress C.<$> genVec Gen.enumBounded) C.<*>
      (IPv4Address C.<$> genVec Gen.enumBounded) C.<*>
      Gen.constant ourMac C.<*>
      Gen.constant ourIP C.<*>
      Gen.bool

    genPackets' =
        PacketStreamM2S C.<$>
        genVec Gen.enumBounded C.<*>
        Gen.maybe Gen.enumBounded C.<*>
        genEthernetHdr C.<*>
        Gen.enumBounded

    genEthernetHdr :: Gen EthernetHeader
    genEthernetHdr =
        EthernetHeader C.<$>
        Gen.constant ourMac C.<*>
        (MacAddress C.<$> genVec Gen.enumBounded) C.<*>
        Gen.constant arpEtherType

    toEthernetHdr ArpPacket{..} = EthernetHeader {
        _macDst = _tha,
        _macSrc = _sha,
        _etherType = arpEtherType
      }

arpReceiverPropertyGenerator
  :: forall (dataWidth :: C.Nat)
   . 1 C.<= dataWidth
  => C.SNat dataWidth
  -> Property
arpReceiverPropertyGenerator C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    genPackets
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable @C.System C.$ arpReceiverC C.$ C.pure ourIP)
    (===)
    where
      model :: [PacketStreamM2S dataWidth EthernetHeader] -> ([ArpEntry],[ArpLite])
      model ethStr = (entries, lites)
        where
          arpDf = L.filter (validArp ourIP) (depacketizeToDfModel C.const ethStr)
          (arpRequests, arpEntries) = L.partition (isRequest ourIP)  arpDf

          validArp ip ArpPacket{..} =
                _htype C.== 1
                C.&& _ptype C.== 0x0800
                C.&& _hlen  C.== 6
                C.&& _plen  C.== 4
                C.&&(_oper C.== 1 C.&& (_tpa C.== ip C.|| _tpa C.== _spa) C.|| _oper C.== 2)

          isRequest ip ArpPacket{..} = _oper C.== 1 C.&& _tpa C.== ip

          entries = (\p -> ArpEntry (_sha p) (_spa p)) C.<$> arpEntries
          lites = (\p -> ArpLite (_sha p) (_spa p) C.False) C.<$> arpRequests

-- | headerBytes mod dataWidth ~ 0
prop_arp_receiver_d1 :: Property
prop_arp_receiver_d1 = arpReceiverPropertyGenerator C.d1

-- | dataWidth < headerBytes
prop_arp_receiver_d11 :: Property
prop_arp_receiver_d11 = arpReceiverPropertyGenerator C.d11

-- | dataWidth ~ headerBytes
prop_arp_receiver_d28 :: Property
prop_arp_receiver_d28 = arpReceiverPropertyGenerator C.d28

-- | dataWidth > headerBytes
prop_arp_receiver_d29 :: Property
prop_arp_receiver_d29 = arpReceiverPropertyGenerator C.d29

tests :: TestTree
tests =
    localOption (mkTimeout 10_000_000 {- 10 seconds -})
  $ localOption (HedgehogTestLimit (Just 500))
  $(testGroupGenerator)
