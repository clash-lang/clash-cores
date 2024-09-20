{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cores.Ethernet.Arp.ArpManager where

import Clash.Cores.Ethernet.Arp.ArpManager
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Clash.Prelude

import qualified Data.List as L

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Protocols
import qualified Protocols.Df as Df
import Protocols.Hedgehog
import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

import Test.Cores.Ethernet.Base (genIPv4Addr, genMacAddr)

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

createDomain
  vSystem
    { vName = "TestDom10Hz"
    , vPeriod = 100_000_000_000
    , vActiveEdge = Rising
    , vResetKind = Asynchronous
    , vInitBehavior = Unknown
    , vResetPolarity = ActiveHigh
    }

ip1, ip2 :: IPv4Address
ip1 = IPv4Address (0xDE :> 0xAD :> 0xBE :> 0xEF :> Nil)
ip2 = IPv4Address (0xDE :> 0xAD :> 0xBE :> 0xEE :> Nil)

mac1 :: MacAddress
mac1 = MacAddress (repeat 0x01)

ourMac :: MacAddress
ourMac = MacAddress (0xDE :> 0xAD :> 0xBE :> 0xEF :> 0x01 :> 0x02 :> Nil)

ourIPv4 :: IPv4Address
ourIPv4 = IPv4Address (0x33 :> 0x44 :> 0x55 :> 0x66 :> Nil)

genArpLite :: Gen ArpLite
genArpLite =
  ArpLite
    <$> genMacAddr
    <*> genIPv4Addr
    <*> Gen.enumBounded

{- | Test proper outgoing requests, relaying and timeouts of the ARP manager.
  Manual test, because circuits containing ArpLookup cannot be automatically tested.
-}
prop_arp_manager :: Property
prop_arp_manager = property $
  do L.zip expectedBwdOut expectedFwdOut === sampleN 32 (bundle (bwdOut, bundle fwdOut))
 where
  fwdIn :: [Maybe IPv4Address]
  fwdIn =
    [ Nothing
    , Nothing
    , Just ip1
    , Just ip1
    , Nothing
    , Just ip1
    , Just ip1
    , Just ip1
    , Just ip1
    ]
      L.++ L.repeat (Just ip2)

  bwdIn :: [(Maybe ArpResponse, Ack)]
  bwdIn =
    L.map (,Ack True) $
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
      , Just ArpEntryNotFound
      ]
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
  expectedBwdOut =
    [ Nothing
    , Nothing
    , Nothing
    , Just (ArpEntryFound mac1) -- Relaying to client circuit from ARP table
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Just (ArpEntryFound mac1) -- Relaying to client circuit from ARP reply
    ]
      L.++ L.replicate 22 Nothing
      L.++ [Just ArpEntryNotFound] -- We were waiting for an ARP reply, but we timed out.
  expectedFwdOut :: [(Maybe IPv4Address, Df.Data ArpLite)]
  expectedFwdOut =
    [ (Nothing, Df.NoData)
    , (Nothing, Df.NoData)
    , (Just ip1, Df.NoData) -- Relay IP lookup to ARP table
    , (Just ip1, Df.NoData)
    , (Nothing, Df.NoData)
    , (Just ip1, Df.NoData)
    , (Just ip1, Df.Data (ArpLite broadcastMac ip1 True)) -- We received ArpEntryNotFound from the ARP table, so send request
    , (Just ip1, Df.NoData)
    , (Just ip1, Df.NoData) -- Received ArpEntryFound
    , (Just ip2, Df.NoData)
    , (Just ip2, Df.Data (ArpLite broadcastMac ip2 True)) -- We received ArpEntryNotFound from the ARP table, so send request
    ]
      L.++ L.replicate 21 (Just ip2, Df.NoData) -- We were waiting for an ARP reply, but we timed out.

arpTransmitterPropertyGenerator ::
  forall (dataWidth :: Nat).
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Property
arpTransmitterPropertyGenerator SNat =
  propWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000}
    (Gen.list (Range.linear 1 10) genArpLite)
    (exposeClockResetEnable model)
    (exposeClockResetEnable @System (arpTransmitterC (pure ourMac) (pure ourIPv4)))
    (===)
 where
  model :: [ArpLite] -> [PacketStreamM2S dataWidth MacAddress]
  model = packetizeFromDfModel _targetMac toArpPkt

  toArpPkt ArpLite{..} =
    newArpPacket ourMac ourIPv4 _targetMac _targetIPv4 _isRequest

arpReceiverPropertyGenerator ::
  forall (dataWidth :: Nat).
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Property
arpReceiverPropertyGenerator SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoStopAfterEmpty = 1000}
    (genPackets (Range.linear 1 5) Abort genPkt)
    (exposeClockResetEnable model)
    (exposeClockResetEnable @System (arpReceiverC $ pure ourIPv4))
 where
  genArpPacket isGratuitous = do
    spa <- genIPv4Addr
    newArpPacket
      <$> genMacAddr
      <*> Gen.constant spa
      <*> Gen.constant ourMac
      <*> Gen.constant (if isGratuitous then spa else ourIPv4)
      <*> Gen.bool

  genPkt am =
    Gen.choice
      [ -- Random packet
        genValidPacket (pure ()) (Range.linear 0 20) am
      , -- Valid ARP reply/request
        do
          arpPkt <- genArpPacket False
          pure (packetizeFromDfModel (pure ()) id [arpPkt])
      , -- Valid gratuitous ARP reply/request
        do
          arpPkt <- genArpPacket True
          pure (packetizeFromDfModel (pure ()) id [arpPkt])
      ]

  model :: [PacketStreamM2S dataWidth ()] -> ([ArpEntry], [ArpLite])
  model ethStr = (entries, lites)
   where
    arpDf = L.filter (isValidArp ourIPv4) (depacketizeToDfModel const ethStr)
    (arpRequests, arpEntries) = L.partition (isRequest ourIPv4) arpDf

    isRequest ip ArpPacket{..} = _oper == 1 && _tpa == ip

    entries = (\p -> ArpEntry (_sha p) (_spa p)) <$> arpEntries
    lites = (\p -> ArpLite (_sha p) (_spa p) False) <$> arpRequests

-- | headerBytes mod dataWidth ~ 0
prop_arp_transmitter_d1 :: Property
prop_arp_transmitter_d1 = arpTransmitterPropertyGenerator d1

-- | dataWidth < headerBytes
prop_arp_transmitter_d15 :: Property
prop_arp_transmitter_d15 = arpTransmitterPropertyGenerator d11

-- | dataWidth ~ headerBytes
prop_arp_transmitter_d28 :: Property
prop_arp_transmitter_d28 = arpTransmitterPropertyGenerator d28

-- | dataWidth > headerBytes
prop_arp_transmitter_d29 :: Property
prop_arp_transmitter_d29 = arpTransmitterPropertyGenerator d29

-- | headerBytes mod dataWidth ~ 0
prop_arp_receiver_d1 :: Property
prop_arp_receiver_d1 = arpReceiverPropertyGenerator d1

-- | dataWidth < headerBytes
prop_arp_receiver_d11 :: Property
prop_arp_receiver_d11 = arpReceiverPropertyGenerator d11

-- | dataWidth ~ headerBytes
prop_arp_receiver_d28 :: Property
prop_arp_receiver_d28 = arpReceiverPropertyGenerator d28

-- | dataWidth > headerBytes
prop_arp_receiver_d29 :: Property
prop_arp_receiver_d29 = arpReceiverPropertyGenerator d29

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 500))
      $(testGroupGenerator)
