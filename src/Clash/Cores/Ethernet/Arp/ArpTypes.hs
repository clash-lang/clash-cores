{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

{-|
Module      : Clash.Cores.Ethernet.Arp.ArpTypes
Description : Provides various data types, aliases, constructors and constants for the Address Resolution Protocol. This module only provides the most common use case of ARP, which is mapping IPv4 addresses to MAC addresses.
-}
module Clash.Cores.Ethernet.Arp.ArpTypes where

import Clash.Prelude

import Protocols

import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Control.DeepSeq ( NFData )


-- | An entry for our ARP table, which maps an IPv4 address to a MAC address.
--   A timestamp should be kept separately from this type.
data ArpEntry
  = ArpEntry {
    _arpMac :: MacAddress,
    _arpIP :: IPv4Address
    } deriving (Generic, Show, ShowX, NFDataX, NFData, Eq)

-- | An ARP response. Either the IPv4 address is not found in the table, or it is and its
--   corresponding MAC address is returned.
data ArpResponse = ArpEntryNotFound | ArpEntryFound MacAddress
  deriving (Generic, Show, ShowX, NFDataX, Eq)

-- | Protocol used to query the ARP service
data ArpLookup (dom :: Domain)

instance Protocol (ArpLookup dom) where
  type Fwd (ArpLookup dom) = Signal dom (Maybe IPv4Address)
  type Bwd (ArpLookup dom) = Signal dom (Maybe ArpResponse)

-- | Structure that contains enough information to construct an outgoing request or reply for IPv4,
--   given that we already have access to our own MAC- and IPv4 address.
data ArpLite
  = ArpLite {
    _targetMac :: MacAddress,
    _targetIPv4 :: IPv4Address,
    _isRequest :: Bool
    } deriving (Generic, Show, ShowX, NFDataX, NFData, Eq)

-- | ARP packet structure. The first four fields are constant for our use case.
data ArpPacket
  = ArpPacket {
    _htype :: BitVector 16,
    -- ^ Hardware type. 0x0001 for Ethernet.
    _ptype :: BitVector 16,
    -- ^ Protocol type. 0x0800 for IPv4.
    _hlen :: BitVector 8,
    -- ^ Length of the hardware adresses. 0x06 for Ethernet.
    _plen :: BitVector 8,
    -- ^ Length of the protocol (internet) addresses. 0x04 for IPv4.
    _oper :: BitVector 16,
    -- ^ Operation that the sender is performing: 0x0001 for request, 0x0002 for reply.
    _sha :: MacAddress,
    -- ^ Sender hardware address
    _spa :: IPv4Address,
    -- ^ Sender protocol address
    _tha :: MacAddress,
    -- ^ Target hardware address
    _tpa :: IPv4Address
    -- ^ Target protocol address
  } deriving (Generic, Eq, Show, ShowX, NFDataX, NFData, BitPack)

-- | ARP's EtherType for multiplexing purposes.
arpEtherType :: BitVector 16
arpEtherType = 0x0806

-- | Construct an IPv4 ARP packet.
newArpPacket
  :: MacAddress
  -- ^ Our MAC address
  -> IPv4Address
  -- ^ Our IP address
  -> MacAddress
  -- ^ Target MAC address
  -> IPv4Address
  -- ^ Target IP address
  -> Bool
  -- ^ We construct a request if this is @True@. Else, we construct a reply
  -> ArpPacket
newArpPacket myMac myIP targetMac targetIP isRequest
  = ArpPacket {
      _htype = 0x0001,
      _ptype = 0x0800,
      _hlen = 0x06,
      _plen = 0x04,
      _oper = if isRequest then 0x0001 else 0x0002,
      _sha = myMac,
      _spa = myIP,
      _tha = targetMac,
      _tpa = targetIP
    }


-- | Construct a link-layer `EthernetHeader` from an ARP packet.
arpToEthernetHeader :: ArpPacket -> EthernetHeader
arpToEthernetHeader ArpPacket{..} =
  EthernetHeader
    { _macDst = _tha
    , _macSrc = _sha
    , _etherType = arpEtherType
    }

-- | Construct a link-layer `EthernetHeader` from a reduced ARP packet.
arpLiteToEthernetHeader ::
  -- | Our MAC address
  MacAddress ->
  ArpLite ->
  EthernetHeader
arpLiteToEthernetHeader ourMac ArpLite{..} =
  EthernetHeader
    { _macDst = _targetMac
    , _macSrc = ourMac
    , _etherType = arpEtherType
    }

-- | Whether we should accept an incoming ARP packet or not.
isValidArp ::
  -- | Our IPv4 address
  IPv4Address ->
  -- | Incoming ARP packet
  ArpPacket ->
  Bool
isValidArp ourIPv4 ArpPacket{..} =
  _htype == 1 &&
  _ptype == 0x0800 &&
  _hlen  == 6 &&
  _plen  == 4 &&
  -- If TPA == SPA, the ARP packet is gratuitous.
  (_oper == 1 && (_tpa == ourIPv4 || _tpa == _spa) || _oper == 2)
