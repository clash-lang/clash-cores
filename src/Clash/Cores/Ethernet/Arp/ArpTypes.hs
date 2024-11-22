{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides various data types, aliases, constructors and constants for the
Address Resolution Protocol (ARP). This module only provides the most common
use case of ARP, which is mapping IPv4 addresses to MAC addresses.
-}
module Clash.Cores.Ethernet.Arp.ArpTypes (
  -- ** Types and constructors
  ArpEntry (..),
  ArpResponse (..),
  ArpOperation (..),
  ArpLite (..),
  ArpPacket (..),
  ArpLookup,
  newArpPacket,

  -- ** Constants
  arpEtherType,

  -- ** Packet classification
  isValidArp,
  isGratuitous,
  expectsReply,
) where

import Clash.Cores.Ethernet.IP.IPv4Types (IPv4Address)
import Clash.Cores.Ethernet.Mac.EthernetTypes (MacAddress)

import Clash.Prelude

import Control.DeepSeq (NFData)

import Protocols (Protocol (..))

{- |
An entry for our ARP table, which maps an IPv4 address to a MAC address.
A timestamp should be kept separately from this type.
-}
data ArpEntry = ArpEntry
  { _arpMac :: MacAddress
  -- ^ MAC address
  , _arpIP :: IPv4Address
  -- ^ Corresponding IPv4 address
  }
  deriving (Eq, Generic, NFData, NFDataX, Show, ShowX)

{- |
An ARP response. Either the IPv4 address is not found in the table, or it is
and its corresponding MAC address is returned.
-}
data ArpResponse = ArpEntryNotFound | ArpEntryFound MacAddress
  deriving (Generic, Show, ShowX, NFDataX, Eq)

-- | Simple request-response protocol used to query the ARP service.
data ArpLookup (dom :: Domain)

instance Protocol (ArpLookup dom) where
  type Fwd (ArpLookup dom) = Signal dom (Maybe IPv4Address)
  type Bwd (ArpLookup dom) = Signal dom (Maybe ArpResponse)

{- |
Structure that contains enough information to construct an outgoing ARP packet,
given that we already have access to our own MAC- and IPv4 address.
-}
data ArpLite = ArpLite
  { _liteTha :: MacAddress
  -- ^ Target hardware address
  , _liteTpa :: IPv4Address
  -- ^ Target protocol address
  , _liteOper :: ArpOperation
  -- ^ Operation that the sender is performing
  }
  deriving (Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | All supported ARP operations (@OPER@). Only request or reply.
data ArpOperation = Request | Reply
  deriving (Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | ARP packet structure. The first four fields are constant for our use case.
data ArpPacket = ArpPacket
  { _htype :: BitVector 16
  -- ^ Hardware type. @0x0001@ for Ethernet
  , _ptype :: BitVector 16
  -- ^ Protocol type. @0x0800@ for IPv4
  , _hlen :: BitVector 8
  -- ^ Length of the hardware adresses. @0x06@ for Ethernet
  , _plen :: BitVector 8
  -- ^ Length of the protocol (internet) addresses. @0x04@ for IPv4
  , _oper :: BitVector 16
  -- ^ Operation that the sender is performing: @0x0001@ for request, @0x0002@ for reply
  , _sha :: MacAddress
  -- ^ Sender hardware address
  , _spa :: IPv4Address
  -- ^ Sender protocol address
  , _tha :: MacAddress
  -- ^ Target hardware address
  , _tpa :: IPv4Address
  -- ^ Target protocol address
  }
  deriving (Generic, Eq, Show, ShowX, NFDataX, NFData, BitPack)

-- | ARP's EtherType for multiplexing purposes.
arpEtherType :: BitVector 16
arpEtherType = 0x0806
{-# INLINE arpEtherType #-}

-- | Construct an IPv4 ARP packet.
newArpPacket ::
  -- | Our MAC address
  MacAddress ->
  -- | Our IPv4 address
  IPv4Address ->
  -- | Target MAC address
  MacAddress ->
  -- | Target IPv4 address
  IPv4Address ->
  -- | Operation to perform
  ArpOperation ->
  ArpPacket
newArpPacket myMac myIP targetMac targetIP operation =
  ArpPacket
    { _htype = 0x0001
    , _ptype = 0x0800
    , _hlen = 0x06
    , _plen = 0x04
    , _oper = case operation of
        Request -> 0x0001
        Reply -> 0x0002
    , _sha = myMac
    , _spa = myIP
    , _tha = targetMac
    , _tpa = targetIP
    }
{-# INLINE newArpPacket #-}

{- |
Whether an ARP packet is gratuitous. Such packets have @SPA@ equal to @TPA@.

This property is defined in
[IETF RFC 5227](https://datatracker.ietf.org/doc/html/rfc5227).
-}
isGratuitous :: ArpPacket -> Bool
isGratuitous ArpPacket{..} = _tpa == _spa
{-# INLINE isGratuitous #-}

{- |
Whether the sender of this ARP packet expects an ARP reply from us or not.
That is, only when the packet is a request directed to our IP address.

This property is defined in
[IETF RFC 826](https://datatracker.ietf.org/doc/html/rfc826).
-}
expectsReply ::
  -- | Our IPv4 address
  IPv4Address ->
  -- | Incoming ARP packet
  ArpPacket ->
  Bool
expectsReply ourIPv4 ArpPacket{..} = _tpa == ourIPv4 && _oper == 1
{-# INLINE expectsReply #-}

{- |
Returns @True@ if @HTYPE = 1@ (Ethernet), @PTYPE = 0x0800@ (IPv4) and the
incoming packet is either:

- A request or reply directed to us;
- A gratitious reply or request.

__NB__: @HLEN@ and @PLEN@ are not validated, as
[IETF RFC 826](https://datatracker.ietf.org/doc/html/rfc826) specifies this
as optional.
-}
isValidArp ::
  -- | Our IPv4 address
  IPv4Address ->
  -- | Incoming ARP packet
  ArpPacket ->
  Bool
isValidArp ourIPv4 pkt@ArpPacket{..} =
  _htype
    == 0x0001
    && _ptype
    == 0x0800
    && (_oper == 1 || _oper == 2)
    && (isGratuitous pkt || _tpa == ourIPv4)
{-# INLINE isValidArp #-}
