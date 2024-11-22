{-# LANGUAGE RecordWildCards #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides circuits and data types to handle the User Datagram Protocol (UDP)
over IPv4, as specified in
[IETF RFC 768](https://datatracker.ietf.org/doc/html/rfc768).
-}
module Clash.Cores.Ethernet.Udp (
  -- * Data types
  UdpHeader (..),
  UdpHeaderLite (..),

  -- * Port swapping
  swapPorts,
  swapPortsL,

  -- * (De)packetization
  udpDepacketizerC,
  udpPacketizerC,
) where

import Clash.Cores.Ethernet.IP.IPv4Types

import Clash.Prelude

import Control.DeepSeq (NFData)

import Protocols
import Protocols.PacketStream

{- |
Full UDP header as defined in
[IETF RFC 768](https://datatracker.ietf.org/doc/html/rfc768).
-}
data UdpHeader = UdpHeader
  { _udpSrcPort :: Unsigned 16
  -- ^ Source port
  , _udpDstPort :: Unsigned 16
  -- ^ Destination port
  , _udpLength :: Unsigned 16
  -- ^ Length of header + payload
  , _udpChecksum :: Unsigned 16
  -- ^ UDP Checksum, we do not validate or generate it
  }
  deriving (BitPack, Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | UDP header without checksum.
data UdpHeaderLite = UdpHeaderLite
  { _udplSrcPort :: Unsigned 16
  -- ^ Source port
  , _udplDstPort :: Unsigned 16
  -- ^ Destination port
  , _udplPayloadLength :: Unsigned 16
  -- ^ Length of payload
  }
  deriving (BitPack, Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | Create a full header from a partial one, by setting the checksum to @0@.
fromUdpLite :: UdpHeaderLite -> UdpHeader
fromUdpLite UdpHeaderLite{..} =
  UdpHeader
    { _udpSrcPort = _udplSrcPort
    , _udpDstPort = _udplDstPort
    , _udpLength = _udplPayloadLength + 8
    , _udpChecksum = 0
    }
{-# INLINE fromUdpLite #-}

-- | Create a partial header from a full one, by dropping the checksum.
toUdpLite :: UdpHeader -> UdpHeaderLite
toUdpLite UdpHeader{..} =
  UdpHeaderLite
    { _udplSrcPort = _udpSrcPort
    , _udplDstPort = _udpDstPort
    , _udplPayloadLength = _udpLength - 8
    }
{-# INLINE toUdpLite #-}

-- | Swap the source and destination ports in a UDP lite header.
swapPortsL :: UdpHeaderLite -> UdpHeaderLite
swapPortsL hdr@UdpHeaderLite{..} =
  hdr
    { _udplSrcPort = _udplDstPort
    , _udplDstPort = _udplSrcPort
    }
{-# INLINE swapPortsL #-}

-- | Swap the source and destination ports in a UDP header.
swapPorts :: UdpHeader -> UdpHeader
swapPorts hdr@UdpHeader{..} =
  hdr
    { _udpSrcPort = _udpDstPort
    , _udpDstPort = _udpSrcPort
    }
{-# INLINE swapPorts #-}

{- |
Parses out the full UDP header from an IPv4 stream, but immediately drops the
checksum without validating it. The first element of the output metadata is
the source IPv4 address of incoming packets.

Inherits latency and throughput from 'depacketizerC', where @headerBytes = 8@.
-}
udpDepacketizerC ::
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth IPv4HeaderLite)
    (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
udpDepacketizerC =
  depacketizerC (\udph ipv4lh -> (_ipv4lSource ipv4lh, toUdpLite udph))

{- |
Serializes UDP headers to an IPv4 stream. The first element of the metadata
is the destination IP for outgoing packets. No checksum is included in the
UDP header.

Inherits latency and throughput from 'packetizerC', where @headerBytes = 8@.
-}
udpPacketizerC ::
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- | Source IPv4 address
  Signal dom IPv4Address ->
  Circuit
    (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
    (PacketStream dom dataWidth IPv4HeaderLite)
udpPacketizerC myIp = mapMetaS (toIp <$> myIp) |> packetizerC fst snd
 where
  toIp srcIp (dstIp, udpLite) = (ipLite, udpHeader)
   where
    udpHeader = fromUdpLite udpLite
    ipLite =
      IPv4HeaderLite
        { _ipv4lSource = srcIp
        , _ipv4lDestination = dstIp
        , _ipv4lProtocol = 0x11
        , _ipv4lPayloadLength = _udpLength udpHeader
        }
