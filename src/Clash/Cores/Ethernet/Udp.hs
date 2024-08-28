{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Udp
Description : Circuits and data types to handle the UDP protocol
-}
module Clash.Cores.Ethernet.Udp
  ( UdpHeaderLite(..)
  , udpDepacketizerC
  , udpPacketizerC
  )
  where

import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Prelude
import Protocols
import Protocols.PacketStream

-- | The full UDP header
data UdpHeader = UdpHeader
  { _udpSrcPort :: Unsigned 16
  -- ^ Source port
  , _udpDstPort :: Unsigned 16
  -- ^ Destination port
  , _udpLength :: Unsigned 16
  -- ^ length of header + payload
  , _udpChecksum :: Unsigned 16
  -- ^ UDP Checksum, We do not validate or generate it
  } deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX)

-- | UDP header
data UdpHeaderLite = UdpHeaderLite
  { _udplSrcPort :: Unsigned 16
  -- ^ Source port
  , _udplDstPort :: Unsigned 16
  -- ^ Destination port
  , _udplPayloadLength :: Unsigned 16
  -- ^ Length of payload
  } deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX)

fromUdpLite :: UdpHeaderLite -> UdpHeader
fromUdpLite UdpHeaderLite{..} = UdpHeader
  { _udpSrcPort = _udplSrcPort
  , _udpDstPort = _udplDstPort
  , _udpLength = _udplPayloadLength + 8
  , _udpChecksum = 0
  }

toUdpLite :: UdpHeader -> UdpHeaderLite
toUdpLite UdpHeader{..} = UdpHeaderLite
  { _udplSrcPort = _udpSrcPort
  , _udplDstPort = _udpDstPort
  , _udplPayloadLength = _udpLength - 8
  }

-- | Parses out the UDP header from an IP stream, but ignores the checksum.
-- The first element of the metadata is the source IP of incoming packets.
udpDepacketizerC
  :: HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => Circuit
       (PacketStream dom n IPv4HeaderLite)
       (PacketStream dom n (IPv4Address, UdpHeaderLite))
udpDepacketizerC = depacketizerC (\udph ipv4lh -> (_ipv4lSource ipv4lh, toUdpLite udph))

-- | Serializes the UDP packet to an IP stream. The first element of the metadata
-- is the destination IP for outgoing packets. No checksum is included in the UDP header.
udpPacketizerC
  :: HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => Signal dom IPv4Address
  -- ^ Source IP address
  -> Circuit
      (PacketStream dom n (IPv4Address, UdpHeaderLite))
      (PacketStream dom n IPv4HeaderLite)
udpPacketizerC myIp = mapMetaS (toIp <$> myIp) |> packetizerC fst snd
  where
    toIp srcIp (dstIp, udpLite) = (ipLite, udpHeader)
      where
        udpHeader = fromUdpLite udpLite
        ipLite = IPv4HeaderLite
                   { _ipv4lSource = srcIp
                   , _ipv4lDestination = dstIp
                   , _ipv4lProtocol = 0x0011
                   , _ipv4lPayloadLength = _udpLength udpHeader
                   }
