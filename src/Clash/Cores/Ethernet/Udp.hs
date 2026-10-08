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
import Data.Maybe (fromMaybe, isJust)

import Protocols
import qualified Protocols.Df as Df
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
  -- ^ UDP checksum; zero indicates that no checksum was supplied
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
Parses the UDP header from an IPv4 stream and validates nonzero checksums,
including the IPv4 pseudo-header. Invalid checksums abort the packet. A zero
checksum is accepted as permitted by UDP over IPv4. The first element of the
output metadata is the source IPv4 address of incoming packets.

Invalid lengths are dropped, truncated payloads are aborted, and bytes beyond
the UDP length are removed. The length check and checksum pipeline add five
cycles to the latency of 'depacketizerC', where @headerBytes = 8@, while
maintaining one transfer per cycle.
-}
udpDepacketizerC ::
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth IPv4HeaderLite)
    (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
udpDepacketizerC =
  depacketizerC (,)
    |> filterMeta (\(udp, ip) -> _udpLength udp >= 8 && _udpLength udp <= _ipv4lPayloadLength ip)
    |> stripPaddingC (\(udp, _) -> _udpLength udp - 8)
    |> verifyUdpChecksumC
    |> mapMeta (\(udp, ip) -> (_ipv4lSource ip, toUdpLite udp))

-- | Validate UDP checksums with pipelined sums. A 32-bit accumulator avoids
-- overflow for maximum-size datagrams; carries are folded only at the output.
verifyUdpChecksumC ::
  forall dom dataWidth.
  (HiddenClockResetEnable dom, KnownNat dataWidth) =>
  Circuit
    (PacketStream dom dataWidth (UdpHeader, IPv4HeaderLite))
    (PacketStream dom dataWidth (UdpHeader, IPv4HeaderLite))
verifyUdpChecksumC =
  forceResetSanity
    |> Circuit (\(fwd, bwd) ->
         let (ack, out) = toSignals pipeline (fwd, Ack . _ready <$> bwd)
          in ((\(Ack ready) -> PacketStreamS2M ready) <$> ack, out))
    |> registerBoth
 where
  -- Df carries per-transfer sums without changing packet metadata.
  pipeline ::
    Circuit
      (Df.Df dom (PacketStreamM2S dataWidth (UdpHeader, IPv4HeaderLite)))
      (Df.Df dom (PacketStreamM2S dataWidth (UdpHeader, IPv4HeaderLite)))
  pipeline =
    fromSignals (mealyB prepare False)
      |> Df.registerFwd
      |> fromSignals (mealyB accumulate (True, 0))
      |> Df.registerFwd
      |> Df.map finish

  prepare oddByte (Nothing, bwd) = (oddByte, (bwd, Nothing))
  prepare oddByte (Just p, bwd@(Ack ready)) =
    (if ready then nextOdd else oddByte, (bwd, Just (p, headerSum, beatSum)))
   where
    (udp, ip) = _meta p
    pseudoHeader :: Vec 6 (BitVector 16)
    pseudoHeader =
      bitCoerce
        ( _ipv4lSource ip
        , _ipv4lDestination ip
        , 0 :: BitVector 8
        , _ipv4lProtocol ip
        , _udpLength udp
        )
    header :: Vec 4 (BitVector 16)
    header = bitCoerce udp
    headerSum :: Unsigned 20
    headerSum = fold (+) (map (zeroExtend . unpack) (pseudoHeader ++ header))
    -- Weight bytes by their position in a 16-bit network-order word. This
    -- also handles odd data widths and an odd final payload byte.
    word :: Index dataWidth -> BitVector 8 -> Unsigned 32
    word i b
      | resize i >= fromMaybe maxBound (_last p) = 0
      | even i /= oddByte = zeroExtend (unpack b) `shiftL` 8
      | otherwise = zeroExtend (unpack b)
    beatSum = fold (+) (0 :> imap word (_data p))
    nextOdd = not (isJust (_last p)) && (oddByte /= odd (natToNum @dataWidth :: Int))

  accumulate st (Nothing, bwd) = (st, (bwd, Nothing))
  accumulate st@(first, acc) (Just (p, headerSum, beatSum), bwd@(Ack ready)) =
    (if ready then nextSt else st, (bwd, Just (p, total)))
   where
    total :: Unsigned 32
    total = (if first then zeroExtend headerSum else acc) + beatSum
    nextSt = if isJust (_last p) then (True, 0) else (False, total)

  finish (p, total) = p{_abort = _abort p || invalid}
   where
    (udp, _) = _meta p
    (hi, lo) = bitCoerce total :: (Unsigned 16, Unsigned 16)
    folded :: Unsigned 17
    folded = add hi lo
    -- Both values fold to negative zero (0xffff) with end-around carry.
    valid = folded == 0xFFFF || folded == 0x1FFFE
    invalid = isJust (_last p) && _udpChecksum udp /= 0 && not valid

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
