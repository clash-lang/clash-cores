{-# LANGUAGE RecordWildCards #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides a circuit that responds to ICMP echo requests.
-}
module Clash.Cores.Ethernet.Icmp (
  IcmpHeader (..),
  IcmpHeaderLite (..),
  toIcmpLite,
  fromIcmpLite,
  icmpEchoResponderC,
) where

import Clash.Prelude

import qualified Data.Bifunctor as B

import Protocols (Circuit, (|>))
import Protocols.PacketStream

import Clash.Cores.Ethernet.IP.IPv4Types (IPv4Address (..), IPv4HeaderLite (..))
import Clash.Cores.Ethernet.InternetChecksum (onesComplementAdd)

-- | Full ICMP header.
data IcmpHeader = IcmpHeader
  { _type :: BitVector 8
  , _code :: BitVector 8
  , _checksum :: BitVector 16
  }
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX)

-- | Small ICMP header, which only contains the checksum.
newtype IcmpHeaderLite = IcmpHeaderLite
  {_checksumL :: BitVector 16}
  deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX)

-- | Create an ICMP echo reply header from an ICMP lite header.
fromIcmpLite :: IcmpHeaderLite -> IcmpHeader
fromIcmpLite IcmpHeaderLite{..} =
  IcmpHeader
    { _type = 0
    , _code = 0
    , _checksum = _checksumL
    }

-- | Drop all information except the checksum.
toIcmpLite :: IcmpHeader -> IcmpHeaderLite
toIcmpLite IcmpHeader{..} = IcmpHeaderLite{_checksumL = _checksum}

{- |
Prepends an ICMP echo reply header to the packet stream.
-}
icmpTransmitterC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth (IPv4HeaderLite, IcmpHeaderLite))
    (PacketStream dom dataWidth IPv4HeaderLite)
icmpTransmitterC = packetizerC fst (fromIcmpLite . snd)

{- |
Parses the first 4 bytes of the stream into an `IcmpHeader`, and verifies
whether the packet is an ICMP echo request (type 8 and code 0). Drops all
other packets. Only the checksum is forwarded in the metadata.
-}
icmpReceiverC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth IPv4HeaderLite)
    (PacketStream dom dataWidth (IPv4HeaderLite, IcmpHeaderLite))
icmpReceiverC =
  depacketizerC (\icmpHdr ipHdr -> (ipHdr, icmpHdr))
    |> filterMeta (\(_, hdr) -> _type hdr == 8 && _code hdr == 0)
    |> mapMeta (B.second toIcmpLite)

{- |
Responds to ICMP echo requests with an echo reply, and drops all other
packets. Assumes that all incoming packets are destined for us.

This circuit only changes the ICMP type of the input packet: from @8@
(Echo Request) to @0@ (Echo Reply). That means we can adjust the input checksum
instead of having to compute it from scratch. For example, if the checksum of
the input packet is @0xABCD@:

>>> import Clash.Prelude
>>> import Clash.Cores.Ethernet.IP.InternetChecksum (onesComplementAdd)

We adjust the checksum as specified by
[IETF RFC 1624](https://datatracker.ietf.org/doc/html/rfc1624):

>>> :{
adjustChecksum :: BitVector 16 -> BitVector 16
adjustChecksum c = complement $ onesComplementAdd (complement c) 0xF7FF
:}

>>> adjustChecksum 0xABCD
0b1011_0011_1100_1101

This method is unfortuntately not foolproof. If all input bytes are @0x00@
except the type and checksum, the ICMP packet will have the checksum @0xF7FF@
and we will adjust it to @0x0000@:

>>> adjustChecksum 0xF7FF
0b0000_0000_0000_0000

Recalculating it from scratch yields @0xFFFF@, and your operating system will
reject the packet because @0xFFFF@ is not @0x0000@. This is a limitation of one's
complement, because @0x0000@ and @0xFFFF@ both represent the number zero.
Because this case rarely happens in practice and because losing a single
echo reply packet is not a big deal, this should not be a problem.
-}
icmpEchoResponderC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- | Our IPv4 address
  Signal dom IPv4Address ->
  Circuit
    (PacketStream dom dataWidth IPv4HeaderLite)
    (PacketStream dom dataWidth IPv4HeaderLite)
icmpEchoResponderC ourIPv4S =
  icmpReceiverC
    |> mapMetaS (updateMeta <$> ourIPv4S)
    |> icmpTransmitterC
 where
  updateMeta ourIPv4 (ipv4, icmp) = (swapIP ipv4, icmp{_checksumL = newChecksum})
   where
    -- Destination might be a broadcast or multicast address,
    -- so we need to explicitly set the source IP to our IP.
    swapIP hdr@IPv4HeaderLite{..} =
      hdr
        { _ipv4lSource = ourIPv4
        , _ipv4lDestination = _ipv4lSource
        }

    newChecksum =
      complement $
        onesComplementAdd (complement $ _checksumL icmp) 0xF7FF
