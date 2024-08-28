{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.Icmp
Description : Top level ICMP module.
-}
module Clash.Cores.Ethernet.Icmp
  ( IcmpHeader(..)
  , IcmpHeaderLite(..)
  , toIcmpLite
  , fromIcmpLite
  , icmpEchoResponderC
  ) where

import Clash.Prelude
import Data.Bifunctor ( second )

import Protocols ( Circuit, (|>) )
import Protocols.PacketStream

import Clash.Cores.Ethernet.IP.InternetChecksum ( onesComplementAdd )
import Clash.Cores.Ethernet.IP.IPv4Types ( IPv4Address, IPv4HeaderLite(..) )


-- | Full ICMP header
data IcmpHeader = IcmpHeader
  { _type :: BitVector 8
  , _code :: BitVector 8
  , _checksum :: BitVector 16
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX)

-- | Small ICMP header with only the type
data IcmpHeaderLite = IcmpHeaderLite
  { _checksumL :: BitVector 16
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX)

-- | Convert lite header to full header
fromIcmpLite :: IcmpHeaderLite -> IcmpHeader
fromIcmpLite IcmpHeaderLite{..} = IcmpHeader
  { _type = 0
  , _code = 0
  , _checksum = _checksumL
  }

-- | Convert full header to lite header
toIcmpLite :: IcmpHeader -> IcmpHeaderLite
toIcmpLite IcmpHeader{..} = IcmpHeaderLite { _checksumL = _checksum }

icmpEchoResponderC ::
  forall (dom :: Domain) (dataWidth :: Nat).
  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Signal dom IPv4Address
  -> Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream dom dataWidth IPv4HeaderLite)
icmpEchoResponderC ourIP = icmpReceiverC
                             |> mapMetaS (updateMeta <$> ourIP)
                             |> icmpTransmitterC

updateMeta ::
  IPv4Address
  -> (IPv4HeaderLite, IcmpHeaderLite)
  -> (IPv4HeaderLite, IcmpHeaderLite)
updateMeta ip (ipv4, icmp) = (adjustIP ip ipv4, adjustIcmp icmp)

adjustIP :: IPv4Address -> IPv4HeaderLite -> IPv4HeaderLite
adjustIP ip hdr@IPv4HeaderLite {..} = hdr {
  _ipv4lSource = ip
  , _ipv4lDestination = _ipv4lSource
}

adjustIcmp :: IcmpHeaderLite -> IcmpHeaderLite
adjustIcmp hdr = hdr { _checksumL = newChecksum }
  where
    newChecksum = complement $ onesComplementAdd (complement $ _checksumL hdr) 0xf7ff

icmpTransmitterC ::
  forall (dom::Domain) (n::Nat).
  HiddenClockResetEnable dom
  => KnownNat n
  => 1 <= n
  => Circuit (PacketStream dom n (IPv4HeaderLite, IcmpHeaderLite)) (PacketStream dom n IPv4HeaderLite)
icmpTransmitterC = packetizerC fst (fromIcmpLite . snd)

-- | A circuit that parses an ICMP Header and output an IcmpHeaderLite
icmpReceiverC :: forall (dom :: Domain) (dataWidth :: Nat).
  ( KnownDomain dom
  , HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 1 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream dom dataWidth (IPv4HeaderLite, IcmpHeaderLite))
icmpReceiverC = depacketizerC (\icmpHdr ipHdr -> (ipHdr, icmpHdr))
                  |> filterMeta (\(_, hdr) -> _type hdr == 8 && _code hdr == 0)
                  |> mapMeta (second toIcmpLite)
