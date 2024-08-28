{-|
Module      : Clash.Cores.Ethernet.Mac.MacPacketizers
Description : Specialized packetizers for ethernet headers.
-}
module Clash.Cores.Ethernet.Mac.MacPacketizers
  ( macPacketizerC
  , macDepacketizerC
  ) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream

import Clash.Cores.Ethernet.Mac.EthernetTypes


-- | Prepends an `EthernetHeader` in the metadata to the packet stream, for each packet.
macPacketizerC ::
  forall dataWidth dom.
  HiddenClockResetEnable dom =>
  KnownNat dataWidth =>
  1 <= dataWidth =>
  Circuit
    (PacketStream dom dataWidth EthernetHeader)
    (PacketStream dom dataWidth ())
macPacketizerC = packetizerC (const ()) id

-- | Parses the first 14 bytes of the incoming PacketStream into an `EthernetHeader`.
macDepacketizerC ::
  forall dataWidth dom.
  HiddenClockResetEnable dom =>
  KnownNat dataWidth =>
  1 <= dataWidth =>
  Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth EthernetHeader)
macDepacketizerC = depacketizerC const
