{-# OPTIONS_HADDOCK hide #-}

{-|
Module      : Clash.Cores.Ethernet.Mac.MacPacketizers
Description : Specialized (de)packetizers for Ethernet headers.
-}
module Clash.Cores.Ethernet.Mac.MacPacketizers (
  macPacketizerC,
  macDepacketizerC,
) where

import Clash.Prelude

import Protocols (Circuit)
import Protocols.PacketStream (depacketizerC, packetizerC, PacketStream)

import Clash.Cores.Ethernet.Mac.EthernetTypes (EthernetHeader)

{- |
Prepends the `EthernetHeader` in the metadata to the packet stream,
for each packet.

Inherits latency and throughput from `packetizerC`.
-}
macPacketizerC ::
  forall dataWidth dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth EthernetHeader)
    (PacketStream dom dataWidth ())
macPacketizerC = packetizerC (const ()) id

{- |
Parses the first 14 bytes of each packet in the incoming packet stream into an
`EthernetHeader`, puts that in the metadata of the packet and strips those
bytes from the stream.

Inherits latency and throughput from `depacketizerC`.
-}
macDepacketizerC ::
  forall dataWidth dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth EthernetHeader)
macDepacketizerC = depacketizerC const
