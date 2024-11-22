{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Specialized (de)packetizers for Ethernet headers.
-}
module Clash.Cores.Ethernet.Mac.MacPacketizers (
  macPacketizerC,
  macDepacketizerC,
) where

import Clash.Prelude

import Protocols (Circuit)
import Protocols.PacketStream (PacketStream, depacketizerC, packetizerC)

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
