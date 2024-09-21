{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{- |
Copyright   : (C) 2024, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Provides a top-level ARP circuit sufficient for most use cases, along with
the individual components it is composed of.
-}
module Clash.Cores.Ethernet.Arp (
  -- * Types, constants and simple operations
  module Clash.Cores.Ethernet.Arp.ArpTypes,

  -- * Top-level ARP stack
  arpC,

  -- * Individual components
  module Clash.Cores.Ethernet.Arp.ArpTable,
  module Clash.Cores.Ethernet.Arp.ArpManager,
) where

import Clash.Prelude

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream

import Clash.Cores.Ethernet.Arp.ArpManager
import Clash.Cores.Ethernet.Arp.ArpTable
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

{- |
A fully functional ARP stack which handles ARP lookups from client circuits.
Maintains a single-entry ARP table which the client circuit can query via the
`ArpLookup` input. If the client-supplied IPv4 address is not found in the table,
it transmits an ARP request for this specific address. The circuit will assert
backpressure until either a reply has been received, or a timeout occurs. The
maximum number of seconds the stack will wait for a reply to this request is
configurable. The timeout (in seconds) of ARP table entries is configurable as well.
All timeouts may be up to a second inaccurate.

Moreover, it takes in an Ethernet stream with the ARP
etherType (0x0806), and updates the ARP table upon receiving a valid ARP
reply or gratuitous ARP packet.
If an ARP request directed to our IPv4 address is received, it transmits a reply.
Outbound requests receive priority over outbound replies in the output stream.

__NB__: does not support Proxy ARP.
-}
arpC ::
  forall
    (dataWidth :: Nat)
    (dom :: Domain)
    (maxAgeSeconds :: Nat)
    (maxWaitMs :: Nat)
    (tableDepth :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (1 <= tableDepth) =>
  -- | Entries are evicted from the ARP table this many seconds after being inserted
  SNat maxAgeSeconds ->
  -- | The maximum amount of milliseconds to wait for an incoming ARP reply
  SNat maxWaitMs ->
  -- | The ARP table will contain @2^depth@ entries
  SNat tableDepth ->
  -- | Our MAC address
  Signal dom MacAddress ->
  -- | Our IPv4 address
  Signal dom IPv4Address ->
  Circuit
    (PacketStream dom dataWidth EthernetHeader, ArpLookup dom)
    (PacketStream dom dataWidth EthernetHeader)
arpC maxAge maxWaitMs tableDepth ourMacS ourIPv4S =
  circuit $ \(ethStream, lookupIn) -> do
    -- Add a skid buffer to improve timing. We don't need the metadata, so we
    -- can throw it away.
    bufferedStream <- mapMeta (const ()) |> registerBoth -< ethStream
    (entry, replyOut) <- arpReceiverC ourIPv4S -< bufferedStream
    (lookupOut, requestOut) <- arpManagerC maxWaitMs -< lookupIn
    () <- arpTableC tableDepth maxAge -< (lookupOut, entry)
    -- Being biased towards outbound requests is favourable, as it
    -- lessens the impact of ARP request DoS attacks. Moreover,
    -- @CollectMode@ @Df.Parallel@ is not always more expensive
    -- than @Df.Skip@ with two sources. Under certain circumstances
    -- it may be cheaper.
    arpPktOut <- Df.roundrobinCollect Df.Parallel -< [replyOut, requestOut]
    arpStreamOut <- arpTransmitterC ourMacS ourIPv4S |> registerBoth -< arpPktOut
    mapMetaS ((\src dst -> EthernetHeader dst src arpEtherType) <$> ourMacS)
      -< arpStreamOut
