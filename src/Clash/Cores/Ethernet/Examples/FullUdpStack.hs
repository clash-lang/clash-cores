{-# language FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{- |
Copyright   :  (C) 2024, Matthijs Muis
                   2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Here, we illustrate the use of of ARP, MAC and IP and UDP components for
construction of a fully featured UDP + ARP + ICMP + IP + MAC stack.
In short, this stack:

* Takes two input streams:

  ** A stream which contains packets from the application layer for transmission;
  ** A stream which contains raw Ethernet packets from the Ethernet RX PHY.

* Uses the Ethernet MAC RX block `macRxStack` to handle the MAC layer and
parts of the physical layer for raw Ethernet packets received from the RX PHY.
Refer to its documentation for a detailed description.

* Use `arpIcmpUdpStackC` (detailed in this example) to handle the upper layers:

  1. It routes packets received from the MAC RX block based on their EtherType
  metadata. Only packets with either ARP or IPv4 EtherTypes are kept.
  2. The ARP stack (`arpC`) handles incoming ARP packets and lookup requests
  from the IPv4 transmit layer. Refer to its documentation for a detailed
  description.
  3. The IPv4 receive layer verifies the checksum of incoming IPv4 packets and
  routes the payload based on the protocol field. Only UDP and ICMP packets are
  kept.
  3. For each received ICMP echo request, an ICMP echo reply is transmitted.
  4. Received UDP packets are passed to the application layer.

* Uses the Ethernet MAC TX block `macTxStack` to handle the MAC layer and
parts of the physical layer for packets to transmit to the TX PHY.
Refer to its documentation for a detailed description.

This example makes use of the @circuit-notation@ plugin, a GHC source plugin
providing a DSL for writing circuit components. See the examples at
`Protocols.Plugin`.

Let us begin with the implementation of `arpIcmpUdpStackC`, which has the
following inputs:

* Our MAC address.

* Our IPv4 address and subnet mask. These may be statically set, or dynamically
assigned via DHCP. @clash-cores@ does not provide hardware DHCP support yet.

* A stream which contains UDP packets from the application layer. The
application layer has to provide a destination IPv4 address and a UDP
header (without checksum) for each packet in the stream.

* A stream which contains arbitrary network-layer packets from the MAC RX block.

Now, we describe line by line, what the stack's implementation is doing. The
point of this is to illustrate how the various components and layers fit
together, so the description is quite abstract and general. Refer to the
documentation of each individual component for a more detailed description.

1. `packetDispatcherC` routes the stream from the MAC RX block into two
separate streams: @arpEthIn@ for ARP packets (EtherType @0x0806@) and
@ipEthIn@ for IPv4 packets (EtherType @0x0800@). Packets with any other
EtherType are dropped.

2. Processing of the incoming IPv4 stream is done by `ipDepacketizerLiteC`,
which parses the first 20 bytes of the stream into an `IPv4Header`. Furthermore,
it verifies the IPv4 checksum and various other fields, aborting packets upon
error. After that, any packets which are not either destined for us or a
broadcast are dropped.

3. `icmpUdpStackC` handles two streams:

  * Incoming UDP packets from the application layer (@udpOut@). It simply
  prepends the corresponding UDP header to each packet in the stream. The UDP
  checksum is not computed, as this is optional for UDP over IPv4. So, it is
  simply transmitted as all zeroes (@0x0000@). Output packets end up in the
  @ipOut@ port, as they need to be processed by the IPv4 transmit layer.

  * Incoming packets processed by the IPv4 receive layer (@ipIn@). These are
  once again routed into two streams, but this time based on the IPv4 Protocol
  field: a stream for ICMP packets (Protocol @0x01@) and a stream for UDP
  packets (Protocol @0x11@). Packets with other protocol numbers are dropped.
  The UDP packets (@udpIn@) is an output of the stack. The ICMP packets are
  processed by `icmpEchoResponderC`, which creates an ICMP echo reply packet
  for each incoming ICMP echo request. All other types of ICMP packets are
  dropped. Echo replies end up in the @ipOut@ port.

  * Because the IPv4 transmit layer has to handle both ICMP echo replies and
  outgoing UDP packets, `packetArbiterC` is necessary to safely merge the two
  streams into one without violating protocol rules.

4. Outgoing IPv4 packets are processed by prepending the IPv4 header to the
stream, including the computed checksum. This is done by `ipPacketizerLiteC`.
Before the processed IPv4 packets can be handed to the MAC TX block, we need
to map their destination IPv4 address to the corresponding MAC address.
`toEthernetStreamC` serves this purpose, using `arpC` for lookups. If no
mapping was found in time (depending on parameters of `arpC`), the packet is
dropped to avoid stalling the network stack forever.

5. Processing of incoming ARP packets is done by `arpC`. It will transmit ARP
replies in response to ARP requests, and ARP requests if `ethernetStreamC`
requests an IPv4 address that is not found in the ARP table.

6. The MAC TX block has to handle both ARP and IPv4 packets, so we need to
arbitrate the two streams again with `packetArbiterC`. It runs in `RoundRobin`
mode to avoid starvation of either stream.

To put it all together, `fullStackC` instantiates the MAC RX and TX blocks
and the IPv4 + ARP + ICMP + UDP stack described above to create a fully
functional network core. The UDP in- and output ports are buffered by
`packetFifoC`, which makes sure that aborted packets are dropped, packets
are delivered without gaps (interleaving @Nothing@s) and backpressure is
absorbed.

Lastly, we implement a UDP echo server. The user still has to provide the
physical layer to which this stack can interface. The Ethernet PHY is
completely interchangeable with this stack. In the example below, we use a
dummy. You have to replace these dummy variables with Ethernet PHY circuits
for your specific hardware (e.g. RGMII, MII or SGMII) that is adapted to the
`PacketStream` protocol, i.e. with type:

>>> :{
dummyTxPhy ::
  (HiddenClockResetEnable domEthTx) =>
  Circuit (PacketStream domEthTx 1 ()) (PacketStream domEthTx 1 ())
dummyTxPhy = undefined
:}

>>> :{
dummyRxPhy ::
  (HiddenClockResetEnable domEthRx) =>
  Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthRx 1 ())
dummyRxPhy = undefined
:}

To implement a UDP echo server, all we have to do is connect `fullStackC` to
the Ethernet PHY and loop the UDP output port back to the UDP input port,
while swapping the source and destination ports:

>>> import qualified Data.Bifunctor as B
>>> ourMacS = pure (MacAddress (repeat 0x00))
>>> ourIPv4 = IPv4Address (192 :> 168 :> 1 :> 1 :> Nil)
>>> ourSubnetMask = IPv4SubnetMask (255 :> 255 :> 255 :> 0 :> Nil)
>>> :{
$(deriveHardwareCrc Crc32_ethernet d8 d1)
udpEchoC ::
  forall dom domEthRx domEthTx.
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthRx) =>
  (KnownDomain domEthTx) =>
  Clock domEthRx ->
  Reset domEthRx ->
  Enable domEthRx ->
  Clock domEthTx ->
  Reset domEthTx ->
  Enable domEthTx ->
  Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
udpEchoC ethRxClk ethRxRst ethRxEn ethTxClk ethTxRst ethTxEn =
  circuit $ \phyRx -> do
    phyRx' <- exposeClockResetEnable dummyRxPhy ethRxClk ethRxRst ethRxEn -< phyRx
    udpIn <- mapMeta (B.second swapPortsL) -< udpOut
    (udpOut, toTxPhy) <-
      fullStackC
        @4
        @dom
        ethRxClk
        ethRxRst
        ethRxEn
        ethTxClk
        ethTxRst
        ethTxEn
        ourMacS
        (pure (ourIPv4, ourSubnetMask))
          -< (udpIn, phyRx')
    exposeClockResetEnable dummyTxPhy ethTxClk ethTxRst ethTxEn -< toTxPhy
:}

-}
module Clash.Cores.Ethernet.Examples.FullUdpStack (
  fullStackC,
  arpIcmpUdpStackC,
  icmpUdpStackC,
) where

import Clash.Cores.Crc (HardwareCrc)
import Clash.Cores.Crc.Catalog

import Clash.Cores.Ethernet.Arp
import Clash.Cores.Ethernet.Examples.RxStacks
import Clash.Cores.Ethernet.Examples.TxStacks
import Clash.Cores.Ethernet.Mac
import Clash.Cores.Ethernet.IPv4
import Clash.Cores.Ethernet.Icmp (icmpEchoResponderC)
import Clash.Cores.Ethernet.Udp

import Clash.Prelude

import Protocols
import Protocols.PacketStream

-- | Full stack from ethernet to ethernet.
fullStackC ::
  forall
    (dataWidth :: Nat)
    (dom :: Domain)
    (domEthRx :: Domain)
    (domEthTx :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthRx) =>
  (KnownDomain domEthTx) =>
  (HardwareCrc Crc32_ethernet 8 1) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Clock domEthRx ->
  Reset domEthRx ->
  Enable domEthRx ->
  Clock domEthTx ->
  Reset domEthTx ->
  Enable domEthTx ->
  -- | Our MAC address
  Signal dom MacAddress ->
  -- | (Our IPv4 address, Our subnet mask)
  Signal dom (IPv4Address, IPv4SubnetMask) ->
  -- | Input: (Packets from application layer, Packets from MAC RX Stack)
  --
  --   Output: (Packets to application layer, Packets to MAC TX stack)
  Circuit
    ( PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)
    , PacketStream domEthRx 1 ()
    )
    ( PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)
    , PacketStream domEthTx 1 ()
    )
fullStackC rxClk rxRst rxEn txClk txRst txEn macS ipS = circuit $ \(udpOut, phyIn) -> do
  ethIn <- macRxStack @dataWidth rxClk rxRst rxEn macS -< phyIn
  udpOutBuffered <- packetFifoC d10 d4 Backpressure -< udpOut
  (udpIn, ethOut) <- arpIcmpUdpStackC macS ipS -< (udpOutBuffered, ethIn)
  udpInBuffered <- packetFifoC d10 d4 Backpressure -< udpIn
  phyOut <- macTxStack txClk txRst txEn -< ethOut
  idC -< (udpInBuffered, phyOut)

-- | Wraps a circuit that handles UDP packets into a stack that handles IP, ICMP
-- and ARP.
arpIcmpUdpStackC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- | Our MAC address
  Signal dom MacAddress ->
  -- | (Our IPv4 address, Our subnet mask)
  Signal dom (IPv4Address, IPv4SubnetMask) ->
  -- | Input: (Packets from application layer, Packets from MAC RX Stack)
  --
  --   Output: (Packets to application layer, Packets to MAC TX stack)
  Circuit
    ( PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)
    , PacketStream dom dataWidth EthernetHeader
    )
    ( PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)
    , PacketStream dom dataWidth EthernetHeader
    )
arpIcmpUdpStackC ourMacS ipS = circuit $ \(udpOut, ethIn) -> do
  [arpEthIn, ipEthIn] <- packetDispatcherC (routeBy _etherType $ 0x0806 :> 0x0800 :> Nil) -< ethIn

  arpEthOut <- arpC d300 d500 d6 ourMacS (fst <$> ipS) -< (arpEthIn, arpLookup)
  ipIn <- filterMetaS (isForMyIp <$> ipS) <| ipDepacketizerLiteC -< ipEthIn
  (udpIn, ipOut) <- icmpUdpStackC ipS -< (udpOut, ipIn)
  (ipEthOut, arpLookup) <- toEthernetStreamC ourMacS <| ipLitePacketizerC -< ipOut
  ethOut <- packetArbiterC RoundRobin -< [arpEthOut, ipEthOut]
  idC -< (udpIn, ethOut)
 where
  isForMyIp (ip, subnet) (_ipv4lDestination -> to) = to == ip || isBroadcast subnet to

icmpUdpStackC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- | (Our IPv4 address, Our subnet mask)
  Signal dom (IPv4Address, IPv4SubnetMask) ->
  -- | Input: (Packets from application layer, Packets from IP RX Stack)
  --
  --   Output: (Packets to application layer, Packets to IP TX stack)
  Circuit
    ( PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)
    , PacketStream dom dataWidth IPv4HeaderLite
    )
    ( PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)
    , PacketStream dom dataWidth IPv4HeaderLite
    )
icmpUdpStackC ipS = circuit $ \(udpOut, ipIn) -> do
  [icmpIn, udpIn] <- packetDispatcherC (routeBy _ipv4lProtocol $ 0x01 :> 0x11 :> Nil) -< ipIn
  icmpOut <- icmpEchoResponderC (fst <$> ipS) -< icmpIn
  udpInParsed <- udpDepacketizerC -< udpIn
  udpOutParsed <- udpPacketizerC (fst <$> ipS) -< udpOut
  ipOut <- packetArbiterC RoundRobin -< [icmpOut, udpOutParsed]
  idC -< (udpInParsed, ipOut)
