{-# language FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{- |
Copyright   :  (C) 2024, Matthijs Muis
                   2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Here, we illustrate the use of of ARP, MAC and IP and UDP components for
construction of a fully featured UDP + ARP + ICMP + IP + MAC stack. In short, this stack:

* Takes in a stream of ethernet frames.

* Uses `macRxStack` to  convert from a @dataWidth@ 1 to a @datawidth@ of 4 bytes, change from the ethernet clock domain to the chip's clock domain, strip the ethernet preamble, validate and strip the frame check sequence, depacketize the `EthernetHeader` into the `_meta` and filter out only frames destined for our MAC address or the broadcast MAC of our subnet (see the `macRxStack` example for a more detailed explanation).

* Has the `arpIcmpUdpStackC` (detailed in this example), which:

    1. Separates ARP payloads from IP payloads;

    2. Handles ARP (send replies to requests, create ARP table entries upon replies, respond to lookups from other stack components) using `arpC`;

    3. Responds to ICMP echo requests with echo replies;

    4. Can be passed an arbitrary circuit that handles UDP input and makes UDP output. In `fullStackC`, the circuit passed swaps the source and destination port of UDP segments and the source and destination IP address of their containing IP packets, creating a simple loopback for UDP.

* Finally, `arpIcmpUdpStackC` uses `macTxStack` to packetize the `EthernetHeader` `_data`, insert padding, the frame check sequence and the preamble, convert from the chip's clock domain to the ethernet clock domain, convert from a @dataWidth@ of 4 bytes to 1 byte and validate insert the interpacket gap (see the `macTxStack` example for a more detailed explanation).

This example makes use of the circuit-notation plugin, a GHC source plugin providing a DSL for writing circuit components. See the examples at `Protocols.Plugin`.

Let us begin with the implementation of `arpIcmpUdpStackC`:

>>> :{
arpIcmpUdpStackC
  :: forall (dataWidth :: Nat) (dom :: Domain)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => KnownNat (DomainPeriod dom)
  => Signal dom MacAddress
  -- ^ My MAC Address
  -> Signal dom (IPv4Address, IPv4Address)
  -- ^ My IP address and the subnet
  -> Circuit (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)) (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
  -- ^ UDP handler circuit
  -> Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream dom dataWidth EthernetHeader)
arpIcmpUdpStackC macAddressS ipS udpCkt = circuit $ \ethIn -> do
  [arpEthIn, ipEthIn] <- packetDispatcherC (routeBy _etherType $ 0x0806 :> 0x0800 :> Nil) -< ethIn
  ipTx <- ipLitePacketizerC <| packetBufferC d10 d4 <| icmpUdpStack <| packetBufferC d10 d4 <| filterMetaS (isForMyIp <$> ipS) <| ipDepacketizerLiteC -< ipEthIn
  (ipEthOut, arpLookup) <- toEthernetStreamC macAddressS -< ipTx
  arpEthOut <- arpC d10 d5 macAddressS (fst <$> ipS) -< (arpEthIn, arpLookup)
  packetArbiterC RoundRobin -< [arpEthOut, ipEthOut]
  where
    icmpUdpStack = circuit $ \ipIn -> do
      [icmpIn, udpIn] <- packetDispatcherC (routeBy _ipv4lProtocol $ 0x0001 :> 0x0011 :> Nil) -< ipIn
      icmpOut <- icmpEchoResponderC @dom @dataWidth (fst <$> ipS) -< icmpIn
      udpInParsed <- udpDepacketizerC -< udpIn
      udpOutParsed <- udpPacketizerC (fst <$> ipS) <| udpCkt -< udpInParsed
      packetArbiterC RoundRobin -< [icmpOut, udpOutParsed]
    isForMyIp (ip, subnet) (_ipv4lDestination -> to) = to == ip || to == ipv4Broadcast ip subnet
}:

`arpIcmpUdpStackC` takes in:

* A `Signal` `MacAddress`, our MAC address. `MacAddress` is simply a wrapper type around a @Vec 6 (BitVector 8)@, a six-byte word.

* A @Signal(IPv4Address,IPv4Address)@ that represents a pair of /my IP address/ and the /subnet mask/ (an `IPv4Address` where the subnet prefix bits are set to 1). Like with `MacAddress`, `IPv4Address` is a wrapper type around a @Vec 4 (BitVector 8)@.

* A circuit that handles UPD packets. It has type @Circuit (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)) (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))@. The circuit takes in the UDP payload from the @data@ field of the packet stream, gets to see the source `IPv4Address` and a lite version of the UDP header (`UdpHeaderLite`) and can output the UDP packet (the payload in the @_data@ of the packet stream and the `UdpHeaderLite` and `IPv4Address` of the destination socket).

Line by line, the stack's implementation does the following:

1. First, it uses `packetDispatcherC` to split the @ethIn@ packet stream into separate streams of ARP packets and IP packets; `packetDispatcherC` takes a vector of predicates @Vec p (a -> Bool)@. It returns a @Vec p (PacketStream dom n a)@ of separated packet streams, and sends an incoming packet to the first matching entry. The predicate vector is constructed using a function `routeBy`, which makes a common design pattern much simpler to implement: usually, `_meta` determines how the packet should be dispatched: in this case @EthernetHeader@ of ethernet frames with an ARP payload has `_etherType` @0x0806@, while frames with IPv4 payload have `_etherType` @0x0800@. `routeBy` facilitates precisely this: it takes a "retrieving" function @f :: meta -> a@ and a vector @Vec a@ of values and returns a vector of predicates, ready to be passed to `packetDispatcherC`. For the exact type, we refer to `routeBy`. We get a vector of streams: @[arpEthIn, ipEthIn]@.

2. The @ipEthIn@ port is connected to the input of `ipDepacketizerLiteC`, which is a convenience composition of the `ipDepacketizerC` with `toLiteC`: it takes a @PacketStream dom n EthernetHeader@ and outputs a @PacketStream dom n IPv4HeaderLite@ by: first, stripping off the EthernetHeader (done in `ipDepacketizerC`), second extracting the `Ipv4Header` in the `_meta` to an `IPv4HeaderLite` (done in `toLiteC`). For the details, see `ipDepacketizerC` and the associated module.

3. Using a locally defined predicate `isForMyIp`, we filter out all packets that do not have either the subnet's broadcast @IPv4Address@ or my @IPv4Address@ as `_ipv4lDestination` field in the `_meta` field.

4. This is then passed to the `icmpUdpStack`, which is preceded and followed by a packet buffer (`packetBufferC`). a circuit which stores words in a buffer until the packet is complete once a packet is complete it will send the entire packet out at once without stalls. If a word in a packet has `_abort` set to true, the packetBuffer will drop the entire packet.

6. These packet buffers have a depth of 2^10 data bits and a depth of 2^4 meta bits. Since one packet may consist of many fragments, which by definition must have the same metadata, but have different content, we buffer metadata separately from content, reducing redundancy.

7. The reason for using packet buffers is because they absorb some backpressure generated by the @ipTx@ port  and the `icmpUdpStack`. In particular, the `toEthernetStreamC` asserts backpressure until `arpC` has given an `ArpResponse` to its `ArpLookup`.

8. From `packetBufferC`, the stream is passed to `ipLitePacketizerC`, which is another convenience composition, now of the components `fromLiteC` and `ipPacketizerC`. `fromLiteC` simply produces from an `IPv4HeaderLite` in the `_meta` of the packet stream, an `IPv4Header`. `ipPacketizerC` packetizes the `IPv4Header` into the `_data` of the packet stream, and puts the destination `IPv4Address` into the `_meta` field.

9. This is convenient since the following ARP lookup service can take the required IP address directly from the `_meta`.

10. The `toEthernetStreamC` reads the destination `IPv4Address` and sends an ARP lookup to `arpC` and, if it gets an `ArpEntryFound` from `arpC` (this information flows to `toEthernetStreamC` via the backward line of the arplookup port), it constructs an `EthernetHeader` with the source and target MAC fields set to our MAC address and the found MAC address respectively, and the appropriate ethertype for IPv4.

11. Finally, a `packetArbiterC` running in `RoundRobin` mode switches in a round-robin fashion between outputting a packet from the @arpEthOut stream and the @ipEthOut stream, i.e.
   it alternatingly will output a complete packet coming from `toEthernetC` and a complete packet from `icmpUdpPacket`.

All code thus far described handles interfacing between the MAC and IP layer. `icmpUdpStack` handles the payload of the IP packets, implementing two functionalities:

* Sending an appropriate ICMP echo response to an ICMP echo request.

* packetizing, handling and packetizing UDP traffic with the provided UPD handler circuit. `icmpUdpStack`, line by line, does the following:

1. The `packetDispatcher` + `routeBy` combination is now used to splite the input stream of type `PacketStream dom 4 IPv4HeaderLite` based on the protocol in the payload. The protocol number in the `IPv4Header` is also present iin `IPv4HeaderLite`, namely the field `_ipv4lProtocol`.

2. The stream `icmpIn` of ICMP packets is passed to `icmpEchoResponderC`. This circuit takes in a `Signal IPv4Adress` namely our IP, to internally:

* Parses the ICMP header from the content of the packet using the generic depacketizer and puts this into the `_meta`, in a tuple with the already present `IPv4HeaderLite`


* Updates simultaneously the IPv4HeaderLite and the IcmpHeader by

* In the IP header, putting the source of the IP packet into the destination

* In the IP header, putting our own IP address into the destination of the ICMP packet.

* In the ICMP header, updating the checksum according to the new type + code combination: we go from @(8,0)@ (echo request) to @(0,0)@ (echo response) and this requires a slight update of the checksum. As said, the `icmpEchoResponderC` takes a signal of our IP, so we need to extract the @fst@ of the input signal with @fst <$>@.

3. The UDP stream is parsed using `udpDepacketizerC`, which returns a `PacketStream dom dataWidth (IPv4Address, UdpHeaderLite)`: it puts the parsed UDP header in a tuple:

* the `IPv4Address` in the tuple is the source address of the IPv4Packet.

* The `UdpHeaderLite` records the source port, destination port and payload length. All are of type `Unsigned 16`.

4. The parsed UDP stream is then handled by the passed circuit.

5. The handled UDP stream is then packetized again by `udpPacketizerC: this packetizer takes in a signal with our IP address and gives a circuit of type @Circuit (PacketStream dom n (IPv4Address, UdpHeaderLite)) (PacketStream dom n IPv4HeaderLite)@. It turns the `UdpHeaderLite` header into a proper `UdpHeader` by setting all 3 mandatory fields (source port, destination port, length) with the fields provided by `UdpHeaderLite`, and puts @0x0000@ in the checksum field of the UDP header. In IPv4, the checksum field for UDP is optional, and is ignored when set to @0x0000@, so this will not cause trouble for the acceptance of the segments. `udpPacketizerC` then deparses this UDP header into the `_data` of the packet stream, and puts the `IPv4Address` of the input stream into the destination, while putting our signalled `IPv4Address` as the source.

6. Finally, `icmpUdpStack` uses another `RoundRobin`-set `packetArbiterC` to provide time division between the `icmpOut` and `udpOutParsed` streams.


`arpIcmpUdpStackC` already handles a lot of things you want your typical network stack to handle: it takes care of ARP, replies to ICMP echo requests and can handle UDP segments in any way that the user wishes.

To complete the example, we will show how this stack can snugly be fitted into a full, ethernet domain - to - ethernet domain stack. `fullStackC` provides a complete and concrete usage of `arpIcmpUdpStackC`: it passes to `arpIcmpUdpStackC` an ad-hoc circuit that simply swaps the source and destination of the `UdpHeaderLite`, making it echo back UDP packets to the original source port. It puts this instantiation of `arpIcmpUdpStack` between Tx and Rx stacks handling Ethernet. Because mind, that `arpIcmpUdpStackC still assumes that the `EthernetHeader` was already parsed from the frame, the clock domain conversion has happened, and the Frame Check Sequence was checked, etc.

>>>:{
fullStackC
  :: forall
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => HardwareCrc Crc32_ethernet 8 4
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => KnownNat (DomainPeriod dom)
  => HiddenClockResetEnable dom
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Signal dom MacAddress
  -- ^ My mac address
  -> Signal dom (IPv4Address, IPv4Address)
  -- ^ Tuple of my IP and subnet mask
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
fullStackC rxClk rxRst rxEn txClk txRst txEn mac ip =
  macRxStack @4 rxClk rxRst rxEn mac
  |> arpIcmpUdpStackC mac ip (mapMeta $ B.second swapPorts)
  |> macTxStack txClk txRst txEn
  where
    swapPorts hdr@UdpHeaderLite{..} = hdr
                                        { _udplSrcPort = _udplDstPort
                                        , _udplDstPort = _udplSrcPort
                                        }
}:

The above stack is complete in the sense that it can take input from a @dataWidth@ 1 packet stream @PacketStream domEthRx 1 ()@ in the clock domain of Ethernet and also produces output in this type and domain.
In other words, it takes in completely serialized ethernet frames and outputs completely serialized ethernet frames. The depacketizing and packetizing
and clock domain conversion is handled by `macRxStack` and `macTxStack`. A type annotation @\@4@ for `macRxStack` is necessary, since the compiler can deduce nowhere that `arpIcmpUdpStack` will use @dataWidth 4.

This stack thus completely handles the Ethernet layer and above. The user should still provide the physical layer to which this stack can interface, and this
depends on the hardware primitives provided by the particular hardware where this stack is used.
In the example below, we use a dummy. You have to replace this dummy variable with an Ethernet TX PHY circuit
for your specific hardware (e.g. RGMII, MII or SGMII) that is adapted to the
PacketStream` protocol, i.e. with type:

>>> :{
dummyTxPhy
  :: HiddenClockResetEnable domEthTx
  => Circuit (PacketStream domEthTx 1 ()) (PacketStream domEthTx 1 ())
dummyTxPhy = undefined
}:

The input type can be replaced with the data type supported by the hardware primitives of
your equipment, as long as the output is `PacketStream domEthTx 1 ()`.

>>> :{
dummyRxPhy
  :: HiddenClockResetEnable domEthTx
  => Circuit (PacketStream domEthTx 1 ()) (PacketStream domEthTx 1 ())
dummyRxPhy = undefined
}:



-}
module Clash.Cores.Ethernet.Examples.FullUdpStack (
  fullStackC,
  arpIcmpUdpStackC,
  icmpUdpStackC,
) where

import Clash.Cores.Crc ( HardwareCrc )
import Clash.Cores.Crc.Catalog ( Crc32_ethernet )

import Clash.Cores.Ethernet.Arp
import Clash.Cores.Ethernet.Examples.RxStacks
import Clash.Cores.Ethernet.Examples.TxStacks
import Clash.Cores.Ethernet.Mac
import Clash.Cores.Ethernet.IPv4
import Clash.Cores.Ethernet.Icmp ( icmpEchoResponderC )
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
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
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
  [icmpIn, udpIn] <- packetDispatcherC (routeBy _ipv4lProtocol $ 0x0001 :> 0x0011 :> Nil) -< ipIn
  icmpOut <- icmpEchoResponderC (fst <$> ipS) -< icmpIn
  udpInParsed <- udpDepacketizerC -< udpIn
  udpOutParsed <- udpPacketizerC (fst <$> ipS) -< udpOut
  ipOut <- packetArbiterC RoundRobin -< [icmpOut, udpOutParsed]
  idC -< (udpInParsed, ipOut)
