{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains an example of a fully modular Ethernet MAC receive stack
which allows the reception of packets over Ethernet II and supports any
output data width bigger than zero.

Example usage:

>>> :set -XViewPatterns
>>> import Clash.Cores.Crc (HardwareCrc, deriveHardwareCrc)
>>> import Clash.Cores.Crc.Catalog (Crc32_ethernet(..))
>>> import Clash.Cores.Ethernet.Mac
>>> import Clash.Prelude
>>> import Protocols
>>> import Protocols.PacketStream

The Ethernet RX PHY is completely interchangeable with this stack. In the
example below, we use a dummy. You have to replace this dummy variable with
an Ethernet RX PHY circuit for your specific hardware (e.g. RGMII, MII or SGMII)
that is adapted to the `PacketStream` protocol, i.e. with type:

>>> :{
dummyRxPhy ::
  (HiddenClockResetEnable domEthRx) =>
  Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthRx 1 ())
dummyRxPhy = undefined
:}

For example, the Lattice ECP5 Colorlight 5A-75B board uses an RGMII PHY,
found at `Clash.Cores.Ethernet.Rgmii.unsafeRgmiiRxC`.

`macRxStack` is the most common Ethernet MAC RX stack that will be sufficient
for most people. That is, it assumes that you want to process the received
bytes in a different clock domain than the Ethernet RX domain. To use it,
all you have to do is specify the data width (in this example 4), the clock
domains, and the RX PHY you want to use.

The stack uses `Clash.Cores.Crc.crcValidator` internally to validate the frame
check sequence of each transmitted Ethernet frame. To be able to use this
component, we need to use `Clash.Cores.Crc.deriveHardwareCrc` to derive the
necessary instance.

>>> :{
$(deriveHardwareCrc Crc32_ethernet d8 d1)
myRxStack ::
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthRx) =>
  (Clock domEthRx) ->
  (Reset domEthRx) ->
  (Enable domEthRx) ->
  Signal dom MacAddress ->
  Circuit (PacketStream domEthRx 1 ()) (PacketStream dom 4 EthernetHeader)
myRxStack ethRxClk ethRxRst ethRxEn ourMacS =
  exposeClockResetEnable dummyRxPhy ethRxClk ethRxRst ethRxEn
    |> macRxStack @4 ethRxClk ethRxRst ethRxEn ourMacS
:}

While this pre-defined stack is very simple to use, it might not be want you
want. Maybe you want to use a vendor-specific async fifo, or maybe you want
some components that are currently operating in the internal domain @dom@ to
operate in the Ethernet RX domain @domEthRx@ (or vice versa). Timing
requirements differ greatly across different PHY protocols and FPGA boards
or ASICs. Maybe you need to add skid buffers (`registerBoth`, `registerBwd`,
or `registerFwd`) between components to make timing pass, or maybe you can
remove them if they are not necessary in order to save resources.

In our standard stack, FCS validation is done in the Ethernet RX domain,
because that allows us to do it at data width 1. This saves a significant
amount of logic resources, even when having to place extra skid buffers to make
timing pass. For very high speed Ethernet standards you might have to do less
work in the Ethernet RX clock domain.

In any case, it is easy to create a custom stack. All you have to do is import
all the necessary components and connect them with the `|>` operator, creating
one big `Circuit`. For example:

>>> :{
$(deriveHardwareCrc Crc32_ethernet d8 d8)
myCustomRxStack ::
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthRx) =>
  (Clock domEthRx) ->
  (Reset domEthRx) ->
  (Enable domEthRx) ->
  Signal dom MacAddress ->
  Circuit (PacketStream domEthRx 1 ()) (PacketStream dom 8 EthernetHeader)
myCustomRxStack ethRxClk ethRxRst ethRxEn ourMacS =
  exposeClockResetEnable dummyRxPhy ethRxClk ethRxRst ethRxEn
    |> exposeClockResetEnable preambleStripperC ethRxClk ethRxRst ethRxEn
    |> exposeClockResetEnable upConverterC ethRxClk ethRxRst ethRxEn
    |> asyncFifoC d4 ethRxClk ethRxRst ethRxEn hasClock hasReset hasEnable
    |> fcsValidatorC
    |> fcsStripperC
    |> macDepacketizerC
    |> filterMetaS (isForMyMac <$> ourMacS)
 where
  isForMyMac myMac (_macDst -> to) = to == myMac || to == broadcastMac
:}

This custom RX stack does almost everything in the internal domain. It also
doesn't use any skid buffers.
-}
module Clash.Cores.Ethernet.Examples.RxStacks (
  macRxStack,
  ipRxStack,
) where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Prelude

import Protocols
import Protocols.PacketStream

import Clash.Cores.Ethernet.IP.IPPacketizers
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac

{- |
Ethernet MAC RX block. Assumes @dom@ is a different domain than
@domEthRx@. For this stack to work, the input @dataWidth@
__MUST__ satisfy the following formula:

@dataWidth * DomainPeriod dom <= DomainPeriod domEthRx@

Processing is done in the following way, in order:

1. The PHY passes raw Ethernet packets to us, which first arrive at
`preambleStripperC`. This component removes the preamble and SFD from each
packet in the stream.

2. A pipeline skid buffer ('registerBoth') is inserted along the path in order
to improve timing.

3. `fcsValidatorC` computes the FCS of each packet in the resulting stream.
If the FCS did not match, the packet is aborted (but not dropped, yet).

4. `upConverterC` upsizes the stream from @1@ byte to @dataWidth@ bytes wide.
This is necessary for full throughput, because we will operate in a slower
clock domain soon.

5. `asyncFifoC` is used to cross clock domains, because the clock domain of
the Ethernet RX PHY is usually different from the clock domain that is used
internally.

6. `fcsStripperC` removes the FCS field from each packet in the stream
(that is, the last 4 bytes).

7. `macDepacketizerC` parses the first 14 bytes of each packet in the stream
into an Ethernet MAC header and puts it in the metadata.

8. Lastly, we drop any packets that are not destined for either our MAC
address or the broadcast MAC address.

The output stream is now ready for further higher-level processing.
For example, it may be routed via the EtherType field in the metadata to a
network layer in hardware. It could also be written straight to RAM via DMA
for further processing by a CPU.
-}
macRxStack ::
  forall
    (dataWidth :: Nat)
    (dom :: Domain)
    (domEthRx :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthRx) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (HardwareCrc Crc32_ethernet 8 1) =>
  -- | Clock signal in the Ethernet RX domain
  Clock domEthRx ->
  -- | Reset signal in the Ethernet RX domain
  Reset domEthRx ->
  -- | Enable signal in the Ethernet RX domain
  Enable domEthRx ->
  -- | Our MAC address
  Signal dom MacAddress ->
  Circuit
    (PacketStream domEthRx 1 ())
    (PacketStream dom dataWidth EthernetHeader)
macRxStack ethRxClk ethRxRst ethRxEn ourMacS =
  withClockResetEnable ethRxClk ethRxRst ethRxEn ethRxCkt
    |> asyncFifoC d4 ethRxClk ethRxRst ethRxEn hasClock hasReset hasEnable
    |> fcsStripperC
    |> macDepacketizerC
    |> filterMetaS (isForMyMac <$> ourMacS)
 where
  ethRxCkt ::
    (HiddenClockResetEnable domEthRx) =>
    Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthRx dataWidth ())
  ethRxCkt =
    preambleStripperC
      |> registerBoth
      |> fcsValidatorC
      |> upConverterC

  isForMyMac myMac (_macDst -> to) = to == myMac || to == broadcastMac

-- | Processes received IP packets
ipRxStack ::
  forall (dataWidth :: Nat) (dom :: Domain) (domEthRx :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthRx) =>
  (HardwareCrc Crc32_ethernet 8 1) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- | Clock signal in the Ethernet RX domain
  Clock domEthRx ->
  -- | Reset signal in the Ethernet RX domain
  Reset domEthRx ->
  -- | Enable signal in the Ethernet RX domain
  Enable domEthRx ->
  -- | Our MAC address
  Signal dom MacAddress ->
  -- | (Our IPv4, Our subnet mask)
  Signal dom (IPv4Address, IPv4SubnetMask) ->
  Circuit
    (PacketStream domEthRx 1 ())
    (PacketStream dom dataWidth IPv4HeaderLite)
ipRxStack ethRxClk ethRxRst ethRxEn ourMacS ipS = circuit $ \raw -> do
  ethernetFrames <- macRxStack ethRxClk ethRxRst ethRxEn ourMacS -< raw
  [ip] <- packetDispatcherC (isIpv4 :> Nil) -< ethernetFrames
  ipDepacketizerLiteC |> filterMetaS (isForMyIp <$> ipS) -< ip
 where
  isIpv4 = (== 0x0800) . _etherType
  isForMyIp (ip, subnet) (_ipv4lDestination -> to) =
    to == ip || to == subnetBroadcast subnet ip
