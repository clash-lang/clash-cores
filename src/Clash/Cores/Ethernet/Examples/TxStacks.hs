{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

This module contains an example of a fully modular MAC transmit stack which
allows the transmission of packets over Ethernet II and supports any input
data width bigger than zero.

Example usage:

>>> import Clash.Cores.Crc (HardwareCrc, deriveHardwareCrc)
>>> import Clash.Cores.Crc.Catalog (Crc32_ethernet(..))
>>> import Clash.Cores.Ethernet.Mac
>>> import Clash.Prelude
>>> import Protocols
>>> import Protocols.PacketStream

The Ethernet TX PHY is completely interchangeable with this stack. In the
example below, we use a dummy. You have to replace this dummy variable with
an Ethernet TX PHY circuit for your specific hardware (e.g. RGMII, MII or SGMII)
that is adapted to the `PacketStream` protocol, i.e. with type:

>>> :{
dummyTxPhy ::
  (HiddenClockResetEnable domEthTx) =>
  Circuit (PacketStream domEthTx 1 ()) (PacketStream domEthTx 1 ())
dummyTxPhy = undefined
:}

For example, the Lattice ECP5 Colorlight 5A-75B board uses an RGMII PHY,
found at 'Clash.Cores.Ethernet.Rgmii.rgmiiTxC'.

'macTxStack' is the most common Ethernet MAC TX stack that will be sufficient
for most people. That is, it inserts an interpacket gap of 12 bytes, pads the
payload to 46 bytes and assumes that you have processed the transmitted bytes
in a different clock domain than the Ethernet TX domain. To use it, all you
have to do is specify the data width (in this example 4), the clock domains,
and the TX PHY you want to use.

The stack uses 'Clash.Cores.Crc.crcEngine' internally to calculate the frame
check sequence of each transmitted Ethernet frame, so that it can be appended
to the packet. To be able to use this component, we need to use
'Clash.Cores.Crc.deriveHardwareCrc' to derive the necessary instance.

>>> :{
$(deriveHardwareCrc Crc32_ethernet d8 d1)
myTxStack ::
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthTx) =>
  (Clock domEthTx) ->
  (Reset domEthTx) ->
  (Enable domEthTx) ->
  Circuit (PacketStream dom 4 EthernetHeader) (PacketStream domEthTx 1 ())
myTxStack ethTxClk ethTxRst ethTxEn =
  macTxStack @4 ethTxClk ethTxRst ethTxEn
    |> exposeClockResetEnable dummyTxPhy ethTxClk ethTxRst ethTxEn
:}

While this pre-defined stack is very simple to use, it might not be want you
want. Maybe you want to use a vendor-specific async fifo, or maybe you want
some components that are currently operating in the internal domain @dom@ to
operate in the Ethernet TX domain @domEthTx@ (or vice versa). Timing
requirements differ greatly across different PHY protocols and FPGA boards or
ASICs. Maybe you need to add skid buffers (`registerBoth`, `registerBwd`, or
`registerFwd`) between components to make timing pass, or maybe you can remove
them if they are not necessary in order to save resources.

In our standard stack, FCS insertion is done in the Ethernet TX domain, because
that allows us to do it at data width 1. This saves a significant amount of
logic resources, even when having to place extra skid buffers to make timing
pass. For very high speed Ethernet standards you might have to do less work in
the Ethernet TX clock domain.

In any case, it is easy to create a custom stack. All you have to do is import
all the necessary components and connect them with the '|>' operator, creating
one big 'Circuit'. For example:

>>> :{
$(deriveHardwareCrc Crc32_ethernet d8 d8)
myCustomTxStack ::
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthTx) =>
  (Clock domEthTx) ->
  (Reset domEthTx) ->
  (Enable domEthTx) ->
  Circuit (PacketStream dom 8 EthernetHeader) (PacketStream domEthTx 1 ())
myCustomTxStack ethTxClk ethTxRst ethTxEn =
  macPacketizerC
    |> paddingInserterC d60
    |> fcsInserterC
    |> preambleInserterC
    |> asyncFifoC d4 hasClock hasReset hasEnable ethTxClk ethTxRst ethTxEn
    |> exposeClockResetEnable downConverterC ethTxClk ethTxRst ethTxEn
    |> exposeClockResetEnable (interpacketGapInserterC d16) ethTxClk ethTxRst ethTxEn
    |> exposeClockResetEnable dummyTxPhy ethTxClk ethTxRst ethTxEn
:}

This custom TX stack does almost everything in the internal domain. For the
sake of illustration, it also uses a bigger interpacket gap than usual, i.e.
16 bytes. It also doesn't use any skid buffers.
-}
module Clash.Cores.Ethernet.Examples.TxStacks (
  macTxStack,
  ipTxStack,
) where

import Clash.Cores.Crc (HardwareCrc)
import Clash.Cores.Crc.Catalog (Crc32_ethernet)
import Clash.Cores.Ethernet.IP.IPPacketizers
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac

import Clash.Prelude

import Protocols (Circuit, (|>))
import Protocols.PacketStream

{- |
Ethernet MAC TX block. Assumes @dom@ is a different domain than
@domEthTx@. For this stack to work, the input @dataWidth@
__MUST__ satisfy the following formula:

@DomainPeriod dom <= DomainPeriod domEthTx * dataWidth@

Processing is done in the following way:

1. The payload stream together with an 'EthernetHeader' in the metadata arrives
at 'macPacketizerC', which prepends this header to the stream. This header
contains the source and destination MAC addresses, and the EtherType of the
payload.

5. `asyncFifoC` is used to cross clock domains, because the clock domain of
the Ethernet TX PHY is usually different from the clock domain that is used
internally.

3. A pipeline skid buffer ('registerBoth') is inserted along the path in order
to improve timing.

4. 'downConverterC' downsizes the stream from @dataWidth@ bytes to @1@ byte
wide. This makes the coming upcoming components more resource-efficient, and
it is possible because we now operate in a faster domain.

5. 'paddingInserterC' pads the Ethernet frame to 60 bytes with null bytes if
necessary. Just 60 bytes, because the FCS is not inserted yet. Inserting that
will cause the Ethernet frame to have the correct minimum length of 64 bytes.

6. The resulting stream passes through 'fcsInserterC', which calculates the
Ethernet CRC over the payload and already inserted Ethernet header and
appends it to the stream.

7. Another pipeline skid buffer is inserted.

8. The last real manipulation of the stream is the insertion of the preamble
to the front of the stream by 'preambleInserterC', that is, 7 bytes of
alternating ones and zeroes followed by the start frame delimiter.

9. Lastly, an interpacket gap of 12 bytes is inserted.
-}
macTxStack ::
  forall
    (dataWidth :: Nat)
    (dom :: Domain)
    (domEthTx :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthTx) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (HardwareCrc Crc32_ethernet 8 1) =>
  -- | Clock signal in the Ethernet TX domain
  Clock domEthTx ->
  -- | Reset signal in the Ethernet TX domain
  Reset domEthTx ->
  -- | Enable signal in the Ethernet TX domain
  Enable domEthTx ->
  Circuit
    (PacketStream dom dataWidth EthernetHeader)
    (PacketStream domEthTx 1 ())
macTxStack ethTxClk ethTxRst ethTxEn =
  macPacketizerC
    |> asyncFifoC d4 hasClock hasReset hasEnable ethTxClk ethTxRst ethTxEn
    |> exposeClockResetEnable ethTxCkt ethTxClk ethTxRst ethTxEn
 where
  ethTxCkt ::
    (HiddenClockResetEnable domEth) =>
    Circuit (PacketStream domEth dataWidth ()) (PacketStream domEth 1 ())
  ethTxCkt =
    registerBoth
      |> downConverterC
      |> paddingInserterC d60
      |> fcsInserterC
      |> registerBoth
      |> preambleInserterC
      |> interpacketGapInserterC d12

-- | Sends IP packets to a known MAC address
ipTxStack ::
  forall
    (dataWidth :: Nat)
    (dom :: Domain)
    (domEthTx :: Domain).
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (HiddenClockResetEnable dom) =>
  (KnownDomain domEthTx) =>
  (HardwareCrc Crc32_ethernet 8 1) =>
  -- | Clock signal in the Ethernet TX domain
  Clock domEthTx ->
  -- | Reset signal in the Ethernet TX domain
  Reset domEthTx ->
  -- | Enable signal in the Ethernet TX domain
  Enable domEthTx ->
  -- | Our MAC address
  Signal dom MacAddress ->
  Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream domEthTx 1 ())
ipTxStack ethTxClk ethTxRst ethTxEn ourMacS =
  ipLitePacketizerC
    |> constToEthernetC
      0x8000
      (MacAddress $ 0x00 :> 0x00 :> 0x00 :> 0xff :> 0xff :> 0xff :> Nil)
      ourMacS
    |> macTxStack ethTxClk ethTxRst ethTxEn
