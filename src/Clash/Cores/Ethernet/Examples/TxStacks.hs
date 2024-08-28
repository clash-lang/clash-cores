{-# language FlexibleContexts #-}

{-|
Module      : Clash.Cores.Ethernet.Examples.TxStacks
Description : Provides the entire transmit stack as a circuit.
-}
module Clash.Cores.Ethernet.Examples.TxStacks
  ( macTxStack
  , ipTxStack
  ) where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Prelude

import Protocols ( Circuit, (|>) )
import Protocols.PacketStream

import Clash.Cores.Ethernet.IP.IPPacketizers
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes
import Clash.Cores.Ethernet.Mac.FrameCheckSequence ( fcsInserterC )
import Clash.Cores.Ethernet.Mac.InterpacketGapInserter ( interpacketGapInserterC )
import Clash.Cores.Ethernet.Mac.MacPacketizers ( macPacketizerC )
import Clash.Cores.Ethernet.Mac.PaddingInserter ( paddingInserterC )
import Clash.Cores.Ethernet.Mac.Preamble ( preambleInserterC )


-- | Processes bytes to send over ethernet
macTxStack
  :: forall (dataWidth :: Nat) (dom :: Domain) (domEth :: Domain)
   . ( KnownNat dataWidth
     , 1 <= dataWidth
     , HiddenClockResetEnable dom
     , KnownDomain domEth
     , HardwareCrc Crc32_ethernet 8 dataWidth
     )
  => Clock domEth
  -> Reset domEth
  -> Enable domEth
  -> Circuit (PacketStream dom dataWidth EthernetHeader) (PacketStream domEth 1 ())
macTxStack ethClk ethRst ethEn =
  macPacketizerC
  |> paddingInserterC d60
  |> fcsInserterC
  |> preambleInserterC
  |> asyncFifoC d4 hasClock hasReset hasEnable ethClk ethRst ethEn
  |> exposeClockResetEnable downConverterC ethClk ethRst ethEn
  |> exposeClockResetEnable interpacketGapInserterC ethClk ethRst ethEn d12

-- | Sends IP packets to a known mac address
ipTxStack
  :: forall (dataWidth :: Nat) (dom :: Domain) (domEth :: Domain)
   . ( KnownNat dataWidth
     , 1 <= dataWidth
     , HiddenClockResetEnable dom
     , KnownDomain domEth
     , HardwareCrc Crc32_ethernet 8 dataWidth
     )
  => Clock domEth
  -> Reset domEth
  -> Enable domEth
  -> Signal dom MacAddress
  -> Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream domEth 1 ())
ipTxStack ethClk ethRst ethEn macAddressS =
  ipLitePacketizerC
  |> constToEthernetC 0x8000 (MacAddress $ 0x00 :> 0x00 :> 0x00 :> 0xff :> 0xff :> 0xff :> Nil) macAddressS
  |> macTxStack ethClk ethRst ethEn
