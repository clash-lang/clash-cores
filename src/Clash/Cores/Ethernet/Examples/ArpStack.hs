{-|
Module      : Clash.Cores.Ethernet.Examples.ArpStack
Description : Fully functional ARP stack.
-}

{-# language FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Clash.Cores.Ethernet.Examples.ArpStack
( arpStackC
) where

import Clash.Prelude

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Cores.Ethernet.Arp
import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.Examples.RxStacks
import Clash.Cores.Ethernet.Examples.TxStacks
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Protocols
import Protocols.PacketStream


-- | TODO replace this by the IPv4 -> Ethernet stream transformer
constArpLookup :: Circuit () (ArpLookup dom)
constArpLookup = Circuit $ \((), _bwdIn) -> ((), fwdOut)
  where
    fwdOut = pure (Just (IPv4Address (192 :> 168 :> 1 :> 254 :> Nil)))

-- | Fully functional ARP stack.
arpStackC
  :: forall
       (dom :: Domain)
       (domEthRx :: Domain)
       (domEthTx :: Domain)
   . KnownDomain dom
  => KnownDomain domEthRx
  => KnownDomain domEthTx
  => KnownNat (DomainPeriod dom)
  => DomainPeriod dom <= 5 * 10^11
  => 1 <= DomainPeriod dom
  => HardwareCrc Crc32_ethernet 8 4
  => HiddenClockResetEnable dom
  => Clock domEthRx
  -> Reset domEthRx
  -> Enable domEthRx
  -> Clock domEthTx
  -> Reset domEthTx
  -> Enable domEthTx
  -> Signal dom MacAddress
  -> Signal dom IPv4Address
  -> Circuit (PacketStream domEthRx 1 ()) (PacketStream domEthTx 1 ())
arpStackC rxClk rxRst rxEn txClk txRst txEn ourMacS ourIPv4S =
  circuit $ \stream -> do
    ethStream <- macRxStack @4 rxClk rxRst rxEn ourMacS -< stream
    [arpStream] <- packetDispatcherC (singleton $ \hdr -> _etherType hdr == arpEtherType) -< ethStream
    lookupIn <- constArpLookup -< ()
    arpOtp <- arpC d300 d2 ourMacS ourIPv4S -< (arpStream, lookupIn)
    ethOtp <- packetArbiterC RoundRobin -< [arpOtp]
    macTxStack txClk txRst txEn -< ethOtp
