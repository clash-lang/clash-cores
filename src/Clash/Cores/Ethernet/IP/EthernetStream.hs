{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Ethernet.IP.EthernetStream (
  toEthernetStreamC,
) where

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Clash.Prelude

import Data.Maybe (isJust)

import Protocols
import Protocols.PacketStream

-- | State of 'toEthernetStreamT'.
data EthernetStreamState
  = Idle
  | DropPacket
  | Forward {_mac :: MacAddress}
  deriving (Generic, NFDataX, Show, ShowX)

-- | State transition function of 'toEthernetStreamC'.
toEthernetStreamT ::
  forall (dataWidth :: Nat).
  (KnownNat dataWidth) =>
  EthernetStreamState ->
  ( Maybe (PacketStreamM2S dataWidth IPv4Address)
  , PacketStreamS2M
  , Maybe ArpResponse
  ) ->
  ( EthernetStreamState
  , ( PacketStreamS2M
    , Maybe (PacketStreamM2S dataWidth MacAddress)
    , Maybe IPv4Address
    )
  )
toEthernetStreamT Idle (transferInM, _, arpResp) =
  (nextSt, (PacketStreamS2M False, Nothing, _meta <$> transferInM))
 where
  nextSt = case arpResp of
    Nothing -> Idle
    Just ArpEntryNotFound -> DropPacket
    Just (ArpEntryFound mac) -> Forward{_mac = mac}
toEthernetStreamT DropPacket (Just transferIn, _, _) =
  (nextSt, (PacketStreamS2M True, Nothing, Nothing))
 where
  nextSt = if isJust (_last transferIn) then Idle else DropPacket
toEthernetStreamT st@Forward{..} (Just transferIn, PacketStreamS2M readyIn, _) =
  (nextSt, (PacketStreamS2M readyIn, Just (_mac <$ transferIn), Nothing))
 where
  nextSt = if isJust (_last transferIn) && readyIn then Idle else st
toEthernetStreamT st (Nothing, _, _) = (st, (PacketStreamS2M True, Nothing, Nothing))

{- |
Bridges the gap between the IPv4 and MAC layer by transforming packets directed
to an IPv4 address (in the metadata) to packets directed to a MAC address.
It does so by sending the IPv4 address in the metadata to the ARP service,
for each packet in the stream. If the ARP service responds with 'ArpEntryNotFound',
the packet is dropped to avoid stalling the network stack.

The maximum latency per packet depends on the configuration of the ARP service,
there are no timers in this component.
-}
toEthernetStreamC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  -- | Our MAC address
  Signal dom MacAddress ->
  Circuit
    (PacketStream dom dataWidth IPv4Address)
    (PacketStream dom dataWidth EthernetHeader, ArpLookup dom)
toEthernetStreamC ourMacS = circuit $ \transferIn -> do
  (withDstMac, req) <- fromSignals resolver -< transferIn
  withEthernetHeader <-
    mapMetaS ((\src dst -> EthernetHeader dst src 0x0800) <$> ourMacS) -< withDstMac
  idC -< (withEthernetHeader, req)
 where
  resolver (transferIn, (readyIn, respIn)) = (readyOut, (transferOut, reqOut))
   where
    (readyOut, transferOut, reqOut) =
      mealyB toEthernetStreamT Idle (transferIn, readyIn, respIn)
