{-# language RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# OPTIONS_HADDOCK hide #-}

{-|
Copyright   : (C) 2024, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Provides individual components which handle the ARP protocol.
-}
module Clash.Cores.Ethernet.Arp.ArpManager (
  arpManagerC,
  arpReceiverC,
  arpTransmitterC,
) where

import Clash.Prelude
import Clash.Signal.Extra (secondTimer)

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream

-- | State of the ARP manager.
data ArpManagerState maxWaitSeconds
  = AwaitLookup {
    -- | Whether we need to keep driving the same ARP request to the transmitter,
    --   because it asserted backpressure.
    _awaitTransmission :: Bool
  }
  | AwaitArpReply {
    -- | The maximum number of seconds to keep waiting for an ARP reply.
    _secondsLeft :: Index (maxWaitSeconds + 1)
  } deriving (Generic, NFDataX, Show, ShowX)

-- | ARP manager transition function.
arpManagerT
  :: forall (maxWaitSeconds :: Nat)
   . 1 <= maxWaitSeconds
  => KnownNat maxWaitSeconds
  => ArpManagerState maxWaitSeconds
  -> (Maybe IPv4Address, Maybe ArpResponse, Ack, Bool)
  -> (ArpManagerState maxWaitSeconds
     , (Maybe ArpResponse, (Maybe IPv4Address, Df.Data ArpLite)))
-- User issues a lookup request. We don't have a timeout, because the ARP table should
-- always respond within a reasonable time frame. If not, there is a bug in the ARP table.
arpManagerT AwaitLookup{..} (Just lookupIPv4, arpResponseIn, Ack readyIn, _) =
  (nextSt, (arpResponseOut, (Just lookupIPv4, arpRequestOut)))
    where
      (arpResponseOut, arpRequestOut, nextSt) = case arpResponseIn of
        Nothing
          -> ( Nothing
             , if _awaitTransmission then Df.Data (ArpLite broadcastMac lookupIPv4 True) else Df.NoData
             , if readyIn && _awaitTransmission then AwaitArpReply maxBound else AwaitLookup False
             )
        Just ArpEntryNotFound
          -> ( Nothing
             , Df.Data (ArpLite broadcastMac lookupIPv4 True)
             , if readyIn then AwaitArpReply maxBound else AwaitLookup True
             )
        Just (ArpEntryFound _)
          -> ( arpResponseIn
             , Df.NoData
             , AwaitLookup False
             )

-- We don't care about incoming backpressure, because we do not send ARP requests in this state.
-- We keep polling the ARP table until either a timeout occurs or the entry is found.
-- This requires the ARP table to handle read and write requests in parallel.
arpManagerT AwaitArpReply{..} (Just lookupIPv4, arpResponseIn, _, secondPassed) =
  (nextSt, (arpResponseOut, (Just lookupIPv4, Df.NoData)))
    where
      newTimer = if secondPassed then satPred SatBound _secondsLeft else _secondsLeft

      (arpResponseOut, nextSt) =
        case (arpResponseIn, _secondsLeft == 0) of
          (Just (ArpEntryFound _), _)
            -> (arpResponseIn, AwaitLookup False)
          (Just ArpEntryNotFound, True)
            -> (arpResponseIn, AwaitLookup False)
          -- Note that we keep driving the same lookup request when the ARP table has not acknowledged
          -- our request yet, even if the time is up. If we don't, we violate protocol invariants.
          -- Therefore timer can be slightly inaccurate, depending on the latency of the ARP table.
          (_, _)
            -> (Nothing, AwaitArpReply newTimer)

arpManagerT st (Nothing, _,  _, _) = (st, (Nothing, (Nothing, Df.NoData)))

-- | This component handles ARP lookup requests by client components. If a lookup IPv4 address is not found
--   in the ARP table, it will broadcast an ARP request to the local network and wait at most @maxWaitSeconds@
--   for a reply. If no reply was received within time, the lookup request is ignored. @maxWaitSeconds@ is inaccurate
--   for up to one second less. For example, if @maxWaitSeconds@ ~ 30, then the component will wait for 29-30 seconds.
--   Does not support clock frequencies lower than 2 Hz.
arpManagerC
  :: forall (dom :: Domain)
            (maxWaitSeconds :: Nat)
   . HiddenClockResetEnable dom
  => KnownDomain dom
  => KnownNat (DomainPeriod dom)
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => 1 <= maxWaitSeconds
  => SNat maxWaitSeconds
  -- ^ The amount of seconds we wait for an incoming ARP reply
  -> Circuit (ArpLookup dom) (ArpLookup dom, Df dom ArpLite)
arpManagerC SNat = fromSignals ckt
  where
    ckt (lookupIPv4S, (arpResponseInS, ackInS)) = (bwdOut, unbundle fwdOut)
      where
        (bwdOut, fwdOut) =
          mealyB arpManagerT (AwaitLookup @maxWaitSeconds False) (lookupIPv4S, arpResponseInS, ackInS, secondTimer)

{- |
Transmits ARP packets upon request by creating a full 'ArpPacket' from the
input 'ArpLite' and packetizing that into a new packet stream. Uses
'packetizeFromDfC' internally to achieve this, and therefore inherits all of
its properties related to latency and throughput.

Because ARP's EtherType and our MAC address are known globally, we do not add
it to the metadata here, only the target MAC address. This makes this circuit
more flexible, because then the top-level ARP circuit decides where to add this
metadata to the stream, allowing for cheaper potential buffers between components.
-}
arpTransmitterC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- | Our MAC address
  Signal dom MacAddress ->
  -- | Our IPv4 address
  Signal dom IPv4Address ->
  Circuit (Df dom ArpLite) (PacketStream dom dataWidth MacAddress)
arpTransmitterC ourMacS ourIPv4S =
  fromSignals (\(fwdIn, bwdIn) -> (bwdIn, go <$> bundle (ourMacS, ourIPv4S, fwdIn)))
    |> packetizeFromDfC toTargetMac constructArpPkt
 where
  go (ourMac, ourIPv4, maybeArpLite) =
    maybeArpLite >>= \arpLite -> Df.Data (ourMac, ourIPv4, arpLite)

  toTargetMac (_, _, arpLite) = _targetMac arpLite

  constructArpPkt (ourMac, ourIPv4, ArpLite{..})
    = newArpPacket ourMac ourIPv4 _targetMac _targetIPv4 _isRequest

{-|
Parses the incoming packet stream into an @ArpPacket@, validates whether this
is a correct IPv4 to Ethernet ARP packet and then throws away all the redundant
information to create either an ARP entry or an ARP (lite) response:

- Outputs ARP entries for any gratuitous ARP packets (@TPA == SPA@) and
  ARP replies (@OPER == 2@).
- Outputs ARP (lite) responses for ARP requests (@OPER == 1@) where
  @TPA@ is our IPv4 address.

Uses 'depacketizeToDfC' internally to do the parsing, so all padding will be
consumed and packets will be dropped if they were aborted.

Assumes that the input stream is either a broadcast or directed towards us, and
that it is routed by the ARP EtherType.
-}
arpReceiverC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- Our IPv4 address
  Signal dom IPv4Address ->
  Circuit
    (PacketStream dom dataWidth ())
    (Df dom ArpEntry, Df dom ArpLite)
arpReceiverC myIP = circuit $ \stream -> do
  -- TODO:
  -- when backpressure is asserted on `arpTransmitter`,
  -- the entire arp stack will stall and this will lead
  -- to corruption on the `arpReceiver` side.
  -- This only happens when the outlink is saturated, but
  -- in the future we want to handle this.
  -- Solution: putting abortOnBackpressure (Packetbuffer) to
  -- before `depacketizetoDfC` should work, as depacketizeToDfC already
  -- implements dropping of
  arpDf <- depacketizeToDfC const -< stream
  arpDf' <- Df.filterS (validArp <$> myIP) -< arpDf
  (arpRequests, arpEntries) <- Df.partitionS (isRequest <$> myIP) -< arpDf'
  lites <- Df.map (\p -> ArpLite (_sha p) (_spa p) False) -< arpRequests
  entries <- Df.map (\p -> ArpEntry (_sha p) (_spa p)) -< arpEntries
  idC -< (entries, lites)
  where
    validArp ip ArpPacket{..} =
            _htype == 1
          && _ptype == 0x0800
          && _hlen  == 6
          && _plen  == 4
          &&(_oper == 1 && (_tpa == ip || _tpa == _spa) || _oper == 2)

    isRequest ip ArpPacket{..} = _oper == 1 && _tpa == ip
