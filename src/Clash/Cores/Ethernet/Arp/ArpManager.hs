{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{-|
Module      : Clash.Cores.Ethernet.Arp.ArpManager
Description : Provides an ARP manager which handles ARP lookups from client circuits.
-}
module Clash.Cores.Ethernet.Arp.ArpManager
  ( arpManagerC
  , arpReceiverC
  , arpTransmitterC
  ) where

import Clash.Prelude
import Clash.Signal.Extra

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream

import Clash.Cores.Ethernet.Arp.ArpTypes
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import qualified Data.Bifunctor as B


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
--   in the ARP table, it will broadcast an ARP request to the local network and wait at most `maxWaitSeconds`
--   for a reply. If no reply was received within time, the lookup request is ignored. `maxWaitSeconds` is inaccurate
--   for up to one second less. For example, if `maxWaitSeconds` ~ 30, then the component will wait for 29-30 seconds.
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

-- | Transmits ARP packets upon request.
arpTransmitterC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => 1 <= dataWidth
  => KnownNat dataWidth
  => Signal dom MacAddress
  -- ^ Our MAC address
  -> Signal dom IPv4Address
  -- ^ Our IPv4 address
  -> Circuit (Df dom ArpLite) (PacketStream dom dataWidth EthernetHeader)
arpTransmitterC ourMacS ourIPv4S = fromSignals bundleWithSrc |> packetizeFromDfC toEthernetHdr constructArpPkt
  where
    bundleWithSrc (fwdIn, bwdIn) = (bwdIn, go <$> bundle (ourMacS, ourIPv4S, fwdIn))
    go (ourMac, ourIPv4, maybeArpLite) = maybeArpLite >>= \arpLite -> Df.Data (ourMac, ourIPv4, arpLite)

    toEthernetHdr (ourMac, _, arpLite)
      = EthernetHeader {
        _macDst = _targetMac arpLite,
        _macSrc = ourMac,
        _etherType = arpEtherType
      }

    constructArpPkt (ourMac, ourIPv4, arpLite)
      = newArpPacket ourMac ourIPv4 (_targetMac arpLite) (_targetIPv4 arpLite) (_isRequest arpLite)

-- | arpReceiverC takes the incoming PacketStream
--   with an ethernet header in the meta data and
--   creates an ARP entry or an ARP response.
--   - It outputs ARP entries for ARP responses (OPER == 2)
--     and GARP messages in the form of an ARP request (OPER == 1) with
--     TPA == SPA.
--   - It outputs ARP lite responses for any other ARP request (OPER == 1 and
--     TPA /= SPA).
arpReceiverC
  :: forall (dom :: Domain) (dataWidth :: Nat)
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => 1 <= dataWidth
  => Signal dom IPv4Address
  -> Circuit (PacketStream dom dataWidth EthernetHeader) (Df dom ArpEntry, Df dom ArpLite)
arpReceiverC myIP = circuit $ \ethStream -> do
  -- TODO:
  -- when backpressure is asserted on `arpTransmitter`,
  -- the entire arp stack will stall and this will lead
  -- to corruption on the `arpReceiver` side.
  -- This only happens when the outlink is saturated, but
  -- in the future we want to handle this.
  -- Solution: putting abortOnBackpressure (Packetbuffer) to
  -- before `depacketizetoDfC` should work, as depacketizeToDfC already
  -- implements dropping of
  arpDf <- depacketizeToDfC const -< ethStream
  arpDf' <- Df.filterS (isValidArp <$> myIP) -< arpDf
  (arpRequests, arpEntries) <- partitionS (isRequest <$> myIP) -< arpDf'
  lites <- Df.map (\p -> ArpLite (_sha p) (_spa p) False) -< arpRequests
  entries <- Df.map (\p -> ArpEntry (_sha p) (_spa p)) -< arpEntries
  idC -< (entries, lites)
  where
    isRequest ip ArpPacket{..} = _oper == 1 && _tpa == ip

-- TODO upstream to clash-protocols
-- | Like 'partition', but can reason over signals.
partitionS ::  forall dom a. Signal dom (a -> Bool) -> Circuit (Df dom a) (Df dom a, Df dom a)
partitionS fS = Circuit (B.second unbundle . unbundle . liftA2 go fS . bundle . B.second bundle)
 where
  go f (Df.Data a, (ackT, ackF))
    | f a = (ackT, (Df.Data a, Df.NoData))
    | otherwise = (ackF, (Df.NoData, Df.Data a))
  go _ _ = (Ack False, (Df.NoData, Df.NoData))
