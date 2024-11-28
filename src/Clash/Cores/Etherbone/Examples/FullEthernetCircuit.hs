{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# LANGUAGE NumericUnderscores #-}

-- | This file implements functions to link @etherboneC@ to the Ethernet stack
-- from clash-cores. It has a wishbone bus connected to it with a
-- @singleMasterInterconnect@ and as peripherals a 4-register scratchpad and a
-- ROM with SDB data in it.
--
-- For a full demo implementation on a Xilinx (xcku040) device, see
-- @XilinxDemo@.

module Clash.Cores.Etherbone.Examples.FullEthernetCircuit where

import Clash.Cores.Etherbone
import Clash.Cores.Etherbone.Base
import Clash.Cores.Etherbone.Examples.Interconnect (singleMasterInterconnect)
import Clash.Cores.Etherbone.Examples.WishboneBus
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Udp
import Clash.Prelude
import Protocols
import Protocols.PacketStream

import qualified Data.Bifunctor as B
import Data.Maybe

-- | Circuit to strip all extra Ethernet padding bytes that are passed to the
-- UDP application. It passes through @_udplPayloadLength@ bytes and drops the
-- rest.
--
-- This might be handled in 'udpDepacketizerC' in the future. Then this function
-- is not needed anymore.
udpPaddingStripperC :: forall dom dataWidth .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  )
  => Circuit (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
             (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
udpPaddingStripperC = Circuit $ mealyB go (0 :: Unsigned 16)
  where
    go count (Nothing,   _)
      = (count, (PacketStreamS2M True, Nothing))
    go count (Just iFwd, PacketStreamS2M oBwd)
      = (nextCount, (PacketStreamS2M iBwd, oFwd))
      where
        count' = count + dataWidth
        nextCount
          | count >= payloadSize = case _last iFwd of
            Just _ -> 0
            _      -> count
          | isJust oFwd && oBwd  = case _last iFwd of
            Just _ -> 0
            _      -> count'
          | otherwise            = count

        hdr = snd $ _meta iFwd
        payloadSize = _udplPayloadLength hdr
        dataWidth = natToNum @dataWidth

        iBwd = isNothing oFwd || oBwd

        oFwd
          | count' < payloadSize               = Just $ iFwd { _last = Nothing }
          | count' < (payloadSize + dataWidth) = Just $ iFwd { _last = Just maxBound }
          | otherwise                          = Nothing
{-# OPAQUE udpPaddingStripperC #-}

-- | Circuit to pass the UDP and IP meta information to the outgoing packet
-- without having to pass this data through the whole Etherbone stack.
ethMetaBypassC ::
  ( HiddenClockResetEnable dom )
  => Circuit ( PacketStream dom DataWidth (IPv4Address, UdpHeaderLite)
             , PacketStream dom DataWidth () )
             ( PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
ethMetaBypassC = Circuit $ B.first unbundle . go . B.first bundle
  where
    go = mealyB goT Nothing

    goT Nothing  ((Nothing, _), _)
      = (Nothing, ((PacketStreamS2M True, PacketStreamS2M False), Nothing))
    goT Nothing  ((Just m, _), _)
      = (st', ((PacketStreamS2M True, PacketStreamS2M False), Nothing))
      where
        st'
          | magic == etherboneMagic = Just $ B.second swapPortsL $ _meta m
          | otherwise               = Nothing

        magic = pack $ take d2 $ _data m
    goT st@(Just m) ((_, iFwd), PacketStreamS2M oBwd)
      = (st', ((PacketStreamS2M True, PacketStreamS2M iBwd), oFwd))
      where
        st'
          | oBwd && isJust (iFwd >>= _last) = Nothing
          | otherwise                       = st
        iBwd = oBwd
        metaMap x = x {_meta = m}
        oFwd = metaMap <$> iFwd
{-# OPAQUE ethMetaBypassC #-}

-- | Full Etherbone implementation making use of the Ethernet core from
-- clash-cores.
fullCircuit ::
  ( HiddenClockResetEnable dom )
  => Circuit (PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
            (PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
fullCircuit = circuit $ \rx -> do
  [rx', rxM] <- fanout -< rx

  rxS <- mapMeta (const ()) -< rx'
  (txS, wbBus) <- etherboneC 0x8000_0000 (pure Nil) -< rxS

  [wb0, wb1] <- singleMasterInterconnect @_ @_ @AddrWidth (0b0 :> 0b1 :> Nil) -< wbBus
  wishboneScratchpad @_ @WBData @_ d4 -< wb0
  wishboneRom @_ @_ @WBData sdbRom -< wb1

  ethMetaBypassC -< (rxM, txS)
{-# OPAQUE fullCircuit #-}

-- | Same as @fullCircuit@ but with the PacketStream stripped to the payload
-- length indicated in the UDP header.
fullCircuitStripped ::
  ( HiddenClockResetEnable dom )
  => Circuit (PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
             (PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
fullCircuitStripped = udpPaddingStripperC |> fullCircuit
