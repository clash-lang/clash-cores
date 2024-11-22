{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides various data types, aliases and constants for the Ethernet protocol.
-}
module Clash.Cores.Ethernet.Mac.EthernetTypes (
  MacAddress (..),
  EthernetHeader (..),
  broadcastMac,
  constToEthernetC,
) where

import Control.DeepSeq (NFData)

import Clash.Prelude

import Protocols (Circuit)
import Protocols.PacketStream (PacketStream, mapMetaS)

-- | Stores a MAC address, which is always 6 bytes long.
newtype MacAddress = MacAddress (Vec 6 (BitVector 8))
  deriving (BitPack, Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | Stores a link-layer Ethernet header.
data EthernetHeader = EthernetHeader
  { _macDst :: MacAddress
  -- ^ Destination MAC address
  , _macSrc :: MacAddress
  -- ^ Source MAC address
  , _etherType :: BitVector 16
  -- ^ EtherType
  }
  deriving (BitPack, Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | Broadcast MAC address.
broadcastMac :: MacAddress
broadcastMac = MacAddress (repeat 0xFF)

{- |
Convert an arbitrary packet stream to an Ethernet stream with a hardcoded
destination MAC address.

Runs at full throughput and provides zero latency.
-}
constToEthernetC ::
  (HiddenClockResetEnable dom) =>
  -- | EtherType
  BitVector 16 ->
  -- | Hardcoded destination MAC address
  MacAddress ->
  -- | Our MAC address
  Signal dom MacAddress ->
  Circuit
    (PacketStream dom dataWidth meta)
    (PacketStream dom dataWidth EthernetHeader)
constToEthernetC etherType macDst ourMacS = mapMetaS (const <$> hdr)
 where
  hdr =
    ( \ourMac ->
        EthernetHeader
          { _macDst = macDst
          , _macSrc = ourMac
          , _etherType = etherType
          }
    )
      <$> ourMacS
