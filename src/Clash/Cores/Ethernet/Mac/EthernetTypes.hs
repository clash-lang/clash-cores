{-|
Module      : Clash.Cores.Ethernet.Mac.EthernetTypes
Description : Provides various data types, aliases and constants for the Ethernet protocol.
-}
module Clash.Cores.Ethernet.Mac.EthernetTypes
  ( MacAddress(..)
  , EthernetHeader(..)
  , broadcastMac
  , constToEthernetC
  ) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream

import Control.DeepSeq ( NFData )


-- | Stores a MAC address, which is always 6 bytes long.
newtype MacAddress = MacAddress (Vec 6 (BitVector 8))
  deriving (BitPack, Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | Stores a link-layer Ethernet header, that is, a destination MAC address,
--   a source MAC address, and an EtherType.
data EthernetHeader = EthernetHeader {
  _macDst :: MacAddress,
  _macSrc :: MacAddress,
  _etherType :: BitVector 16
} deriving (BitPack, Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | Broadcast MAC address.
broadcastMac :: MacAddress
broadcastMac = MacAddress (repeat 0xFF)

{- |
Convert an arbitrary stream to an Ethernet stream with a hardcoded destination
MAC address.
-}
constToEthernetC ::
  HiddenClockResetEnable dom
  -- | EtherType
  => BitVector 16
  -- | Hardcoded Target MAC address
  -> MacAddress
  -- | Our MAC address
  -> Signal dom MacAddress
  -> Circuit
      (PacketStream dom dataWidth meta)
      (PacketStream dom dataWidth EthernetHeader)
constToEthernetC etherType macDst ourMac = mapMetaS (const <$> hdr)
  where
    hdr = (\src -> EthernetHeader
      {
      _macDst = macDst,
      _macSrc = src,
      _etherType = etherType
      }) <$> ourMac
