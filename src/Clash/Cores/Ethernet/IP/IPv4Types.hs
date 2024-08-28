{-# language RecordWildCards #-}

{-|
Module      : Clash.Cores.Ethernet.IP.IPv4Types
Description : Provides various data types, aliases and constants for IPv4.
-}
module Clash.Cores.Ethernet.IP.IPv4Types
  ( IPv4Address(..)
  , IPv4Header(..)
  , IPv4HeaderLite(..)
  , toLite
  , toLiteC
  , fromLite
  , fromLiteC
  , ipv4Broadcast
  ) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream

import Control.DeepSeq ( NFData )
import qualified Data.Bifunctor as B
import Data.Tuple

-- | IPv4 address.
newtype IPv4Address = IPv4Address (Vec 4 (BitVector 8))
  deriving (Generic, Show, ShowX, NFDataX, NFData, Eq, BitPack)

bitCoerceMap2
  :: forall a b
   . BitPack a
  => BitPack b
  => BitSize a ~ BitSize b
  => (a -> a -> a)
  -> b -> b -> b
bitCoerceMap2 f x y = bitCoerce $ f (bitCoerce x) (bitCoerce y)

-- | `Bits` instance, borrowed from `BitVector`.
instance Bits IPv4Address where
  (.&.) = bitCoerceMap2 @(BitVector 32) (.&.)
  (.|.) = bitCoerceMap2 @(BitVector 32) (.|.)
  xor = bitCoerceMap2 @(BitVector 32) xor
  complement = bitCoerceMap @(BitVector 32) complement
  shift a n = bitCoerceMap @(BitVector 32) (`shift` n) a
  rotate a n = bitCoerceMap @(BitVector 32) (`rotate` n) a
  bitSize = finiteBitSize . bitCoerce @IPv4Address @(BitVector 32)
  bitSizeMaybe = bitSizeMaybe . bitCoerce @IPv4Address @(BitVector 32)
  isSigned = isSigned . bitCoerce @IPv4Address @(BitVector 32)
  testBit = testBit . bitCoerce @IPv4Address @(BitVector 32)
  bit = bitCoerce @(BitVector 32) . bit
  popCount = popCount . bitCoerce @IPv4Address @(BitVector 32)


-- | (Almost) full IPv4 header. Does not contain options field.
data IPv4Header = IPv4Header
  { _ipv4Version :: BitVector 4
  , _ipv4Ihl :: Unsigned 4
  , _ipv4Dscp :: BitVector 6
  , _ipv4Ecn :: BitVector 2
  , _ipv4Length :: Unsigned 16
  , _ipv4Id :: BitVector 16
  , _ipv4FlagReserved :: Bool
  , _ipv4FlagDF :: Bool
  , _ipv4FlagMF :: Bool
  , _ipv4FragmentOffset :: BitVector 13
  , _ipv4Ttl :: Unsigned 8
  , _ipv4Protocol :: Unsigned 8
  , _ipv4Checksum :: BitVector 16
  , _ipv4Source :: IPv4Address
  , _ipv4Destination :: IPv4Address
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

-- | Partial IPv4 header.
data IPv4HeaderLite = IPv4HeaderLite
  { _ipv4lSource :: IPv4Address
  , _ipv4lDestination :: IPv4Address
  , _ipv4lProtocol :: Unsigned 8
  , _ipv4lPayloadLength :: Unsigned 16
  } deriving (Show, ShowX, Eq, Generic, BitPack, NFDataX, NFData)

toLite :: IPv4Header -> IPv4HeaderLite
toLite IPv4Header {..} = IPv4HeaderLite
  { _ipv4lSource = _ipv4Source
  , _ipv4lDestination = _ipv4Destination
  , _ipv4lProtocol = _ipv4Protocol
  , _ipv4lPayloadLength = _ipv4Length - 20 -- We do not support IHLs other than 5
  }

-- | Shrinks IPv4 headers
toLiteC :: Circuit (PacketStream dom n IPv4Header) (PacketStream dom n IPv4HeaderLite)
toLiteC = Circuit (swap . unbundle . go . bundle)
  where
    go = fmap $ B.first $ fmap $ fmap toLite

fromLite :: IPv4HeaderLite -> IPv4Header
fromLite header = IPv4Header { _ipv4Version = 4
                             , _ipv4Ihl = ipv4Ihl
                             , _ipv4Dscp = 0
                             , _ipv4Ecn = 0
                             , _ipv4Length = _ipv4lPayloadLength header + 20
                             , _ipv4Id = 0
                             , _ipv4FlagReserved = False
                             , _ipv4FlagDF = False
                             , _ipv4FlagMF = False
                             , _ipv4FragmentOffset = 0
                             , _ipv4Ttl = 64
                             , _ipv4Protocol = _ipv4lProtocol header
                             , _ipv4Checksum = 0
                             , _ipv4Source = _ipv4lSource header
                             , _ipv4Destination = _ipv4lDestination header
                             }
  where
    ipv4Ihl = 5

-- | Produce a full IPv4 header from a lite one.
--   Note that this does *not* compute the checksum.
fromLiteC :: Circuit (PacketStream dom n IPv4HeaderLite) (PacketStream dom n IPv4Header)
fromLiteC = Circuit (swap . unbundle . go . bundle)
  where
    go = fmap $ B.first $ fmap $ fmap fromLite

-- | Computes the IPv4 broadcast address.
ipv4Broadcast
  :: IPv4Address
  -- ^ Host address
  -> IPv4Address
  -- ^ Subnet mask
  -> IPv4Address
  -- ^ Broadcast address
ipv4Broadcast address subnet = address .|. complement subnet
