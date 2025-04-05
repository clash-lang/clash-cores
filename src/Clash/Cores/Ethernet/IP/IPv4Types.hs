{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides various data types, aliases and constants for IPv4.
IPv4 options and fragmentation are not supported.
-}
module Clash.Cores.Ethernet.IP.IPv4Types (
  -- ** Types
  IPv4Address (..),
  IPv4SubnetMask (..),
  IPv4Header (..),
  IPv4HeaderLite (..),

  -- ** Conversion between partial and full headers
  toLite,
  fromLite,

  -- ** Constants
  globalBroadcast,
  localhost,
  unspecified,

  -- ** Subnet broadcast
  subnetBroadcast,

  -- ** Address classification
  isUnspecified,
  isLoopback,
  isPrivate,
  isLinkLocal,
  isBroadcast,
  isMulticast,
) where

import Clash.Prelude

import Control.DeepSeq (NFData)

-- | IPv4 address.
newtype IPv4Address = IPv4Address (Vec 4 (BitVector 8))
  deriving (BitPack, Eq, Generic, NFData, NFDataX)

-- | Prints IPv4 addresses in decimal notation.
instance Show IPv4Address where
  show (IPv4Address addr) = show (map toInteger addr)

instance ShowX IPv4Address where
  showX = show

-- | IPv4 subnet mask.
newtype IPv4SubnetMask = IPv4SubnetMask (Vec 4 (BitVector 8))
  deriving (BitPack, Eq, Generic, NFData, NFDataX)

-- | Prints IPv4 subnet masks in decimal notation.
instance Show IPv4SubnetMask where
  show (IPv4SubnetMask addr) = show (map toInteger addr)

instance ShowX IPv4SubnetMask where
  showX = show

bitCoerceMap2 ::
  forall a b.
  (BitPack a) =>
  (BitPack b) =>
  (BitSize a ~ BitSize b) =>
  (a -> a -> a) ->
  b ->
  b ->
  b
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

-- | `Bits` instance, borrowed from `BitVector`.
instance Bits IPv4SubnetMask where
  (.&.) = bitCoerceMap2 @(BitVector 32) (.&.)
  (.|.) = bitCoerceMap2 @(BitVector 32) (.|.)
  xor = bitCoerceMap2 @(BitVector 32) xor
  complement = bitCoerceMap @(BitVector 32) complement
  shift a n = bitCoerceMap @(BitVector 32) (`shift` n) a
  rotate a n = bitCoerceMap @(BitVector 32) (`rotate` n) a
  bitSize = finiteBitSize . bitCoerce @IPv4SubnetMask @(BitVector 32)
  bitSizeMaybe = bitSizeMaybe . bitCoerce @IPv4SubnetMask @(BitVector 32)
  isSigned = isSigned . bitCoerce @IPv4SubnetMask @(BitVector 32)
  testBit = testBit . bitCoerce @IPv4SubnetMask @(BitVector 32)
  bit = bitCoerce @(BitVector 32) . bit
  popCount = popCount . bitCoerce @IPv4SubnetMask @(BitVector 32)

-- | (Almost) full IPv4 header. Does not contain the options field.
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
  }
  deriving (BitPack, Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | Partial IPv4 header.
data IPv4HeaderLite = IPv4HeaderLite
  { _ipv4lSource :: IPv4Address
  , _ipv4lDestination :: IPv4Address
  , _ipv4lProtocol :: Unsigned 8
  , _ipv4lPayloadLength :: Unsigned 16
  }
  deriving (BitPack, Eq, Generic, NFData, NFDataX, Show, ShowX)

{- |
Convert a full 'IPv4Header' to a partial 'IPv4HeaderLite'. The payload length
is derived from the total length in the IPv4 header minus 20, because we only
support @IHL = 5@.
-}
toLite :: IPv4Header -> IPv4HeaderLite
toLite header =
  IPv4HeaderLite
    { _ipv4lSource = _ipv4Source header
    , _ipv4lDestination = _ipv4Destination header
    , _ipv4lProtocol = _ipv4Protocol header
    , _ipv4lPayloadLength = _ipv4Length header - 20
    }

{- |
Convert a partial 'IPv4HeaderLite' to a full 'IPv4Header', in the following
way:

- TTL is set to @64@;
- Checksum is initialized to @0x0000@;
- IHL is set to @5@;
- Total length is the payload length plus 20;
- Version is set to @4@;
- All fields related to fragmentation, DSCP and ECN are set to @0@.
- All flags are set to @False@.
-}
fromLite :: IPv4HeaderLite -> IPv4Header
fromLite header =
  IPv4Header
    { _ipv4Version = 4
    , _ipv4Ihl = 5
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

-- | Global IPv4 broadcast address, @255.255.255.255@
globalBroadcast :: IPv4Address
globalBroadcast = IPv4Address (repeat 255)

-- | An alias for the usual IPv4 loopback address, @127.0.0.1@
localhost :: IPv4Address
localhost = IPv4Address (127 :> 0 :> 0 :> 1 :> Nil)

-- | An alias for the unspecified IPv4 address, @0.0.0.0@
unspecified :: IPv4Address
unspecified = IPv4Address (repeat 0)

{- |
Computes the IPv4 subnet broadcast address, i.e. the address with all host
bits set.

=== __doctests setup__
>>> import Clash.Prelude

=== Examples
>>> ip1 = IPv4Address (192 :> 168 :> 1 :> 23 :> Nil)
>>> ip2 = IPv4Address (224 :> 0 :> 0 :> 1 :> Nil)

>>> mask1 = IPv4SubnetMask (255 :> 255 :> 255 :> 0 :> Nil)
>>> mask2 = IPv4SubnetMask (240 :> 0 :> 0 :> 0 :> Nil)

>>> subnetBroadcast mask1 ip1
192 :> 168 :> 1 :> 255 :> Nil
>>> subnetBroadcast mask2 ip2
239 :> 255 :> 255 :> 255 :> Nil
-}
subnetBroadcast :: IPv4SubnetMask -> IPv4Address -> IPv4Address
subnetBroadcast mask address = address .|. complement (bitCoerce mask)
{-# INLINE subnetBroadcast #-}

{- |
Returns @True@ if the address is unspecified, i.e. when it is @0.0.0.0@.
For example, nodes which are not yet assigned an address via DHCP may use
this address.

=== __doctests setup__
>>> import Clash.Prelude

=== Examples
>>> isUnspecified (IPv4Address (0 :> 0 :> 0 :> 0 :> Nil))
True
>>> isUnspecified (IPv4Address (192 :> 168 :> 1 :> 1 :> Nil))
False
-}
isUnspecified :: IPv4Address -> Bool
isUnspecified = (==) unspecified
{-# INLINE isUnspecified #-}

{- |
Returns @True@ if the address is a loopback address, i.e. when it is in the
range @127.0.0.0/8@.

This property is defined in
[IETF RFC 1122](https://datatracker.ietf.org/doc/html/rfc1122).

=== __doctests setup__
>>> import Clash.Prelude

=== Examples
>>> isLoopback (IPv4Address (127 :> 0 :> 0 :> 1 :> Nil))
True
>>> isLoopback (IPv4Address (192 :> 168 :> 1 :> 123 :> Nil))
False
-}
isLoopback :: IPv4Address -> Bool
isLoopback (IPv4Address addr) = head addr == 127
{-# INLINE isLoopback #-}

{- |
Returns @True@ if the address is either:

- The global broadcast address (@255.255.255.255@);
- The subnet broadcast address (all host bits set to @1@).

=== __doctests setup__
>>> import Clash.Prelude

=== Examples
>>> subnetMask = IPv4SubnetMask (255 :> 255 :> 255 :> 0 :> Nil)

>>> isBroadcast subnetMask (IPv4Address (192 :> 168 :> 1 :> 255 :> Nil))
True
>>> isBroadcast subnetMask globalBroadcast
True
>>> isBroadcast subnetMask (IPv4Address (192 :> 168 :> 1 :> 254 :> Nil))
False
-}
isBroadcast :: IPv4SubnetMask -> IPv4Address -> Bool
isBroadcast mask addr =
  (bitCoerce addr :: BitVector 32) .|. bitCoerce mask == 0xFFFFFFFF
{-# INLINE isBroadcast #-}

{- |
Returns @True@ if the address is a multicast address, i.e. when it is in the
range @224.0.0.0/4@. More explicitly, the most significant octet ranges from
@224@ to @239@.

This property is defined in
[IETF RFC 5771](https://datatracker.ietf.org/doc/html/rfc5771).

=== __doctests setup__
>>> import Clash.Prelude

=== Examples
>>> isMulticast (IPv4Address (234 :> 120 :> 42 :> 67 :> Nil))
True
-}
isMulticast :: IPv4Address -> Bool
isMulticast (IPv4Address addr) =
  pack (take d4 (bitCoerce addr :: Vec 32 Bit)) == 0b1110
{-# INLINE isMulticast #-}

{- |
Returns @True@ if the address is private. The private address ranges include:

- @10.0.0.0/8@
- @172.16.0.0/12@
- @192.168.0.0/16@

This property is defined in
[IETF RFC 1918](https://datatracker.ietf.org/doc/html/rfc1918).

=== __doctests setup__
>>> import Clash.Prelude

=== Examples
>>> isPrivate (IPv4Address (10 :> 11 :> 12 :> 13 :> Nil))
True
>>> isPrivate (IPv4Address (172 :> 23 :> 4 :> 153 :> Nil))
True
>>> isPrivate (IPv4Address (192 :> 168 :> 1 :> 123 :> Nil))
True
>>> isPrivate (IPv4Address (123 :> 124 :> 125 :> 126 :> Nil))
False
-}
isPrivate :: IPv4Address -> Bool
isPrivate (IPv4Address (a :> b :> _)) =
  (a == 10)
    || (a == 172 && pack (take d4 (unpack b :: Vec 8 Bit)) == 0b0001)
    || (a == 192 && b == 168)
{-# INLINE isPrivate #-}

{- |
Returns @True@ if the input is a link-local address, i.e. when it is in the
range @169.254.0.0/16@.

This property is defined in
[IETF RFC 3927](https://datatracker.ietf.org/doc/html/rfc3927).

=== __doctests setup__
>>> import Clash.Prelude

=== Examples
>>> isLinkLocal (IPv4Address (169 :> 254 :> 140 :> 23 :> Nil))
True
-}
isLinkLocal :: IPv4Address -> Bool
isLinkLocal (IPv4Address (a :> b :> _)) = a == 169 && b == 254
{-# INLINE isLinkLocal #-}
