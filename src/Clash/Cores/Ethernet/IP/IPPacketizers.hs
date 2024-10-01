{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# OPTIONS_HADDOCK hide #-}

{-|
Module      : Clash.Cores.Ethernet.IP.IPPacketizers
Description : Specialized packetizer and depacketizer for IP headers.
-}
module Clash.Cores.Ethernet.IP.IPPacketizers
  ( ipPacketizerC
  , ipLitePacketizerC
  , ipDepacketizerC
  , ipDepacketizerLiteC
  , verifyChecksumC
  ) where

import Clash.Prelude

import Clash.Cores.Ethernet.InternetChecksum
import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.Mac.EthernetTypes

import qualified Data.Bifunctor as B
import Data.Functor
import Data.Maybe
import Data.Type.Equality (type (==))

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream
import GHC.TypeLits.KnownNat (KnownBool)

-- | Packetize a packet stream with the IPv4HeaderLite meta data
-- giving default values for header data that are not in IPv4HeaderLite.
ipLitePacketizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat) .
  ( HiddenClockResetEnable dom
  , KnownDomain dom
  , 1 <= dataWidth
  , KnownNat dataWidth)
  => Circuit (PacketStream dom dataWidth IPv4HeaderLite) (PacketStream dom dataWidth IPv4Address)
ipLitePacketizerC = fromLiteC |> ipPacketizerC

-- | Packetize a packet stream with the IPv4Header meta data.
ipPacketizerC
  :: forall (dom :: Domain)
            (dataWidth :: Nat)
   . ( HiddenClockResetEnable dom
     , KnownDomain dom
     , 1 <= dataWidth
     , KnownNat dataWidth
     )
  => Circuit (PacketStream dom dataWidth IPv4Header) (PacketStream dom dataWidth IPv4Address)
ipPacketizerC = setChecksumC |> packetizerC _ipv4Destination id

-- | Internal state of `setChecksumC`
data ChecksumS
  = Wait    -- ^ Waiting for new packet
  | Compute -- ^ Computing checksum
  | Forward (BitVector 16) -- ^ Forwarding data
  deriving (Eq, Generic, NFDataX)

-- | Set the checksum in the IPv4Header of the metatype
setChecksumC
  :: forall dom dataWidth
   . HiddenClockResetEnable dom
  => KnownNat dataWidth
  => KnownDomain dom
  => Circuit (PacketStream dom dataWidth IPv4Header) (PacketStream dom dataWidth IPv4Header)
setChecksumC = Circuit $ \(fwdInS, bwdInS) ->
  let
    s = register Wait s'
    (s', unbundle -> (bwdOutS, fwdOutS)) = unbundle $ bundle (s, fwdInS, bwdInS, counter, checksum) <&> \case
      (Wait, Just _, _, _, _)         -> (Compute, stall)
      (Wait, _, _, _, _)              -> (Wait, stall)
      (Compute, fwdIn, bwdIn, 0, c)   -> go c fwdIn bwdIn
      (Compute, _, _, _, _)           -> (Compute, stall)
      (Forward c, fwdIn, bwdIn, _, _) -> go c fwdIn bwdIn

    go c fwdIn bwdIn = (s'', (bwdIn, replaceChecksum c fwdIn))
      where
        s'' | isJust fwdIn && isJust (_last (fromJustX fwdIn)) = Wait
            | otherwise = Forward c

    stall = (PacketStreamS2M False, Nothing)
    replaceChecksum c mp = ((\h -> h {_ipv4Checksum = c}) <$>) <$> mp

    -- Calculating the checksum
    replaceBuffer = (s .==. pure Wait) .&&. isJust <$> fwdInS
    ipHeader = bitCoerce . _meta . fromJustX <$> fwdInS
    buffer :: Signal dom (Vec 10 (BitVector 16))
    counter :: Signal dom (Index 11)
    buffer = register (ensureSpine defaultBytes) (mux replaceBuffer ipHeader ((<<+ defaultBytes) <$> buffer))
    counter = register 0 $ mux replaceBuffer 10 (satPred SatBound <$> counter)
    checksum = complement <$> internetChecksum replaceBuffer (Just . head <$> buffer)

    defaultBytes = errorX "ipPacketizerC: undefined value in header register"
   in (bwdOutS, fwdOutS)

-- | Parses the IPv4 header. Does not support parsing options in the header.
-- If the checksum is invalid or options are given, the abort bit is set.
ipDepacketizerC
  :: forall (dom :: Domain) (n :: Nat)
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     )
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n IPv4Header)
ipDepacketizerC = depacketizerC const |> verifyIPHdr
  where
    verifyIPHdr = Circuit $ \(fwdIn, bwdIn) -> (bwdIn, (go <$>) <$> fwdIn)
    go p =
      let
       header = _meta p
       abort =
         _ipv4Ihl header /= 5 ||
         _ipv4Version header /= 4 ||
         _ipv4FlagReserved header ||
         _ipv4FlagMF header
      in p {_abort = _abort p || abort}

-- | Version of `ipDepacketizerC` that only keeps some of the IPv4 header fields.
ipDepacketizerLiteC
  :: forall (dom :: Domain) (n :: Nat)
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     )
  => Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n IPv4HeaderLite)
ipDepacketizerLiteC = ipDepacketizerC |> toLiteC

{- |
Verify the IPv4 checksum.
-}
verifyChecksumC ::
  forall dataWidth meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownBool (CmpNat dataWidth 20 == LT)) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (NFDataX meta) =>
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
verifyChecksumC = circuit $ \stream -> do
  [s1, s2] <- fanout -< stream
  delayed <- delayCkt -< s1
  checksum <- calculateChecksumC -< s2
  verifyChecksumC' -< (delayed, checksum)
 where
  delayCkt = case compareSNat d2 (SNat @(20 `DivRU` dataWidth)) of
    SNatLE -> delayStreamC (SNat @(20 `DivRU` dataWidth - 1))
    SNatGT -> idC

data VerifyChecksumState = Idle | DropPacket | Forward'
  deriving (Generic, NFDataX, Show, ShowX)

{- |
Not full throughput.
-}
verifyChecksumC' ::
  forall dataWidth meta dom.
  (HiddenClockResetEnable dom) =>
  (NFDataX meta) =>
  Circuit
    (PacketStream dom dataWidth meta, Df dom (BitVector 16))
    (PacketStream dom dataWidth meta)
verifyChecksumC' = Circuit (B.first unbundle . mealyB go Idle . B.first bundle)
 where
  go Idle ((_, checksumM), _) = (nextSt, ((PacketStreamS2M False, Ack True), Nothing))
   where
    nextSt = case checksumM of
      Df.NoData -> Idle
      Df.Data checksum -> if checksum == 0 then Forward' else DropPacket

  go Forward' ((fwdIn, _), bwdIn) = (nextSt, ((bwdIn, Ack False), fwdIn))
   where
    nextSt = case fwdIn of
      Just transferIn | isJust (_last transferIn) && _ready bwdIn -> Idle
      _ -> Forward'

  go DropPacket ((fwdIn, _), _) = (nextSt, ((PacketStreamS2M True, Ack False), Nothing))
   where
    nextSt = case fwdIn of
      Just transferIn | isJust (_last transferIn) -> Idle
      _ -> DropPacket
