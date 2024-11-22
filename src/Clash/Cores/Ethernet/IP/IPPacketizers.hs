{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Specialized packetizer and depacketizer for IP headers.
-}
module Clash.Cores.Ethernet.IP.IPPacketizers (
  ipPacketizerC,
  ipLitePacketizerC,
  ipDepacketizerC,
  ipDepacketizerLiteC,
) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream

import Clash.Cores.Ethernet.IP.IPv4Types
import Clash.Cores.Ethernet.InternetChecksum
import Clash.Cores.Ethernet.Mac.EthernetTypes

import Data.Functor
import Data.Maybe
import Data.Type.Equality

{- |
Prepends a full IPv4 header to each packet in the stream. Before prepending
the header, the header checksum is computed and set.

Fields that are not in the partial `IPv4HeaderLite` are given specific values,
see `fromLite`.
-}
ipLitePacketizerC ::
  forall (dom :: Domain) (dataWidth :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth IPv4HeaderLite)
    (PacketStream dom dataWidth IPv4Address)
ipLitePacketizerC = mapMeta fromLite |> ipPacketizerC

{- |
Prepends a full IPv4 header to each packet in the stream. Before prepending
the header, the header checksum is computed and set.
-}
ipPacketizerC ::
  forall (dom :: Domain) (dataWidth :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth IPv4Header)
    (PacketStream dom dataWidth IPv4Address)
ipPacketizerC = setChecksumC |> packetizerC _ipv4Destination id

-- | Internal state of `setChecksumC`
data ChecksumS
  = -- | Waiting for new packet
    Wait
  | -- | Computing checksum
    Compute
  | -- | Forwarding data
    Forward (BitVector 16)
  deriving (Eq, Generic, NFDataX)

-- | Compute the IPv4 header checksum and set it in the metadata.
setChecksumC ::
  forall dom dataWidth.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  Circuit (PacketStream dom dataWidth IPv4Header) (PacketStream dom dataWidth IPv4Header)
setChecksumC = Circuit $ \(fwdInS, bwdInS) ->
  let
    s = register Wait s'
    (s', unbundle -> (bwdOutS, fwdOutS)) =
      unbundle $ bundle (s, fwdInS, bwdInS, counter, checksum) <&> \case
        (Wait, Just _, _, _, _) -> (Compute, stall)
        (Wait, _, _, _, _) -> (Wait, stall)
        (Compute, fwdIn, bwdIn, 0, c) -> go c fwdIn bwdIn
        (Compute, _, _, _, _) -> (Compute, stall)
        (Forward c, fwdIn, bwdIn, _, _) -> go c fwdIn bwdIn

    go c fwdIn bwdIn = (s'', (bwdIn, replaceChecksum c fwdIn))
     where
      s''
        | isJust fwdIn && isJust (_last (fromJustX fwdIn)) = Wait
        | otherwise = Forward c

    stall = (PacketStreamS2M False, Nothing)
    replaceChecksum c mp = ((\h -> h{_ipv4Checksum = c}) <$>) <$> mp

    -- Calculating the checksum
    replaceBuffer = (s .==. pure Wait) .&&. isJust <$> fwdInS
    ipHeader = bitCoerce . _meta . fromJustX <$> fwdInS
    buffer :: Signal dom (Vec 10 (BitVector 16))
    counter :: Signal dom (Index 11)
    buffer =
      register
        (repeat defaultBytes)
        (mux replaceBuffer ipHeader ((<<+ defaultBytes) <$> buffer))
    counter = register 0 $ mux replaceBuffer 10 (satPred SatBound <$> counter)
    checksum = complement <$> internetChecksum replaceBuffer (Just . head <$> buffer)

    defaultBytes = deepErrorX "ipPacketizerC: undefined value in header register"
   in
    (bwdOutS, fwdOutS)

{- |
Parses the first 20 bytes of each packet in the stream into an `IPv4Header`
and puts it in the metadata. The header checksum is verified. Fragmentation
and options are not supported.

A packet is aborted when:

- The checksum does not match;
- The version field is not @4@;
- The IHL field is not @5@ (options are given);
- The MF (More Fragments) bit is set.
-}
ipDepacketizerC ::
  forall (dom :: Domain) (dataWidth :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth EthernetHeader)
    (PacketStream dom dataWidth IPv4Header)
ipDepacketizerC = verifyChecksum |> depacketizerC const |> verifyIPHdr
 where
  verifyIPHdr = Circuit $ \(fwdIn, bwdIn) -> (bwdIn, (go <$>) <$> fwdIn)
  go p =
    let
      header = _meta p
      abort =
        (_ipv4Ihl header /= 5)
          || (_ipv4Version header /= 4)
          || _ipv4FlagReserved header
          || _ipv4FlagMF header
     in
      p{_abort = _abort p || abort}

-- | Like `ipDepacketizerC`, but only keeps some of the IPv4 header fields.
ipDepacketizerLiteC ::
  forall (dom :: Domain) (dataWidth :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth EthernetHeader)
    (PacketStream dom dataWidth IPv4HeaderLite)
ipDepacketizerLiteC = ipDepacketizerC |> mapMeta toLite

data VerifyChecksumS n
  = -- | Checking. Contains accumulator, remaining bytes, and byte from previous
    -- packet in case of odd data widths
    DeCheck (BitVector 16) (Index (20 `DivRU` n)) (BitVector 8)
  | -- | Forwarding. Bool is True if checksum was invalid
    DeForward Bool
  deriving (Generic, NFDataX)

{- | Verifies the internet checksum of the first 20 bytes of each packet. Sets
the abort bit from byte 21 onwards if the checksum is invalid. Data is left
intact.
-}
verifyChecksum ::
  forall dom n.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , 1 <= n
  ) =>
  Circuit (PacketStream dom n EthernetHeader) (PacketStream dom n EthernetHeader)
verifyChecksum = case (divProof, divModProof, modLeProof) of
  (SNatLE, Just Refl, SNatLE) -> Circuit $ mealyB go (DeCheck 0 0 undefined)
  _ ->
    clashCompileError
      "ipDepacketizerC: absurd in verifyChecksum: 2 * (n `Div` 2) + n `Mod` 2 not equal to n"
 where
  divProof = compareSNat d1 (SNat @(20 `DivRU` n))
  divModProof = sameNat (SNat @n) (SNat @(2 * (n `Div` 2) + n `Mod` 2))
  modLeProof = compareSNat (SNat @(20 `Mod` n)) (SNat @n)

  go ::
    (2 * (n `Div` 2) + n `Mod` 2 ~ n, 20 `Mod` n <= n) =>
    (1 <= 20 `DivRU` n) =>
    VerifyChecksumS n ->
    (Maybe (PacketStreamM2S n EthernetHeader), PacketStreamS2M) ->
    (VerifyChecksumS n, (PacketStreamS2M, Maybe (PacketStreamM2S n EthernetHeader)))
  go (DeForward invalid) (fwd, bwd) = (nextSt, (bwd, (\p -> p{_abort = _abort p || invalid}) <$> fwd))
   where
    nextSt =
      if isJust fwd && _ready bwd && isJust (_last $ fromJustX fwd)
        then DeCheck 0 0 undefined
        else DeForward invalid
  go s@(DeCheck{}) (Nothing, bwd) = (s, (bwd, Nothing))
  go s@(DeCheck acc i byte) (Just fwdIn, PacketStreamS2M inReady) = (s', (PacketStreamS2M inReady, Just fwdOut))
   where
    finalHeaderFragment = i == maxBound

    -- Set all data bytes to zero
    (dataLo, dataHi0) = splitAtI @(20 `Mod` n) @(n - 20 `Mod` n) $ _data fwdIn
    dataHi1 = if finalHeaderFragment then repeat 0 else dataHi0
    -- If our datawidth is aligned we don't need to partial checksum of the data
    header = case sameNat d0 (SNat @(20 `Mod` n)) of
      Just Refl -> _data fwdIn
      _ -> dataLo ++ dataHi1

    -- We verify that 2 * n Div 2 + n Mod 2 = n, and distinguish between
    -- even and odd data widths. For odd data widths, we have to save the
    -- extra byte in even packets and use it in the odd packets.
    (acc', byte') =
      case ( sameNat (SNat @(n `Mod` 2)) d0
           , sameNat (SNat @(n `Mod` 2)) d1
           ) of
        (Just Refl, _) ->
          -- n mod 2 = 0
          let xs :: Vec (n `Div` 2) (BitVector 16)
              xs = bitCoerce header
           in (fold onesComplementAdd (acc :> xs), undefined)
        (_, Just Refl) ->
          -- n mod 2 = 1
          let xs :: Vec (n `Div` 2 + 1) (BitVector 16)
              xs = bitCoerce $ if even i then init header :< 0 :< 0 else byte :> header
           in (fold onesComplementAdd (acc :> xs), last header)
        _ ->
          clashCompileError
            "ipDepacketizerC: absurd in verifyChecksum: n `Mod` 2 not equal to 0 or 1"

    invalid = acc' /= 0xFFFF -- Note that we haven't taken the complement
    s'
      | not inReady = s
      | isJust (_last fwdIn) = DeCheck 0 0 undefined
      | finalHeaderFragment = DeForward invalid
      | otherwise = DeCheck acc' (succ i) byte'

    fwdOut =
      if finalHeaderFragment
        then fwdIn{_abort = _abort fwdIn || invalid}
        else fwdIn
