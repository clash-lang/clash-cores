{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides circuits to insert, validate and strip the FCS of Ethernet frames.
-}
module Clash.Cores.Ethernet.Mac.FrameCheckSequence (
  fcsInserterC,
  fcsValidatorC,
  fcsStripperC,
) where

import Clash.Cores.Crc (HardwareCrc, crcEngine, crcValidator)
import Clash.Cores.Crc.Catalog (Crc32_ethernet (..))

import Clash.Prelude

import Clash.Sized.Vector.Extra (appendVec)

import Data.Maybe
import Data.Maybe.Extra (toMaybe)
import Data.Type.Equality ((:~:) (Refl))

import Protocols
import Protocols.PacketStream
import Protocols.PacketStream.Extra (dropTailC)

toCrcInput ::
  (KnownNat dataWidth) =>
  -- | Enable
  Bool ->
  -- | Start of new CRC
  Bool ->
  -- | Transaction to feed
  PacketStreamM2S dataWidth () ->
  Maybe (Bool, Index dataWidth, Vec dataWidth (BitVector 8))
toCrcInput en isFirst PacketStreamM2S{..} =
  toMaybe
    (en && _last /= Just 0)
    (isFirst, maybe maxBound (resize . (\i -> i - 1)) _last, _data)

-- | State of 'fcsInserterT'.
data FcsInserterState dataWidth
  = FcsCopy
      {_cachedFwd :: Maybe (PacketStreamM2S dataWidth ())}
  | FcsInsert
      { _aborted :: Bool
      , _cachedFwd :: Maybe (PacketStreamM2S dataWidth ())
      , _valid :: Index 5
      -- ^ how many bytes of _cachedCrc are valid
      , _cachedCrc :: Vec 4 (BitVector 8)
      }
  deriving (Show, Generic, NFDataX)

-- | State transition function of 'fcsInserterC'.
fcsInserterT ::
  forall dataWidth.
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  FcsInserterState dataWidth ->
  ( Vec 4 (BitVector 8)
  , Maybe (PacketStreamM2S dataWidth ())
  , PacketStreamS2M
  ) ->
  ( FcsInserterState dataWidth
  , ( Maybe (PacketStreamM2S dataWidth ())
    , Bool
    )
  )
fcsInserterT (FcsCopy Nothing) (_, fwdIn, _) = (FcsCopy fwdIn, (Nothing, True))
fcsInserterT st@(FcsCopy (Just cache@(PacketStreamM2S{..}))) (ethCrcBytes, fwdIn, bwdIn) =
  (nextStOut, (Just fwdOut, _ready bwdIn))
 where
  (combined, leftover) = splitAtI $ appendVec (fromJust _last) _data ethCrcBytes

  nextLast :: Index (dataWidth + 1) -> Maybe (Index (dataWidth + 1))
  nextLast i = case compareSNat d4 (SNat @dataWidth) of
    SNatLE -> toMaybe (i <= natToNum @(dataWidth - 4)) (i + 4)
    _ -> Nothing

  insertCrc = nextLast <$> _last

  fwdOut = case insertCrc of
    Just l -> cache{_data = combined, _last = l}
    Nothing -> cache

  nextSt =
    if maybe True isJust insertCrc
      then FcsCopy fwdIn
      else
        FcsInsert
          { _aborted = _abort
          , _cachedFwd = fwdIn
          , _valid = 4 - resize (maxBound - fromJustX _last)
          , _cachedCrc = leftover
          }

  nextStOut = if _ready bwdIn then nextSt else st
fcsInserterT st@(FcsInsert{..}) (_, _, bwdIn) = (nextStOut, (Just dataOut, False))
 where
  finished = _valid <= natToNum @(Min dataWidth 4)
  (outBytes, nextBytes) = splitAtI $ _cachedCrc ++ repeat 0x00
  dataOut =
    PacketStreamM2S
      { _data = outBytes
      , _last = toMaybe finished (resize _valid)
      , _meta = ()
      , _abort = _aborted
      }

  nextSt =
    if finished
      then FcsCopy _cachedFwd
      else
        st
          { _valid = _valid - natToNum @dataWidth
          , _cachedCrc = nextBytes
          }

  nextStOut = if _ready bwdIn then nextSt else st

{- |
Computes the Ethernet CRC ('Crc32_ethernet') over each packet in the stream
and appends this CRC to the corresponding packet in the output stream.

__NB__: does not support zero-byte packets. Feeding a zero-byte packet to this
component will result in /undefined behaviour/.
-}
fcsInserterC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
  (KnownNat dataWidth) =>
  -- | Ethernet FCS inserter circuit
  Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsInserterC = forceResetSanity |> fromSignals go
 where
  go (fwdIn, bwdIn) = (bwdOut, fwdOut)
   where
    fwdInX = fromJustX <$> fwdIn
    transferOccured = ready .&&. isJust <$> fwdIn
    crcIn = liftA3 toCrcInput transferOccured isFirst fwdInX

    isFirst = regEn True transferOccured $ isJust . _last <$> fwdInX
    ethCrc = crcEngine Crc32_ethernet crcIn
    ethCrcBytes = reverse . unpack <$> ethCrc

    bwdOut = PacketStreamS2M <$> ready

    (fwdOut, ready) = mealyB fcsInserterT (FcsCopy Nothing) (ethCrcBytes, fwdIn, bwdIn)

-- | State of 'fcsValidatorT'.
newtype FcsValidatorState dataWidth = FcsValidatorState
  { _cachedFwd :: Maybe (PacketStreamM2S dataWidth ())
  }
  deriving (Show, Generic, NFDataX)

-- | State transition function of 'fcsValidator'.
fcsValidatorT ::
  forall (dataWidth :: Nat).
  (KnownNat dataWidth) =>
  FcsValidatorState dataWidth ->
  ( Bool
  , Maybe (PacketStreamM2S dataWidth ())
  , PacketStreamS2M
  ) ->
  ( FcsValidatorState dataWidth
  , (Bool, Maybe (PacketStreamM2S dataWidth ()))
  )
fcsValidatorT st@FcsValidatorState{..} (valid, fwdIn, bwdIn) =
  (nextSt, (readyOut, fwdOut))
 where
  fwdOut =
    ( \pkt ->
        if isJust (_last pkt)
          then pkt{_abort = _abort pkt || not valid}
          else pkt
    )
      <$> _cachedFwd

  readyOut = isNothing _cachedFwd || _ready bwdIn

  nextSt
    | isNothing fwdOut || _ready bwdIn = FcsValidatorState fwdIn
    | otherwise = st

fcsValidator ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
  ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
  , Signal dom PacketStreamS2M
  ) ->
  ( Signal dom PacketStreamS2M
  , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
  )
fcsValidator (fwdIn, bwdIn) = (PacketStreamS2M <$> ready, fwdOut)
 where
  fwdInX = fromJustX <$> fwdIn
  crcEnable = isJust <$> fwdIn .&&. ready
  valid = crcValidator Crc32_ethernet crcIn
  crcIn = liftA3 toCrcInput crcEnable isFirst fwdInX
  isFirst = regEn True crcEnable (isJust . _last <$> fwdInX)

  (ready, fwdOut) =
    mealyB
      fcsValidatorT
      (FcsValidatorState Nothing)
      (valid, fwdIn, bwdIn)

{- |
Computes the Ethernet CRC ('Crc32_ethernet') over each packet in the stream
and asserts '_abort' on the last transfer of the packet if the computed CRC
did not match the last 4 bytes of the stream.

__NB__: does not remove the FCS field (last 4 bytes of the stream).
Use 'fcsStripperC' for that.

__NB__: does not support zero-byte packets. Feeding a zero-byte packet to this
component will result in /undefined behaviour/.
-}
fcsValidatorC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
  -- | Ethernet FCS validator circuit
  Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsValidatorC = case sameNat d1 (SNat @dataWidth) of
  Just Refl ->
    forceResetSanity
      |> fromSignals fcsValidator
  Nothing ->
    -- If the data width is bigger than 1 byte, null bytes have to be set to
    -- 0x00 for the CRC validator to work. At data width 1 it is not a problem,
    -- because then we only feed valid bytes to the CRC validator.
    forceResetSanity
      |> zeroOutInvalidBytesC
      |> fromSignals fcsValidator

{- |
Removes the last 4 bytes of each packet in the stream, the width of the
Ethernet FCS field. This is just a specialized version of 'dropTailC'.

__NB__: does not validate the FCS field. Use 'fcsValidatorC' for that.
-}
fcsStripperC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- | Ethernet FCS stripper circuit
  Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsStripperC = dropTailC d4
