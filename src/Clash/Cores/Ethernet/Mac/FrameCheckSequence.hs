{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

{-|
Module      : Clash.Cores.Ethernet.Mac.FrameCheckSequence
Description : Provides circuits to insert, validate and strip the FCS of an Ethernet frame.
-}
module Clash.Cores.Ethernet.Mac.FrameCheckSequence (
  fcsInserterC,
  fcsValidatorC,
  fcsStripperC,
) where

-- crc
import Clash.Cores.Crc (crcEngine, crcValidator, HardwareCrc)
import Clash.Cores.Crc.Catalog (Crc32_ethernet(..))

-- vector
import Clash.Sized.Vector.Extra (appendVec)

-- prelude
import Clash.Prelude

-- maybe
import Data.Maybe
import Data.Maybe.Extra

-- protocols
import Protocols
import Protocols.PacketStream

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
  toMaybe en (isFirst, fromMaybe maxBound _last, _data)

fcsInserterT
  :: forall dataWidth
  . KnownNat dataWidth
  => 1 <= dataWidth
  => FcsInserterState dataWidth
  -> ( Vec 4 (BitVector 8)
     , Maybe (PacketStreamM2S dataWidth ())
     , PacketStreamS2M)
  -> ( FcsInserterState dataWidth
     , ( Maybe (PacketStreamM2S dataWidth ())
       , Bool))
fcsInserterT (FcsCopy Nothing) ( _, fwdIn, _) = (FcsCopy fwdIn, (Nothing, True))

fcsInserterT st@(FcsCopy (Just cache@(PacketStreamM2S{..}))) (ethCrcBytes, fwdIn, PacketStreamS2M readyIn)
  = (nextSt, (Just fwdOut, readyIn))
  where
    (combined, leftover) = splitAtI $ appendVec (fromJust _last) _data ethCrcBytes

    nextLast i = case compareSNat d5 (SNat @dataWidth) of
      SNatLE -> toMaybe (i < natToNum @(dataWidth - 4)) $ i + 4
      _ -> Nothing

    insertCrc = nextLast <$> _last

    fwdOut = case insertCrc of
      Just l -> cache { _data = combined, _last = l }
      Nothing -> cache

    nextStIfReady = if maybe True isJust insertCrc
      then FcsCopy fwdIn
      else FcsInsert
        { _aborted = _abort
        , _cachedFwd = fwdIn
        -- Since we know we are in a case where we are not transmitting the entire CRC out
        -- it's guaranteed that dataWidth - 4 <= lastIdx <= dataWidth - 1
        -- This means we don't need to look at entire state space of the index.
        -- Only the last 2 bits matter. But since dataWidth might not be 4 byte
        -- aligned we need to wrapping subtract Mod dataWidth 4 to align the index.
        -- Normally wrapping subtract is relatively expensive but since 4
        -- is a power of two we get it for free. But it means we have to do
        -- arithmetic with BitVector/Unsigned type and not index.
        --
        -- We could go even further beyond and just pass through the last 2 bits without
        -- correction and handle that in `FcsInsert`.
        , _valid = unpack $ resize (pack $ fromJustX _last) - natToNum @(Mod dataWidth 4)
        , _cachedCrc = leftover
        }

    nextSt = if readyIn then nextStIfReady else st

fcsInserterT st@(FcsInsert{..}) (_, _, PacketStreamS2M readyIn) = (nextSt, (Just dataOut, False))
  where
    finished = _valid <= natToNum @(Min (dataWidth - 1) 3)
    (outBytes, nextBytes) = splitAtI $ _cachedCrc ++ repeat 0
    dataOut = PacketStreamM2S
      { _data = outBytes
      , _last = toMaybe finished $ resize _valid
      , _meta = ()
      , _abort = _aborted
      }

    nextStIfReady =
      if finished
        then FcsCopy _cachedFwd
        else st
          { _valid =  _valid - natToNum @dataWidth
          , _cachedCrc = nextBytes
          }

    nextSt = if readyIn then nextStIfReady else st

-- | States of the FcsInserter
data FcsInserterState dataWidth
  = FcsCopy
      { _cachedFwd :: Maybe (PacketStreamM2S dataWidth ()) }
  | FcsInsert
      { _aborted :: Bool
      , _cachedFwd :: Maybe (PacketStreamM2S dataWidth ())
      , _valid :: Index 4
      -- ^ how many bytes of _cachedCrc are valid
      , _cachedCrc :: Vec 4 (BitVector 8)
      }
  deriving (Show, Generic, NFDataX)

-- | fcsInserter
fcsInserter
  :: forall (dataWidth :: Nat) (dom :: Domain)
  .  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M
     )
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
fcsInserter (fwdIn, bwdIn) = (bwdOut, fwdOut)
  where
    fwdInX = fromJustX <$> fwdIn
    transferOccured = ready .&&. isJust <$> fwdIn
    crcIn = liftA3 toCrcInput transferOccured isFirst fwdInX

    isFirst = regEn True transferOccured $ isJust . _last <$> fwdInX
    ethCrc = crcEngine Crc32_ethernet crcIn
    ethCrcBytes = reverse . unpack <$> ethCrc

    bwdOut = PacketStreamS2M <$> ready

    (fwdOut, ready) = mealyB fcsInserterT (FcsCopy Nothing) (ethCrcBytes, fwdIn, bwdIn)

{- |
Computes the Ethernet CRC (4 bytes) of each packet in the input stream and
appends this CRC to the corresponding packet in the output stream.
-}
fcsInserterC
  :: forall (dataWidth :: Nat) (dom :: Domain)
  .  KnownDomain dom
  => KnownNat dataWidth
  => HiddenClockResetEnable dom
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsInserterC = forceResetSanity |> fromSignals fcsInserter

-- | State of 'fcsValidatorT'.
newtype FcsValidatorState dataWidth = FcsValidatorState
  { _cachedFwd :: Maybe (PacketStreamM2S dataWidth ())
  }
  deriving (Show, Generic, NFDataX)

-- | State transition function of 'fcsValidator'.
fcsValidatorT ::
  forall (dataWidth :: Nat).
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
  (KnownNat dataWidth) =>
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
-}
fcsValidatorC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (HardwareCrc Crc32_ethernet 8 dataWidth) =>
  Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsValidatorC = forceResetSanity |> fromSignals fcsValidator

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
  Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsStripperC = dropTailC d4
