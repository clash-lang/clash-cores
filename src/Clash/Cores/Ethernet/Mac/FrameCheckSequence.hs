{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}

{-|
Module      : Clash.Cores.Ethernet.Mac.FrameCheckSequence
Description : Provides circuits to insert and validate the FCS of an ethernet frame.
-}
module Clash.Cores.Ethernet.Mac.FrameCheckSequence
  ( fcsInserterC
  , fcsValidatorC
  ) where

-- crc
import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog

-- prelude
import Clash.Prelude

-- maybe
import Data.Maybe

-- vector
import Clash.Sized.Vector.Extra

-- protocols
import Protocols
import Protocols.PacketStream

import Data.Maybe.Extra


toCRCInput
  :: KnownNat dataWidth
  => Bool
  -> PacketStreamM2S dataWidth ()
  -> (Bool, Index dataWidth, Vec dataWidth (BitVector 8))
toCRCInput isFirst (PacketStreamM2S{..}) = (isFirst, fromMaybe maxBound _last, _data)

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
  :: forall (dom :: Domain) (dataWidth :: Nat)
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
    crcIn = toMaybe <$> transferOccured <*> liftA2 toCRCInput isFirst fwdInX

    isFirst = regEn True transferOccured $ isJust . _last <$> fwdInX
    ethCrc = crcEngine Crc32_ethernet crcIn
    ethCrcBytes = reverse . unpack <$> ethCrc

    bwdOut = PacketStreamS2M <$> ready

    (fwdOut, ready) = mealyB fcsInserterT (FcsCopy Nothing) (ethCrcBytes, fwdIn, bwdIn)


-- | Computes the Crc-32 of the packets in the stream and inserts these as four (4) bytes at the end of each
-- packet in the stream.
fcsInserterC
  :: forall (dom :: Domain) (dataWidth :: Nat)
  .  KnownDomain dom
  => KnownNat dataWidth
  => HiddenClockResetEnable dom
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsInserterC = forceResetSanity |> fromSignals fcsInserter

fcsValidatorT
  :: forall dataWidth
  . KnownNat dataWidth
  => FcsValidatorState dataWidth
  -> ( Bool
     , Maybe (PacketStreamM2S dataWidth ())
     , PacketStreamS2M)
  -> ( FcsValidatorState dataWidth
     , ( Maybe (PacketStreamM2S dataWidth ())
       , Bool))
fcsValidatorT (FcsValidatorState Nothing validated) ( _, fwdIn, _) = (FcsValidatorState fwdIn validated, (Nothing, True))

fcsValidatorT st@(FcsValidatorState (Just cache) validated) (valid, fwdIn, PacketStreamS2M readyIn)
  = (nextSt, (Just fwdOut, readyIn))
  where
    outValid = if isJust (_last cache) then valid || validated else valid
    fwdOut = if isJust (_last cache) then cache { _abort = _abort cache|| not outValid } else cache
    nextStIfReady = FcsValidatorState fwdIn False
    nextSt = if readyIn then nextStIfReady else st {_validated = outValid}

data FcsValidatorState dataWidth =
    FcsValidatorState
    { _cachedFwd :: Maybe (PacketStreamM2S dataWidth ())
    , _validated :: Bool
    }
  deriving (Show, Generic, NFDataX)


fcsValidator
  :: forall (dom :: Domain) (dataWidth :: Nat)
  .  HiddenClockResetEnable dom
  => KnownNat dataWidth
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M
     )
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     )
fcsValidator (fwdIn, bwdIn) = (bwdOut, fwdOut)
  where
    fwdInX = fromJustX <$> fwdIn
    transferOccured = ready .&&. isJust <$> fwdIn
    crcIn = toMaybe <$> transferOccured <*> liftA2 toCRCInput isFirst fwdInX

    isFirst = regEn True transferOccured $ isJust . _last <$> fwdInX
    valid = crcValidator Crc32_ethernet crcIn

    bwdOut = PacketStreamS2M <$> ready

    (fwdOut, ready) = mealyB fcsValidatorT (FcsValidatorState Nothing False) (valid, fwdIn, bwdIn)

-- | Validates a packet which contains the Crc-32 in its final four (4) bytes. Asserts abort signal
-- in the last fragment of this packet if invalid, else it does not change the abort.
fcsValidatorC
  :: forall (dom :: Domain) (dataWidth :: Nat)
  .  KnownDomain dom
  => KnownNat dataWidth
  => HiddenClockResetEnable dom
  => HardwareCrc Crc32_ethernet 8 dataWidth
  => Circuit
    (PacketStream dom dataWidth ())
    (PacketStream dom dataWidth ())
fcsValidatorC = forceResetSanity |> fromSignals fcsValidator
