{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
Provides a circuit that delays a stream by a configurable amount of transfers.
-}
module Protocols.PacketStream.Extra (
  delayStreamC,
  dropTailC,
) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream.Base

import Data.Constraint.Deferrable ((:~:) (Refl))
import Data.Maybe
import Data.Constraint (Dict (Dict))
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Bifunctor as B

-- | Copied from `clash-protocols`
strictlyPositiveDivRu ::
  forall a b . (1 <= a, 1 <= b) => Dict (1 <= (a `DivRU` b))
strictlyPositiveDivRu = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | State of 'delayStreamT'.
data DelayState n = DelayState
  { _size :: Index (n + 1)
  -- ^ The number of valid transactions in the buffer.
  , _readPtr :: Index n
  -- ^ Current block RAM read address.
  , _writePtr :: Index n
  -- ^ Current block RAM write address.
  , _flush :: Bool
  -- ^ Iff true, transmit all remaining transactions of the current packet,
  --   regardless of whether the buffer is full or not.
  , _metaWriteEn :: Bool
  -- ^ If true, write to the meta buffer. We need this in case of LHS stalls.
  }
  deriving (Generic, NFDataX, Show, ShowX)

-- | State transition function of 'delayStreamC'.
delayStreamT ::
  forall
    (n :: Nat)
    (dataWidth :: Nat)
    (meta :: Type).
  (KnownNat n) =>
  (KnownNat dataWidth) =>
  (1 <= n) =>
  (1 <= dataWidth) =>
  (NFDataX meta) =>
  DelayState n ->
  ( Maybe (PacketStreamM2S dataWidth meta)
  , PacketStreamS2M
  , PacketStreamM2S dataWidth ()
  , meta
  ) ->
  ( DelayState n
  , ( PacketStreamS2M
    , Maybe (PacketStreamM2S dataWidth meta)
    , Index n
    , Maybe (Index n, PacketStreamM2S dataWidth ())
    , Maybe meta
    )
  )
delayStreamT st (fwdIn, bwdIn, buff, metaBuf) =
  (nextStOut, (bwdOut, fwdOut, _readPtr nextStOut, writeCmd, mWriteCmd))
 where
  emptyBuf = _size st == 0
  fullBuf = _size st == maxBound

  readEn = case fwdIn of
    Nothing -> not emptyBuf && _flush st
    Just _ -> fullBuf || _flush st

  bwdOut = PacketStreamS2M (isNothing fwdIn || not readEn || _ready bwdIn)

  (readPtr', fwdOut) =
    if readEn
      then (satSucc SatWrap (_readPtr st), Just (buff{_meta = metaBuf}))
      else (_readPtr st, Nothing)

  (writeCmd, mWriteCmd, writePtr') = case fwdIn of
    Nothing -> (Nothing, Nothing, _writePtr st)
    Just inPkt ->
      ( if readEn && not (_ready bwdIn)
          then Nothing
          else Just (_writePtr st, inPkt{_meta = ()})
      , if _metaWriteEn st || (emptyBuf || readEn && isJust (_last buff) && _ready bwdIn)
          then Just (_meta inPkt)
          else Nothing
      , satSucc SatWrap (_writePtr st)
      )

  metaWriteEn' = isNothing fwdIn && (_metaWriteEn st || isJust (_last buff))

  (size', flush') = case fwdIn of
    Nothing -> (_size st - 1, isNothing (_last buff) && _flush st)
    Just inPkt
      | _flush st ->
          (_size st, not (readEn && isJust (_last buff)) && _flush st)
      | otherwise ->
          (satSucc SatBound (_size st), isJust (_last inPkt))

  nextSt = DelayState size' readPtr' writePtr' flush' metaWriteEn'
  nextStOut =
    if isJust fwdIn
      && (isNothing fwdOut || _ready bwdIn)
      || isNothing fwdIn
      && (readEn && _ready bwdIn)
      then nextSt
      else st

{- |
Forwards incoming packets with @n@ transactions latency. Because of potential
stalls this is not the same as @n@ clock cycles. Assumes that all packets
passing through this component are bigger than @n@ transactions. If not, this
component has __UNDEFINED BEHAVIOUR__ and things will break.
-}
delayStreamC ::
  forall
    (dataWidth :: Nat)
    (meta :: Type)
    (dom :: Domain)
    (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= n) =>
  (1 <= dataWidth) =>
  (NFDataX meta) =>
  -- | The number of transactions to delay
  SNat n ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
delayStreamC SNat = forceResetSanity |> fromSignals ckt
 where
  ckt (fwdInS, bwdInS) = (bwdOutS, fwdOutS)
   where
    -- Store the contents of transactions without metadata.
    -- We only need write before read semantics in case n ~ 1.
    bram :: Signal dom (PacketStreamM2S dataWidth ())
    bram = case sameNat d1 (SNat @n) of
      Nothing -> blockRamU NoClearOnReset (SNat @n) readAddr writeCmd
      Just Refl -> readNew (blockRamU NoClearOnReset (SNat @n)) readAddr writeCmd

    -- There are at most two packets in the blockram, but they are required
    -- to be bigger than @n@ transactions. Thus, we only need to store the
    -- metadata once.
    metaBuffer :: Signal dom meta
    metaBuffer = regMaybe (deepErrorX "delayStream: undefined initial meta") mWriteCmd

    (bwdOutS, fwdOutS, readAddr, writeCmd, mWriteCmd) =
      unbundle
        $ mealy delayStreamT (DelayState @n 0 0 0 False True) input

    input = bundle (fwdInS, bwdInS, bram, metaBuffer)

-- | Information about the tail of a packet, for dropping purposes.
data DropTailInfo dataWidth delay = DropTailInfo
  { _dtAborted :: Bool
  -- ^ Whether any fragment of the packet was aborted
  , _newIdx :: Index (dataWidth + 1)
  -- ^ The adjusted byte enable
  , _drops :: Index (delay + 1)
  -- ^ The amount of transfers to drop from the tail
  , _wait :: Bool
  -- ^ Iff true, apply changes to transfer the next clock cycle instead
  }

{- |
Transmits information about a single packet upon seeing its last transfer.
-}
transmitDropInfoC ::
  forall (dataWidth :: Nat) (delay :: Nat) (meta :: Type) (dom :: Domain) (n :: Nat).
  (KnownNat dataWidth) =>
  (KnownNat delay) =>
  (1 <= dataWidth) =>
  (1 <= n) =>
  (HiddenClockResetEnable dom) =>
  SNat n ->
  Circuit
    (PacketStream dom dataWidth meta)
    (Df dom (DropTailInfo dataWidth delay))
transmitDropInfoC SNat = forceResetSanity |> fromSignals (mealyB go False)
 where
  go aborted (Nothing, _) = (aborted, (PacketStreamS2M True, Nothing))
  go aborted (Just PacketStreamM2S{..}, Ack readyIn) = (nextAborted, (PacketStreamS2M readyIn, fwdOut))
   where
    (nextAborted, fwdOut) = case _last of
      Nothing -> (aborted || _abort, Nothing)
      Just i -> (not readyIn && (aborted || _abort), Just (toDropTailInfo i))

    toDropTailInfo i =
      DropTailInfo
        { _dtAborted = aborted || _abort
        , _newIdx = newIdx
        , _drops = drops
        , _wait = wait
        }
     where
      (newIdx, drops, wait) = case ( compareSNat (SNat @dataWidth) (SNat @n)
                                   , sameNat d0 (SNat @(n `Mod` dataWidth))
                                   ) of
        (SNatLE, Nothing) ->
          let smaller = (resize i :: Index n) <= natToNum @(n - dataWidth)
           in ( satSub SatWrap i (natToNum @(n `Mod` dataWidth) + (if smaller then 1 else 0))
              , if smaller
                  then natToNum @(n `DivRU` dataWidth)
                  else natToNum @(n `Div` dataWidth)
              , not smaller
              )
        (SNatLE, Just Refl) ->
          (i, natToNum @(n `Div` dataWidth), False)
        (SNatGT, _) ->
          if i > natToNum @n
            then (i - natToNum @n, 0, True)
            else (maxBound - (natToNum @n - i), 1, False)

{- |
Gets a delayed packet stream as input together with non-delayed
'DropTailInfo', so that dropping can be done while correctly preserving
'_abort' and adjusting '_last'.
-}
dropTailC' ::
  forall (dataWidth :: Nat) (delay :: Nat) (meta :: Type) (dom :: Domain).
  (KnownDomain dom) =>
  (KnownNat dataWidth) =>
  (KnownNat delay) =>
  (HiddenClockResetEnable dom) =>
  Circuit
    (PacketStream dom dataWidth meta, Df dom (DropTailInfo dataWidth delay))
    (PacketStream dom dataWidth meta)
dropTailC' = fromSignals (B.first unbundle . mealyB go (0, Nothing) . B.first bundle)
 where
  go (0, cache) ((fwdIn, infoIn), bwdIn) = (nextStOut, ((bwdOut1, bwdOut2), fwdOut))
   where
    (bwdOut1, bwdOut2)
      | isNothing fwdOut = (PacketStreamS2M True, Ack True)
      | otherwise = (bwdIn, Ack (_ready bwdIn))

    (nextSt, fwdOut) = case (fwdIn, infoIn) of
      (Nothing, _) ->
        ((0, cache), Nothing)
      (Just pkt, Nothing) ->
        case cache of
          Nothing ->
            ((0, Nothing), Just pkt)
          Just (aborted, newIdx, delayy) ->
            ((delayy, Nothing), Just pkt{_abort = aborted, _last = Just newIdx})
      (Just pkt, Just inf) ->
        if _wait inf
          then ((0, Just (_dtAborted inf, _newIdx inf, _drops inf)), Just pkt)
          else
            ( (_drops inf, Nothing)
            , Just pkt{_last = Just (_newIdx inf), _abort = _dtAborted inf}
            )

    nextStOut = if isNothing fwdOut || _ready bwdIn then nextSt else (0, cache)
  go (cycles, cache) _ = ((cycles - 1, cache), ((PacketStreamS2M True, Ack True), Nothing))

{- |
Removes the last @n@ bytes from each packet in the packet stream.
If any dropped transfers had '_abort' set, this will be preserved by
setting the '_abort' of an earlier transfer that is not dropped.
__NB__: assumes that packets are longer than @ceil(n / dataWidth)@ transfers.
If this is not the case, the behaviour of this component is undefined.
-}
dropTailC ::
  forall (dataWidth :: Nat) (meta :: Type) (dom :: Domain) (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (1 <= n) =>
  (NFDataX meta) =>
  SNat n ->
  Circuit
    (PacketStream dom dataWidth meta)
    (PacketStream dom dataWidth meta)
dropTailC SNat = case strictlyPositiveDivRu @n @dataWidth of
  Dict ->
    forceResetSanity
      |> circuit
        ( \stream -> do
            [s1, s2] <- fanout -< stream
            delayed <- delayStreamC (SNat @(n `DivRU` dataWidth)) -< s1
            info <- transmitDropInfoC @dataWidth @(n `DivRU` dataWidth) (SNat @n) -< s2
            dropTailC' @dataWidth @(n `DivRU` dataWidth) -< (delayed, info)
        )
