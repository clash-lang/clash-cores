{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides a component which pads packets with null bytes, to a customizable
minimum packet size.
-}
module Clash.Cores.Ethernet.Mac.PaddingInserter (
  paddingInserterC,
) where

import Clash.Prelude

import Control.Monad (guard)

import Data.Maybe (isJust)
import Data.Maybe.Extra (toMaybe)
import Data.Type.Equality ((:~:) (Refl))

import Protocols (Circuit, fromSignals, (|>))
import Protocols.PacketStream

{- |
State of 'paddingInserterT'.
Counts up to @ceil(padBytes / dataWidth)@ transfers per packet, which is
the amount of transfers needed to fill @padBytes@ bytes.
-}
data PaddingInserterState (dataWidth :: Nat) (padBytes :: Nat)
  = Filling (Index (DivRU padBytes dataWidth))
  | Full
  | Padding (Index (DivRU padBytes dataWidth))
  deriving (Generic, NFDataX, Show, ShowX)

-- | Computes the size of the last fragment of a padded packet.
lastSize ::
  forall (dataWidth :: Nat) (padBytes :: Nat).
  (KnownNat dataWidth) =>
  (KnownNat padBytes) =>
  (1 <= dataWidth) =>
  Index (dataWidth + 1)
lastSize = case sameNat d0 (SNat @(padBytes `Mod` dataWidth)) of
  Just Refl -> maxBound
  _ -> natToNum @(padBytes `Mod` dataWidth)

paddingInserterT ::
  forall (dataWidth :: Nat) (padBytes :: Nat).
  (KnownNat dataWidth) =>
  (KnownNat padBytes) =>
  (1 <= dataWidth) =>
  (1 <= padBytes) =>
  PaddingInserterState dataWidth padBytes ->
  ( Maybe (PacketStreamM2S dataWidth ())
  , PacketStreamS2M
  ) ->
  ( PaddingInserterState dataWidth padBytes
  , ( PacketStreamS2M
    , Maybe (PacketStreamM2S dataWidth ())
    )
  )
-- If state is Filling, forward the input from sink with updated _last
paddingInserterT st@(Filling i) (Just fwdIn, bwdIn) =
  (nextStOut, (bwdIn, Just fwdOut))
 where
  done = i == maxBound
  -- If we are not done filling, then set _last to @Nothing@. Otherwise, set
  -- _last to the maximum of the index that would reach the minimum packet
  -- size, and the _last of the input transfer.
  fwdOut =
    fwdIn
      { _last = guard done >> max (lastSize @dataWidth @padBytes) <$> _last fwdIn
      }

  nextSt = case (done, _last fwdIn) of
    (True, Nothing) -> Full
    (True, Just _) -> Filling 0
    (False, Nothing) -> Filling (i + 1)
    (False, Just _) -> Padding (i + 1)

  nextStOut = if _ready bwdIn then nextSt else st

-- If state is Full, forward the input from sink
paddingInserterT Full (Just fwdIn, bwdIn) = (nextStOut, (bwdIn, Just fwdIn))
 where
  nextStOut = if _ready bwdIn && isJust (_last fwdIn) then Filling 0 else Full

-- If state is Padding, send out null-bytes to source and backpressure to sink
paddingInserterT st@(Padding i) (_, bwdIn) =
  (nextStOut, (PacketStreamS2M False, Just fwdOut))
 where
  done = i == maxBound
  fwdOut =
    PacketStreamM2S
      { _data = repeat 0
      , _last = toMaybe done (lastSize @dataWidth @padBytes)
      , _meta = ()
      , _abort = False
      }
  nextSt = if done then Filling 0 else Padding (i + 1)
  nextStOut = if _ready bwdIn then nextSt else st
paddingInserterT st (Nothing, bwdIn) = (st, (bwdIn, Nothing))

{- |
Pads packets with null bytes (@0x00@) to a minimum of @padBytes@ bytes. Provides
zero latency: transmits bytes the same clock cycle as they are received. Only
runs at full throughput for packets that do not need padding. For packets that
do need padding, it will assert backpressure in order to append the padding to
the packet.
-}
paddingInserterC ::
  forall (dataWidth :: Nat) (padBytes :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (1 <= padBytes) =>
  -- | The minimum size of output packets
  SNat padBytes ->
  Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
paddingInserterC SNat =
  zeroOutInvalidBytesC
    |> fromSignals (mealyB (paddingInserterT @_ @padBytes) (Filling 0))
