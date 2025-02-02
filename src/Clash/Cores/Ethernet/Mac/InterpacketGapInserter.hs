{-|
Module      : Clash.Cores.Ethernet.Mac.InterpacketGapInserter
Description : Provides a circuit which inserts a configurable-length interpacket gap between packets.
-}
module Clash.Cores.Ethernet.Mac.InterpacketGapInserter
  ( interpacketGapInserterC
  ) where

import Clash.Prelude

import Protocols.PacketStream
import Protocols ( Circuit, fromSignals )

import Data.Maybe


data InterpacketGapInserterState gapSize
  = -- | Assert backpressure for @gapSize@ cycles.
    Insert {_counter :: Index gapSize}
  | -- | Forward incoming transfers until a packet boundary.
    Forward
  deriving (Generic, NFDataX, Show, ShowX)

-- | State transition function of the interpacket gap inserter, in mealy form.
gapInserterT ::
  forall (gapSize :: Nat).
  1 <= gapSize =>
  KnownNat gapSize =>
  InterpacketGapInserterState gapSize ->
  (Maybe (PacketStreamM2S 1 ()), PacketStreamS2M) ->
  ( InterpacketGapInserterState gapSize,
    (PacketStreamS2M, Maybe (PacketStreamM2S 1 ()))
  )
gapInserterT Insert{_counter = c} _ = (nextSt, (PacketStreamS2M False, Nothing))
 where
  nextSt = if c == 0 then Forward else Insert (c - 1)
gapInserterT Forward (fwdIn, bwdIn) = (nextSt, (bwdIn, fwdIn))
 where
  nextSt = case fwdIn of
    Just transferIn | isJust (_last transferIn) -> Insert maxBound
    _ -> Forward

{- |
Inserts an interpacket gap between packets. More specifically, asserts
backpressure for a given number of clock cycles after receiving a transfer
with `_last` set. During these cycles, the output of this component is
@Nothing@.
-}
interpacketGapInserterC ::
  forall (gapSize :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom =>
  1 <= gapSize =>
  -- | The amount of clock cycles this component will stall after each packet boundary
  SNat gapSize ->
  Circuit (PacketStream dom 1 ()) (PacketStream dom 1 ())
interpacketGapInserterC SNat = fromSignals (mealyB (gapInserterT @gapSize) Forward)
