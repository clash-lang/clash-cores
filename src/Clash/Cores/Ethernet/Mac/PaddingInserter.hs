{-|
Module      : Clash.Cores.Ethernet.Mac.PaddingInserter
Description : Provides paddingInserterC for padding ethernet frames to a customizable amount of bytes.
-}
module Clash.Cores.Ethernet.Mac.PaddingInserter
  ( paddingInserterC
  ) where

import Clash.Prelude

import Protocols ( Circuit, fromSignals )
import Protocols.PacketStream

import Control.Monad ( guard )
import Data.Maybe ( isJust )
import Data.Maybe.Extra ( toMaybe )


-- | State of the paddingInserter circuit.
-- Counts up to ceil(`padBytes`/`dataWidth`) packets, which is
-- the amount of packets needed to fill `padBytes` bytes.
data PaddingInserterState (dataWidth :: Nat) (padBytes :: Nat)
  = Filling { count :: Index (DivRU padBytes dataWidth) }
  | Full
  | Padding { count :: Index (DivRU padBytes dataWidth) }
  deriving (Eq, Show, Generic, NFDataX)

paddingInserter
  :: forall (dataWidth :: Nat) (padBytes :: Nat) (dom :: Domain)
   . HiddenClockResetEnable dom
  => 1 <= dataWidth
  => 1 <= padBytes
  => KnownNat dataWidth
  => KnownNat padBytes
  => SNat padBytes
  -> ( Signal dom (Maybe (PacketStreamM2S dataWidth ()))
     , Signal dom PacketStreamS2M)
  -- ^ Input packet stream from the source
  --   Input backpressure from the sink
  -> ( Signal dom PacketStreamS2M
     , Signal dom (Maybe (PacketStreamM2S dataWidth ())))
  -- ^ Output backpressure to the source
  --   Output packet stream to the sink
paddingInserter _ = mealyB go (Filling 0)
  where
    padding = PacketStreamM2S {_data = repeat 0, _last = Nothing, _meta = (), _abort = False}
    lastIdx = natToNum @((padBytes - 1) `Mod` dataWidth)
    go
      :: PaddingInserterState dataWidth padBytes
      -> (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M)
      -> (PaddingInserterState dataWidth padBytes, (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ())))
    -- If state is Full, forward the input from sink
    go Full (Nothing, bwd) = (Full, (bwd, Nothing))
    go Full (Just fwd, bwd@(PacketStreamS2M inReady)) = (if inReady && isJust (_last fwd) then Filling 0 else Full, (bwd, Just fwd))

    -- If state is Padding, send out null-bytes to source and backpressure to sink
    go st@(Padding i) (_, PacketStreamS2M inReady) = (if inReady then st' else st, (PacketStreamS2M False, Just fwdOut))
      where
        done = i == maxBound
        st' = if done then Filling 0 else Padding (i + 1)
        fwdOut = padding {_last = toMaybe done lastIdx}

    -- If state is Filling, forward the input from sink with updated _last
    go (Filling i) (Nothing, bwd) = (Filling i, (bwd, Nothing))
    go st@(Filling i) (Just fwdIn, bwd@(PacketStreamS2M inReady)) = (if inReady then st' else st, (bwd, Just fwdOut))
      where
        done = i == maxBound
        next = i + 1
        st' = case (done, _last fwdIn) of
          (True,  Nothing) -> Full
          (True,  Just _ ) -> Filling 0
          (False, Nothing) -> Filling next
          (False, Just _ ) -> Padding next
        -- If i < maxBound, then set _last to Nothing
        -- Otherwise, set _last to the maximum of the
        -- index that would reach the minimum frame size,
        -- and the _last of fwdIn
        fwdOut = fwdIn {_last = guard done >> max lastIdx <$> _last fwdIn}

-- | Pads ethernet frames to a minimum of `padBytes` bytes.
-- Assumes that all invalid bytes are set to 0.
-- Sends bytes the same clock cycle as they are received.
paddingInserterC
  :: forall (dataWidth :: Nat) (padBytes :: Nat) (dom :: Domain)
   . HiddenClockResetEnable dom
  => 1 <= dataWidth
  => 1 <= padBytes
  => KnownNat dataWidth
  => KnownNat padBytes
  => SNat padBytes
  -> Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
paddingInserterC padBytes = fromSignals (paddingInserter padBytes)
