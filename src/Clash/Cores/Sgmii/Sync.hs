{-# LANGUAGE ViewPatterns #-}

{- |
Copyright   :  (C) 2024-2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Synchronization process, as defined in IEEE 802.3 Figure 36-9
-}
module Clash.Cores.Sgmii.Sync
  ( OutputQueue
  , SyncState (..)
  , outputQueueO
  , outputQueueT
  , sync
  , syncO
  , syncT
  )
where

import Clash.Cores.LineCoding.Lc8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (isNothing)

-- | State type of the output queue for 'sync'
type OutputQueue = Vec 3 (CodeGroup, Bool, Symbol8b10b, Even, Status)

-- | State type of 'sync'. This contains all states as they are defined in IEEE
--   802.3 Clause 36.
data SyncState
  = LossOfSync {_cg :: CodeGroup, _rd :: Bool, _rxEven :: Even}
  | CommaDetect {_cg :: CodeGroup, _rd :: Bool, _i :: Index 3}
  | AcquireSync {_cg :: CodeGroup, _rd :: Bool, _rxEven :: Even, _i :: Index 3}
  | SyncAcquired {_cg :: CodeGroup, _rd :: Bool, _rxEven :: Even, _i :: Index 3}
  | SyncAcquiredA
      { _cg :: CodeGroup
      , _rd :: Bool
      , _rxEven :: Even
      , _goodCgs :: Index 4
      , _i :: Index 3
      }
  deriving (Generic, NFDataX, Show)

-- | State transition function for 'sync'. Takes the state as defined in
--   'SyncState', a the new incoming code group from the deserialization block
--   and returns the next state as defined in Clause 36 of IEEE 802.3. As is
--   described in the documentation for 'Sgmii.pcsReceive', this function also
--   does the decoding of 10-bit code groups (which is usually done by
--   'Sgmii.pcsReceive') as it needs the information provided by the decode
--   function to determine whether a code group corresponds to a valid data
--   word.
syncT ::
  -- | Current state
  SyncState ->
  -- | New input codegroup
  (BitVector 10, Bool, Symbol8b10b) ->
  -- | New state and output tuple
  SyncState
syncT s (cg, rd, sym) = case s of
  LossOfSync{}
    | isNothing comma -> LossOfSync cg rdUpd rxEven
    | otherwise -> CommaDetect cg rdUpd 0
  CommaDetect{}
    | not (isDw sym) -> LossOfSync cg rdUpd Even
    | _i s == 0 -> AcquireSync cg rdUpd Even (_i s)
    | otherwise -> SyncAcquired cg rdUpd Even 0
  AcquireSync{}
    | not (isValidSymbol sym) -> LossOfSync cg rdUpd rxEven
    | cg `elem` commas && rxEven == Even -> LossOfSync cg rdUpd rxEven
    | cg `elem` commas && rxEven == Odd -> CommaDetect cg rdUpd 1
    | otherwise -> AcquireSync cg rdUpd rxEven 0
  SyncAcquired{}
    | _i s == maxBound && not (isValidSymbol sym) -> LossOfSync cg rdUpd rxEven
    | _i s == maxBound && cg `elem` commas && rxEven == Even ->
        LossOfSync cg rdUpd rxEven
    | not (isValidSymbol sym) -> SyncAcquired cg rdUpd rxEven (_i s + 1)
    | cg `elem` commas && rxEven == Even ->
        SyncAcquired cg rdUpd rxEven (_i s + 1)
    | _i s == 0 -> SyncAcquired cg rdUpd rxEven 0
    | otherwise -> SyncAcquiredA cg rdUpd rxEven goodCgs (_i s)
  SyncAcquiredA{}
    | _i s == maxBound && not (isValidSymbol sym) -> LossOfSync cg rdUpd rxEven
    | _i s == maxBound && cg `elem` commas && rxEven == Even ->
        LossOfSync cg rdUpd rxEven
    | not (isValidSymbol sym) -> SyncAcquired cg rdUpd rxEven (_i s + 1)
    | cg `elem` commas && rxEven == Even ->
        SyncAcquired cg rdUpd rxEven (_i s + 1)
    | _i s == 0 && goodCgs == maxBound -> SyncAcquired cg rdUpd rxEven 0
    | goodCgs == maxBound -> SyncAcquired cg rdUpd rxEven (_i s - 1)
    | otherwise -> SyncAcquiredA cg rdUpd rxEven goodCgs (_i s)
 where
  comma = elemIndex cg commas
  rdUpd = case s of
    LossOfSync{} -> maybe rd bitCoerce comma
    _ -> rd
  rxEven = nextEven (_rxEven s)
  goodCgs = case s of
    SyncAcquiredA{} -> _goodCgs s + 1
    _ -> 0

-- | Output function for 'sync'. Takes the state as defined in 'SyncState' and
--   returns a tuple containing the outputs as defined in Clause 36 of IEEE
--   802.3
syncO ::
  -- | Current state
  SyncState ->
  -- | New state and output tuple
  (SyncState, CodeGroup, Even, Status)
syncO s = case s of
  LossOfSync{} -> (s, _cg s, rxEven, Fail)
  CommaDetect{} -> (s, _cg s, Even, Fail)
  AcquireSync{} -> (s, _cg s, rxEven, Fail)
  _ -> (s, _cg s, rxEven, Ok)
 where
  rxEven = nextEven (_rxEven s)

-- | Transition function for the inputs of 'Sgmii.pcsReceive'. This is used to
--   keep a small list of "future" values for 'Symbol8b10b', such that these can
--   be used in 'Sgmii.checkEnd'.
outputQueueT ::
  -- | Current state with three values for all inputs
  OutputQueue ->
  -- | New input values for the code group, running disparity, data word, 'Even'
  --   signal and 'Status;
  (CodeGroup, Bool, Symbol8b10b, Even, Status) ->
  -- | New state
  OutputQueue
outputQueueT s i = s <<+ i

-- | Output function for the output queue, where the values are taken from the
--   current state
outputQueueO ::
  -- Current state with three values for all inputs
  OutputQueue ->
  -- | New output with one value for everything except 'Symbol8b10b' for the
  --   prescient 'Sgmii.checkEnd' function.
  (CodeGroup, Bool, Vec 3 Symbol8b10b, Even, Status)
outputQueueO s = (cg, rd, dw, rxEven, syncStatus)
 where
  (head -> cg, head -> rd, dw, head -> rxEven, head -> syncStatus) = unzip5 s

-- | Takes a code group and runs it through the state machine as defined in
--   IEEE 802.3 Clause 36 to check whether the signal is synchronized. If it is
--   not, output 'Status' @Fail@ and try to re-aquire synchronization, else
--   simply pass through the new running disparity and 'Symbol8b10b' from the
--   decoded code group as well as the 'Even' signal. The current code word is
--   also propagated as it is required by 'Sgmii.pcsReceive'. This function
--   contains a list of data words as these need to be used by the prescient
--   'Sgmii.checkEnd' function.
sync ::
  (HiddenClockResetEnable dom) =>
  -- | New code group from the PHY
  Signal dom CodeGroup ->
  -- | A tuple containing the input code group, running disparity, a new
  --   'Symbol8b10b', the new value for 'Even' and the current synchronization
  --   status
  ( Signal dom CodeGroup
  , Signal dom Bool
  , Signal dom (Vec 3 Symbol8b10b)
  , Signal dom Even
  , Signal dom Status
  )
sync rxCg =
  mooreB
    outputQueueT
    outputQueueO
    (repeat (0, False, Dw 0, Odd, Fail))
    (cg, rd, sym, rxEven, syncStatus)
 where
  (rd, sym) = unbundle $ decode8b10bSC rxCg

  (_, cg, rxEven, syncStatus) =
    mooreB syncT syncO (LossOfSync 0 False Even) (rxCg, rd, sym)
{-# OPAQUE sync #-}
