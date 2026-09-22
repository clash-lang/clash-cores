{-# LANGUAGE RecordWildCards #-}

{- |
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Bit slip function that word-aligns a stream of bits based on received
  comma values
-}
module Clash.Cores.Sgmii.BitSlip (
  BitSlipState (..),
  bitSlip,
  bitSlipO,
  bitSlipT,
)
where

import Clash.Cores.Sgmii.Common
import Clash.Prelude

-- | State variable for 'bitSlip', with the two states as described in
--   'bitSlipT'. Due to timing constraints, not all functions can be executed in
--   the same cycle, which is why intermediate values are saved in the record
--   for 'BSFail'.
--
--   Code groups are received bit 0 first (bit 0 is @a@ in IEEE 802.3
--   Figure 36-3), so a code group that starts @k@ bits into the previous
--   received word consists of bit @k@ and up of the previous word followed by
--   the first @k@ bits of the current word. '_rx' holds the previous and the
--   current word, '_hist' the ten candidate code groups for @k = 0 .. 9@ from
--   the previous cycle.
data BitSlipState
  = BSFail
      { _rx :: (CodeGroup, CodeGroup)
      , _commaLocs :: Vec 8 (Index 10)
      , _hist :: Vec 10 CodeGroup
      }
  | BSOk {_rx :: (CodeGroup, CodeGroup), _commaLoc :: Index 10}
  deriving (Generic, NFDataX, Show)

-- | The candidate code groups in a previous and a current word: the code group
--   starting @k@ bits into the previous word, for @k = 0 .. 9@
alignments :: (CodeGroup, CodeGroup) -> Vec 10 CodeGroup
alignments (prev, cur) = map align indicesI
 where
  align :: Index 10 -> CodeGroup
  align k = resize ((cur ++# prev) `shiftR` fromEnum k)

-- | State transition function for 'bitSlip', where the initial state is the
--   training state, and after 8 consecutive commas have been detected at the
--   same index in the status register it moves into the 'BSOk' state where the
--   recovered index is used to shift the output 'BitVector'
bitSlipT ::
  -- | Current state
  BitSlipState ->
  -- | New input values
  (BitVector 10, Status) ->
  -- | New state
  BitSlipState
bitSlipT BSFail{..} (cg, _)
  | Just i <- commaLoc, _commaLocs == repeat i = BSOk rx i
  | otherwise = BSFail rx commaLocs hist
 where
  rx = (snd _rx, cg)
  commaLocs = maybe _commaLocs (_commaLocs <<+) commaLoc

  hist = alignments rx

  commaLoc = elemIndex True $ map (`elem` commas) _hist
bitSlipT BSOk{..} (cg, syncStatus)
  | syncStatus == Fail = BSFail rx (repeat _commaLoc) (repeat 0)
  | otherwise = BSOk rx _commaLoc
 where
  rx = (snd _rx, cg)

-- | Output function for 'bitSlip' that selects the code group at the
--   calculated alignment, or the one at the last tried alignment when no
--   alignment has been found yet
bitSlipO ::
  -- | Current state
  BitSlipState ->
  -- | New output value
  (BitSlipState, BitVector 10, Status)
bitSlipO s = (s, alignments (_rx s) !! commaLoc, bsStatus)
 where
  (commaLoc, bsStatus) = case s of
    BSFail{} -> (last (_commaLocs s), Fail)
    BSOk{} -> (_commaLoc s, Ok)

-- | Function that takes a stream of received 10-bit words and returns the
--   stream of word-aligned code groups: once a comma has been detected at the
--   same alignment eight times in a row, the words are shifted so that code
--   groups start at bit 0. The output lags the input by one word.
bitSlip ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | Input code group
  Signal dom (BitVector 10) ->
  -- | Current sync status from 'Sgmii.sync'
  Signal dom Status ->
  -- | Output code group
  (Signal dom (BitVector 10), Signal dom Status)
bitSlip cg1 syncStatus = (register 0 cg2, register Fail bsStatus)
 where
  (_, cg2, bsStatus) =
    mooreB
      bitSlipT
      bitSlipO
      (BSFail (0, 0) (repeat 0) (repeat 0))
      (cg1, syncStatus)
{-# OPAQUE bitSlip #-}
