{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

A generator that sends the fixed test frame of "Kcu105.Sgmii.TestFrame" at
regular intervals, to exercise the transmit path independently of the
receive path.
-}
module Kcu105.Sgmii.FrameGen where

import Clash.Explicit.Prelude
import Data.Maybe (isJust)
import Kcu105.Sgmii.Domains
import Kcu105.Sgmii.TestFrame (longFrameBytes, testFrameBytes)

-- | The short test frame as GMII bytes
shortFrame :: Vec 72 (BitVector 8)
shortFrame = $(listToVecTH (fmap fromInteger testFrameBytes :: [BitVector 8]))

-- | The long test frame as GMII bytes
longFrame :: Vec 326 (BitVector 8)
longFrame = $(listToVecTH (fmap fromInteger longFrameBytes :: [BitVector 8]))

-- | While enabled, send the short and the long test frame alternately, one
--   frame about every 0.5 ms (2^16 cycles). Returns @TX_EN@ and @TXD@.
frameGenerator ::
  Clock Pcs125 ->
  Reset Pcs125 ->
  Signal Pcs125 Bool ->
  (Signal Pcs125 Bool, Signal Pcs125 (BitVector 8))
frameGenerator clk rst enable =
  mealyB clk rst enableGen go (0 :: Unsigned 16, False, Nothing) enable
 where
  go (timer, long, sending) en = ((timer + 1, long', sending'), (isJust sending, byte))
   where
    len = if long then 326 else 72 :: Int
    byte = case sending of
      Just i
        | long -> longFrame !! i
        | otherwise -> shortFrame !! i
      Nothing -> 0
    (long', sending') = case sending of
      Just i
        | fromEnum i + 1 == len -> (not long, Nothing)
        | otherwise -> (long, Just (i + 1))
      Nothing
        | en && timer == 0 -> (long, Just (0 :: Index 326))
        | otherwise -> (long, Nothing)
