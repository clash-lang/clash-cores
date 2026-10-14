{-# LANGUAGE OverloadedRecordDot #-}

{- |
  Copyright   :  (C) 2016, University of Twente
                     2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Shift register of the I2C byte controller.
-}
module Clash.Cores.I2c.ByteController.ShiftRegister where

import Clash.Prelude

data ShiftRegister = ShiftRegister
  { sr :: Vec 8 Bit
  -- ^ Shift register, most significant bit first
  , dcnt :: Index 8
  -- ^ Number of bits left to shift
  }
  deriving (Show, Generic, NFDataX)

shiftStartState :: ShiftRegister
shiftStartState =
  ShiftRegister
    { sr = repeat low
    , dcnt = 0
    }

-- | Shift register that loads a byte to transmit, and shifts in received bits.
shiftRegister ::
  -- | Load @din@ into the shift register
  Bool ->
  -- | Shift in @coreRxd@
  Bool ->
  -- | Data to load
  Vec 8 Bit ->
  -- | Bit to shift in
  Bit ->
  ShiftRegister ->
  ShiftRegister
shiftRegister ld shiftSr din coreRxd s
  | ld = ShiftRegister{sr = din, dcnt = maxBound}
  | shiftSr =
      -- When reading a byte, the eighth bit is shifted in when the counter is
      -- already zero. The counter wraps around, but is reloaded on the next
      -- load anyway.
      ShiftRegister{sr = s.sr <<+ coreRxd, dcnt = satPred SatWrap s.dcnt}
  | otherwise = s
