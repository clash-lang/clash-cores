{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | I2C target model to test the I2C controller against. This is a copy of the
target in @test/Test/Cores/I2c/Model.hs@.
-}
module Target where

import Clash.Prelude

data TargetConfig = TargetConfig
  { addr :: BitVector 7
  -- ^ Address the target responds to
  , stretch :: Unsigned 16
  -- ^ Number of cycles the target holds SCL low after every falling edge of SCL
  , regs :: Vec 16 (BitVector 8)
  -- ^ Initial register file
  }
  deriving (Show)

data TargetMode
  = -- | Not addressed, wait for a (repeated) START condition
    Ignore
  | -- | Receive the address byte
    RxAddr
  | -- | Receive the register pointer
    RxPtr
  | -- | Receive data, written to the register file
    RxData
  | -- | Transmit data from the register file
    Tx
  deriving (Eq, Show, Generic, NFDataX)

data TargetState = TargetState
  { prevScl :: Bit
  , prevSda :: Bit
  , mode :: TargetMode
  , nextMode :: TargetMode
  -- ^ Mode after the acknowledge bit of a received byte
  , bitCnt :: Index 10
  -- ^ Number of rising SCL edges in the current byte, including the
  -- acknowledge bit
  , shift :: BitVector 8
  , controllerAck :: Bit
  -- ^ Acknowledge bit sent by the controller after transmitting a byte
  , sclOut :: Bit
  , sdaOut :: Bit
  , stretchCnt :: Unsigned 16
  , ptr :: Index 16
  , regs :: Vec 16 (BitVector 8)
  }
  deriving (Show, Generic, NFDataX)

{- | I2C target with a register file. Writes start with a register pointer,
followed by data bytes written to consecutive registers. Reads return data from
consecutive registers, starting at the current register pointer.

The target samples the bus every clock cycle, and changes SDA only after a
falling edge of SCL. Outputs are registered.
-}
i2cTarget ::
  (HiddenClockResetEnable dom) =>
  TargetConfig ->
  -- | Bus SCL and SDA
  Signal dom (Bit, Bit) ->
  -- | Driven SCL and SDA, and register file
  Signal dom ((Bit, Bit), Vec 16 (BitVector 8))
i2cTarget cfg = moore targetT out initState
 where
  out s = ((s.sclOut, s.sdaOut), s.regs)

  initState =
    TargetState
      { prevScl = high
      , prevSda = high
      , mode = Ignore
      , nextMode = Ignore
      , bitCnt = 0
      , shift = 0
      , controllerAck = high
      , sclOut = high
      , sdaOut = high
      , stretchCnt = 0
      , ptr = 0
      , regs = cfg.regs
      }

  targetT s (scl, sda) =
    (stretch (protocol s)){prevScl = scl, prevSda = sda}
   where
    sclHigh = s.prevScl == high && scl == high
    startCond = sclHigh && s.prevSda == high && sda == low
    stopCond = sclHigh && s.prevSda == low && sda == high
    rising = s.prevScl == low && scl == high
    falling = s.prevScl == high && scl == low

    stretch st
      | falling && s.mode /= Ignore && cfg.stretch > 0 =
          st{sclOut = low, stretchCnt = cfg.stretch - 1}
      | st.stretchCnt == 0 = st{sclOut = high}
      | otherwise = st{stretchCnt = st.stretchCnt - 1}

    protocol st
      | startCond = st{mode = RxAddr, bitCnt = 0, sdaOut = high}
      | stopCond = st{mode = Ignore, bitCnt = 0, sdaOut = high}
      | rising = onRising st
      | falling = onFalling st
      | otherwise = st

    onRising st = case st.mode of
      Ignore -> st
      Tx | st.bitCnt == 8 -> st{bitCnt = 9, controllerAck = sda}
      _
        | st.bitCnt < 8 && st.mode /= Tx ->
            st{bitCnt = st.bitCnt + 1, shift = truncateB (st.shift ++# pack sda)}
        | otherwise -> st{bitCnt = st.bitCnt + 1}

    onFalling st = case (st.mode, st.bitCnt) of
      (Ignore, _) -> st
      -- Release SDA so the controller can acknowledge
      (Tx, 8) -> st{sdaOut = high}
      -- Byte received, acknowledge or not
      (_, 8) -> receive st
      -- End of acknowledge bit
      (Tx, 9)
        | st.controllerAck == low -> transmit st
        | otherwise -> st{mode = Ignore, sdaOut = high}
      (_, 9)
        | st.nextMode == Tx -> transmit st
        | otherwise -> st{mode = st.nextMode, bitCnt = 0, sdaOut = high}
      -- Next bit to transmit
      (Tx, n)
        | n > 0 -> st{sdaOut = st.shift ! (7 - fromEnum n)}
      _ -> st

    receive st = case st.mode of
      RxAddr
        | slice d7 d1 st.shift == cfg.addr ->
            ack (if lsb st.shift == high then Tx else RxPtr) st
        | otherwise -> st{nextMode = Ignore}
      RxPtr -> ack RxData st{ptr = unpack (truncateB st.shift)}
      _ ->
        ack
          RxData
          st
            { regs = replace st.ptr st.shift st.regs
            , ptr = satSucc SatWrap st.ptr
            }

    ack next st = st{sdaOut = low, nextMode = next}

    transmit st =
      st
        { mode = Tx
        , bitCnt = 0
        , shift = byte
        , sdaOut = msb byte
        , ptr = satSucc SatWrap st.ptr
        }
     where
      byte = st.regs !! st.ptr

