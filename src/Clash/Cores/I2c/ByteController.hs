{-# LANGUAGE OverloadedRecordDot #-}

{- |
  Copyright   :  (C) 2016, University of Twente
                     2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  I2C byte controller. Translates byte-level commands from the host into
  single-bit commands for the bit controller.
-}
module Clash.Cores.I2c.ByteController (byteController) where

import Clash.Prelude

import Clash.Cores.I2c.ByteController.ShiftRegister
import Clash.Cores.I2c.Types

data ByteStateMachine = Idle | Start | Read | Write | Ack | Stop
  deriving (Eq, Show, Generic, NFDataX)

data ByteControllerS = ByteS
  { srState :: ShiftRegister
  , byteStateM :: ByteStateMachine
  -- ^ State machine
  , coreCmd :: I2cCommand
  -- ^ coreCmd register
  , coreTxd :: Bit
  -- ^ coreTxd register
  , shiftSr :: Bool
  -- ^ Shift sr
  , ld :: Bool
  -- ^ Load values in to sr
  , hostAck :: Bool
  -- ^ Host cmd acknowledge register
  , ackOut :: Bool
  -- ^ Target ack register
  }
  deriving (Show, Generic, NFDataX)

type ByteControllerI = (Bool, Bool, Bool, Bool, Bool, BitVector 8, BitRespSig)
type ByteControllerO = (Bool, Bool, BitVector 8, BitCtrlSig)

{- | I2C byte controller. See 'Clash.Cores.I2c.i2c' for a description of the
host interface.
-}
byteController ::
  (HiddenClockResetEnable dom) =>
  -- | Start
  Signal dom Bool ->
  -- | Stop
  Signal dom Bool ->
  -- | Read
  Signal dom Bool ->
  -- | Write
  Signal dom Bool ->
  -- | ACK bit to send after reading a byte
  Signal dom Bool ->
  -- | Byte to write
  Signal dom (BitVector 8) ->
  -- | Response of the bit controller
  Signal dom BitRespSig ->
  -- | 1. Host command acknowledge
  --   2. ACK bit received after writing a byte
  --   3. Byte read
  --   4. Command for the bit controller
  ( Signal dom Bool
  , Signal dom Bool
  , Signal dom (BitVector 8)
  , Signal dom BitCtrlSig
  )
byteController start stop rd wr ackIn din bitResp =
  mealyB byteControllerT byteControllerInit (start, stop, rd, wr, ackIn, din, bitResp)

byteControllerInit :: ByteControllerS
byteControllerInit =
  ByteS
    { srState = shiftStartState
    , byteStateM = Idle
    , coreCmd = I2cNop
    , coreTxd = low
    , shiftSr = False
    , ld = False
    , hostAck = False
    , ackOut = True
    }

byteControllerT ::
  ByteControllerS -> ByteControllerI -> (ByteControllerS, ByteControllerO)
byteControllerT s (start, stop, rd, wr, ackIn, din, ~(coreAck, al, coreRxd)) =
  (s', (s.hostAck, s.ackOut, v2bv s.srState.sr, (s.coreCmd, s.coreTxd)))
 where
  -- generate go-signal
  go = (rd || wr || stop) && not s.hostAck

  cntDone = s.srState.dcnt == 0

  -- Values for when the state machine below does not override them
  defaults =
    s
      { srState = shiftRegister s.ld s.shiftSr (bv2v din) coreRxd s.srState
      , coreTxd = head s.srState.sr
      , shiftSr = False
      , ld = False
      , hostAck = False
      }

  -- state machine
  s'
    | al = defaults{coreCmd = I2cNop, coreTxd = low, byteStateM = Idle, ackOut = True}
    | otherwise = case s.byteStateM of
        Idle
          | not go -> defaults
          | start -> load (transition Start I2cStart)
          | rd -> load (transition Read I2cRead)
          | wr -> load (transition Write I2cWrite)
          | otherwise -> load (transition Stop I2cStop)
        Start
          | not coreAck -> defaults
          | rd -> load (transition Read I2cRead)
          | otherwise -> load (transition Write I2cWrite)
        Write
          | not coreAck -> defaults
          | cntDone -> transition Ack I2cRead
          | otherwise -> defaults{coreCmd = I2cWrite, shiftSr = True}
        Read
          | not coreAck -> defaults
          | otherwise ->
              (if cntDone then transition Ack I2cWrite else defaults{coreCmd = I2cRead})
                { shiftSr = True
                , coreTxd = bitCoerce ackIn
                }
        Ack
          | not coreAck -> defaults{coreTxd = bitCoerce ackIn}
          | otherwise ->
              -- check for stop; Should a STOP command be generated?
              ( if stop
                  then transition Stop I2cStop
                  else -- generate command acknowledge signal
                    (transition Idle I2cNop){hostAck = True}
              )
                { ackOut = bitCoerce coreRxd
                , coreTxd = high
                }
        Stop
          | not coreAck -> defaults
          | otherwise -> (transition Idle I2cNop){hostAck = True}

  transition st cmd = defaults{byteStateM = st, coreCmd = cmd}
  load st = st{ld = True}
