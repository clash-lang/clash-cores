{-# LANGUAGE OverloadedRecordDot #-}

{- |
  Copyright   :  (C) 2016, University of Twente
                     2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  I2C bit controller. Translates single-bit commands into SCL and SDA
  transitions, and handles clock stretching, clock synchronization, and
  arbitration.
-}
module Clash.Cores.I2c.BitController (bitController) where

import Clash.Prelude

import Clash.Cores.I2c.BitController.BusCtrl
import Clash.Cores.I2c.BitController.StateMachine
import Clash.Cores.I2c.Types

data BitControllerS = BitS
  { busState :: BusStatusCtrl
  , stateMachine :: StateMachine
  , dout :: Bit
  -- ^ dout register
  , dSclOen :: Bool
  -- ^ Delayed sclOen signal
  , clkEn :: Bool
  -- ^ State machine clock enable
  , targetWait :: Bool
  -- ^ Clock generation signal
  , cnt :: Unsigned 16
  -- ^ Clock divider counter
  }
  deriving (Show, Generic, NFDataX)

type BitControllerI = (Unsigned 16, BitCtrlSig, I2cIn)
type BitControllerO = (BitRespSig, Bool, I2cOut)

{- | I2C bit controller

The state machine advances one phase every @clkCnt + 1@ clock cycles. Every bit
takes four phases, plus one phase in which the state machine is idle and accepts
the next command. When releasing SCL, the clock divider is paused until SCL is
observed to be high. This only works if @clkCnt@ is at least 3.
-}
bitController ::
  (HiddenClockResetEnable dom) =>
  -- | Clock divider (@clkCnt@)
  Signal dom (Unsigned 16) ->
  -- | Command and bit to write
  Signal dom BitCtrlSig ->
  -- | I2C bus input
  Signal dom I2cIn ->
  -- | 1. Response to the byte controller
  --   2. Bus busy: a START condition has been detected, but no STOP condition
  --      since
  --   3. I2C bus output
  (Signal dom BitRespSig, Signal dom Bool, Signal dom I2cOut)
bitController clkCnt bitCtrl i2cI =
  mealyB bitControllerT bitControllerInit (clkCnt, bitCtrl, i2cI)

bitControllerInit :: BitControllerS
bitControllerInit =
  BitS
    { stateMachine = stateMachineStart
    , busState = busStartState
    , dout = high
    , dSclOen = False
    , clkEn = True
    , targetWait = False
    , cnt = 0
    }

bitControllerT :: BitControllerS -> BitControllerI -> (BitControllerS, BitControllerO)
bitControllerT s (clkCnt, (cmd, din), i2cI) =
  (s', ((sm.cmdAck, bus.al, s.dout), bus.busy, i2cO))
 where
  sm = s.stateMachine
  bus = s.busState

  s' =
    BitS
      { busState =
          busStatusCtrl clkCnt cmd s.clkEn i2cI sm.bitStateM sm.sdaChk sm.sdaOen bus
      , stateMachine = bitStateMachine bus.al s.clkEn cmd din sm
      , -- generate dout signal, store dout on rising edge of SCL
        dout = if sScl == high && dScl == low then sSda else s.dout
      , -- Whenever the target is not ready it can delay the cycle by pulling
        -- SCL low. Delay sclOen.
        dSclOen = sm.sclOen
      , clkEn = clkEnNext
      , -- targetWait is asserted when the controller wants to drive SCL high,
        -- but the target pulls it low. targetWait remains asserted until the
        -- target releases SCL.
        targetWait = (controllerSclHigh || s.targetWait) && sScl == low
      , cnt = cntNext
      }

  (sScl, sSda) = bus.sI2c
  dScl = fst bus.dI2c
  controllerSclHigh = sm.sclOen && not s.dSclOen

  -- controller drives SCL high, but another controller pulls it low
  -- controller start counting down its low cycle now (clock synchronization)
  sclSync = dScl == high && sScl == low && sm.sclOen

  -- generate clk enable signal
  (cntNext, clkEnNext)
    | s.cnt == 0 || sclSync = (clkCnt, True)
    | s.targetWait = (s.cnt, False)
    | otherwise = (s.cnt - 1, False)

  -- assign outputs
  i2cO = (low, sm.sclOen, low, sm.sdaOen)
