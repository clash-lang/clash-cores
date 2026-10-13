{-# LANGUAGE OverloadedRecordDot #-}

{- |
  Copyright   :  (C) 2016, University of Twente
                     2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  State machine of the I2C bit controller. Every I2C bit is divided into four
  (five for START) phases. A phase lasts until the next clock enable generated
  by the bit controller.
-}
module Clash.Cores.I2c.BitController.StateMachine where

import Clash.Prelude

import Clash.Cores.I2c.Types

data BitStateMachine
  = Idle
  | Start (Index 5)
  | Stop (Index 4)
  | Read (Index 4)
  | Write (Index 4)
  deriving (Eq, Show, Generic, NFDataX)

data StateMachine = StateMachine
  { sclOen :: Bool
  -- ^ SCL output enable register (active low)
  , sdaOen :: Bool
  -- ^ SDA output enable register (active low)
  , sdaChk :: Bool
  -- ^ Check SDA status (multi-controller arbitration)
  , cmdAck :: Bool
  -- ^ Command completed
  , bitStateM :: BitStateMachine
  -- ^ State machine
  }
  deriving (Show, Generic, NFDataX)

stateMachineStart :: StateMachine
stateMachineStart =
  StateMachine
    { sclOen = True
    , sdaOen = True
    , sdaChk = False
    , cmdAck = False
    , bitStateM = Idle
    }

bitStateMachine ::
  -- | Arbitration lost
  Bool ->
  -- | Clock enable
  Bool ->
  -- | Command
  I2cCommand ->
  -- | Bit to write
  Bit ->
  StateMachine ->
  StateMachine
bitStateMachine al clkEn cmd din s
  -- Release the bus when arbitration is lost
  | al = stateMachineStart
  | not clkEn = s{cmdAck = False}
  | otherwise = case s.bitStateM of
      Idle -> s{bitStateM = cmdState, sdaChk = False, cmdAck = False}
      --                      next     SCL      SDA    check  ack
      -- start
      Start 0 -> phase (Start 1) s.sclOen True False False -- set SDA high
      Start 1 -> phase (Start 2) True True False False -- set SCL high
      Start 2 -> phase (Start 3) True False False False -- set SDA low
      Start 3 -> phase (Start 4) True False False False -- keep SDA low
      Start _ -> phase Idle False False False True -- set SCL low
      -- stop
      Stop 0 -> phase (Stop 1) False False False False -- set SDA low
      Stop 1 -> phase (Stop 2) True False False False -- set SCL high
      Stop 2 -> phase (Stop 3) True False False False -- keep SCL high
      Stop _ -> phase Idle True True False True -- set SDA high
      -- read
      Read 0 -> phase (Read 1) False True False False -- tri-state SDA
      Read 1 -> phase (Read 2) True True False False -- set SCL high
      Read 2 -> phase (Read 3) True True False False -- keep SCL high
      Read _ -> phase Idle False True False True -- set SCL low
      -- write
      Write 0 -> phase (Write 1) False sdaBit False False -- set SDA
      Write 1 -> phase (Write 2) True sdaBit False False -- set SCL high
      -- Allow SCL and SDA some time to settle before checking SDA
      Write 2 -> phase (Write 3) True sdaBit True False -- check SDA
      Write _ -> phase Idle False sdaBit False True -- set SCL low
 where
  sdaBit = din == high

  cmdState = case cmd of
    I2cStart -> Start 0
    I2cStop -> Stop 0
    I2cWrite -> Write 0
    I2cRead -> Read 0
    I2cNop -> Idle

  phase next scl sda chk ack =
    StateMachine
      { bitStateM = next
      , sclOen = scl
      , sdaOen = sda
      , sdaChk = chk
      , cmdAck = ack
      }
