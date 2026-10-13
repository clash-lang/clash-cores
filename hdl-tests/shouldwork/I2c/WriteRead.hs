{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

{- | Write a number of registers of an I2C target, and read them back using a
repeated START condition. The target stretches the clock.
-}
module WriteRead where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Prelude (withClockResetEnable)

import Clash.Cores.I2c
import Target

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Unsigned 16) ->
  Signal System Bool ->
  Signal System Bool ->
  Signal System Bool ->
  Signal System Bool ->
  Signal System Bool ->
  Signal System (BitVector 8) ->
  Signal System I2cIn ->
  ( Signal System (BitVector 8)
  , Signal System Bool
  , Signal System Bool
  , Signal System Bool
  , Signal System Bool
  , Signal System I2cOut
  )
topEntity clk rst en = withClockResetEnable clk rst en i2c
{-# OPAQUE topEntity #-}

-- | Host command: start, stop, read, write, ackIn, din
type Cmd = (Bool, Bool, Bool, Bool, Bool, BitVector 8)

noCmd :: Cmd
noCmd = (False, False, False, False, False, 0)

addr :: BitVector 7
addr = 0x34

initRegs :: Vec 16 (BitVector 8)
initRegs = iterate d16 (+ 0x11) 0x01

-- | Write 0xDEADBEEF to registers 3 to 6, then read registers 2 to 7
cmds :: Vec 15 Cmd
cmds =
  -- Write transaction
  (True, False, False, True, False, addr ++# 0)
    :> (False, False, False, True, False, 3)
    :> (False, False, False, True, False, 0xDE)
    :> (False, False, False, True, False, 0xAD)
    :> (False, False, False, True, False, 0xBE)
    :> (False, True, False, True, False, 0xEF)
    -- Read transaction
    :> (True, False, False, True, False, addr ++# 0)
    :> (False, False, False, True, False, 2)
    :> (True, False, False, True, False, addr ++# 1)
    :> (False, False, True, False, False, 0)
    :> (False, False, True, False, False, 0)
    :> (False, False, True, False, False, 0)
    :> (False, False, True, False, False, 0)
    :> (False, False, True, False, False, 0)
    -- NACK the last byte
    :> (False, True, True, False, True, 0)
    :> Nil

expectedRegs :: Vec 16 (BitVector 8)
expectedRegs = imap upd initRegs
 where
  upd i r = case i of
    3 -> 0xDE
    4 -> 0xAD
    5 -> 0xBE
    6 -> 0xEF
    _ -> r

expectedReads :: Vec 6 (BitVector 8)
expectedReads = select d2 d1 d6 expectedRegs

testBench :: Signal System Bool
testBench = done
 where
  clk = tbClockGen (not <$> done)
  rst = resetGen
  en = enableGen

  (dout, hostAck, _busy, al, ackOut, i2cO) =
    topEntity clk rst en (pure 9) start stop rd wr ackIn din bus

  -- Host: issue the next command after every command acknowledge
  cmdIdx :: Signal System (Index 16)
  cmdIdx = register clk rst en 0 (mux hostAck (satSucc SatBound <$> cmdIdx) cmdIdx)
  cmd = (\i -> if i == maxBound then noCmd else cmds !! i) <$> cmdIdx
  (start, stop, rd, wr, ackIn, din) = unbundle cmd

  readIdx :: Signal System (Index 6)
  readIdx = register clk rst en 0 (mux readAck (satSucc SatBound <$> readIdx) readIdx)
  readAck = hostAck .&&. rd
  writeAck = hostAck .&&. wr
  expectedRead = (expectedReads !!) <$> readIdx

  -- Open-drain bus
  (targetOut, regs) =
    unbundle
      ( withClockResetEnable clk rst en $
          i2cTarget (TargetConfig{addr, stretch = 5, regs = initRegs}) bus
      )
  controllerOut =
    (\(sclO, sclOen, sdaO, sdaOen) -> (drive sclO sclOen, drive sdaO sdaOen)) <$> i2cO
  drive o oen = if oen then high else o
  bus =
    liftA2
      (\(scl0, sda0) (scl1, sda1) -> (scl0 .&. scl1, sda0 .&. sda1))
      controllerOut
      targetOut

  cycles :: Signal System (Unsigned 16)
  cycles = register clk rst en 0 (cycles + 1)
  finished = cmdIdx .==. pure maxBound
  timeout = cycles .==. 30_000

  done =
    assert clk rst "Read data" (mux readAck dout expectedRead) expectedRead $
      assert clk rst "Target ACK" (ackOut .&&. writeAck) (pure False) $
        assert clk rst "Arbitration lost" al (pure False) $
          assert clk rst "Timeout" timeout (pure False) $
            assert clk rst "Registers" (mux finished regs (pure expectedRegs)) (pure expectedRegs) $
              finished .||. timeout
{-# OPAQUE testBench #-}
