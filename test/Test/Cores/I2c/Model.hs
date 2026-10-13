{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Simulation models to test the I2C controller against: an I2C target with a
  register file, a host issuing byte commands to the controller, and an
  open-drain bus connecting any number of controllers to the target.
-}
module Test.Cores.I2c.Model where

import qualified Data.List as L
import Data.Maybe (fromMaybe)

import Clash.Cores.I2c
import Clash.Prelude

-- | Target address used throughout the tests
targetAddr :: BitVector 7
targetAddr = 0x34

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

-- | Byte-level command issued by the host
data Cmd = Cmd
  { start :: Bool
  , stop :: Bool
  , read :: Bool
  , write :: Bool
  , ackIn :: Bool
  , din :: BitVector 8
  , delay :: Int
  -- ^ Number of cycles to wait before issuing the command
  }
  deriving (Show, Eq, Generic, NFDataX)

noCmd :: Cmd
noCmd = Cmd False False False False False 0 0

writeCmd :: BitVector 8 -> Cmd
writeCmd b = noCmd{write = True, din = b}

readCmd :: Bool -> Cmd
readCmd ackIn = noCmd{read = True, ackIn}

addrByte :: BitVector 7 -> Bool -> BitVector 8
addrByte addr rd = addr ++# boolToBV rd

-- | Write @bytes@ to consecutive registers, starting at register @ptr@
writeTransaction :: BitVector 7 -> Index 16 -> [BitVector 8] -> [Cmd]
writeTransaction addr ptr bytes =
  [ (writeCmd (addrByte addr False)){start = True}
  , writeCmd (resize (pack ptr))
  ]
    <> withStop (L.map writeCmd bytes)

-- | Read @n@ consecutive registers, starting at register @ptr@
readTransaction :: BitVector 7 -> Index 16 -> Int -> [Cmd]
readTransaction addr ptr n =
  [ (writeCmd (addrByte addr False)){start = True}
  , writeCmd (resize (pack ptr))
  , (writeCmd (addrByte addr True)){start = True}
  ]
    -- Acknowledge every byte but the last
    <> withStop (L.replicate (n - 1) (readCmd False) <> [readCmd True])

withStop :: [Cmd] -> [Cmd]
withStop [] = [noCmd{stop = True}]
withStop cmds = L.init cmds <> [(L.last cmds){stop = True}]

{- | Host issuing commands to the I2C controller. Holds a command until it is
acknowledged. Drops all remaining commands when arbitration is lost.
-}
host ::
  (HiddenClockResetEnable dom) =>
  [Cmd] ->
  -- | Command acknowledge
  Signal dom Bool ->
  -- | Arbitration lost
  Signal dom Bool ->
  -- | Current command, if any, and whether the host has run out of commands
  Signal dom (Maybe Cmd, Bool)
host cmds0 = curry (moore hostT out (cmds0, 0 :: Int) . bundle)
 where
  out (cmd : _, waited) | waited >= cmd.delay = (Just cmd, False)
  out (cmds, _) = (Nothing, L.null cmds)

  hostT (cmds, waited) (hostAck, al)
    | al = ([], 0)
    | hostAck = (L.drop 1 cmds, 0)
    | otherwise = (cmds, waited + 1)

-- | Signals of a controller and its host, sampled every clock cycle
data Observation = Observation
  { cmd :: Maybe Cmd
  , done :: Bool
  -- ^ Host has run out of commands
  , dout :: BitVector 8
  , hostAck :: Bool
  , busy :: Bool
  , al :: Bool
  , ackOut :: Bool
  , scl :: Bit
  -- ^ SCL as driven by the controller
  , sda :: Bit
  -- ^ SDA as driven by the controller
  }
  deriving (Show, Generic, NFDataX)

controller ::
  (HiddenClockResetEnable dom) =>
  -- | Clock divider
  Unsigned 16 ->
  [Cmd] ->
  -- | Bus SCL and SDA
  Signal dom (Bit, Bit) ->
  Signal dom Observation
controller clkCnt cmds i2cI =
  Observation <$> cmd <*> done <*> dout <*> hostAck <*> busy <*> al <*> ackOut <*> scl <*> sda
 where
  (cmd, done) = unbundle (host cmds hostAck al)
  field f = fromMaybe False . fmap f <$> cmd
  (dout, hostAck, busy, al, ackOut, i2cO) =
    i2c
      (pure clkCnt)
      (field (.start))
      (field (.stop))
      (field (.read))
      (field (.write))
      (field (.ackIn))
      (maybe 0 (.din) <$> cmd)
      i2cI
  scl = (\(sclO, sclOen, _, _) -> if sclOen then high else sclO) <$> i2cO
  sda = (\(_, _, sdaO, sdaOen) -> if sdaOen then high else sdaO) <$> i2cO

-- | Controllers, each with a clock divider and commands, connected to a single
-- target
system ::
  (HiddenClockResetEnable dom) =>
  TargetConfig ->
  [(Unsigned 16, [Cmd])] ->
  Signal dom ([Observation], Vec 16 (BitVector 8))
system targetCfg controllers = bundle (sequenceA obss, regs)
 where
  obss = L.map (\(clkCnt, cmds) -> controller clkCnt cmds bus) controllers
  (targetOut, regs) = unbundle (i2cTarget targetCfg bus)

  -- Open-drain bus: any device can pull a line low
  bus = L.foldr (liftA2 wiredAnd) targetOut (L.map (fmap (\o -> (o.scl, o.sda))) obss)
  wiredAnd (scl0, sda0) (scl1, sda1) = (scl0 .&. scl1, sda0 .&. sda1)

{- | Simulate a system until all hosts have run out of commands, and the bus has
settled. Returns 'Nothing' if that takes longer than the given number of
cycles.
-}
simulate ::
  -- | Maximum number of cycles
  Int ->
  TargetConfig ->
  [(Unsigned 16, [Cmd])] ->
  Maybe [([Observation], Vec 16 (BitVector 8))]
simulate maxCycles targetCfg controllers
  | L.length settled == settle = Just (running <> settled)
  | otherwise = Nothing
 where
  allSamples = sampleN maxCycles (system @System targetCfg controllers)
  (running, done) = L.span (not . L.all (.done) . fst) (L.drop 1 allSamples)
  settled = L.take settle done
  -- Every command takes at least one clock divider period to complete
  settle = 20 * (1 + L.maximum (0 : L.map (fromIntegral . fst) controllers))

-- | Results of all commands a controller completed: the byte read, and the ACK
-- bit received.
results :: [Observation] -> [(BitVector 8, Bool)]
results obss = [(o.dout, o.ackOut) | o <- obss, o.hostAck]
