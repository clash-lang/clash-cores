{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation model of Intel's Virtual JTAG IP, see
"Clash.Cores.Intel.Debug.VirtualJtag".
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Intel.Debug.VirtualJtag.Internal
  ( -- * Scans
    VirtualJtagScan(..)
  , simAction
  , checkScans
    -- * States
  , VirtualJtagState(..)
  , TapState(..)
  , tapNext
    -- * Model
  , ModelCycle(..)
  , hostTmsTdi
  , modelCycles
  ) where

import Prelude

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Char (toUpper)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Numeric (showHex)
import Numeric.Natural (Natural)

import Clash.Core.TermLiteral (TermLiteral(..), deriveTermLiteral)
import Clash.Sized.BitVector (Bit, high, low)
import Clash.Class.BitPack (BitPack)
import Clash.XException (NFDataX, ShowX)

-- | A scan of the JTAG host in simulation. See 'Clash.Cores.Intel.Debug.VirtualJtag.virtualJtag'.
data VirtualJtagScan
  = IrScan Integer
  -- ^ Virtual IR scan: shift the given value, of the width of the virtual
  -- instruction register, into the virtual instruction register
  | DrScan Natural Integer
  -- ^ Virtual DR scan of the given length (at least one bit), shifting in the
  -- given value, least significant bit first
  | Idle Natural
  -- ^ Stop the JTAG clock for the given number of milliseconds before the next
  -- scan. Only affects HDL simulation, see
  -- 'Clash.Cores.Intel.Debug.VirtualJtag.virtualJtag'.
  deriving (Show, Read, Eq, Lift)

deriveTermLiteral ''VirtualJtagScan

-- | States of the virtual JTAG state machine that the Virtual JTAG IP signals
-- to the design. They correspond to the states of the JTAG TAP controller while
-- the virtual instruction register (@Ir@ states) or a virtual data register
-- (@Dr@ states) is scanned.
data VirtualJtagState
  = CaptureDr
  | ShiftDr
  | Exit1Dr
  | PauseDr
  | Exit2Dr
  | UpdateDr
  | CaptureIr
  | UpdateIr
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFDataX, ShowX, BitPack)

-- | States of a JTAG TAP controller (IEEE 1149.1)
data TapState
  = TestLogicReset
  | RunTestIdle
  | SelectDrScan
  | CaptureDrTap
  | ShiftDrTap
  | Exit1DrTap
  | PauseDrTap
  | Exit2DrTap
  | UpdateDrTap
  | SelectIrScan
  | CaptureIrTap
  | ShiftIrTap
  | Exit1IrTap
  | PauseIrTap
  | Exit2IrTap
  | UpdateIrTap
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Next state of a TAP controller, for the given value of TMS
tapNext :: TapState -> Bool -> TapState
tapNext s tms = case s of
  TestLogicReset -> if tms then TestLogicReset else RunTestIdle
  RunTestIdle -> if tms then SelectDrScan else RunTestIdle
  SelectDrScan -> if tms then SelectIrScan else CaptureDrTap
  CaptureDrTap -> if tms then Exit1DrTap else ShiftDrTap
  ShiftDrTap -> if tms then Exit1DrTap else ShiftDrTap
  Exit1DrTap -> if tms then UpdateDrTap else PauseDrTap
  PauseDrTap -> if tms then Exit2DrTap else PauseDrTap
  Exit2DrTap -> if tms then UpdateDrTap else ShiftDrTap
  UpdateDrTap -> if tms then SelectDrScan else RunTestIdle
  SelectIrScan -> if tms then TestLogicReset else CaptureIrTap
  CaptureIrTap -> if tms then Exit1IrTap else ShiftIrTap
  ShiftIrTap -> if tms then Exit1IrTap else ShiftIrTap
  Exit1IrTap -> if tms then UpdateIrTap else PauseIrTap
  PauseIrTap -> if tms then Exit2IrTap else PauseIrTap
  Exit2IrTap -> if tms then UpdateIrTap else ShiftIrTap
  UpdateIrTap -> if tms then SelectDrScan else RunTestIdle

-- | Value of the @sld_sim_action@ parameter, which makes the simulation models
-- of the Virtual JTAG IP perform the scans. Scans are separated by commas, and
-- consist of the time to wait before the scan (in milliseconds), the type (1
-- for an IR scan, 2 for a DR scan), the value and its length, all in
-- hexadecimal. 'Idle' adds to the time of the next scan.
simAction ::
  -- | Width of the virtual instruction register
  Int ->
  [VirtualJtagScan] ->
  -- | @sld_sim_action@, @sld_sim_n_scan@ and @sld_sim_total_length@
  (String, Int, Integer)
simAction irWidth scans0 =
  ( if null scans1 then "" else "(" <> intercalate "," (map render scans1) <> ")"
  , length scans1
  , sum [l | (_, _, _, l) <- scans1] )
 where
  scans1 = go 0 (checkScans irWidth scans0)
  go t = \case
    [] -> []
    Idle d : ss -> go (t + d) ss
    IrScan v : ss -> (t, 1 :: Int, v, toInteger irWidth) : go 0 ss
    DrScan l v : ss -> (t, 2, v, toInteger l) : go 0 ss
  render (t, ty, v, l) =
    "(" <> intercalate "," [hex (toInteger t), show ty, hexDigits l (v .&. (2 ^ l - 1)), hex l] <> ")"
  hex x = map toUpper (showHex x "")
  -- Exactly as many digits as needed for a value of the given length
  hexDigits l x =
    let s = hex x
        n = fromInteger ((l + 3) `div` 4)
    in replicate (n - length s) '0' <> s

-- | Check that the scans start with an 'IrScan', that DR scans have at least
-- one bit, and that the values fit
checkScans :: Int -> [VirtualJtagScan] -> [VirtualJtagScan]
checkScans irWidth scans
  | (DrScan _ _ : _) <- dropWhile isIdle scans =
      err "the first scan must be an IrScan: the virtual instruction register selects the data register"
  | any isEmptyDr scans = err "DR scans must have at least one bit"
  | (s : _) <- filter (not . fits) scans = err ("the value doesn't fit in the scan: " <> show s)
  | otherwise = scans
 where
  err msg = error ("virtualJtag: " <> msg)
  isIdle = \case Idle _ -> True; _ -> False
  isEmptyDr = \case DrScan 0 _ -> True; _ -> False
  fits = \case
    IrScan v -> v >= 0 && v < 2 ^ irWidth
    DrScan l v -> v >= 0 && v < 2 ^ l
    Idle _ -> True

-- | Values of TMS and TDI that the JTAG host in the simulation models of
-- Intel's Virtual JTAG IP drives, for every cycle of TCK. The JTAG TAP
-- controller samples them at the rising edge at the end of the cycle.
--
-- The host resets the TAP controller, loads the @USER1@ instruction, performs
-- the scans, and resets the TAP controller again. A virtual IR scan is a DR
-- scan with the @USER1@ instruction: a scan that tells the hub to capture
-- the virtual instruction register, followed by a scan of the virtual
-- instruction register and a select bit. A virtual DR scan is a DR scan with
-- the @USER0@ instruction. The host switches between these instructions when
-- needed, but it doesn't know about the @USER1@ instruction it loads after
-- the reset until after the first scan. So it loads it again for the first
-- scan, which must be an IR scan (see 'checkScans').
hostTmsTdi ::
  -- | Width of the virtual instruction register
  Int ->
  [VirtualJtagScan] ->
  [(Bool, Bit)]
hostTmsTdi irWidth scans =
     reset
  <> loadIr user1
  <> go False (checkScans irWidth scans)
  <> replicate 6 (True, low)
 where
  -- Hold TMS high to reset the TAP controller, move to Run-Test/Idle
  reset = replicate 6 (True, low) <> [(False, low)]

  -- From Run-Test/Idle or an Update state, load an instruction and move to
  -- Update-IR. The last bit is shifted when leaving Shift-IR.
  loadIr bits =
       [(True, low), (True, low), (False, low), (False, low)]
    <> [(False, b) | b <- init bits]
    <> [(True, last bits), (True, low)]

  -- The bits of the instructions, as the host shifts them in. Note that it
  -- shifts in one bit more for USER0, which doesn't matter: the hub only
  -- distinguishes USER1 from the other instructions.
  user1 = bitsOf 10 0x00E
  user0 = bitsOf 11 0x00C

  -- From Run-Test/Idle or an Update state, shift the bits into the data
  -- register and move to Update-DR
  shiftDr bits =
       [(True, low), (False, low), (False, low)]
    <> [(False, b) | b <- init bits]
    <> [(True, last bits), (True, last bits)]

  -- The first argument is whether the host knows the TAP controller has the
  -- USER1 instruction
  go :: Bool -> [VirtualJtagScan] -> [(Bool, Bit)]
  go user1Known = \case
    [] -> []
    Idle _ : ss -> go user1Known ss
    IrScan v : ss ->
         (if user1Known then [] else loadIr user1)
      <> shiftDr ([high, high, low, low] <> replicate (irWidth - 4) low <> [low])
      <> shiftDr (bitsOf irWidth v <> [high])
      <> go True ss
    DrScan l v : ss ->
         (if user1Known then loadIr user0 else [])
      <> shiftDr (bitsOf (fromIntegral l) v)
      <> go False ss

  bitsOf :: Int -> Integer -> [Bit]
  bitsOf n v = [if testBit v i then high else low | i <- [0 .. n - 1]]

-- | Outputs of the Virtual JTAG IP during a cycle of TCK, as a design clocked
-- by TCK sees them at the rising edge at the end of the cycle.
data ModelCycle = ModelCycle
  { mcTap :: TapState
  -- ^ State of the TAP controller. Before the first edge of TCK, it is
  -- Test-Logic-Reset, as on hardware. (Depending on the simulator, the HDL
  -- models start in an initial state or in Test-Logic-Reset.)
  , mcState :: Maybe VirtualJtagState
  -- ^ State of the virtual JTAG state machine
  , mcTdi :: Bit
  -- ^ TDI. Only meaningful in 'ShiftDr'; holds its value otherwise.
  , mcIrIn :: Integer
  -- ^ Virtual instruction register
  } deriving (Show, Eq)

-- | The outputs of the simulation model, cycle by cycle, until the host is
-- done. After that, the TAP controller stays in Test-Logic-Reset.
modelCycles ::
  -- | Width of the virtual instruction register
  Int ->
  [VirtualJtagScan] ->
  [ModelCycle]
modelCycles irWidth scans = go initial (hostTmsTdi irWidth scans)
 where
  -- Width of the hub's shift register: the virtual instruction register, a
  -- select bit and three more bits
  hubWidth = irWidth + 4
  msb x = testBit x (hubWidth - 1)
  -- The virtual instruction register is held in the bits below the select bit
  irSlice x = (x `shiftR` 3) .&. (2 ^ irWidth - 1)

  initial = St
    { stTap = TestLogicReset, stIr = 0, stUser1 = False
    , stHub = 0, stHold = 0, stTdi = low }

  go st [] = cycles st (True, low)
  go st (i : is) = let (c, st') = step st i in c : go st' is

  -- After the host is done, TCK stops. In Clash simulation it continues, with
  -- TMS held high.
  cycles st i = let (c, st') = step st i in c : cycles st' i

  step st@St{..} (tms, tdi) =
    let
      isTap s = stTap == s
      tlr = isTap TestLogicReset
      -- The hub registers the instruction register at the falling edge in the
      -- middle of this cycle, if a scan with the select bit set ends.
      hold
        | tlr = 0
        | isTap Exit1DrTap && msb stHub = stHub
        | otherwise = stHold
      sel = msb hold
      virtualState = case stTap of
        CaptureDrTap | not stUser1 && sel -> Just CaptureDr
        ShiftDrTap | not stUser1 && sel -> Just ShiftDr
        Exit1DrTap | not stUser1 && sel -> Just Exit1Dr
        PauseDrTap | not stUser1 && sel -> Just PauseDr
        Exit2DrTap | not stUser1 && sel -> Just Exit2Dr
        UpdateDrTap | not stUser1 && sel -> Just UpdateDr
        CaptureDrTap | stUser1 && sel -> Just CaptureIr
        UpdateDrTap | stUser1 && sel -> Just UpdateIr
        _ -> Nothing
      tdiOut
        | tlr = low
        | isTap ShiftDrTap && not stUser1 = tdi
        | otherwise = stTdi
      out = ModelCycle
        { mcTap = stTap
        , mcState = virtualState
        , mcTdi = tdiOut
        , mcIrIn = irSlice hold
        }
      -- The rising edge at the end of this cycle
      tap' = tapNext stTap tms
      reset' = tlr || tap' == TestLogicReset
      ir'
        | reset' = 0 :: Integer
        | isTap ShiftIrTap = (stIr `shiftR` 1) .|. (if tdi == high then 1 `shiftL` 9 else 0)
        | otherwise = stIr
      user1'
        | reset' = False
        | isTap UpdateIrTap = stIr == 0x00E
        | otherwise = stUser1
      hub'
        | reset' = 0
        | isTap ShiftDrTap && stUser1 =
            (stHub `shiftR` 1) .|. (if tdi == high then 1 `shiftL` (hubWidth - 1) else 0)
        | otherwise = stHub
    in
      ( out
      , st { stTap = tap', stIr = ir', stUser1 = user1', stHub = hub'
           , stHold = hold, stTdi = tdiOut } )

-- | State of the model, after a rising edge of TCK
data St = St
  { stTap :: TapState
  , stIr :: Integer
  -- ^ Instruction register of the TAP controller
  , stUser1 :: Bool
  -- ^ Whether the TAP controller has the USER1 instruction
  , stHub :: Integer
  -- ^ Shift register of the hub
  , stHold :: Integer
  -- ^ Hold register of the hub
  , stTdi :: Bit
  -- ^ Last value of TDI shifted into a virtual data register
  }
