{-# LANGUAGE TemplateHaskell #-}

module AltsyncramDualClockTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Memory

import AltsyncramTypes (Dat, Addr, Op, withInit)
import IntelTestCommon

import qualified Prelude as P

-- | Domains of port B, faster and slower than 'System'
createDomain vSystem{vName = "B7", vPeriod = 7000}
createDomain vSystem{vName = "B25", vPeriod = 25000}

-- | Stimuli of a port: enable and operation
type PortStim = (Bool, Op)

{- | Random stimuli for both ports, for a test of 6 microseconds, with port A
at 10 ns and port B at the given period (in nanoseconds). Both end with
cycles without writes, so that the outputs settle while the other port
finishes. Writes only happen if the port writes.
-}
genStimuli :: Bool -> Bool -> Int -> Int -> ([PortStim], [PortStim])
genStimuli writesA writesB bPeriod = evalG $ do
  as <- listOf (duration `div` 10) (genStim writesA)
  bs <- listOf (duration `div` bPeriod) (genStim writesB)
  pure (as <> idle (300 `div` 10), bs <> idle (300 `div` bPeriod))
 where
  duration = 6000
  idle n = P.replicate n (True, (0, Nothing))
  genStim writes = do
    en <- not <$> chance 10
    a <- elements [0 .. maxBound]
    w <- if writes then chance 50 else pure False
    be <- elements [0b01, 0b10, 0b11, 0b11]
    d <- genBV
    pure (en, (a, if w then Just (be, d) else Nothing))

-- | A ROM, reading the address of port A
dutRom ::
  (KnownSymbol fam) =>
  SSymbol fam ->
  PortConfig ->
  Clock System ->
  Signal System PortStim ->
  Signal System Dat
dutRom fam cfgA clk stim =
  altsyncramRom fam withInit cfgA clk (toEnable (fst <$> stim)) (fst . snd <$> stim)

-- | A dual-port ROM
dutDpRom ::
  (KnownSymbol fam, KnownDomain b) =>
  SSymbol fam ->
  MixedReadDuringWrite ->
  PortConfig ->
  PortConfig ->
  Circuit2 System b PortStim PortStim Dat Dat
dutDpRom fam mixed cfgA cfgB clkA clkB stimA stimB =
  altsyncramDualPortRom fam withInit{mixedReadDuringWrite = mixed} cfgA cfgB
    clkA (toEnable (fst <$> stimA)) (fst . snd <$> stimA)
    clkB (toEnable (fst <$> stimB)) (fst . snd <$> stimB)

-- | A simple dual-port RAM with two clocks, writing with the operation of port
-- A and reading the address of port B, unless port B's operation is a write.
-- Port A has no output: it returns a constant.
dutSdp ::
  (KnownSymbol fam, KnownDomain b) =>
  SSymbol fam ->
  AltsyncramConfig ->
  PortConfig ->
  Circuit2 System b PortStim PortStim Bit Dat
dutSdp fam cfg cfgB clkA clkB stimA stimB =
  ( pure 0
  , altsyncramSimpleDualPortDualClock fam cfg cfgB
      clkA (toEnable (fst <$> stimA)) (toWrite . snd <$> stimA)
      clkB (toEnable (fst <$> stimB)) (toRead . snd <$> stimB) )
 where
  toWrite (a, w) = (\(be, d) -> (a, be, d)) <$> w
  toRead (a, w) = maybe (Just a) (const Nothing) w

-- | A true dual-port RAM with two clocks
dutTdp ::
  (KnownSymbol fam, KnownDomain b) =>
  SSymbol fam ->
  AltsyncramConfig ->
  PortConfig ->
  PortConfig ->
  Circuit2 System b PortStim PortStim Dat Dat
dutTdp fam cfg cfgA cfgB clkA clkB stimA stimB =
  altsyncramTrueDualPortDualClock fam cfg cfgA cfgB
    clkA (toEnable (fst <$> stimA)) (snd <$> stimA)
    clkB (toEnable (fst <$> stimB)) (snd <$> stimB)

-- | Intel's simulation model of M9K blocks only supports 'MixedOldData'
oldMixed :: AltsyncramConfig
oldMixed = defAltsyncramConfig{mixedReadDuringWrite = MixedOldData}
