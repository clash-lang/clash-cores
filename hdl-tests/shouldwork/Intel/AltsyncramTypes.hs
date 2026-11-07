{-# LANGUAGE TemplateHaskell #-}

module AltsyncramTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Memory

import IntelTestCommon
import Language.Haskell.TH.Syntax (lift, runIO)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

-- | Number of cycles per test bench
nCycles :: Int
nCycles = 600

type Dat = BitVector 16
type Addr = Index 6
type Op = (Addr, Maybe (BitVector 2, Dat))

-- | Stimuli: enable, and the operation of both ports
type Stimuli = (Bool, Op, Op)

-- | Random operations on a small memory, so that the ports often access the
-- same address
genStimuli :: Int -> [Stimuli]
genStimuli = evalG $ listOf nCycles $ (,,) <$> (not <$> chance 10) <*> genOp <*> genOp
 where
  genOp = do
    a <- elements [0 .. maxBound]
    w <- chance 50
    be <- elements [0b01, 0b10, 0b11, 0b11]
    d <- genBV
    pure (a, if w then Just (be, d) else Nothing)

{- | Configuration with an initialization file, written to the temporary
directory while compiling
-}
withInit :: AltsyncramConfig
withInit =
  defAltsyncramConfig
    { initFile = Just $(runIO (do
        tmp <- getTemporaryDirectory
        let path = tmp </> "clash-cores-intel-altsyncram-test.mif"
        writeMifFile path ([0x1234, 0xabcd, 0x0f0f, 0xffff, 0x0001] :: [BitVector 16])
        pure path) >>= lift)
    }

-- | A single-port RAM, using the operation of port A
dutSp ::
  (KnownSymbol fam) =>
  SSymbol fam ->
  AltsyncramConfig ->
  PortConfig ->
  Clock System ->
  Signal System Stimuli ->
  Signal System Dat
dutSp fam cfg cfgA clk stim = altsyncramSinglePort fam cfg cfgA clk (toEnable en) addr wr
 where
  (en, opA, _) = unbundle stim
  (addr, wr) = unbundle opA

-- | A simple dual-port RAM, writing with the operation of port A and reading
-- the address of port B, unless port B's operation is a write
dutSdp ::
  (KnownSymbol fam) =>
  SSymbol fam ->
  AltsyncramConfig ->
  PortConfig ->
  Clock System ->
  Signal System Stimuli ->
  Signal System Dat
dutSdp fam cfg cfgB clk stim =
  altsyncramSimpleDualPort fam cfg cfgB clk (toEnable en) (toWrite <$> opA) (toRead <$> opB)
 where
  (en, opA, opB) = unbundle stim
  toWrite (a, w) = (\(be, d) -> (a, be, d)) <$> w
  toRead (a, w) = maybe (Just a) (const Nothing) w

-- | A true dual-port RAM
dutTdp ::
  (KnownSymbol fam) =>
  SSymbol fam ->
  AltsyncramConfig ->
  PortConfig ->
  PortConfig ->
  Clock System ->
  Signal System Stimuli ->
  Signal System (Dat, Dat)
dutTdp fam cfg cfgA cfgB clk stim =
  bundle (altsyncramTrueDualPort fam cfg cfgA cfgB clk (toEnable en) opA opB)
 where
  (en, opA, opB) = unbundle stim

port :: Bool -> ReadDuringWrite -> PortConfig
port = PortConfig
