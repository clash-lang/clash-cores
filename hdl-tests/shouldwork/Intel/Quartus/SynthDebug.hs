{- | Synthesis test of "Clash.Cores.Intel.Debug": In-System Sources and Probes
and Virtual JTAG, see "QuartusSynth".
-}
module SynthDebug where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Debug

createDomain vSystem{vName="Tck", vPeriod=100000}

-- | A 16 bit virtual data register, initialized and probed by In-System
-- Sources and Probes
topEntity :: Clock System -> Signal System (BitVector 16)
topEntity clk = output
 where
  initial = sourcesProbes "INIT" (0xcafe :: BitVector 16) clk output
  output = unsafeSynchronizer (vjTck vj) clk dr
  vj = virtualJtag @Tck @2 (Just 1) (IrScan 1 :> DrScan 16 0xbeef :> Nil) (vjIrIn vj) tdo
  tdo = lsb <$> sr
  sr = register (vjTck vj) noReset enableGen 0 (shift <$> vjState vj <*> vjTdi vj <*> sr)
  shift (Just ShiftDr) b s = b +>>. s
  shift _ _ s = s
  dr = register (vjTck vj) noReset enableGen 0
    (mux ((== Just UpdateDr) <$> vjState vj) sr
      (mux (vjTestLogicReset vj) (unsafeSynchronizer clk (vjTck vj) initial) dr))
