{- | Synthesis test of "Clash.Cores.Intel.Config": the chip ID, read with the
internal oscillator, see "QuartusSynth".
-}
module SynthConfig where

import Clash.Explicit.Prelude

import Data.Maybe (isJust)

import Clash.Cores.Intel.Config

createDomain vSystem{vName="Osc", vPeriod=hzToPeriod 116e6}

-- | Outputs a bit of the chip ID, selected by the input, so that Quartus
-- keeps all of it
topEntity :: Signal Osc (Index 64) -> Signal Osc (Bit, Bool)
topEntity sel = bundle (idBit, isJust <$> cid)
 where
  clk = internalOscillator @Osc
  cid = chipId 0x0123456789abcdef clk resetGen
  idBit = (\c i -> maybe 0 (! i) c) <$> cid <*> sel
