{- | Synthesis test of the internal oscillator, for device families without a
chip ID block, see "QuartusSynth".
-}
module SynthOscillator where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Config

createDomain vSystem{vName="Osc", vPeriod=hzToPeriod 80e6}

topEntity :: Signal Osc (Unsigned 8)
topEntity = cnt
 where
  clk = internalOscillator @Osc
  cnt = register clk resetGen enableGen 0 (cnt + 1)
