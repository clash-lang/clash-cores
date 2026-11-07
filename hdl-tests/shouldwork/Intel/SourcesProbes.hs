{- | Test of "Clash.Cores.Intel.Debug.SourcesProbes" against the simulation
models of @altsource_probe@ in Intel's @altera_mf@ library. These models don't
simulate the JTAG connection, and don't drive the sources at all. So this test
only checks that HDL simulators elaborate and simulate the instances with
Intel's models: that the parameter and port names, types and widths match. A
test bench using the sources would not be checking anything.
-}
module SourcesProbes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Debug.SourcesProbes (sourcesProbes)
import Clash.Explicit.Testbench (tbClockGen)

import IntelIpSim (simLibraries)

data Probes = Probes
  { probeCounter :: Index 100
  , probeFlag :: Bool
  , probeVec :: Vec 3 (Signed 5)
  } deriving (Generic, BitPack, NFDataX)

sourcesProbesTB :: Signal System Bool
sourcesProbesTB = simLibraries <$> done
 where
  clk = tbClockGen (not <$> done)
  cnt = register clk noReset enableGen 0 (satSucc SatBound <$> cnt)
  -- Sources and probes of several types, a single bit source, no probes
  sources0 :: Signal System (Unsigned 8, Bool)
  sources0 = sourcesProbes "TB0" (0xab, True) clk (Probes <$> cnt <*> pure True <*> pure (repeat (-3)))
  sources1 :: Signal System Bit
  sources1 = sourcesProbes "" high clk cnt
  sources2 :: Signal System (Vec 2 (Signed 9))
  sources2 = sourcesProbes "" (-1 :> 100 :> Nil) clk (pure ())
  -- Make sure Clash doesn't remove the instances
  done0 = register clk noReset enableGen False ((== maxBound) <$> cnt)
  done = (sources0, sources1, sources2) `hwSeqX` done0
{-# ANN sourcesProbesTB (Synthesize "sourcesProbesTB" [] (PortName "done")) #-}
