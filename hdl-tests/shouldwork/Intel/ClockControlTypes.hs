module ClockControlTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Clock

import IntelTestCommon

import qualified Prelude as P

-- | Domain of the output clock
createDomain vSystem{vName = "Out", vPeriod = 10000}

-- | Random enables, in phases that are mostly asserted, mostly deasserted, or
-- in between. They end asserted, as the model of the output clock needs to know
-- when its next edge is.
genStimuli :: Int -> [Bool]
genStimuli = evalG $ (<> [True]) . P.concat <$> listOf 20 (do
  p <- elements [0, 10, 50, 90, 100]
  listOf 20 (chance p))

{- | A counter clocked by the output clock, observed from the input domain. The
counter is sampled from the input domain, not the other way around: the
output clock's edges arrive a delta cycle after those of the input clock in
HDL simulation.
-}
observe :: Clock System -> Clock Out -> Signal System (Unsigned 8)
observe clkA clkB = delay clkA enableGen 0 (unsafeSynchronizer clkB clkA cnt)
 where
  cnt = delay clkB enableGen 0 (cnt + 1)

dut :: ClockControlFamily -> Clock System -> Signal System Bool -> Signal System (Unsigned 8)
dut fam clk ena = observe clk (altclkctrl @System @Out fam clk ena)
