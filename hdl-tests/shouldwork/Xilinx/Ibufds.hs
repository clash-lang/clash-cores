module Ibufds where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Ibufds
import Clash.Explicit.Testbench
import Clash.Xilinx.ClockGen

type Dom = XilinxSystem

topEntity ::
  DiffClock Dom ->
  Reset Dom ->
  Signal Dom (Index 10)
topEntity diffClk rst = o
 where
  clk = ibufdsClock diffClk
  o = register clk rst enableGen 0 $ fmap (satSucc SatBound) o
{-# OPAQUE topEntity #-}

testBench ::
  Signal Dom Bool
testBench = done
 where
  o = topEntity clkDiff rst
  done = o .==. pure maxBound
  clkSE = tbClockGen (not <$> done)
  clkDiff = clockToDiffClock clkSE
  rst = resetGen
{-# OPAQUE testBench #-}
