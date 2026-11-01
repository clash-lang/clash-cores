module ConfigTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.IcapE3
import Clash.Cores.Xilinx.Unisim.StartupE3
import UnisimTestCommon

-- | Domain of the test bench
type A = XilinxSystem

-- | Domain of the internal configuration oscillator: 50 MHz
createDomain vXilinxSystem{vName="Cfgm", vPeriod=hzToPeriod 50e6}

-- | The outputs of a 'startupE3':
--
--   * The number of edges of its oscillator clock in the last two cycles of
--     'A', which is 1 if it runs at 50 MHz. The phase of the oscillator in HDL
--     simulation doesn't match the one in Clash simulation, but this number
--     doesn't depend on it, except during the first cycles: the oscillator of
--     the UNISIM model starts at time 0, 100 ns before the first edge of the
--     test bench clock.
--
--   * End of startup: the UNISIM model asserts it after 100 ns.
--
-- The first two outputs are masked during the first 20 cycles.
--
--   * The reconfiguration request.
dutStartupE3 :: Clock A -> Signal A (Unsigned 8, Bool, Bool)
dutStartupE3 clk = bundle (mux startup 0 (cnt2 - cnt0), eos, startupPreq out)
 where
  out = startupE3 @Cfgm (pure 0) startupE3DefaultInput
  clkCfgm = startupCfgMClk out
  cntCfgm = delay clkCfgm enableGen 0 (cntCfgm + 1)
  cnt2 = delay clk enableGen 0 (unsafeSynchronizer clkCfgm clk cntCfgm)
  cnt1 = delay clk enableGen 0 cnt2
  cnt0 = delay clk enableGen 0 cnt1
  startup = cycleCount clk .<. pure (20 :: Index 32)
  eos = (||) <$> startup <*> startupEos out

-- | The outputs of an idle 'icapE3', except for 'icapAvail': the UNISIM model
-- only makes the port available after 1.2 ms of simulated time.
dutIcapE3 :: Clock A -> Signal A (BitVector 32, Bool, Bool)
dutIcapE3 clk = bundle (icapData out, icapPrDone out, icapPrError out)
 where
  out = icapE3 clk (pure False) (pure False) (pure 0)
