module Series7ConfigTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Series7.Config
import UnisimTestCommon

-- | Domain of the test bench
type A = XilinxSystem

-- | Domain of the test bench of 'startupE2': two periods of its oscillator,
-- see 'dutStartupE2'
createDomain vXilinxSystem{vName="S", vPeriod=2 * 15384}

-- | Domain of the internal configuration oscillator of 'startupE2', with the
-- period of the UNISIM model's oscillator
createDomain vXilinxSystem{vName="Cfgm", vPeriod=15384}

-- | The outputs of a 'startupE2':
--
--   * The number of edges of its oscillator clock in the last cycle of 'S',
--     which is 2. The phase of the oscillator in HDL simulation doesn't match
--     the one in Clash simulation, but this number doesn't depend on it,
--     except during the first cycles: the oscillator of the UNISIM model starts
--     at time 0, one period of 'S' before the first edge of the test bench
--     clock.
--
--   * End of startup: the UNISIM model asserts it after 100 ns.
--
-- The first two outputs are masked during the first 10 cycles.
--
--   * The reconfiguration request.
dutStartupE2 :: Clock S -> Signal S (Unsigned 8, Bool, Bool)
dutStartupE2 clk = bundle (mux startup 0 (cnt1 - cnt0), eos, startupPreq out)
 where
  out = startupE2 @Cfgm startupE2DefaultInput
  clkCfgm = startupCfgMClk out
  cntCfgm = delay clkCfgm enableGen 0 (cntCfgm + 1)
  cnt1 = delay clk enableGen 0 (unsafeSynchronizer clkCfgm clk cntCfgm)
  cnt0 = delay clk enableGen 0 cnt1
  startup = cycleCount clk .<. pure (10 :: Index 32)
  eos = (||) <$> startup <*> startupEos out

-- | The output of an idle 'icapE2'. The UNISIM model only finishes its
-- initialization after 1.2 ms of simulated time; it outputs 0 until then.
dutIcapE2 :: Clock A -> Signal A (BitVector 32)
dutIcapE2 clk = icapE2 clk (pure False) (pure True) (pure 0)

-- | Number of cycles to wait for the simulated global set/reset of the UNISIM
-- models to end (after 100 ns): the VHDL model of @DNA_PORT@ ignores its clock
-- until then.
startupCycles :: Index 32
startupCycles = 16

-- | 'readDnaPort', in reset until 'startupCycles'
dutReadDnaPort :: Clock A -> Signal A (Maybe (BitVector 57))
dutReadDnaPort clk = readDnaPort clk rst enableGen simDna
 where
  rst = unsafeFromActiveHigh (delay clk enableGen True (cycleCount clk .<. pure startupCycles))

-- | A 'dnaPort' with random inputs. It reads at edge @startupCycles + 1@, and
-- in 2 out of 16 cycles after that. Its output is masked until that first read.
dutDnaPort :: Clock A -> Signal A Bit
dutDnaPort clk = mux (cycleCount clk .<=. pure (startupCycles + 1)) 0 dout
 where
  dout = dnaPort simDna clk din rd sh
  rnd = genTestData @A @(Unsigned 4, Bool, Bit) randomSeed clk
  rd = delay clk enableGen False $
    (\n (r, _, _) -> n == startupCycles || (n > startupCycles && r < 2))
      <$> cycleCount clk <*> rnd
  sh = delay clk enableGen False ((\(_, s, _) -> s) <$> rnd)
  din = delay clk enableGen 0 ((\(_, _, d) -> d) <$> rnd)
