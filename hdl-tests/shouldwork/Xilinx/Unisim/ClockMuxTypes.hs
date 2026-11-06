{-# LANGUAGE AllowAmbiguousTypes #-}
module ClockMuxTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.ClockBuffer
import UnisimTestCommon

{- Domains of the clock multiplexer tests: the two input clocks, the clock of
the control signals and the output clock. The input and control clocks are
unrelated. All clocks have an edge at the start of the simulation, but after
that, no two edges (rising or falling) of different clocks come closer than
69 ps during the first 30 us, the length of the tests. The UNISIM model of
BUFGCTRL has delays of 1 ps in its latches, so the switching would be decided
by those delays if the edges came closer; see "Clash.Cores.Xilinx.Unisim.ClockBuffer".
The periods are long so that edges are far apart in a reasonable number of
cycles: 300 cycles of I0.
-}
createDomain vXilinxSystem{vName="M0", vPeriod=100000}
createDomain vXilinxSystem{vName="M1", vPeriod=91458}
createDomain vXilinxSystem{vName="MC", vPeriod=46490}
createDomain vXilinxSystem{vName="MO", vPeriod=91458}

-- | Number of cycles of the input clocks at the start of the simulation during
-- which the observations are masked. The UNISIM model does not run during the
-- global set/reset (the first 100 ns), so the multiplexer's state only agrees
-- with Clash simulation after the control signals have been stable for a
-- while after that.
startupCycles :: Index 32
startupCycles = 20

-- | Number of cycles of the control clock at the start of the simulation
-- during which the control signals select I0
stableCycles :: Index 32
stableCycles = 16

{- | The number of edges of the multiplexer's output between consecutive edges
of clock @dom@, which is one of the input clocks. Edges of the output caused by
edges of @dom@ come a few delta cycles after those edges in VHDL simulation,
so they are counted in the next cycle, in HDL simulation as well as in
Clash's. Edges of the output caused by edges of the other input happen in
between edges of @dom@, as the input clocks are unrelated.

Clash's output clock has an edge at the start of the simulation, and the
UNISIM model produces no edges during the global set/reset. Counting
differences (rather than a running total) makes the observations independent
of those, once the startup cycles are over.
-}
observe ::
  (KnownDomain dom) =>
  Clock dom ->
  Clock MO ->
  Signal dom (Unsigned 4)
observe clk clkO = mux (cycleCount clk .<=. pure startupCycles) 0 (resize <$> (cnt1 - cnt0))
 where
  cnt = delay clkO enableGen 0 (cnt + 1 :: Signal MO (Unsigned 8))
  cnt1 = delay clk enableGen 0 (unsafeSynchronizer clkO clk cnt)
  cnt0 = delay clk enableGen 0 cnt1

{- | Random control signals of one input of a 'bufgctrl', driven by registers.
They select the input (and nothing else) during the first 'stableCycles'
cycles if the first argument is 'True', and deselect it otherwise. After that,
the select toggles in one out of eight cycles, and the enable is deasserted
in one out of eight cycles.
-}
randomControl ::
  (KnownDomain dom) =>
  -- | Select during startup
  Bool ->
  Int ->
  Clock dom ->
  Signal dom BufgctrlControl
randomControl sel0 seed clk = ctrl
 where
  ctrl = delay clk enableGen initCtrl (next <$> cycleCount clk <*> genTestData seed clk <*> ctrl)
  initCtrl = BufgctrlControl{bufgctrlS = sel0, bufgctrlCe = True, bufgctrlIgnore = False}
  next :: Index 32 -> (Unsigned 3, Unsigned 3) -> BufgctrlControl -> BufgctrlControl
  next n (rS, rCe) c
    | n < stableCycles = initCtrl
    | otherwise =
        c { bufgctrlS = if rS == 0 then not (bufgctrlS c) else bufgctrlS c
          , bufgctrlCe = rCe /= 0
          }

-- | Random select, selecting I0 during the first 'stableCycles' cycles, and
-- toggling in one out of eight cycles after that
randomSelect :: (KnownDomain dom) => Int -> Clock dom -> Signal dom Bool
randomSelect seed clk = s
 where
  s = delay clk enableGen False (next <$> cycleCount clk <*> genTestData seed clk <*> s)
  next :: Index 32 -> Unsigned 3 -> Bool -> Bool
  next n r s0
    | n < stableCycles = False
    | otherwise = if r == 0 then not s0 else s0

-- | Observations of the output of a clock multiplexer from both input domains
type Dut = Clock M0 -> Clock M1 -> Clock MC -> (Signal M0 (Unsigned 4), Signal M1 (Unsigned 4))

observeBoth :: Clock M0 -> Clock M1 -> Clock MO -> (Signal M0 (Unsigned 4), Signal M1 (Unsigned 4))
observeBoth clk0 clk1 clkO = (observe clk0 clkO, observe clk1 clkO)

-- | Random enable, asserted during the first 'stableCycles' cycles, and
-- deasserted in one out of eight cycles after that
randomEnable :: (KnownDomain dom) => Int -> Clock dom -> Signal dom Bool
randomEnable seed clk =
  delay clk enableGen True
    ((\n r -> n < stableCycles || r /= (0 :: Unsigned 3)) <$> cycleCount clk <*> genTestData seed clk)

-- | 'bufgctrl' with the control signals of both inputs in domain 'MC'
dutBufgctrl :: BufgctrlConfig -> Dut
dutBufgctrl cfg clk0 clk1 clkC = observeBoth clk0 clk1 clkO
 where
  clkO =
    bufgctrl @M0 @MC @M1 @MC @MO
      cfg
      clk0 clkC (randomControl True randomSeed clkC)
      clk1 clkC (randomControl False (randomSeed + 1) clkC)

{- | 'bufgctrl' with both @IGNORE@s asserted, and the control signals of both
inputs in domain 'MC'. The selects are complementary: if both inputs could get
selected at the same time, which can happen when they ignore their clocks, the
UNISIM model oscillates.
-}
dutBufgctrlIgnore :: Dut
dutBufgctrlIgnore clk0 clk1 clkC = observeBoth clk0 clk1 clkO
 where
  s = randomSelect randomSeed clkC
  ctrl sel ce = BufgctrlControl{bufgctrlS = sel, bufgctrlCe = ce, bufgctrlIgnore = True}
  clkO =
    bufgctrl @M0 @MC @M1 @MC @MO
      bufgctrlDefaultConfig
      clk0 clkC (ctrl <$> (not <$> s) <*> randomEnable (randomSeed + 1) clkC)
      clk1 clkC (ctrl <$> s <*> randomEnable (randomSeed + 2) clkC)

-- | 'bufgctrl' with the control signals of each input in the domain of that
-- input
dutBufgctrlSync :: Dut
dutBufgctrlSync clk0 clk1 _clkC = observeBoth clk0 clk1 clkO
 where
  clkO =
    bufgctrl @M0 @M0 @M1 @M1 @MO
      bufgctrlDefaultConfig
      clk0 clk0 (randomControl True randomSeed clk0)
      clk1 clk1 (randomControl False (randomSeed + 1) clk1)

dutBufgmuxCtrl :: Dut
dutBufgmuxCtrl clk0 clk1 clkC =
  observeBoth clk0 clk1 (bufgmuxCtrl @M0 @M1 @MC @MO clk0 clk1 clkC (randomSelect randomSeed clkC))

dutBufgmux :: Dut
dutBufgmux clk0 clk1 clkC =
  observeBoth clk0 clk1 (bufgmux @M0 @M1 @MC @MO clk0 clk1 clkC (randomSelect randomSeed clkC))
