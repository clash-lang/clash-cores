module DelayTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Delay
import UnisimTestCommon

-- | Domain of the control and data signals. The delay lines delay by less than
-- 3 ns, so a register on this clock sampling the delayed data sees the same
-- value as it would without delay.
type Dom = XilinxSystem

-- | Cycle from which on the test benches issue commands. Before that, the
-- delay lines are reset and given time to come out of reset.
startCycle :: Index 16
startCycle = 10

-- | Reset, asserted during the first four cycles. Driven by a register, as the
-- resets are asynchronous.
--
-- The tests don't assert the reset after that: after a reset, the UNISIM
-- Verilog models report an undefined tap value (in Vivado's simulator) until a
-- load, and the VHDL models start accepting commands one cycle earlier than
-- the Verilog models.
initialReset :: Clock Dom -> Reset Dom
initialReset clk =
  unsafeFromActiveHigh $ delay clk enableGen True ((< 3) <$> cycleCount @_ @16 clk)

-- | Random commands, starting with a load at 'startCycle'. Loads are replaced
-- by holds for 'DelayVariable' delays, which don't support them.
randomCommand :: DelayType -> Int -> Clock Dom -> Signal Dom DelayCommand
randomCommand typ seed clk =
  delay clk enableGen DelayHold (toCommand <$> cycleCount clk <*> genTestData seed clk)
 where
  toCommand :: Index 16 -> (Unsigned 2, Unsigned 9) -> DelayCommand
  toCommand cnt (op, n)
    | cnt < startCycle = DelayHold
    | cnt == startCycle, typ /= DelayVariable = DelayLoad n
    | otherwise = case op of
        0 -> DelayHold
        1 -> DelayIncrement
        2 -> DelayDecrement
        _ | typ == DelayVariable -> DelayHold
          | otherwise -> DelayLoad n

-- | Random @EN_VTC@, asserted in one out of 8 cycles from 'startCycle' on
randomEnVtc :: Int -> Clock Dom -> Signal Dom Bool
randomEnVtc seed clk =
  delay clk enableGen False $
    (\cnt r -> cnt > startCycle && r == (0 :: Unsigned 3))
      <$> cycleCount clk
      <*> genTestData seed clk

-- | An 'idelaye3' or 'odelaye3' with random inputs. Returns the delayed data
-- and the tap value, both sampled by a register. The tap value is masked
--
--   * while @EN_VTC@ is asserted: the UNISIM VHDL models report an undefined
--     tap value then;
--
--   * for 'DelayFixed' delays: the UNISIM Verilog models report an undefined
--     tap value (in Vivado's simulator) after the global set/reset.
dutDelay :: Bool -> DelaySource -> DelayConfig -> Clock Dom -> Signal Dom (Bit, Unsigned 9)
dutDelay isInput src cfg clk = delay clk enableGen (0, 0) (bundle (dataOut, tapsMasked))
 where
  rst = initialReset clk
  cmd = randomCommand (delayType cfg) (randomSeed + 2) clk
  enVtc = randomEnVtc (randomSeed + 3) clk
  dataIn = delay clk enableGen 0 (genTestData randomSeed clk)
  (dataOut, taps)
    | isInput = idelaye3 src cfg clk rst enVtc cmd dataIn
    | otherwise = odelaye3 cfg clk rst enVtc cmd dataIn
  tapsMasked
    | delayType cfg == DelayFixed = pure 0
    | otherwise = mux enVtc 0 taps

-- | An 'idelayctrl', its output sampled by a register
dutIdelayctrl :: Clock Dom -> Signal Dom Bool
dutIdelayctrl clk = delay clk enableGen False (idelayctrl clk (initialReset clk))
