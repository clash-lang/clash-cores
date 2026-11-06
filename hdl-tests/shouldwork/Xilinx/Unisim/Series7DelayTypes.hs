module Series7DelayTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Series7.Delay
import UnisimTestCommon

-- | Domain of the control and data signals. The delay lines delay by less than
-- 3.1 ns (31 taps of 78 ps plus 600 ps), so a register on this clock sampling
-- the delayed data sees the same value as it would without delay.
type Dom = XilinxSystem

-- | Cycle from which on the test benches issue commands
startCycle :: Index 16
startCycle = 4

-- | Random commands, starting with a load at 'startCycle'
randomCommand :: Int -> Clock Dom -> Signal Dom DelayCommand
randomCommand seed clk =
  delay clk enableGen DelayHold (toCommand <$> cycleCount clk <*> genTestData seed clk)
 where
  toCommand :: Index 16 -> (Unsigned 3, Unsigned 5) -> DelayCommand
  toCommand cnt (op, n)
    | cnt < startCycle = DelayHold
    | cnt == startCycle = DelayLoad n
    | otherwise = case op of
        0 -> DelayHold
        1 -> DelayHold
        2 -> DelayIncrement
        3 -> DelayIncrement
        4 -> DelayDecrement
        5 -> DelayDecrement
        _ -> DelayLoad n

-- | An 'idelaye2' or 'odelaye2' with random inputs. Returns the delayed data
-- and the tap value, both sampled by a register.
dutDelay :: Bool -> DelaySource -> DelayConfig -> Clock Dom -> Signal Dom (Bit, Unsigned 5)
dutDelay isInput src cfg clk = delay clk enableGen (0, 0) (bundle (dataOut, taps))
 where
  cmd = randomCommand (randomSeed + 2) clk
  dataIn = delay clk enableGen 0 (genTestData randomSeed clk)
  (dataOut, taps)
    | isInput = idelaye2 src cfg clk cmd dataIn
    | otherwise = odelaye2 cfg clk cmd dataIn
