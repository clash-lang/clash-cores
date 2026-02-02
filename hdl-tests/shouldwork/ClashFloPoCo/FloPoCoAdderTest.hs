{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module FloPoCoAdderTest where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.ClashFloPoCo.Example

-- | Top entity that uses the FloPoCo adder
--
-- Takes two floating point inputs and produces their sum.
-- The result is pipelined with 2 cycles of latency.
topEntity
  :: Clock XilinxSystem
  -> Signal XilinxSystem Float
  -> Signal XilinxSystem Float
  -> Signal XilinxSystem Float
topEntity clk a b =
  let a' = toSignal (fromSignal @XilinxSystem a :: DSignal XilinxSystem 0 Float)
      b' = toSignal (fromSignal @XilinxSystem b :: DSignal XilinxSystem 0 Float)
      result = floPoCoAdder clk a' b'
  in toSignal result
{-# OPAQUE topEntity #-}

-- | Testbench that verifies the FloPoCo adder
testBench :: Signal XilinxSystem Bool
testBench = done
  where
    testInput = stimuliGenerator clk rst
      $(listToVecTH [(1.0 :: Float, 2.0 :: Float)
                    ,(3.5, 4.5)
                    ,(10.0, 20.0)
                    ,(-5.0, 5.0)
                    ,(0.0, 0.0)
                    ])
    
    expectedOutput = outputVerifier' clk rst
      $(listToVecTH ([undefined, undefined] -- Pipeline delay of 2 cycles
                  ++ [3.0 :: Float, 8.0, 30.0, 0.0, 0.0]))
    
    done = expectedOutput actualOutput
    actualOutput = topEntity clk (fst <$> testInput) (snd <$> testInput)
    clk = tbClockGen (not <$> done)
    rst = resetGen
