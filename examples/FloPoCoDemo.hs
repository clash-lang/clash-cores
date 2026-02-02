{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- |
Simple demonstration of the ClashFloPoCo Example module.

This module can be compiled with Clash to generate HDL:

@
cabal run clash -- examples/FloPoCoDemo.hs --vhdl
@

This will generate VHDL files in the @vhdl@ directory.
-}
module FloPoCoDemo where

import Clash.Explicit.Prelude
import Clash.Cores.ClashFloPoCo.Example

-- | Simple top entity that adds two floating point numbers
--
-- This demonstrates the FloPoCo integration by creating a pipelined
-- floating point adder. The inputs are delayed by 0 cycles, and the
-- output has 2 additional cycles of latency from the FloPoCo pipeline.
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
