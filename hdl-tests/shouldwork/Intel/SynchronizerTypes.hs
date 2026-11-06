module SynchronizerTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Cdc

import IntelTestCommon

-- | Number of cycles per test bench
nCycles :: Int
nCycles = 300

-- | Stimuli: reset and input
type Stimuli = (Bool, BitVector 4)

genStimuli :: Int -> [Stimuli]
genStimuli = evalG $ do
  ss <- listOf nCycles ((,) <$> chance 4 <*> genBV)
  -- Start in reset
  pure ([(True, 0) | _ <- [1 .. 3 :: Int]] <> ss)

{- | Undefined in the first cycle of a reset. Intel's VHDL model of the
synchronizer resets synchronously, its Verilog model asynchronously.
-}
maskResetStart :: (NFDataX a) => Clock System -> Signal System Bool -> Signal System a -> Signal System a
maskResetStart clk rst = mux (rst .&&. fmap not rstPrev) (pure (deepErrorX "reset starts"))
 where
  rstPrev = register clk noReset enableGen False rst

-- | A 'stdSynchronizer' for every bit of the input
dutSingle ::
  (KnownNat depth, 2 <= depth) =>
  SNat depth ->
  Clock System ->
  Signal System Stimuli ->
  Signal System (BitVector 4)
dutSingle depth clk stim = maskResetStart clk rstB (pack <$> bundle outs)
 where
  (rstB, din) = unbundle stim
  rst = unsafeFromActiveHigh rstB
  outs :: Vec 4 (Signal System Bit)
  outs = map (\i -> stdSynchronizer depth clk rst ((! i) <$> din)) indicesI

-- | A 'stdSynchronizerBundle'
dutBundle ::
  (KnownNat depth, 2 <= depth) =>
  SNat depth ->
  Clock System ->
  Signal System Stimuli ->
  Signal System (BitVector 4)
dutBundle depth clk stim =
  maskResetStart clk rstB (stdSynchronizerBundle depth clk (unsafeFromActiveHigh rstB) din)
 where
  (rstB, din) = unbundle stim
