module ShiftTapsTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Memory

import IntelTestCommon

-- | Stimuli: enable and input
type Stimuli = (Bool, BitVector 8)

genStimuli :: Int -> [Stimuli]
genStimuli = evalG $ listOf 400 ((,) <$> (not <$> chance 20) <*> genBV)

dut ::
  forall nTaps distance dom fam.
  ( KnownDomain dom, KnownNat nTaps, 1 <= nTaps, KnownNat distance, 3 <= distance
  , KnownSymbol fam ) =>
  SSymbol fam ->
  SNat nTaps ->
  SNat distance ->
  Clock dom ->
  Signal dom Stimuli ->
  Signal dom (BitVector 8, Vec nTaps (BitVector 8))
dut fam nTaps distance clk stim =
  bundle (altshiftTaps fam nTaps distance clk (toEnable en) x)
 where
  (en, x) = unbundle stim
