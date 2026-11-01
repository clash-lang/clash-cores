{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module UnisimTestCommon where

import Clash.Explicit.Prelude
import Language.Haskell.TH (runIO)
import System.Random

-- | Seed for the random test data, picked when this module is compiled. Test
-- benches and the Template Haskell splices generating their expected outputs
-- use the same seed, as they import it from this module.
randomSeed :: Int
randomSeed = $(runIO (randomIO @Int) >>= lift)

-- | Random test data, a new value every clock cycle
genTestData ::
  forall dom a.
  (KnownDomain dom, BitPack a, BitSize a <= 64) =>
  Int ->
  Clock dom ->
  Signal dom a
genTestData seed clk = (unpack . truncateToSize . pack) <$> out
 where
  (out, gen) = unbundle $ genWord64 <$> delay clk enableGen (mkStdGen seed) gen
  truncateToSize :: BitVector 64 -> BitVector (BitSize a)
  truncateToSize = leToPlus @(BitSize a) @64 truncateB

-- | Number of clock cycles since the start of the simulation, saturating at
-- its maximum
cycleCount :: (KnownDomain dom, KnownNat n) => Clock dom -> Signal dom (Index n)
cycleCount clk = cnt
 where
  cnt = delay clk enableGen 0 (satSucc SatBound <$> cnt)

-- dummy implementation
instance NFDataX StdGen where
  deepErrorX = errorX
  hasUndefined = const False
  ensureSpine = id
  rnfX = const ()
