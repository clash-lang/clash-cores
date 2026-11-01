{-# LANGUAGE NumericUnderscores #-}

-- | Test infrastructure shared by the @DSP48E1@ and @DSP48E2@ test benches.
-- The stimuli are random, generated from a fixed seed per test bench. The
-- expected outputs are computed with the Haskell simulation model; the test
-- bench compares them to the outputs of the UNISIM model.
module Dsp48TestCommon where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (BitVector(BV))

import Language.Haskell.TH (ExpQ, tupE)
import System.Random (StdGen, mkStdGen, randomR)

import qualified Prelude as P

-- | Number of cycles in each test bench
type Cycles = 1000

-- | During the first cycles, all resets are asserted, and the outputs are not
-- checked. This covers the global set/reset (GSR) pulse of the Verilog UNISIM
-- models (100 ns), which the VHDL models don't have.
warmup :: Int
warmup = 16

-- * Random generation

-- | Minimal state monad threading a random generator
newtype G a = G { runG :: StdGen -> (a, StdGen) }

instance Functor G where
  fmap f (G g) = G (\s -> let (a, s1) = g s in (f a, s1))

instance Applicative G where
  pure a = G (\s -> (a, s))
  G f <*> G a = G (\s -> let (f1, s1) = f s; (a1, s2) = a s1 in (f1 a1, s2))

instance Monad G where
  G a >>= f = G (\s -> let (a1, s1) = a s in runG (f a1) s1)

evalG :: G a -> Int -> a
evalG g seed = fst (runG g (mkStdGen seed))

uniformInteger :: Integer -> Integer -> G Integer
uniformInteger lo hi = G (randomR (lo, hi))

genBV :: forall n. KnownNat n => G (BitVector n)
genBV = fromInteger <$> uniformInteger 0 (2 P.^ natToNum @n @Integer - 1)

genBit :: G Bit
genBit = unpack <$> genBV

-- | 'True' with the given probability (in percent)
chance :: Integer -> G Bool
chance p = (< p) <$> uniformInteger 0 99

elements :: [a] -> G a
elements xs = (xs P.!!) . fromInteger <$> uniformInteger 0 (toInteger (P.length xs) - 1)

-- | How to generate the C input. The pattern detector rarely matches random
-- data, so the pattern detector tests use C values that make matches likely.
data CGen
  = CRandom
  -- ^ Uniformly random
  | CSmall
  -- ^ Mostly values below 8. When used as pattern with a rounding mask, only a
  -- few bits are compared.
  | CMostlyOnes
  -- ^ Mostly values with all but the three least significant bits set. When
  -- used as mask, only a few bits are compared.
  | CSmallOdd
  -- ^ Like 'CSmall', but mostly odd. The VHDL and Verilog @DSP48E1@ UNISIM
  -- models disagree on the rounding masks for even C values.

genC :: CGen -> G (BitVector 48)
genC CRandom = genBV
genC CSmall = do
  small <- chance 80
  if small then zeroExtend <$> genBV @3 else genBV
genC CMostlyOnes = complement <$> genC CSmall
genC CSmallOdd = do
  c <- genC CSmall
  odd' <- chance 90
  pure (if odd' then c .|. 1 else c)

-- * Circuits under test

type Dom = XilinxSystem

-- * Expected outputs

-- | Simulate a circuit with the Haskell model, and return the expected values
-- along with a mask of the bits to check. Undefined bits, and all bits during
-- the warmup, are not checked.
expected ::
  (BitPack i, BitPack o, NFDataX i) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  [i] ->
  [(BitVector (BitSize o), BitVector (BitSize o))]
expected circuit is = P.zipWith check [0 ..] outs
 where
  outs = sampleN_lazy (P.length is) (circuit clockGen (fromList is))
  check n o
    | n < warmup = (0, 0)
    | otherwise = case isX (pack o) of
        Left _ -> (0, 0)
        Right (BV m v) ->
          let care = complement (fromIntegral m) in (fromIntegral v .&. care, care)

expectedValues ::
  (BitPack i, BitPack o, NFDataX i) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  [i] ->
  [BitVector (BitSize o)]
expectedValues circuit = P.map fst . expected circuit

expectedMasks ::
  (BitPack i, BitPack o, NFDataX i) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  [i] ->
  [BitVector (BitSize o)]
expectedMasks circuit = P.map snd . expected circuit

-- | Memory blobs with the stimuli, expected values, and masks of the bits to
-- check, for 'tb'
blobs ::
  (BitPack i, BitPack o, NFDataX i) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  [i] ->
  ExpQ
blobs circuit is =
  tupE
    [ memBlobTH Nothing is
    , memBlobTH Nothing (expectedValues circuit is)
    , memBlobTH Nothing (expectedMasks circuit is)
    ]

-- * Test bench

tb ::
  forall i o.
  (BitPack i, BitPack o) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  ( MemBlob Cycles (BitSize i)
  , MemBlob Cycles (BitSize o)
  , MemBlob Cycles (BitSize o) ) ->
  Signal Dom Bool
tb circuit (stimBlob, expectedBlob, maskBlob) = done
 where
  clk = tbClockGen (not <$> done)

  cnt :: Signal Dom (Index Cycles)
  cnt = register clk noReset enableGen 0 (satSucc SatBound <$> cnt)

  stim = unpack . asyncRomBlob stimBlob <$> cnt
  actual = pack <$> circuit clk stim
  expectedVal = asyncRomBlob expectedBlob <$> cnt
  mask = asyncRomBlob maskBlob <$> cnt

  done =
    assert clk noReset "DSP48 outputs"
      ((.&.) <$> actual <*> mask)
      ((.&.) <$> expectedVal <*> mask)
      ((== maxBound) <$> cnt)
{-# INLINE tb #-}
