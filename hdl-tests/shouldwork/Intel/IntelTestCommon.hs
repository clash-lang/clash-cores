{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Test infrastructure for the Intel primitives and megafunctions. The test
benches compare the Haskell simulation models of the wrappers in
"Clash.Cores.Intel" against Intel's simulation libraries, see "IntelSimLib".

== Stimuli and expected values

The stimuli are random, generated from a fixed seed per test bench. The
expected outputs are computed with the Haskell simulation model at compile
time, and stored in memory blobs along with the stimuli. Bits that are
undefined in the Haskell model are not checked.
-}
module IntelTestCommon where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Annotations.Primitive (HDL (..), Primitive (..), hasBlackBox)
import Clash.Sized.Internal.BitVector (BitVector (BV))

import Data.Bits (shiftL, shiftR, xor)
import Data.String.Interpolate (__i)
import Data.Word (Word64)
import Language.Haskell.TH (ExpQ, tupE)

import IntelSimLib (intelSimLibraries)

import qualified Prelude as P

-- * Intel's simulation libraries

$(intelSimLibraries "alteraMfLib" "alteraLib")

{- | Returns its argument, but makes Intel's simulation libraries part of the
HDL of the test bench using it, see "IntelSimLib". Apply it to the @done@
signal of a test bench.
-}
withIntelSim :: Signal dom Bool -> Signal dom Bool
withIntelSim = fmap (bitToBool . alteraLib . alteraMfLib . boolToBit)

-- * Pull-up resistor

{- | Pull-up resistors on a wire. Clash simulates wires with a v'PullUp'
default, but doesn't add a pull-up in HDL. Neither do the Intel buffers: they
set the @weak_pull_up_resistor@ parameter, which the simulation models ignore.
-}
pullUp :: (KnownNat n) => BiSignalIn 'PullUp dom n -> BiSignalOut 'PullUp dom n
pullUp pad = writeToBiSignal pad (pure (Nothing :: Maybe (BitVector n)))
{-# OPAQUE pullUp #-}
{-# ANN pullUp hasBlackBox #-}
{-# ANN pullUp (InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
  BlackBox:
    name: IntelTestCommon.pullUp
    kind: Declaration
    renderVoid: RenderVoid
    template: |-
      pullup ~GENSYM[pull_up][0] [~SIZE[~TYP[1]]-1:0] (~ARG[1]);
  |]) #-}
{-# ANN pullUp (InlineYamlPrimitive [VHDL] [__i|
  BlackBox:
    name: IntelTestCommon.pullUp
    kind: Declaration
    renderVoid: RenderVoid
    template: |-
      ~ARG[1] <= (others => 'H');
  |]) #-}

{- | Converts the weak levels of a VHDL @std_logic@ (@\'H\'@, @\'L\'@) to
strong ones. The VHDL models of Intel's buffers pass the level of a pad
through, so a released pad with a pull-up reads @\'H\'@, which doesn't compare
equal to @\'1\'@. Verilog simulators do this conversion themselves.
-}
strengthen :: Signal dom Bit -> Signal dom Bit
strengthen = id
{-# OPAQUE strengthen #-}
{-# ANN strengthen hasBlackBox #-}
{-# ANN strengthen (InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
  BlackBox:
    name: IntelTestCommon.strengthen
    kind: Expression
    template: ~ARG[0]
  |]) #-}
{-# ANN strengthen (InlineYamlPrimitive [VHDL] [__i|
  BlackBox:
    name: IntelTestCommon.strengthen
    kind: Expression
    template: to_X01(~ARG[0])
  |]) #-}

-- | 'strengthen' for a 'BitVector'
strengthenBV :: Signal dom (BitVector n) -> Signal dom (BitVector n)
strengthenBV = id
{-# OPAQUE strengthenBV #-}
{-# ANN strengthenBV hasBlackBox #-}
{-# ANN strengthenBV (InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
  BlackBox:
    name: IntelTestCommon.strengthenBV
    kind: Expression
    template: ~ARG[0]
  |]) #-}
{-# ANN strengthenBV (InlineYamlPrimitive [VHDL] [__i|
  BlackBox:
    name: IntelTestCommon.strengthenBV
    kind: Expression
    template: to_X01(~ARG[0])
  |]) #-}

-- * Random generation

-- | Minimal state monad threading a (xorshift64*) random generator
newtype G a = G {runG :: Word64 -> (a, Word64)}

instance Functor G where
  fmap f (G g) = G (\s -> let (a, s1) = g s in (f a, s1))

instance Applicative G where
  pure a = G (\s -> (a, s))
  G f <*> G a = G (\s -> let (f1, s1) = f s; (a1, s2) = a s1 in (f1 a1, s2))

instance Monad G where
  G a >>= f = G (\s -> let (a1, s1) = a s in runG (f a1) s1)

evalG :: G a -> Int -> a
evalG g seed = fst (runG g (0x9E3779B97F4A7C15 * (P.fromIntegral seed + 1)))

-- | Random 64-bit word
genWord :: G Word64
genWord = G step
 where
  step s0 = (s3 * 0x2545F4914F6CDD1D, s3)
   where
    s1 = s0 `xor` (s0 `shiftR` 12)
    s2 = s1 `xor` (s1 `shiftL` 25)
    s3 = s2 `xor` (s2 `shiftR` 27)

-- | Uniformly random integer in the given (inclusive) range
uniformInteger :: Integer -> Integer -> G Integer
uniformInteger lo hi = do
  ws <- sequence (P.replicate 4 genWord)
  let r = P.foldl (\acc w -> acc * 2 P.^ (64 :: Int) + toInteger w) 0 ws
  pure (lo + r `mod` (hi - lo + 1))

genBV :: forall n. (KnownNat n) => G (BitVector n)
genBV = fromInteger <$> uniformInteger 0 (2 P.^ natToNum @n @Integer - 1)

genUnsigned :: forall n. (KnownNat n) => G (Unsigned n)
genUnsigned = unpack <$> genBV

genBit :: G Bit
genBit = unpack <$> genBV

genBool :: G Bool
genBool = bitToBool <$> genBit

-- | 'True' with the given probability (in percent)
chance :: Integer -> G Bool
chance p = (< p) <$> uniformInteger 0 99

elements :: [a] -> G a
elements xs = (xs P.!!) . P.fromInteger <$> uniformInteger 0 (toInteger (P.length xs) - 1)

-- | A list of random values
listOf :: Int -> G a -> G [a]
listOf n g = sequence (P.replicate n g)

-- * Expected outputs

-- | Expected values along with a mask of the bits to check. Undefined bits are
-- not checked.
withMask :: (BitPack a) => a -> (BitVector (BitSize a), BitVector (BitSize a))
withMask o = case isX (pack o) of
  Left _ -> (0, 0)
  Right (BV m v) ->
    let care = complement (P.fromIntegral m) in (P.fromIntegral v .&. care, care)

-- | Pack a stimulus, replacing undefined bits (such as the field of a
-- 'Nothing') by zeroes.
definedBits :: (BitPack a) => a -> BitVector (BitSize a)
definedBits a = case pack a of
  BV m v -> P.fromIntegral v .&. complement (P.fromIntegral m)

-- | Simulate a circuit with the Haskell model, and return the expected values
-- along with a mask of the bits to check.
expected ::
  forall dom i o.
  (KnownDomain dom, BitPack i, BitPack o, NFDataX i) =>
  (Clock dom -> Signal dom i -> Signal dom o) ->
  [i] ->
  [(BitVector (BitSize o), BitVector (BitSize o))]
expected circuit is =
  P.map withMask (sampleN_lazy (P.length is) (circuit clockGen (fromList is)))

-- | Memory blobs with the stimuli, expected values, and masks of the bits to
-- check, for 'tb'
blobs ::
  forall dom i o.
  (KnownDomain dom, BitPack i, BitPack o, NFDataX i) =>
  (Clock dom -> Signal dom i -> Signal dom o) ->
  [i] ->
  ExpQ
blobs circuit is =
  tupE
    [ memBlobTH Nothing (P.map definedBits is)
    , memBlobTH Nothing (P.map fst es)
    , memBlobTH Nothing (P.map snd es)
    ]
 where
  es = expected circuit is

-- * Test benches

{- | Test bench for a circuit in a single domain. Plays the stimuli and
compares the outputs to the expected values, for every bit in the mask.
-}
tb ::
  forall dom n i o.
  (KnownDomain dom, KnownNat n, 1 <= n, BitPack i, BitPack o) =>
  (Clock dom -> Signal dom i -> Signal dom o) ->
  ( MemBlob n (BitSize i)
  , MemBlob n (BitSize o)
  , MemBlob n (BitSize o)
  ) ->
  Signal dom Bool
tb circuit (stimBlob, expectedBlob, maskBlob) = withIntelSim done
 where
  clk = tbClockGen (not <$> done)

  cnt :: Signal dom (Index n)
  cnt = register clk noReset enableGen 0 (satSucc SatBound <$> cnt)

  stim = unpack . asyncRomBlob stimBlob <$> cnt
  actual = pack <$> circuit clk stim
  expectedVal = asyncRomBlob expectedBlob <$> cnt
  mask = asyncRomBlob maskBlob <$> cnt

  done =
    assert
      clk
      noReset
      "outputs"
      ((.&.) <$> actual <*> mask)
      ((.&.) <$> expectedVal <*> mask)
      ((== maxBound) <$> cnt)
{-# INLINE tb #-}
