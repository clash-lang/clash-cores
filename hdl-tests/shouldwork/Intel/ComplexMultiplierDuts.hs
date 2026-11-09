{- | Functions under test, stimuli, and the configurations of the reference
netlists for "ComplexMultiplier". They are defined here, as the Template
Haskell in "ComplexMultiplier" uses them.
-}
module ComplexMultiplierDuts where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Data.Complex (Complex (..))
import Data.Word (Word64)

import qualified Clash.Signal.Delayed as D

import Clash.Cores.Intel.ComplexMultiplier

import ComplexMultiplierSim (RefConfig (..))
import IpTestCommon

-- | Components of operand A (real, imaginary) and operand B, as bit vectors
type Operands a b = (BitVector a, BitVector a, BitVector b, BitVector b)

-- | Operands: all combinations of interesting bit patterns (zero, one, all
-- ones, the most significant bit only, all but the most significant bit),
-- followed by @n@ random ones with random magnitudes.
operands ::
  forall a b.
  (KnownNat a, KnownNat b) =>
  Int ->
  Word64 ->
  [Operands a b]
operands n seed =
  [(w, x, y, z) | w <- specials, x <- specials, y <- specials, z <- specials]
    <> P.take n (go (randoms seed))
 where
  specials :: forall m. KnownNat m => [BitVector m]
  specials = [0, 1, maxBound, 1 `rotateR` 1, maxBound `shiftR` 1]
  go (k : w : x : y : z : rs) =
    (rnd k 0 w, rnd k 8 x, rnd k 16 y, rnd k 24 z) : go rs
  go _ = []
  -- A random number, shifted right by a random amount and negated with
  -- probability one half, so that small and negative numbers are common
  rnd :: forall m. KnownNat m => Word64 -> Int -> Word64 -> BitVector m
  rnd k i v =
    let s = fromIntegral ((k `shiftR` i) `mod` natToNum @(m + 1))
        r = resize (fromIntegral v :: BitVector 64) `shiftR` s
     in if testBit k (i + 7) then negate r else r

-- | Run a complex multiplier on the operands
cmul ::
  forall f g a b d o.
  (BitPack (f a), BitPack (g b), BitSize (f a) ~ a, BitSize (g b) ~ b) =>
  ( Clock Dom ->
    Reset Dom ->
    Enable Dom ->
    DSignal Dom 0 (Complex (f a)) ->
    DSignal Dom 0 (Complex (g b)) ->
    DSignal Dom d o
  ) ->
  Dut (Operands a b) o
cmul f clk rst en i = D.toSignal (f clk rst en (D.fromSignal (opA <$> i)) (D.fromSignal (opB <$> i)))
 where
  opA (ar, ai, _, _) = unpack ar :+ unpack ai
  opB (_, _, br, bi) = unpack br :+ unpack bi

-- | Like 'cmul', with the operands swapped
cmulSwapped ::
  forall f g a b d o.
  (BitPack (f a), BitPack (g b), BitSize (f a) ~ a, BitSize (g b) ~ b) =>
  ( Clock Dom ->
    Reset Dom ->
    Enable Dom ->
    DSignal Dom 0 (Complex (g b)) ->
    DSignal Dom 0 (Complex (f a)) ->
    DSignal Dom d o
  ) ->
  Dut (Operands a b) o
cmulSwapped f clk rst en i = D.toSignal (f clk rst en (D.fromSignal (opB <$> i)) (D.fromSignal (opA <$> i)))
 where
  opA (ar, ai, _, _) = unpack ar :+ unpack ai
  opB (_, _, br, bi) = unpack br :+ unpack bi

-- | Signed 8 x 8 bits, exact: combinational, and with latencies 1 and 4
signedDut :: Dut (Operands 8 8) (Vec 3 (Complex (Signed 17)))
signedDut clk rst en i =
  bundle
    (  cmul @Signed @Signed (complexMultiply @0) clk rst en i
    :> cmul @Signed @Signed (complexMultiplyWith @1 Conventional) clk rst en i
    :> cmul @Signed @Signed (complexMultiply @4) clk rst en i
    :> Nil )
{-# OPAQUE signedDut #-}

-- | Signed 12 x 10 bits, wrapped around to 10 bits, with latencies 2 and 11
signedNarrowDut :: Dut (Operands 12 10) (Vec 2 (Complex (Signed 10)))
signedNarrowDut clk rst en i =
  bundle
    (  cmul @Signed @Signed (complexMultiply @2) clk rst en i
    :> cmul @Signed @Signed (complexMultiplyWith @11 Canonical) clk rst en i
    :> Nil )
{-# OPAQUE signedNarrowDut #-}

-- | Signed 36 x 20 bits, using several embedded multipliers per product,
-- sign extended to 60 bits, with latencies 3 and 14
signedWideDut :: Dut (Operands 36 20) (Vec 2 (Complex (Signed 60)))
signedWideDut clk rst en i =
  bundle
    (  cmul @Signed @Signed (complexMultiply @3) clk rst en i
    :> cmul @Signed @Signed (complexMultiply @14) clk rst en i
    :> Nil )
{-# OPAQUE signedWideDut #-}

-- | Unsigned 9 x 7 bits, zero extended to 18 bits
unsignedDut :: Dut (Operands 9 7) (Vec 2 (Complex (Unsigned 18)))
unsignedDut clk rst en i =
  bundle
    (  cmul @Unsigned @Unsigned (complexMultiply @0) clk rst en i
    :> cmul @Unsigned @Unsigned (complexMultiply @5) clk rst en i
    :> Nil )
{-# OPAQUE unsignedDut #-}

-- | Unsigned 9 x 7 bits, truncated to 12 bits
unsignedNarrowDut :: Dut (Operands 9 7) (Vec 1 (Complex (Unsigned 12)))
unsignedNarrowDut clk rst en i =
  bundle (cmul @Unsigned @Unsigned (complexMultiply @2) clk rst en i :> Nil)
{-# OPAQUE unsignedNarrowDut #-}

-- | Signed 10 bits times unsigned 6 bits and vice versa, exact
mixedDut :: Dut (Operands 10 6) (Vec 2 (Complex (Signed 17)))
mixedDut clk rst en i =
  bundle
    (  cmul @Signed @Unsigned (complexMultiply @5) clk rst en i
    :> cmulSwapped @Signed @Unsigned (complexMultiply @1) clk rst en i
    :> Nil )
{-# OPAQUE mixedDut #-}

-- | One bit operands
tinyDut :: Dut (Operands 1 1) (Vec 2 (Complex (Signed 4)))
tinyDut clk rst en i =
  bundle
    (  cmul @Signed @Signed (complexMultiply @1) clk rst en i
    :> cmul @Unsigned @Signed (complexMultiply @0) clk rst en i
    :> Nil )
{-# OPAQUE tinyDut #-}

-- | Configurations of all instances in the functions under test
refConfigs :: [RefConfig]
refConfigs =
  [ cfg 8 8 17 s s 0 auto
  , cfg 8 8 17 s s 1 "CONVENTIONAL"
  , cfg 8 8 17 s s 4 auto
  , cfg 12 10 10 s s 2 auto
  , cfg 12 10 10 s s 11 "CANONICAL"
  , cfg 36 20 60 s s 3 auto
  , cfg 36 20 60 s s 14 auto
  , cfg 9 7 18 u u 0 auto
  , cfg 9 7 18 u u 5 auto
  , cfg 9 7 12 u u 2 auto
  , cfg 10 6 17 s u 5 auto
  , cfg 6 10 17 u s 1 auto
  , cfg 1 1 4 s s 1 auto
  , cfg 1 1 4 u s 0 auto
  ]
 where
  cfg = RefConfig
  s = "SIGNED"
  u = "UNSIGNED"
  auto = "AUTO"
