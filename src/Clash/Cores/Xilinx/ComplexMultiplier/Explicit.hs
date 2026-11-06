{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Complex Multiplier LogiCORE IP v6.0](https://docs.amd.com/v/u/en-US/pg104-cmpy),
with explicit clocks and enables. See "Clash.Cores.Xilinx.ComplexMultiplier"
for an overview.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.ComplexMultiplier.Explicit
  ( -- * Instantiating IP
    complexMultiplyWith
  , complexMultiply
  , complexMultiplyRoundWith
  , complexMultiplyRound
  , NaturalWidth
    -- * Latency
  , ComplexMultiplyDefDelay
  , DspTiles
    -- * Customizing IP
  , Config(..)
  , defConfig
  , MultiplierConstruction(..)
  , OptimizeGoal(..)
  ) where

import Clash.Explicit.Prelude

import Data.Complex (Complex)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Xilinx.ComplexMultiplier.Internal

import qualified Clash.Signal.Delayed as D

-- | Customizable complex multiplication with truncation.
--
-- Multiplies a complex number with @a@-bit components by one with @b@-bit
-- components. The exact product has @'NaturalWidth' a b = a + b + 1@ bits per
-- component. The result consists of its @o@ most significant bits: the exact
-- product divided by @2^(a + b + 1 - o)@, rounded towards negative infinity.
-- The result never overflows.
--
-- The latency @d@ can be any number of clock cycles from 1 to 55: the IP
-- pipelines its multipliers less when @d@ is smaller than its fully pipelined
-- latency, and delays its output with shift registers when @d@ is larger.
-- 'complexMultiply' uses 'ComplexMultiplyDefDelay'.
--
-- The component widths @a@ and @b@ should be between 8 and 63 bits, and @o@
-- between 2 and @'NaturalWidth' a b@.
--
-- For example, the full precision product of two complex numbers with 16-bit
-- components, after 6 clock cycles:
--
-- @
-- mul16 ::
--   KnownDomain dom =>
--   Clock dom ->
--   Enable dom ->
--   DSignal dom 0 (Complex (Signed 16)) ->
--   DSignal dom 0 (Complex (Signed 16)) ->
--   DSignal dom 6 (Complex (Signed 33))
-- mul16 = complexMultiplyWith \@6 defConfig
-- @
complexMultiplyWith ::
  forall d a b o dom k.
  ( KnownDomain dom
  , KnownNat a
  , KnownNat b
  , KnownNat o
  , KnownNat d
  , 1 <= d, d <= 55
  , 8 <= a, a <= 63
  , 8 <= b, b <= 63
  , 2 <= o, o <= NaturalWidth a b
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Operand A
  DSignal dom k (Complex (Signed a)) ->
  -- | Operand B
  DSignal dom k (Complex (Signed b)) ->
  -- | Product
  DSignal dom (k + d) (Complex (Signed o))
complexMultiplyWith cfg clk en x y
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (truncateModel <$> x <*> y)
  und = withFrozenCallStack $ errorX "Initial values of complexMultiply undefined"

  synth =
    D.unsafeFromSignal $ unpackComplex <$>
      cmpyInst @d @a @b @o cfg clk en
        (packComplex <$> D.toSignal x)
        (packComplex <$> D.toSignal y)
{-# INLINE complexMultiplyWith #-}

-- | Complex multiplication with truncation, with the default configuration
-- ('defConfig') and latency ('ComplexMultiplyDefDelay'). See
-- 'complexMultiplyWith'.
complexMultiply ::
  forall a b o dom k.
  ( KnownDomain dom
  , KnownNat a
  , KnownNat b
  , KnownNat o
  , KnownNat (ComplexMultiplyDefDelay a b)
  , 1 <= ComplexMultiplyDefDelay a b, ComplexMultiplyDefDelay a b <= 55
  , 8 <= a, a <= 63
  , 8 <= b, b <= 63
  , 2 <= o, o <= NaturalWidth a b
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  -- | Operand A
  DSignal dom k (Complex (Signed a)) ->
  -- | Operand B
  DSignal dom k (Complex (Signed b)) ->
  -- | Product
  DSignal dom (k + ComplexMultiplyDefDelay a b) (Complex (Signed o))
complexMultiply =
  withFrozenCallStack $ complexMultiplyWith @(ComplexMultiplyDefDelay a b) defConfig
{-# INLINE complexMultiply #-}

-- | Customizable complex multiplication with random rounding.
--
-- Like 'complexMultiplyWith', but the result is rounded to the nearest
-- multiple of @2^k@, with @k = a + b + 1 - o@, instead of rounded down. The
-- /round carry/ input (the IP's @ROUND_CY@) decides how to round a product
-- that lies exactly halfway between two results: down if it is 0, up if it
-- is 1. The IP adds @2^(k-1) - 1@ plus the round carry to the exact product,
-- and then truncates it. A random round carry, or one that is uncorrelated with
-- the products, gives unbiased rounding; see /Rounding/ in PG104 for
-- suggestions. Rounding never overflows.
--
-- The output must be narrower than the natural width: @o < 'NaturalWidth' a b@.
-- Otherwise, the same constraints as for 'complexMultiplyWith' apply.
complexMultiplyRoundWith ::
  forall d a b o dom k.
  ( KnownDomain dom
  , KnownNat a
  , KnownNat b
  , KnownNat o
  , KnownNat d
  , 1 <= d, d <= 55
  , 8 <= a, a <= 63
  , 8 <= b, b <= 63
  , 2 <= o, o + 1 <= NaturalWidth a b
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Round carry
  DSignal dom k Bit ->
  -- | Operand A
  DSignal dom k (Complex (Signed a)) ->
  -- | Operand B
  DSignal dom k (Complex (Signed b)) ->
  -- | Product
  DSignal dom (k + d) (Complex (Signed o))
complexMultiplyRoundWith cfg clk en cy x y
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (roundModel <$> cy <*> x <*> y)
  und = withFrozenCallStack $ errorX "Initial values of complexMultiplyRound undefined"

  synth =
    D.unsafeFromSignal $ unpackComplex <$>
      cmpyRoundInst @d @a @b @o cfg clk en
        (D.toSignal cy)
        (packComplex <$> D.toSignal x)
        (packComplex <$> D.toSignal y)
{-# INLINE complexMultiplyRoundWith #-}

-- | Complex multiplication with random rounding, with the default
-- configuration ('defConfig') and latency ('ComplexMultiplyDefDelay'). See
-- 'complexMultiplyRoundWith'.
complexMultiplyRound ::
  forall a b o dom k.
  ( KnownDomain dom
  , KnownNat a
  , KnownNat b
  , KnownNat o
  , KnownNat (ComplexMultiplyDefDelay a b)
  , 1 <= ComplexMultiplyDefDelay a b, ComplexMultiplyDefDelay a b <= 55
  , 8 <= a, a <= 63
  , 8 <= b, b <= 63
  , 2 <= o, o + 1 <= NaturalWidth a b
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  -- | Round carry
  DSignal dom k Bit ->
  -- | Operand A
  DSignal dom k (Complex (Signed a)) ->
  -- | Operand B
  DSignal dom k (Complex (Signed b)) ->
  -- | Product
  DSignal dom (k + ComplexMultiplyDefDelay a b) (Complex (Signed o))
complexMultiplyRound =
  withFrozenCallStack $ complexMultiplyRoundWith @(ComplexMultiplyDefDelay a b) defConfig
{-# INLINE complexMultiplyRound #-}
