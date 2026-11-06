{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Complex Multiplier LogiCORE IP v6.0](https://docs.amd.com/v/u/en-US/pg104-cmpy).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @cmpy@/.../@.clash.tcl@ which needs to be executed in the Vivado project
to create the proper entity. Simulation in Clash produces bit-identical
results to the IP, cycle for cycle.

The IP multiplies two complex numbers, given as 'Complex' numbers of 'Signed'
integers:

@
(ar :+ ai) * (br :+ bi) = (ar * br - ai * bi) :+ (ar * bi + ai * br)
@

Operand A has components of @a@ bits, operand B components of @b@ bits. The
exact product has components of @'E.NaturalWidth' a b = a + b + 1@ bits. The
output has @o@ bits per component: the @o@ most significant bits of the exact
product. The IP either truncates the discarded bits ('complexMultiply') or
rounds them away ('complexMultiplyRound'), see /Rounding/ in PG104. The result
never overflows. Fixed-point operands work the same way: the product of numbers
with @fa@ and @fb@ fractional bits has @fa + fb@ fractional bits, of which the
output keeps @fa + fb - (a + b + 1 - o)@.

All IP instantiated by this module has the following properties:

* Non-blocking AXI4-Stream interfaces. @TVALID@ on the inputs is always
  asserted, @TVALID@ on the output is ignored.
* One product per clock cycle, after a fixed latency
* A clock enable, no reset input
* No @TLAST@, no @TUSER@

The latency of the IP is set through the delay argument of the 'DSignal'. It
can be any number of clock cycles from 1 to 55. Other customization is done
through the t'E.Config' argument of the customizable functions; it does not
affect the results. For each operation, there also exists a function that uses
the defaults: the settings from 'E.defConfig' and the latency
'E.ComplexMultiplyDefDelay', which fully pipelines the IP.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

For example, the full precision product of two complex numbers with 16-bit
components, with the default latency:

@
mul16 ::
  (HiddenClock dom, HiddenEnable dom) =>
  DSignal dom 0 (Complex (Signed 16)) ->
  DSignal dom 0 (Complex (Signed 16)) ->
  DSignal dom (ComplexMultiplyDefDelay 16 16) (Complex (Signed 33))
mul16 = complexMultiply
@

Not supported:

* Blocking mode, back pressure (@TREADY@), reset
* Passing @TLAST@ and @TUSER@ from the inputs to the output. Delay them with
  'Clash.Signal.Delayed.delayedI', using the same 'Enable', instead: in
  non-blocking mode, that is exactly what the IP does.
* A latency of zero: the IP then has no clock input
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.ComplexMultiplier
  ( -- * Instantiating IP
    complexMultiplyWith
  , complexMultiply
  , complexMultiplyRoundWith
  , complexMultiplyRound
  , E.NaturalWidth
    -- * Latency
  , E.ComplexMultiplyDefDelay
  , E.DspTiles
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.MultiplierConstruction(..)
  , E.OptimizeGoal(..)
  ) where

import Clash.Prelude

import Data.Complex (Complex)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Xilinx.ComplexMultiplier.Explicit as E
import Clash.Cores.Xilinx.ComplexMultiplier.Explicit (NaturalWidth, ComplexMultiplyDefDelay)

-- | Customizable complex multiplication with truncation. See
-- 'E.complexMultiplyWith'.
complexMultiplyWith ::
  forall d a b o dom k.
  ( HiddenClock dom
  , HiddenEnable dom
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
  E.Config ->
  -- | Operand A
  DSignal dom k (Complex (Signed a)) ->
  -- | Operand B
  DSignal dom k (Complex (Signed b)) ->
  -- | Product
  DSignal dom (k + d) (Complex (Signed o))
complexMultiplyWith cfg =
  withFrozenCallStack $ hideEnable $ hideClock $ E.complexMultiplyWith @d cfg
{-# INLINE complexMultiplyWith #-}

-- | Complex multiplication with truncation, with the default configuration
-- and latency. See 'E.complexMultiplyWith'.
complexMultiply ::
  forall a b o dom k.
  ( HiddenClock dom
  , HiddenEnable dom
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
  -- | Operand A
  DSignal dom k (Complex (Signed a)) ->
  -- | Operand B
  DSignal dom k (Complex (Signed b)) ->
  -- | Product
  DSignal dom (k + ComplexMultiplyDefDelay a b) (Complex (Signed o))
complexMultiply = withFrozenCallStack $ hideEnable $ hideClock E.complexMultiply
{-# INLINE complexMultiply #-}

-- | Customizable complex multiplication with random rounding. See
-- 'E.complexMultiplyRoundWith'.
complexMultiplyRoundWith ::
  forall d a b o dom k.
  ( HiddenClock dom
  , HiddenEnable dom
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
  E.Config ->
  -- | Round carry
  DSignal dom k Bit ->
  -- | Operand A
  DSignal dom k (Complex (Signed a)) ->
  -- | Operand B
  DSignal dom k (Complex (Signed b)) ->
  -- | Product
  DSignal dom (k + d) (Complex (Signed o))
complexMultiplyRoundWith cfg =
  withFrozenCallStack $ hideEnable $ hideClock $ E.complexMultiplyRoundWith @d cfg
{-# INLINE complexMultiplyRoundWith #-}

-- | Complex multiplication with random rounding, with the default
-- configuration and latency. See 'E.complexMultiplyRoundWith'.
complexMultiplyRound ::
  forall a b o dom k.
  ( HiddenClock dom
  , HiddenEnable dom
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
  -- | Round carry
  DSignal dom k Bit ->
  -- | Operand A
  DSignal dom k (Complex (Signed a)) ->
  -- | Operand B
  DSignal dom k (Complex (Signed b)) ->
  -- | Product
  DSignal dom (k + ComplexMultiplyDefDelay a b) (Complex (Signed o))
complexMultiplyRound = withFrozenCallStack $ hideEnable $ hideClock E.complexMultiplyRound
{-# INLINE complexMultiplyRound #-}
