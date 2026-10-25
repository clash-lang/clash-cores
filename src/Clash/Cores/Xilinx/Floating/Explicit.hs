{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
                  2022,      Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://docs.amd.com/v/u/en-US/pg060-floating-point)
in single precision ('Float'), with explicit clock and enable arguments.

"Clash.Cores.Xilinx.Floating" has the same functions with a hidden clock and
enable. See there for how the functions instantiate the IP, how they behave and
the maximum latency of each operation.

Related modules:

* "Clash.Cores.Xilinx.Floating.Half.Explicit" and
  "Clash.Cores.Xilinx.Floating.Double.Explicit" are the same for half and
  double precision.

* "Clash.Cores.Xilinx.Floating.Generic.Explicit" has the same functions,
  generic in the precision.
-}

module Clash.Cores.Xilinx.Floating.Explicit
  ( -- * Instantiating IP
    addWith
  , add
  , AddDefDelay
  , subWith
  , sub
  , SubDefDelay
  , mulWith
  , mul
  , MulDefDelay
  , divWith
  , div
  , DivDefDelay
  , Ordering(..)
  , toMaybeOrdering
  , compare
  , compareWith
  , CompareDefDelay
  , fromU32With
  , fromU32
  , FromU32DefDelay
  , fromS32With
  , fromS32
  , FromS32DefDelay
  , sqrtWith
  , sqrt
  , SqrtDefDelay
  , abs
  , fromU64With
  , fromU64
  , FromU64DefDelay
  , fromS64With
  , fromS64
  , FromS64DefDelay
  , toS32With
  , toS32
  , ToS32DefDelay
  , convertWith
  , convert
  , ConvertDefDelay
  , toHalfWith
  , toHalf
  , ToHalfDefDelay
  , toDoubleWith
  , toDouble
  , ToDoubleDefDelay
    -- * Customizing IP
  , Config(..)
  , defConfig
  , ArchOpt(..)
  , DspUsage(..)
  , BMemUsage(..)
    -- * Additional functions
  , xilinxNaN
  ) where

import Clash.Explicit.Prelude hiding
  (Ordering(..), add, sub, mul, div, compare, sqrt, abs)

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric.Half (Half)

import Clash.Cores.Xilinx.Floating.Generic.Explicit (defConfig)
import qualified Clash.Cores.Xilinx.Floating.Generic.Explicit as G
import Clash.Cores.Xilinx.Floating.Internal
  ( ArchOpt(..), BMemUsage(..), Config(..), DspUsage(..), Ordering(..)
  , XilinxFloat, toMaybeOrdering, xilinxNaN )

-- | Customizable floating point addition.
addWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addWith = withFrozenCallStack G.addWith
{-# INLINE addWith #-}

-- | Floating point addition with default settings.
add
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + AddDefDelay) Float
add = withFrozenCallStack G.add
{-# INLINE add #-}

-- | The default delay for floating point addition with default customization.
type AddDefDelay = G.AddDefDelay Float

-- | Customizable floating point subtraction.
subWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
subWith = withFrozenCallStack G.subWith
{-# INLINE subWith #-}

-- | Floating point subtraction with default settings.
sub
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + SubDefDelay) Float
sub = withFrozenCallStack G.sub
{-# INLINE sub #-}

-- | The default delay for floating point subtraction with default
-- customization.
type SubDefDelay = G.SubDefDelay Float

-- | Customizable floating point multiplication.
mulWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulWith = withFrozenCallStack G.mulWith
{-# INLINE mulWith #-}

-- | Floating point multiplication with default settings.
mul
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + MulDefDelay) Float
mul = withFrozenCallStack G.mul
{-# INLINE mul #-}

-- | The default delay for floating point multiplication with default
-- customization.
type MulDefDelay = G.MulDefDelay Float

-- | Customizable floating point division.
divWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divWith = withFrozenCallStack G.divWith
{-# INLINE divWith #-}

-- | Floating point division with default settings.
div
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + DivDefDelay) Float
div = withFrozenCallStack G.div
{-# INLINE div #-}

-- | The default delay for floating point division with default customization.
type DivDefDelay = G.DivDefDelay Float

-- | Customizable conversion of @Unsigned 32@ to @Float@
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU32With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Unsigned 32)
  -> DSignal dom (n + d) Float
fromU32With = withFrozenCallStack G.fromU32With
{-# INLINE fromU32With #-}

-- | Conversion of @Unsigned 32@ to @Float@, with default delay
fromU32
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Unsigned 32)
  -> DSignal dom (n + FromU32DefDelay) Float
fromU32 = withFrozenCallStack G.fromU32
{-# INLINE fromU32 #-}

-- | The default delay for conversion of @Unsigned 32@ to @Float@
type FromU32DefDelay = G.FromU32DefDelay Float

-- | Customizable conversion of @Signed 32@ to @Float@
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS32With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Signed 32)
  -> DSignal dom (n + d) Float
fromS32With = withFrozenCallStack G.fromS32With
{-# INLINE fromS32With #-}

-- | Conversion of @Signed 32@ to @Float@, with default delay
fromS32
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Signed 32)
  -> DSignal dom (n + FromS32DefDelay) Float
fromS32 = withFrozenCallStack G.fromS32
{-# INLINE fromS32 #-}

-- | The default delay for conversion of @Signed 32@ to @Float@
type FromS32DefDelay = G.FromS32DefDelay Float

-- | Customizable floating point comparison
--
-- Produces 'Clash.Cores.Xilinx.Floating.Explicit.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
compareWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Ordering
compareWith = withFrozenCallStack G.compareWith
{-# INLINE compareWith #-}

-- | Floating point comparison, with default delay
--
-- Produces 'Clash.Cores.Xilinx.Floating.Explicit.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
compare
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + CompareDefDelay) Ordering
compare = withFrozenCallStack G.compare
{-# INLINE compare #-}

-- | The default delay for @Float@ comparison
type CompareDefDelay = G.CompareDefDelay Float

-- | Customizable floating point square root.
--
-- The square root of a negative zero is a negative zero. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the square root of a negative subnormal number is a negative zero too. The
-- square root of any other negative number, including negative infinity, is
-- NaN.
--
-- The t'Config' makes no difference in single precision: only the square root
-- in half precision uses it (@bMemUsage@).
sqrtWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
sqrtWith = withFrozenCallStack G.sqrtWith
{-# INLINE sqrtWith #-}

-- | Floating point square root with default settings.
--
-- The square root of a negative zero is a negative zero. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the square root of a negative subnormal number is a negative zero too. The
-- square root of any other negative number, including negative infinity, is
-- NaN.
sqrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + SqrtDefDelay) Float
sqrt = withFrozenCallStack G.sqrt
{-# INLINE sqrt #-}

-- | The default delay for floating point square root with default
-- customization.
type SqrtDefDelay = G.SqrtDefDelay Float

-- | Floating point absolute value.
--
-- The IP only clears the sign bit. Unlike the other operations, it leaves
-- subnormal numbers and the payloads of NaNs alone.
--
-- The IP only supports a latency of zero, so the result has the same delay as
-- the input. The IP is then combinational, without a clock or clock enable,
-- so this function does not take them either.
abs
  :: forall dom n
   . KnownDomain dom
  => DSignal dom n Float
  -> DSignal dom n Float
abs = G.abs
{-# INLINE abs #-}

-- | Customizable conversion of @Unsigned 64@ to @Float@. It rounds to
-- nearest, ties to even.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU64With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Unsigned 64)
  -> DSignal dom (n + d) Float
fromU64With = withFrozenCallStack G.fromU64With
{-# INLINE fromU64With #-}

-- | Conversion of @Unsigned 64@ to @Float@, with default delay. It rounds to
-- nearest, ties to even.
fromU64
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Unsigned 64)
  -> DSignal dom (n + FromU64DefDelay) Float
fromU64 = withFrozenCallStack G.fromU64
{-# INLINE fromU64 #-}

-- | The default delay for conversion of @Unsigned 64@ to @Float@
type FromU64DefDelay = G.FromU64DefDelay Float

-- | Customizable conversion of @Signed 64@ to @Float@. It rounds to
-- nearest, ties to even.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS64With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Signed 64)
  -> DSignal dom (n + d) Float
fromS64With = withFrozenCallStack G.fromS64With
{-# INLINE fromS64With #-}

-- | Conversion of @Signed 64@ to @Float@, with default delay. It rounds to
-- nearest, ties to even.
fromS64
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Signed 64)
  -> DSignal dom (n + FromS64DefDelay) Float
fromS64 = withFrozenCallStack G.fromS64
{-# INLINE fromS64 #-}

-- | The default delay for conversion of @Signed 64@ to @Float@
type FromS64DefDelay = G.FromS64DefDelay Float

-- | Customizable conversion of @Float@ to @Signed 32@.
--
-- It rounds to nearest, ties to even, so @2.5@ becomes @2@ and @3.5@ becomes
-- @4@. Numbers that are out of range after rounding, including infinities,
-- saturate to 'minBound' or 'maxBound'. Every NaN becomes 'minBound'.
-- Subnormal numbers become zero.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toS32With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) (Signed 32)
toS32With = withFrozenCallStack G.toS32With
{-# INLINE toS32With #-}

-- | Conversion of @Float@ to @Signed 32@, with default delay.
--
-- It rounds to nearest, ties to even, so @2.5@ becomes @2@ and @3.5@ becomes
-- @4@. Numbers that are out of range after rounding, including infinities,
-- saturate to 'minBound' or 'maxBound'. Every NaN becomes 'minBound'.
-- Subnormal numbers become zero.
toS32
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + ToS32DefDelay) (Signed 32)
toS32 = withFrozenCallStack G.toS32
{-# INLINE toS32 #-}

-- | The default delay for conversion of @Float@ to @Signed 32@
type ToS32DefDelay = G.ToS32DefDelay Float

-- | Customizable conversion of @Float@ to precision @b@, which can be @Float@
-- itself.
--
-- Conversions to a narrower precision round to nearest, ties to even. Results
-- below the smallest normal number of the target precision become a zero of
-- the same sign, results beyond its largest finite number an infinity of the
-- same sign. Other conversions are exact. Like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates, also when converting to the same precision.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
convertWith
  :: forall b d dom n
   . ( XilinxFloat b
     , KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) b
convertWith = withFrozenCallStack G.convertWith
{-# INLINE convertWith #-}

-- | Conversion of @Float@ to precision @b@, which can be @Float@ itself, with
-- default delay.
--
-- Conversions to a narrower precision round to nearest, ties to even. Results
-- below the smallest normal number of the target precision become a zero of
-- the same sign, results beyond its largest finite number an infinity of the
-- same sign. Other conversions are exact. Like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates, also when converting to the same precision.
convert
  :: forall b dom n
   . ( XilinxFloat b
     , KnownNat (ConvertDefDelay b)
     , KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + ConvertDefDelay b) b
convert = withFrozenCallStack G.convert
{-# INLINE convert #-}

-- | The default delay for conversion of @Float@ to precision @b@
type ConvertDefDelay b = G.ConvertDefDelay Float b

-- | Customizable conversion of @Float@ to @Half@.
--
-- It rounds to nearest, ties to even. Results below the smallest normal
-- number of the target precision become a zero of the same sign, results
-- beyond its largest finite number an infinity of the same sign. Like the
-- other operations, the IP treats subnormal inputs as a zero of the same
-- sign and turns every NaN into the NaN it generates.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toHalfWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Half
toHalfWith = withFrozenCallStack G.convertWith
{-# INLINE toHalfWith #-}

-- | Conversion of @Float@ to @Half@, with default delay.
--
-- It rounds to nearest, ties to even. Results below the smallest normal
-- number of the target precision become a zero of the same sign, results
-- beyond its largest finite number an infinity of the same sign. Like the
-- other operations, the IP treats subnormal inputs as a zero of the same
-- sign and turns every NaN into the NaN it generates.
toHalf
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + ToHalfDefDelay) Half
toHalf = withFrozenCallStack G.convert
{-# INLINE toHalf #-}

-- | The default delay for conversion of @Float@ to @Half@
type ToHalfDefDelay = G.ConvertDefDelay Float Half

-- | Customizable conversion of @Float@ to @Double@.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toDoubleWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Double
toDoubleWith = withFrozenCallStack G.convertWith
{-# INLINE toDoubleWith #-}

-- | Conversion of @Float@ to @Double@, with default delay.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
toDouble
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + ToDoubleDefDelay) Double
toDouble = withFrozenCallStack G.convert
{-# INLINE toDouble #-}

-- | The default delay for conversion of @Float@ to @Double@
type ToDoubleDefDelay = G.ConvertDefDelay Float Double
