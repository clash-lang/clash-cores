{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
                  2022,      Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://docs.amd.com/v/u/en-US/pg060-floating-point).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @floating_point@/.../@.clash.tcl@ which needs to be executed in the Vivado
project to create the proper entity. Simulation in Clash produces bit-identical
results to synthesis.

Most functions allow customization of the Xilinx IP. Valid combinations will
need to be gleaned from, e.g., the Vivado wizard (IP Catalog -> Floating-point).
All IP instantiated by this module always has the following properties:

* Single precision, apart from the integers and other precisions that
  conversions take or produce
* Non-blocking
* No reset input
* No optional output fields, no @TLAST@, no @TUSER@
* @TVALID@ on the inputs is always asserted, @TVALID@ on the output is ignored.
Note that it would appear the Xilinx IP does not use these signals in
non-blocking mode anyway.
* 1 cycle per operation (meaning there need not be any dummy cycles between
consecutive inputs)

The latency of the IP is set through the delay argument of the 'DSignal'. Other
customization is done through the t'Clash.Cores.Xilinx.Floating.Config'
argument of customizable functions.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

'abs' takes no clock or enable: the IP only supports a latency of zero for it,
so its result has the same delay as its input.

The Xilinx IP does not support calculations with subnormal numbers. All
subnormal numbers are rounded to zero on both input and output. Note that this
introduces a slight bias as the larger subnormal numbers are closer to the
smallest normal number, but they are rounded to zero nonetheless! The IP
rounds a result to 24 significant bits before checking whether it is
subnormal, so some results that IEEE rounding would round up to the smallest
normal number become zero: those still below it after rounding to 24 bits.
Zeros keep their sign: a negative subnormal number becomes a negative zero.
Every NaN becomes the NaN the IP generates,
'Clash.Cores.Xilinx.Floating.xilinxNaN'.

The operations round to nearest, ties to even. Some have further details:

* 'sqrt': the square root of a negative zero is a negative zero, so that of a
  negative subnormal number is too. The square root of any other negative
  number, including negative infinity, is NaN.

* 'abs' only clears the sign bit. Unlike the other operations, it leaves
  subnormal numbers alone, and NaNs keep their payload.

* 'toS32' rounds to nearest, ties to even, and saturates numbers that are out
  of range after rounding, including infinities, to 'minBound' or 'maxBound'.
  Every NaN becomes 'minBound'.

* 'toHalf' and the other conversions to a narrower precision round like the
  arithmetic operations, including the treatment of results below the smallest
  normal number. Numbers beyond the largest finite number of the narrower
  precision become an infinity. Conversions to the same or a wider precision
  are exact, apart from the treatment of subnormal numbers and NaNs, which
  applies to conversions to the same precision too.

For each customizable operation, there also exists a function that uses the
defaults. These functions use the settings from
'Clash.Cores.Xilinx.Floating.defConfig' and the maximum delay for the Xilinx IP
with that configuration. That delay is also defined as a type variable for
delay annotation in circuits.

Related modules:

* "Clash.Cores.Xilinx.Floating.Explicit" has the same functions with explicit
  clock and enable arguments.

* "Clash.Cores.Xilinx.Floating.Half" and "Clash.Cores.Xilinx.Floating.Double"
  are the same for half and double precision.

* "Clash.Cores.Xilinx.Floating.Generic" has the same functions, generic in the
  precision.

= Maximum latency

The latency of an operation can be anything from zero up to a maximum, which
depends on the operation and its configuration. Vivado reports an error when it
generates the IP for a higher latency. A lower latency means fewer pipeline
stages, which can lower the maximum clock frequency.

The maximum latencies in single precision, as reported by Vivado 2022.1:

+-------------------------------------------+---------------------------------------+-----------------+
| Function                                  | Configuration                         | Maximum latency |
+===========================================+=======================================+=================+
| 'addWith', 'subWith'                      | @SpeedArch@, @NoDspUsage@             | 12              |
+-------------------------------------------+---------------------------------------+-----------------+
| 'addWith', 'subWith'                      | @SpeedArch@, @MediumDspUsage@         | 11              |
+-------------------------------------------+---------------------------------------+-----------------+
| 'addWith', 'subWith'                      | @SpeedArch@, @FullDspUsage@ (default) | 11              |
+-------------------------------------------+---------------------------------------+-----------------+
| 'addWith', 'subWith'                      | @SpeedArch@, @MaxDspUsage@            | /rejected/      |
+-------------------------------------------+---------------------------------------+-----------------+
| 'addWith', 'subWith'                      | @LatencyArch@, any @DspUsage@         | 8               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'mulWith'                                 | @NoDspUsage@                          | 8               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'mulWith'                                 | @MediumDspUsage@                      | 8               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'mulWith'                                 | @FullDspUsage@ (default)              | 8               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'mulWith'                                 | @MaxDspUsage@                         | 6               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'divWith'                                 | any                                   | 28              |
+-------------------------------------------+---------------------------------------+-----------------+
| 'compareWith'                             |                                       | 2               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'fromU32With'                             |                                       | 5               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'fromS32With'                             |                                       | 6               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'sqrtWith'                                | any                                   | 28              |
+-------------------------------------------+---------------------------------------+-----------------+
| 'abs'                                     |                                       | 0 (only)        |
+-------------------------------------------+---------------------------------------+-----------------+
| 'fromU64With'                             |                                       | 6               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'fromS64With'                             |                                       | 7               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'toS32With'                               |                                       | 6               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'convertWith', to @Float@                 |                                       | 2               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'toHalfWith', 'convertWith' to @Half@     |                                       | 3               |
+-------------------------------------------+---------------------------------------+-----------------+
| 'toDoubleWith', 'convertWith' to @Double@ |                                       | 2               |
+-------------------------------------------+---------------------------------------+-----------------+

/rejected/ means that Vivado rejects the configuration itself, whatever the
latency. 'mulWith' ignores the @archOpt@ setting. With @LatencyArch@, Vivado
ignores the @dspUsage@ setting of 'addWith' and 'subWith'. 'sqrtWith' only
uses the @bMemUsage@ setting in half precision. The IP only supports a latency
of zero for 'abs'. These maximum latencies are the same on all measured
devices: 7-series (Artix-7, Kintex-7 and Spartan-7), UltraScale (Kintex
UltraScale) and UltraScale+ (Kintex UltraScale+ and Zynq UltraScale+). Other
versions of Vivado might accept other latencies.

When changing the configuration, note that the maximum latency can be lower
than the default delay, for example for 'mulWith' with @MaxDspUsage@.
-}

module Clash.Cores.Xilinx.Floating
  ( -- * Instantiating IP
    addWith
  , add
  , E.AddDefDelay
  , subWith
  , sub
  , E.SubDefDelay
  , mulWith
  , mul
  , E.MulDefDelay
  , divWith
  , div
  , E.DivDefDelay
  , fromU32With
  , fromU32
  , E.FromU32DefDelay
  , E.Ordering(..)
  , E.toMaybeOrdering
  , compare
  , compareWith
  , E.CompareDefDelay
  , fromS32With
  , fromS32
  , E.FromS32DefDelay
  , sqrtWith
  , sqrt
  , E.SqrtDefDelay
  , abs
  , fromU64With
  , fromU64
  , E.FromU64DefDelay
  , fromS64With
  , fromS64
  , E.FromS64DefDelay
  , toS32With
  , toS32
  , E.ToS32DefDelay
  , convertWith
  , convert
  , E.ConvertDefDelay
  , toHalfWith
  , toHalf
  , E.ToHalfDefDelay
  , toDoubleWith
  , toDouble
  , E.ToDoubleDefDelay
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.ArchOpt(..)
  , E.DspUsage(..)
  , E.BMemUsage(..)
    -- * Additional functions
  , E.xilinxNaN
  ) where

import Clash.Prelude hiding
  (Ordering(..), add, sub, mul, div, compare, sqrt, abs)

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric.Half (Half)

import qualified Clash.Cores.Xilinx.Floating.Explicit as E
import Clash.Cores.Xilinx.Floating.Internal (XilinxFloat)

-- | Customizable floating point addition.
addWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.addWith cfg
{-# INLINE addWith #-}

-- | Floating point addition with default settings.
add
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.AddDefDelay) Float
add = withFrozenCallStack $ hideEnable $ hideClock E.add
{-# INLINE add #-}

-- | Customizable floating point subtraction.
subWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
subWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.subWith cfg
{-# INLINE subWith #-}

-- | Floating point subtraction with default settings.
sub
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.SubDefDelay) Float
sub = withFrozenCallStack $ hideEnable $ hideClock E.sub
{-# INLINE sub #-}

-- | Customizable floating point multiplication.
mulWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.mulWith cfg
{-# INLINE mulWith #-}

-- | Floating point multiplication with default settings.
mul
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.MulDefDelay) Float
mul = withFrozenCallStack $ hideEnable $ hideClock E.mul
{-# INLINE mul #-}

-- | Customizable floating point division.
divWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.divWith cfg
{-# INLINE divWith #-}

-- | Floating point division with default settings.
div
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.DivDefDelay) Float
div = withFrozenCallStack $ hideEnable $ hideClock E.div
{-# INLINE div #-}

-- | Customizable floating point comparison
--
-- Produces 'Clash.Cores.Xilinx.Floating.NaN' if any of the inputs is NaN.
-- Otherwise, it behaves like Haskell's 'Prelude.compare'.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
compareWith
  :: forall d dom n
   . ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) E.Ordering
compareWith = E.compareWith hasClock hasEnable
{-# INLINE compareWith #-}

-- | Floating point comparison, with default delay
--
-- Produces 'Clash.Cores.Xilinx.Floating.NaN' if any of the inputs is NaN.
-- Otherwise, it behaves like Haskell's 'Prelude.compare'.
compare
  :: forall dom n
   . ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.CompareDefDelay) E.Ordering
compare = compareWith
{-# INLINE compare #-}

-- | Customizable conversion of @Unsigned 32@ to @Float@
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU32With
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Unsigned 32)
  -> DSignal dom (n + d) Float
fromU32With = withFrozenCallStack $ hideEnable $ hideClock E.fromU32With
{-# INLINE fromU32With #-}

-- | Conversion of @Unsigned 32@ to @Float@, with default delay
fromU32
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Unsigned 32)
  -> DSignal dom (n + E.FromU32DefDelay) Float
fromU32 = withFrozenCallStack $ hideEnable $ hideClock E.fromU32
{-# INLINE fromU32 #-}

-- | Customizable conversion of @Signed 32@ to @Float@
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS32With
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Signed 32)
  -> DSignal dom (n + d) Float
fromS32With = withFrozenCallStack $ hideEnable $ hideClock E.fromS32With
{-# INLINE fromS32With #-}

-- | Conversion of @Signed 32@ to @Float@, with default delay
fromS32
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Signed 32)
  -> DSignal dom (n + E.FromS32DefDelay) Float
fromS32 = withFrozenCallStack $ hideEnable $ hideClock E.fromS32
{-# INLINE fromS32 #-}

-- | Customizable floating point square root.
--
-- The square root of a negative zero is a negative zero. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the square root of a negative subnormal number is a negative zero too. The
-- square root of any other negative number, including negative infinity, is
-- NaN.
--
-- The t'E.Config' makes no difference in single precision: only the square root
-- in half precision uses it (@bMemUsage@).
sqrtWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
sqrtWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.sqrtWith cfg
{-# INLINE sqrtWith #-}

-- | Floating point square root with default settings.
--
-- The square root of a negative zero is a negative zero. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the square root of a negative subnormal number is a negative zero too. The
-- square root of any other negative number, including negative infinity, is
-- NaN.
sqrt
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.SqrtDefDelay) Float
sqrt = withFrozenCallStack $ hideEnable $ hideClock E.sqrt
{-# INLINE sqrt #-}

-- | Floating point absolute value.
--
-- The IP only clears the sign bit. Unlike the other operations, it leaves
-- subnormal numbers and the payloads of NaNs alone.
--
-- The IP only supports a latency of zero, so the result has the same delay as
-- the input. The IP is then combinational, without a clock or clock enable,
-- so this function does not take them either.
abs
  :: KnownDomain dom
  => DSignal dom n Float
  -> DSignal dom n Float
abs = E.abs
{-# INLINE abs #-}

-- | Customizable conversion of @Unsigned 64@ to @Float@. It rounds to
-- nearest, ties to even.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU64With
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Unsigned 64)
  -> DSignal dom (n + d) Float
fromU64With = withFrozenCallStack $ hideEnable $ hideClock E.fromU64With
{-# INLINE fromU64With #-}

-- | Conversion of @Unsigned 64@ to @Float@, with default delay. It rounds to
-- nearest, ties to even.
fromU64
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Unsigned 64)
  -> DSignal dom (n + E.FromU64DefDelay) Float
fromU64 = withFrozenCallStack $ hideEnable $ hideClock E.fromU64
{-# INLINE fromU64 #-}

-- | Customizable conversion of @Signed 64@ to @Float@. It rounds to
-- nearest, ties to even.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS64With
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Signed 64)
  -> DSignal dom (n + d) Float
fromS64With = withFrozenCallStack $ hideEnable $ hideClock E.fromS64With
{-# INLINE fromS64With #-}

-- | Conversion of @Signed 64@ to @Float@, with default delay. It rounds to
-- nearest, ties to even.
fromS64
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Signed 64)
  -> DSignal dom (n + E.FromS64DefDelay) Float
fromS64 = withFrozenCallStack $ hideEnable $ hideClock E.fromS64
{-# INLINE fromS64 #-}

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
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + d) (Signed 32)
toS32With = withFrozenCallStack $ hideEnable $ hideClock E.toS32With
{-# INLINE toS32With #-}

-- | Conversion of @Float@ to @Signed 32@, with default delay.
--
-- It rounds to nearest, ties to even, so @2.5@ becomes @2@ and @3.5@ becomes
-- @4@. Numbers that are out of range after rounding, including infinities,
-- saturate to 'minBound' or 'maxBound'. Every NaN becomes 'minBound'.
-- Subnormal numbers become zero.
toS32
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.ToS32DefDelay) (Signed 32)
toS32 = withFrozenCallStack $ hideEnable $ hideClock E.toS32
{-# INLINE toS32 #-}

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
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + d) b
convertWith = withFrozenCallStack $ hideEnable $ hideClock E.convertWith
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
     , KnownNat (E.ConvertDefDelay b)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.ConvertDefDelay b) b
convert = withFrozenCallStack $ hideEnable $ hideClock E.convert
{-# INLINE convert #-}

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
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + d) Half
toHalfWith = withFrozenCallStack $ hideEnable $ hideClock E.toHalfWith
{-# INLINE toHalfWith #-}

-- | Conversion of @Float@ to @Half@, with default delay.
--
-- It rounds to nearest, ties to even. Results below the smallest normal
-- number of the target precision become a zero of the same sign, results
-- beyond its largest finite number an infinity of the same sign. Like the
-- other operations, the IP treats subnormal inputs as a zero of the same
-- sign and turns every NaN into the NaN it generates.
toHalf
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.ToHalfDefDelay) Half
toHalf = withFrozenCallStack $ hideEnable $ hideClock E.toHalf
{-# INLINE toHalf #-}

-- | Customizable conversion of @Float@ to @Double@.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toDoubleWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + d) Double
toDoubleWith = withFrozenCallStack $ hideEnable $ hideClock E.toDoubleWith
{-# INLINE toDoubleWith #-}

-- | Conversion of @Float@ to @Double@, with default delay.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
toDouble
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.ToDoubleDefDelay) Double
toDouble = withFrozenCallStack $ hideEnable $ hideClock E.toDouble
{-# INLINE toDouble #-}
