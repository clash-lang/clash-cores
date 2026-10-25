{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://docs.amd.com/v/u/en-US/pg060-floating-point)
in double precision ('Double').

This module has the same functions as "Clash.Cores.Xilinx.Floating", but for
'Double' instead of 'Float'. See there for how the functions instantiate the IP
and how they behave. The differences follow from the precision: the IP rounds a
result to 53 significant bits before checking whether it is subnormal, and the
maximum latencies are different (see below).

Related modules:

* "Clash.Cores.Xilinx.Floating.Double.Explicit" has the same functions with
  explicit clock and enable arguments.

* "Clash.Cores.Xilinx.Floating" and "Clash.Cores.Xilinx.Floating.Half" are the
  same for single and half precision.

* "Clash.Cores.Xilinx.Floating.Generic" has the same functions, generic in the
  precision.

= Maximum latency

The latency of an operation can be anything from zero up to a maximum, which
depends on the operation, its configuration and sometimes the device family.
Vivado reports an error when it generates the IP for a higher latency. A lower
latency means fewer pipeline stages, which can lower the maximum clock
frequency.

The maximum latencies in double precision, as reported by Vivado 2022.1, for
7-series devices and for UltraScale and UltraScale+ devices:

+-----------------------------------------+---------------------------------------+------------+---------------+
| Function                                | Configuration                         | 7-series   | UltraScale(+) |
+=========================================+=======================================+============+===============+
| 'addWith', 'subWith'                    | @SpeedArch@, @NoDspUsage@             | 12         | 12            |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'addWith', 'subWith'                    | @SpeedArch@, @MediumDspUsage@         | /rejected/ | /rejected/    |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'addWith', 'subWith'                    | @SpeedArch@, @FullDspUsage@ (default) | 14         | 14            |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'addWith', 'subWith'                    | @SpeedArch@, @MaxDspUsage@            | /rejected/ | /rejected/    |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'addWith', 'subWith'                    | @LatencyArch@, any @DspUsage@         | 8          | 8             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'mulWith'                               | @NoDspUsage@                          | 9          | 9             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'mulWith'                               | @MediumDspUsage@                      | 15         | 12            |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'mulWith'                               | @FullDspUsage@ (default)              | 15         | 12            |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'mulWith'                               | @MaxDspUsage@                         | 16         | 13            |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'divWith'                               | any                                   | 57         | 57            |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'compareWith'                           |                                       | 2          | 2             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'fromU32With'                           |                                       | 5          | 5             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'fromS32With'                           |                                       | 6          | 6             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'sqrtWith'                              | any                                   | 57         | 57            |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'abs'                                   |                                       | 0 (only)   | 0 (only)      |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'fromU64With'                           |                                       | 6          | 6             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'fromS64With'                           |                                       | 7          | 7             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'toS32With'                             |                                       | 6          | 6             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'convertWith', to @Double@              |                                       | 2          | 2             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'toHalfWith', 'convertWith' to @Half@   |                                       | 3          | 3             |
+-----------------------------------------+---------------------------------------+------------+---------------+
| 'toFloatWith', 'convertWith' to @Float@ |                                       | 3          | 3             |
+-----------------------------------------+---------------------------------------+------------+---------------+

/rejected/ means that Vivado rejects the configuration itself, whatever the
latency. 'mulWith' ignores the @archOpt@ setting. With @LatencyArch@, Vivado
ignores the @dspUsage@ setting of 'addWith' and 'subWith'. 'sqrtWith' only
uses the @bMemUsage@ setting in half precision. The IP only supports a latency
of zero for 'abs'. The measured 7-series devices are Artix-7, Kintex-7 and Spartan-7; the UltraScale and
UltraScale+ devices are Kintex UltraScale, Kintex UltraScale+ and Zynq
UltraScale+. Other versions of Vivado might accept other latencies.

The functions without @With@ use
'Clash.Cores.Xilinx.Floating.Double.defConfig' and the default delays
(t'Clash.Cores.Xilinx.Floating.Double.AddDefDelay' and so on). These are the
maximum latencies for that configuration, except for 'mul' on 7-series
devices: t'Clash.Cores.Xilinx.Floating.Double.MulDefDelay' is 12, the maximum
on UltraScale and UltraScale+ devices. When changing the configuration, note
that the maximum latency can be lower than the default delay, for example for
'addWith' with @NoDspUsage@.
-}

module Clash.Cores.Xilinx.Floating.Double
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
  , toFloatWith
  , toFloat
  , E.ToFloatDefDelay
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

import qualified Clash.Cores.Xilinx.Floating.Double.Explicit as E
import Clash.Cores.Xilinx.Floating.Internal (XilinxFloat)

-- | Customizable floating point addition.
addWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
addWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.addWith cfg
{-# INLINE addWith #-}

-- | Floating point addition with default settings.
add
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.AddDefDelay) Double
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
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
subWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.subWith cfg
{-# INLINE subWith #-}

-- | Floating point subtraction with default settings.
sub
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.SubDefDelay) Double
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
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
mulWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.mulWith cfg
{-# INLINE mulWith #-}

-- | Floating point multiplication with default settings.
mul
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.MulDefDelay) Double
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
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
divWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.divWith cfg
{-# INLINE divWith #-}

-- | Floating point division with default settings.
div
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.DivDefDelay) Double
div = withFrozenCallStack $ hideEnable $ hideClock E.div
{-# INLINE div #-}

-- | Customizable floating point comparison
--
-- Produces 'Clash.Cores.Xilinx.Floating.Double.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
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
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) E.Ordering
compareWith = E.compareWith hasClock hasEnable
{-# INLINE compareWith #-}

-- | Floating point comparison, with default delay
--
-- Produces 'Clash.Cores.Xilinx.Floating.Double.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
compare
  :: forall dom n
   . ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.CompareDefDelay) E.Ordering
compare = compareWith
{-# INLINE compare #-}

-- | Customizable conversion of @Unsigned 32@ to @Double@
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
  -> DSignal dom (n + d) Double
fromU32With = withFrozenCallStack $ hideEnable $ hideClock E.fromU32With
{-# INLINE fromU32With #-}

-- | Conversion of @Unsigned 32@ to @Double@, with default delay
fromU32
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Unsigned 32)
  -> DSignal dom (n + E.FromU32DefDelay) Double
fromU32 = withFrozenCallStack $ hideEnable $ hideClock E.fromU32
{-# INLINE fromU32 #-}

-- | Customizable conversion of @Signed 32@ to @Double@
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
  -> DSignal dom (n + d) Double
fromS32With = withFrozenCallStack $ hideEnable $ hideClock E.fromS32With
{-# INLINE fromS32With #-}

-- | Conversion of @Signed 32@ to @Double@, with default delay
fromS32
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Signed 32)
  -> DSignal dom (n + E.FromS32DefDelay) Double
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
-- The t'E.Config' makes no difference in double precision: only the square root
-- in half precision uses it (@bMemUsage@).
sqrtWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
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
  => DSignal dom n Double
  -> DSignal dom (n + E.SqrtDefDelay) Double
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
  => DSignal dom n Double
  -> DSignal dom n Double
abs = E.abs
{-# INLINE abs #-}

-- | Customizable conversion of @Unsigned 64@ to @Double@. It rounds to
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
  -> DSignal dom (n + d) Double
fromU64With = withFrozenCallStack $ hideEnable $ hideClock E.fromU64With
{-# INLINE fromU64With #-}

-- | Conversion of @Unsigned 64@ to @Double@, with default delay. It rounds to
-- nearest, ties to even.
fromU64
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Unsigned 64)
  -> DSignal dom (n + E.FromU64DefDelay) Double
fromU64 = withFrozenCallStack $ hideEnable $ hideClock E.fromU64
{-# INLINE fromU64 #-}

-- | Customizable conversion of @Signed 64@ to @Double@. It rounds to
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
  -> DSignal dom (n + d) Double
fromS64With = withFrozenCallStack $ hideEnable $ hideClock E.fromS64With
{-# INLINE fromS64With #-}

-- | Conversion of @Signed 64@ to @Double@, with default delay. It rounds to
-- nearest, ties to even.
fromS64
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Signed 64)
  -> DSignal dom (n + E.FromS64DefDelay) Double
fromS64 = withFrozenCallStack $ hideEnable $ hideClock E.fromS64
{-# INLINE fromS64 #-}

-- | Customizable conversion of @Double@ to @Signed 32@.
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
  => DSignal dom n Double
  -> DSignal dom (n + d) (Signed 32)
toS32With = withFrozenCallStack $ hideEnable $ hideClock E.toS32With
{-# INLINE toS32With #-}

-- | Conversion of @Double@ to @Signed 32@, with default delay.
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
  => DSignal dom n Double
  -> DSignal dom (n + E.ToS32DefDelay) (Signed 32)
toS32 = withFrozenCallStack $ hideEnable $ hideClock E.toS32
{-# INLINE toS32 #-}

-- | Customizable conversion of @Double@ to precision @b@, which can be @Double@
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
  => DSignal dom n Double
  -> DSignal dom (n + d) b
convertWith = withFrozenCallStack $ hideEnable $ hideClock E.convertWith
{-# INLINE convertWith #-}

-- | Conversion of @Double@ to precision @b@, which can be @Double@ itself, with
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
  => DSignal dom n Double
  -> DSignal dom (n + E.ConvertDefDelay b) b
convert = withFrozenCallStack $ hideEnable $ hideClock E.convert
{-# INLINE convert #-}

-- | Customizable conversion of @Double@ to @Half@.
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
  => DSignal dom n Double
  -> DSignal dom (n + d) Half
toHalfWith = withFrozenCallStack $ hideEnable $ hideClock E.toHalfWith
{-# INLINE toHalfWith #-}

-- | Conversion of @Double@ to @Half@, with default delay.
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
  => DSignal dom n Double
  -> DSignal dom (n + E.ToHalfDefDelay) Half
toHalf = withFrozenCallStack $ hideEnable $ hideClock E.toHalf
{-# INLINE toHalf #-}

-- | Customizable conversion of @Double@ to @Float@.
--
-- It rounds to nearest, ties to even. Results below the smallest normal
-- number of the target precision become a zero of the same sign, results
-- beyond its largest finite number an infinity of the same sign. Like the
-- other operations, the IP treats subnormal inputs as a zero of the same
-- sign and turns every NaN into the NaN it generates.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toFloatWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom (n + d) Float
toFloatWith = withFrozenCallStack $ hideEnable $ hideClock E.toFloatWith
{-# INLINE toFloatWith #-}

-- | Conversion of @Double@ to @Float@, with default delay.
--
-- It rounds to nearest, ties to even. Results below the smallest normal
-- number of the target precision become a zero of the same sign, results
-- beyond its largest finite number an infinity of the same sign. Like the
-- other operations, the IP treats subnormal inputs as a zero of the same
-- sign and turns every NaN into the NaN it generates.
toFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom (n + E.ToFloatDefDelay) Float
toFloat = withFrozenCallStack $ hideEnable $ hideClock E.toFloat
{-# INLINE toFloat #-}
