{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://docs.amd.com/v/u/en-US/pg060-floating-point)
in half precision ('Half').

This module has the same functions as "Clash.Cores.Xilinx.Floating", but for
'Half' instead of 'Float'. See there for how the functions instantiate the IP
and how they behave. The differences follow from the precision: the IP rounds a
result to 11 significant bits before checking whether it is subnormal, and the
maximum latencies are different (see below). The IP does not support
conversions between integers and half precision, so this module has no
@fromU32@, @fromS32@, @fromU64@, @fromS64@ and @toS32@. In half precision,
'sqrtWith' can use block memory, see t'Clash.Cores.Xilinx.Floating.Half.BMemUsage'.
Like in single precision, Clash simulation gives the correctly rounded results
for 'exp' and 'log', which can differ in the last bit from the IP's, which are
only accurate to one unit in the last place. In half precision, the IP rounds
'recip' and 'rsqrt' correctly, so for them simulation and hardware agree.

The IP's exception flags are not available, except for the accumulator
('accumWithFlags'), but they follow cheaply from the inputs and the result:
see "Clash.Cores.Xilinx.Floating#flags".

Related modules:

* "Clash.Cores.Xilinx.Floating.Half.Explicit" has the same functions with
  explicit clock and enable arguments.

* "Clash.Cores.Xilinx.Floating" and "Clash.Cores.Xilinx.Floating.Double" are
  the same for single and double precision.

= Maximum latency

The latency of an operation can be anything from zero up to a maximum, which
depends on the operation and its configuration. Vivado reports an error when it
generates the IP for a higher latency. A lower latency means fewer pipeline
stages, which can lower the maximum clock frequency.

The maximum latencies in half precision, as reported by Vivado 2022.1:

+----------------------+---------------------------------------+-----------------+
| Function             | Configuration                         | Maximum latency |
+======================+=======================================+=================+
| 'addWith', 'subWith' | @SpeedArch@, @NoDspUsage@             | 8               |
+----------------------+---------------------------------------+-----------------+
| 'addWith', 'subWith' | @SpeedArch@, @MediumDspUsage@         | 11              |
+----------------------+---------------------------------------+-----------------+
| 'addWith', 'subWith' | @SpeedArch@, @FullDspUsage@ (default) | 11              |
+----------------------+---------------------------------------+-----------------+
| 'addWith', 'subWith' | @SpeedArch@, @MaxDspUsage@            | /rejected/      |
+----------------------+---------------------------------------+-----------------+
| 'addWith', 'subWith' | @LatencyArch@, any @DspUsage@         | 7               |
+----------------------+---------------------------------------+-----------------+
| 'mulWith'            | @NoDspUsage@                          | 6               |
+----------------------+---------------------------------------+-----------------+
| 'mulWith'            | @MediumDspUsage@                      | /rejected/      |
+----------------------+---------------------------------------+-----------------+
| 'mulWith'            | @FullDspUsage@ (default)              | 6               |
+----------------------+---------------------------------------+-----------------+
| 'mulWith'            | @MaxDspUsage@                         | 8               |
+----------------------+---------------------------------------+-----------------+
| 'divWith'            | any                                   | 15              |
+----------------------+---------------------------------------+-----------------+
| 'compareWith'        |                                       | 2               |
+----------------------+---------------------------------------+-----------------+
| 'sqrtWith'           | @NoBMemUsage@ (default)               | 15              |
+----------------------+---------------------------------------+-----------------+
| 'sqrtWith'           | @FullBMemUsage@                       | 4 (minimum 1)   |
+----------------------+---------------------------------------+-----------------+
| 'abs'                |                                       | 0 (only)        |
+----------------------+---------------------------------------+-----------------+
| 'toFloatWith'        |                                       | 2               |
+----------------------+---------------------------------------+-----------------+
| 'toDoubleWith'       |                                       | 2               |
+----------------------+---------------------------------------+-----------------+
| 'recipWith'          | any                                   | 4 (minimum 1)   |
+----------------------+---------------------------------------+-----------------+
| 'rsqrtWith'          | any                                   | 4 (minimum 1)   |
+----------------------+---------------------------------------+-----------------+
| 'expWith'            | any                                   | 13              |
+----------------------+---------------------------------------+-----------------+
| 'logWith'            | @NoDspUsage@                          | 17              |
+----------------------+---------------------------------------+-----------------+
| 'logWith'            | @MediumDspUsage@                      | 17              |
+----------------------+---------------------------------------+-----------------+
| 'logWith'            | @FullDspUsage@ (default)              | 17              |
+----------------------+---------------------------------------+-----------------+
| 'logWith'            | @MaxDspUsage@                         | /rejected/      |
+----------------------+---------------------------------------+-----------------+
| 'fmaWith'            | @NoDspUsage@, @MaxDspUsage@           | /rejected/      |
+----------------------+---------------------------------------+-----------------+
| 'fmaWith'            | @MediumDspUsage@                      | 15              |
+----------------------+---------------------------------------+-----------------+
| 'fmaWith'            | @FullDspUsage@ (default)              | 17              |
+----------------------+---------------------------------------+-----------------+
| 'accumWith'          | @NoDspUsage@                          | 12-21 (19)      |
+----------------------+---------------------------------------+-----------------+
| 'accumWith'          | @MediumDspUsage@ (default)            | 14-21 (21)      |
+----------------------+---------------------------------------+-----------------+
| 'accumWith'          | @FullDspUsage@                        | 15-22 (22)      |
+----------------------+---------------------------------------+-----------------+
| 'accumWith'          | @MaxDspUsage@                         | /rejected/      |
+----------------------+---------------------------------------+-----------------+

/rejected/ means that Vivado rejects the configuration itself, whatever the
latency. Notes on the table:

* 'mulWith' and 'fmaWith' ignore @archOpt@. With @LatencyArch@, Vivado ignores
  the @dspUsage@ setting of 'addWith' and 'subWith'.

* The IP builds 'recipWith' and 'rsqrtWith' from block memory without DSP
  slices, whatever the configuration, and 'expWith' ignores its configuration.
  'logWith' supports at most @MediumDspUsage@, and uses it instead of
  @FullDspUsage@ (the default).

* 'abs' only supports a latency of zero. All other operations support a
  latency of zero as well, except the accumulator and the entries marked
  /minimum 1/: 'sqrtWith' with @FullBMemUsage@, 'recipWith' and 'rsqrtWith'.
  The IP is then combinational, and the 'Enable' has no effect. The
  accumulator needs a latency of at least one: 'accumWith' and
  'accumWithFlags' require @1 <= d@.

* The maximum latency of 'accumWith' and 'accumWithFlags' also depends on
  their bounds: wider accumulators need more cycles. The table gives the lowest
  and the highest maximum latency over all bounds the IP accepts, and in
  parentheses the maximum latency for the default bounds
  ('Clash.Cores.Xilinx.Floating.Half.defAccumConfig').

* The maximum latencies are the same on all measured devices: 7-series
  (Artix-7, Kintex-7 and Spartan-7), UltraScale (Kintex UltraScale) and
  UltraScale+ (Kintex UltraScale+ and Zynq UltraScale+). Those of 'fmaWith' and
  the accumulator were only measured on Kintex-7 and Kintex UltraScale+, and
  those of the accumulator only for a sample of the bounds. Other versions of
  Vivado might accept other latencies.

The functions without @With@ use 'Clash.Cores.Xilinx.Floating.Half.defConfig'
and the default delays (t'Clash.Cores.Xilinx.Floating.Half.AddDefDelay' and so
on). These are the maximum latencies for that configuration. With another
configuration, the maximum latency can be lower than the default delay, for
example for 'addWith' with @NoDspUsage@. 'accum' uses
'Clash.Cores.Xilinx.Floating.Half.defAccumConfig', and
t'Clash.Cores.Xilinx.Floating.Half.AccumDefDelay' is the maximum latency for
those settings.
-}

module Clash.Cores.Xilinx.Floating.Half
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
  , E.Ordering(..)
  , E.toMaybeOrdering
  , compare
  , compareWith
  , E.CompareDefDelay
  , sqrtWith
  , sqrt
  , E.SqrtDefDelay
  , abs
  , toFloatWith
  , toFloat
  , E.ToFloatDefDelay
  , toDoubleWith
  , toDouble
  , E.ToDoubleDefDelay
  , recipWith
  , recip
  , E.RecipDefDelay
  , rsqrtWith
  , rsqrt
  , E.RSqrtDefDelay
  , expWith
  , exp
  , E.ExpDefDelay
  , logWith
  , log
  , E.LogDefDelay
  , fmaWith
  , fma
  , E.FmaDefDelay
  , accumWith
  , accum
  , accumWithFlags
  , E.AccumDefDelay
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.ArchOpt(..)
  , E.DspUsage(..)
  , E.BMemUsage(..)
  , E.AccumConfig(..)
  , E.defAccumConfig
  , E.AccumFlags(..)
    -- * Additional functions
  , E.xilinxNaN
  ) where

import Clash.Prelude hiding
  (Ordering(..), add, sub, mul, div, compare, sqrt, abs, recip, exp, log)

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric.Half (Half)

import qualified Clash.Cores.Xilinx.Floating.Half.Explicit as E

-- | Customizable floating point addition.
addWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
addWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.addWith cfg
{-# INLINE addWith #-}

-- | Floating point addition with default settings.
add
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + E.AddDefDelay) Half
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
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
subWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.subWith cfg
{-# INLINE subWith #-}

-- | Floating point subtraction with default settings.
sub
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + E.SubDefDelay) Half
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
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
mulWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.mulWith cfg
{-# INLINE mulWith #-}

-- | Floating point multiplication with default settings.
mul
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + E.MulDefDelay) Half
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
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
divWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.divWith cfg
{-# INLINE divWith #-}

-- | Floating point division with default settings.
div
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + E.DivDefDelay) Half
div = withFrozenCallStack $ hideEnable $ hideClock E.div
{-# INLINE div #-}

-- | Customizable floating point comparison
--
-- Produces 'Clash.Cores.Xilinx.Floating.Half.NaN' if any of the inputs is
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
  => DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + d) E.Ordering
compareWith = E.compareWith hasClock hasEnable
{-# INLINE compareWith #-}

-- | Floating point comparison, with default delay
--
-- Produces 'Clash.Cores.Xilinx.Floating.Half.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
compare
  :: forall dom n
   . ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + E.CompareDefDelay) E.Ordering
compare = compareWith
{-# INLINE compare #-}

-- | Customizable floating point square root.
--
-- The square root of a negative zero is a negative zero. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the square root of a negative subnormal number is a negative zero too. The
-- square root of any other negative number, including negative infinity, is
-- NaN.
--
-- With @bMemUsage = 'Clash.Cores.Xilinx.Floating.Half.FullBMemUsage'@, the
-- IP uses block memory, which lowers its maximum latency but needs a latency
-- of at least one. The IP ignores the other settings.
sqrtWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
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
  => DSignal dom n Half
  -> DSignal dom (n + E.SqrtDefDelay) Half
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
  => DSignal dom n Half
  -> DSignal dom n Half
abs = E.abs
{-# INLINE abs #-}

-- | Customizable conversion of @Half@ to @Float@.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toFloatWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom (n + d) Float
toFloatWith = withFrozenCallStack $ hideEnable $ hideClock E.toFloatWith
{-# INLINE toFloatWith #-}

-- | Conversion of @Half@ to @Float@, with default delay.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
toFloat
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom (n + E.ToFloatDefDelay) Float
toFloat = withFrozenCallStack $ hideEnable $ hideClock E.toFloat
{-# INLINE toFloat #-}

-- | Customizable conversion of @Half@ to @Double@.
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
  => DSignal dom n Half
  -> DSignal dom (n + d) Double
toDoubleWith = withFrozenCallStack $ hideEnable $ hideClock E.toDoubleWith
{-# INLINE toDoubleWith #-}

-- | Conversion of @Half@ to @Double@, with default delay.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
toDouble
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom (n + E.ToDoubleDefDelay) Double
toDouble = withFrozenCallStack $ hideEnable $ hideClock E.toDouble
{-# INLINE toDouble #-}

-- | Customizable floating point reciprocal, @1 / x@.
--
-- The reciprocal of a zero is an infinity of the same sign, and that of an
-- infinity a zero of the same sign. Like the other operations, the IP treats
-- subnormal inputs as a zero of the same sign and flushes results below the
-- smallest normal number to a zero of the same sign.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- reciprocal rounded to nearest, ties to even. In half precision, the IP
-- rounds correctly too, so simulation and hardware agree.
--
-- The t'Clash.Cores.Xilinx.Floating.Half.Config' makes no difference: in
-- half precision, the IP builds the reciprocal from block memory, without DSP
-- slices. The latency must be at least one.
recipWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
recipWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.recipWith cfg
{-# INLINE recipWith #-}

-- | Floating point reciprocal, @1 / x@, with default settings.
--
-- The reciprocal of a zero is an infinity of the same sign, and that of an
-- infinity a zero of the same sign. Like the other operations, the IP treats
-- subnormal inputs as a zero of the same sign and flushes results below the
-- smallest normal number to a zero of the same sign.
--
-- Clash simulation gives the correctly rounded result, and so does the IP in
-- half precision. See 'recipWith'.
recip
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom (n + E.RecipDefDelay) Half
recip = withFrozenCallStack $ hideEnable $ hideClock E.recip
{-# INLINE recip #-}

-- | Customizable floating point reciprocal square root, @1 / sqrt x@.
--
-- The reciprocal square root of a zero is an infinity of the same sign. Like
-- the other operations, the IP treats subnormal inputs as a zero of the same
-- sign, so the reciprocal square root of a negative subnormal number is
-- negative infinity. That of any other negative number, including negative
-- infinity, is NaN. That of positive infinity is positive zero.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- reciprocal square root rounded to nearest, ties to even. In half precision,
-- the IP rounds correctly too, so simulation and hardware agree.
--
-- The t'Clash.Cores.Xilinx.Floating.Half.Config' makes no difference: in
-- half precision, the IP builds the reciprocal square root from block memory,
-- without DSP slices. The latency must be at least one.
rsqrtWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
rsqrtWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.rsqrtWith cfg
{-# INLINE rsqrtWith #-}

-- | Floating point reciprocal square root, @1 / sqrt x@, with default settings.
--
-- The reciprocal square root of a zero is an infinity of the same sign. Like
-- the other operations, the IP treats subnormal inputs as a zero of the same
-- sign, so the reciprocal square root of a negative subnormal number is
-- negative infinity. That of any other negative number, including negative
-- infinity, is NaN. That of positive infinity is positive zero.
--
-- Clash simulation gives the correctly rounded result, and so does the IP in
-- half precision. See 'rsqrtWith'.
rsqrt
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom (n + E.RSqrtDefDelay) Half
rsqrt = withFrozenCallStack $ hideEnable $ hideClock E.rsqrt
{-# INLINE rsqrt #-}

-- | Customizable floating point exponential, @e^x@.
--
-- Like the other operations, the IP treats subnormal inputs as a zero, whose
-- exponential is one. The exponential of positive infinity is positive
-- infinity, that of negative infinity positive zero. Results beyond the
-- largest finite number become positive infinity, results below the smallest
-- normal number positive zero.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- exponential rounded to nearest, ties to even. The IP is only accurate to one
-- unit in the last place (PG060), so the hardware can differ from the
-- simulation in the last bit: the IP's C model, which matches the IP bit for
-- bit, differs from the correctly rounded result for 3.9% of the inputs with a
-- finite result other than zero and one.
--
-- The t'Clash.Cores.Xilinx.Floating.Half.Config' makes no difference: in half
-- precision, the IP always uses @FullDspUsage@ and no block memory for the
-- exponential.
expWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
expWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.expWith cfg
{-# INLINE expWith #-}

-- | Floating point exponential, @e^x@, with default settings.
--
-- Like the other operations, the IP treats subnormal inputs as a zero, whose
-- exponential is one. The exponential of positive infinity is positive
-- infinity, that of negative infinity positive zero. Results beyond the
-- largest finite number become positive infinity, results below the smallest
-- normal number positive zero.
--
-- Clash simulation gives the correctly rounded result, while the IP is only
-- accurate to one unit in the last place, so they can differ in the last bit.
-- See 'expWith'.
exp
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom (n + E.ExpDefDelay) Half
exp = withFrozenCallStack $ hideEnable $ hideClock E.exp
{-# INLINE exp #-}

-- | Customizable floating point natural logarithm, @log x@.
--
-- The logarithm of a zero of either sign is negative infinity. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the logarithm of a subnormal number, even a negative one, is negative
-- infinity too. That of any other negative number, including negative
-- infinity, is NaN. That of positive infinity is positive infinity, and that
-- of one positive zero.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- logarithm rounded to nearest, ties to even. The IP is only accurate to one
-- unit in the last place (PG060), so the hardware can differ from the
-- simulation in the last bit: the IP's C model, which matches the IP bit for
-- bit, differs from the correctly rounded result for 0.3% of the inputs with a
-- finite, non-zero result.
--
-- Only the @dspUsage@ setting of the t'Clash.Cores.Xilinx.Floating.Half.Config'
-- matters. In half precision, the IP supports at most @MediumDspUsage@ for the
-- logarithm: it rejects @MaxDspUsage@, and with @FullDspUsage@, the default,
-- this function uses @MediumDspUsage@ instead.
logWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
logWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.logWith cfg
{-# INLINE logWith #-}

-- | Floating point natural logarithm, @log x@, with default settings.
--
-- The logarithm of a zero of either sign is negative infinity. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the logarithm of a subnormal number, even a negative one, is negative
-- infinity too. That of any other negative number, including negative
-- infinity, is NaN. That of positive infinity is positive infinity, and that
-- of one positive zero.
--
-- Clash simulation gives the correctly rounded result, while the IP is only
-- accurate to one unit in the last place, so they can differ in the last bit.
-- See 'logWith'.
log
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom (n + E.LogDefDelay) Half
log = withFrozenCallStack $ hideEnable $ hideClock E.log
{-# INLINE log #-}

-- | Customizable fused multiply-add: @fmaWith cfg x y z@ computes @x * y + z@
-- with a single rounding, like C's @fma@. For a fused multiply-subtract, flip
-- the sign bit of the addend.
--
-- Like the other operations, the IP treats subnormal inputs as a zero of the
-- same sign and flushes results below the smallest normal number to a zero of
-- the same sign. The product is exact, so a product beyond the largest finite
-- number still gives a finite result if the addend brings the sum back in
-- range. An infinity times a zero is NaN, and so is an infinite product plus
-- an infinity of the opposite sign. See
-- 'Clash.Cores.Xilinx.Floating.Explicit.fmaWith' for the details.
--
-- Only the @dspUsage@ setting of the t'Clash.Cores.Xilinx.Floating.Half.Config'
-- matters. The IP accepts @MediumDspUsage@ and @FullDspUsage@ (the default),
-- and rejects @NoDspUsage@ and @MaxDspUsage@.
fmaWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
fmaWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.fmaWith cfg
{-# INLINE fmaWith #-}

-- | Fused multiply-add with default settings: @fma x y z@ computes
-- @x * y + z@ with a single rounding, like C's @fma@. See 'fmaWith'.
fma
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + E.FmaDefDelay) Half
fma = withFrozenCallStack $ hideEnable $ hideClock E.fma
{-# INLINE fma #-}

-- | Customizable accumulator: @accumWith cfg lst x@ gives the running sum of
-- the inputs @x@. An input with @lst@ set is the last one of its
-- accumulation: its result is the complete sum, and the next input starts a
-- new accumulation. De-asserting the 'Enable' stalls the IP: the input of that
-- cycle is ignored.
--
-- The IP adds in fixed point, with the bounds of the
-- t'Clash.Cores.Xilinx.Floating.Half.AccumConfig': inputs are truncated towards
-- zero to a multiple of @2^accumLsb@ and added exactly, and the result is the
-- sum rounded __towards zero__. Inputs whose magnitude reaches
-- @2^(accumInputMsb + 1)@ and sums whose magnitude reaches
-- @2^(accumMsb + 1)@ make the result NaN until the end of the accumulation.
-- See 'Clash.Cores.Xilinx.Floating.Explicit.accumWith' for the details and
-- the bounds the IP accepts.
accumWith
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , 1 <= d
     , HasCallStack
     )
  => E.AccumConfig
  -> DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
accumWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.accumWith cfg
{-# INLINE accumWith #-}

-- | Accumulator with the IP's default settings
-- ('Clash.Cores.Xilinx.Floating.Half.defAccumConfig'). See 'accumWith'.
accum
  :: ( HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n Half
  -> DSignal dom (n + E.AccumDefDelay) Half
accum = withFrozenCallStack $ hideEnable $ hideClock E.accum
{-# INLINE accum #-}

-- | Customizable accumulator that also outputs the IP's exception flags, see
-- t'Clash.Cores.Xilinx.Floating.Half.AccumFlags' and
-- 'Clash.Cores.Xilinx.Floating.Explicit.accumWithFlags'.
accumWithFlags
  :: ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , 1 <= d
     , HasCallStack
     )
  => E.AccumConfig
  -> DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n Half
  -> DSignal dom (n + d) (Half, E.AccumFlags)
accumWithFlags cfg =
  withFrozenCallStack $ hideEnable . hideClock $ E.accumWithFlags cfg
{-# INLINE accumWithFlags #-}
