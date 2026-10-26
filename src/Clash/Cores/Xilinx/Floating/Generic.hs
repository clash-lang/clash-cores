{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://docs.amd.com/v/u/en-US/pg060-floating-point),
for every precision it supports: 'Numeric.Half.Half', 'Float' (single
precision) and 'Double'.

This module has the same functions as "Clash.Cores.Xilinx.Floating", but
generic in the precision, see
t'Clash.Cores.Xilinx.Floating.Generic.XilinxFloat'. See
"Clash.Cores.Xilinx.Floating" for how the functions instantiate the IP and how
they behave. The default delays depend on the precision, so they are type
families. The IP does not support conversions between integers and half
precision, so 'fromU32' and 'fromS32' only produce 'Float' and 'Double', see
t'Clash.Cores.Xilinx.Floating.Generic.FromInt32', and likewise for 'fromU64'
and 'fromS64' (t'Clash.Cores.Xilinx.Floating.Generic.FromInt64') and 'toS32'
(t'Clash.Cores.Xilinx.Floating.Generic.ToInt32'). 'convert' converts between
any two precisions. Like in "Clash.Cores.Xilinx.Floating", Clash simulation
gives the correctly rounded results for 'recip', 'rsqrt', 'exp' and 'log',
which can differ in the last bit from the IP's, which are only accurate to one
unit in the last place (except for 'recip' and 'rsqrt' in half precision, which
the IP rounds correctly).

Related modules:

* "Clash.Cores.Xilinx.Floating.Generic.Explicit" has the same functions with
  explicit clock and enable arguments.

* "Clash.Cores.Xilinx.Floating.Half", "Clash.Cores.Xilinx.Floating" and
  "Clash.Cores.Xilinx.Floating.Double" have the same functions for one fixed
  precision each, which avoids type annotations and type applications.

= Maximum latency

The latency of an operation can be anything from zero up to a maximum, which
depends on the precision, the operation, its configuration and sometimes the
device family. Vivado reports an error when it generates the IP for a higher
latency. A lower latency means fewer pipeline stages, which can lower the
maximum clock frequency.

The maximum latencies, as reported by Vivado 2022.1:

+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| Function                   | Configuration                           | Half          | Float      | Double, 7-series | Double, UltraScale(+) |
+============================+=========================================+===============+============+==================+=======================+
| 'addWith', 'subWith'       | @SpeedArch@, @NoDspUsage@               | 8             | 12         | 12               | 12                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'addWith', 'subWith'       | @SpeedArch@, @MediumDspUsage@           | 11            | 11         | /rejected/       | /rejected/            |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'addWith', 'subWith'       | @SpeedArch@, @FullDspUsage@ (default)   | 11            | 11         | 14               | 14                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'addWith', 'subWith'       | @SpeedArch@, @MaxDspUsage@              | /rejected/    | /rejected/ | /rejected/       | /rejected/            |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'addWith', 'subWith'       | @LatencyArch@, any @DspUsage@           | 7             | 8          | 8                | 8                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'mulWith'                  | @NoDspUsage@                            | 6             | 8          | 9                | 9                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'mulWith'                  | @MediumDspUsage@                        | /rejected/    | 8          | 15               | 12                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'mulWith'                  | @FullDspUsage@ (default)                | 6             | 8          | 15               | 12                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'mulWith'                  | @MaxDspUsage@                           | 8             | 6          | 16               | 13                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'divWith'                  | any                                     | 15            | 28         | 57               | 57                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'compareWith'              |                                         | 2             | 2          | 2                | 2                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'fromU32With'              |                                         | n/a           | 5          | 5                | 5                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'fromS32With'              |                                         | n/a           | 6          | 6                | 6                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'sqrtWith'                 | @NoBMemUsage@ (default)                 | 15            | 28         | 57               | 57                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'sqrtWith'                 | @FullBMemUsage@                         | 4 (minimum 1) | 28         | 57               | 57                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'abs'                      |                                         | 0 (only)      | 0 (only)   | 0 (only)         | 0 (only)              |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'fromU64With'              |                                         | n/a           | 6          | 6                | 6                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'fromS64With'              |                                         | n/a           | 7          | 7                | 7                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'toS32With'                |                                         | n/a           | 6          | 6                | 6                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'convertWith', to @Half@   |                                         | 2             | 3          | 3                | 3                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'convertWith', to @Float@  |                                         | 2             | 2          | 3                | 3                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'convertWith', to @Double@ |                                         | 2             | 2          | 2                | 2                     |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'recipWith'                | @NoDspUsage@                            | 4 (minimum 1) | 36         | 35               | 35                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'recipWith'                | @MediumDspUsage@, @MaxDspUsage@         | 4 (minimum 1) | /rejected/ | 35               | 35                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'recipWith'                | @FullDspUsage@ (default)                | 4 (minimum 1) | 29         | 35               | 35                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'rsqrtWith'                | @NoDspUsage@                            | 4 (minimum 1) | 37         | 112              | 110                   |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'rsqrtWith'                | @MediumDspUsage@, @MaxDspUsage@         | 4 (minimum 1) | /rejected/ | 112              | 110                   |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'rsqrtWith'                | @FullDspUsage@ (default)                | 4 (minimum 1) | 32         | 112              | 110                   |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'expWith'                  | @NoDspUsage@, @NoBMemUsage@             | 13            | 23         | 39               | 39                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'expWith'                  | @NoDspUsage@, @FullBMemUsage@           | 13            | 24         | 40               | 40                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'expWith'                  | @MediumDspUsage@, @NoBMemUsage@         | 13            | 20         | 44               | 44                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'expWith'                  | @MediumDspUsage@, @FullBMemUsage@       | 13            | 21         | 45               | 45                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'expWith'                  | @FullDspUsage@, @NoBMemUsage@ (default) | 13            | 29         | 56               | 56                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'expWith'                  | @FullDspUsage@, @FullBMemUsage@         | 13            | 29         | 57               | 57                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'expWith'                  | @MaxDspUsage@                           | 13            | /rejected/ | /rejected/       | /rejected/            |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'logWith'                  | @NoDspUsage@                            | 17            | 23         | 37               | 37                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'logWith'                  | @MediumDspUsage@                        | 17            | 22         | 52               | 48                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'logWith'                  | @FullDspUsage@ (default)                | 17            | 28         | 67               | 64                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'logWith'                  | @MaxDspUsage@                           | /rejected/    | /rejected/ | /rejected/       | /rejected/            |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'fmaWith'                  | @NoDspUsage@, @MaxDspUsage@             | /rejected/    | /rejected/ | /rejected/       | /rejected/            |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'fmaWith'                  | @MediumDspUsage@                        | 15            | 16         | 26               | 26                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'fmaWith'                  | @FullDspUsage@ (default)                | 17            | 19         | 29               | 29                    |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'accumWith'                | @NoDspUsage@                            | 12-21 (19)    | 12-33 (19) | 12-62 (18)       | 12-62 (18)            |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'accumWith'                | @MediumDspUsage@ (default)              | 14-21 (21)    | 14-40 (22) | 14-156 (22)      | 14-156 (22)           |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'accumWith'                | @FullDspUsage@                          | 15-22 (22)    | 15-41 (23) | 16-158 (24)      | 16-158 (24)           |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+
| 'accumWith'                | @MaxDspUsage@                           | /rejected/    | /rejected/ | /rejected/       | /rejected/            |
+----------------------------+-----------------------------------------+---------------+------------+------------------+-----------------------+

/rejected/ means that Vivado rejects the configuration itself, whatever the
latency. 'mulWith' ignores the @archOpt@ setting. With @LatencyArch@, Vivado
ignores the @dspUsage@ setting of 'addWith' and 'subWith'. 'sqrtWith' only
uses the @bMemUsage@ setting in half precision; with @FullBMemUsage@ it also
needs a latency of at least one. The IP only supports a latency of zero for
'abs'. For 'convertWith', the columns are the precision of the input. In half
precision, the IP builds 'recipWith' and 'rsqrtWith' from block memory without
DSP slices, whatever the configuration, and they need a latency of at least
one; 'expWith' ignores the configuration; and 'logWith' supports at most
@MediumDspUsage@, so with @FullDspUsage@ (the default) it uses @MediumDspUsage@
instead. In double precision, 'recipWith' and 'rsqrtWith' always use
@FullDspUsage@. 'expWith' only uses the @bMemUsage@ setting in single and
double precision. Only multiplication, the reciprocal square root and the
logarithm in 'Double' have different maxima on 7-series devices than on
UltraScale and UltraScale+ devices. The measured 7-series devices are Artix-7,
Kintex-7 and Spartan-7; the UltraScale and UltraScale+ devices are Kintex
UltraScale, Kintex UltraScale+ and Zynq UltraScale+. Other versions of Vivado
might accept other latencies.

For 'fmaWith', the IP accepts only @MediumDspUsage@ and @FullDspUsage@ and
ignores @archOpt@. 'accumWith' (and 'accumWithFlags') only use the
@accumDspUsage@ setting of their configuration, but their maximum latency also
depends on its bounds: wider accumulators need more cycles. The table gives the
range (as @lowest-highest@) over all bounds the IP accepts, and in parentheses
the maximum latency for the default bounds
('Clash.Cores.Xilinx.Floating.Generic.defAccumConfig'). Their minimum latency is
one. The maximum latencies of 'fmaWith' and 'accumWith' were measured on
Kintex-7 and Kintex UltraScale+ devices, where they are the same, for the
accumulator for a sample of the bounds.

The functions without @With@ use
'Clash.Cores.Xilinx.Floating.Generic.defConfig' and the default delays
(t'Clash.Cores.Xilinx.Floating.Generic.AddDefDelay' and so on). These are the
maximum latencies for that configuration, except for 'mul', 'rsqrt' and 'log'
of 'Double' on 7-series devices:
t'Clash.Cores.Xilinx.Floating.Generic.MulDefDelay',
t'Clash.Cores.Xilinx.Floating.Generic.RSqrtDefDelay' and
t'Clash.Cores.Xilinx.Floating.Generic.LogDefDelay' are 12, 110 and 64 for
'Double', the maxima on UltraScale and UltraScale+ devices. With another
configuration, the maximum latency can be lower than the default delay, for
example for 'addWith' with @NoDspUsage@ in half and double precision. 'accum'
uses 'Clash.Cores.Xilinx.Floating.Generic.defAccumConfig', and
t'Clash.Cores.Xilinx.Floating.Generic.AccumDefDelay' is the maximum latency
for those settings.

= Exception flags #flags#

The IP can output exception flags (underflow, overflow, invalid operation and
divide by zero), but the functions in these modules do not, except for the
accumulator ('accumWithFlags'). The flags of the other operations follow
cheaply from their inputs and result, as described below. Here @x@, @y@ and @z@
are the inputs the way the IP sees them: a subnormal input counts as a zero of
the same sign. These rules give exactly the flags of the IP's C model, which
matches the IP bit for bit; the test suite checks them for every operation.

* __Invalid operation__ is raised if the result is NaN while no input is NaN:
  for adding infinities of opposite signs (in 'addWith' and 'fmaWith') or
  subtracting infinities of the same sign (in 'subWith'), for an infinity
  times a zero (in 'mulWith' and 'fmaWith'), for zero divided by zero and
  infinity divided by infinity, and for the square root, the reciprocal square
  root and the logarithm of a number below zero (not of negative zero).
  'toS32With' raises it for NaN and the infinities.
  'compareWith' and 'abs' never raise it, not even for signalling NaNs.

* __Divide by zero__ is raised by 'divWith' if @x@ is finite and not zero and
  @y@ is a zero, and by 'recipWith', 'rsqrtWith' and 'logWith' if @x@ is a
  zero. The result is then an infinity.

* __Overflow__ is raised if the result is an infinity while all inputs are
  finite and there was no division by zero: the result rounded beyond the
  largest finite number. 'toS32With' raises it if @x@ rounds to a number out
  of range, that is if @x >= 2^31 - 1/2@ or @x < -2^31 - 1/2@, which includes
  the infinities (which also raise invalid operation).

* __Underflow__ is raised if the result is a zero while all inputs are finite
  but the exact result is not zero: the IP flushed a result below the smallest
  normal number to zero. It is not raised for exact results that round up to
  the smallest normal number. Per operation, if the result is a zero and all
  inputs are finite, underflow is raised by: 'addWith' if @x /= negate y@;
  'subWith' if @x /= y@; 'mulWith' if @x /= 0@ and @y /= 0@; 'divWith' if
  @x /= 0@; 'fmaWith' if the exact @x * y + z@ is not zero, which is cheap to
  tell only if @z@ is zero (then like 'mulWith'); 'recipWith' and 'expWith'
  always; and 'convertWith' to a narrower precision if @x /= 0@. The square
  root, the reciprocal square root and the logarithm never underflow.

* The conversions from integers, 'compareWith' and 'abs' never raise any flag,
  and neither do conversions to the same or a wider precision.
-}

module Clash.Cores.Xilinx.Floating.Generic
  ( -- * Precisions
    E.XilinxFloat(..)
  , E.FromInt32
  , E.FromInt64
  , E.ToInt32
    -- * Instantiating IP
  , addWith
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

import qualified Clash.Cores.Xilinx.Floating.Generic.Explicit as E

-- | Customizable floating point addition.
addWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + d) a
addWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.addWith cfg
{-# INLINE addWith #-}

-- | Floating point addition with default settings.
add
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.AddDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + E.AddDefDelay a) a
add = withFrozenCallStack $ hideEnable $ hideClock E.add
{-# INLINE add #-}

-- | Customizable floating point subtraction.
subWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + d) a
subWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.subWith cfg
{-# INLINE subWith #-}

-- | Floating point subtraction with default settings.
sub
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.SubDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + E.SubDefDelay a) a
sub = withFrozenCallStack $ hideEnable $ hideClock E.sub
{-# INLINE sub #-}

-- | Customizable floating point multiplication.
mulWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + d) a
mulWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.mulWith cfg
{-# INLINE mulWith #-}

-- | Floating point multiplication with default settings.
mul
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.MulDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + E.MulDefDelay a) a
mul = withFrozenCallStack $ hideEnable $ hideClock E.mul
{-# INLINE mul #-}

-- | Customizable floating point division.
divWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + d) a
divWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.divWith cfg
{-# INLINE divWith #-}

-- | Floating point division with default settings.
div
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.DivDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + E.DivDefDelay a) a
div = withFrozenCallStack $ hideEnable $ hideClock E.div
{-# INLINE div #-}

-- | Customizable floating point comparison
--
-- Produces 'Clash.Cores.Xilinx.Floating.Generic.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
compareWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + d) E.Ordering
compareWith = E.compareWith hasClock hasEnable
{-# INLINE compareWith #-}

-- | Floating point comparison, with default delay
--
-- Produces 'Clash.Cores.Xilinx.Floating.Generic.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
compare
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.CompareDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + E.CompareDefDelay a) E.Ordering
compare = compareWith
{-# INLINE compare #-}

-- | Customizable conversion of @Unsigned 32@ to a floating point number
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU32With
  :: forall a d dom n
   . ( E.FromInt32 a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Unsigned 32)
  -> DSignal dom (n + d) a
fromU32With = withFrozenCallStack $ hideEnable $ hideClock E.fromU32With
{-# INLINE fromU32With #-}

-- | Conversion of @Unsigned 32@ to a floating point number, with default delay
fromU32
  :: forall a dom n
   . ( E.FromInt32 a
     , KnownNat (E.FromU32DefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Unsigned 32)
  -> DSignal dom (n + E.FromU32DefDelay a) a
fromU32 = withFrozenCallStack $ hideEnable $ hideClock E.fromU32
{-# INLINE fromU32 #-}

-- | Customizable conversion of @Signed 32@ to a floating point number
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS32With
  :: forall a d dom n
   . ( E.FromInt32 a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Signed 32)
  -> DSignal dom (n + d) a
fromS32With = withFrozenCallStack $ hideEnable $ hideClock E.fromS32With
{-# INLINE fromS32With #-}

-- | Conversion of @Signed 32@ to a floating point number, with default delay
fromS32
  :: forall a dom n
   . ( E.FromInt32 a
     , KnownNat (E.FromS32DefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Signed 32)
  -> DSignal dom (n + E.FromS32DefDelay a) a
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
-- Only the @bMemUsage@ setting of the
-- t'Clash.Cores.Xilinx.Floating.Generic.Config' matters, and only in half
-- precision: with 'Clash.Cores.Xilinx.Floating.Generic.FullBMemUsage' the IP
-- uses block memory, which lowers its maximum latency but needs a latency of
-- at least one.
sqrtWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom (n + d) a
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
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.SqrtDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + E.SqrtDefDelay a) a
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
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownDomain dom
     )
  => DSignal dom n a
  -> DSignal dom n a
abs = E.abs
{-# INLINE abs #-}

-- | Customizable conversion of @Unsigned 64@ to a floating point number. It
-- rounds to nearest, ties to even.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU64With
  :: forall a d dom n
   . ( E.FromInt64 a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Unsigned 64)
  -> DSignal dom (n + d) a
fromU64With = withFrozenCallStack $ hideEnable $ hideClock E.fromU64With
{-# INLINE fromU64With #-}

-- | Conversion of @Unsigned 64@ to a floating point number, with default delay.
-- It rounds to nearest, ties to even.
fromU64
  :: forall a dom n
   . ( E.FromInt64 a
     , KnownNat (E.FromU64DefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Unsigned 64)
  -> DSignal dom (n + E.FromU64DefDelay a) a
fromU64 = withFrozenCallStack $ hideEnable $ hideClock E.fromU64
{-# INLINE fromU64 #-}

-- | Customizable conversion of @Signed 64@ to a floating point number. It
-- rounds to nearest, ties to even.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS64With
  :: forall a d dom n
   . ( E.FromInt64 a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n (Signed 64)
  -> DSignal dom (n + d) a
fromS64With = withFrozenCallStack $ hideEnable $ hideClock E.fromS64With
{-# INLINE fromS64With #-}

-- | Conversion of @Signed 64@ to a floating point number, with default delay.
-- It rounds to nearest, ties to even.
fromS64
  :: forall a dom n
   . ( E.FromInt64 a
     , KnownNat (E.FromS64DefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n (Signed 64)
  -> DSignal dom (n + E.FromS64DefDelay a) a
fromS64 = withFrozenCallStack $ hideEnable $ hideClock E.fromS64
{-# INLINE fromS64 #-}

-- | Customizable conversion of a floating point number to @Signed 32@.
--
-- It rounds to nearest, ties to even, so @2.5@ becomes @2@ and @3.5@ becomes
-- @4@. Numbers that are out of range after rounding, including infinities,
-- saturate to 'minBound' or 'maxBound'. Every NaN becomes 'minBound'.
-- Subnormal numbers become zero.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toS32With
  :: forall a d dom n
   . ( E.ToInt32 a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + d) (Signed 32)
toS32With = withFrozenCallStack $ hideEnable $ hideClock E.toS32With
{-# INLINE toS32With #-}

-- | Conversion of a floating point number to @Signed 32@, with default delay.
--
-- It rounds to nearest, ties to even, so @2.5@ becomes @2@ and @3.5@ becomes
-- @4@. Numbers that are out of range after rounding, including infinities,
-- saturate to 'minBound' or 'maxBound'. Every NaN becomes 'minBound'.
-- Subnormal numbers become zero.
toS32
  :: forall a dom n
   . ( E.ToInt32 a
     , KnownNat (E.ToS32DefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + E.ToS32DefDelay a) (Signed 32)
toS32 = withFrozenCallStack $ hideEnable $ hideClock E.toS32
{-# INLINE toS32 #-}

-- | Customizable conversion of a floating point number in precision @a@ to
-- precision @b@. The precisions can be the same.
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
  :: forall a b d dom n
   . ( E.XilinxFloat a
     , E.XilinxFloat b
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + d) b
convertWith = withFrozenCallStack $ hideEnable $ hideClock E.convertWith
{-# INLINE convertWith #-}

-- | Conversion of a floating point number in precision @a@ to precision @b@,
-- with default delay. The precisions can be the same.
--
-- Conversions to a narrower precision round to nearest, ties to even. Results
-- below the smallest normal number of the target precision become a zero of
-- the same sign, results beyond its largest finite number an infinity of the
-- same sign. Other conversions are exact. Like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates, also when converting to the same precision.
convert
  :: forall a b dom n
   . ( E.XilinxFloat a
     , E.XilinxFloat b
     , KnownNat (E.ConvertDefDelay a b)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + E.ConvertDefDelay a b) b
convert = withFrozenCallStack $ hideEnable $ hideClock E.convert
{-# INLINE convert #-}

-- | Customizable floating point reciprocal, @1 / x@.
--
-- The reciprocal of a zero is an infinity of the same sign, and that of an
-- infinity a zero of the same sign. Like the other operations, the IP treats
-- subnormal inputs as a zero of the same sign and flushes results below the
-- smallest normal number to a zero of the same sign.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- reciprocal rounded to nearest, ties to even. The IP is only accurate to one
-- unit in the last place in single and double precision; in half precision it
-- rounds correctly too. See
-- 'Clash.Cores.Xilinx.Floating.Generic.Explicit.recipWith' for how often they
-- differ and which settings matter.
recipWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom (n + d) a
recipWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.recipWith cfg
{-# INLINE recipWith #-}

-- | Floating point reciprocal, @1 / x@, with default settings.
--
-- The reciprocal of a zero is an infinity of the same sign, and that of an
-- infinity a zero of the same sign. Like the other operations, the IP treats
-- subnormal inputs as a zero of the same sign and flushes results below the
-- smallest normal number to a zero of the same sign.
--
-- Clash simulation gives the correctly rounded result, while the IP in single
-- and double precision is only accurate to one unit in the last place, so they
-- can differ in the last bit. See 'recipWith'.
recip
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.RecipDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + E.RecipDefDelay a) a
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
-- reciprocal square root rounded to nearest, ties to even. The IP is only
-- accurate to one unit in the last place in single and double precision; in
-- half precision it rounds correctly too. See
-- 'Clash.Cores.Xilinx.Floating.Generic.Explicit.rsqrtWith' for how often they
-- differ and which settings matter.
rsqrtWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom (n + d) a
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
-- Clash simulation gives the correctly rounded result, while the IP in single
-- and double precision is only accurate to one unit in the last place, so they
-- can differ in the last bit. See 'rsqrtWith'.
rsqrt
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.RSqrtDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + E.RSqrtDefDelay a) a
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
-- unit in the last place, so the hardware can differ from the simulation in
-- the last bit. See 'Clash.Cores.Xilinx.Floating.Generic.Explicit.expWith' for
-- how often they differ and which settings matter.
expWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom (n + d) a
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
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.ExpDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + E.ExpDefDelay a) a
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
-- unit in the last place, so the hardware can differ from the simulation in
-- the last bit. See 'Clash.Cores.Xilinx.Floating.Generic.Explicit.logWith' for
-- how often they differ and which settings matter.
logWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom (n + d) a
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
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.LogDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom (n + E.LogDefDelay a) a
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
-- 'Clash.Cores.Xilinx.Floating.Generic.Explicit.fmaWith' for the details and
-- which settings matter.
fmaWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + d) a
fmaWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.fmaWith cfg
{-# INLINE fmaWith #-}

-- | Fused multiply-add with default settings: @fma x y z@ computes
-- @x * y + z@ with a single rounding, like C's @fma@. See 'fmaWith'.
fma
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.FmaDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom n a
  -> DSignal dom (n + E.FmaDefDelay a) a
fma = withFrozenCallStack $ hideEnable $ hideClock E.fma
{-# INLINE fma #-}

-- | Customizable accumulator: @accumWith cfg lst x@ gives the running sum of
-- the inputs @x@. An input with @lst@ set is the last one of its
-- accumulation: its result is the complete sum, and the next input starts a
-- new accumulation. De-asserting the 'Enable' stalls the IP: the input of that
-- cycle is ignored.
--
-- The IP adds in fixed point, with the bounds of the
-- t'Clash.Cores.Xilinx.Floating.Generic.AccumConfig': inputs are truncated
-- towards zero to a multiple of @2^accumLsb@ and added exactly, and the result
-- is the sum rounded __towards zero__. Inputs whose magnitude reaches
-- @2^(accumInputMsb + 1)@ and sums whose magnitude reaches
-- @2^(accumMsb + 1)@ make the result NaN until the end of the accumulation.
-- See 'Clash.Cores.Xilinx.Floating.Generic.Explicit.accumWith' for the details
-- and the bounds the IP accepts.
accumWith
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , 1 <= d
     , HasCallStack
     )
  => E.AccumConfig
  -> DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n a
  -> DSignal dom (n + d) a
accumWith cfg = withFrozenCallStack $ hideEnable . hideClock $ E.accumWith cfg
{-# INLINE accumWith #-}

-- | Accumulator with the IP's default settings
-- ('Clash.Cores.Xilinx.Floating.Generic.defAccumConfig'). See 'accumWith'.
accum
  :: forall a dom n
   . ( E.XilinxFloat a
     , KnownNat (E.AccumDefDelay a)
     , HiddenClock dom
     , HiddenEnable dom
     , HasCallStack
     )
  => DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n a
  -> DSignal dom (n + E.AccumDefDelay a) a
accum = withFrozenCallStack $ hideEnable $ hideClock E.accum
{-# INLINE accum #-}

-- | Customizable accumulator that also outputs the IP's exception flags, see
-- t'Clash.Cores.Xilinx.Floating.Generic.AccumFlags' and
-- 'Clash.Cores.Xilinx.Floating.Generic.Explicit.accumWithFlags'.
accumWithFlags
  :: forall a d dom n
   . ( E.XilinxFloat a
     , HiddenClock dom
     , HiddenEnable dom
     , KnownNat d
     , 1 <= d
     , HasCallStack
     )
  => E.AccumConfig
  -> DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n a
  -> DSignal dom (n + d) (a, E.AccumFlags)
accumWithFlags cfg =
  withFrozenCallStack $ hideEnable . hideClock $ E.accumWithFlags cfg
{-# INLINE accumWithFlags #-}
