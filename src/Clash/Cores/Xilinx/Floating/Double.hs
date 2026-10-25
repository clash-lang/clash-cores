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

+----------------------+---------------------------------------+------------+---------------+
| Function             | Configuration                         | 7-series   | UltraScale(+) |
+======================+=======================================+============+===============+
| 'addWith', 'subWith' | @SpeedArch@, @NoDspUsage@             | 12         | 12            |
+----------------------+---------------------------------------+------------+---------------+
| 'addWith', 'subWith' | @SpeedArch@, @MediumDspUsage@         | /rejected/ | /rejected/    |
+----------------------+---------------------------------------+------------+---------------+
| 'addWith', 'subWith' | @SpeedArch@, @FullDspUsage@ (default) | 14         | 14            |
+----------------------+---------------------------------------+------------+---------------+
| 'addWith', 'subWith' | @SpeedArch@, @MaxDspUsage@            | /rejected/ | /rejected/    |
+----------------------+---------------------------------------+------------+---------------+
| 'addWith', 'subWith' | @LatencyArch@, any @DspUsage@         | 8          | 8             |
+----------------------+---------------------------------------+------------+---------------+
| 'mulWith'            | @NoDspUsage@                          | 9          | 9             |
+----------------------+---------------------------------------+------------+---------------+
| 'mulWith'            | @MediumDspUsage@                      | 15         | 12            |
+----------------------+---------------------------------------+------------+---------------+
| 'mulWith'            | @FullDspUsage@ (default)              | 15         | 12            |
+----------------------+---------------------------------------+------------+---------------+
| 'mulWith'            | @MaxDspUsage@                         | 16         | 13            |
+----------------------+---------------------------------------+------------+---------------+
| 'divWith'            | any                                   | 57         | 57            |
+----------------------+---------------------------------------+------------+---------------+
| 'compareWith'        |                                       | 2          | 2             |
+----------------------+---------------------------------------+------------+---------------+
| 'fromU32With'        |                                       | 5          | 5             |
+----------------------+---------------------------------------+------------+---------------+
| 'fromS32With'        |                                       | 6          | 6             |
+----------------------+---------------------------------------+------------+---------------+

/rejected/ means that Vivado rejects the configuration itself, whatever the
latency. 'mulWith' ignores the @archOpt@ setting. With @LatencyArch@, Vivado
ignores the @dspUsage@ setting of 'addWith' and 'subWith'. The measured
7-series devices are Artix-7, Kintex-7 and Spartan-7; the UltraScale and
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
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.ArchOpt(..)
  , E.DspUsage(..)
  , E.BMemUsage(..)
    -- * Additional functions
  , E.xilinxNaN
  ) where

import Clash.Prelude hiding (Ordering(..), add, sub, mul, div, compare)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Xilinx.Floating.Double.Explicit as E

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
