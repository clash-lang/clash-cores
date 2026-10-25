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
t'Clash.Cores.Xilinx.Floating.Generic.FromInt32'.

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

+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| Function             | Configuration                         | Half       | Float      | Double, 7-series | Double, UltraScale(+) |
+======================+=======================================+============+============+==================+=======================+
| 'addWith', 'subWith' | @SpeedArch@, @NoDspUsage@             | 8          | 12         | 12               | 12                    |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'addWith', 'subWith' | @SpeedArch@, @MediumDspUsage@         | 11         | 11         | /rejected/       | /rejected/            |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'addWith', 'subWith' | @SpeedArch@, @FullDspUsage@ (default) | 11         | 11         | 14               | 14                    |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'addWith', 'subWith' | @SpeedArch@, @MaxDspUsage@            | /rejected/ | /rejected/ | /rejected/       | /rejected/            |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'addWith', 'subWith' | @LatencyArch@, any @DspUsage@         | 7          | 8          | 8                | 8                     |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'mulWith'            | @NoDspUsage@                          | 6          | 8          | 9                | 9                     |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'mulWith'            | @MediumDspUsage@                      | /rejected/ | 8          | 15               | 12                    |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'mulWith'            | @FullDspUsage@ (default)              | 6          | 8          | 15               | 12                    |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'mulWith'            | @MaxDspUsage@                         | 8          | 6          | 16               | 13                    |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'divWith'            | any                                   | 15         | 28         | 57               | 57                    |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'compareWith'        |                                       | 2          | 2          | 2                | 2                     |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'fromU32With'        |                                       | n/a        | 5          | 5                | 5                     |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+
| 'fromS32With'        |                                       | n/a        | 6          | 6                | 6                     |
+----------------------+---------------------------------------+------------+------------+------------------+-----------------------+

/rejected/ means that Vivado rejects the configuration itself, whatever the
latency. 'mulWith' ignores the @archOpt@ setting. With @LatencyArch@, Vivado
ignores the @dspUsage@ setting of 'addWith' and 'subWith'. Only 'Double'
multiplication has a different maximum on 7-series devices than on UltraScale
and UltraScale+ devices. The measured 7-series devices are Artix-7, Kintex-7
and Spartan-7; the UltraScale and UltraScale+ devices are Kintex UltraScale,
Kintex UltraScale+ and Zynq UltraScale+. Other versions of Vivado might accept
other latencies.

The functions without @With@ use
'Clash.Cores.Xilinx.Floating.Generic.defConfig' and the default delays
(t'Clash.Cores.Xilinx.Floating.Generic.AddDefDelay' and so on). These are the
maximum latencies for that configuration, except for 'mul' of 'Double' on
7-series devices: t'Clash.Cores.Xilinx.Floating.Generic.MulDefDelay' is 12 for
'Double', the maximum on UltraScale and UltraScale+ devices. When changing the
configuration, note that the maximum latency can be lower than the default
delay, for example for 'addWith' with @NoDspUsage@.
-}

module Clash.Cores.Xilinx.Floating.Generic
  ( -- * Precisions
    E.XilinxFloat
  , E.FromInt32
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
