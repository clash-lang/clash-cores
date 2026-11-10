{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's Floating Point Functions IP (@altera_fp_functions@), as
described in the
[Floating-Point IP Cores User Guide (UG-01058)](https://www.intel.com/content/www/us/en/docs/programmable/683750/).
This module uses a hidden clock, reset and enable. See
"Clash.Cores.Intel.Floating.Explicit" for the documentation of the IP's
behavior and how Clash instantiates it.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Floating
  ( -- * Arithmetic
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
  , mulAddWith
  , mulAdd
  , E.MulAddDefDelay
    -- * Functions
  , E.DeviceFamily(..)
  , sqrtWith
  , sqrt
  , E.SqrtDefDelay
  , cbrtWith
  , cbrt
  , E.CbrtDefDelay
  , hypotWith
  , hypot
  , E.HypotDefDelay
  , recipWith
  , recip
  , E.RecipDefDelay
  , rsqrtWith
  , rsqrt
  , E.RsqrtDefDelay
  , expWith
  , exp
  , E.ExpDefDelay
  , exp2With
  , exp2
  , E.Exp2DefDelay
  , exp10With
  , exp10
  , E.Exp10DefDelay
  , logWith
  , log
  , E.LogDefDelay
  , log2With
  , log2
  , E.Log2DefDelay
  , log10With
  , log10
  , E.Log10DefDelay
  , log1pWith
  , log1p
  , E.Log1pDefDelay
    -- * Comparison
  , E.Ordering(..)
  , E.toMaybeOrdering
  , compareWith
  , compare
  , E.CompareDefDelay
  , E.Predicate(..)
  , predicateWith
  , predicate
    -- * Conversion
  , fromS32With
  , fromS32
  , E.FromS32DefDelay
  , fromU32With
  , fromU32
  , E.FromU32DefDelay
  , toS32With
  , toS32
  , E.ToS32DefDelay
  , toU32With
  , toU32
  , E.ToU32DefDelay
  , toDoubleWith
  , toDouble
  , E.ToDoubleDefDelay
  , fromDoubleWith
  , fromDouble
  , E.FromDoubleDefDelay
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
    -- * Simulation models
  , E.intelNaN
  ) where

import Clash.Prelude hiding (Ordering(..), add, sub, mul, div, compare, sqrt, recip, exp, log)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Intel.Floating.Explicit as E

-- | Customizable floating point addition. See 'E.addWith'.
addWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addWith cfg = withFrozenCallStack $ hideClockResetEnable (E.addWith cfg)
{-# INLINE addWith #-}

-- | Floating point addition, with the default configuration and delay. See
-- 'E.add'.
add
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.AddDefDelay) Float
add = withFrozenCallStack $ hideClockResetEnable E.add
{-# INLINE add #-}

-- | Customizable floating point subtraction. See 'E.subWith'.
subWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
subWith cfg = withFrozenCallStack $ hideClockResetEnable (E.subWith cfg)
{-# INLINE subWith #-}

-- | Floating point subtraction, with the default configuration and delay. See
-- 'E.sub'.
sub
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.SubDefDelay) Float
sub = withFrozenCallStack $ hideClockResetEnable E.sub
{-# INLINE sub #-}

-- | Customizable floating point multiplication. See 'E.mulWith'.
mulWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulWith cfg = withFrozenCallStack $ hideClockResetEnable (E.mulWith cfg)
{-# INLINE mulWith #-}

-- | Floating point multiplication, with the default configuration and delay. See
-- 'E.mul'.
mul
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.MulDefDelay) Float
mul = withFrozenCallStack $ hideClockResetEnable E.mul
{-# INLINE mul #-}

-- | Customizable floating point division. See 'E.divWith'.
divWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divWith cfg = withFrozenCallStack $ hideClockResetEnable (E.divWith cfg)
{-# INLINE divWith #-}

-- | Floating point division, with the default configuration and delay. See
-- 'E.div'.
div
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.DivDefDelay) Float
div = withFrozenCallStack $ hideClockResetEnable E.div
{-# INLINE div #-}

-- | Customizable floating point multiply-add, @a * b + c@. See 'E.mulAddWith'.
mulAddWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulAddWith cfg = withFrozenCallStack $ hideClockResetEnable (E.mulAddWith cfg)
{-# INLINE mulAddWith #-}

-- | Floating point multiply-add, @a * b + c@, with the default configuration and delay. See
-- 'E.mulAdd'.
mulAdd
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.MulAddDefDelay) Float
mulAdd = withFrozenCallStack $ hideClockResetEnable E.mulAdd
{-# INLINE mulAdd #-}

-- | Customizable floating point square root. See 'E.sqrtWith'.
sqrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
sqrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.sqrtWith cfg fam)
{-# INLINE sqrtWith #-}

-- | Floating point square root, with the default configuration and delay. See
-- 'E.sqrt'.
sqrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.SqrtDefDelay) Float
sqrt fam = withFrozenCallStack $ hideClockResetEnable (E.sqrt fam)
{-# INLINE sqrt #-}

-- | Customizable floating point cube root. See 'E.cbrtWith'.
cbrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
cbrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.cbrtWith cfg fam)
{-# INLINE cbrtWith #-}

-- | Floating point cube root, with the default configuration and delay. See
-- 'E.cbrt'.
cbrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.CbrtDefDelay) Float
cbrt fam = withFrozenCallStack $ hideClockResetEnable (E.cbrt fam)
{-# INLINE cbrt #-}

-- | Customizable floating point three-dimensional hypotenuse. See
-- 'E.hypotWith'.
hypotWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
hypotWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.hypotWith cfg fam)
{-# INLINE hypotWith #-}

-- | Floating point three-dimensional hypotenuse, with the default
-- configuration and delay. See 'E.hypot'.
hypot
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.HypotDefDelay) Float
hypot fam = withFrozenCallStack $ hideClockResetEnable (E.hypot fam)
{-# INLINE hypot #-}

-- | Customizable floating point reciprocal. See 'E.recipWith'.
recipWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
recipWith cfg = withFrozenCallStack $ hideClockResetEnable (E.recipWith cfg)
{-# INLINE recipWith #-}

-- | Floating point reciprocal, with the default configuration and delay. See
-- 'E.recip'.
recip
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.RecipDefDelay) Float
recip = withFrozenCallStack $ hideClockResetEnable (E.recip)
{-# INLINE recip #-}

-- | Customizable floating point reciprocal square root. See 'E.rsqrtWith'.
rsqrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
rsqrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.rsqrtWith cfg fam)
{-# INLINE rsqrtWith #-}

-- | Floating point reciprocal square root, with the default configuration and delay. See
-- 'E.rsqrt'.
rsqrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.RsqrtDefDelay) Float
rsqrt fam = withFrozenCallStack $ hideClockResetEnable (E.rsqrt fam)
{-# INLINE rsqrt #-}

-- | Customizable floating point exponential. See 'E.expWith'.
expWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
expWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.expWith cfg fam)
{-# INLINE expWith #-}

-- | Floating point exponential, with the default configuration and delay. See
-- 'E.exp'.
exp
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.ExpDefDelay) Float
exp fam = withFrozenCallStack $ hideClockResetEnable (E.exp fam)
{-# INLINE exp #-}

-- | Customizable floating point base-2 exponential. See 'E.exp2With'.
exp2With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
exp2With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.exp2With cfg fam)
{-# INLINE exp2With #-}

-- | Floating point base-2 exponential, with the default configuration and
-- delay. See 'E.exp2'.
exp2
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.Exp2DefDelay) Float
exp2 fam = withFrozenCallStack $ hideClockResetEnable (E.exp2 fam)
{-# INLINE exp2 #-}

-- | Customizable floating point base-10 exponential. See 'E.exp10With'.
exp10With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
exp10With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.exp10With cfg fam)
{-# INLINE exp10With #-}

-- | Floating point base-10 exponential, with the default configuration and
-- delay. See 'E.exp10'.
exp10
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.Exp10DefDelay) Float
exp10 fam = withFrozenCallStack $ hideClockResetEnable (E.exp10 fam)
{-# INLINE exp10 #-}

-- | Customizable floating point natural logarithm. See 'E.logWith'.
logWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
logWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.logWith cfg fam)
{-# INLINE logWith #-}

-- | Floating point natural logarithm, with the default configuration and delay. See
-- 'E.log'.
log
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.LogDefDelay) Float
log fam = withFrozenCallStack $ hideClockResetEnable (E.log fam)
{-# INLINE log #-}

-- | Customizable floating point base 2 logarithm. See 'E.log2With'.
log2With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
log2With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log2With cfg fam)
{-# INLINE log2With #-}

-- | Floating point base 2 logarithm, with the default configuration and delay. See
-- 'E.log2'.
log2
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.Log2DefDelay) Float
log2 fam = withFrozenCallStack $ hideClockResetEnable (E.log2 fam)
{-# INLINE log2 #-}

-- | Customizable floating point base 10 logarithm. See 'E.log10With'.
log10With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
log10With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log10With cfg fam)
{-# INLINE log10With #-}

-- | Floating point base 10 logarithm, with the default configuration and delay. See
-- 'E.log10'.
log10
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.Log10DefDelay) Float
log10 fam = withFrozenCallStack $ hideClockResetEnable (E.log10 fam)
{-# INLINE log10 #-}

-- | Customizable floating point natural logarithm of one plus its argument, @log (1 + x)@. See 'E.log1pWith'.
log1pWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
log1pWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log1pWith cfg fam)
{-# INLINE log1pWith #-}

-- | Floating point natural logarithm of one plus its argument, @log (1 + x)@, with the default configuration and delay. See
-- 'E.log1p'.
log1p
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Float
  -> DSignal dom (n + E.Log1pDefDelay) Float
log1p fam = withFrozenCallStack $ hideClockResetEnable (E.log1p fam)
{-# INLINE log1p #-}

-- | Customizable floating point conversion of @Signed 32@ to @Float@. See 'E.fromS32With'.
fromS32With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n (Signed 32)
  -> DSignal dom (n + d) Float
fromS32With cfg = withFrozenCallStack $ hideClockResetEnable (E.fromS32With cfg)
{-# INLINE fromS32With #-}

-- | Floating point conversion of @Signed 32@ to @Float@, with the default configuration and delay. See
-- 'E.fromS32'.
fromS32
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n (Signed 32)
  -> DSignal dom (n + E.FromS32DefDelay) Float
fromS32 = withFrozenCallStack $ hideClockResetEnable E.fromS32
{-# INLINE fromS32 #-}

-- | Customizable floating point conversion of @Unsigned 32@ to @Float@. See 'E.fromU32With'.
fromU32With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n (Unsigned 32)
  -> DSignal dom (n + d) Float
fromU32With cfg = withFrozenCallStack $ hideClockResetEnable (E.fromU32With cfg)
{-# INLINE fromU32With #-}

-- | Floating point conversion of @Unsigned 32@ to @Float@, with the default configuration and delay. See
-- 'E.fromU32'.
fromU32
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n (Unsigned 32)
  -> DSignal dom (n + E.FromU32DefDelay) Float
fromU32 = withFrozenCallStack $ hideClockResetEnable E.fromU32
{-# INLINE fromU32 #-}

-- | Customizable floating point conversion of @Float@ to @Signed 32@. See 'E.toS32With'.
toS32With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom (n + d) (Signed 32)
toS32With cfg = withFrozenCallStack $ hideClockResetEnable (E.toS32With cfg)
{-# INLINE toS32With #-}

-- | Floating point conversion of @Float@ to @Signed 32@, with the default configuration and delay. See
-- 'E.toS32'.
toS32
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.ToS32DefDelay) (Signed 32)
toS32 = withFrozenCallStack $ hideClockResetEnable E.toS32
{-# INLINE toS32 #-}

-- | Customizable floating point conversion of @Float@ to @Unsigned 32@. See 'E.toU32With'.
toU32With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom (n + d) (Unsigned 32)
toU32With cfg = withFrozenCallStack $ hideClockResetEnable (E.toU32With cfg)
{-# INLINE toU32With #-}

-- | Floating point conversion of @Float@ to @Unsigned 32@, with the default configuration and delay. See
-- 'E.toU32'.
toU32
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.ToU32DefDelay) (Unsigned 32)
toU32 = withFrozenCallStack $ hideClockResetEnable E.toU32
{-# INLINE toU32 #-}

-- | Customizable comparison of floating point numbers. See 'E.predicateWith'.
predicateWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.Predicate
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Bool
predicateWith cfg p = withFrozenCallStack $ hideClockResetEnable (E.predicateWith cfg p)
{-# INLINE predicateWith #-}

-- | Comparison of floating point numbers with the default configuration and
-- delay. See 'E.predicate'.
predicate
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.Predicate
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.CompareDefDelay) Bool
predicate p = withFrozenCallStack $ hideClockResetEnable (E.predicate p)
{-# INLINE predicate #-}

-- | Customizable comparison of floating point numbers. See 'E.compareWith'.
compareWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) E.Ordering
compareWith cfg = withFrozenCallStack $ hideClockResetEnable (E.compareWith cfg)
{-# INLINE compareWith #-}

-- | Comparison of floating point numbers with the default configuration and
-- delay. See 'E.compare'.
compare
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + E.CompareDefDelay) E.Ordering
compare = withFrozenCallStack $ hideClockResetEnable E.compare
{-# INLINE compare #-}

-- | Customizable conversion of 'Float' to 'Double'. See 'E.toDoubleWith'.
toDoubleWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Float
  -> DSignal dom (n + d) Double
toDoubleWith cfg = withFrozenCallStack $ hideClockResetEnable (E.toDoubleWith cfg)
{-# INLINE toDoubleWith #-}

-- | Conversion of 'Float' to 'Double', with the default configuration and delay. See
-- 'E.toDouble'.
toDouble
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Float
  -> DSignal dom (n + E.ToDoubleDefDelay) Double
toDouble = withFrozenCallStack $ hideClockResetEnable E.toDouble
{-# INLINE toDouble #-}

-- | Customizable conversion of 'Double' to 'Float'. See 'E.fromDoubleWith'.
fromDoubleWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Double
  -> DSignal dom (n + d) Float
fromDoubleWith cfg = withFrozenCallStack $ hideClockResetEnable (E.fromDoubleWith cfg)
{-# INLINE fromDoubleWith #-}

-- | Conversion of 'Double' to 'Float', with the default configuration and delay. See
-- 'E.fromDouble'.
fromDouble
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom (n + E.FromDoubleDefDelay) Float
fromDouble = withFrozenCallStack $ hideClockResetEnable E.fromDouble
{-# INLINE fromDouble #-}
