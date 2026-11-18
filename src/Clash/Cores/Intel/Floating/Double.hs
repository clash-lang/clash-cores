{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's Floating Point Functions IP (@altera_fp_functions@) in
double precision ('Double'), as described in the
[Floating-Point IP Cores User Guide (UG-01058)](https://www.intel.com/content/www/us/en/docs/programmable/683750/).
This module uses a hidden clock, reset and enable. See
"Clash.Cores.Intel.Floating.Double.Explicit" for the documentation of the IP's
behavior in double precision, and "Clash.Cores.Intel.Floating.Explicit" for
how Clash instantiates it.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Floating.Double
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
    -- * Functions
  , E.DeviceFamily(..)
  , sqrtWith
  , sqrt
  , E.SqrtDefDelay
  , rsqrtWith
  , rsqrt
  , E.RsqrtDefDelay
  , recipWith
  , recip
  , E.RecipDefDelay
  , expWith
  , exp
  , E.ExpDefDelay
  , hypotWith
  , hypot
  , E.HypotDefDelay
  , log10With
  , log10
  , E.Log10DefDelay
  , cbrtWith
  , cbrt
  , E.CbrtDefDelay
  , logWith
  , log
  , E.LogDefDelay
  , log1pWith
  , log1p
  , E.Log1pDefDelay
  , log2With
  , log2
  , E.Log2DefDelay
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
    -- * Simulation models
  , E.intelNaN
  , E.addModel
  , E.subModel
  , E.mulModel
  , E.divModel
  , E.sqrtModel
  , E.rsqrtModel
  , E.recipModel
  , E.expModel
  , E.hypotModel
  , E.log10Model
  , E.cbrtModel
  , E.logModel
  , E.log1pModel
  , E.log2Model
  ) where

import Clash.Prelude hiding (add, sub, mul, div, sqrt, recip, exp, log)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Intel.Floating.Double.Explicit as E

-- | Customizable floating point addition, in double precision. See
-- 'E.addWith'.
addWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
addWith cfg = withFrozenCallStack $ hideClockResetEnable (E.addWith cfg)
{-# INLINE addWith #-}

-- | Floating point addition, in double precision, with the default
-- configuration and delay. See 'E.add'.
add
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.AddDefDelay) Double
add = withFrozenCallStack $ hideClockResetEnable E.add
{-# INLINE add #-}

-- | Customizable floating point subtraction, in double precision. See
-- 'E.subWith'.
subWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
subWith cfg = withFrozenCallStack $ hideClockResetEnable (E.subWith cfg)
{-# INLINE subWith #-}

-- | Floating point subtraction, in double precision, with the default
-- configuration and delay. See 'E.sub'.
sub
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.SubDefDelay) Double
sub = withFrozenCallStack $ hideClockResetEnable E.sub
{-# INLINE sub #-}

-- | Customizable floating point multiplication, in double precision. See
-- 'E.mulWith'.
mulWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
mulWith cfg = withFrozenCallStack $ hideClockResetEnable (E.mulWith cfg)
{-# INLINE mulWith #-}

-- | Floating point multiplication, in double precision, with the default
-- configuration and delay. See 'E.mul'.
mul
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.MulDefDelay) Double
mul = withFrozenCallStack $ hideClockResetEnable E.mul
{-# INLINE mul #-}

-- | Customizable floating point division, in double precision. See
-- 'E.divWith'.
divWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
divWith cfg = withFrozenCallStack $ hideClockResetEnable (E.divWith cfg)
{-# INLINE divWith #-}

-- | Floating point division, in double precision, with the default
-- configuration and delay. See 'E.div'.
div
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.DivDefDelay) Double
div = withFrozenCallStack $ hideClockResetEnable E.div
{-# INLINE div #-}

-- | Customizable floating point square root, in double precision. See
-- 'E.sqrtWith'.
sqrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
sqrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.sqrtWith cfg fam)
{-# INLINE sqrtWith #-}

-- | Floating point square root, in double precision, with the default
-- configuration and delay. See 'E.sqrt'.
sqrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + E.SqrtDefDelay) Double
sqrt fam = withFrozenCallStack $ hideClockResetEnable (E.sqrt fam)
{-# INLINE sqrt #-}

-- | Customizable floating point reciprocal square root, @1 / sqrt x@, in
-- double precision. See 'E.rsqrtWith'.
rsqrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
rsqrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.rsqrtWith cfg fam)
{-# INLINE rsqrtWith #-}

-- | Floating point reciprocal square root, @1 / sqrt x@, in double precision,
-- with the default configuration and delay. See 'E.rsqrt'.
rsqrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + E.RsqrtDefDelay) Double
rsqrt fam = withFrozenCallStack $ hideClockResetEnable (E.rsqrt fam)
{-# INLINE rsqrt #-}

-- | Customizable floating point reciprocal, @1 / x@, in double precision. See
-- 'E.recipWith'.
recipWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
recipWith cfg = withFrozenCallStack $ hideClockResetEnable (E.recipWith cfg)
{-# INLINE recipWith #-}

-- | Floating point reciprocal, @1 / x@, in double precision, with the default
-- configuration and delay. See 'E.recip'.
recip
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Double
  -> DSignal dom (n + E.RecipDefDelay) Double
recip = withFrozenCallStack $ hideClockResetEnable E.recip
{-# INLINE recip #-}

-- | Customizable floating point exponential, @e^x@, in double precision. See
-- 'E.expWith'.
expWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
expWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.expWith cfg fam)
{-# INLINE expWith #-}

-- | Floating point exponential, @e^x@, in double precision, with the default
-- configuration and delay. See 'E.exp'.
exp
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + E.ExpDefDelay) Double
exp fam = withFrozenCallStack $ hideClockResetEnable (E.exp fam)
{-# INLINE exp #-}

-- | Customizable floating point three-dimensional hypotenuse,
-- @sqrt (x^2 + y^2 + z^2)@, in double precision. See 'E.hypotWith'.
hypotWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
hypotWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.hypotWith cfg fam)
{-# INLINE hypotWith #-}

-- | Floating point three-dimensional hypotenuse, @sqrt (x^2 + y^2 + z^2)@, in
-- double precision, with the default configuration and delay. See 'E.hypot'.
hypot
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + E.HypotDefDelay) Double
hypot fam = withFrozenCallStack $ hideClockResetEnable (E.hypot fam)
{-# INLINE hypot #-}

-- | Customizable floating point base 10 logarithm, @log10 x@, in double
-- precision. See 'E.log10With'.
log10With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
log10With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log10With cfg fam)
{-# INLINE log10With #-}

-- | Floating point base 10 logarithm, @log10 x@, in double precision, with the
-- default configuration and delay. See 'E.log10'.
log10
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + E.Log10DefDelay) Double
log10 fam = withFrozenCallStack $ hideClockResetEnable (E.log10 fam)
{-# INLINE log10 #-}

-- | Customizable floating point cube root, in double
-- precision. See 'E.cbrtWith'.
cbrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
cbrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.cbrtWith cfg fam)
{-# INLINE cbrtWith #-}

-- | Floating point cube root, in double precision, with the
-- default configuration and delay. See 'E.cbrt'.
cbrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + E.CbrtDefDelay) Double
cbrt fam = withFrozenCallStack $ hideClockResetEnable (E.cbrt fam)
{-# INLINE cbrt #-}

-- | Customizable floating point natural logarithm, in double
-- precision. See 'E.logWith'.
logWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
logWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.logWith cfg fam)
{-# INLINE logWith #-}

-- | Floating point natural logarithm, in double precision, with the
-- default configuration and delay. See 'E.log'.
log
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + E.LogDefDelay) Double
log fam = withFrozenCallStack $ hideClockResetEnable (E.log fam)
{-# INLINE log #-}

-- | Customizable floating point natural logarithm of one plus its argument, @log (1 + x)@, in double
-- precision. See 'E.log1pWith'.
log1pWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
log1pWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log1pWith cfg fam)
{-# INLINE log1pWith #-}

-- | Floating point natural logarithm of one plus its argument, @log (1 + x)@, in double precision, with the
-- default configuration and delay. See 'E.log1p'.
log1p
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + E.Log1pDefDelay) Double
log1p fam = withFrozenCallStack $ hideClockResetEnable (E.log1p fam)
{-# INLINE log1p #-}

-- | Customizable floating point base 2 logarithm, @log2 x@, in double
-- precision. See 'E.log2With'.
log2With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
log2With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log2With cfg fam)
{-# INLINE log2With #-}

-- | Floating point base 2 logarithm, @log2 x@, in double precision, with the
-- default configuration and delay. See 'E.log2'.
log2
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Double
  -> DSignal dom (n + E.Log2DefDelay) Double
log2 fam = withFrozenCallStack $ hideClockResetEnable (E.log2 fam)
{-# INLINE log2 #-}
