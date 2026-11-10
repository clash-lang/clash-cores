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
  , recipWith
  , recip
  , E.RecipDefDelay
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
  , E.recipModel
  ) where

import Clash.Prelude hiding (add, sub, mul, div, sqrt, recip)

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
