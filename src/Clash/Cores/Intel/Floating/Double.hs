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
  ( -- * Functions
    E.DeviceFamily(..)
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
  , E.sqrtModel
  , E.recipModel
  ) where

import Clash.Prelude hiding (sqrt, recip)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Intel.Floating.Double.Explicit as E

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
