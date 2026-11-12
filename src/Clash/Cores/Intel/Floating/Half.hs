{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's Floating Point Functions IP (@altera_fp_functions@) in
half precision (t'Half'), as described in the
[Floating-Point IP Cores User Guide (UG-01058)](https://www.intel.com/content/www/us/en/docs/programmable/683750/).
This module uses a hidden clock, reset and enable. See
"Clash.Cores.Intel.Floating.Half.Explicit" for the documentation of the IP's
behavior in half precision, and "Clash.Cores.Intel.Floating.Explicit" for how
Clash instantiates it.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Floating.Half
  ( -- * Functions
    E.DeviceFamily(..)
  , sqrtWith
  , sqrt
  , E.SqrtDefDelay
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
  , cbrtWith
  , cbrt
  , E.CbrtDefDelay
  , hypotWith
  , hypot
  , E.HypotDefDelay
  , sinPiWith
  , sinPi
  , E.SinPiDefDelay
  , cosPiWith
  , cosPi
  , E.CosPiDefDelay
  , tanPiWith
  , tanPi
  , E.TanPiDefDelay
  , asinWith
  , asin
  , E.AsinDefDelay
  , asinPiWith
  , asinPi
  , E.AsinPiDefDelay
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
    -- * Simulation models
  , E.intelNaN
  , E.sqrtModel
  , E.recipModel
  , E.rsqrtModel
  , E.expModel
  , E.exp2Model
  , E.exp10Model
  , E.logModel
  , E.log2Model
  , E.log10Model
  , E.log1pModel
  , E.cbrtModel
  , E.hypotModel
  , E.sinPiModel
  , E.cosPiModel
  , E.tanPiModel
  , E.asinModel
  , E.asinPiModel
  ) where

import Clash.Prelude hiding (sqrt, recip, exp, log, asin)

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric.Half (Half)

import qualified Clash.Cores.Intel.Floating.Half.Explicit as E

-- | Customizable floating point square root, in half precision. See
-- 'E.sqrtWith'.
sqrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
sqrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.sqrtWith cfg fam)
{-# INLINE sqrtWith #-}

-- | Floating point square root, in half precision, with the default configuration
-- and delay. See 'E.sqrt'.
sqrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.SqrtDefDelay) Half
sqrt fam = withFrozenCallStack $ hideClockResetEnable (E.sqrt fam)
{-# INLINE sqrt #-}

-- | Customizable floating point reciprocal, @1 / x@, in half precision. See
-- 'E.recipWith'.
recipWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
recipWith cfg = withFrozenCallStack $ hideClockResetEnable (E.recipWith cfg)
{-# INLINE recipWith #-}

-- | Floating point reciprocal, @1 / x@, in half precision, with the default configuration
-- and delay. See 'E.recip'.
recip
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => DSignal dom n Half
  -> DSignal dom (n + E.RecipDefDelay) Half
recip = withFrozenCallStack $ hideClockResetEnable (E.recip)
{-# INLINE recip #-}

-- | Customizable floating point reciprocal square root, @1 / sqrt x@, in half precision. See
-- 'E.rsqrtWith'.
rsqrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
rsqrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.rsqrtWith cfg fam)
{-# INLINE rsqrtWith #-}

-- | Floating point reciprocal square root, @1 / sqrt x@, in half precision, with the default configuration
-- and delay. See 'E.rsqrt'.
rsqrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.RsqrtDefDelay) Half
rsqrt fam = withFrozenCallStack $ hideClockResetEnable (E.rsqrt fam)
{-# INLINE rsqrt #-}

-- | Customizable floating point exponential, @e^x@, in half precision. See
-- 'E.expWith'.
expWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
expWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.expWith cfg fam)
{-# INLINE expWith #-}

-- | Floating point exponential, @e^x@, in half precision, with the default configuration
-- and delay. See 'E.exp'.
exp
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.ExpDefDelay) Half
exp fam = withFrozenCallStack $ hideClockResetEnable (E.exp fam)
{-# INLINE exp #-}

-- | Customizable floating point base-2 exponential, @2^x@, in half precision. See
-- 'E.exp2With'.
exp2With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
exp2With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.exp2With cfg fam)
{-# INLINE exp2With #-}

-- | Floating point base-2 exponential, @2^x@, in half precision, with the default configuration
-- and delay. See 'E.exp2'.
exp2
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.Exp2DefDelay) Half
exp2 fam = withFrozenCallStack $ hideClockResetEnable (E.exp2 fam)
{-# INLINE exp2 #-}

-- | Customizable floating point base-10 exponential, @10^x@, in half precision. See
-- 'E.exp10With'.
exp10With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
exp10With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.exp10With cfg fam)
{-# INLINE exp10With #-}

-- | Floating point base-10 exponential, @10^x@, in half precision, with the default configuration
-- and delay. See 'E.exp10'.
exp10
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.Exp10DefDelay) Half
exp10 fam = withFrozenCallStack $ hideClockResetEnable (E.exp10 fam)
{-# INLINE exp10 #-}

-- | Customizable floating point natural logarithm, in half precision. See
-- 'E.logWith'.
logWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
logWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.logWith cfg fam)
{-# INLINE logWith #-}

-- | Floating point natural logarithm, in half precision, with the default configuration
-- and delay. See 'E.log'.
log
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.LogDefDelay) Half
log fam = withFrozenCallStack $ hideClockResetEnable (E.log fam)
{-# INLINE log #-}

-- | Customizable floating point base 2 logarithm, in half precision. See
-- 'E.log2With'.
log2With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
log2With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log2With cfg fam)
{-# INLINE log2With #-}

-- | Floating point base 2 logarithm, in half precision, with the default configuration
-- and delay. See 'E.log2'.
log2
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.Log2DefDelay) Half
log2 fam = withFrozenCallStack $ hideClockResetEnable (E.log2 fam)
{-# INLINE log2 #-}

-- | Customizable floating point base 10 logarithm, in half precision. See
-- 'E.log10With'.
log10With
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
log10With cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log10With cfg fam)
{-# INLINE log10With #-}

-- | Floating point base 10 logarithm, in half precision, with the default configuration
-- and delay. See 'E.log10'.
log10
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.Log10DefDelay) Half
log10 fam = withFrozenCallStack $ hideClockResetEnable (E.log10 fam)
{-# INLINE log10 #-}

-- | Customizable floating point @log (1 + x)@, in half precision. See
-- 'E.log1pWith'.
log1pWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
log1pWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.log1pWith cfg fam)
{-# INLINE log1pWith #-}

-- | Floating point @log (1 + x)@, in half precision, with the default configuration
-- and delay. See 'E.log1p'.
log1p
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.Log1pDefDelay) Half
log1p fam = withFrozenCallStack $ hideClockResetEnable (E.log1p fam)
{-# INLINE log1p #-}

-- | Customizable floating point cube root, in half precision. See
-- 'E.cbrtWith'.
cbrtWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
cbrtWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.cbrtWith cfg fam)
{-# INLINE cbrtWith #-}

-- | Floating point cube root, in half precision, with the default configuration
-- and delay. See 'E.cbrt'.
cbrt
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.CbrtDefDelay) Half
cbrt fam = withFrozenCallStack $ hideClockResetEnable (E.cbrt fam)
{-# INLINE cbrt #-}

-- | Customizable floating point three-dimensional hypotenuse, @sqrt (x^2 + y^2 + z^2)@, in half precision. See
-- 'E.hypotWith'.
hypotWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
hypotWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.hypotWith cfg fam)
{-# INLINE hypotWith #-}

-- | Floating point three-dimensional hypotenuse, @sqrt (x^2 + y^2 + z^2)@, in half precision, with the default configuration
-- and delay. See 'E.hypot'.
hypot
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + E.HypotDefDelay) Half
hypot fam = withFrozenCallStack $ hideClockResetEnable (E.hypot fam)
{-# INLINE hypot #-}

-- | Customizable floating point sine of pi times x, @sin (pi * x)@, in half precision. See
-- 'E.sinPiWith'.
sinPiWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
sinPiWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.sinPiWith cfg fam)
{-# INLINE sinPiWith #-}

-- | Floating point sine of pi times x, @sin (pi * x)@, in half precision, with the default configuration
-- and delay. See 'E.sinPi'.
sinPi
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.SinPiDefDelay) Half
sinPi fam = withFrozenCallStack $ hideClockResetEnable (E.sinPi fam)
{-# INLINE sinPi #-}

-- | Customizable floating point cosine of pi times x, @cos (pi * x)@, in half precision. See
-- 'E.cosPiWith'.
cosPiWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
cosPiWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.cosPiWith cfg fam)
{-# INLINE cosPiWith #-}

-- | Floating point cosine of pi times x, @cos (pi * x)@, in half precision, with the default configuration
-- and delay. See 'E.cosPi'.
cosPi
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.CosPiDefDelay) Half
cosPi fam = withFrozenCallStack $ hideClockResetEnable (E.cosPi fam)
{-# INLINE cosPi #-}

-- | Customizable floating point tangent of pi times x, @tan (pi * x)@, in half precision. See
-- 'E.tanPiWith'.
tanPiWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
tanPiWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.tanPiWith cfg fam)
{-# INLINE tanPiWith #-}

-- | Floating point tangent of pi times x, @tan (pi * x)@, in half precision, with the default configuration
-- and delay. See 'E.tanPi'.
tanPi
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.TanPiDefDelay) Half
tanPi fam = withFrozenCallStack $ hideClockResetEnable (E.tanPi fam)
{-# INLINE tanPi #-}

-- | Customizable floating point arcsine, @asin x@, in radians, in half precision. See
-- 'E.asinWith'.
asinWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
asinWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.asinWith cfg fam)
{-# INLINE asinWith #-}

-- | Floating point arcsine, @asin x@, in radians, in half precision, with the default configuration
-- and delay. See 'E.asin'.
asin
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.AsinDefDelay) Half
asin fam = withFrozenCallStack $ hideClockResetEnable (E.asin fam)
{-# INLINE asin #-}

-- | Customizable floating point arcsine divided by pi, @asin x / pi@, in half precision. See
-- 'E.asinPiWith'.
asinPiWith
  :: ( HiddenClockResetEnable dom
     , KnownNat d
     , HasCallStack
     )
  => E.Config
  -> E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
asinPiWith cfg fam = withFrozenCallStack $ hideClockResetEnable (E.asinPiWith cfg fam)
{-# INLINE asinPiWith #-}

-- | Floating point arcsine divided by pi, @asin x / pi@, in half precision, with the default configuration
-- and delay. See 'E.asinPi'.
asinPi
  :: ( HiddenClockResetEnable dom
     , HasCallStack
     )
  => E.DeviceFamily
  -> DSignal dom n Half
  -> DSignal dom (n + E.AsinPiDefDelay) Half
asinPi fam = withFrozenCallStack $ hideClockResetEnable (E.asinPi fam)
{-# INLINE asinPi #-}
