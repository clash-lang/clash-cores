{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's CORDIC IP (@ALTERA_CORDIC@), as described in the
[ALTERA_CORDIC IP Core User Guide (UG-20017)](https://www.intel.com/content/www/us/en/docs/programmable/683808/).
This module uses a hidden clock, reset and enable. See
"Clash.Cores.Intel.Cordic.Explicit" for the documentation of the IP's
behavior and how Clash instantiates it.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Cordic
  ( -- * Sine and cosine
    sinCos
  , sinCosUnsigned
    -- * Simulation models
  , E.sinCosModel
  , E.sinCosUnsignedModel
  ) where

import Clash.Prelude

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Intel.Cordic.Explicit as E

-- | Sine and cosine of an angle in radians between -pi and pi. See
-- 'E.sinCos'.
sinCos ::
  forall d dom n fIn fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , fOut <= fIn
  , fOut <= 34
  , HasCallStack
  ) =>
  -- | Angle, in radians
  DSignal dom n (SFixed 3 fIn) ->
  -- | Cosine and sine
  DSignal dom (n + d) (SFixed 2 fOut, SFixed 2 fOut)
sinCos = withFrozenCallStack $ hideClockResetEnable (E.sinCos @d)
{-# INLINE sinCos #-}

-- | Sine and cosine of an angle in radians between 0 and pi\/2. See
-- 'E.sinCosUnsigned'.
sinCosUnsigned ::
  forall d dom n fIn fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , fOut <= 34
  , HasCallStack
  ) =>
  -- | Angle, in radians
  DSignal dom n (UFixed 1 fIn) ->
  -- | Cosine and sine
  DSignal dom (n + d) (UFixed 1 fOut, UFixed 1 fOut)
sinCosUnsigned = withFrozenCallStack $ hideClockResetEnable (E.sinCosUnsigned @d)
{-# INLINE sinCosUnsigned #-}
