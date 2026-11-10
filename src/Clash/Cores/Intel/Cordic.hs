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
    -- * Arctangent
  , atan2
  , atan2Unsigned
    -- * Vector translation
  , vectorTranslate
  , vectorTranslateUnsigned
  , vectorTranslateCompensated
  , vectorTranslateUnsignedCompensated
    -- * Vector rotation
  , vectorRotate
    -- * Simulation models
  , E.sinCosModel
  , E.sinCosUnsignedModel
  , E.atan2Model
  , E.atan2UnsignedModel
  , E.vectorTranslateModel
  , E.vectorTranslateUnsignedModel
  , E.vectorTranslateCompensatedModel
  , E.vectorTranslateUnsignedCompensatedModel
  , E.vectorRotateModel
  ) where

import Clash.Prelude hiding (atan2)

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

-- | The arctangent of @y \/ x@ in radians, in the quadrant of the point @(x,
-- y)@; the first argument is @y@. See 'E.atan2'.
atan2 ::
  forall d dom n i f fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat i
  , KnownNat f
  , KnownNat fOut
  , 1 <= i
  , 1 <= f
  , 4 <= i + f
  , i + f <= 64
  , 1 <= fOut
  , fOut <= 45
  , HasCallStack
  ) =>
  -- | y
  DSignal dom n (SFixed i f) ->
  -- | x
  DSignal dom n (SFixed i f) ->
  -- | atan2(y, x), in radians
  DSignal dom (n + d) (SFixed 3 fOut)
atan2 = withFrozenCallStack $ hideClockResetEnable (E.atan2 @d)
{-# INLINE atan2 #-}

-- | The arctangent of @y \/ x@ in radians for non-negative @x@ and @y@; the
-- first argument is @y@. See 'E.atan2Unsigned'.
atan2Unsigned ::
  forall d dom n i f fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat i
  , KnownNat f
  , KnownNat fOut
  , 1 <= f
  , 4 <= i + f
  , i + f <= 64
  , 1 <= fOut
  , fOut <= 40
  , HasCallStack
  ) =>
  -- | y
  DSignal dom n (UFixed i f) ->
  -- | x
  DSignal dom n (UFixed i f) ->
  -- | atan2(y, x), in radians
  DSignal dom (n + d) (SFixed 2 fOut)
atan2Unsigned = withFrozenCallStack $ hideClockResetEnable (E.atan2Unsigned @d)
{-# INLINE atan2Unsigned #-}

-- | The angle and the magnitude, times the CORDIC gain, of the vector @(x, y)@. See 'E.vectorTranslate'.
vectorTranslate ::
  forall d dom n i f fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat i
  , KnownNat f
  , KnownNat fOut
  , 1 <= i
  , 1 <= f
  , 4 <= i + f
  , i + f <= 64
  , 1 <= fOut
  , fOut <= 16
  , i <= fOut + 2
  , HasCallStack
  ) =>
  -- | x
  DSignal dom n (SFixed i f) ->
  -- | y
  DSignal dom n (SFixed i f) ->
  -- | Angle, in radians, and magnitude
  DSignal dom (n + d) (SFixed 3 fOut, UFixed (i + 1) (fOut + 2 - i))
vectorTranslate = withFrozenCallStack $ hideClockResetEnable (E.vectorTranslate @d)
{-# INLINE vectorTranslate #-}

-- | The angle and the magnitude of the vector @(x, y)@. See 'E.vectorTranslateCompensated'.
vectorTranslateCompensated ::
  forall d dom n i f fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat i
  , KnownNat f
  , KnownNat fOut
  , 1 <= i
  , 1 <= f
  , 4 <= i + f
  , i + f <= 64
  , 1 <= fOut
  , fOut <= 16
  , i <= fOut + 2
  , HasCallStack
  ) =>
  -- | x
  DSignal dom n (SFixed i f) ->
  -- | y
  DSignal dom n (SFixed i f) ->
  -- | Angle, in radians, and magnitude
  DSignal dom (n + d) (SFixed 3 fOut, UFixed i (fOut + 2 - i))
vectorTranslateCompensated = withFrozenCallStack $ hideClockResetEnable (E.vectorTranslateCompensated @d)
{-# INLINE vectorTranslateCompensated #-}

-- | The angle and the magnitude, times the CORDIC gain, of the vector @(x, y)@
-- with non-negative coordinates. See 'E.vectorTranslateUnsigned'.
vectorTranslateUnsigned ::
  forall d dom n i f fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat i
  , KnownNat f
  , KnownNat fOut
  , 1 <= f
  , 4 <= i + f
  , i + f <= 64
  , 1 <= fOut
  , fOut <= 16
  , i <= fOut + 1
  , HasCallStack
  ) =>
  -- | x
  DSignal dom n (UFixed i f) ->
  -- | y
  DSignal dom n (UFixed i f) ->
  -- | Angle, in radians, and magnitude
  DSignal dom (n + d) (SFixed 2 fOut, UFixed (i + 2) (fOut + 1 - i))
vectorTranslateUnsigned = withFrozenCallStack $ hideClockResetEnable (E.vectorTranslateUnsigned @d)
{-# INLINE vectorTranslateUnsigned #-}

-- | The angle and the magnitude of the vector @(x, y)@ with non-negative
-- coordinates. See 'E.vectorTranslateUnsignedCompensated'.
vectorTranslateUnsignedCompensated ::
  forall d dom n i f fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat i
  , KnownNat f
  , KnownNat fOut
  , 1 <= f
  , 4 <= i + f
  , i + f <= 64
  , 1 <= fOut
  , fOut <= 16
  , i <= fOut + 1
  , HasCallStack
  ) =>
  -- | x
  DSignal dom n (UFixed i f) ->
  -- | y
  DSignal dom n (UFixed i f) ->
  -- | Angle, in radians, and magnitude
  DSignal dom (n + d) (SFixed 2 fOut, UFixed (i + 1) (fOut + 1 - i))
vectorTranslateUnsignedCompensated = withFrozenCallStack $ hideClockResetEnable (E.vectorTranslateUnsignedCompensated @d)
{-# INLINE vectorTranslateUnsignedCompensated #-}

-- | Rotate the vector @(x, y)@ by an angle in radians, returning the rotated
-- vector times the CORDIC gain. See 'E.vectorRotate'.
vectorRotate ::
  forall d dom n f fOut.
  ( HiddenClockResetEnable dom
  , KnownNat d
  , KnownNat f
  , KnownNat fOut
  , 3 <= f
  , f <= 44
  , fOut <= 48
  , HasCallStack
  ) =>
  -- | x
  DSignal dom n (SFixed 2 f) ->
  -- | y
  DSignal dom n (SFixed 2 f) ->
  -- | Angle, in radians
  DSignal dom n (SFixed 3 f) ->
  -- | Rotated x and y, times K
  DSignal dom (n + d) (SFixed 3 fOut, SFixed 3 fOut)
vectorRotate = withFrozenCallStack $ hideClockResetEnable (E.vectorRotate @d)
{-# INLINE vectorRotate #-}
