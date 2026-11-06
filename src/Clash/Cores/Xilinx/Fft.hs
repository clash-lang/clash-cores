{-|
Copyright  :  (C) 2026,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Fast Fourier Transform LogiCORE IP v9.1](https://docs.amd.com/v/u/en-US/pg109-xfft),
with a hidden clock and reset. See "Clash.Cores.Xilinx.Fft.Explicit" for the
documentation.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Fft
  ( -- * Instantiating IP
    fft
    -- * Frames
  , E.Direction(..)
  , E.FrameConfig(..)
  , E.Groups
  , E.scaleByN
  , E.FftOutput(..)
    -- * Scaling
  , E.Scaling(..)
  , E.KnownScaling(..)
  , E.KnownFft
  , E.ByteWidth
  , E.ArithScaling(..)
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.Rounding(..)
  , E.OutputOrdering(..)
  , E.ComplexMultiplier(..)
  , E.ButterflyArithmetic(..)
  ) where

import Clash.Prelude

import Data.Complex (Complex)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Xilinx.Fft.Explicit as E
import Clash.Cores.Xilinx.Fft.Explicit (FftOutput, FrameConfig, KnownFft, OutputWidth)

-- | Transform frames of @2^nfft@ complex samples of @bw@ bits, with latency
-- @d@. See 'E.fft'.
fft ::
  forall d s nfft bw dom.
  ( HiddenClock dom
  , HiddenReset dom
  , KnownNat d
  , 7 <= d
  , KnownFft s nfft bw
  , HasCallStack
  ) =>
  E.Config ->
  -- | Configuration of the frame that starts in this clock cycle
  Signal dom (FrameConfig s nfft) ->
  -- | Input samples
  Signal dom (Maybe (Complex (Signed bw))) ->
  -- | Whether the IP accepts a sample in this clock cycle, and the output
  -- samples
  (Signal dom Bool, Signal dom (Maybe (FftOutput s nfft (OutputWidth s nfft bw))))
fft cfg = withFrozenCallStack $ hideReset (hideClock (E.fft @d cfg))
{-# INLINE fft #-}
