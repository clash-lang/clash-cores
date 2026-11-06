{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx FIR Compiler LogiCORE IP v7.2](https://docs.amd.com/r/en-US/pg149-fir-compiler),
with a hidden clock and enable. See "Clash.Cores.Xilinx.Fir.Explicit" for the
details: the data formats, the latency, the configuration and what is not
supported.

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @fir@/.../@.clash.tcl@ which needs to be executed in the Vivado project
to create the proper entity. Simulation in Clash produces bit-identical results
to the IP, cycle for cycle.

For example, a symmetric 5-tap low-pass filter of 16-bit data, with its
output rounded to 16 bits, and its latency of 9 clock cycles:

@
lowPass ::
  (HiddenClock dom, HiddenEnable dom) =>
  DSignal dom 0 (Signed 16) ->
  DSignal dom 9 (Signed 16)
lowPass =
  firWith \@9
    defConfig{rounding = ConvergentRoundingToEven}
    (1 :> 4 :> 6 :> 4 :> 1 :> Nil :: Vec 5 (Signed 8))
@
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Fir
  ( -- * Instantiating IP
    firWith
  , fir
  , firValidWith
  , firValid
    -- * Numbers
  , E.FirNumber(..)
  , E.FirOutput
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.Rounding(..)
  , E.CoefficientStructure(..)
  , E.DspSlice(..)
    -- * Latency and output width
  , E.firLatency
  , E.firValidLatency
  , E.fullPrecisionWidth
  ) where

import Clash.Prelude

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Xilinx.Fir.Explicit as E
import Clash.Cores.Xilinx.Fir.Explicit (FirNumber, FirOutput)

-- | Customizable FIR filter. See 'E.firWith'.
firWith ::
  forall d a c wd wc wo n dom k.
  ( HiddenClock dom
  , HiddenEnable dom
  , FirNumber a
  , FirNumber c
  , FirNumber (FirOutput a c)
  , KnownNat d
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , HasCallStack
  ) =>
  E.Config ->
  -- | Coefficients
  Vec n (c wc) ->
  -- | Data
  DSignal dom k (a wd) ->
  -- | Filtered data
  DSignal dom (k + d) (FirOutput a c wo)
firWith cfg coefs =
  withFrozenCallStack $ hideEnable $ hideClock $ E.firWith @d cfg coefs
{-# INLINE firWith #-}

-- | FIR filter with full precision output. See 'E.fir'.
fir ::
  forall d a c wd wc wo n dom k.
  ( HiddenClock dom
  , HiddenEnable dom
  , FirNumber a
  , FirNumber c
  , FirNumber (FirOutput a c)
  , KnownNat d
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , HasCallStack
  ) =>
  -- | Coefficients
  Vec n (c wc) ->
  -- | Data
  DSignal dom k (a wd) ->
  -- | Filtered data
  DSignal dom (k + d) (FirOutput a c wo)
fir coefs = withFrozenCallStack $ hideEnable $ hideClock $ E.fir @d coefs
{-# INLINE fir #-}

-- | Customizable FIR filter for valid-qualified samples, at most one every @p@
-- clock cycles. See 'E.firValidWith'.
firValidWith ::
  forall d p a c wd wc wo n dom k.
  ( HiddenClock dom
  , HiddenEnable dom
  , FirNumber a
  , FirNumber c
  , FirNumber (FirOutput a c)
  , KnownNat d
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , 1 <= p
  , HasCallStack
  ) =>
  E.Config ->
  -- | Sample period @p@: the number of clock cycles per sample
  SNat p ->
  -- | Coefficients
  Vec n (c wc) ->
  -- | Data
  DSignal dom k (Maybe (a wd)) ->
  -- | Ready (@TREADY@), and the filtered data
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firValidWith cfg p coefs =
  withFrozenCallStack $ hideEnable $ hideClock $ E.firValidWith @d cfg p coefs
{-# INLINE firValidWith #-}

-- | FIR filter for valid-qualified samples, at most one every @p@ clock
-- cycles, with full precision output. See 'E.firValid'.
firValid ::
  forall d p a c wd wc wo n dom k.
  ( HiddenClock dom
  , HiddenEnable dom
  , FirNumber a
  , FirNumber c
  , FirNumber (FirOutput a c)
  , KnownNat d
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , 1 <= p
  , HasCallStack
  ) =>
  -- | Sample period @p@: the number of clock cycles per sample
  SNat p ->
  -- | Coefficients
  Vec n (c wc) ->
  -- | Data
  DSignal dom k (Maybe (a wd)) ->
  -- | Ready (@TREADY@), and the filtered data
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firValid p coefs = withFrozenCallStack $ hideEnable $ hideClock $ E.firValid @d p coefs
{-# INLINE firValid #-}
