{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx FIR Compiler LogiCORE IP v7.2](https://docs.amd.com/r/en-US/pg149-fir-compiler).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @fir@/.../@.clash.tcl@ which needs to be executed in the Vivado project
to create the proper entity. Simulation in Clash produces bit-identical results
to the IP, cycle for cycle.

= Filters

The IP computes @y[t] = sum_i c_i * x[t - i]@ for a fixed set of coefficients
@c_0, c_1, ...@ and the samples @x@, and rounds @y@ to the output width. The
coefficients are given as a 'Vec' of 'Signed' or 'Unsigned' numbers, and are
compiled into the IP. Clash reads them while it generates HDL, so they should
be constants that Clash sees directly: a literal 'Vec' or one made with
'listToVecTH'. Clash does not evaluate functions applied to them, such as
'map', at that point. You can compute coefficients in Template Haskell:

@
lowPass :: Vec 11 (Signed 16)
lowPass = $(listToVecTH (designLowPass 11 0.2 :: [Signed 16]))
@

The data and the coefficients are integers. The output is the exact sum of
products, so for fixed point data and coefficients the output has as many
fractional bits as the data and coefficients together, minus the bits the
rounding discards.

= Data formats

* The data is 'Signed' or 'Unsigned', of 2 to 48 bits. The coefficients are
  'Signed' or 'Unsigned', of 2 to 48 bits; there should be at least 2 of them.
  The IP limits the widths further when both are wide. Vivado reports an error
  when creating the IP if it does not support a combination.

* The IP infers the sign of the coefficients from their values: they are
  unsigned if none of them is negative. Unsigned data with signed coefficients
  therefore needs a negative coefficient. Simulation checks this.

* The output is 'Unsigned' if the data and coefficients are, and 'Signed'
  otherwise, see 'FirOutput'.

* With 'FullPrecision' output, the output width should equal the width of the
  exact result, see 'fullPrecisionWidth'. Otherwise, the output width should
  be less than that, and the IP discards the least significant bits as given
  by the 'Rounding' mode. The output keeps the most significant bits of the full
  precision result. An output that does not fit the output width, which only
  happens for a few extreme inputs and coefficients, wraps around.

= Configuration

All IP instantiated by this module has the following properties:

* Single rate, single channel filter, which takes a sample every clock cycle
* The systolic multiply-accumulate architecture
* Non-blocking AXI4-Stream interfaces, a clock enable, no reset input, no
  @TLAST@ or @TUSER@. @TVALID@ on the input is always asserted, @TVALID@ on
  the output is ignored.

The latency of the IP is the delay argument of the resulting 'DSignal'. Like
the latency of the CORDIC IP, and unlike that of the Floating-Point IP, it
cannot be configured: it depends on the coefficients and the configuration.
The delay must therefore equal the IP's latency. Simulation checks this, and
fails with an error message stating the correct latency if it does not. HDL
generation does not check it. Function 'firLatency' computes the latency, which
is also the latency Vivado reports for the IP. It depends on:

* the number of coefficients and their structure: symmetric coefficients
  halve the number of multiplications, see 'CoefficientStructure';
* the widths of the data and coefficients, if a multiplication needs several
  DSP slices, which depends on the family of the device ('dspSlice');
* the rounding mode;
* the number of columns of DSP slices the filter needs ('dspColumnLength').

De-asserting the 'Enable' signal stalls the whole IP.

The IP does not accept a sample in the first clock cycle in which it is
enabled: its @TREADY@ output is low then. It processes the samples of all
later (enabled) cycles. The history of the filter starts out as all zeros, so
the output for the sample of the second enabled cycle is @c_0 * x@. The
outputs of the first cycles, until the latency has passed, are undefined.

= Not supported

* Lower sample rates (multiple clock cycles per sample), interpolation and
  decimation
* Hilbert transformers with the IP's two outputs: the IP gives coefficients
  that form a Hilbert transformer a second output, the delayed input. Use
  'NegativeSymmetric' for such coefficients.
* Multiple channels or parallel paths, multiple coefficient sets, reloadable
  coefficients
* Quantization of real-valued coefficients by the IP. Quantize them yourself.
* The transpose multiply-accumulate architecture, optimization goals other
  than area, and other settings that only affect the implementation
* Blocking AXI4-Stream interfaces (@TREADY@ on the output), resets, @TLAST@
  and @TUSER@
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Fir.Explicit
  ( -- * Instantiating IP
    firWith
  , fir
    -- * Numbers
  , FirNumber(..)
  , FirOutput
    -- * Customizing IP
  , Config(..)
  , defConfig
  , Rounding(..)
  , CoefficientStructure(..)
  , DspSlice(..)
    -- * Latency and output width
  , firLatency
  , fullPrecisionWidth
  ) where

import Clash.Explicit.Prelude
import qualified Prelude as P

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Xilinx.Fir.Internal hiding (fullPrecisionWidth)
import qualified Clash.Cores.Xilinx.Fir.Internal as I

import qualified Clash.Signal.Delayed as D

-- | Customizable FIR filter.
--
-- The delay @d@ must equal the latency of the IP, see 'firLatency'. With
-- 'FullPrecision' output, the output width @wo@ must equal
-- 'fullPrecisionWidth'; otherwise it should be less than that. Simulation
-- checks both.
--
-- For example, a 5-tap low-pass filter of 16-bit data, rounded to a 16-bit
-- output:
--
-- @
-- filter ::
--   Clock dom ->
--   Enable dom ->
--   DSignal dom 0 (Signed 16) ->
--   DSignal dom 9 (Signed 16)
-- filter = firWith \@9 defConfig{rounding = ConvergentRoundingToEven} (1 :> 4 :> 6 :> 4 :> 1 :> Nil)
-- @
firWith ::
  forall d a c wd wc wo n dom k.
  ( KnownDomain dom
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
  Config ->
  -- | Coefficients
  Vec n (c wc) ->
  Clock dom ->
  Enable dom ->
  -- | Data
  DSignal dom k (a wd) ->
  -- | Filtered data
  DSignal dom (k + d) (FirOutput a c wo)
firWith cfg coefs clk en x
  | clashSimulation = sim
  | otherwise = synth
 where
  params =
    mkParams cfg (P.map toInteger (toList coefs))
      (natToNum @wd) (numberIsSigned @a) (natToNum @wc) (numberIsSigned @c) (natToNum @wo)

  sim =
    withFrozenCallStack (checkParams @d params) `seq`
      D.unsafeFromSignal
        ( numberFromInteger @(FirOutput a c) @wo
            <$> firModel params clk en (toInteger <$> D.toSignal x) )

  synth =
    D.unsafeFromSignal $
          numberFromPaddedBitVector @(FirOutput a c) @wo
      <$> firInst @a @c @wd @wc @wo cfg coefs clk en (D.toSignal x)
{-# INLINE firWith #-}

-- | FIR filter with the default configuration, 'defConfig': full precision
-- output. See 'firWith'.
fir ::
  forall d a c wd wc wo n dom k.
  ( KnownDomain dom
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
  Clock dom ->
  Enable dom ->
  -- | Data
  DSignal dom k (a wd) ->
  -- | Filtered data
  DSignal dom (k + d) (FirOutput a c wo)
fir = withFrozenCallStack $ firWith @d defConfig
{-# INLINE fir #-}

-- | Latency of the IP for a configuration, set of coefficients and data type
-- @a wd@. This is the latency Vivado reports for the IP. The output width does
-- not matter.
--
-- >>> import Clash.Prelude
-- >>> firLatency @Signed @16 defConfig (1 :> 2 :> 3 :> 4 :> Nil :: Vec 4 (Signed 8))
-- 9
-- >>> firLatency @Signed @16 defConfig (1 :> 2 :> 2 :> 1 :> Nil :: Vec 4 (Signed 8))
-- 8
firLatency ::
  forall a wd c wc n.
  (FirNumber a, FirNumber c, KnownNat wd, KnownNat wc) =>
  Config ->
  Vec n (c wc) ->
  Int
firLatency cfg coefs =
  latency $
    mkParams cfg (P.map toInteger (toList coefs))
      (natToNum @wd) (numberIsSigned @a) (natToNum @wc) (numberIsSigned @c) 0

-- | Width of the exact output of a filter with data of type @a wd@ and the
-- given coefficients: the data width plus @ceil (log2 (sum |c_i|))@, plus one
-- for unsigned data and coefficients of which at least one is negative.
--
-- >>> import Clash.Prelude
-- >>> fullPrecisionWidth @Signed @16 (1 :> 4 :> 6 :> 4 :> 1 :> Nil :: Vec 5 (Signed 8))
-- 20
fullPrecisionWidth ::
  forall a wd c wc n.
  (FirNumber a, FirNumber c, KnownNat wd, KnownNat wc) =>
  Vec n (c wc) ->
  Int
fullPrecisionWidth coefs =
  I.fullPrecisionWidth (natToNum @wd) (numberIsSigned @a) (P.map toInteger (toList coefs))
