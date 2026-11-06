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
'map', at that point. You can compute coefficients in Template Haskell, with a
function @designLowPass@ defined in another module:

@
lowPass :: Vec 11 (Signed 16)
lowPass = $(listToVecTH (designLowPass 11 0.2 :: [Signed 16]))
@

There are two kinds of filters:

* 'firWith' and 'fir' take a sample every clock cycle.

* 'firValidWith' and 'firValid' take valid-qualified samples ('Maybe'), at
  most one every @p@ clock cycles: the /sample period/. They give
  valid-qualified outputs, and the IP's @TREADY@. A longer sample period lets
  the IP use fewer DSP slices.

* 'firDecimateWith' and 'firDecimate' are like 'firValidWith', but decimate by
  an integer factor @r@: they give an output for every @r@ samples.

* 'firInterpolateWith' and 'firInterpolate' are like 'firValidWith', but
  interpolate by an integer factor @l@: they give @l@ outputs for every
  sample, at most one per clock cycle.

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

* Single rate, decimating or interpolating, single channel filter
* The systolic multiply-accumulate architecture
* Non-blocking AXI4-Stream interfaces without an input FIFO, a clock enable, no
  reset input, no @TLAST@ or @TUSER@. For 'firWith', @TVALID@ on the input is
  always asserted, and @TVALID@ on the output is ignored.

The latency of the IP is the delay argument of the resulting 'DSignal'. Like
the latency of the CORDIC IP, and unlike that of the Floating-Point IP, it
cannot be configured: it depends on the coefficients and the configuration.
The delay must therefore equal the IP's latency. Simulation checks this, and
fails with an error message stating the correct latency if it does not. HDL
generation does not check it. Functions 'firLatency', 'firValidLatency',
'firDecimateLatency' and 'firInterpolateLatency' compute the latency, which is
also the latency Vivado reports for the IP. It depends on:

* the number of coefficients and their structure: symmetric coefficients
  halve the number of multiplications, see 'CoefficientStructure';
* the sample period and the decimation or interpolation factor;
* the widths of the data and coefficients, if a multiplication needs several
  DSP slices, which depends on the family of the device ('dspSlice'), and on
  the width of the full precision result, if it exceeds the 48 bits of the
  accumulator of a DSP slice;
* the rounding mode;
* the number of columns of DSP slices the filter needs ('dspColumnLength').

De-asserting the 'Enable' signal stalls the whole IP.

The IP does not accept a sample in the first clock cycle in which it is
enabled: its @TREADY@ output is low then. It processes the samples of all
later (enabled) cycles. The history of the filter starts out as all zeros, so
the output for the sample of the second enabled cycle is @c_0 * x@. The
outputs of the first cycles, until the latency has passed, are undefined.

= Not supported

* Interpolation with a sample period shorter than the interpolation factor,
  for which the IP gives several outputs per clock cycle, and fractional rate
  changes
* Interpolators that exploit symmetric coefficients: use 'NonSymmetric' for
  interpolators, see 'firInterpolateWith'.
* Vivado rejects symmetric rounding for some decimators and interpolators.
  It reports an error when creating the IP then.
* Half-band decimators: the IP decimates half-band coefficients by two with a
  dedicated half-band decimator. Simulation reports an error for those; use
  'Symmetric' for such coefficients.
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
  , firValidWith
  , firValid
  , firDecimateWith
  , firDecimate
  , firInterpolateWith
  , firInterpolate
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
  , firValidLatency
  , firDecimateLatency
  , firInterpolateLatency
  , fullPrecisionWidth
  ) where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Data.Maybe (fromMaybe, isJust)

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
      (natToNum @wd) (numberIsSigned @a) (natToNum @wc) (numberIsSigned @c) (natToNum @wo) 1
      SingleRate

  sim =
    withFrozenCallStack (checkParams @d params) `seq`
      D.unsafeFromSignal
        ( numberFromInteger @(FirOutput a c) @wo
            <$> firModel params clk en (toInteger <$> D.toSignal x) )

  synth = D.unsafeFromSignal (numberFromPaddedBitVector @(FirOutput a c) @wo <$> tdata)
   where
    (_tready, _tvalid, tdata) =
      firInst @a @c @wd @wc @wo cfg SingleRate 1 coefs clk en (pure True) (D.toSignal x)
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
      (natToNum @wd) (numberIsSigned @a) (natToNum @wc) (numberIsSigned @c) 0 1 SingleRate

-- | Customizable FIR filter for samples that arrive at most once every @p@
-- clock cycles: the IP's /sample period/. With a longer sample period, the IP
-- needs fewer DSP slices: each of them handles several coefficients, one per
-- clock cycle.
--
-- The samples are valid-qualified: 'Nothing' means there is no sample in that
-- clock cycle. So are the outputs: the output for a sample comes out @d@
-- (enabled) clock cycles after the IP accepted the sample, in the IP's latency
-- (see 'firValidLatency'), and is 'Nothing' in all other clock cycles.
--
-- The function also gives the IP's @TREADY@: whether the IP is ready to
-- accept a sample in the same clock cycle. The IP accepts a sample in a clock
-- cycle in which it is enabled and ready, and the sample is 'Just'. Other
-- samples are lost. The IP is not ready in the first clock cycle in which it is
-- enabled, and for a few clock cycles after it accepts a sample: at most @p -
-- 1@, but fewer if it can process a sample faster than that. So a stream of
-- samples at most one every @p@ clock cycles is always accepted, except in
-- the first enabled clock cycle.
--
-- With a sample period of more than one clock cycle, the IP spreads the
-- coefficients (or pairs of symmetric coefficients) evenly over its
-- multiply-add units. If they do not fill those exactly, it pads them with
-- zeros. It pads symmetric coefficients at both ends, to keep them symmetric,
-- which delays the output by a sample per padded pair, or by two samples for
-- a half-band filter. For example, 10 symmetric coefficients (5 pairs) with a
-- sample period of 2 use three multiply-add units of two pairs each: one pair
-- of zeros pads the coefficients, and the output is delayed by one sample. The
-- simulation model does the same.
--
-- A sample period of 1 gives a filter like 'firWith', but with valid-qualified
-- samples. Everything else is as for 'firWith'.
firValidWith ::
  forall d p a c wd wc wo n dom k.
  ( KnownDomain dom
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
  Config ->
  -- | Sample period @p@: the number of clock cycles per sample
  SNat p ->
  -- | Coefficients
  Vec n (c wc) ->
  Clock dom ->
  Enable dom ->
  -- | Data
  DSignal dom k (Maybe (a wd)) ->
  -- | Ready (@TREADY@), and the filtered data
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firValidWith cfg p@SNat = firRate @d cfg SingleRate (snatToNum p)
{-# INLINE firValidWith #-}

-- | Worker of 'firValidWith' and 'firDecimateWith'
firRate ::
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
  Rate ->
  -- | Sample period
  Int ->
  Vec n (c wc) ->
  Clock dom ->
  Enable dom ->
  DSignal dom k (Maybe (a wd)) ->
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firRate cfg rt p coefs clk en x
  | clashSimulation = sim
  | otherwise = synth
 where
  params =
    mkParams cfg (P.map toInteger (toList coefs))
      (natToNum @wd) (numberIsSigned @a) (natToNum @wc) (numberIsSigned @c) (natToNum @wo)
      p rt

  sim =
    withFrozenCallStack (checkParams @d params) `seq`
      ( D.unsafeFromSignal ready
      , D.unsafeFromSignal (fmap (numberFromInteger @(FirOutput a c) @wo) <$> out) )
   where
    (ready, out) = firValidModel params clk en (fmap toInteger <$> D.toSignal x)

  synth =
    ( D.unsafeFromSignal tready
    , D.unsafeFromSignal (toOutput <$> tvalid <*> tdata) )
   where
    (tready, tvalid, tdata) =
      firInst @a @c @wd @wc @wo cfg rt (toInteger p) coefs clk en
        (isJust <$> D.toSignal x)
        (fromMaybe 0 <$> D.toSignal x)
    toOutput valid dat
      | valid = Just (numberFromPaddedBitVector @(FirOutput a c) @wo dat)
      | otherwise = Nothing
{-# INLINE firRate #-}

-- | FIR filter for samples that arrive at most once every @p@ clock cycles,
-- with the default configuration, 'defConfig': full precision output. See
-- 'firValidWith'.
firValid ::
  forall d p a c wd wc wo n dom k.
  ( KnownDomain dom
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
  Clock dom ->
  Enable dom ->
  -- | Data
  DSignal dom k (Maybe (a wd)) ->
  -- | Ready (@TREADY@), and the filtered data
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firValid = withFrozenCallStack $ firValidWith @d defConfig
{-# INLINE firValid #-}

-- | Latency of the IP for 'firValidWith', for a configuration, sample period,
-- set of coefficients and data type @a wd@. This is the latency Vivado reports
-- for the IP.
--
-- >>> import Clash.Prelude
-- >>> firValidLatency @Signed @16 defConfig{dspSlice = Dsp48E1} d4 (3 :> (-5) :> 7 :> 11 :> (-13) :> 100 :> 200 :> (-300) :> 17 :> Nil :: Vec 9 (Signed 16))
-- 12
firValidLatency ::
  forall a wd c wc n p.
  (FirNumber a, FirNumber c, KnownNat wd, KnownNat wc) =>
  Config ->
  SNat p ->
  Vec n (c wc) ->
  Int
firValidLatency cfg p@SNat coefs =
  latency $
    mkParams cfg (P.map toInteger (toList coefs))
      (natToNum @wd) (numberIsSigned @a) (natToNum @wc) (numberIsSigned @c) 0 (snatToNum p)
      SingleRate

-- | Customizable decimating FIR filter: it filters valid-qualified samples, at
-- most one every @p@ clock cycles, and gives one output for every @r@ samples
-- it accepts. The output is the output of the filter for the last of every
-- group of @r@ accepted samples, counting from the first sample the IP
-- accepts, and comes out @d@ (enabled) clock cycles after the IP accepted
-- that sample: the IP's latency, see 'firDecimateLatency'.
--
-- The decimator has @r * p@ clock cycles to compute an output. A longer
-- sample period, or a larger decimation factor, lets the IP use fewer DSP
-- slices. Like 'firValidWith', the IP may pad symmetric coefficients with
-- zeros at both ends to fill its multiply-add units, which delays the output
-- by a sample per padded pair (two for a half-band filter); the simulation
-- model does the same.
--
-- The IP only exploits the zeros of half-band coefficients when it decimates
-- by two; for other decimation factors it treats them as symmetric
-- coefficients.
--
-- Everything else, including the ready signal, is as for 'firValidWith'.
firDecimateWith ::
  forall d r p a c wd wc wo n dom k.
  ( KnownDomain dom
  , FirNumber a
  , FirNumber c
  , FirNumber (FirOutput a c)
  , KnownNat d
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , 2 <= r
  , 1 <= p
  , HasCallStack
  ) =>
  Config ->
  -- | Decimation factor @r@
  SNat r ->
  -- | Sample period @p@: the number of clock cycles per input sample
  SNat p ->
  -- | Coefficients
  Vec n (c wc) ->
  Clock dom ->
  Enable dom ->
  -- | Data
  DSignal dom k (Maybe (a wd)) ->
  -- | Ready (@TREADY@), and the filtered, decimated data
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firDecimateWith cfg r@SNat p@SNat =
  firRate @d cfg (Decimation (snatToNum r)) (snatToNum p)
{-# INLINE firDecimateWith #-}

-- | Decimating FIR filter with the default configuration, 'defConfig': full
-- precision output. See 'firDecimateWith'.
firDecimate ::
  forall d r p a c wd wc wo n dom k.
  ( KnownDomain dom
  , FirNumber a
  , FirNumber c
  , FirNumber (FirOutput a c)
  , KnownNat d
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , 2 <= r
  , 1 <= p
  , HasCallStack
  ) =>
  -- | Decimation factor @r@
  SNat r ->
  -- | Sample period @p@: the number of clock cycles per input sample
  SNat p ->
  -- | Coefficients
  Vec n (c wc) ->
  Clock dom ->
  Enable dom ->
  -- | Data
  DSignal dom k (Maybe (a wd)) ->
  -- | Ready (@TREADY@), and the filtered, decimated data
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firDecimate = withFrozenCallStack $ firDecimateWith @d defConfig
{-# INLINE firDecimate #-}

-- | Latency of the IP for 'firDecimateWith', for a configuration, decimation
-- factor, sample period, set of coefficients and data type @a wd@.
--
-- >>> import Clash.Prelude
-- >>> firDecimateLatency @Signed @16 defConfig{dspSlice = Dsp48E1} d2 d1 (3 :> (-5) :> 7 :> 11 :> (-13) :> 100 :> 200 :> (-300) :> 17 :> Nil :: Vec 9 (Signed 16))
-- 12
firDecimateLatency ::
  forall a wd c wc n r p.
  (FirNumber a, FirNumber c, KnownNat wd, KnownNat wc) =>
  Config ->
  SNat r ->
  SNat p ->
  Vec n (c wc) ->
  Int
firDecimateLatency cfg r@SNat p@SNat coefs =
  latency $
    mkParams cfg (P.map toInteger (toList coefs))
      (natToNum @wd) (numberIsSigned @a) (natToNum @wc) (numberIsSigned @c) 0 (snatToNum p)
      (Decimation (snatToNum r))

-- | Customizable interpolating FIR filter: it filters valid-qualified samples,
-- at most one every @p@ clock cycles, and gives @l@ outputs for every sample
-- it accepts. The sample period should be at least the interpolation factor,
-- @p >= l@, so that the IP gives at most one output per clock cycle.
--
-- The outputs are those of the filter for the input with @l - 1@ zeros after
-- every sample: output @m@ (counting from 0) for sample @x[j]@ is
-- @sum_i c_(m + l*i) * x[j - i]@. Output 0 comes out @d@ (enabled) clock
-- cycles after the IP accepted the sample (the IP's latency, see
-- 'firInterpolateLatency'), and output @m@ another @m * (p \`div\` l)@ cycles
-- later. Each output is 'Just' for a single clock cycle; the output is
-- 'Nothing' in all other cycles. The IP is not ready for a new sample for
-- @l * (p \`div\` l) - 1@ cycles after it accepts one, which can be fewer than
-- @p - 1@.
--
-- The full precision output width is that of the widest phase: the data width
-- plus @ceil (log2 (sum_i |c_(m + l*i)|))@ for the phase @m@ with the largest
-- sum, plus one for unsigned data and signed coefficients.
--
-- Only the 'NonSymmetric' coefficient structure is supported: the IP exploits
-- symmetric coefficients of an interpolator in ways the simulation model does
-- not cover, so simulation reports an error for those unless the
-- configuration says 'NonSymmetric'. This costs DSP slices, not results.
--
-- Everything else is as for 'firValidWith'.
firInterpolateWith ::
  forall d l p a c wd wc wo n dom k.
  ( KnownDomain dom
  , FirNumber a
  , FirNumber c
  , FirNumber (FirOutput a c)
  , KnownNat d
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , 2 <= l
  , l <= p
  , HasCallStack
  ) =>
  Config ->
  -- | Interpolation factor @l@
  SNat l ->
  -- | Sample period @p@: the number of clock cycles per input sample
  SNat p ->
  -- | Coefficients
  Vec n (c wc) ->
  Clock dom ->
  Enable dom ->
  -- | Data
  DSignal dom k (Maybe (a wd)) ->
  -- | Ready (@TREADY@), and the filtered, interpolated data
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firInterpolateWith cfg l@SNat p@SNat =
  firRate @d cfg (Interpolation (snatToNum l)) (snatToNum p)
{-# INLINE firInterpolateWith #-}

-- | Interpolating FIR filter with full precision output and the
-- 'NonSymmetric' coefficient structure. See 'firInterpolateWith'.
firInterpolate ::
  forall d l p a c wd wc wo n dom k.
  ( KnownDomain dom
  , FirNumber a
  , FirNumber c
  , FirNumber (FirOutput a c)
  , KnownNat d
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , 2 <= l
  , l <= p
  , HasCallStack
  ) =>
  -- | Interpolation factor @l@
  SNat l ->
  -- | Sample period @p@: the number of clock cycles per input sample
  SNat p ->
  -- | Coefficients
  Vec n (c wc) ->
  Clock dom ->
  Enable dom ->
  -- | Data
  DSignal dom k (Maybe (a wd)) ->
  -- | Ready (@TREADY@), and the filtered, interpolated data
  (DSignal dom k Bool, DSignal dom (k + d) (Maybe (FirOutput a c wo)))
firInterpolate =
  withFrozenCallStack $ firInterpolateWith @d defConfig{coefficientStructure = NonSymmetric}
{-# INLINE firInterpolate #-}

-- | Latency of the IP for 'firInterpolateWith', for a configuration,
-- interpolation factor, sample period, set of coefficients and data type
-- @a wd@: the latency of the first output for a sample.
--
-- >>> import Clash.Prelude
-- >>> firInterpolateLatency @Signed @16 defConfig{coefficientStructure = NonSymmetric} d2 d4 (3 :> (-5) :> 7 :> 11 :> (-13) :> 100 :> 200 :> (-300) :> 17 :> Nil :: Vec 9 (Signed 16))
-- 10
firInterpolateLatency ::
  forall a wd c wc n l p.
  (FirNumber a, FirNumber c, KnownNat wd, KnownNat wc) =>
  Config ->
  SNat l ->
  SNat p ->
  Vec n (c wc) ->
  Int
firInterpolateLatency cfg l@SNat p@SNat coefs =
  latency $
    mkParams cfg (P.map toInteger (toList coefs))
      (natToNum @wd) (numberIsSigned @a) (natToNum @wc) (numberIsSigned @c) 0 (snatToNum p)
      (Interpolation (snatToNum l))

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
