{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- |
  Copyright   :  (C) 2023-2024, Floriaan Bulten
                     2026, Rowan Goemans
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Rowan Goemans <goemansrowan@gmail.com>

  Sigma-delta ADC, originally contributed by FloriaanB in
  <https://github.com/clash-lang/clash-compiler/pull/2617>.

  Connect the analog signal to the positive input of an external comparator
  (or a suitable LVDS input), and the output of an RC low-pass network to its
  negative input. Drive that network with the registered feedback output.
  The comparator samples must already be captured in the circuit's clock
  domain. When using DDR or a deserializer, serialize the feedback word in
  the same bit order as the input samples.

  The RC time constant depends on the comparator, I/O voltage, sampling
  frequency and required analog bandwidth; this digital core does not choose
  component values.
-}
module Clash.Cores.SigmaDeltaADC (
  sigmaDeltaADC,
  sigmaDeltaSampleRate,
) where

import Clash.Prelude
import Clash.Sized.Internal.BitVector (popCountBV)
import qualified Prelude as P

-- | Latch the most significant bits when a complete sum is available.
truncateAccum ::
  forall adcWidth accumExtra.
  (KnownNat adcWidth, KnownNat accumExtra) =>
  BitVector adcWidth ->
  (BitVector (adcWidth + accumExtra), Bool) ->
  (BitVector adcWidth, BitVector adcWidth)
truncateAccum accum (sigma, ready)
  | ready = (fst (split sigma), accum)
  | otherwise = (accum, accum)

{- | Count ones over a complete conversion window, then discard the extra
accumulator bits. The ready signal is aligned with the registered result.
-}
accumulator ::
  forall adcWidth accumExtra inputWidth dom.
  ( HiddenClockResetEnable dom
  , KnownNat accumExtra
  , KnownNat adcWidth
  , KnownNat inputWidth
  , inputWidth <= (adcWidth + accumExtra)
  ) =>
  SNat (accumExtra + adcWidth) ->
  Signal dom (BitVector inputWidth) ->
  (Signal dom (BitVector adcWidth), Signal dom Bool)
accumulator _ numOnes = (accum, register False rollover)
 where
  accum = mealyB truncateAccum 0 (sigma, rollover)
  (sigma, rollover) =
    mealyB accumulatorCounter (0 :: BitVector (accumExtra + adcWidth), 0) numOnes

{- | Saturate the sum before truncating: an all-ones input represents full
scale, which must not wrap to zero. Include the last input in each window.
-}
accumulatorCounter ::
  forall sigmaWidth inputWidth.
  (KnownNat sigmaWidth, KnownNat inputWidth, inputWidth <= sigmaWidth) =>
  (BitVector sigmaWidth, Unsigned (sigmaWidth - inputWidth + 1)) ->
  BitVector inputWidth ->
  ( (BitVector sigmaWidth, Unsigned (sigmaWidth - inputWidth + 1))
  , (BitVector sigmaWidth, Bool)
  )
accumulatorCounter (sigma, count) numOnes =
  ((if rollover then 0 else total, count + 1), (total, rollover))
 where
  rollover = count == maxBound
  total = resize (min (extend sigma + resize numOnes) limit)
  limit = extend (maxBound :: BitVector sigmaWidth) :: BitVector (sigmaWidth + 1)

{- | Average non-overlapping blocks of 2^filterDepth accumulator results.
A depth of zero passes each result through with one register of latency.
-}
boxFilter ::
  forall dom adcWidth filterDepth.
  (HiddenClockResetEnable dom, KnownNat adcWidth, KnownNat filterDepth) =>
  SNat filterDepth ->
  Signal dom (BitVector adcWidth) ->
  Signal dom Bool ->
  (Signal dom (BitVector adcWidth), Signal dom Bool)
boxFilter _ dataIn accumRdy = (dataOut, register False latchResult)
 where
  count = regEn (0 :: Unsigned filterDepth) accumRdy (count + 1)
  accum = mealyB filterAccumulator 0 (count, dataIn, accumRdy)
  latchResult = accumRdy .&&. (count .==. pure maxBound)
  dataOut = mealyB truncateAccum 0 (accum, latchResult)

-- | Include the current sample in the completed sum before clearing it.
filterAccumulator ::
  forall filterDepth adcWidth.
  (KnownNat filterDepth, KnownNat adcWidth) =>
  BitVector (filterDepth + adcWidth) ->
  (Unsigned filterDepth, BitVector adcWidth, Bool) ->
  (BitVector (filterDepth + adcWidth), BitVector (filterDepth + adcWidth))
filterAccumulator accum (count, dataIn, ready)
  | not ready = (accum, accum)
  | count == maxBound = (0, total)
  | otherwise = (total, total)
 where
  total = accum + extend dataIn

{- | Sigma-delta ADC with configurable output resolution and two stages of
decimation.

For an @adcWidth@-bit result and an @accumWidth = adcWidth + accumExtra@-bit
accumulator, each conversion counts @2^accumWidth@ comparator samples.
It saturates the sum at @2^accumWidth - 1@ and drops @accumExtra@ least
significant bits. Increasing @accumExtra@ increases oversampling at the
cost of conversion rate. The box filter then averages @2^filterDepth@
conversions, rounding down. Increasing its depth reduces output rate and
bandwidth; zero disables averaging.

The input contains @inputWidth + 1@ successive samples per clock. This
number must be a power of two so each conversion window contains exactly
@2^accumWidth@ samples. The accumulator must be wide enough to represent
the population count of an input word.

With the enable continuously asserted, the output rate is
@clockHz * (inputWidth + 1) / 2^(accumWidth + filterDepth)@.
Use 'sigmaDeltaSampleRate' to calculate it. The first valid result appears
after a full block plus two register stages: for a block of @N@ input
clock cycles, inputs at cycles @0 .. N-1@ produce a result at cycle @N+1@.
No partial result is emitted after reset. Output data holds its last
result between conversions. Disabling the circuit freezes all state,
including the ready flag; consume results only on enabled cycles.
-}
sigmaDeltaADC ::
  forall adcWidth accumExtra filterDepth inputWidth dom.
  ( HiddenClockResetEnable dom
  , KnownNat adcWidth
  , KnownNat accumExtra
  , KnownNat inputWidth
  , 1 <= adcWidth
  , inputWidth + 1 ~ 2 ^ CLog 2 (inputWidth + 1)
  , CLog 2 (inputWidth + 2) <= (adcWidth + accumExtra)
  ) =>
  -- | Accumulator width; at least the output width and population-count width.
  SNat (accumExtra + adcWidth) ->
  -- | Logarithm (base two) of the number of conversions to average.
  SNat filterDepth ->
  -- | Captured comparator output; one or more successive samples per clock.
  Signal dom (BitVector (inputWidth + 1)) ->
  -- | Registered feedback to the RC network, digital result, and result valid.
  ( Signal dom (BitVector (inputWidth + 1))
  , Signal dom (BitVector adcWidth)
  , Signal dom Bool
  )
sigmaDeltaADC accumWidth filterDepth@SNat analogCmp = (delta, digitalOut, sampleRdy)
 where
  delta = register 0 analogCmp
  (accum, accumRdy) = accumulator accumWidth numOnes
  (digitalOut, sampleRdy) = boxFilter filterDepth accum accumRdy
  numOnes :: Signal dom (BitVector (CLog 2 (inputWidth + 2)))
  numOnes = (pack . countOnes) <$> analogCmp
  countOnes :: BitVector (inputWidth + 1) -> Unsigned (CLog 2 (inputWidth + 2))
  countOnes bits = fromIntegral (popCountBV @inputWidth bits)

{- | Output sample rate in hertz, assuming the circuit is always enabled.
Takes the same accumulator and filter widths as 'sigmaDeltaADC', the
number of comparator samples per clock, and the clock frequency in hertz.

For example, a 12-bit accumulator, filter depth 2, and four samples per
100 MHz clock give @100000000 * 4 / 2^(12+2) = 24414.0625@ samples/second.
-}
sigmaDeltaSampleRate ::
  forall accumWidth filterDepth samplesPerCycle.
  ( 1 <= samplesPerCycle
  , samplesPerCycle ~ 2 ^ CLog 2 samplesPerCycle
  , CLog 2 (samplesPerCycle + 1) <= accumWidth
  ) =>
  SNat accumWidth ->
  SNat filterDepth ->
  SNat samplesPerCycle ->
  Rational ->
  Rational
sigmaDeltaSampleRate accumWidth filterDepth samplesPerCycle clockHz =
  clockHz
    * snatToNum samplesPerCycle
    / fromInteger (2 P.^ (snatToNum accumWidth + snatToNum filterDepth :: Integer))
