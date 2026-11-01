{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx CORDIC LogiCORE IP v6.0](https://docs.amd.com/v/u/en-US/pg105-cordic).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @cordic@/.../@.clash.tcl@ which needs to be executed in the Vivado project
to create the proper entity. Simulation in Clash produces bit-identical
results to the IP, cycle by cycle.

= Functions

The IP computes one of the following functions. The documentation of each
function lists the range of inputs the IP supports. Results for inputs outside
that range are unspecified (PG105: "unpredictable"); the simulation model gives
/a/ result, but not necessarily the IP's.

* 'rotate': rotate a vector by a phase. Inputs: X and Y in [-1, 1], the phase
  in [-pi, pi]. The outputs are scaled by the CORDIC scale factor (about 1.6468)
  unless compensation scaling is enabled.

* 'translate': magnitude and phase of a vector. Inputs: X and Y in [-1, 1]. The
  magnitude is scaled by the CORDIC scale factor unless compensation scaling
  is enabled.

* 'sinCos': cosine and sine of a phase in [-pi, pi].

* 'arcTan': angle of a vector, @atan2(y, x)@. Inputs: X and Y in [-1, 1].

Without coarse rotation ('E.coarseRotation'), phases are limited to
[-pi\/4, pi\/4] and vectors to angles in that range.

= Data formats

The widths of the inputs and outputs of the IP are given by the types of the
arguments and results:

* X and Y values are signed fixed point numbers with two integer bits (one of
  which is the sign): 'SFixed' @2@ @f@. This is the IP's "1QN" format.

* Phases are signed fixed point numbers with three integer bits: 'SFixed' @3@
  @f@, the IP's "2QN" format. They are in radians, or in units of pi radians
  if the phase format is 'E.ScaledRadians'.

The IP's input width applies to all inputs and its output width to all outputs.
Functions that take or produce both X\/Y values and phases are therefore
parameterized by the number of fractional bits of the phase: the X and Y values
have one more fractional bit.

= Configuration

All IP instantiated by this module has the following properties:

* Parallel architecture with maximum pipelining
* Non-blocking AXI4-Stream interfaces, no reset input, no @TLAST@ or @TUSER@
* @TVALID@ on the inputs is always asserted, @TVALID@ on the output is ignored
* One result per clock cycle

The latency of the IP is the delay argument of the resulting 'DSignal'. Unlike
the Floating-Point IP, the CORDIC IP's latency cannot be configured: it is
determined by the function and its configuration. The delay must therefore
equal the IP's latency. Simulation checks this, and fails with an error message
stating the correct latency if it does not. HDL generation does not check it.
For each function, there is a type family for the latency with the default
configuration (e.g. 'E.SinCosDefDelay') and a function computing the latency for
any configuration (e.g. 'E.latencySinCos'). The latencies were measured in
Vivado's simulator on 7 series and UltraScale+ devices.

Other customization is done through the 'E.Config' argument of customizable
functions, see 'E.Config' and 'E.defConfig'. Unlike the configuration of the
Floating-Point IP, these options affect the results.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

= Rounding

The IP truncates its outputs by default. It also supports three ways of
rounding to nearest (see 'E.RoundMode'). Note that 'E.RoundNearestEven' rounds ties
of /negative/ results towards an /odd/ result. For example, if the result
before rounding is -2.5 times the least significant bit of the output, the IP
gives -3 times the least significant bit.

= Not supported

* The word serial architecture, and pipelining modes other than maximum
  pipelining
* Blocking AXI4-Stream interfaces, resets, @TLAST@ and @TUSER@
* Compensation scaling with DSP slices (/Embedded Multiplier/), see
  'E.CompensationScaling'
-}

module Clash.Cores.Xilinx.Cordic
  ( -- * Instantiating IP
    rotateWith
  , rotate
  , E.RotateDefDelay
  , translateWith
  , translate
  , E.TranslateDefDelay
  , sinCosWith
  , sinCos
  , E.SinCosDefDelay
  , arcTanWith
  , arcTan
  , E.ArcTanDefDelay
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.RoundMode(..)
  , E.PhaseFormat(..)
  , E.CompensationScaling(..)
    -- * Latency
  , E.latencyRotate
  , E.latencyTranslate
  , E.latencySinCos
  , E.latencyArcTan
  ) where

import Clash.Prelude hiding (rotate)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Xilinx.Cordic.Explicit as E

-- | Customizable vector rotation, see 'E.rotateWith'.
rotateWith ::
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  E.Config ->
  -- | Vector @(x, y)@
  DSignal dom n (SFixed 2 (fIn + 1), SFixed 2 (fIn + 1)) ->
  -- | Phase
  DSignal dom n (SFixed 3 fIn) ->
  -- | Rotated vector
  DSignal dom (n + d) (SFixed 2 (fOut + 1), SFixed 2 (fOut + 1))
rotateWith cfg = withFrozenCallStack $ hideEnable $ hideClock $ E.rotateWith cfg
{-# INLINE rotateWith #-}

-- | Vector rotation with default settings, see 'E.rotate'.
rotate ::
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  DSignal dom n (SFixed 2 (fIn + 1), SFixed 2 (fIn + 1)) ->
  DSignal dom n (SFixed 3 fIn) ->
  DSignal dom (n + E.RotateDefDelay fOut) (SFixed 2 (fOut + 1), SFixed 2 (fOut + 1))
rotate = withFrozenCallStack $ hideEnable $ hideClock E.rotate
{-# INLINE rotate #-}

-- | Customizable vector translation, see 'E.translateWith'.
translateWith ::
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  E.Config ->
  DSignal dom n (SFixed 2 fIn, SFixed 2 fIn) ->
  DSignal dom (n + d) (SFixed 2 (fOut + 1), SFixed 3 fOut)
translateWith cfg = withFrozenCallStack $ hideEnable $ hideClock $ E.translateWith cfg
{-# INLINE translateWith #-}

-- | Vector translation with default settings, see 'E.translate'.
translate ::
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  DSignal dom n (SFixed 2 fIn, SFixed 2 fIn) ->
  DSignal dom (n + E.TranslateDefDelay fOut) (SFixed 2 (fOut + 1), SFixed 3 fOut)
translate = withFrozenCallStack $ hideEnable $ hideClock E.translate
{-# INLINE translate #-}

-- | Customizable sine and cosine, see 'E.sinCosWith'.
sinCosWith ::
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  E.Config ->
  DSignal dom n (SFixed 3 fIn) ->
  DSignal dom (n + d) (SFixed 2 fOut, SFixed 2 fOut)
sinCosWith cfg = withFrozenCallStack $ hideEnable $ hideClock $ E.sinCosWith cfg
{-# INLINE sinCosWith #-}

-- | Sine and cosine with default settings, see 'E.sinCos'.
sinCos ::
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  DSignal dom n (SFixed 3 fIn) ->
  DSignal dom (n + E.SinCosDefDelay fOut) (SFixed 2 fOut, SFixed 2 fOut)
sinCos = withFrozenCallStack $ hideEnable $ hideClock E.sinCos
{-# INLINE sinCos #-}

-- | Customizable arctangent, see 'E.arcTanWith'.
arcTanWith ::
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  E.Config ->
  DSignal dom n (SFixed 2 fIn, SFixed 2 fIn) ->
  DSignal dom (n + d) (SFixed 3 fOut)
arcTanWith cfg = withFrozenCallStack $ hideEnable $ hideClock $ E.arcTanWith cfg
{-# INLINE arcTanWith #-}

-- | Arctangent with default settings, see 'E.arcTan'.
arcTan ::
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  DSignal dom n (SFixed 2 fIn, SFixed 2 fIn) ->
  DSignal dom (n + E.ArcTanDefDelay fOut) (SFixed 3 fOut)
arcTan = withFrozenCallStack $ hideEnable $ hideClock E.arcTan
{-# INLINE arcTan #-}
