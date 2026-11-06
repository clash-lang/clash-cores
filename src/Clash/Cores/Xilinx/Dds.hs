{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx DDS Compiler LogiCORE IP v6.0](https://docs.amd.com/v/u/en-US/pg141-dds-compiler).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @dds_compiler@/.../@.clash.tcl@ which needs to be executed in the Vivado
project to create the proper entity. Simulation in Clash produces bit-identical
results to the IP, cycle by cycle.

A direct digital synthesizer (DDS), or numerically controlled oscillator
(NCO), consists of two parts, which the IP can implement together or on their
own:

* A /phase generator/ ('phaseGeneratorWith'): an accumulator that adds a phase
  increment to the phase every sample, followed by an adder for a phase
  offset.

* A /SIN\/COS lookup table/ ('sinCosLutWith'): it converts a phase to its sine
  and\/or cosine.

* Both together ('ddsWith'): a complete DDS, which produces a sine and\/or
  cosine at a frequency set by the phase increment.

= Phases and samples

Phases are fractions of a full cycle, of type 'E.Phase' @pw@ (an 'SFixed' @0@
@pw@): @0.25@ is a quarter of a cycle, or 90 degrees. They wrap around, so the
range @[-0.5, 0.5)@ covers the full circle (@0.5@ wraps to @-0.5@, the same
phase). The phase width @pw@ is the width of the IP's phase accumulator
(/Phase Width/ in PG141). The phase increment is the output frequency divided
by the clock frequency, so the frequency resolution is the clock frequency
divided by @2^pw@. Negative increments give negative frequencies.

The sine and cosine are 'E.Sample' @f@ values (an 'SFixed' @1@ @f@), with an
output width of @f + 1@ bits (/Output Width/ in PG141). With the default
'E.FullRange' amplitude, the amplitude is @1 - 2^-(f - 1)@; with 'E.UnitCircle'
it is @1\/2@.

The IP is configured with /Hardware Parameters/ (see /Parameter Selection/ in
PG141): the types give the phase and output widths, and the phase increment
and offset are phases. PG141 explains how its /System Parameters/ (spurious
free dynamic range and frequency resolution) map to these: the phase width
follows from the frequency resolution, and the output width is one bit per 6
dB of SFDR.

The IP supports these widths:

* The phase width is 3 to 48 bits, or 3 to 16 bits for the SIN\/COS LUT only.
* The output width (@f + 1@) is 3 to 26 bits.
* For a complete DDS, at least one of them is at most 16 bits: the lookup
  table has @2^min(pw, f + 1)@ entries, which the phase addresses with its
  most significant bits.

= Phase increment and offset

The phase increment ('E.Increment') and offset ('E.Offset') can each be:

* /fixed/ when the IP is generated;

* /programmable/: set at run time through the IP's CONFIG channel, a
  @Signal dom (Maybe (increment, offset))@ argument. A 'Just' in a clock
  cycle sets the values used from the next clock cycle on.

* /streaming/: a new value for every sample, from the IP's input PHASE channel,
  a @DSignal dom n (increment, offset)@ argument. A streaming increment can
  come with a resynchronization flag, which resets the accumulated phase.

The phase offset can also be absent. The types of the CONFIG and input PHASE
channel follow from these choices: the type of an increment or offset that is
not programmable or streaming is @()@ in that channel. For example, a DDS with
a programmable increment (to set its frequency) and a streaming offset (for
phase modulation) has type:

@
fmPm ::
  (HiddenClock dom, HiddenEnable dom) =>
  Signal dom (Maybe (Phase 32, ())) ->
  DSignal dom n ((), Phase 32) ->
  DSignal dom (n + 6) (Sample 15, Sample 15)
fmPm = ddsWith defConfig (ProgrammableIncrement 0.01) StreamingOffset SineCosineOutput
@

and a fixed frequency quadrature oscillator:

@
oscillator :: (HiddenClock dom, HiddenEnable dom) => DSignal dom 6 (Sample 15, Sample 15)
oscillator = dds (0.0123 :: Phase 32)
@

= Timing

All IP instantiated by this module produces one output sample per clock
cycle, with a fixed latency: it uses non-blocking AXI4-Stream interfaces, the
input PHASE channel is always valid, and the output's @TVALID@ is ignored.
Without an input PHASE channel, the input is a @DSignal dom n ((), ())@ that
acts as the clock of the samples: the IP produces a sample every clock cycle
(@pure ((), ())@ does the job).

The latency of the IP is the delay @d@ of the resulting 'DSignal', which
configures the IP's latency (/Latency Options/ in PG141). The first @d@
outputs are undefined. The IP supports latencies of up to 21 cycles, and at
least:

* 1 cycle for a phase generator, or a SIN\/COS LUT;
* 1 cycle for a complete DDS with its lookup table in distributed memory, and
  2 cycles with its lookup table in block RAM. With 'E.AutoMemory', the IP uses
  block RAM for tables of more than 32 entries: if both the phase width and
  the output width are more than 5 bits.

Vivado and the simulation model report an error for other latencies. For
reference, the latencies Vivado picks for the highest clock frequency
(/Auto/ latency) are:

* for a complete DDS: 7 cycles if the table has more than 1024 entries, else
  3, plus one if there is a phase offset;
* for a SIN\/COS LUT: 6 cycles for a phase of more than 10 bits, else 2;
* for a phase generator: 1 cycle, plus one if there is a phase offset.

De-asserting the 'Enable' signal of a function stalls the whole IP: the
pipeline, the phase accumulator and the CONFIG channel.

= Not supported

* Phase dithering (/Noise Shaping/ \"Phase Dithering\"): the IP adds a pseudo
  random sequence to the phase, which PG141 does not specify.
* Taylor series corrected noise shaping. The IP corrects the lookup table with
  a multiplication by the truncated part of the phase, but PG141 does not
  document the widths and rounding of that datapath, and comparing with
  Xilinx's C model did not pin them down exactly.
* Rasterized mode
* More than one (time division multiplexed) channel
* Blocking AXI4-Stream interfaces (@TREADY@), resets, @TLAST@ and @TUSER@
* The phase output of a complete DDS: use 'phaseGeneratorWith' and
  'sinCosLutWith' if you need both phase and sine\/cosine.
* Sign and magnitude outputs (/Output Form/)
* DSP slices for the phase accumulator (/DSP48 Use/), and the optimization goal
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Dds
  ( -- * Instantiating IP
    ddsWith
  , dds
  , phaseGeneratorWith
  , phaseGenerator
  , sinCosLutWith
  , sinCos
    -- * Phases and samples
  , E.Phase
  , E.Sample
    -- * Phase increment and offset
  , E.Increment(..)
  , E.Offset(..)
  , E.ConfigUpdate
  , E.PhaseInput
    -- * Outputs
  , E.Output(..)
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.NoiseShaping(..)
  , E.AmplitudeMode(..)
  , E.MemoryType(..)
    -- * Field packing
  , E.Field(..)
  , E.IncrementField(..)
  , E.DataOutput(..)
  , E.PhaseChannel
  , E.PhaseInputWidth
  , E.Pad8
  ) where

import Clash.Prelude

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Xilinx.Dds.Explicit as E
import Clash.Cores.Xilinx.Dds.Explicit (Phase, Sample)

-- | Customizable DDS, see 'E.ddsWith'.
ddsWith ::
  forall d dom n pw f is ic os oc o.
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat pw
  , KnownNat f
  , E.PhaseChannel is os ic oc
  , E.DataOutput o
  , HasCallStack
  ) =>
  E.Config ->
  E.Increment pw is ic ->
  E.Offset pw os oc ->
  E.Output f o ->
  -- | CONFIG channel
  Signal dom (Maybe (E.ConfigUpdate ic oc)) ->
  -- | Input PHASE channel
  DSignal dom n (E.PhaseInput is os) ->
  DSignal dom (n + d) o
ddsWith cfg inc off out =
  withFrozenCallStack $ hideEnable $ hideClock $ E.ddsWith @d cfg inc off out
{-# INLINE ddsWith #-}

-- | Quadrature oscillator with a fixed phase increment, see 'E.dds'.
dds ::
  forall d dom pw f.
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat pw
  , KnownNat f
  , HasCallStack
  ) =>
  -- | Phase increment: the output frequency divided by the clock frequency
  Phase pw ->
  DSignal dom d (Sample f, Sample f)
dds = withFrozenCallStack $ hideEnable $ hideClock $ E.dds @d
{-# INLINE dds #-}

-- | Customizable phase generator, see 'E.phaseGeneratorWith'.
phaseGeneratorWith ::
  forall d dom n pw is ic os oc.
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat pw
  , E.PhaseChannel is os ic oc
  , HasCallStack
  ) =>
  E.Increment pw is ic ->
  E.Offset pw os oc ->
  -- | CONFIG channel
  Signal dom (Maybe (E.ConfigUpdate ic oc)) ->
  -- | Input PHASE channel
  DSignal dom n (E.PhaseInput is os) ->
  DSignal dom (n + d) (Phase pw)
phaseGeneratorWith inc off =
  withFrozenCallStack $ hideEnable $ hideClock $ E.phaseGeneratorWith @d inc off
{-# INLINE phaseGeneratorWith #-}

-- | Phase generator with a fixed phase increment, see 'E.phaseGenerator'.
phaseGenerator ::
  forall d dom pw.
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat pw
  , HasCallStack
  ) =>
  -- | Phase increment
  Phase pw ->
  DSignal dom d (Phase pw)
phaseGenerator = withFrozenCallStack $ hideEnable $ hideClock $ E.phaseGenerator @d
{-# INLINE phaseGenerator #-}

-- | Customizable SIN\/COS lookup table, see 'E.sinCosLutWith'.
sinCosLutWith ::
  forall d dom n pw f o.
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat pw
  , KnownNat f
  , E.DataOutput o
  , HasCallStack
  ) =>
  E.Config ->
  E.Output f o ->
  DSignal dom n (Phase pw) ->
  DSignal dom (n + d) o
sinCosLutWith cfg out = withFrozenCallStack $ hideEnable $ hideClock $ E.sinCosLutWith @d cfg out
{-# INLINE sinCosLutWith #-}

-- | Sine and cosine of a phase, see 'E.sinCos'.
sinCos ::
  forall d dom n pw f.
  ( HiddenClock dom
  , HiddenEnable dom
  , KnownNat d
  , KnownNat pw
  , KnownNat f
  , HasCallStack
  ) =>
  DSignal dom n (Phase pw) ->
  DSignal dom (n + d) (Sample f, Sample f)
sinCos = withFrozenCallStack $ hideEnable $ hideClock $ E.sinCos @d
{-# INLINE sinCos #-}
