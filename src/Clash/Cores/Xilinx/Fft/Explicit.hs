{-|
Copyright  :  (C) 2026,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Fast Fourier Transform LogiCORE IP v9.1](https://docs.amd.com/v/u/en-US/pg109-xfft).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @fft@/.../@.clash.tcl@ which needs to be executed in the Vivado project
to create the proper entity. Simulation in Clash produces bit-identical results
to the IP, cycle for cycle, as long as the input meets the requirements below.

The IP computes the discrete Fourier transform (or its inverse) of frames of
@N = 2^nfft@ complex samples, with @nfft@ from 3 to 16. All IP instantiated by
this module has the following properties:

* The /Pipelined, Streaming I\/O/ architecture: the IP can take a new sample
  every clock cycle, and transforms frame after frame without gaps.
* Fixed-point input samples of @bw@ bits (8 to 34) for both the real and
  imaginary part, and phase factors of 8 to 34 bits.
* A fixed transform length.
* A synchronous reset, no clock enable.
* The output is always ready (the IP's non-realtime mode, with
  @m_axis_data_tready@ asserted). Every output sample must be consumed in the
  clock cycle in which it comes out.
* The output order is bit reversed or natural, see 'OutputOrdering'.

= Interface

A frame starts with the first sample the IP accepts after a reset, or after the
last sample of the previous frame. The IP accepts a sample in a clock cycle in
which the input is 'Just' and the IP is ready: the first output of the
functions in this module. The IP is ready except during a reset and for three
clock cycles after it.

__The samples of a frame must come in consecutive clock cycles.__ There can be
any number of clock cycles between two frames. The IP itself accepts gaps
within a frame, but then pauses its processing, which delays the output of all
frames in flight in a way that we do not model. The simulation model gives
undefined output from the first gap within a frame until the next reset.

The configuration of a frame is the value of the frame configuration input in
the clock cycle in which the IP accepts its first sample: the 'direction' of the
transform and, for 'Scaled', the 'scaleSchedule'.

The second output gives the samples of the transform as they come out of the
IP: their index, their value, whether it is the last sample of a frame, and
the status of the frame ('FrameStatus').

For example, a forward transform of 64 samples of 16 bits, scaled by @1/N@,
with the output in natural order, on a Kintex-7 FPGA:

> fft64 ::
>   Clock XilinxSystem ->
>   Reset XilinxSystem ->
>   Signal XilinxSystem (Maybe (Complex (Signed 16))) ->
>   ( Signal XilinxSystem Bool
>   , Signal XilinxSystem (Maybe (FftOutput 'Scaled 6 16)) )
> fft64 clk rst =
>   fft @215 defConfig{outputOrdering = NaturalOrder} clk rst
>     (pure (FrameConfig Forward scaleByN))

= Latency

The latency @d@ of the IP is a type argument of the functions in this module.
It is the number of clock cycles from the IP accepting the first sample of a
frame, when it is idle, to the first output sample of that frame. Frames that
directly follow each other have the same latency.

The IP chooses its implementation, and therefore its latency, itself, based
on all its parameters (including the implementation options in t'Config') and
the targeted FPGA family. We do not know a formula for it, so you need to tell
the simulation model. The IP itself ignores @d@: a wrong latency only makes the
simulation model differ from the IP.

To find the latency, simulate the IP once, for example with a test bench that
compares it with the simulation model (a wrong latency shows up as output that
comes too early or too late). The /Latency/ tab of Vivado's IP customization
dialog shows the number of clock cycles from the first input sample to the
last output sample of a frame (PG109, "Information Tabs"); @d@ should be that
number minus @N - 1@.

Some latencies on a Kintex-7 part (@xc7k70t@), with 'defConfig' unless noted:

+------+----------------------+------------+--------------------------------+-----+
| N    | Scaling              | Data bits  | Other options                  | d   |
+======+======================+============+================================+=====+
| 8    | 'Scaled'             | 16         |                                | 45  |
+------+----------------------+------------+--------------------------------+-----+
| 16   | 'Scaled'             | 16         | 'CmpyPerformance',             | 65  |
|      |                      |            | 'ButterflyDsps'                |     |
+------+----------------------+------------+--------------------------------+-----+
| 16   | 'Scaled'             | 8          | 8-bit phase factors,           | 86  |
|      |                      |            | 'NaturalOrder'                 |     |
+------+----------------------+------------+--------------------------------+-----+
| 32   | 'Scaled'             | 16         | 'ConvergentRounding'           | 118 |
+------+----------------------+------------+--------------------------------+-----+
| 32   | 'BlockFloatingPoint' | 16         | 'ConvergentRounding',          | 152 |
|      |                      |            | 'NaturalOrder'                 |     |
+------+----------------------+------------+--------------------------------+-----+
| 64   | 'Scaled'             | 16         | 'NaturalOrder'                 | 215 |
+------+----------------------+------------+--------------------------------+-----+
| 64   | 'BlockFloatingPoint' | 16         |                                | 214 |
+------+----------------------+------------+--------------------------------+-----+
| 128  | 'Unscaled'           | 16         |                                | 217 |
+------+----------------------+------------+--------------------------------+-----+
| 256  | 'Scaled'             | 12         | 20-bit phase factors,          | 367 |
|      |                      |            | 'ConvergentRounding'           |     |
+------+----------------------+------------+--------------------------------+-----+
| 512  | 'Unscaled'           | 24         | 12-bit phase factors           | 625 |
+------+----------------------+------------+--------------------------------+-----+

= Arithmetic

The IP uses a radix-2² decimation-in-frequency FFT. See
'Clash.Cores.Xilinx.Fft.Internal.fftFrameInteger' for where it rounds. With
'Scaled', a frame can overflow: its values then wrap around and are unusable.
The simulation model reports the overflow like the IP, but gives undefined
values for the samples of such a frame.

= Not supported

* The /Radix-4/, /Radix-2/ and /Radix-2 Lite Burst I\/O/ architectures
* Run-time configurable transform length, cyclic prefix insertion,
  floating-point data, multiple channels
* Back pressure on the output, the realtime throttle scheme
* A clock enable: the IP's AXI4-Stream buffers keep running while @aclken@ is
  low, which we did not model
* Gaps within a frame (see above)
* The status channel and event outputs of the IP. Their information is
  available through 'FrameStatus', except for the events that report a
  violation of the requirements above.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Fft.Explicit
  ( -- * Instantiating IP
    fft
    -- * Frames
  , Direction(..)
  , FrameConfig(..)
  , Groups
  , scaleByN
  , FftOutput(..)
    -- * Scaling
  , Scaling(..)
  , KnownScaling(..)
  , KnownFft
  , ByteWidth
  , ArithScaling(..)
    -- * Customizing IP
  , Config(..)
  , defConfig
  , Rounding(..)
  , OutputOrdering(..)
  , ComplexMultiplier(..)
  , ButterflyArithmetic(..)
  ) where

import Clash.Explicit.Prelude

import Data.Complex (Complex)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Xilinx.Fft.Internal

{- | Transform frames of @2^nfft@ complex samples of @bw@ bits, with latency @d@.
See the module documentation for the requirements on the input, and the
meaning of @d@.

The scaling @s@ determines the width of the output samples ('OutputWidth'), the
scaling schedule in the frame configuration ('ScaleSchedule') and the status of
the output samples ('FrameStatus'). The latency should be at least 7.
-}
fft ::
  forall d s nfft bw dom.
  ( KnownDomain dom
  , KnownNat d
  , 7 <= d
  , KnownFft s nfft bw
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  -- | Synchronous reset. Assert it for at least two clock cycles.
  Reset dom ->
  -- | Configuration of the frame that starts in this clock cycle
  Signal dom (FrameConfig s nfft) ->
  -- | Input samples
  Signal dom (Maybe (Complex (Signed bw))) ->
  -- | Whether the IP accepts a sample in this clock cycle, and the output
  -- samples
  (Signal dom Bool, Signal dom (Maybe (FftOutput s nfft (OutputWidth s nfft bw))))
fft cfg clk rst frameCfg input
  | clashSimulation = withFrozenCallStack sim
  | otherwise = fftSynth cfg clk rst frameCfg input
 where
  sim = unbundle (fftModel @d cfg (unsafeToActiveHigh rst) frameCfg input)
{-# INLINE fft #-}
