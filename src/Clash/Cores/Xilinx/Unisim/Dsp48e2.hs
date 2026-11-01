{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

The @DSP48E2@ primitive of UltraScale and UltraScale+ FPGAs: a 27x18 multiplier
with a 27-bit pre-adder, followed by a 48-bit ALU with an accumulator, SIMD
modes, a pattern detector, and cascade connections to neighboring slices. See
[UG579](https://docs.amd.com/r/en-US/ug579-ultrascale-dsp) for a description of
its functionality and
[UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/DSP48E2)
for the instantiation template.

'dsp48e2' exposes the primitive as is: the attributes are set by a
'Dsp48e2Config', and every input and output port is a field of
'Dsp48e2Input' and 'Dsp48e2Output'. The Haskell simulation model is a
cycle-accurate translation of the UNISIM simulation model (@DSP48E2.v@) of
Vivado 2022.1, and is tested against it. In particular:

  * After configuration of the FPGA, all registers are zero, regardless of the
    initial value behavior of the domain.

  * Like the UNISIM model, the model leaves the 'carryOut' bits that are not
    meaningful undefined: the bits of the SIMD lanes that are not used, and
    bits 0 to 2 when multiplying (@OPMODE[3:0] = 0101@) or doing a logic
    operation (@ALUMODE[3:2] /= 00@). 'overflow' and 'underflow' are undefined
    when the pattern detector is disabled.

  * The UNISIM model warns that some operating modes \"may show deviation
    between simulation results and hardware behavior\". In these modes, the
    outputs of the ALU ('dataP', 'pcOut', 'carryOut', 'carryCascOut',
    'multSignOut', 'xorOut' and, in turn, the pattern detector outputs) are
    undefined, as is the output register once it has captured such a value.
    These modes are:

      * @OPMODE[1:0] = 11@ (A:B) when @USE_MULT = MULTIPLY@ and (A, B, and M)
        or (A, B, and P) or (M and P) are not registered.

      * @OPMODE[3:0] = 0101@ (multiplier) when @USE_MULT = NONE@ or when using
        SIMD.

      * @CARRYINSEL = 010@ (@CARRYCASCIN@) when multiplying.

    The model also considers these modes invalid:

      * Selecting only one of the two partial products of the multiplier
        (@OPMODE[1:0] = 01@ but @OPMODE[3:2] /= 01@, or vice versa). UG579
        requires selecting both, and the UNISIM model's partial products differ
        from the hardware's.

      * Selecting the P feedback (@OPMODE[8:7] = 01@, @OPMODE[1:0] = 10@,
        @OPMODE[6:4] = 010@, @100@ or @11x@, or @CARRYINSEL = 100@, @101@ or
        @111@) without the output register (@PREG = 0@). The feedback comes
        from the output register, so the Verilog UNISIM model uses the unused
        (zero) output register, whereas the VHDL UNISIM model creates a
        combinational loop.

  * Static attribute combinations that the UNISIM model rejects are rejected
    by 'dsp48e2' as well, both in simulation and when generating HDL. See
    'Dsp48e2Config'.

  * The UNISIM model drives @MULTSIGNOUT@ with the carry out of the 49th bit of
    the ALU, which the model copies. Its value is only meaningful in the
    \"MACC extend\" configuration described in UG579. When @OPMODE[6:4] = 100@
    and @OPMODE[1:0] = 00@, the X multiplexer selects @MULTSIGNIN@ shifted left
    by one, like the Verilog UNISIM model. (The VHDL UNISIM model doesn't shift
    it, which is inconsistent with how @MULTSIGNOUT@ is computed.)

  * @IS_CLK_INVERTED@ is set when the domain's active edge is the falling edge.

All registers of the primitive have synchronous resets, and none of them are
connected to the domain's reset: use the reset fields of 'Dsp48e2Input' (e.g.
'rstP') instead.

=== Example

A multiply-accumulate with a latency of three cycles. This is what
'multiplyAccumulate' does, minus the enable:

@
mac ::
  KnownDomain dom =>
  Clock dom ->
  -- | Start a new sum
  Signal dom Bool ->
  Signal dom (Signed 27) ->
  Signal dom (Signed 18) ->
  Signal dom (Signed 48)
mac clk start a b =
  unpack \<$\> dataP (dsp48e2 defDsp48e2Config clk inputs)
 where
  inputs = defDsp48e2Input
    { dataA = resize . pack \<$\> a
    , dataB = pack \<$\> b
      -- The OPMODE register delays the control by one cycle, but the product
      -- arrives at the ALU two cycles after the inputs.
    , opMode = mux (delay clk enableGen False start)
        (pure 0b00_000_01_01) -- P = M
        (pure 0b00_010_01_01) -- P = P + M
    }
@
-}

module Clash.Cores.Xilinx.Unisim.Dsp48e2
  ( -- * Primitive
    dsp48e2
  , Dsp48e2Input(..)
  , defDsp48e2Input
  , Dsp48e2Output(..)
  , cascade

    -- * Convenience functions
  , multiplyAdd
  , multiplyAccumulate

    -- * Configuration
  , Dsp48e2Config(..)
  , defDsp48e2Config
  , InputSource(..)
  , AMultSel(..)
  , BMultSel(..)
  , PreAddInSel(..)
  , UseMult(..)
  , UseSimd(..)
  , XorSimd(..)
  , SelPattern(..)
  , SelMask(..)
  , AutoResetPatDet(..)
  , AutoResetPriority(..)
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Unisim.Dsp48.Internal
import Clash.Cores.Xilinx.Unisim.Dsp48e2.Internal
  hiding (dsp48e2, multiplyAdd, multiplyAccumulate)
import qualified Clash.Cores.Xilinx.Unisim.Dsp48e2.Internal as Internal

-- | Instantiate a @DSP48E2@, configured by a 'Dsp48e2Config'. See the module
-- documentation for the simulation behavior and an example.
dsp48e2 ::
  forall dom.
  (KnownDomain dom, HasCallStack) =>
  Dsp48e2Config ->
  -- | @CLK@
  Clock dom ->
  Dsp48e2Input dom ->
  Dsp48e2Output dom
dsp48e2 = Internal.dsp48e2
{-# INLINE dsp48e2 #-}

-- | Multiply and add on a single @DSP48E2@: @a * b + c@, with a latency of two
-- cycles. A and B go through one input register, C through the C register,
-- and the result through the output register. The multiplier output register
-- is not used.
multiplyAdd ::
  forall dom n.
  (KnownDomain dom, HasCallStack) =>
  Clock dom ->
  -- | Global enable, drives all clock enables
  Enable dom ->
  -- | @a@
  DSignal dom n (Signed 27) ->
  -- | @b@
  DSignal dom n (Signed 18) ->
  -- | @c@
  DSignal dom n (Signed 48) ->
  DSignal dom (n + 2) (Signed 48)
multiplyAdd = Internal.multiplyAdd
{-# INLINE multiplyAdd #-}

-- | Multiply-accumulate on a single @DSP48E2@, with a latency of three cycles:
-- the output is the sum of the products of @a@ and @b@ since the last time
-- @start@ was asserted (including that cycle). All registers on the path from
-- @a@ and @b@ to the output are used.
--
-- The accumulator is 48 bits wide and wraps around on overflow.
multiplyAccumulate ::
  forall dom n.
  (KnownDomain dom, HasCallStack) =>
  Clock dom ->
  -- | Global enable, drives all clock enables
  Enable dom ->
  -- | @start@: start a new sum
  DSignal dom n Bool ->
  -- | @a@
  DSignal dom n (Signed 27) ->
  -- | @b@
  DSignal dom n (Signed 18) ->
  DSignal dom (n + 3) (Signed 48)
multiplyAccumulate = Internal.multiplyAccumulate
{-# INLINE multiplyAccumulate #-}
