{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

The @DSP48E1@ primitive of 7-series FPGAs: a 25x18 multiplier with a 25-bit
pre-adder, followed by a 48-bit ALU with an accumulator, SIMD modes, a pattern
detector, and cascade connections to neighboring slices. See
[UG479](https://docs.amd.com/v/u/en-US/ug479_7Series_DSP48E1) for a
description of its functionality and
[UG953](https://docs.amd.com/r/en-US/ug953-vivado-7series-libraries/DSP48E1)
for the instantiation template.

'dsp48e1' exposes the primitive as is: the attributes are set by a
'Dsp48e1Config', and every input and output port is a field of
'Dsp48e1Input' and 'Dsp48e1Output'. The API mirrors
"Clash.Cores.Xilinx.Unisim.Dsp48e2", and the attribute types shared by both
primitives are the same types. As the field names are the same as well, import
at most one of the two modules unqualified.

The Haskell simulation model is a cycle-accurate translation of the UNISIM
Verilog simulation model (@DSP48E1.v@) of Vivado 2022.1, and is tested against
both the Verilog and the VHDL UNISIM models. In particular:

  * After configuration of the FPGA, all registers are zero, regardless of the
    initial value behavior of the domain.

  * Wherever the UNISIM model drives an output with X, the model's output is
    undefined:

      * The 'carryOut' bits that are not meaningful in the SIMD mode, and all
        'carryOut' bits when multiplying (@OPMODE[3:0] = 0101@) or doing a
        logic operation (@ALUMODE[3:2] /= 00@).

      * 'overflow' and 'underflow' when neither the pattern detector nor the
        output register is used.

      * The multiplier output when the @CARRYINSEL@ pin is @010@ (this ends up
        in the multiplier register when @CEM@ is asserted), and when the
        (registered) @CARRYINSEL@ is @010@.

      * 'patternDetect' and 'patternBDetect' when the combination of
        (registered) @ALUMODE@, @OPMODE@ and @CARRYINSEL@ is invalid. The
        model uses the table of valid combinations from the UNISIM model. Like
        that model, it keeps the result of the previous check when
        @ALUMODE[3:2] = 10@. The UNISIM model can check intermediate
        combinations when several control signals change at the same time, so
        if any of them changed, the result is undefined instead.

    Undefined values propagate through the registers, like in an HDL
    simulator.

  * The model is /more/ undefined than the UNISIM Verilog model in these
    cases:

      * When @OPMODE[1:0] = 11@ (A:B) with @USE_MULT = MULTIPLY@ while (A, B,
        and M) or (A, B, and P) or (M and P) are not registered, the ALU
        outputs are undefined. The Verilog model keeps the previous value of
        its X multiplexer, the VHDL model drives X.

      * The VHDL and Verilog UNISIM models disagree on a few details, in which
        case the model's output is undefined:

          * the internal multiplier carry (@CARRYINSEL = 110@) when the
            INMODE-selected B multiplier input and the B2 register have
            different signs;

          * the registered 'multSignOut', which is the sign of the multiplier
            register in the VHDL model, when it differs from the (selected)
            sign the Verilog model registers;

          * the pattern detector outputs for the rounding masks when they
            depend on the bits shifted into the mask, and whenever any bit of
            the ALU output is undefined (the VHDL model drives them low then);

          * 'carryOut' and 'carryCascOut' while the automatic reset condition
            holds (the VHDL model drives them low then).

      * When the automatic reset condition is undefined, the output registers
        become undefined: the UNISIM models treat an undefined condition as no
        reset, but the condition might be defined in the UNISIM models.

      * Selecting the P feedback (@OPMODE[1:0] = 10@, @OPMODE[6:4] = 010@,
        @100@ or @11x@, or @CARRYINSEL = 100@, @101@ or @111@) without the
        output register (@PREG = 0@) makes the ALU outputs undefined. This is a
        combinational loop in the UNISIM models.

      * The UNISIM models don't check the OPMODE during the first 100 ns of a
        simulation. The Haskell model has no notion of time, and checks it
        from the first cycle.

      * An undefined value is undefined as a whole in many places where an HDL
        simulator would still produce some defined bits.

  * Static attribute combinations that the UNISIM model rejects are rejected
    by 'dsp48e1' as well, both in simulation and when generating HDL. See
    'Dsp48e1Config'.

  * @IS_CLK_INVERTED@ is set when the domain's active edge is the falling edge.

All registers of the primitive have synchronous resets, and none of them are
connected to the domain's reset: use the reset fields of 'Dsp48e1Input' (e.g.
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
  Signal dom (Signed 25) ->
  Signal dom (Signed 18) ->
  Signal dom (Signed 48)
mac clk start a b =
  unpack \<$\> dataP (dsp48e1 defDsp48e1Config clk inputs)
 where
  inputs = defDsp48e1Input
    { dataA = resize . pack \<$\> a
    , dataB = pack \<$\> b
      -- The OPMODE register delays the control by one cycle, but the product
      -- arrives at the ALU two cycles after the inputs.
    , opMode = mux (delay clk enableGen False start)
        (pure 0b000_01_01) -- P = M
        (pure 0b010_01_01) -- P = P + M
    }
@
-}

module Clash.Cores.Xilinx.Unisim.Dsp48e1
  ( -- * Primitive
    dsp48e1
  , Dsp48e1Input(..)
  , defDsp48e1Input
  , Dsp48e1Output(..)
  , cascade

    -- * Convenience functions
  , multiplyAdd
  , multiplyAccumulate

    -- * Configuration
  , Dsp48e1Config(..)
  , defDsp48e1Config
  , InputSource(..)
  , UseMult(..)
  , UseSimd(..)
  , SelPattern(..)
  , SelMask(..)
  , AutoResetPatDet(..)
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Unisim.Dsp48.Internal
import Clash.Cores.Xilinx.Unisim.Dsp48e1.Internal
  hiding (dsp48e1, multiplyAdd, multiplyAccumulate)
import qualified Clash.Cores.Xilinx.Unisim.Dsp48e1.Internal as Internal

-- | Instantiate a @DSP48E1@, configured by a 'Dsp48e1Config'. See the module
-- documentation for the simulation behavior and an example.
dsp48e1 ::
  forall dom.
  (KnownDomain dom, HasCallStack) =>
  Dsp48e1Config ->
  -- | @CLK@
  Clock dom ->
  Dsp48e1Input dom ->
  Dsp48e1Output dom
dsp48e1 = Internal.dsp48e1
{-# INLINE dsp48e1 #-}

-- | Multiply and add on a single @DSP48E1@: @a * b + c@, with a latency of two
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
  DSignal dom n (Signed 25) ->
  -- | @b@
  DSignal dom n (Signed 18) ->
  -- | @c@
  DSignal dom n (Signed 48) ->
  DSignal dom (n + 2) (Signed 48)
multiplyAdd = Internal.multiplyAdd
{-# INLINE multiplyAdd #-}

-- | Multiply-accumulate on a single @DSP48E1@, with a latency of three cycles:
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
  DSignal dom n (Signed 25) ->
  -- | @b@
  DSignal dom n (Signed 18) ->
  DSignal dom (n + 3) (Signed 48)
multiplyAccumulate = Internal.multiplyAccumulate
{-# INLINE multiplyAccumulate #-}
