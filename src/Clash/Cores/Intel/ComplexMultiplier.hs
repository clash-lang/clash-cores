{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's @altmult_complex@ megafunction (the ALTMULT_COMPLEX IP), a
complex multiplier. See /ALTMULT_COMPLEX (Complex Multiplier) IP Core/ in the
[Integer Arithmetic IP Cores User Guide (UG-01063)](https://www.intel.com/content/www/us/en/docs/programmable/683490/).

The megafunction multiplies two complex numbers:

@
(ar :+ ai) * (br :+ bi) = (ar * br - ai * bi) :+ (ar * bi + ai * br)
@

Operand A has components of @a@ bits, operand B components of @b@ bits, and
the product components of @o@ bits. Each operand is either 'Signed' or
'Unsigned'. The product is computed exactly and then wrapped around to @o@
bits: the result keeps the @o@ least significant bits, and is sign extended
if @o@ is larger than the exact product (@a + b + 1@ bits). If both operands
are 'Unsigned', the product is 'Unsigned' and computed modulo @2^(a + b)@:
negative real parts wrap around, and the carry of the imaginary part is lost,
see 'complexMultiplyModel'.

Like @lpm_divide@ ("Clash.Cores.Intel.Divide"), @altmult_complex@ is a
parameterized megafunction that Quartus implements directly: Clash simply
instantiates it, and Quartus generates the multiplier (using the embedded
multipliers of the device) when it synthesizes the design. In VHDL, the
component is the one from @altera_mf.altera_mf_components@ in Quartus's
synthesis libraries.

The latency is set through the delay argument of the 'DSignal'. It can be any
number of clock cycles from 0 (a combinational multiplier) to 14. All pipeline
registers are enabled by the 'Enable', and cleared by the reset, which is
connected to the asynchronous clear (@aclr@). The outputs during reset, and
until the pipeline has filled again, are undefined in simulation.

=== Device families

The user guide lists Arria II, Arria V, Arria 10, Cyclone IV, Cyclone V,
MAX 10 and Stratix V. On Stratix V, Arria V, Cyclone V and Arria 10 only
'Signed' operands are supported: Quartus reports an error otherwise.

=== Simulation

Intel's simulation libraries don't contain a model of @altmult_complex@. The
Haskell model in this module is checked, cycle by cycle, against the netlists
that Quartus Prime Lite 21.1 generates for the megafunction (with its
@clearbox@ tool), for Cyclone IV E, MAX 10 and Cyclone 10 LP, simulated with
the plain atom models of these families. To simulate a design using this
module in an HDL simulator, generate such a netlist for every configuration
used, e.g. with

> clearbox altmult_complex DEVICE_FAMILY="MAX 10" WIDTH_A=8 WIDTH_B=8 WIDTH_RESULT=17 \
>   REPRESENTATION_A=SIGNED REPRESENTATION_B=SIGNED PIPELINE=4 IMPLEMENTATION_STYLE=AUTO \
>   CBX_SINGLE_OUTPUT_FILE=ON CBX_FILE=cmul_8x8.v \
>   dataa_real dataa_imag datab_real datab_imag result_real result_imag clock aclr ena

and a Verilog module @altmult_complex@, with the parameters and ports of the
megafunction, that instantiates the netlist matching its parameters. The
netlists need the atom models of the family (e.g. @fiftyfivenm_atoms.v@) and
@altera_primitives.v@ from @quartus\/eda\/sim_lib@. The tests of this module
in the @clash-cores@ repository do this, see
@hdl-tests/shouldwork/Intel/ComplexMultiplierSim.hs@.

Not supported:

* The synchronous clear (@sclr@), available on Arria 10 only
* The dynamic complex mode (the @complex@ input), available on Stratix V
  only

#differences#

=== Differences from Intel's documentation

* /Unsigned operands./ The
  [Integer Arithmetic IP Cores User Guide](https://www.intel.com/content/www/us/en/docs/programmable/683490/)
  (UG-01063, version 2016.06.10, chapter 12, \"Features\" and Table 12-3)
  lists unsigned operands as supported and lets @WIDTH_RESULT@ go up to 256
  bits, without saying that the product is not exact for a wide enough
  result. If both operands are unsigned, the megafunction computes the real
  and the imaginary part modulo @2^(a + b)@, whatever @WIDTH_RESULT@: a
  negative real part wraps around and the imaginary part loses its carry.
  Found by simulating the netlists Quartus Prime Lite 21.1 generates
  (Cyclone IV E, MAX 10 and Cyclone 10 LP), exhaustively for operands of up
  to 3 bits and on corner cases and random inputs up to 256 bits; see
  'complexMultiplyModel'.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.ComplexMultiplier
  ( -- * Complex multiplication
    complexMultiply
  , complexMultiplyWith
  , ImplementationStyle(..)
    -- * Number representations
  , ComplexRepresentation(..)
  , ProductRepresentation
    -- * Simulation model
  , complexMultiplyModel
  ) where

import Clash.Explicit.Prelude
import Data.Complex (Complex(..), imagPart, realPart)
import Data.Kind (Type)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Signal.Delayed as D

import Clash.Cores.Intel.Floating.Internal (pipeline)
import Clash.Cores.Intel.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (unPort)

-- | How Quartus builds the complex multiplier from simple multipliers, the
-- @IMPLEMENTATION_STYLE@ parameter. It doesn't change the results.
data ImplementationStyle
  = Auto
  -- ^ Let Quartus choose, based on the device family and the widths. This is
  -- the default.
  | Canonical
  -- ^ Three multipliers instead of four, at the expense of more logic.
  -- According to the user guide, this is only available for devices older
  -- than Stratix III, and for widths below 18 bits. Quartus uses 'Auto'
  -- otherwise.
  | Conventional
  -- ^ Four multipliers, with as little logic as possible
  deriving (Eq, Show)

-- | Number representations supported by @altmult_complex@: 'Signed' and
-- 'Unsigned'.
class ComplexRepresentation (f :: Nat -> Type) where
  -- | Value of @representation_a@ or @representation_b@
  representation :: String

instance ComplexRepresentation Signed where
  representation = "SIGNED"

instance ComplexRepresentation Unsigned where
  representation = "UNSIGNED"

-- | Representation of the product of operands with representations @f@ and
-- @g@: 'Unsigned' if both operands are 'Unsigned', 'Signed' otherwise.
type family ProductRepresentation (f :: Nat -> Type) (g :: Nat -> Type) :: Nat -> Type where
  ProductRepresentation Unsigned Unsigned = Unsigned
  ProductRepresentation f g = Signed

-- | Simulation model of @altmult_complex@: the product of two complex
-- numbers, wrapped around to @o@ bits.
--
-- If at least one of the operands is 'Signed', the product is computed
-- exactly and wraps around like 'fromInteger' does: the result consists of
-- the @o@ least significant bits of the exact product. With @o >= a + b + 1@,
-- the result is exact.
--
-- If both operands are 'Unsigned', the product is computed modulo
-- @2^(a + b)@, like 'Unsigned' arithmetic on @a + b@ bits, and then resized to
-- @o@ bits. So the real part @ar * br - ai * bi@ wraps around if it is
-- negative, and the imaginary part @ar * bi + ai * br@, which needs @a + b + 1@
-- bits, loses its most significant bit. This is how @altmult_complex@
-- behaves in Quartus Prime Lite 21.1 (observed for Cyclone IV E, MAX 10 and
-- Cyclone 10 LP), unlike what the user guide suggests, see
-- "Clash.Cores.Intel.ComplexMultiplier#differences".
complexMultiplyModel ::
  forall f g o a b.
  ( KnownNat a
  , KnownNat b
  , ComplexRepresentation f
  , ComplexRepresentation g
  , Integral (f a)
  , Integral (g b)
  , Num (ProductRepresentation f g o)
  ) =>
  Complex (f a) ->
  Complex (g b) ->
  Complex (ProductRepresentation f g o)
complexMultiplyModel (ar :+ ai) (br :+ bi) =
  fromInteger (wrap re) :+ fromInteger (wrap im)
 where
  i :: Integral x => x -> Integer
  i = toInteger
  re = i ar * i br - i ai * i bi
  im = i ar * i bi + i ai * i br
  bothUnsigned = representation @f == "UNSIGNED" && representation @g == "UNSIGNED"
  wrap x
    | bothUnsigned = x `mod` bit (natToNum @(a + b))
    | otherwise = x

-- | Complex multiplication using @altmult_complex@, with 'Auto' as the
-- 'ImplementationStyle'. See the module documentation and
-- 'complexMultiplyModel'.
--
-- For example, the exact product of two complex numbers with 16-bit
-- components, with a latency of 4 clock cycles:
--
-- @
-- mul16 ::
--   KnownDomain dom =>
--   Clock dom ->
--   Reset dom ->
--   Enable dom ->
--   DSignal dom 0 (Complex (Signed 16)) ->
--   DSignal dom 0 (Complex (Signed 16)) ->
--   DSignal dom 4 (Complex (Signed 33))
-- mul16 = complexMultiply \@4
-- @
complexMultiply ::
  forall d o f g a b dom k.
  ( KnownDomain dom
  , KnownNat d
  , d <= 14
  , KnownNat a
  , KnownNat b
  , KnownNat o
  , 1 <= a, a <= 256
  , 1 <= b, b <= 256
  , 1 <= o, o <= 256
  , ComplexRepresentation f
  , ComplexRepresentation g
  , BitPack (f a)
  , BitPack (g b)
  , BitPack (ProductRepresentation f g o)
  , BitSize (ProductRepresentation f g o) ~ o
  , Integral (f a)
  , Integral (g b)
  , Num (ProductRepresentation f g o)
  , NFDataX (ProductRepresentation f g o)
  , HasCallStack
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Operand A
  DSignal dom k (Complex (f a)) ->
  -- | Operand B
  DSignal dom k (Complex (g b)) ->
  -- | Product
  DSignal dom (k + d) (Complex (ProductRepresentation f g o))
complexMultiply = withFrozenCallStack (complexMultiplyWith @d Auto)
{-# INLINE complexMultiply #-}

-- | Like 'complexMultiply', with a configurable 'ImplementationStyle'
complexMultiplyWith ::
  forall d o f g a b dom k.
  ( KnownDomain dom
  , KnownNat d
  , d <= 14
  , KnownNat a
  , KnownNat b
  , KnownNat o
  , 1 <= a, a <= 256
  , 1 <= b, b <= 256
  , 1 <= o, o <= 256
  , ComplexRepresentation f
  , ComplexRepresentation g
  , BitPack (f a)
  , BitPack (g b)
  , BitPack (ProductRepresentation f g o)
  , BitSize (ProductRepresentation f g o) ~ o
  , Integral (f a)
  , Integral (g b)
  , Num (ProductRepresentation f g o)
  , NFDataX (ProductRepresentation f g o)
  , HasCallStack
  ) =>
  ImplementationStyle ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Operand A
  DSignal dom k (Complex (f a)) ->
  -- | Operand B
  DSignal dom k (Complex (g b)) ->
  -- | Product
  DSignal dom (k + d) (Complex (ProductRepresentation f g o))
complexMultiplyWith style clk rst en x y
  | clashSimulation =
      pipeline und clk rst en (complexMultiplyModel <$> x <*> y)
  | otherwise =
      D.unsafeFromSignal ((:+) <$> (unpack <$> unPort re) <*> (unpack <$> unPort im))
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of complexMultiply"
  xs = D.toSignal x
  ys = D.toSignal y
  (re, im) = go
  go ::
    ( Port "result_real" dom (BitVector o)
    , Port "result_imag" dom (BitVector o) )
  go =
    inst
      (instConfig "altmult_complex")
        { library = Just "altera_mf"
        , libraryImport = Just "altera_mf.altera_mf_components.all" }
      (Param @"width_a" @Integer (natToNum @a))
      (Param @"width_b" @Integer (natToNum @b))
      (Param @"width_result" @Integer (natToNum @o))
      (Param @"representation_a" @String (representation @f))
      (Param @"representation_b" @String (representation @g))
      (Param @"pipeline" @Integer (natToNum @d))
      (Param @"implementation_style" @String (styleParam style))
      (ClockPort @"clock" clk)
      (ResetPort @"aclr" @ActiveHigh rst)
      (Port @"ena" (boolToBit <$> fromEnable en))
      (Port @"dataa_real" (pack . realPart <$> xs))
      (Port @"dataa_imag" (pack . imagPart <$> xs))
      (Port @"datab_real" (pack . realPart <$> ys))
      (Port @"datab_imag" (pack . imagPart <$> ys))
{-# INLINE complexMultiplyWith #-}

-- | Value of @implementation_style@
styleParam :: ImplementationStyle -> String
styleParam = \case
  Auto -> "AUTO"
  Canonical -> "CANONICAL"
  Conventional -> "CONVENTIONAL"
