{-|
Copyright  :  (C) 2026,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Divider Generator LogiCORE IP v5.1](https://docs.amd.com/v/u/en-US/pg151-div-gen).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @div_gen@/.../@.clash.tcl@ which needs to be executed in the Vivado
project to create the proper entity. Simulation in Clash produces bit-identical
results to the IP, cycle for cycle.

All IP instantiated by this module has the following properties:

* Non-blocking AXI4-Stream interfaces. @TVALID@ on the inputs is always
  asserted, @TVALID@ on the output is ignored.
* 1 cycle per division: a new division can start every clock cycle, and the
  result of each comes out after a fixed latency.
* A clock enable, no reset input
* Division by zero detection. Results of a division by zero are 'Nothing'.
  The IP's quotient and remainder are undefined in that case.
* No @TLAST@, no @TUSER@ on the inputs

The latency of the IP is set through the delay argument of the 'DSignal'.
Other customization is done through the 'Config' argument of 'quotRemWith'.
For each operation, there also exists a function that uses the defaults: the
maximum (fully pipelined) latency of the IP and, for 'quotRem', the settings
from 'defConfig'. That latency is also defined as a type for delay annotation
in circuits.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

The IP supports the following operations, see PG151 for details:

* 'quotRem': integer division with remainder, using the IP's /Radix2/ or
  /LutMult/ algorithm and its /Remainder/ output
* 'quotFrac': division with a quotient and a fractional part, using the
  IP's /Radix2/ algorithm and its /Fractional/ output

Not supported:

* Blocking mode, back pressure (@TREADY@), reset
* Passing @TLAST@ and @TUSER@ from the inputs to the output
* /Radix2/ with more than one clock cycle per division
* A latency of zero: the IP then has no clock input
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Divider.Explicit
  ( -- * Instantiating IP
    quotRemWith
  , quotRem
  , quotFracWith
  , quotFrac
    -- * Operands
  , DividerOperand(Fraction, FractionWidth, QuotRemDefDelay, QuotFracDefDelay)
    -- * Customizing IP
  , Config(..)
  , defConfig
  , Algorithm(..)
  ) where

import Clash.Explicit.Prelude hiding (quotRem)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Xilinx.Divider.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (OptionName, XilinxWizardOption(..))

import qualified Clash.Signal.Delayed as D

-- | Customizable integer division with remainder.
--
-- The quotient is truncated towards zero, and the remainder has the sign of
-- the dividend, like Haskell's 'Prelude.quotRem'. When dividing the most
-- negative dividend by -1, the quotient wraps around to the most negative
-- dividend. Division by zero gives 'Nothing'.
--
-- The quotient has the width of the dividend, @m@, and the remainder has the
-- width of the divisor, @n@. Both should be between 2 and 64 bits for
-- 'Radix2'; see 'Algorithm' for the limits of 'LutMult'. The latency @d@
-- should be at least 1, and at most 'QuotRemDefDelay' for 'Radix2' or 8
-- (unsigned) or 10 (signed) for 'LutMult'. Vivado reports an error when
-- creating the IP otherwise.
quotRemWith ::
  forall d a m n dom k.
  ( KnownDomain dom
  , DividerOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat d
  , 1 <= d
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Dividend
  DSignal dom k (a m) ->
  -- | Divisor
  DSignal dom k (a n) ->
  -- | Quotient and remainder
  DSignal dom (k + d) (Maybe (a m, a n))
quotRemWith Config{algorithm} clk en x y
  | clashSimulation = sim
  | otherwise = synth
 where
  sim =
        uncurry quotRemModel . operandsFromBits
    <$> delayI und en clk (operandsToBits <$> x <*> y)
  und = withFrozenCallStack $ errorX "Initial values of quotRem undefined"

  synth = D.unsafeFromSignal (toResult <$> tuser <*> tdata)
   where
    (tuser, tdata) =
      dividerInst @d @(ByteWidth m) @(ByteWidth n) @(ByteWidth m + ByteWidth n)
        "quotRem"
        (opts @a @m @n (algorithmToTcl algorithm) "Remainder" (natToNum @n))
        clk en (padOperand x) (padOperand y)

  toResult :: Bit -> BitVector (ByteWidth m + ByteWidth n) -> Maybe (a m, a n)
  toResult divByZero dout
    | divByZero == 1 = Nothing
    | otherwise = Just (operandFromBitVector (resize q), operandFromBitVector (resize r))
   where
    (q, r) = split @_ @(ByteWidth m) @(ByteWidth n) dout
{-# INLINE quotRemWith #-}

-- | Integer division with remainder, with default settings. See 'quotRemWith'.
quotRem ::
  forall a m n dom k.
  ( KnownDomain dom
  , DividerOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat (QuotRemDefDelay a m)
  , 1 <= QuotRemDefDelay a m
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  -- | Dividend
  DSignal dom k (a m) ->
  -- | Divisor
  DSignal dom k (a n) ->
  -- | Quotient and remainder
  DSignal dom (k + QuotRemDefDelay a m) (Maybe (a m, a n))
quotRem = withFrozenCallStack $ quotRemWith @(QuotRemDefDelay a m) defConfig
{-# INLINE quotRem #-}

-- | Customizable division with fractional output, using the IP's /Radix2/
-- algorithm.
--
-- Gives the quotient, as in 'quotRemWith', and the remainder divided by the
-- divisor with @f@ fractional bits, truncated towards zero: the 'Fraction'.
-- For 'Signed' operands, the fraction has the sign of the quotient, so
-- @x / y@ is the quotient plus the fraction (in both cases rounded towards
-- zero). For example, with 5-bit operands and @f = 4@, -9 / 4 gives quotient
-- -2 and fraction -0.25. Division by zero gives 'Nothing'.
--
-- The quotient has the width of the dividend, @m@. The operand widths and the
-- 'FractionWidth' should be between 2 and 64 bits. The latency @d@ should be
-- at least 1 and at most 'QuotFracDefDelay'. Vivado reports an error when
-- creating the IP otherwise.
quotFracWith ::
  forall d a m n f dom k.
  ( KnownDomain dom
  , DividerOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat f
  , KnownNat (FractionWidth a f)
  , KnownNat d
  , 1 <= d
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  -- | Dividend
  DSignal dom k (a m) ->
  -- | Divisor
  DSignal dom k (a n) ->
  -- | Quotient and fraction
  DSignal dom (k + d) (Maybe (a m, Fraction a f))
quotFracWith clk en x y
  | clashSimulation = sim
  | otherwise = synth
 where
  sim =
        uncurry (quotFracModel @a @m @n @f) . operandsFromBits
    <$> delayI und en clk (operandsToBits <$> x <*> y)
  und = withFrozenCallStack $ errorX "Initial values of quotFrac undefined"

  synth = D.unsafeFromSignal (toResult <$> tuser <*> tdata)
   where
    (tuser, tdata) =
      dividerInst @d @(ByteWidth m) @(ByteWidth n) @(ByteWidth (m + FractionWidth a f))
        "quotFrac"
        ( opts @a @m @n "Radix2" "Fractional"
            (natToNum @(FractionWidth a f)) )
        clk en (padOperand x) (padOperand y)

  toResult ::
    Bit ->
    BitVector (ByteWidth (m + FractionWidth a f)) ->
    Maybe (a m, Fraction a f)
  toResult divByZero dout
    | divByZero == 1 = Nothing
    | otherwise = Just (operandFromBitVector q, fractionFromBitVector @a @f r)
   where
    (q, r) = split @_ @m @(FractionWidth a f) (resize dout)
{-# INLINE quotFracWith #-}

-- | Division with fractional output, with the default (maximum) latency. See
-- 'quotFracWith'.
quotFrac ::
  forall a m n f dom k.
  ( KnownDomain dom
  , DividerOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat f
  , KnownNat (FractionWidth a f)
  , KnownNat (QuotFracDefDelay a m f)
  , 1 <= QuotFracDefDelay a m f
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  -- | Dividend
  DSignal dom k (a m) ->
  -- | Divisor
  DSignal dom k (a n) ->
  -- | Quotient and fraction
  DSignal dom (k + QuotFracDefDelay a m f) (Maybe (a m, Fraction a f))
quotFrac = withFrozenCallStack $ quotFracWith @(QuotFracDefDelay a m f)
{-# INLINE quotFrac #-}

-- | IP options for the algorithm, the operand sign, the remainder type and the
-- fractional width. The IP ignores the latter for remainder outputs, but the
-- fractional width should still be in range.
opts ::
  forall a m n.
  (DividerOperand a, KnownNat m, KnownNat n) =>
  String ->
  String ->
  Integer ->
  Vec 6 (OptionName, XilinxWizardOption)
opts algo remType fracWidth =
     ("CONFIG.algorithm_type",              StrOpt algo)
  :> ("CONFIG.operand_sign",                StrOpt (operandSign @a))
  :> ("CONFIG.dividend_and_quotient_width", IntegerOpt (natToNum @m))
  :> ("CONFIG.divisor_width",               IntegerOpt (natToNum @n))
  :> ("CONFIG.remainder_type",              StrOpt remType)
  :> ("CONFIG.fractional_width",            IntegerOpt fracWidth)
  :> Nil
{-# INLINE opts #-}

-- | The simulation models delay their inputs rather than their outputs: the
-- inputs have an 'NFDataX' instance, which 'delayI' needs, for every
-- 'DividerOperand'.
operandsToBits ::
  (DividerOperand a, KnownNat m, KnownNat n) =>
  a m ->
  a n ->
  (BitVector m, BitVector n)
operandsToBits x y = (operandToBitVector x, operandToBitVector y)

operandsFromBits ::
  (DividerOperand a, KnownNat m, KnownNat n) =>
  (BitVector m, BitVector n) ->
  (a m, a n)
operandsFromBits (x, y) = (operandFromBitVector x, operandFromBitVector y)

-- | Pad an operand to whole bytes, as the IP's AXI4-Stream interface expects.
-- The IP ignores the padding.
padOperand ::
  forall a n dom k.
  (DividerOperand a, KnownNat n) =>
  DSignal dom k (a n) ->
  Signal dom (BitVector (ByteWidth n))
padOperand = fmap (resize . operandToBitVector) . D.toSignal
{-# INLINE padOperand #-}
