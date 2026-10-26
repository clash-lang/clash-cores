{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
                  2022,      Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://docs.amd.com/v/u/en-US/pg060-floating-point)
in single precision ('Float'), with explicit clock and enable arguments.

"Clash.Cores.Xilinx.Floating" has the same functions with a hidden clock and
enable. See there for how the functions instantiate the IP, how they behave and
the maximum latency of each operation.

Related modules:

* "Clash.Cores.Xilinx.Floating.Half.Explicit" and
  "Clash.Cores.Xilinx.Floating.Double.Explicit" are the same for half and
  double precision.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Clash.Cores.Xilinx.Floating.Explicit
  ( -- * Instantiating IP
    addWith
  , add
  , AddDefDelay
  , subWith
  , sub
  , SubDefDelay
  , mulWith
  , mul
  , MulDefDelay
  , divWith
  , div
  , DivDefDelay
  , Ordering(..)
  , toMaybeOrdering
  , compare
  , compareWith
  , CompareDefDelay
  , fromU32With
  , fromU32
  , FromU32DefDelay
  , fromS32With
  , fromS32
  , FromS32DefDelay
  , sqrtWith
  , sqrt
  , SqrtDefDelay
  , abs
  , fromU64With
  , fromU64
  , FromU64DefDelay
  , fromS64With
  , fromS64
  , FromS64DefDelay
  , toS32With
  , toS32
  , ToS32DefDelay
  , toHalfWith
  , toHalf
  , ToHalfDefDelay
  , toDoubleWith
  , toDouble
  , ToDoubleDefDelay
  , recipWith
  , recip
  , RecipDefDelay
  , rsqrtWith
  , rsqrt
  , RSqrtDefDelay
  , expWith
  , exp
  , ExpDefDelay
  , logWith
  , log
  , LogDefDelay
  , fmaWith
  , fma
  , FmaDefDelay
  , accumWith
  , accum
  , accumWithFlags
  , AccumDefDelay
    -- * Customizing IP
  , Config(..)
  , defConfig
  , ArchOpt(..)
  , DspUsage(..)
  , BMemUsage(..)
  , AccumConfig(..)
  , defAccumConfig
  , AccumFlags(..)
    -- * Additional functions
  , xilinxNaN
  ) where

import Clash.Explicit.Prelude hiding
  (Ordering(..), add, sub, mul, div, compare, sqrt, abs, recip, exp, log)

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric.Half (Half)

import Clash.Cores.Xilinx.Floating.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | Customizable floating point addition.
addWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
addWith Config{archOpt, dspUsage} clk en x y
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxBinary (+) <$> x <*> y)
  und = withFrozenCallStack $ deepErrorX "Initial values of add undefined"
  extraOpts =
       ("CONFIG.Add_Sub_Value",  StrOpt "Add")
    :> ("CONFIG.C_Optimization", StrOpt (archOptToTcl archOpt))
    :> ("CONFIG.C_Mult_Usage",   StrOpt (dspUsageToTcl dspUsage))
    :> precisionOpts "Single"
  synth = binaryInst "add" "Add_Subtract" extraOpts clk en x y
{-# INLINE addWith #-}

-- | Floating point addition with default settings.
add
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + AddDefDelay) Float
add = withFrozenCallStack $ addWith defConfig
{-# INLINE add #-}

-- | The default delay for floating point addition with default customization.
type AddDefDelay = 11

-- | Customizable floating point subtraction.
subWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
subWith Config{archOpt, dspUsage} clk en x y
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxBinary (-) <$> x <*> y)
  und = withFrozenCallStack $ deepErrorX "Initial values of sub undefined"
  extraOpts =
       ("CONFIG.Add_Sub_Value",  StrOpt "Subtract")
    :> ("CONFIG.C_Optimization", StrOpt (archOptToTcl archOpt))
    :> ("CONFIG.C_Mult_Usage",   StrOpt (dspUsageToTcl dspUsage))
    :> precisionOpts "Single"
  synth = binaryInst "sub" "Add_Subtract" extraOpts clk en x y
{-# INLINE subWith #-}

-- | Floating point subtraction with default settings.
sub
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + SubDefDelay) Float
sub = withFrozenCallStack $ subWith defConfig
{-# INLINE sub #-}

-- | The default delay for floating point subtraction with default
-- customization.
type SubDefDelay = 11

-- | Customizable floating point multiplication.
mulWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
mulWith Config{dspUsage} clk en x y
  | clashSimulation = sim
  | otherwise = synth
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of mul undefined"
  sim = delayI und en clk (xilinxBinary (*) <$> x <*> y)
  -- Only double precision has a low-latency multiplier: in half and single
  -- precision, Vivado warns about any other setting than speed-optimized.
  extraOpts =
       ("CONFIG.C_Mult_Usage", StrOpt (dspUsageToTcl dspUsage))
    :> ("CONFIG.C_Optimization", StrOpt (archOptToTcl SpeedArch))
    :> precisionOpts "Single"
  synth = binaryInst "mul" "Multiply" extraOpts clk en x y
{-# INLINE mulWith #-}

-- | Floating point multiplication with default settings.
mul
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + MulDefDelay) Float
mul = withFrozenCallStack $ mulWith defConfig
{-# INLINE mul #-}

-- | The default delay for floating point multiplication with default
-- customization.
type MulDefDelay = 8

-- | Customizable floating point division.
divWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
divWith !_ clk en x y
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxBinary (/) <$> x <*> y)
  und = withFrozenCallStack $ deepErrorX "Initial values of div undefined"
  synth = binaryInst "div" "Divide" (precisionOpts "Single") clk en x y
{-# INLINE divWith #-}

-- | Floating point division with default settings.
div
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + DivDefDelay) Float
div = withFrozenCallStack $ divWith defConfig
{-# INLINE div #-}

-- | The default delay for floating point division with default customization.
type DivDefDelay = 28

-- | Customizable conversion of @Unsigned 32@ to @Float@
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU32With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Unsigned 32)
  -> DSignal dom (n + d) Float
fromU32With =
  withFrozenCallStack $ fixedToFloatWith "fromU32" "Uint32" "Single"
{-# INLINE fromU32With #-}

-- | Conversion of @Unsigned 32@ to @Float@, with default delay
fromU32
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Unsigned 32)
  -> DSignal dom (n + FromU32DefDelay) Float
fromU32 = withFrozenCallStack fromU32With
{-# INLINE fromU32 #-}

-- | The default delay for conversion of @Unsigned 32@ to @Float@
type FromU32DefDelay = 5

-- | Customizable conversion of @Signed 32@ to @Float@
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS32With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Signed 32)
  -> DSignal dom (n + d) Float
fromS32With =
  withFrozenCallStack $ fixedToFloatWith "fromS32" "Int32" "Single"
{-# INLINE fromS32With #-}

-- | Conversion of @Signed 32@ to @Float@, with default delay
fromS32
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Signed 32)
  -> DSignal dom (n + FromS32DefDelay) Float
fromS32 = withFrozenCallStack fromS32With
{-# INLINE fromS32 #-}

-- | The default delay for conversion of @Signed 32@ to @Float@
type FromS32DefDelay = 6

-- | Customizable floating point comparison
--
-- Produces 'Clash.Cores.Xilinx.Floating.Explicit.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
compareWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Ordering
compareWith clk ena a b
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und ena clk (xilinxCompare <$> a <*> b)
  und = withFrozenCallStack $ errorX "Initial values of compare undefined"
  extraOpts =
       ("CONFIG.C_Compare_Operation",     StrOpt "Condition_Code")
    :> ("CONFIG.A_Precision_Type",        StrOpt "Single")
    :> ("CONFIG.C_A_Exponent_Width",      IntegerOpt 8)
    :> ("CONFIG.C_A_Fraction_Width",      IntegerOpt 24)
    :> ("CONFIG.Result_Precision_Type",   StrOpt "Custom")
    :> ("CONFIG.C_Result_Exponent_Width", IntegerOpt 4)
    :> ("CONFIG.C_Result_Fraction_Width", IntegerOpt 0)
    :> ("CONFIG.C_Mult_Usage",            StrOpt "No_Usage")
    :> ("CONFIG.C_Rate",                  IntegerOpt 1)
    :> Nil
  synth = binaryInst "compare" "Compare" extraOpts clk ena a b
{-# INLINE compareWith #-}

-- | Floating point comparison, with default delay
--
-- Produces 'Clash.Cores.Xilinx.Floating.Explicit.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'Prelude.compare'.
compare
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + CompareDefDelay) Ordering
compare = compareWith
{-# INLINE compare #-}

-- | The default delay for @Float@ comparison
type CompareDefDelay = 2

-- | Customizable floating point square root.
--
-- The square root of a negative zero is a negative zero. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the square root of a negative subnormal number is a negative zero too. The
-- square root of any other negative number, including negative infinity, is
-- NaN.
--
-- The t'Config' makes no difference in single precision: only the square root
-- in half precision uses it (@bMemUsage@).
sqrtWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
sqrtWith Config{bMemUsage} clk en x
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxSqrt <$> x)
  und = withFrozenCallStack $ deepErrorX "Initial values of sqrt undefined"
  -- In single precision, Vivado ignores this setting, with a warning if it is
  -- 'FullBMemUsage'
  extraOpts =
       ("CONFIG.C_BRAM_Usage", StrOpt (bMemUsageToTcl bMemUsage))
    :> precisionOpts "Single"
  synth = unaryInst "sqrt" "Square_root" extraOpts clk en x
{-# INLINE sqrtWith #-}

-- | Floating point square root with default settings.
--
-- The square root of a negative zero is a negative zero. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the square root of a negative subnormal number is a negative zero too. The
-- square root of any other negative number, including negative infinity, is
-- NaN.
sqrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + SqrtDefDelay) Float
sqrt = withFrozenCallStack $ sqrtWith defConfig
{-# INLINE sqrt #-}

-- | The default delay for floating point square root with default
-- customization.
type SqrtDefDelay = 28

-- | Floating point absolute value.
--
-- The IP only clears the sign bit. Unlike the other operations, it leaves
-- subnormal numbers and the payloads of NaNs alone.
--
-- The IP only supports a latency of zero, so the result has the same delay as
-- the input. The IP is then combinational, without a clock or clock enable,
-- so this function does not take them either.
abs
  :: forall dom n
   . KnownDomain dom
  => DSignal dom n Float
  -> DSignal dom n Float
abs x
  | clashSimulation = xilinxAbs <$> x
  | otherwise =
      unaryCombinationalInst "abs" "Absolute" (precisionOpts "Single") x
{-# INLINE abs #-}

-- | Customizable conversion of @Unsigned 64@ to @Float@. It rounds to
-- nearest, ties to even.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromU64With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Unsigned 64)
  -> DSignal dom (n + d) Float
fromU64With =
  withFrozenCallStack $ fixedToFloatWith "fromU64" "Uint64" "Single"
{-# INLINE fromU64With #-}

-- | Conversion of @Unsigned 64@ to @Float@, with default delay. It rounds to
-- nearest, ties to even.
fromU64
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Unsigned 64)
  -> DSignal dom (n + FromU64DefDelay) Float
fromU64 = withFrozenCallStack fromU64With
{-# INLINE fromU64 #-}

-- | The default delay for conversion of @Unsigned 64@ to @Float@
type FromU64DefDelay = 6

-- | Customizable conversion of @Signed 64@ to @Float@. It rounds to
-- nearest, ties to even.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
fromS64With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Signed 64)
  -> DSignal dom (n + d) Float
fromS64With =
  withFrozenCallStack $ fixedToFloatWith "fromS64" "Int64" "Single"
{-# INLINE fromS64With #-}

-- | Conversion of @Signed 64@ to @Float@, with default delay. It rounds to
-- nearest, ties to even.
fromS64
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n (Signed 64)
  -> DSignal dom (n + FromS64DefDelay) Float
fromS64 = withFrozenCallStack fromS64With
{-# INLINE fromS64 #-}

-- | The default delay for conversion of @Signed 64@ to @Float@
type FromS64DefDelay = 7

-- | Customizable conversion of @Float@ to @Signed 32@.
--
-- It rounds to nearest, ties to even, so @2.5@ becomes @2@ and @3.5@ becomes
-- @4@. Numbers that are out of range after rounding, including infinities,
-- saturate to 'minBound' or 'maxBound'. Every NaN becomes 'minBound'.
-- Subnormal numbers become zero.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toS32With
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) (Signed 32)
toS32With clk en inp
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk $ fmap xilinxToS32 inp
  und = withFrozenCallStack $ errorX "Initial values of toS32 undefined"
  extraOpts =
       ("CONFIG.A_Precision_Type",        StrOpt "Single")
    :> ("CONFIG.Result_Precision_Type",   StrOpt "Int32")
    :> ("CONFIG.C_Result_Exponent_Width", IntegerOpt 32)
    :> ("CONFIG.C_Result_Fraction_Width", IntegerOpt 0)
    :> ("CONFIG.C_Rate",                  IntegerOpt 1)
    :> Nil
  synth = unaryInst "toS32" "Float_to_fixed" extraOpts clk en inp
{-# INLINE toS32With #-}

-- | Conversion of @Float@ to @Signed 32@, with default delay.
--
-- It rounds to nearest, ties to even, so @2.5@ becomes @2@ and @3.5@ becomes
-- @4@. Numbers that are out of range after rounding, including infinities,
-- saturate to 'minBound' or 'maxBound'. Every NaN becomes 'minBound'.
-- Subnormal numbers become zero.
toS32
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + ToS32DefDelay) (Signed 32)
toS32 = withFrozenCallStack toS32With
{-# INLINE toS32 #-}

-- | The default delay for conversion of @Float@ to @Signed 32@
type ToS32DefDelay = 6

-- | Customizable conversion of @Float@ to @Half@.
--
-- It rounds to nearest, ties to even. Results below the smallest normal
-- number of the target precision become a zero of the same sign, results
-- beyond its largest finite number an infinity of the same sign. Like the
-- other operations, the IP treats subnormal inputs as a zero of the same
-- sign and turns every NaN into the NaN it generates.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toHalfWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Half
toHalfWith clk en inp
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk $ fmap xilinxConvert inp
  und = withFrozenCallStack $ deepErrorX "Initial values of toHalf undefined"
  extraOpts =
       ("CONFIG.A_Precision_Type",      StrOpt "Single")
    :> ("CONFIG.Result_Precision_Type", StrOpt "Half")
    :> Nil
  synth = unaryInst "toHalf" "Float_to_float" extraOpts clk en inp
{-# INLINE toHalfWith #-}

-- | Conversion of @Float@ to @Half@, with default delay.
--
-- It rounds to nearest, ties to even. Results below the smallest normal
-- number of the target precision become a zero of the same sign, results
-- beyond its largest finite number an infinity of the same sign. Like the
-- other operations, the IP treats subnormal inputs as a zero of the same
-- sign and turns every NaN into the NaN it generates.
toHalf
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + ToHalfDefDelay) Half
toHalf = withFrozenCallStack toHalfWith
{-# INLINE toHalf #-}

-- | The default delay for conversion of @Float@ to @Half@
type ToHalfDefDelay = 3

-- | Customizable conversion of @Float@ to @Double@.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
--
-- Only the delay is configurable, so this function does not take a @Config@
-- argument.
toDoubleWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Double
toDoubleWith clk en inp
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk $ fmap xilinxConvert inp
  und = withFrozenCallStack $ deepErrorX "Initial values of toDouble undefined"
  extraOpts =
       ("CONFIG.A_Precision_Type",      StrOpt "Single")
    :> ("CONFIG.Result_Precision_Type", StrOpt "Double")
    :> Nil
  synth = unaryInst "toDouble" "Float_to_float" extraOpts clk en inp
{-# INLINE toDoubleWith #-}

-- | Conversion of @Float@ to @Double@, with default delay.
--
-- The conversion is exact, except that, like the other operations, the IP
-- treats subnormal inputs as a zero of the same sign and turns every NaN into
-- the NaN it generates.
toDouble
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + ToDoubleDefDelay) Double
toDouble = withFrozenCallStack toDoubleWith
{-# INLINE toDouble #-}

-- | The default delay for conversion of @Float@ to @Double@
type ToDoubleDefDelay = 2

-- | Customizable floating point reciprocal, @1 / x@.
--
-- The reciprocal of a zero is an infinity of the same sign, and that of an
-- infinity a zero of the same sign. Like the other operations, the IP treats
-- subnormal inputs as a zero of the same sign and flushes results below the
-- smallest normal number to a zero of the same sign.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- reciprocal rounded to nearest, ties to even. The IP is only accurate to one
-- unit in the last place (PG060), so the hardware can differ from the
-- simulation in the last bit: the IP's C model, which matches the IP bit for
-- bit, differs from the correctly rounded result for 2.2% of all inputs.
--
-- Only the @dspUsage@ setting of the t'Config' matters. The IP rejects
-- @MediumDspUsage@ and @MaxDspUsage@.
recipWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
recipWith Config{dspUsage} clk en x
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxRecip <$> x)
  und = withFrozenCallStack $ deepErrorX "Initial values of recip undefined"
  extraOpts =
       ("CONFIG.C_Mult_Usage", StrOpt (dspUsageToTcl dspUsage))
    :> precisionOpts "Single"
  synth = unaryInst "recip" "Reciprocal" extraOpts clk en x
{-# INLINE recipWith #-}

-- | Floating point reciprocal, @1 / x@, with default settings.
--
-- The reciprocal of a zero is an infinity of the same sign, and that of an
-- infinity a zero of the same sign. Like the other operations, the IP treats
-- subnormal inputs as a zero of the same sign and flushes results below the
-- smallest normal number to a zero of the same sign.
--
-- Clash simulation gives the correctly rounded result, while the IP is only
-- accurate to one unit in the last place, so they can differ in the last bit.
-- See 'recipWith'.
recip
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + RecipDefDelay) Float
recip = withFrozenCallStack $ recipWith defConfig
{-# INLINE recip #-}

-- | The default delay for floating point reciprocal with default
-- customization.
type RecipDefDelay = 29

-- | Customizable floating point reciprocal square root, @1 / sqrt x@.
--
-- The reciprocal square root of a zero is an infinity of the same sign. Like
-- the other operations, the IP treats subnormal inputs as a zero of the same
-- sign, so the reciprocal square root of a negative subnormal number is
-- negative infinity. That of any other negative number, including negative
-- infinity, is NaN. That of positive infinity is positive zero.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- reciprocal square root rounded to nearest, ties to even. The IP is only
-- accurate to one unit in the last place (PG060), so the hardware can differ
-- from the simulation in the last bit: the IP's C model, which matches the IP
-- bit for bit, differs from the correctly rounded result for 2.0% of all
-- inputs.
--
-- Only the @dspUsage@ setting of the t'Config' matters. The IP rejects
-- @MediumDspUsage@ and @MaxDspUsage@.
rsqrtWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
rsqrtWith Config{dspUsage} clk en x
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxRSqrt <$> x)
  und = withFrozenCallStack $ deepErrorX "Initial values of rsqrt undefined"
  extraOpts =
       ("CONFIG.C_Mult_Usage", StrOpt (dspUsageToTcl dspUsage))
    :> precisionOpts "Single"
  synth = unaryInst "rsqrt" "Rec_Square_Root" extraOpts clk en x
{-# INLINE rsqrtWith #-}

-- | Floating point reciprocal square root, @1 / sqrt x@, with default settings.
--
-- The reciprocal square root of a zero is an infinity of the same sign. Like
-- the other operations, the IP treats subnormal inputs as a zero of the same
-- sign, so the reciprocal square root of a negative subnormal number is
-- negative infinity. That of any other negative number, including negative
-- infinity, is NaN. That of positive infinity is positive zero.
--
-- Clash simulation gives the correctly rounded result, while the IP is only
-- accurate to one unit in the last place, so they can differ in the last bit.
-- See 'rsqrtWith'.
rsqrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + RSqrtDefDelay) Float
rsqrt = withFrozenCallStack $ rsqrtWith defConfig
{-# INLINE rsqrt #-}

-- | The default delay for floating point reciprocal square root with default
-- customization.
type RSqrtDefDelay = 32

-- | Customizable floating point exponential, @e^x@.
--
-- Like the other operations, the IP treats subnormal inputs as a zero, whose
-- exponential is one. The exponential of positive infinity is positive
-- infinity, that of negative infinity positive zero. Results beyond the
-- largest finite number become positive infinity, results below the smallest
-- normal number positive zero.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- exponential rounded to nearest, ties to even. The IP is only accurate to one
-- unit in the last place (PG060), so the hardware can differ from the
-- simulation in the last bit: the IP's C model, which matches the IP bit for
-- bit, differs from the correctly rounded result for about 16% of random
-- inputs between -90 and 89.
--
-- The @dspUsage@ and @bMemUsage@ settings of the t'Config' matter. The IP
-- rejects @MaxDspUsage@.
expWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
expWith Config{dspUsage, bMemUsage} clk en x
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxExp <$> x)
  und = withFrozenCallStack $ deepErrorX "Initial values of exp undefined"
  extraOpts =
       ("CONFIG.C_Mult_Usage", StrOpt (dspUsageToTcl dspUsage))
    :> ("CONFIG.C_BRAM_Usage", StrOpt (bMemUsageToTcl bMemUsage))
    :> precisionOpts "Single"
  synth = unaryInst "exp" "Exponential" extraOpts clk en x
{-# INLINE expWith #-}

-- | Floating point exponential, @e^x@, with default settings.
--
-- Like the other operations, the IP treats subnormal inputs as a zero, whose
-- exponential is one. The exponential of positive infinity is positive
-- infinity, that of negative infinity positive zero. Results beyond the
-- largest finite number become positive infinity, results below the smallest
-- normal number positive zero.
--
-- Clash simulation gives the correctly rounded result, while the IP is only
-- accurate to one unit in the last place, so they can differ in the last bit.
-- See 'expWith'.
exp
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + ExpDefDelay) Float
exp = withFrozenCallStack $ expWith defConfig
{-# INLINE exp #-}

-- | The default delay for floating point exponential with default
-- customization.
type ExpDefDelay = 29

-- | Customizable floating point natural logarithm, @log x@.
--
-- The logarithm of a zero of either sign is negative infinity. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the logarithm of a subnormal number, even a negative one, is negative
-- infinity too. That of any other negative number, including negative
-- infinity, is NaN. That of positive infinity is positive infinity, and that
-- of one positive zero.
--
-- __Accuracy.__ Clash simulation gives the correctly rounded result: the exact
-- logarithm rounded to nearest, ties to even. The IP is only accurate to one
-- unit in the last place (PG060), so the hardware can differ from the
-- simulation in the last bit: the IP mostly rounds towards zero, so its C
-- model, which matches the IP bit for bit, differs from the correctly rounded
-- result for about half of all inputs.
--
-- Only the @dspUsage@ setting of the t'Config' matters. The IP rejects
-- @MaxDspUsage@.
logWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
logWith Config{dspUsage} clk en x
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxLog <$> x)
  und = withFrozenCallStack $ deepErrorX "Initial values of log undefined"
  extraOpts =
       ("CONFIG.C_Mult_Usage", StrOpt (dspUsageToTcl dspUsage))
    :> precisionOpts "Single"
  synth = unaryInst "log" "Logarithm" extraOpts clk en x
{-# INLINE logWith #-}

-- | Floating point natural logarithm, @log x@, with default settings.
--
-- The logarithm of a zero of either sign is negative infinity. Like the other
-- operations, the IP treats subnormal inputs as a zero of the same sign, so
-- the logarithm of a subnormal number, even a negative one, is negative
-- infinity too. That of any other negative number, including negative
-- infinity, is NaN. That of positive infinity is positive infinity, and that
-- of one positive zero.
--
-- Clash simulation gives the correctly rounded result, while the IP is only
-- accurate to one unit in the last place, so they can differ in the last bit.
-- See 'logWith'.
log
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom (n + LogDefDelay) Float
log = withFrozenCallStack $ logWith defConfig
{-# INLINE log #-}

-- | The default delay for floating point natural logarithm with default
-- customization.
type LogDefDelay = 28

-- | Customizable fused multiply-add: @fmaWith cfg clk en x y z@ computes
-- @x * y + z@ with a single rounding, like C's @fma@. The IP computes the
-- product exactly, adds the addend and rounds the exact sum once, to nearest,
-- ties to even. For a fused multiply-subtract, @x * y - z@, flip the sign bit
-- of the addend.
--
-- Like the other operations, the IP treats subnormal inputs as a zero of the
-- same sign and every NaN as NaN, and rounds as if the exponent were
-- unbounded: results below the smallest normal number become a zero of the
-- same sign, results beyond the largest finite number an infinity of the same
-- sign. As the product is not rounded, a product beyond the largest finite
-- number still gives a finite result if the addend brings the sum back in
-- range. Simulation and the IP agree bit for bit.
--
-- The special cases follow IEEE 754: an infinity times a zero is 'xilinxNaN',
-- whatever the addend, and so is the sum of an infinite product and an
-- infinite addend of the opposite sign. An exact result of zero is a positive
-- zero, unless both the product and the addend are negative zeros: so
-- @x * y + (-(x * y))@ is a positive zero, @(-0) * 1 + (-0)@ a negative one.
--
-- Only the @dspUsage@ setting of the t'Config' matters. The IP accepts
-- 'MediumDspUsage' and 'FullDspUsage' (the default), and rejects 'NoDspUsage'
-- and 'MaxDspUsage'. It supports every precision, see
-- "Clash.Cores.Xilinx.Floating", "Clash.Cores.Xilinx.Floating.Half" and
-- "Clash.Cores.Xilinx.Floating.Double" for the maximum latencies.
fmaWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
fmaWith Config{dspUsage} clk en x y z
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (xilinxFma <$> x <*> y <*> z)
  und = withFrozenCallStack $ deepErrorX "Initial values of fma undefined"
  extraOpts =
       ("CONFIG.Add_Sub_Value", StrOpt "Add")
    :> ("CONFIG.C_Mult_Usage",  StrOpt (dspUsageToTcl dspUsage))
    :> precisionOpts "Single"
  synth = ternaryInst "fma" "FMA" extraOpts clk en x y z
{-# INLINE fmaWith #-}

-- | Fused multiply-add with default settings: @fma clk en x y z@ computes
-- @x * y + z@ with a single rounding, like C's @fma@. Subnormal inputs count as
-- a zero of the same sign, and results below the smallest normal number
-- become a zero of the same sign. See 'fmaWith'.
fma
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + FmaDefDelay) Float
fma = withFrozenCallStack $ fmaWith defConfig
{-# INLINE fma #-}

-- | The default delay for fused multiply-add with default customization.
type FmaDefDelay = 19

-- | Customizable accumulator: @accumWith cfg clk en lst x@ gives the running
-- sum of the inputs @x@. The result for an input is the sum of that input and
-- the inputs before it, back to the start of the accumulation. An input with
-- @lst@ set is the last one of its accumulation: its result is the complete
-- sum, and the next input starts a new accumulation. The first input after
-- configuration starts one too.
--
-- De-asserting the 'Enable' stalls the IP: the input of that cycle is ignored,
-- @lst@ included, and the output holds, like for the other operations.
--
-- __Fixed point.__ The IP does not add in floating point: it adds into a
-- fixed-point number whose bounds are set by the t'AccumConfig'.
--
-- * Every input is first truncated towards zero to a multiple of
--   @2^accumLsb@, so inputs whose magnitude is below @2^accumLsb@ count as
--   zero. Like in the other operations, subnormal inputs count as zero too.
--
-- * The sum of the truncated inputs is exact: no rounding errors accumulate,
--   and the order of the inputs does not matter.
--
-- * The result is the sum, rounded __towards zero__ to the precision (not to
--   nearest, like the other operations). A sum of zero gives a positive zero,
--   even for negative zero inputs. A sum below the smallest normal number
--   gives a zero of the same sign; that needs an @accumLsb@ smaller than the
--   smallest exponent of the precision (-14, -126 or -1022). A sum beyond the
--   largest finite number gives the largest finite number of the same sign
--   (rounding towards zero never gives an infinity); that needs an
--   @accumMsb@ of at least the largest exponent (15, 127 or 1023).
--
-- __Overflow.__ A finite input whose magnitude is at least
-- @2^(accumInputMsb + 1)@ overflows the input, and the sum overflows once its
-- magnitude reaches @2^(accumMsb + 1)@. The IP does not wrap or saturate:
-- either overflow makes the result NaN ('xilinxNaN'), up to and including the
-- last input of the accumulation. 'accumWithFlags' tells them apart.
--
-- __Special inputs.__ A NaN input makes the result NaN until the end of the
-- accumulation, and so does seeing infinities of both signs. Otherwise, an
-- infinite input makes the result an infinity of the same sign until the end
-- of the accumulation. The IP keeps adding finite inputs to its fixed-point
-- sum meanwhile, so that sum (or an input) can still overflow and turn the
-- result into NaN.
--
-- __Bounds.__ Vivado accepts bounds with
--
-- > emin <= accumMsb <= emax + 54
-- > emin - p + 1 <= accumLsb < accumInputMsb <= min accumMsb emax
--
-- where @emin@ is the exponent of the smallest normal number (-14, -126 or
-- -1022), @emax@ that of the largest finite numbers (15, 127 or 1023) and @p@
-- the number of significant bits (11, 24 or 53). Clash simulation reports an
-- error for other bounds. Vivado rejects most of them, but it silently
-- changes some into valid ones: in half precision, for example, it raises an
-- @accumMsb@ of -15 to -14. The hardware would then not match the simulation.
--
-- __Latency.__ The latency @d@ must be at least one: unlike the other
-- operations, the accumulator does not support a latency of zero. The maximum
-- latency depends on the bounds and on the DSP usage (@accumDspUsage@, which
-- can be 'NoDspUsage', 'MediumDspUsage' or 'FullDspUsage'), see
-- "Clash.Cores.Xilinx.Floating", "Clash.Cores.Xilinx.Floating.Half" and
-- "Clash.Cores.Xilinx.Floating.Double".
accumWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , 1 <= d
     , HasCallStack
     )
  => AccumConfig
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n Float
  -> DSignal dom (n + d) Float
accumWith cfg clk en lst x
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (fst <$> accumModel "Single" cfg en lst x)
  und = withFrozenCallStack $ deepErrorX "Initial values of accum undefined"
  synth = accumInst (accumOpts "Single" False cfg) clk en lst x
{-# INLINE accumWith #-}

-- | Accumulator with the IP's default settings ('defAccumConfig'): the running
-- sum of the inputs, see 'accumWith'. Inputs are truncated towards zero to a
-- multiple of @2^-31@, and the result is rounded towards zero. Inputs and sums
-- with a magnitude of @2^33@ or more give NaN.
accum
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Enable dom
  -> DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n Float
  -> DSignal dom (n + AccumDefDelay) Float
accum = withFrozenCallStack $ accumWith defAccumConfig
{-# INLINE accum #-}

-- | Customizable accumulator that also outputs the IP's exception flags: like
-- 'accumWith', with the flags of the accumulation so far next to each result,
-- see t'AccumFlags'.
--
-- This function does not output the IP's underflow and overflow flags (a sum
-- below the smallest normal number or beyond the largest finite number): the
-- IP only has them for bounds that make them possible, and they change the
-- width of its flag output. Underflow needs an @accumLsb@ smaller than the
-- smallest exponent of the precision, overflow an @accumMsb@ larger than the
-- largest one, see 'accumWith'.
accumWithFlags
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , 1 <= d
     , HasCallStack
     )
  => AccumConfig
  -> Clock dom
  -> Enable dom
  -> DSignal dom n Bool
  -- ^ Whether the input is the last one of an accumulation
  -> DSignal dom n Float
  -> DSignal dom (n + d) (Float, AccumFlags)
accumWithFlags cfg clk en lst x
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk (accumModel "Single" cfg en lst x)
  und = withFrozenCallStack $
    deepErrorX "Initial values of accumWithFlags undefined"
  synth = accumFlagsInst (accumOpts "Single" True cfg) clk en lst x
{-# INLINE accumWithFlags #-}

-- | The default delay of the accumulator with the default settings
-- ('defAccumConfig'): the maximum latency for them, the same on all measured
-- devices.
type AccumDefDelay = 22

-- | The IP's default settings of the accumulator in single precision, see
-- 'accumWith': @accumMsb = 32@, @accumLsb = -31@ and @accumInputMsb = 32@, and
-- 'MediumDspUsage'. Double precision has the same defaults
-- ('Clash.Cores.Xilinx.Floating.Double.Explicit.defAccumConfig'). In half
-- precision ('Clash.Cores.Xilinx.Floating.Half.Explicit.defAccumConfig'),
-- @accumLsb = -24@ and @accumInputMsb = 15@, so that every input fits.
defAccumConfig :: AccumConfig
defAccumConfig =
  AccumConfig
    { accumMsb = 32
    , accumLsb = -31
    , accumInputMsb = 32
    , accumDspUsage = MediumDspUsage
    }
{-# INLINE defAccumConfig #-}
