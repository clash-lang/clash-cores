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
    -- * Customizing IP
  , Config(..)
  , defConfig
  , ArchOpt(..)
  , DspUsage(..)
  , BMemUsage(..)
    -- * Additional functions
  , xilinxNaN
  ) where

import Clash.Explicit.Prelude hiding
  (Ordering(..), add, sub, mul, div, compare, sqrt, abs)

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
  extraOpts =
       ("CONFIG.C_Mult_Usage", StrOpt (dspUsageToTcl dspUsage))
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
