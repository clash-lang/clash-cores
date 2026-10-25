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
    -- * Customizing IP
  , Config(..)
  , defConfig
  , ArchOpt(..)
  , DspUsage(..)
  , BMemUsage(..)
    -- * Additional functions
  , xilinxNaN
  ) where

import Clash.Explicit.Prelude hiding (Ordering(..), add, sub, mul, div, compare)

import GHC.Stack (HasCallStack, withFrozenCallStack)

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
fromU32With clk en inp
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk $ fmap fromIntegral inp
  und = withFrozenCallStack $ errorX "Initial values of fromU32 undefined"
  extraOpts =
       ("CONFIG.A_Precision_Type",      StrOpt "Uint32")
    :> ("CONFIG.C_A_Exponent_Width",    IntegerOpt 32)
    :> ("CONFIG.C_A_Fraction_Width",    IntegerOpt 0)
    :> ("CONFIG.C_Rate",                IntegerOpt 1)
    :> ("CONFIG.Result_Precision_Type", StrOpt "Single")
    :> Nil
  synth = unaryInst "fromU32" "Fixed_to_float" extraOpts clk en inp
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
fromS32With clk en inp
  | clashSimulation = sim
  | otherwise = synth
 where
  sim = delayI und en clk $ fmap fromIntegral inp
  und = withFrozenCallStack $ errorX "Initial values of fromS32 undefined"
  extraOpts =
       ("CONFIG.A_Precision_Type",      StrOpt "Int32")
    :> ("CONFIG.C_A_Exponent_Width",    IntegerOpt 32)
    :> ("CONFIG.C_A_Fraction_Width",    IntegerOpt 0)
    :> ("CONFIG.C_Rate",                IntegerOpt 1)
    :> ("CONFIG.Result_Precision_Type", StrOpt "Single")
    :> Nil
  synth = unaryInst "fromS32" "Fixed_to_float" extraOpts clk en inp
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
