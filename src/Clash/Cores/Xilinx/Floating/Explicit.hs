{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
                  2022,      Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://www.xilinx.com/support/documentation/ip_documentation/floating_point/v7_1/pg060-floating-point.pdf).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @floating_point@/.../@.clash.tcl@ which needs to be executed in the Vivado
project to create the proper entity. Simulation in Clash produces bit-identical
results to synthesis.

Most functions allow customization of the Xilinx IP. Valid combinations will
need to be gleaned from, e.g., the Vivado wizard (IP Catalog -> Floating-point).
All IP instantiated by this module always has the following properties:

* Single precision
* Non-blocking
* No reset input
* No optional output fields, no @TLAST@, no @TUSER@
* @TVALID@ on the inputs is always asserted, @TVALID@ on the output is ignored.
Note that it would appear the Xilinx IP does not use these signals in
non-blocking mode anyway.
* 1 cycle per operation (meaning there need not be any dummy cycles between
consecutive inputs)

The latency of the IP is set through the delay argument of the 'DSignal'. Other
customization is done through the 'Config' argument of customizable functions.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

The Xilinx IP does not support calculations with subnormal numbers. All
subnormal numbers are rounded to zero on both input and output. Note that this
introduces a slight bias as the larger subnormal numbers are closer to the
smallest normal number, but they are rounded to zero nonetheless!

For each customizable operation, there also exists a function that uses the
defaults. These functions use the settings from 'defConfig' and the maximum
delay for the Xilinx IP with that configuration. That delay is also defined as a
type variable for delay annotation in circuits.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

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

import Clash.Cores.Xilinx.Floating.Annotations
import Clash.Cores.Xilinx.Floating.BlackBoxes
import Clash.Cores.Xilinx.Floating.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import qualified Clash.Signal.Delayed as D

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
addWith !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x + y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of add undefined"
{-# OPAQUE addWith #-}
{-# ANN addWith (vhdlBinaryPrim 'addWith 'addTclTF "add") #-}
{-# ANN addWith (veriBinaryPrim 'addWith 'addTclTF "add") #-}

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
subWith !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x - y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of sub undefined"
{-# OPAQUE subWith #-}
{-# ANN subWith (vhdlBinaryPrim 'subWith 'subTclTF "sub") #-}
{-# ANN subWith (veriBinaryPrim 'subWith 'subTclTF "sub") #-}

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
mulWith !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x * y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of mul undefined"
{-# OPAQUE mulWith #-}
{-# ANN mulWith (vhdlBinaryPrim 'mulWith 'mulTclTF "mul") #-}
{-# ANN mulWith (veriBinaryPrim 'mulWith 'mulTclTF "mul") #-}

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
divWith !_ clk en (conditionFloatF -> x) (conditionFloatF -> y) =
  delayI und en clk . conditionFloatF $ x / y
 where
  und = withFrozenCallStack $ deepErrorX "Initial values of div undefined"
{-# OPAQUE divWith #-}
{-# ANN divWith (vhdlBinaryPrim 'divWith 'divTclTF "div") #-}
{-# ANN divWith (veriBinaryPrim 'divWith 'divTclTF "div") #-}

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
  | otherwise = D.unsafeFromSignal synth
 where
  sim = delayI und en clk $ fmap fromIntegral inp
  und = withFrozenCallStack $ errorX "Initial values of fromU32 undefined"
  synth = unpack <$> unPort (snd go)
   where
    go ::
      ( Port "m_axis_result_tvalid" dom Bit
      , Port "m_axis_result_tdata" dom (BitVector 32)
      )
    go =
      instWithXilinxWizard
        (instConfig "fromU32")
        (XilinxWizard
          { wiz_name = "floating_point"
          , wiz_vendor = "xilinx.com"
          , wiz_library = "ip"
          , wiz_version = "7.1"
          , wiz_options =
               ("CONFIG.Operation_Type",     StrOpt "Fixed_to_float")
            :> ("CONFIG.A_Precision_Type",   StrOpt "Uint32")
            :> ("CONFIG.Flow_Control",       StrOpt "NonBlocking")
            :> ("CONFIG.Has_ACLKEN",         BoolOpt True)
            :> ("CONFIG.C_A_Exponent_Width", IntegerOpt 32)
            :> ("CONFIG.C_A_Fraction_Width", IntegerOpt 0)
            :> ("CONFIG.Has_RESULT_TREADY",  BoolOpt False)
            :> ("CONFIG.C_Latency",          IntegerOpt (natToNum @d))
            :> ("CONFIG.C_Rate",             IntegerOpt 1)
            :> ("CONFIG.Maximum_Latency",    BoolOpt False)
            :> Nil
          }
        )
        (ClockPort @"aclk" clk)
        (Port @"aclken" (boolToBit <$> fromEnable en))
        (Port @"s_axis_a_tdata" (pack <$> D.toSignal inp))
        (Port @"s_axis_a_tvalid" (pure 1 :: Signal dom Bit))
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
  | otherwise = D.unsafeFromSignal synth
 where
  sim = delayI und en clk $ fmap fromIntegral inp
  und = withFrozenCallStack $ errorX "Initial values of fromS32 undefined"
  synth = unpack <$> unPort (snd go)
   where
    go ::
      ( Port "m_axis_result_tvalid" dom Bit
      , Port "m_axis_result_tdata" dom (BitVector 32)
      )
    go =
      instWithXilinxWizard
        (instConfig "fromS32")
        (XilinxWizard
          { wiz_name = "floating_point"
          , wiz_vendor = "xilinx.com"
          , wiz_library = "ip"
          , wiz_version = "7.1"
          , wiz_options =
               ("CONFIG.Operation_Type",     StrOpt "Fixed_to_float")
            :> ("CONFIG.A_Precision_Type",   StrOpt "Int32")
            :> ("CONFIG.Flow_Control",       StrOpt "NonBlocking")
            :> ("CONFIG.Has_ACLKEN",         BoolOpt True)
            :> ("CONFIG.C_A_Exponent_Width", IntegerOpt 32)
            :> ("CONFIG.C_A_Fraction_Width", IntegerOpt 0)
            :> ("CONFIG.Has_RESULT_TREADY",  BoolOpt False)
            :> ("CONFIG.C_Latency",          IntegerOpt (natToNum @d))
            :> ("CONFIG.C_Rate",             IntegerOpt 1)
            :> ("CONFIG.Maximum_Latency",    BoolOpt False)
            :> Nil
          }
        )
        (ClockPort @"aclk" clk)
        (Port @"aclken" (boolToBit <$> fromEnable en))
        (Port @"s_axis_a_tdata" (pack <$> D.toSignal inp))
        (Port @"s_axis_a_tvalid" (pure 1 :: Signal dom Bit))
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
-- NaN. Otherwise, it behaves like Haskell's 'P.compare'.
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
  | otherwise = D.unsafeFromSignal synth
 where
  sim = delayI und ena clk (xilinxCompare <$> a <*> b)
  und = withFrozenCallStack $ errorX "Initial values of compare undefined"
  synth = unpack . resize <$> unPort (snd go)
   where
    go ::
      ( Port "m_axis_result_tvalid" dom Bit
      , Port "m_axis_result_tdata" dom (BitVector 8)
      )
    go =
      instWithXilinxWizard
        (instConfig "fromS32")
        (XilinxWizard
          { wiz_name = "floating_point"
          , wiz_vendor = "xilinx.com"
          , wiz_library = "ip"
          , wiz_version = "7.1"
          , wiz_options =
                 ("CONFIG.Operation_Type",          StrOpt "Compare")
              :> ("CONFIG.C_Compare_Operation",     StrOpt "Condition_Code")
              :> ("CONFIG.Flow_Control",            StrOpt "NonBlocking")
              :> ("CONFIG.Maximum_Latency",         BoolOpt False)
              :> ("CONFIG.Has_ACLKEN",              BoolOpt True)
              :> ("CONFIG.A_Precision_Type",        StrOpt "Single")
              :> ("CONFIG.C_A_Exponent_Width",      IntegerOpt 8)
              :> ("CONFIG.C_A_Fraction_Width",      IntegerOpt 24)
              :> ("CONFIG.Result_Precision_Type",   StrOpt "Custom")
              :> ("CONFIG.C_Result_Exponent_Width", IntegerOpt 4)
              :> ("CONFIG.C_Result_Fraction_Width", IntegerOpt 0)
              :> ("CONFIG.C_Mult_Usage",            StrOpt "No_Usage")
              :> ("CONFIG.Has_RESULT_TREADY",       BoolOpt False)
              :> ("CONFIG.C_Latency",               IntegerOpt (natToNum @d))
              :> ("CONFIG.C_Rate",                  IntegerOpt 1)
              :> Nil
          }
        )
        (ClockPort @"aclk" clk)
        (Port @"aclken" (boolToBit <$> fromEnable ena))
        (Port @"s_axis_a_tdata" (pack <$> D.toSignal a))
        (Port @"s_axis_a_tvalid" (pure 1 :: Signal dom Bit))
        (Port @"s_axis_b_tdata" (pack <$> D.toSignal b))
        (Port @"s_axis_b_tvalid" (pure 1 :: Signal dom Bit))
{-# INLINE compareWith #-}

-- | Floating point comparison, with default delay
--
-- Produces 'Clash.Cores.Xilinx.Floating.Explicit.NaN' if any of the inputs is
-- NaN. Otherwise, it behaves like Haskell's 'P.compare'.
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

-- | Default customization options.
--
-- For those operations that support it, the default options are:
--
-- * Speed-optimized architecture ('SpeedArch')
-- * Full DSP slice usage ('FullDspUsage')
-- * Exponential operator does not use block memory ('NoBMemUsage')
defConfig :: Config
defConfig = Config
  { archOpt = SpeedArch
  , dspUsage = FullDspUsage
  , bMemUsage = NoBMemUsage
  }
