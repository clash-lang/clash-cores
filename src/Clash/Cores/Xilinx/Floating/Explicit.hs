{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
                  2022,      Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Floating-Point LogiCORE IP v7.1](https://docs.amd.com/v/u/en-US/pg060-floating-point).

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
smallest normal number, but they are rounded to zero nonetheless! The IP rounds a
result to 24 significant bits before checking whether it is subnormal, so even a
result that IEEE rounding would round up to the smallest normal number is
rounded to zero if it is below it.

For each customizable operation, there also exists a function that uses the
defaults. These functions use the settings from 'defConfig' and the maximum
delay for the Xilinx IP with that configuration. That delay is also defined as a
type variable for delay annotation in circuits.
-}

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

import Clash.Cores.Xilinx.Floating.Generic.Explicit (defConfig)
import qualified Clash.Cores.Xilinx.Floating.Generic.Explicit as G
import Clash.Cores.Xilinx.Floating.Internal
  ( ArchOpt(..), BMemUsage(..), Config(..), DspUsage(..), Ordering(..)
  , toMaybeOrdering, xilinxNaN )

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
addWith = withFrozenCallStack G.addWith
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
add = withFrozenCallStack G.add
{-# INLINE add #-}

-- | The default delay for floating point addition with default customization.
type AddDefDelay = G.AddDefDelay Float

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
subWith = withFrozenCallStack G.subWith
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
sub = withFrozenCallStack G.sub
{-# INLINE sub #-}

-- | The default delay for floating point subtraction with default
-- customization.
type SubDefDelay = G.SubDefDelay Float

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
mulWith = withFrozenCallStack G.mulWith
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
mul = withFrozenCallStack G.mul
{-# INLINE mul #-}

-- | The default delay for floating point multiplication with default
-- customization.
type MulDefDelay = G.MulDefDelay Float

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
divWith = withFrozenCallStack G.divWith
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
div = withFrozenCallStack G.div
{-# INLINE div #-}

-- | The default delay for floating point division with default customization.
type DivDefDelay = G.DivDefDelay Float

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
fromU32With = withFrozenCallStack G.fromU32With
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
fromU32 = withFrozenCallStack G.fromU32
{-# INLINE fromU32 #-}

-- | The default delay for conversion of @Unsigned 32@ to @Float@
type FromU32DefDelay = G.FromU32DefDelay Float

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
fromS32With = withFrozenCallStack G.fromS32With
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
fromS32 = withFrozenCallStack G.fromS32
{-# INLINE fromS32 #-}

-- | The default delay for conversion of @Signed 32@ to @Float@
type FromS32DefDelay = G.FromS32DefDelay Float

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
compareWith = withFrozenCallStack G.compareWith
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
compare = withFrozenCallStack G.compare
{-# INLINE compare #-}

-- | The default delay for @Float@ comparison
type CompareDefDelay = G.CompareDefDelay Float
