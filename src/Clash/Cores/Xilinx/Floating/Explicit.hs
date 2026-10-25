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

* "Clash.Cores.Xilinx.Floating.Generic.Explicit" has the same functions,
  generic in the precision.
-}

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
