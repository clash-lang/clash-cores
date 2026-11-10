{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's Floating Point Functions IP (@altera_fp_functions@) in
double precision ('Double'), as described in the
[Floating-Point IP Cores User Guide (UG-01058)](https://www.intel.com/content/www/us/en/docs/programmable/683750/).

This module has the same functions as "Clash.Cores.Intel.Floating.Explicit",
but for 'Double' instead of 'Float'. See there for how the functions
instantiate the IP, how the IP is reset and enabled and how it treats
subnormal numbers. "Clash.Cores.Intel.Floating.Double" has the same functions
with a hidden clock, reset and enable.

The supported functions are 'sqrt' and 'recip'. In double precision the IP
computes them with the algorithms of single precision, scaled up: the square
root with a polynomial of degree five instead of two. Clash simulation gives
the IP's results bit for bit: the models ('sqrtModel' and 'recipModel') were
obtained by black-box testing of the IP, see Note [Bit-exact double precision
models] in "Clash.Cores.Intel.Floating.Internal.BitExactDouble", and were
verified against the simulated IP on millions of inputs for MAX 10, Cyclone IV
E, Cyclone 10 LP and Cyclone V, at several latencies. The results of the
square root depend on the multipliers of the device family, so it takes a
'DeviceFamily' argument.

The other functions of the IP, its arithmetic, comparisons and conversions are
not supported in double precision.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Floating.Double.Explicit
  ( -- * Functions
    DeviceFamily(..)
  , sqrtWith
  , sqrt
  , SqrtDefDelay
  , recipWith
  , recip
  , RecipDefDelay
    -- * Customizing IP
  , Config(..)
  , defConfig
    -- * Simulation models
  , intelNaN
  , sqrtModel
  , recipModel
  ) where

import Clash.Explicit.Prelude hiding (sqrt, recip)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Intel.Floating.Internal
  (Config(..), DeviceFamily(..), defConfig, pipeline, unaryInst)
import Clash.Cores.Intel.Floating.Internal.Double

-- | Customizable floating point square root, in double precision, see
-- 'sqrtModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family (12 on
-- MAX 10 and Cyclone V).
sqrtWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
sqrtWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (sqrtModel fam <$> x)
  | otherwise = unaryInst "fp_sqrt_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of sqrt"
  ip = fpFunctionsDouble cfg "ROOTS" "SQRT" (natToNum @d) Nil
{-# INLINE sqrtWith #-}

-- | Floating point square root, in double precision, with the default
-- configuration and delay. See 'sqrtWith'.
sqrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + SqrtDefDelay) Double
sqrt = withFrozenCallStack $ sqrtWith defConfig
{-# INLINE sqrt #-}

-- | The default delay of 'sqrt': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type SqrtDefDelay = 33

-- | Customizable floating point reciprocal, @1 / x@, in double precision, see
-- 'recipModel'. The IP's result is bit-exactly modelled.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family (14 on
-- MAX 10, 15 on Cyclone V).
recipWith
  :: forall d dom n
   . ( KnownDomain dom
     , KnownNat d
     , HasCallStack
     )
  => Config
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
recipWith cfg clk rst en x
  | clashSimulation = pipeline und clk rst en (recipModel <$> x)
  | otherwise = unaryInst "fp_recip_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of recip"
  ip = fpFunctionsDouble cfg "ARITH" "INV" (natToNum @d) Nil
{-# INLINE recipWith #-}

-- | Floating point reciprocal, @1 / x@, in double precision, with the default
-- configuration and delay. See 'recipWith'.
recip
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + RecipDefDelay) Double
recip = withFrozenCallStack $ recipWith defConfig
{-# INLINE recip #-}

-- | The default delay of 'recip': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type RecipDefDelay = 45
