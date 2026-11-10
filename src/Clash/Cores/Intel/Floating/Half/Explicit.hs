{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's Floating Point Functions IP (@altera_fp_functions@) in
half precision (t'Half': 5 exponent bits and 10 fraction bits, the IP's
\"custom\" format), as described in the
[Floating-Point IP Cores User Guide (UG-01058)](https://www.intel.com/content/www/us/en/docs/programmable/683750/).

This module has the same functions as "Clash.Cores.Intel.Floating.Explicit",
but for t'Half' instead of 'Float'. See there for how the functions
instantiate the IP, how the IP is reset and enabled and how it treats
subnormal numbers. "Clash.Cores.Intel.Floating.Half" has the same functions
with a hidden clock, reset and enable.

The supported functions are 'sqrt', 'recip' and 'rsqrt'. In half precision the IP computes them
with the same algorithms as in single precision, scaled down (polynomials of
degree one instead of two). Clash simulation gives the IP's results bit for
bit: the models ('sqrtModel' etc.) were obtained by black-box testing of the
IP, see Note [Bit-exact half precision models] in
"Clash.Cores.Intel.Floating.Internal.BitExactHalf", and were verified against
the simulated IP on /all/ inputs, for MAX 10, Cyclone IV E, Cyclone 10 LP and
Cyclone V, at several latencies. Like in single precision, the results of
most functions depend on the multipliers of the device family, so they take
a 'DeviceFamily' argument.

The arithmetic functions, comparisons and conversions of the IP are not
supported in half precision: their models would be the ones of single
precision, but they have too many inputs to verify them all.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Floating.Half.Explicit
  ( -- * Functions
    DeviceFamily(..)
  , sqrtWith
  , sqrt
  , SqrtDefDelay
  , recipWith
  , recip
  , RecipDefDelay
  , rsqrtWith
  , rsqrt
  , RsqrtDefDelay
    -- * Customizing IP
  , Config(..)
  , defConfig
    -- * Simulation models
  , intelNaN
  , sqrtModel
  , recipModel
  , rsqrtModel
  ) where

import Clash.Explicit.Prelude hiding (sqrt, recip)

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric.Half (Half)

import Clash.Cores.Intel.Floating.Internal
  (Config(..), DeviceFamily(..), defConfig, pipeline, unaryInst)
import Clash.Cores.Intel.Floating.Internal.Half

-- | Customizable floating point square root, in half precision, see
-- 'sqrtModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
sqrtWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((sqrtModel fam) <$> x)
  | otherwise = unaryInst "fp_sqrt_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of sqrt"
  ip = fpFunctionsHalf cfg "ROOTS" "SQRT" (natToNum @d) Nil
{-# INLINE sqrtWith #-}

-- | Floating point square root, in half precision, with the default configuration
-- and delay. See 'sqrtWith'.
sqrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + SqrtDefDelay) Half
sqrt = withFrozenCallStack $ sqrtWith defConfig
{-# INLINE sqrt #-}

-- | The default delay of 'sqrt': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type SqrtDefDelay = 3

-- | Customizable floating point reciprocal, @1 / x@, in half precision, see
-- 'recipModel'. The IP's result is bit-exactly modelled.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
recipWith cfg clk rst en x
  | clashSimulation = pipeline und clk rst en (recipModel <$> x)
  | otherwise = unaryInst "fp_recip_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of recip"
  ip = fpFunctionsHalf cfg "ARITH" "INV" (natToNum @d) Nil
{-# INLINE recipWith #-}

-- | Floating point reciprocal, @1 / x@, in half precision, with the default configuration
-- and delay. See 'recipWith'.
recip
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + RecipDefDelay) Half
recip = withFrozenCallStack $ recipWith defConfig
{-# INLINE recip #-}

-- | The default delay of 'recip': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type RecipDefDelay = 9

-- | Customizable floating point reciprocal square root, @1 / sqrt x@, in half precision, see
-- 'rsqrtModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
rsqrtWith
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
rsqrtWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((rsqrtModel fam) <$> x)
  | otherwise = unaryInst "fp_rsqrt_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of rsqrt"
  ip = fpFunctionsHalf cfg "ROOTS" "INV_SQRT" (natToNum @d) Nil
{-# INLINE rsqrtWith #-}

-- | Floating point reciprocal square root, @1 / sqrt x@, in half precision, with the default configuration
-- and delay. See 'rsqrtWith'.
rsqrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + RsqrtDefDelay) Half
rsqrt = withFrozenCallStack $ rsqrtWith defConfig
{-# INLINE rsqrt #-}

-- | The default delay of 'rsqrt': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type RsqrtDefDelay = 3
