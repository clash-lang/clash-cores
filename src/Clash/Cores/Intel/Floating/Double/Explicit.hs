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

The supported functions are the arithmetic ('add', 'sub', 'mul' and 'div'),
'sqrt', 'rsqrt', 'recip' and 'exp'. The arithmetic and the reciprocal are correctly rounded,
with the deviations from IEEE 754 described in
"Clash.Cores.Intel.Floating.Explicit" (subnormal inputs are treated as zero
and results below the smallest normal number are flushed to zero, after
rounding with an unbounded exponent). In double precision the IP
computes them with the algorithms of single precision, scaled up: the square
root with a polynomial of degree five instead of two. Clash simulation gives
the IP's results bit for bit: the models ('sqrtModel', 'rsqrtModel', 'recipModel' and 'expModel') were
obtained by black-box testing of the IP, see Note [Bit-exact double precision
models] in "Clash.Cores.Intel.Floating.Internal.BitExactDouble", and were
verified against the simulated IP on millions of inputs for MAX 10, Cyclone IV
E, Cyclone 10 LP and Cyclone V, at several latencies. The results of the
square root, the reciprocal square root and the exponential depend on the
multipliers of the device family, so they take a 'DeviceFamily' argument.

The other functions of the IP, its multiply-add, comparisons and conversions
are not supported in double precision.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Floating.Double.Explicit
  ( -- * Arithmetic
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
    -- * Functions
  , DeviceFamily(..)
  , sqrtWith
  , sqrt
  , SqrtDefDelay
  , rsqrtWith
  , rsqrt
  , RsqrtDefDelay
  , recipWith
  , recip
  , RecipDefDelay
  , expWith
  , exp
  , ExpDefDelay
    -- * Customizing IP
  , Config(..)
  , defConfig
    -- * Simulation models
  , intelNaN
  , addModel
  , subModel
  , mulModel
  , divModel
  , sqrtModel
  , rsqrtModel
  , recipModel
  , expModel
  ) where

import Clash.Explicit.Prelude hiding (add, sub, mul, div, sqrt, recip, exp)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Intel.Floating.Internal
  (Config(..), DeviceFamily(..), binaryInst, defConfig, pipeline, unaryInst)
import Clash.Cores.Intel.Floating.Internal.Double

-- | Customizable floating point addition, in double precision, see
-- 'addModel'. The IP's result is bit-exactly modelled.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
addWith
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
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
addWith cfg clk rst en x y
  | clashSimulation = pipeline und clk rst en (addModel <$> x <*> y)
  | otherwise = binaryInst "fp_add_double" ip clk rst en x y
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of add"
  ip = fpFunctionsDouble cfg "ARITH" "ADD" (natToNum @d) Nil
{-# INLINE addWith #-}

-- | Floating point addition, in double precision, with the default
-- configuration and delay. See 'addWith'.
add
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + AddDefDelay) Double
add = withFrozenCallStack $ addWith defConfig
{-# INLINE add #-}

-- | The default delay of 'add': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type AddDefDelay = 18

-- | Customizable floating point subtraction, in double precision, see
-- 'subModel'. The IP's result is bit-exactly modelled.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
subWith
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
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
subWith cfg clk rst en x y
  | clashSimulation = pipeline und clk rst en (subModel <$> x <*> y)
  | otherwise = binaryInst "fp_sub_double" ip clk rst en x y
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of sub"
  ip = fpFunctionsDouble cfg "ARITH" "SUB" (natToNum @d) Nil
{-# INLINE subWith #-}

-- | Floating point subtraction, in double precision, with the default
-- configuration and delay. See 'subWith'.
sub
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + SubDefDelay) Double
sub = withFrozenCallStack $ subWith defConfig
{-# INLINE sub #-}

-- | The default delay of 'sub': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type SubDefDelay = 18

-- | Customizable floating point multiplication, in double precision, see
-- 'mulModel'. The IP's result is bit-exactly modelled.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
mulWith
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
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
mulWith cfg clk rst en x y
  | clashSimulation = pipeline und clk rst en (mulModel <$> x <*> y)
  | otherwise = binaryInst "fp_mul_double" ip clk rst en x y
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of mul"
  ip = fpFunctionsDouble cfg "ARITH" "MUL" (natToNum @d) Nil
{-# INLINE mulWith #-}

-- | Floating point multiplication, in double precision, with the default
-- configuration and delay. See 'mulWith'.
mul
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + MulDefDelay) Double
mul = withFrozenCallStack $ mulWith defConfig
{-# INLINE mul #-}

-- | The default delay of 'mul': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type MulDefDelay = 12

-- | Customizable floating point division, in double precision, see
-- 'divModel'. The IP's result is bit-exactly modelled.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
divWith
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
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
divWith cfg clk rst en x y
  | clashSimulation = pipeline und clk rst en (divModel <$> x <*> y)
  | otherwise = binaryInst "fp_div_double" ip clk rst en x y
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of div"
  ip = fpFunctionsDouble cfg "ARITH" "DIV" (natToNum @d) Nil
{-# INLINE divWith #-}

-- | Floating point division, in double precision, with the default
-- configuration and delay. See 'divWith'.
div
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + DivDefDelay) Double
div = withFrozenCallStack $ divWith defConfig
{-# INLINE div #-}

-- | The default delay of 'div': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type DivDefDelay = 50

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

-- | Customizable floating point reciprocal square root, @1 / sqrt x@, in
-- double precision, see 'rsqrtModel'. The IP's result is bit-exactly
-- modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family (12 on
-- all four families).
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
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
rsqrtWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (rsqrtModel fam <$> x)
  | otherwise = unaryInst "fp_rsqrt_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of rsqrt"
  ip = fpFunctionsDouble cfg "ROOTS" "INV_SQRT" (natToNum @d) Nil
{-# INLINE rsqrtWith #-}

-- | Floating point reciprocal square root, @1 / sqrt x@, in double precision,
-- with the default configuration and delay. See 'rsqrtWith'.
rsqrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + RsqrtDefDelay) Double
rsqrt = withFrozenCallStack $ rsqrtWith defConfig
{-# INLINE rsqrt #-}

-- | The default delay of 'rsqrt': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest (Cyclone IV E and Cyclone 10 LP; 31 on MAX 10, 26 on Cyclone V).
type RsqrtDefDelay = 32

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

-- | Customizable floating point exponential, @e^x@, in double precision, see
-- 'expModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers and
-- lookup tables, see 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
expWith
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
expWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (expModel fam <$> x)
  | otherwise = unaryInst "fp_exp_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of exp"
  ip = fpFunctionsDouble cfg "EXP_LOG" "EXPE" (natToNum @d) Nil
{-# INLINE expWith #-}

-- | Floating point exponential, @e^x@, in double precision, with the default
-- configuration and delay. See 'expWith'.
exp
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + ExpDefDelay) Double
exp = withFrozenCallStack $ expWith defConfig
{-# INLINE exp #-}

-- | The default delay of 'exp': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type ExpDefDelay = 57
