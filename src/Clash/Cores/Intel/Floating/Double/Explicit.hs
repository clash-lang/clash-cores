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
'sqrt', 'rsqrt', 'recip', 'exp', 'hypot', 'log10', 'cbrt', 'log', 'log1p', 'log2' and 'exp2'. The arithmetic and the reciprocal are correctly rounded,
with the deviations from IEEE 754 described in
"Clash.Cores.Intel.Floating.Explicit" (subnormal inputs are treated as zero
and results below the smallest normal number are flushed to zero, after
rounding with an unbounded exponent). In double precision the IP
computes them with the algorithms of single precision, scaled up: the square
root with a polynomial of degree five instead of two. Clash simulation gives
the IP's results bit for bit: the models ('sqrtModel', 'rsqrtModel', 'recipModel', 'expModel',
'hypotModel', 'log10Model', 'cbrtModel', 'logModel', 'log1pModel', 'log2Model' and
'exp2Model') were
obtained by black-box testing of the IP, see Note [Bit-exact double precision
models] in "Clash.Cores.Intel.Floating.Internal.BitExactDouble", and were
verified against the simulated IP on millions of inputs for MAX 10, Cyclone IV
E, Cyclone 10 LP and (except for 'exp2' and 'log2') Cyclone V, at several
latencies. The results of the
square root, the reciprocal square root and the exponential depend on the
multipliers of the device family, so they take a 'DeviceFamily' argument.

The models of 'exp2' and 'log2' cover MAX 10, Cyclone IV E and Cyclone 10 LP
only. On Cyclone V the IP gives other results for some inputs, which are not
modelled: Clash simulation of these functions for Cyclone V raises an error.
The generated HDL is the same IP on all families.

The models' polynomial coefficients are computed from first principles, except
in two table segments of 'rsqrt', one of 'log' (which 'log1p' uses too), two
of 'log2' and one of 'exp2', where they were observed from the IP's outputs
(see 'rsqrtModel', 'logModel', 'log2Model' and 'exp2Model').

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
  , hypotWith
  , hypot
  , HypotDefDelay
  , log10With
  , log10
  , Log10DefDelay
  , cbrtWith
  , cbrt
  , CbrtDefDelay
  , logWith
  , log
  , LogDefDelay
  , log1pWith
  , log1p
  , Log1pDefDelay
  , log2With
  , log2
  , Log2DefDelay
  , exp2With
  , exp2
  , Exp2DefDelay
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
  , hypotModel
  , log10Model
  , cbrtModel
  , logModel
  , log1pModel
  , log2Model
  , exp2Model
  ) where

import Clash.Explicit.Prelude hiding (add, sub, mul, div, sqrt, recip, exp, log)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Intel.Floating.Internal
  (Config(..), DeviceFamily(..), binaryInst, defConfig, pipeline, ternaryInst, unaryInst)
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

-- | Customizable floating point three-dimensional hypotenuse,
-- @sqrt (x^2 + y^2 + z^2)@, in double precision, see 'hypotModel'. The IP's
-- result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
hypotWith
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
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + d) Double
hypotWith cfg fam clk rst en x y z
  | clashSimulation = pipeline und clk rst en (hypotModel fam <$> x <*> y <*> z)
  | otherwise = ternaryInst "fp_hypot_double" ip clk rst en x y z
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of hypot"
  ip = fpFunctionsDouble cfg "ROOTS" "HYPOT" (natToNum @d) Nil
{-# INLINE hypotWith #-}

-- | Floating point three-dimensional hypotenuse, @sqrt (x^2 + y^2 + z^2)@, in
-- double precision, with the default configuration and delay. See
-- 'hypotWith'.
hypot
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom n Double
  -> DSignal dom (n + HypotDefDelay) Double
hypot = withFrozenCallStack $ hypotWith defConfig
{-# INLINE hypot #-}

-- | The default delay of 'hypot': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest (MAX 10; Cyclone IV E and Cyclone 10 LP: 50, Cyclone V: 44).
type HypotDefDelay = 52

-- | Customizable floating point base 10 logarithm, @log10 x@, in double
-- precision, see 'log10Model'. The IP's result is bit-exactly modelled.
--
-- For Cyclone V, Quartus Prime Lite 21.1 does not generate this IP for any
-- latency (it reports that the frequency target is too low), only for a
-- target frequency, which this function cannot ask for. The simulation model
-- does cover Cyclone V: it gives the results of the IP generated for a target
-- frequency.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
log10With
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
log10With cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (log10Model fam <$> x)
  | otherwise = unaryInst "fp_log10_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of log10"
  ip = fpFunctionsDouble cfg "EXP_LOG" "LOG10" (natToNum @d) Nil
{-# INLINE log10With #-}

-- | Floating point base 10 logarithm, @log10 x@, in double precision, with the
-- default configuration and delay. See 'log10With'.
log10
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + Log10DefDelay) Double
log10 = withFrozenCallStack $ log10With defConfig
{-# INLINE log10 #-}

-- | The default delay of 'log10': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest (Cyclone IV E and Cyclone 10 LP; MAX 10: 48, Cyclone V: 42).
type Log10DefDelay = 49

-- | Customizable floating point cube root, in double
-- precision, see 'cbrtModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
cbrtWith
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
cbrtWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (cbrtModel fam <$> x)
  | otherwise = unaryInst "fp_cbrt_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of cbrt"
  ip = fpFunctionsDouble cfg "ROOTS" "CBRT" (natToNum @d) Nil
{-# INLINE cbrtWith #-}

-- | Floating point cube root, in double precision, with the
-- default configuration and delay. See 'cbrtWith'.
cbrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + CbrtDefDelay) Double
cbrt = withFrozenCallStack $ cbrtWith defConfig
{-# INLINE cbrt #-}

-- | The default delay of 'cbrt': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest (Cyclone IV E and Cyclone 10 LP; MAX 10: 37, Cyclone V: 30).
type CbrtDefDelay = 38

-- | Customizable floating point natural logarithm, in double
-- precision, see 'logModel'. The IP's result is bit-exactly modelled.
--
-- For Cyclone V, Quartus Prime Lite 21.1 does not generate this IP for any
-- latency (it reports that the frequency target is too low), only for a
-- target frequency, which this function cannot ask for. The simulation model
-- does cover Cyclone V: it gives the results of the IP generated for a target
-- frequency.
-- In one of the 2048 segments of its table, the model uses coefficients
-- observed from the IP's outputs instead of computed ones, see 'logModel'.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
logWith
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
logWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (logModel fam <$> x)
  | otherwise = unaryInst "fp_log_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of log"
  ip = fpFunctionsDouble cfg "EXP_LOG" "LOGE" (natToNum @d) Nil
{-# INLINE logWith #-}

-- | Floating point natural logarithm, in double precision, with the
-- default configuration and delay. See 'logWith'.
log
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + LogDefDelay) Double
log = withFrozenCallStack $ logWith defConfig
{-# INLINE log #-}

-- | The default delay of 'log': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest (Cyclone IV E and Cyclone 10 LP; MAX 10: 48, Cyclone V: 44).
type LogDefDelay = 49

-- | Customizable floating point natural logarithm of one plus its argument, @log (1 + x)@, in double
-- precision, see 'log1pModel'. The IP's result is bit-exactly modelled.
--
-- For Cyclone V, Quartus Prime Lite 21.1 does not generate this IP for any
-- latency (it reports that the frequency target is too low), only for a
-- target frequency, which this function cannot ask for. The simulation model
-- does cover Cyclone V: it gives the results of the IP generated for a target
-- frequency.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
log1pWith
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
log1pWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (log1pModel fam <$> x)
  | otherwise = unaryInst "fp_log1p_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of log1p"
  ip = fpFunctionsDouble cfg "EXP_LOG" "LOG1PX" (natToNum @d) Nil
{-# INLINE log1pWith #-}

-- | Floating point natural logarithm of one plus its argument, @log (1 + x)@, in double precision, with the
-- default configuration and delay. See 'log1pWith'.
log1p
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + Log1pDefDelay) Double
log1p = withFrozenCallStack $ log1pWith defConfig
{-# INLINE log1p #-}

-- | The default delay of 'log1p': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest (MAX 10, Cyclone IV E and Cyclone 10 LP; Cyclone V: 55).
type Log1pDefDelay = 59

-- | Customizable floating point base 2 logarithm, @log2 x@, in double
-- precision, see 'log2Model'. The IP's result is bit-exactly modelled.
--
-- Clash simulation supports MAX 10, Cyclone IV E and Cyclone 10 LP only: for
-- Cyclone V the IP's results are not modelled, and simulation raises an error.
-- In two of the 512 segments of its table, the model uses coefficients
-- observed from the IP's outputs instead of computed ones, see 'log2Model'.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
log2With
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
log2With cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (log2Model fam <$> x)
  | otherwise = unaryInst "fp_log2_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of log2"
  ip = fpFunctionsDouble cfg "EXP_LOG" "LOG2" (natToNum @d) Nil
{-# INLINE log2With #-}

-- | Floating point base 2 logarithm, @log2 x@, in double precision, with the
-- default configuration and delay. See 'log2With'.
log2
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + Log2DefDelay) Double
log2 = withFrozenCallStack $ log2With defConfig
{-# INLINE log2 #-}

-- | The default delay of 'log2': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest (MAX 10, Cyclone IV E and Cyclone 10 LP; Cyclone V: 47).
type Log2DefDelay = 51

-- | Customizable floating point base 2 exponential, @2^x@, in double
-- precision, see 'exp2Model'. The IP's result is bit-exactly modelled.
--
-- Clash simulation supports MAX 10, Cyclone IV E and Cyclone 10 LP only: for
-- Cyclone V the IP's results are not modelled, and simulation raises an error.
-- In one of the 64 segments of its table, the model uses coefficients
-- observed from the IP's outputs instead of computed ones, see 'exp2Model'.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
exp2With
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
exp2With cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en (exp2Model fam <$> x)
  | otherwise = unaryInst "fp_exp2_double" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of exp2"
  ip = fpFunctionsDouble cfg "EXP_LOG" "EXP2" (natToNum @d) Nil
{-# INLINE exp2With #-}

-- | Floating point base 2 exponential, @2^x@, in double precision, with the
-- default configuration and delay. See 'exp2With'.
exp2
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Double
  -> DSignal dom (n + Exp2DefDelay) Double
exp2 = withFrozenCallStack $ exp2With defConfig
{-# INLINE exp2 #-}

-- | The default delay of 'exp2': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest (MAX 10, Cyclone IV E and Cyclone 10 LP; Cyclone V: 28).
type Exp2DefDelay = 40
