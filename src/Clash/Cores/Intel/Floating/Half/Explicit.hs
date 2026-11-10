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

The supported functions are 'sqrt', 'recip', 'rsqrt', 'exp', 'exp2', 'exp10', 'log', 'log2', 'log10', 'log1p', 'cbrt', 'hypot', 'sinPi', 'cosPi' and 'tanPi'. In half precision the IP computes them
with the same algorithms as in single precision, scaled down (polynomials of
degree one instead of two). Clash simulation gives the IP's results bit for
bit: the models ('sqrtModel' etc.) were obtained by black-box testing of the
IP, see Note [Bit-exact half precision models] in
"Clash.Cores.Intel.Floating.Internal.BitExactHalf", and were verified against
the simulated IP on /all/ inputs, for MAX 10, Cyclone IV E, Cyclone 10 LP and
Cyclone V, at several latencies. Like in single precision, the results of
most functions depend on the multipliers of the device family, so they take
a 'DeviceFamily' argument.

The user guide promises faithful rounding (within one unit in the last place)
for the square root, the reciprocal square root, the exponentials, the
logarithms and the trigonometric functions. In half precision, only the
square root, the reciprocal square root, the cube root and the base 2
logarithm keep that promise: the other functions are off by more than a unit
in the last place for some inputs, see their models. The reciprocal is
correctly rounded.

Of the trigonometric functions, the sine, cosine and tangent of pi times the
argument are supported, like in single precision.

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
  , expWith
  , exp
  , ExpDefDelay
  , exp2With
  , exp2
  , Exp2DefDelay
  , exp10With
  , exp10
  , Exp10DefDelay
  , logWith
  , log
  , LogDefDelay
  , log2With
  , log2
  , Log2DefDelay
  , log10With
  , log10
  , Log10DefDelay
  , log1pWith
  , log1p
  , Log1pDefDelay
  , cbrtWith
  , cbrt
  , CbrtDefDelay
  , hypotWith
  , hypot
  , HypotDefDelay
  , sinPiWith
  , sinPi
  , SinPiDefDelay
  , cosPiWith
  , cosPi
  , CosPiDefDelay
  , tanPiWith
  , tanPi
  , TanPiDefDelay
    -- * Customizing IP
  , Config(..)
  , defConfig
    -- * Simulation models
  , intelNaN
  , sqrtModel
  , recipModel
  , rsqrtModel
  , expModel
  , exp2Model
  , exp10Model
  , logModel
  , log2Model
  , log10Model
  , log1pModel
  , cbrtModel
  , hypotModel
  , sinPiModel
  , cosPiModel
  , tanPiModel
  ) where

import Clash.Explicit.Prelude hiding (sqrt, recip, exp, log)

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Numeric.Half (Half)

import Clash.Cores.Intel.Floating.Internal
  (Config(..), DeviceFamily(..), defConfig, pipeline, scaleByPi, ternaryInst, unaryInst)
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

-- | Customizable floating point exponential, @e^x@, in half precision, see
-- 'expModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
expWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((expModel fam) <$> x)
  | otherwise = unaryInst "fp_exp_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of exp"
  ip = fpFunctionsHalf cfg "EXP_LOG" "EXPE" (natToNum @d) Nil
{-# INLINE expWith #-}

-- | Floating point exponential, @e^x@, in half precision, with the default configuration
-- and delay. See 'expWith'.
exp
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + ExpDefDelay) Half
exp = withFrozenCallStack $ expWith defConfig
{-# INLINE exp #-}

-- | The default delay of 'exp': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type ExpDefDelay = 12

-- | Customizable floating point base-2 exponential, @2^x@, in half precision, see
-- 'exp2Model'. The IP's result is bit-exactly modelled.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
exp2With cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((exp2Model fam) <$> x)
  | otherwise = unaryInst "fp_exp2_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of exp2"
  ip = fpFunctionsHalf cfg "EXP_LOG" "EXP2" (natToNum @d) Nil
{-# INLINE exp2With #-}

-- | Floating point base-2 exponential, @2^x@, in half precision, with the default configuration
-- and delay. See 'exp2With'.
exp2
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + Exp2DefDelay) Half
exp2 = withFrozenCallStack $ exp2With defConfig
{-# INLINE exp2 #-}

-- | The default delay of 'exp2': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type Exp2DefDelay = 6

-- | Customizable floating point base-10 exponential, @10^x@, in half precision, see
-- 'exp10Model'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
exp10With
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
exp10With cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((exp10Model fam) <$> x)
  | otherwise = unaryInst "fp_exp10_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of exp10"
  ip = fpFunctionsHalf cfg "EXP_LOG" "EXP10" (natToNum @d) Nil
{-# INLINE exp10With #-}

-- | Floating point base-10 exponential, @10^x@, in half precision, with the default configuration
-- and delay. See 'exp10With'.
exp10
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + Exp10DefDelay) Half
exp10 = withFrozenCallStack $ exp10With defConfig
{-# INLINE exp10 #-}

-- | The default delay of 'exp10': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type Exp10DefDelay = 14

-- | Customizable floating point natural logarithm, in half precision, see
-- 'logModel'. The IP's result is bit-exactly modelled.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
logWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((logModel fam) <$> x)
  | otherwise = unaryInst "fp_log_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of log"
  ip = fpFunctionsHalf cfg "EXP_LOG" "LOGE" (natToNum @d) Nil
{-# INLINE logWith #-}

-- | Floating point natural logarithm, in half precision, with the default configuration
-- and delay. See 'logWith'.
log
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + LogDefDelay) Half
log = withFrozenCallStack $ logWith defConfig
{-# INLINE log #-}

-- | The default delay of 'log': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type LogDefDelay = 12

-- | Customizable floating point base 2 logarithm, in half precision, see
-- 'log2Model'. The IP's result is bit-exactly modelled.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
log2With cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((log2Model fam) <$> x)
  | otherwise = unaryInst "fp_log2_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of log2"
  ip = fpFunctionsHalf cfg "EXP_LOG" "LOG2" (natToNum @d) Nil
{-# INLINE log2With #-}

-- | Floating point base 2 logarithm, in half precision, with the default configuration
-- and delay. See 'log2With'.
log2
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + Log2DefDelay) Half
log2 = withFrozenCallStack $ log2With defConfig
{-# INLINE log2 #-}

-- | The default delay of 'log2': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type Log2DefDelay = 12

-- | Customizable floating point base 10 logarithm, in half precision, see
-- 'log10Model'. The IP's result is bit-exactly modelled.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
log10With cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((log10Model fam) <$> x)
  | otherwise = unaryInst "fp_log10_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of log10"
  ip = fpFunctionsHalf cfg "EXP_LOG" "LOG10" (natToNum @d) Nil
{-# INLINE log10With #-}

-- | Floating point base 10 logarithm, in half precision, with the default configuration
-- and delay. See 'log10With'.
log10
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + Log10DefDelay) Half
log10 = withFrozenCallStack $ log10With defConfig
{-# INLINE log10 #-}

-- | The default delay of 'log10': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type Log10DefDelay = 12

-- | Customizable floating point @log (1 + x)@, in half precision, see
-- 'log1pModel'. The IP's result is bit-exactly modelled.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
log1pWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((log1pModel fam) <$> x)
  | otherwise = unaryInst "fp_log1p_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of log1p"
  ip = fpFunctionsHalf cfg "EXP_LOG" "LOG1PX" (natToNum @d) Nil
{-# INLINE log1pWith #-}

-- | Floating point @log (1 + x)@, in half precision, with the default configuration
-- and delay. See 'log1pWith'.
log1p
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + Log1pDefDelay) Half
log1p = withFrozenCallStack $ log1pWith defConfig
{-# INLINE log1p #-}

-- | The default delay of 'log1p': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type Log1pDefDelay = 18

-- | Customizable floating point cube root, in half precision, see
-- 'cbrtModel'. The IP's result is bit-exactly modelled.
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
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
cbrtWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((cbrtModel fam) <$> x)
  | otherwise = unaryInst "fp_cbrt_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of cbrt"
  ip = fpFunctionsHalf cfg "ROOTS" "CBRT" (natToNum @d) Nil
{-# INLINE cbrtWith #-}

-- | Floating point cube root, in half precision, with the default configuration
-- and delay. See 'cbrtWith'.
cbrt
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + CbrtDefDelay) Half
cbrt = withFrozenCallStack $ cbrtWith defConfig
{-# INLINE cbrt #-}

-- | The default delay of 'cbrt': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type CbrtDefDelay = 4

-- | Customizable floating point three-dimensional hypotenuse, @sqrt (x^2 + y^2 + z^2)@, in half precision, see
-- 'hypotModel'. The IP's result is bit-exactly modelled.
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
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + d) Half
hypotWith cfg fam clk rst en x y z
  | clashSimulation = pipeline und clk rst en ((hypotModel fam) <$> x <*> y <*> z)
  | otherwise = ternaryInst "fp_hypot_half" ip clk rst en x y z
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of hypot"
  ip = fpFunctionsHalf cfg "ROOTS" "HYPOT" (natToNum @d) Nil
{-# INLINE hypotWith #-}

-- | Floating point three-dimensional hypotenuse, @sqrt (x^2 + y^2 + z^2)@, in half precision, with the default configuration
-- and delay. See 'hypotWith'.
hypot
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom n Half
  -> DSignal dom (n + HypotDefDelay) Half
hypot = withFrozenCallStack $ hypotWith defConfig
{-# INLINE hypot #-}

-- | The default delay of 'hypot': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type HypotDefDelay = 16

-- | Customizable floating point sine of pi times x, @sin (pi * x)@, in half precision, see
-- 'sinPiModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
sinPiWith
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
sinPiWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((sinPiModel fam) <$> x)
  | otherwise = unaryInst "fp_sinPi_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of sinPi"
  ip = fpFunctionsHalf cfg "TRIG" "SIN" (natToNum @d) (scaleByPi :> Nil)
{-# INLINE sinPiWith #-}

-- | Floating point sine of pi times x, @sin (pi * x)@, in half precision, with the default configuration
-- and delay. See 'sinPiWith'.
sinPi
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + SinPiDefDelay) Half
sinPi = withFrozenCallStack $ sinPiWith defConfig
{-# INLINE sinPi #-}

-- | The default delay of 'sinPi': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type SinPiDefDelay = 11

-- | Customizable floating point cosine of pi times x, @cos (pi * x)@, in half precision, see
-- 'cosPiModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
cosPiWith
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
cosPiWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((cosPiModel fam) <$> x)
  | otherwise = unaryInst "fp_cosPi_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of cosPi"
  ip = fpFunctionsHalf cfg "TRIG" "COS" (natToNum @d) (scaleByPi :> Nil)
{-# INLINE cosPiWith #-}

-- | Floating point cosine of pi times x, @cos (pi * x)@, in half precision, with the default configuration
-- and delay. See 'cosPiWith'.
cosPi
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + CosPiDefDelay) Half
cosPi = withFrozenCallStack $ cosPiWith defConfig
{-# INLINE cosPi #-}

-- | The default delay of 'cosPi': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type CosPiDefDelay = 11

-- | Customizable floating point tangent of pi times x, @tan (pi * x)@, in half precision, see
-- 'tanPiModel'. The IP's result is bit-exactly modelled.
--
-- The 'DeviceFamily' has to be the family of the Quartus project: the IP
-- computes slightly differently on families with different multipliers, see
-- 'DeviceFamily'.
--
-- The latency @d@ is set by the type. Quartus fails to generate the IP if the
-- latency is lower than the IP can achieve on the target device family.
tanPiWith
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
tanPiWith cfg fam clk rst en x
  | clashSimulation = pipeline und clk rst en ((tanPiModel fam) <$> x)
  | otherwise = unaryInst "fp_tanPi_half" ip clk rst en x
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of tanPi"
  ip = fpFunctionsHalf cfg "TRIG" "TAN" (natToNum @d) (scaleByPi :> Nil)
{-# INLINE tanPiWith #-}

-- | Floating point tangent of pi times x, @tan (pi * x)@, in half precision, with the default configuration
-- and delay. See 'tanPiWith'.
tanPi
  :: forall dom n
   . ( KnownDomain dom
     , HasCallStack
     )
  => DeviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> DSignal dom n Half
  -> DSignal dom (n + TanPiDefDelay) Half
tanPi = withFrozenCallStack $ tanPiWith defConfig
{-# INLINE tanPi #-}

-- | The default delay of 'tanPi': the latency Quartus Prime Lite 21.1 picks
-- for a target frequency of 200 MHz, on the device family where this is the
-- largest.
type TanPiDefDelay = 19
