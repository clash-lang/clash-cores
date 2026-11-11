{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation models and instantiation helpers for
"Clash.Cores.Intel.Floating".
-}

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Floating.Internal
  ( -- * Configuration
    Config(..)
  , defConfig
  , Ordering(..)
  , toMaybeOrdering
  , Predicate(..)
  , DeviceFamily(..)
  , familyMultipliers
    -- * Simulation models
  , intelNaN
  , flushDenormal
  , addModel
  , subModel
  , mulModel
  , divModel
  , mulAddModel
  , predicateModel
  , compareModel
  , fromIntegralModel
  , toS32Model
  , toU32Model
  , toDoubleModel
  , fromDoubleModel
  , roundToFloat
  , sqrtModel
  , cbrtModel
  , hypotModel
  , recipModel
  , rsqrtModel
  , expModel
  , exp2Model
  , exp10Model
  , logModel
  , log2Model
  , log10Model
  , log1pModel
  , sinPiModel
  , cosPiModel
  , tanPiModel
  , sinModel
  , cosModel
  , tanModel
  , atanModel
  , atanPiModel
    -- * Instantiation
  , fpFunctions
  , fpFunctionsFormat
  , scaleByPi
  , fxpParams
  , fpFpParams
  , predicateFunction
  , pipeline
  , unaryInst
  , binaryInst
  , ternaryInst
  ) where

import Clash.Explicit.Prelude hiding (Ordering(..))
import qualified Prelude as P

import GHC.Float
  (castFloatToWord32, castWord32ToFloat, castWord64ToDouble, double2Float, float2Double)

import qualified Clash.Signal.Delayed as D

import Data.Word (Word32)

import Clash.Cores.Intel.Floating.Internal.BitExact
import Clash.Cores.Intel.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (unPort)

-- | Customize the Intel floating point IP. These settings influence how the IP
-- is implemented. They do not affect its behavior.
data Config = Config
  { hardFp :: !Bool
    -- ^ Use the hard floating point DSP blocks of Arria 10 and Stratix 10
    -- devices, if the operation supports them. Ignored for other families.
    -- Note that the behavior of the hard floating point blocks is not
    -- modelled or tested: Quartus Prime Lite doesn't support these devices.
  } deriving (Eq, Show)

-- | Default configuration: 'hardFp' is enabled, like in the IP Parameter
-- Editor.
defConfig :: Config
defConfig = Config{hardFp = True}

-- | Result of 'Clash.Cores.Intel.Floating.compare'. Unlike Prelude's
-- 'P.Ordering', this has a constructor for unordered inputs. The constructors
-- are the same as the ones of "Clash.Cores.Xilinx.Floating"'s @Ordering@.
data Ordering
  = EQ
  | LT
  | GT
  | NaN
  -- ^ At least one of the inputs is NaN
  deriving (Generic, NFDataX, BitPack, Eq, ShowX, Show, Lift)

-- | Converts t'Ordering' to Prelude's 'P.Ordering'. If the given ordering is
-- 'NaN', 'Nothing' is returned.
toMaybeOrdering :: Ordering -> Maybe P.Ordering
toMaybeOrdering = \case
  EQ -> Just P.EQ
  LT -> Just P.LT
  GT -> Just P.GT
  NaN -> Nothing

-- | Comparison predicates of the Floating Point Functions IP
data Predicate
  = Lt
  -- ^ Less than
  | Le
  -- ^ Less than or equal
  | Eq
  -- ^ Equal
  | Ne
  -- ^ Not equal. Note that this is 'False' if any input is NaN, unlike
  -- IEEE 754's "not equal" and Haskell's @(/=)@.
  | Gt
  -- ^ Greater than
  | Ge
  -- ^ Greater than or equal
  deriving (Eq, Show, Generic, NFDataX, Lift, Enum, Bounded)

-- | Device family the IP is generated for. The square root, reciprocal square
-- root, exponential and logarithms of the IP compute differently depending on
-- the multipliers of the family, see Note [Bit-exact models] in
-- "Clash.Cores.Intel.Floating.Internal.BitExact". The family has to match the
-- one of the Quartus project (and @CLASH_INTEL_SIM_FAMILY@ when simulating the
-- generated HDL), otherwise the Clash simulation and the IP can differ in the
-- last bit.
--
-- Only the families supported by Quartus Prime Lite 21.1 that have embedded
-- multipliers are listed. The IP on other families (Arria, Stratix, Agilex)
-- has not been tested.
data DeviceFamily
  = CycloneIVE
  -- ^ Cyclone IV E (18x18 multipliers)
  | CycloneV
  -- ^ Cyclone V (27x27 multipliers)
  | Cyclone10LP
  -- ^ Cyclone 10 LP (18x18 multipliers)
  | Max10
  -- ^ MAX 10 (18x18 multipliers)
  deriving (Eq, Show, Enum, Bounded, Generic, NFDataX, Lift)

-- | The kind of multipliers the IP uses on a device family. On families with
-- 18x18 multipliers, the IP truncates some of its products.
familyMultipliers :: DeviceFamily -> Multipliers
familyMultipliers = \case
  CycloneV -> FullMultipliers
  CycloneIVE -> TruncatedMultipliers
  Cyclone10LP -> TruncatedMultipliers
  Max10 -> TruncatedMultipliers

-- | Lift a function on bit patterns to 'Float'
onBits :: (Word32 -> Word32) -> Float -> Float
onBits f = castWord32ToFloat . f . castFloatToWord32

-- | Model of the square root of the IP. It is faithfully rounded (within one
-- unit in the last place), and not always to nearest; this model gives the
-- IP's results bit for bit, see Note [Bit-exact models] in
-- "Clash.Cores.Intel.Floating.Internal.BitExact". Subnormal inputs are treated
-- as zero, @sqrt (-0) = -0@, and negative inputs give 'intelNaN'.
sqrtModel :: DeviceFamily -> Float -> Float
sqrtModel fam = onBits (sqrtSingle (familyMultipliers fam))

-- | Model of the cube root of the IP, see 'sqrtModel'. Zero and subnormal
-- inputs give a zero of the same sign, negative inputs the negated cube root
-- of their magnitude. Unlike the other functions of the IP, a NaN input gives
-- a NaN with the sign of the input: 'intelNaN' for a positive NaN and
-- @0xFF800001@ for a negative one.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all inputs in
-- [1, 8) and (-8, -1] (which covers every segment of its tables and all
-- fractions), on all inputs on a grid with a spacing of 97 bit patterns, and
-- on three million random and special inputs, for MAX 10, Cyclone IV E,
-- Cyclone 10 LP and Cyclone V, at several latencies.
cbrtModel :: DeviceFamily -> Float -> Float
cbrtModel fam = onBits (cbrtSingle (familyMultipliers fam))

-- | Model of the three-dimensional hypotenuse of the IP,
-- @sqrt (x^2 + y^2 + z^2)@, see 'sqrtModel'. The IP truncates the squares to
-- 24 bits, and the smaller ones further when it aligns them to the largest,
-- rounds their sum to nearest and takes its square root like 'sqrtModel', but
-- with a larger rounding constant. So the result is not always faithfully
-- rounded: it can be off by up to about 1.4 units in the last place in either
-- direction (by a unit or more for about 0.5% of random inputs), and even
-- @hypot x 0 0@ is sometimes one unit above @|x|@. Subnormal inputs are
-- treated as zero and the signs of the inputs are ignored. A NaN input gives 'intelNaN', otherwise an
-- infinite input gives infinity; results of @2^128@ or more overflow to
-- infinity.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) for MAX 10,
-- Cyclone IV E, Cyclone 10 LP and Cyclone V on 97 million inputs each: all
-- inputs of 14 binades with the other inputs fixed, chosen to reach every
-- input of the square root and the edge cases of the alignment; inputs with
-- exponents that differ by up to 30 in all positions; all combinations of
-- special values; inputs near the overflow boundary; and two million inputs
-- that tell candidate models apart or are close to a rounding boundary. Other
-- latencies and frequencies were checked on 4.7 million of those inputs.
hypotModel :: DeviceFamily -> Float -> Float -> Float -> Float
hypotModel fam x y z =
  castWord32ToFloat
    (hypotSingle (familyMultipliers fam) (castFloatToWord32 x) (castFloatToWord32 y) (castFloatToWord32 z))

-- | Model of the reciprocal of the IP. It is the IEEE 754 reciprocal, rounded
-- to nearest, ties to even, of the input with subnormal inputs treated as zero
-- and the exponent unbounded. Results below the smallest normal number are
-- flushed to zero, except when the unbounded exponent is one below the one of
-- the smallest normal number: then the IP returns the result's fraction bits
-- with a zero exponent field, dropping the leading one: that bit pattern is a
-- subnormal number with a different value. On every device family, the IP gives the
-- same results.
recipModel :: Float -> Float
recipModel = onBits recipSingle

-- | Model of the reciprocal square root of the IP, see 'sqrtModel'. Zero and
-- subnormal inputs give infinity of their sign, negative inputs and NaN give
-- 'intelNaN'.
rsqrtModel :: DeviceFamily -> Float -> Float
rsqrtModel fam = onBits (rsqrtSingle (familyMultipliers fam))

-- | Model of the exponential of the IP, see 'sqrtModel'. Note that the IP
-- computes @exp (-0)@ and the exponential of small negative numbers as the
-- largest number below one (@0x3F7FFFFF@). Results below the smallest normal
-- number are flushed to zero.
expModel :: DeviceFamily -> Float -> Float
expModel fam = onBits (expSingle (familyMultipliers fam))

-- | Model of the base-2 exponential of the IP, see 'sqrtModel'. The IP
-- truncates its input to 23 fraction bits, so for inputs below one in
-- magnitude the result can be off by up to two units in the last place: unlike
-- what the user guide promises, it is not always faithfully rounded. Like
-- 'expModel', the IP computes @2^(-0)@ and the base-2 exponential of small
-- negative numbers as the largest number below one (@0x3F7FFFFF@). Results
-- below the smallest normal number are flushed to zero.
exp2Model :: DeviceFamily -> Float -> Float
exp2Model fam = onBits (exp2Single (familyMultipliers fam))

-- | Model of the base-10 exponential of the IP, see 'sqrtModel'. The IP adds a
-- whole unit in the last place before truncating the result, so it is often
-- one unit above the correctly rounded result: @10^0@ is @1 + 2^-23@
-- (@0x3F800001@) and @10^(-0)@ is @0x3F7FFFFE@. Just below a multiple of
-- @log10 2@, the result can be off by a few units in the last place: unlike
-- what the user guide promises, it is not always faithfully rounded. Results
-- below the smallest normal number are flushed to zero.
exp10Model :: DeviceFamily -> Float -> Float
exp10Model fam = onBits (exp10Single (familyMultipliers fam))

-- | Model of the natural logarithm of the IP, see 'sqrtModel'. Zero and
-- subnormal inputs give minus infinity, negative inputs and NaN give
-- 'intelNaN'.
logModel :: DeviceFamily -> Float -> Float
logModel fam = onBits (logSingle (familyMultipliers fam))

-- | Model of the base 2 logarithm of the IP, see 'sqrtModel'. Powers of two
-- give exact results. Zero and subnormal inputs give minus infinity, negative
-- inputs and NaN give 'intelNaN'.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all inputs in
-- [0.25, 4) and on about three million random, special and hard-to-round
-- inputs, for MAX 10, Cyclone IV E, Cyclone 10 LP and Cyclone V, at several
-- latencies.
log2Model :: DeviceFamily -> Float -> Float
log2Model fam = onBits (log2Single (familyMultipliers fam))

-- | Model of the base 10 logarithm of the IP, see 'sqrtModel'. Zero and
-- subnormal inputs give minus infinity, negative inputs and NaN give
-- 'intelNaN'.
--
-- Verified like 'log2Model'.
log10Model :: DeviceFamily -> Float -> Float
log10Model fam = onBits (log10Single (familyMultipliers fam))

-- | Model of @log (1 + x)@ of the IP, see 'sqrtModel'. Small inputs keep their
-- precision: the IP computes @x * P (1 + x)@ for @1 + x@ in @[0.5, 2)@, and
-- for @|x| < 2^-24@ the result is @x@. Zero and subnormal inputs give a zero of
-- the same sign, -1 gives minus infinity, inputs below -1 and NaN give
-- 'intelNaN'.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all inputs
-- in @[2^-8, 4)@, @(-1, -2^-8]@ and two binades of large inputs for the Cyclone
-- V datapath, on @[2^-6, 2)@ and @(-1, -2^-6]@ for the other one, on all inputs
-- in @[0.5, 2)@ and @(-1, -0.25]@ for every configuration, and on about 2.4
-- million random, special and hard-to-round inputs, for MAX 10, Cyclone IV E,
-- Cyclone 10 LP and Cyclone V, at several latencies.
log1pModel :: DeviceFamily -> Float -> Float
log1pModel fam = onBits (log1pSingle (familyMultipliers fam))

-- | Model of the IP's @sin (pi * x)@ (the \"SIN\" function with the angle
-- represented as a multiple of pi), see 'sqrtModel'. The IP computes
-- @r * P(r)@ with @r@ the distance of @x@ to the nearest integer and @P@ a
-- polynomial approximation of @sin (pi * r) / r@. For @x@ in @[2^-12, 0.5)@
-- it drops the last bit of @x@'s significand, so there the result can be off
-- by up to about two and a half units in the last place: unlike what the user
-- guide promises, it is not always faithfully rounded. Integers and inputs of
-- magnitude @2^23@ and above give a zero with the sign of @x@, as do zero and
-- subnormal inputs. Infinities give 'intelNaN' with the sign of the input,
-- other NaN inputs 'intelNaN' with the opposite sign.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all positive
-- inputs in @[2^-15, 2)@ and all NaN inputs (for both datapaths), on all
-- inputs in @[0.5, 1)@ for every configuration and on two million random and
-- special inputs, for MAX 10, Cyclone IV E, Cyclone 10 LP and Cyclone V, at
-- several latencies.
sinPiModel :: DeviceFamily -> Float -> Float
sinPiModel fam = onBits (sinPiSingle (familyMultipliers fam))

-- | Model of the IP's @cos (pi * x)@ (the \"COS\" function with the angle
-- represented as a multiple of pi), see 'sqrtModel' and 'sinPiModel': the IP
-- computes @sin (pi * r)@ with @r@ the distance of @x@ to the nearest half
-- integer, with the polynomial of 'sinPiModel'. Inputs of magnitude below
-- @2^-13@ give exactly one; half integers give positive zero; NaN and the
-- infinities give 'intelNaN'.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all positive
-- inputs in @[2^-15, 2^-7)@ (Cyclone V datapath) and @[2^-8, 2)@ (other
-- families), on all inputs in @[0.5, 1)@ for every configuration and on two
-- million random and special inputs, for MAX 10, Cyclone IV E, Cyclone 10 LP
-- and Cyclone V, at several latencies.
cosPiModel :: DeviceFamily -> Float -> Float
cosPiModel fam = onBits (cosPiSingle (familyMultipliers fam))

-- | Model of the IP's @tan (pi * x)@ (the \"TAN\" function with the angle
-- represented as a multiple of pi), see 'sqrtModel'. The IP divides its sine
-- of pi times x ('sinPiModel') by its cosine of pi times x ('cosPiModel'),
-- both rounded to single precision, multiplying by a polynomial approximation
-- of the reciprocal, so the result can be off by a few units in the last place
-- (up to about five for inputs in @[2^-12, 0.5)@, where the sine drops the
-- last bit of the input): unlike what the user guide promises, it is not
-- always faithfully rounded. Inputs of magnitude below @2^-13@ give @x@ times pi
-- rounded to single precision, rounded; zero and subnormal inputs are
-- returned unchanged; integers give a zero and half integers an infinity,
-- with the sign of the quotient; NaN and the infinities give 'intelNaN'.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all positive
-- inputs in @[0.25, 1)@, on every 7th input in @[2^-15, 0.25)@, every 31st in
-- @[1, 2^24)@, every third in @[0.25, 0.5)@ for every configuration and on two
-- million random and special inputs, for MAX 10, Cyclone IV E, Cyclone 10 LP
-- and Cyclone V, at several latencies.
tanPiModel :: DeviceFamily -> Float -> Float
tanPiModel fam = onBits (tanPiSingle (familyMultipliers fam))

-- | Model of the IP's sine of @x@ in radians (the \"SIN\" function), see
-- 'sqrtModel' and 'sinPiModel'. The IP reduces @x@ to @|x| / pi@ modulo 2 with
-- 54 fraction bits (from an 80-bit window of the bits of 1 / pi) and then
-- computes @sin (pi * r)@ for the distance @r@ to the nearest integer as
-- @r * P(r)@, with @P@ a polynomial approximation of @sin (pi * r) / r@ and
-- @r@ truncated to 26 significant bits. Inputs of magnitude below @2^-11@
-- (and some below @2^-10@) give @x@; zero and subnormal inputs give a zero
-- with the sign of @x@; NaN and the infinities give 'intelNaN' with a sign
-- that depends on the input's bits.
--
-- The result can be off by up to about 1.6 units in the last place: unlike
-- what the user guide promises, it is not always faithfully rounded.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all positive
-- inputs in @[2^-11, 2^-9)@ and @[0.5, 4)@, every 97th input in
-- @[2^-27, 2^-11)@ and every 173rd input in @[4, 2^128)@, all positive NaN and
-- infinite inputs (for both
-- datapaths), on every third input in @[1, 2)@ for every configuration, on
-- all inputs within @2^-10 * pi@ of a multiple of @pi / 2@ below @2^13@, on
-- the inputs where the result depends on the precision of the reduction and
-- on about 2.4 million random, special and hard-to-reduce inputs, for MAX 10,
-- Cyclone IV E, Cyclone 10 LP and Cyclone V, at several latencies.
sinModel :: DeviceFamily -> Float -> Float
sinModel fam = onBits (sinSingle (familyMultipliers fam))

-- | Model of the IP's cosine of @x@ in radians (the \"COS\" function), see
-- 'sqrtModel' and 'sinModel': the IP computes @sin (pi * r)@ with @r@ the
-- distance of @|x| / pi@ to the nearest half integer, as for 'sinModel' (but
-- with 55 fraction bits).
-- Inputs of magnitude below @2^-11@ give exactly one; NaN and the infinities
-- give 'intelNaN'.
--
-- The result can be off by up to about 1.2 units in the last place: unlike
-- what the user guide promises, it is not always faithfully rounded.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all positive
-- inputs in @[2^-11, 2^-9)@ and @[0.5, 4)@, every 97th input in
-- @[2^-27, 2^-11)@ and every 173rd input in @[4, 2^128)@ (for both
-- datapaths), on every third input in @[1, 2)@ for every configuration, on
-- all inputs within @2^-10 * pi@ of a multiple of @pi / 2@ below @2^13@, on
-- the inputs where the result depends on the precision of the reduction and
-- on about 2.4 million random, special and hard-to-reduce inputs, for MAX 10,
-- Cyclone IV E, Cyclone 10 LP and Cyclone V, at several latencies.
cosModel :: DeviceFamily -> Float -> Float
cosModel fam = onBits (cosSingle (familyMultipliers fam))

-- | Model of the IP's tangent of @x@ in radians (the \"TAN\" function), see
-- 'sqrtModel'. The IP divides its sine ('sinModel') by its cosine
-- ('cosModel'), both rounded to single precision, as for 'tanPiModel'.
-- Inputs of magnitude below @2^-11@ give @x@; zero and subnormal inputs give
-- a zero with the sign of @x@; NaN and the infinities give 'intelNaN'.
--
-- The result can be off by up to about 2.5 units in the last place: unlike
-- what the user guide promises, it is not always faithfully rounded.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all positive
-- inputs in @[2^-11, 2^-9)@ and @[0.5, 4)@, every 97th input in
-- @[2^-27, 2^-11)@ and every 173rd input in @[4, 2^128)@ (for both
-- datapaths), on every third input in @[1, 2)@ for every configuration, on
-- all inputs within @2^-10 * pi@ of a multiple of @pi / 2@ below @2^13@, on
-- the inputs where the result depends on the precision of the reduction and
-- on about 2.4 million random, special and hard-to-reduce inputs, for MAX 10,
-- Cyclone IV E, Cyclone 10 LP and Cyclone V, at several latencies.
tanModel :: DeviceFamily -> Float -> Float
tanModel fam = onBits (tanSingle (familyMultipliers fam))

-- | Model of the IP's arctangent (the \"ARCTAN\" function), see 'sqrtModel'.
-- For @|x| <= 1@ the IP computes @x * P(x)@, with @P@ a polynomial
-- approximation of @atan x / x@, rounded once. For @|x| > 1@ it computes
-- @pi / 2 - atan z@ with @z = 1 / x@ from a polynomial approximation of the
-- reciprocal, in fixed point: pi \/ 2 rounded up to @2^-24@ minus @atan z@
-- rounded down to @2^-24@, truncated to single precision. Inputs of magnitude
-- below @2^-12@ give @x@; zero and subnormal inputs give a zero with the sign
-- of @x@; the infinities give pi \/ 2 rounded down (@0x3FC90FDA@) with the
-- sign of @x@, and NaN gives 'intelNaN'.
--
-- The result can be off by up to about 2.2 units in the last place: unlike
-- what the user guide promises, it is not always faithfully rounded.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) for both
-- datapaths (MAX 10 and Cyclone V) on all positive inputs in @[2^-8, 2)@,
-- every 16th input of either sign in @[1, 2^34)@, a sample of 19 million
-- inputs below @2^-9@, 2 million random and special inputs and all inputs
-- whose results are exact ties at @2^-24@ or close to one; and for Cyclone IV
-- E and Cyclone 10 LP, and all four families at the minimum latency, on all
-- inputs in @[1, 2)@, the tie inputs and the random ones.
atanModel :: DeviceFamily -> Float -> Float
atanModel fam = onBits (atanSingle (familyMultipliers fam))

-- | Model of the IP's arctangent divided by pi (the \"ARCTAN\" function with
-- the angle represented as a multiple of pi), see 'sqrtModel': the IP
-- multiplies its arctangent ('atanModel'), rounded to single precision, by
-- 1 \/ pi rounded to single precision (@0x3EA2F983@), rounding to nearest
-- with ties away from zero and flushing results below the smallest normal
-- number to zero; on MAX 10, Cyclone IV E and Cyclone 10 LP this product
-- leaves out some partial products. NaN gives 'intelNaN'.
--
-- The result can be off by up to about 1.9 units in the last place: unlike
-- what the user guide promises, it is not always faithfully rounded.
--
-- Verified against the simulated IP (Quartus Prime Lite 21.1) on all
-- positive inputs in @[0.5, 2)@ (MAX 10, Cyclone V) or @[1, 2)@ (Cyclone IV
-- E, Cyclone 10 LP), every 128th input of either sign in @[1, 2^34)@, 2
-- million random and special inputs and the inputs where the arctangent is
-- an exact tie or close to one, for all four families.
atanPiModel :: DeviceFamily -> Float -> Float
atanPiModel fam = onBits (atanPiSingle (familyMultipliers fam))

-- | NaN as generated by the Intel IP: positive, with the least significant bit
-- of the mantissa set (@0x7F800001@). Whatever the inputs, the arithmetic
-- functions of the IP generate this NaN. Conversions between 'Float' and
-- 'Double' keep the sign of NaNs, see 'fromDoubleModel'.
intelNaN :: Float
intelNaN = castWord32ToFloat 0x7F800001

-- | Treat a subnormal number as zero, keeping its sign, like the IP does with
-- its inputs.
flushDenormal :: Float -> Float
flushDenormal x
  | isDenormalized x = if isNegativeZero x || x < 0 then -0 else 0
  | otherwise = x

-- | Round to 'Float' like the IP does: round to nearest, ties to even, as if
-- the exponent were unbounded. A result that rounds to a magnitude below the
-- smallest normal number flushes to zero, keeping its sign. NaNs become
-- 'intelNaN'.
--
-- The 'Double' must be the result of a single IEEE 754 operation on 'Float'
-- operands (addition, subtraction, multiplication or division): rounding such
-- a result first to 'Double' and then to 'Float' is the same as rounding it to
-- 'Float' directly.
roundToFloat :: Double -> Float
roundToFloat d
  | isNaN d = intelNaN
  | abs d >= minNormal = double2Float d
  | d == 0 = double2Float d
  | otherwise =
      -- Round as if the exponent were unbounded: scale the number into the
      -- normal range of 'Float' (exact, as 'Double' has a larger range) and
      -- round there. As the number is below the smallest normal number, it
      -- either rounds up to it, or it is flushed.
      let scaled = double2Float (d * 2 ^^ (64 :: Int))
      in if abs scaled >= 2 ^^ (-126 + 64 :: Int)
           then (if d < 0 then -1 else 1) * 2 ^^ (-126 :: Int)
           else (if d < 0 then -0 else 0)
 where
  minNormal = 2 ^^ (-126 :: Int)

-- | Model of a binary operation of the IP
binaryModel :: (Double -> Double -> Double) -> Float -> Float -> Float
binaryModel op x y =
  roundToFloat (float2Double (flushDenormal x) `op` float2Double (flushDenormal y))

-- | Model of the floating point addition of the IP: IEEE 754 addition, rounding
-- to nearest with ties to even, but with subnormal inputs treated as zero and
-- results flushed to zero as described in 'roundToFloat'. NaN results are
-- always 'intelNaN'.
addModel :: Float -> Float -> Float
addModel = binaryModel (+)

-- | Model of the floating point subtraction of the IP, see 'addModel'
subModel :: Float -> Float -> Float
subModel = binaryModel (-)

-- | Model of the floating point multiplication of the IP, see 'addModel'
mulModel :: Float -> Float -> Float
mulModel = binaryModel (*)

-- | Model of the floating point division of the IP, see 'addModel'
divModel :: Float -> Float -> Float
divModel = binaryModel (/)

-- | Model of the multiply-add function of the IP: @a * b + c@. Note that this
-- is /not/ a fused multiply-add: the product is rounded (and flushed) before
-- the addition, like 'mulModel' does.
mulAddModel :: Float -> Float -> Float -> Float
mulAddModel a b c = addModel (mulModel a b) c

-- | Model of the comparison functions of the IP. Subnormal inputs are treated
-- as zero. Every predicate is 'False' if any of the inputs is NaN.
predicateModel :: Predicate -> Float -> Float -> Bool
predicateModel p x0 y0
  | isNaN x || isNaN y = False
  | otherwise = case p of
      Lt -> x < y
      Le -> x <= y
      Eq -> x == y
      Ne -> x /= y
      Gt -> x > y
      Ge -> x >= y
 where
  x = flushDenormal x0
  y = flushDenormal y0

-- | Model of 'Clash.Cores.Intel.Floating.compare'
compareModel :: Float -> Float -> Ordering
compareModel x y
  | predicateModel Lt x y = LT
  | predicateModel Eq x y = EQ
  | predicateModel Gt x y = GT
  | otherwise = NaN

-- | Model of the conversion of integers to floating point numbers of the IP:
-- round to nearest, ties to even.
fromIntegralModel :: Integral a => a -> Float
fromIntegralModel i = double2Float (fromIntegral i)

-- | Round to nearest integer, ties away from zero. Saturates to the given
-- bounds, NaN is treated as infinity with the same sign.
roundAwaySaturate :: Integer -> Integer -> Float -> Integer
roundAwaySaturate lo hi x0
  | isNaN x || isInfinite x = if x < 0 || isNegativeZero x || signNaN then lo else hi
  | otherwise = max lo (min hi rounded)
 where
  x = flushDenormal x0
  -- Sign of a NaN
  signNaN = isNaN x && testBit (pack x) 31
  d = float2Double x
  -- Exact in 'Double' for magnitudes where 'Float' has a fraction; larger
  -- numbers are integers already.
  rounded
    | abs d >= 4503599627370496 = P.truncate d
    | d >= 0 = P.floor (d + 0.5)
    | otherwise = P.ceiling (d - 0.5)

-- | Model of the conversion of floating point numbers to @Signed 32@ of the
-- IP: round to nearest, ties away from zero. Out of range numbers saturate,
-- NaN converts like infinity of the same sign.
toS32Model :: Float -> Signed 32
toS32Model = fromInteger . roundAwaySaturate (-2147483648) 2147483647

-- | Model of the conversion of floating point numbers to @Unsigned 32@ of the
-- IP, see 'toS32Model'. Negative numbers convert to zero.
toU32Model :: Float -> Unsigned 32
toU32Model = fromInteger . roundAwaySaturate 0 4294967295

-- | Model of the conversion of 'Float' to 'Double' of the IP. The conversion is
-- exact, except that subnormal numbers are treated as zero. Unlike the
-- arithmetic functions, conversions keep the sign of a NaN: the result is
-- @0x7FF0000000000001@ or @0xFFF0000000000001@.
toDoubleModel :: Float -> Double
toDoubleModel x
  | isNaN x = castWord64ToDouble (if testBit (pack x) 31 then 0xFFF0000000000001 else 0x7FF0000000000001)
  | otherwise = float2Double (flushDenormal x)

-- | Model of the conversion of 'Double' to 'Float' of the IP. Subnormal inputs
-- are treated as zero, the result is rounded like 'roundToFloat' does. NaNs
-- keep their sign: the result is @0x7F800001@ or @0xFF800001@.
fromDoubleModel :: Double -> Float
fromDoubleModel x
  | isNaN x = castWord32ToFloat (if testBit (pack x) 63 then 0xFF800001 else 0x7F800001)
  | isDenormalized x = if testBit (pack x) 63 then -0 else 0
  | otherwise = roundToFloat x

-- | Parameters of an @altera_fp_functions@ IP (Floating Point Functions) in
-- single precision
fpFunctions ::
  KnownNat n =>
  Config ->
  -- | Function family, e.g. @ARITH@
  String ->
  -- | Function, e.g. @ADD@
  String ->
  -- | Latency
  Integer ->
  -- | Extra parameters
  Vec n (String, IpParam) ->
  IntelIp (n + 7)
fpFunctions = fpFunctionsFormat (("fp_format", StrParam "single") :> Nil)
{-# INLINE fpFunctions #-}

-- | Parameters of an @altera_fp_functions@ IP (Floating Point Functions) in
-- the given format
fpFunctionsFormat ::
  (KnownNat m, KnownNat n) =>
  -- | Format parameters, e.g. @fp_format@
  Vec m (String, IpParam) ->
  Config ->
  -- | Function family, e.g. @ARITH@
  String ->
  -- | Function, e.g. @ADD@
  String ->
  -- | Latency
  Integer ->
  -- | Extra parameters
  Vec n (String, IpParam) ->
  IntelIp (n + m + 6)
fpFunctionsFormat format cfg family function latency extra = IntelIp
  { ipKind = "altera_fp_functions"
  , ipParams =
       ("FUNCTION_FAMILY", StrParam family)
    :> (family <> "_function", StrParam function)
    :> (format
    ++ ("performance_goal", StrParam "latency")
    :> ("latency_target", IntegerParam latency)
    :> ("gen_enable", BoolParam True)
    :> ("enable_hard_fp", BoolParam (hardFp cfg))
    :> extra)
  }
{-# INLINE fpFunctionsFormat #-}

-- | The extra parameter of 'fpFunctions' for the trigonometric functions of pi
-- times their argument (\"Represent angle as multiple of Pi\")
scaleByPi :: (String, IpParam)
scaleByPi = ("scale_by_pi", BoolParam True)

-- | Parameters for conversions between floating point numbers and 32 bit
-- integers
fxpParams ::
  -- | Signed?
  Bool ->
  Vec 3 (String, IpParam)
fxpParams signed =
     ("fxpt_width", IntegerParam 32)
  :> ("fxpt_fraction", IntegerParam 0)
  :> ("fxpt_sign", StrParam (if signed then "1" else "0"))
  :> Nil
{-# INLINE fxpParams #-}

-- | Parameters for conversions between floating point formats
fpFpParams ::
  -- | Input format, e.g. @single@
  String ->
  -- | Output format, e.g. @double@
  String ->
  Vec 2 (String, IpParam)
fpFpParams fmtIn fmtOut =
     ("fp_in_format", StrParam fmtIn)
  :> ("fp_out_format", StrParam fmtOut)
  :> Nil
{-# INLINE fpFpParams #-}

-- | Name of the comparison function of the IP
predicateFunction :: Predicate -> String
predicateFunction = \case
  Lt -> "LT"
  Le -> "LE"
  Eq -> "EQ"
  Ne -> "NEQ"
  Gt -> "GT"
  Ge -> "GE"

-- | Simulation model of the pipeline of the IP: @d@ registers that are
-- enabled by the 'Enable' signal. The registers have undefined initial values,
-- and are undefined after a reset: the IP clears its registers, but the
-- corresponding outputs don't have specified values.
pipeline ::
  forall d n dom a.
  (KnownDomain dom, KnownNat d, NFDataX a) =>
  -- | Undefined value
  a ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  DSignal dom n a ->
  DSignal dom (n + d) a
pipeline und clk rst en =
  D.unsafeFromSignal . last . iterate (SNat @(d + 1)) (register clk rst en und) . D.toSignal
{-# INLINE pipeline #-}

-- | 'BitVector' of an 'Enable', for the @en@ port of the IP
enPort :: Enable dom -> Signal dom (BitVector 1)
enPort en = pack . boolToBit <$> fromEnable en

-- | Instantiate a Floating Point Functions IP with a single input
unaryInst ::
  forall d n dom m a b.
  (KnownDomain dom, KnownNat m, BitPack a, BitPack b) =>
  -- | Instance name
  String ->
  IntelIp m ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  DSignal dom n a ->
  DSignal dom (n + d) b
unaryInst nm ip clk rst en a = D.unsafeFromSignal (unpack <$> unPort q)
 where
  q :: Port "q" dom (BitVector (BitSize b))
  q =
    instWithIntelIp
      (instConfig nm)
      ip
      (ClockPort @"clk" clk)
      (ResetPort @"areset" @ActiveHigh rst)
      (Port @"en" (enPort en))
      (Port @"a" (pack <$> D.toSignal a))
{-# INLINE unaryInst #-}

-- | Instantiate a Floating Point Functions IP with two inputs
binaryInst ::
  forall d n dom m a b c.
  (KnownDomain dom, KnownNat m, BitPack a, BitPack b, BitPack c) =>
  -- | Instance name
  String ->
  IntelIp m ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  DSignal dom n a ->
  DSignal dom n b ->
  DSignal dom (n + d) c
binaryInst nm ip clk rst en a b = D.unsafeFromSignal (unpack <$> unPort q)
 where
  q :: Port "q" dom (BitVector (BitSize c))
  q =
    instWithIntelIp
      (instConfig nm)
      ip
      (ClockPort @"clk" clk)
      (ResetPort @"areset" @ActiveHigh rst)
      (Port @"en" (enPort en))
      (Port @"a" (pack <$> D.toSignal a))
      (Port @"b" (pack <$> D.toSignal b))
{-# INLINE binaryInst #-}

-- | Instantiate a Floating Point Functions IP with three inputs
ternaryInst ::
  forall d n dom m a b c e.
  (KnownDomain dom, KnownNat m, BitPack a, BitPack b, BitPack c, BitPack e) =>
  -- | Instance name
  String ->
  IntelIp m ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  DSignal dom n a ->
  DSignal dom n b ->
  DSignal dom n c ->
  DSignal dom (n + d) e
ternaryInst nm ip clk rst en a b c = D.unsafeFromSignal (unpack <$> unPort q)
 where
  q :: Port "q" dom (BitVector (BitSize e))
  q =
    instWithIntelIp
      (instConfig nm)
      ip
      (ClockPort @"clk" clk)
      (ResetPort @"areset" @ActiveHigh rst)
      (Port @"en" (enPort en))
      (Port @"a" (pack <$> D.toSignal a))
      (Port @"b" (pack <$> D.toSignal b))
      (Port @"c" (pack <$> D.toSignal c))
{-# INLINE ternaryInst #-}
