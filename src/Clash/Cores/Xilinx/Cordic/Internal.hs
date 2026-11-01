{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation model and configuration of the Xilinx CORDIC LogiCORE IP v6.0. See
"Clash.Cores.Xilinx.Cordic" for the user facing API.

The simulation model is a CORDIC implementation following
[PG105](https://docs.amd.com/v/u/en-US/pg105-cordic) and the classic papers by
Volder (1959) and Walther (1971). It operates on arbitrarily large 'Integer's
holding fixed-point numbers, reproducing the datapath of the IP:

* The number of iterations @n@ and the internal precision @P@ (the width of the
  datapath) are determined as described in PG105 (/Advanced Configuration
  Parameters/), or set explicitly.

* X and Y are held with @P - 2@ fractional bits, the phase with @P - 3@
  fractional bits. The micro-rotation shifts truncate (round towards negative
  infinity), as an arithmetic shift does.

* The arctangent constants are rounded to the nearest representable value. The
  initial value of the vector for Sin and Cos is the inverse of the CORDIC
  scale factor, rounded to the nearest representable value.

* Coarse rotation rotates vector inputs into the first quadrant (-45 to +45
  degrees) and reduces phase inputs to that same range by subtracting a
  multiple of pi\/2, rotating the result back afterwards. The multiples of pi\/2
  and the quadrant boundaries are rounded up to the next representable value.

* Compensation scaling multiplies the result by the inverse of the CORDIC scale
  factor rounded to the nearest representable value, truncating the product to
  the internal precision. Whether the multiplier is built from LUTs, block RAM
  or DSP slices does not affect the result.

* Rounding of the outputs happens at the very end. Round to nearest even breaks
  ties of negative numbers towards an /odd/ result.

* Outputs that do not fit the output width wrap around. This can happen for
  the magnitude of rotated and translated vectors, which the CORDIC scale
  factor grows beyond 2 unless compensation scaling is enabled.

These details were validated against Xilinx's bit-accurate C model of the IP,
see @test\/Test\/Cores\/Xilinx\/Cordic.hs@, and against the IP itself in
Vivado's simulator.

__NB__: None of the functions in this module are synthesizable.
-}

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Cordic.Internal
  ( -- * Configuration
    Config(..)
  , defConfig
  , RoundMode(..)
  , PhaseFormat(..)
  , CompensationScaling(..)
  , CordicFunction(..)
    -- * Resolved parameters
  , Params(..)
  , mkParams
  , autoIterations
  , autoPrecision
  , latency
    -- * Models
  , modelRotate
  , modelTranslate
  , modelSinCos
  , modelArcTan
    -- * Wizard options
  , functionToTcl
  , roundModeToTcl
  , phaseFormatToTcl
  , compensationScalingToTcl
    -- * Constants (exported for testing)
  , piCeil
  , atanRounded
  , inverseScaleFactor
  , isqrt
  ) where

import Prelude

import Data.Bits (shiftL, shiftR, testBit)
import Data.Ratio (denominator, numerator, (%))
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

-- | Rounding of the outputs, see /Round Mode/ in PG105.
data RoundMode
  = Truncate
    -- ^ Round towards negative infinity
  | RoundPosInf
    -- ^ Round to nearest, ties towards positive infinity
  | RoundPosNegInf
    -- ^ Round to nearest, ties away from zero
  | RoundNearestEven
    -- ^ Round to nearest, ties to even. __NB__: the IP breaks ties of
    -- negative numbers towards an /odd/ result. See
    -- "Clash.Cores.Xilinx.Cordic" for details.
  deriving (Eq, Show, Generic, Lift)

-- | Representation of phases (angles), see /Phase Format/ in PG105.
data PhaseFormat
  = Radians
    -- ^ The phase is in radians
  | ScaledRadians
    -- ^ The phase is in units of pi radians, so @1@ means pi radians
  deriving (Eq, Show, Generic, Lift)

-- | Compensation of the CORDIC scale factor for 'Clash.Cores.Xilinx.Cordic.rotate'
-- and 'Clash.Cores.Xilinx.Cordic.translate', see /Compensation Scaling/ in PG105.
-- Both kinds of compensation give the same results; they differ in the
-- resources they use and in their latency.
--
-- The IP can also compensate with DSP slices (/Embedded Multiplier/). This is
-- not supported: it gives the same results as the other two, but its latency
-- depends on the multiplier's operand widths and the device family in ways we
-- could not pin down.
data CompensationScaling
  = NoScaleCompensation
    -- ^ Do not compensate: the outputs are scaled by the CORDIC scale factor,
    -- which is approximately 1.6468.
  | LutBased
    -- ^ Compensate with a constant coefficient multiplier built from LUTs
  | BramBased
    -- ^ Compensate with a constant coefficient multiplier using block RAM
  deriving (Eq, Show, Generic, Lift)

-- | Customization of the Xilinx CORDIC IP. Not every option applies to every
-- function; options that do not apply are ignored.
data Config = Config
  { roundMode :: !RoundMode
    -- ^ Rounding of the outputs
  , phaseFormat :: !PhaseFormat
    -- ^ Representation of phases. Applies to functions with a circular phase
    -- input or output: rotate, translate, sin and cos, and arctan.
  , coarseRotation :: !Bool
    -- ^ Extend the range of the inputs to the full circle. If 'False', inputs
    -- are limited to the first quadrant: phases between -pi\/4 and pi\/4 and
    -- vectors with an angle in that range. Applies to rotate, translate, sin
    -- and cos, and arctan.
  , compensationScaling :: !CompensationScaling
    -- ^ Compensation of the CORDIC scale factor. Applies to rotate and
    -- translate.
  , iterations :: !(Maybe Int)
    -- ^ Number of iterations. 'Nothing' selects the number automatically,
    -- based on the output width (see /Advanced Configuration Parameters/ in
    -- PG105). Must be between 1 and 48.
  , precision :: !(Maybe Int)
    -- ^ Width of the internal datapath. 'Nothing' selects the width
    -- automatically, based on the output width and the number of iterations.
    -- Must be at least the output width and at most 48.
  } deriving (Eq, Show, Generic)

-- | Default customization options, matching the defaults of the IP:
--
-- * Truncate the outputs ('Truncate')
-- * Phases in radians ('Radians')
-- * Coarse rotation enabled
-- * No compensation of the CORDIC scale factor ('NoScaleCompensation')
-- * Automatically determined number of iterations and precision
defConfig :: Config
defConfig = Config
  { roundMode = Truncate
  , phaseFormat = Radians
  , coarseRotation = True
  , compensationScaling = NoScaleCompensation
  , iterations = Nothing
  , precision = Nothing
  }

-- | The functions the IP can compute
data CordicFunction
  = Rotate
  | Translate
  | SinCos
  | ArcTan
  deriving (Eq, Show, Generic, Lift)

functionToTcl :: CordicFunction -> String
functionToTcl = \case
  Rotate -> "Rotate"
  Translate -> "Translate"
  SinCos -> "Sin_and_Cos"
  ArcTan -> "Arc_Tan"

roundModeToTcl :: RoundMode -> String
roundModeToTcl = \case
  Truncate -> "Truncate"
  RoundPosInf -> "Round_Pos_Inf"
  RoundPosNegInf -> "Round_Pos_Neg_Inf"
  RoundNearestEven -> "Nearest_Even"

phaseFormatToTcl :: PhaseFormat -> String
phaseFormatToTcl = \case
  Radians -> "Radians"
  ScaledRadians -> "Scaled_Radians"

compensationScalingToTcl :: CompensationScaling -> String
compensationScalingToTcl = \case
  NoScaleCompensation -> "No_Scale_Compensation"
  LutBased -> "LUT_based"
  BramBased -> "BRAM"

-- | Configuration with all automatic parameters resolved
data Params = Params
  { function :: !CordicFunction
  , inputWidth :: !Int
  , outputWidth :: !Int
  , nIterations :: !Int
  , internalPrecision :: !Int
  , coarse :: !Bool
    -- ^ Coarse rotation, only 'True' for functions that support it
  , scaled :: !Bool
    -- ^ Phases in scaled radians
  , rounding :: !RoundMode
  , compensate :: !Bool
    -- ^ Compensation scaling, only 'True' for functions that support it
  , scaleImplementation :: !CompensationScaling
  } deriving (Eq, Show)

-- | Whether a function supports coarse rotation
hasCoarseRotation :: CordicFunction -> Bool
hasCoarseRotation f = f `elem` [Rotate, Translate, SinCos, ArcTan]

-- | Whether a function supports compensation scaling
hasCompensation :: CordicFunction -> Bool
hasCompensation f = f `elem` [Rotate, Translate]

-- | Number of iterations the IP picks when it is set to 0, see /Iterations/ in
-- PG105.
autoIterations :: CordicFunction -> Bool -> Int -> Int
autoIterations f coarse_ wOut
  | coarse_ && hasCoarseRotation f = wOut - 2
  | otherwise = wOut

-- | Internal precision the IP picks when it is set to 0, see /Precision/ in
-- PG105: the output width plus the number of bits needed to count the
-- iterations.
autoPrecision :: Int -> Int -> Int
autoPrecision wOut nIter = wOut + ceilLog2 nIter

-- | Resolve the automatic parameters of a configuration
mkParams ::
  CordicFunction ->
  Config ->
  -- | Input width
  Int ->
  -- | Output width
  Int ->
  Params
mkParams f cfg wIn wOut = Params
  { function = f
  , inputWidth = wIn
  , outputWidth = wOut
  , nIterations = n
  , internalPrecision = maybe (autoPrecision wOut n) id (precision cfg)
  , coarse = coarseRotation cfg && hasCoarseRotation f
  , scaled = phaseFormat cfg == ScaledRadians
  , rounding = roundMode cfg
  , compensate = compensationScaling cfg /= NoScaleCompensation && hasCompensation f
  , scaleImplementation = compensationScaling cfg
  }
 where
  n = case iterations cfg of
    Just i | i > 0 -> i
    _ -> autoIterations f (coarseRotation cfg) wOut

-- | Latency of the IP in clock cycles, for the parallel architecture with
-- maximum pipelining and non-blocking AXI4-Stream interfaces. Measured in
-- Vivado's simulator for a wide range of configurations, on 7 series and
-- UltraScale+ devices:
--
-- * The functions take two cycles more than the number of iterations, plus
--   four for coarse rotation, plus the latency of the multiplier for
--   compensation scaling.
latency :: Params -> Int
latency Params{..} = nIterations + 2 + (if coarse then 4 else 0) + scaling
 where
  scaling
    | not compensate = 0
    | otherwise = case scaleImplementation of
        -- Constant coefficient multipliers: the input is split into chunks
        -- of 6 (LUT) or 9 (block RAM) bits, whose partial products are added
        -- by a pipelined adder tree.
        LutBased -> 1 + ceilLog2 (ceilDiv internalPrecision 6)
        BramBased -> 3 + ceilLog2 (ceilDiv internalPrecision 9)
        NoScaleCompensation -> 0
  ceilDiv a b = (a + b - 1) `div` b

-- | @ceil(log2 k)@ for positive @k@
ceilLog2 :: Int -> Int
ceilLog2 k = length (takeWhile (< k) (iterate (* 2) 1))

------------------------------------------------------------------------------
-- Fixed point helpers
------------------------------------------------------------------------------

-- | Multiply by @2^k@. For negative @k@, divide and round towards negative
-- infinity.
shiftTo :: Int -> Integer -> Integer
shiftTo k v
  | k >= 0 = v `shiftL` k
  | otherwise = v `shiftR` negate k

-- | Drop @k@ fractional bits from a number, rounding as the IP does.
roundOff :: RoundMode -> Int -> Integer -> Integer
roundOff _ 0 v = v
roundOff rnd k v
  | k < 0 = v `shiftL` negate k
  | otherwise = case rnd of
      Truncate -> t
      RoundPosInf -> (v + half) `shiftR` k
      RoundPosNegInf
        | v >= 0 -> (v + half) `shiftR` k
        | otherwise -> negate ((negate v + half) `shiftR` k)
      RoundNearestEven
        | r > half -> t + 1
        | r == half && (testBit t 0 /= (v < 0)) -> t + 1
        | otherwise -> t
 where
  half = 1 `shiftL` (k - 1)
  t = v `shiftR` k
  r = v - (t `shiftL` k)

-- | Rotate a vector by a multiple of 90 degrees: @rot90 k (x, y)@ rotates
-- @(x, y)@ by @k * 90@ degrees counterclockwise.
rot90 :: Int -> (Integer, Integer) -> (Integer, Integer)
rot90 k (x, y) = case k `mod` 4 of
  0 -> (x, y)
  1 -> (negate y, x)
  2 -> (negate x, negate y)
  _ -> (y, negate x)

-- | Quadrant of a vector: the multiple of 90 degrees @m@ such that rotating the
-- vector by @-m * 90@ degrees gives a vector with an angle between -45 and 45
-- degrees. Vectors on the diagonals go to the quadrant counterclockwise from
-- them, except for those in the first and fourth quadrant (positive X).
vectorQuadrant :: (Integer, Integer) -> Int
vectorQuadrant (x, y)
  | x >= 0 && abs x >= abs y = 0
  | x < 0 && abs x > abs y = 2
  | y > 0 = 1
  | otherwise = -1

------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------

-- | Integer square root: the largest @r@ with @r * r <= n@.
isqrt :: Integer -> Integer
isqrt n
  | n < 0 = error "isqrt: negative argument"
  | n < 2 = n
  | otherwise = go (1 `shiftL` ((bitLength n + 1) `div` 2))
 where
  go r =
    let r' = (r + n `div` r) `div` 2
    in if r' >= r then r else go r'
  bitLength m = length (takeWhile (> 0) (iterate (`shiftR` 1) m))

-- | Rational lower and upper bounds of a real number, with a requested
-- absolute accuracy of @2^-bits@.
type Bounds = Int -> (Rational, Rational)

-- | Bounds on @atan(1/m)@ for integer @m >= 2@, from its Taylor series.
atanRecipBounds :: Integer -> Bounds
atanRecipBounds m bits = (s - r, s + r)
 where
  -- Terms of the series: (-1)^k / ((2k+1) m^(2k+1))
  terms = [ (if even k then 1 else -1) % ((2 * k + 1) * m ^ (2 * k + 1)) | k <- [0 :: Integer ..] ]
  eps = 1 % (2 ^ (bits + 2))
  (used, rest) = span ((>= eps) . abs) terms
  s = sum used
  -- Alternating series with decreasing terms: the remainder is bounded by the
  -- first omitted term.
  r = case rest of
    (t : _) -> abs t
    [] -> 0

-- | Bounds on pi, using Machin's formula: @pi = 16 atan(1/5) - 4 atan(1/239)@.
piBounds :: Bounds
piBounds bits = (16 * lo5 - 4 * hi239, 16 * hi5 - 4 * lo239)
 where
  (lo5, hi5) = atanRecipBounds 5 (bits + 5)
  (lo239, hi239) = atanRecipBounds 239 (bits + 3)

-- | Round a real number given by its bounds to an integer, increasing the
-- accuracy until the bounds agree.
roundBounds :: (Rational -> Integer) -> Bounds -> Integer
roundBounds rnd bnds = go 64
 where
  go bits =
    let (lo, hi) = bnds bits
    in if rnd lo == rnd hi then rnd lo else go (2 * bits)

scaleBounds :: Int -> Bounds -> Bounds
scaleBounds k bnds bits =
  let (lo, hi) = bnds (bits + max 0 k)
  in (lo * 2 ^^ k, hi * 2 ^^ k)

-- | Divide bounds by bounds on a positive number
divBounds :: Bounds -> Bounds -> Bounds
divBounds num den bits =
  let (nLo, nHi) = num (bits + 4)
      (dLo, dHi) = den (bits + 4)
      cands = [a / b | a <- [nLo, nHi], b <- [dLo, dHi]]
  in (minimum cands, maximum cands)

ratFloor :: Rational -> Integer
ratFloor q = numerator q `div` denominator q

ratCeil :: Rational -> Integer
ratCeil q = negate (ratFloor (negate q))

ratRound :: Rational -> Integer
ratRound q = ratFloor (q + 1 % 2)

-- | @ceil(c * pi * 2^k)@, for a rational @c@
piCeil :: Rational -> Int -> Integer
piCeil c k = roundBounds ratCeil (scaleBounds k mulC)
 where
  mulC bits = let (lo, hi) = piBounds (bits + 8) in (lo * c, hi * c)

-- | @round(atan(2^-i) * 2^k)@, or @round(atan(2^-i) / pi * 2^k)@ for scaled
-- radians.
atanRounded :: Bool -> Int -> Int -> Integer
atanRounded scaledRadians i k = roundBounds ratRound (scaleBounds k val)
 where
  a = atanRecipBounds (2 ^ i)
  val | scaledRadians = divBounds a piBounds
      | otherwise = a

-- | Round @sqrt(num / den) * 2^k@ to the nearest integer, ties towards positive
-- infinity (ties cannot occur for the values we use).
roundSqrtRatio :: Integer -> Integer -> Int -> Integer
roundSqrtRatio num den k = (isqrt ((num * 4 ^ (k + 1)) `div` den) + 1) `div` 2

-- | Inverse of the circular CORDIC scale factor for @n@ iterations,
-- @prod_{i=1..n} 1 / sqrt(1 + 2^-2i)@, rounded to @k@ fractional bits.
inverseScaleFactor :: Int -> Int -> Integer
inverseScaleFactor n k = roundSqrtRatio num den k
 where
  num = product [4 ^ i | i <- [1 .. n]]
  den = product [4 ^ i + 1 | i <- [1 .. n]]

------------------------------------------------------------------------------
-- CORDIC
------------------------------------------------------------------------------

-- | Fractional bits of X and Y in the datapath
fracXY :: Params -> Int
fracXY p = internalPrecision p - 2

-- | Fractional bits of the phase in the datapath
fracPhase :: Params -> Int
fracPhase p = internalPrecision p - 3

-- | Circular rotation mode: rotate @(x, y)@ by @z@, driving @z@ to zero.
-- Returns the rotated vector, scaled by the CORDIC scale factor.
circularRotation :: [(Int, Integer)] -> Integer -> (Integer, Integer) -> (Integer, Integer)
circularRotation steps = go steps
 where
  go [] _ xy = xy
  go ((i, a) : rest) z (x, y)
    | z >= 0 = go rest (z - a) (x - (y `shiftR` i), y + (x `shiftR` i))
    | otherwise = go rest (z + a) (x + (y `shiftR` i), y - (x `shiftR` i))

-- | Circular vectoring mode: rotate @(x, y)@ to the X axis, accumulating the
-- angle. Returns the (scaled) magnitude and the angle.
circularVectoring :: [(Int, Integer)] -> (Integer, Integer) -> (Integer, Integer)
circularVectoring steps = go steps 0
 where
  go [] z (x, _) = (x, z)
  go ((i, a) : rest) z (x, y)
    | y < 0 = go rest (z - a) (x - (y `shiftR` i), y + (x `shiftR` i))
    | otherwise = go rest (z + a) (x + (y `shiftR` i), y - (x `shiftR` i))

-- | Iterations of the circular CORDIC: shifts and arctangents
circularSteps :: Params -> [(Int, Integer)]
circularSteps p =
  [ (i, atanRounded (scaled p) i (fracPhase p)) | i <- [1 .. nIterations p] ]

-- | Multiple of pi\/4 in the phase unit, rounded up to the phase precision of
-- the datapath
quarterPi :: Params -> Rational -> Integer
quarterPi p c
  | scaled p = ratCeil (c * 2 ^^ f / 4)
  | otherwise = piCeil (c / 4) f
 where
  f = fracPhase p

-- | Coarse rotation of a phase: the quadrant @k@ and the phase minus
-- @k * pi/2@.
coarsePhase :: Params -> Integer -> (Int, Integer)
coarsePhase p = \z ->
  let
    k | z >= c3 = 2
      | z >= c1 = 1
      | z < negate c3 = -2
      | z < negate c1 = -1
      | otherwise = 0
    offset = case k of
      2 -> pi1
      1 -> pi2
      -1 -> negate pi2
      -2 -> negate pi1
      _ -> 0
  in (k, z - offset)
 where
  -- Quadrant boundaries: pi/4 and 3pi/4
  c1 = quarterPi p 1
  c3 = quarterPi p 3
  -- pi/2 and pi
  pi2 = quarterPi p 2
  pi1 = quarterPi p 4

-- | Constant added to the angle for a vector quadrant, see 'vectorQuadrant'.
-- For the third quadrant, the sign is chosen to keep the angle in range.
quadrantAngle :: Params -> Int -> Integer -> Integer
quadrantAngle p = \m z -> case m of
  0 -> 0
  1 -> pi2
  -1 -> negate pi2
  _ | z >= 0 -> negate pi1
    | otherwise -> pi1
 where
  pi2 = quarterPi p 2
  pi1 = quarterPi p 4

-- | Compensate the CORDIC scale factor, if enabled
compensateScale :: Params -> Integer -> Integer
compensateScale p
  | compensate p = \v -> (v * k) `shiftR` f
  | otherwise = id
 where
  f = fracXY p
  k = inverseScaleFactor (nIterations p) f

-- | Round X or Y from the datapath to the output
roundXY :: Params -> Integer -> Integer
roundXY p = roundOff (rounding p) (fracXY p - (outputWidth p - 2))

-- | Round a phase from the datapath to the output
roundPhase :: Params -> Integer -> Integer
roundPhase p = roundOff (rounding p) (fracPhase p - (outputWidth p - 3))

-- | Convert X or Y from the input to the datapath
inXY :: Params -> Integer -> Integer
inXY p = shiftTo (fracXY p - (inputWidth p - 2))

-- | Convert a phase from the input to the datapath
inPhase :: Params -> Integer -> Integer
inPhase p = shiftTo (fracPhase p - (inputWidth p - 3))

-- | Model of the vector rotation. Inputs and outputs are the raw integer
-- values of the fixed point numbers: X and Y with @inputWidth - 2@ fractional
-- bits, the phase with @inputWidth - 3@ fractional bits, the results with
-- @outputWidth - 2@ fractional bits.
modelRotate :: Params -> (Integer, Integer) -> Integer -> (Integer, Integer)
modelRotate p = \(xIn, yIn) phaseIn ->
  let
    xy = (inXY p xIn, inXY p yIn)
    z = inPhase p phaseIn
    m | coarse p = vectorQuadrant xy
      | otherwise = 0
    (k, z') | coarse p = coarse' z
            | otherwise = (0, z)
    rotated = circularRotation steps z' (rot90 (negate m) xy)
    (x, y) = rot90 (m + k) rotated
  in (out x, out y)
 where
  steps = circularSteps p
  coarse' = coarsePhase p
  out = roundXY p . compensateScale p

-- | Model of the vector translation, see 'modelRotate'. Returns the magnitude
-- (with @outputWidth - 2@ fractional bits) and the phase (with
-- @outputWidth - 3@ fractional bits).
modelTranslate :: Params -> (Integer, Integer) -> (Integer, Integer)
modelTranslate p = \(xIn, yIn) ->
  let
    xy = (inXY p xIn, inXY p yIn)
    m | coarse p = vectorQuadrant xy
      | otherwise = 0
    (x, z) = circularVectoring steps (rot90 (negate m) xy)
    z' = z + quadrant m z
  in (roundXY p (scale x), roundPhase p z')
 where
  steps = circularSteps p
  quadrant = quadrantAngle p
  scale = compensateScale p

-- | Model of the arctangent, see 'modelTranslate'
modelArcTan :: Params -> (Integer, Integer) -> Integer
modelArcTan p = snd . modelTranslate p

-- | Model of sin and cos, see 'modelRotate'. Returns @(cos, sin)@.
modelSinCos :: Params -> Integer -> (Integer, Integer)
modelSinCos p = \phaseIn ->
  let
    z = inPhase p phaseIn
    (k, z') | coarse p = coarse' z
            | otherwise = (0, z)
    (x, y) = rot90 k (circularRotation steps z' (x0, 0))
  in (roundXY p x, roundXY p y)
 where
  steps = circularSteps p
  coarse' = coarsePhase p
  x0 = inverseScaleFactor (nIterations p) (fracXY p)
