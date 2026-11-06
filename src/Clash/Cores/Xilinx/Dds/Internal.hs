{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation model and configuration of the Xilinx DDS Compiler LogiCORE IP v6.0.
See "Clash.Cores.Xilinx.Dds" for the user facing API.

The simulation model follows the description of the IP in
[PG141](https://docs.amd.com/v/u/en-US/pg141-dds-compiler). It operates on
'Integer's holding the raw bits of phases and samples:

* The phase generator accumulates the phase increment (PINC) modulo
  @2^phaseWidth@. The accumulated phase of a sample includes that sample's
  increment. A resynchronization sets the accumulated phase to the increment
  of that sample. The phase offset (POFF) is added to the accumulated phase,
  again modulo @2^phaseWidth@.

* The SIN\/COS lookup table is addressed by the most significant bits of the
  phase: the phase is truncated to the table's address width. The table holds
  @round(A * sin(2 pi (k + o) \/ 2^a))@ and @round(A * cos(2 pi (k + o) \/ 2^a))@
  for addresses @k@ of @a@ bits. The amplitude @A@ is @2^(w-1) - 2@ for a full
  range output of @w@ bits, and @2^(w-2)@ for a unit circle output. PG141
  defines both amplitudes and the table (/Theory of Operation/, /Amplitude
  Mode/).

* The address width @a@ and the offset @o@ are not documented; they were found
  by comparing with Xilinx's bit-accurate C model. The SIN\/COS LUT only
  configuration uses the full phase as address, and @o = 0@. The complete DDS
  uses @a = min(phaseWidth, outputWidth)@, and @o = 1\/2@ (half an address
  step, which centers the phase truncation error) if the output is no wider
  than the phase, else @o = 0@.

* One table entry rounds differently from the formula: for 26-bit full range
  outputs and a table of @2^16@ entries, @A * sin(2 pi 2607 \/ 2^16)@ is
  @8299645.4999997...@, which the IP and the C model round up. The model has
  this as an exception, see 'tableQuirk'.

* Negating the sine or cosine negates the table value.

Taylor series correction (PG141, /Taylor Series Corrected DDS/) uses a table
of 2048 entries and corrects its values with the rest of the phase. PG141
describes the principle, not the arithmetic. The structure below comes from
the block names and widths in Vivado's utilization report of the synthesized
IP (a multiplier of the phase error by pi with an 18-bit result, two
multiply-adders for the first order term, and for second order a block RAM
for the square of the phase error and two more multiply-adders); the
rounding was then pinned down by comparing with the C model, including on
inputs chosen to hit rounding ties:

* The table holds the sine and cosine at the centres of its entries,
  @round(A * K * sin(2 pi (j + 1\/2) \/ 2048))@, in the output width. For first
  order correction (outputs of up to 21 bits) @K = 1 - (pi \/ 2048)^2 \/ 4@,
  which halves the maximum error of a linear approximation; for second order
  correction (22 bits and more) @K = 1@. Negated outputs negate these values.

* The phase error is the rest of the phase relative to the centre of the
  entry, a signed fraction @x@ of an entry, of which the IP uses 17 bits
  (rounded down). It is multiplied by @floor(pi * 2^15)@, and the product is
  rounded down to 14 fractional bits: @d@, the phase error in radians times
  1024.

* The multipliers take the table values as 18-bit operands: their 18 most
  significant bits for outputs of more than 18 bits, and for narrower ones the
  value with a one appended after its least significant bit.

* First order: @sin(phase) = S + C' * d@ (and @cos(phase) = C - S' * d@), where
  @S'@ and @C'@ are the operands. The product is added to the table value at
  the full precision of the product, and the sum is rounded to 6 bits less
  (ties up).

* Second order: the square of the phase error, @(pi x)^2 \/ 2@, comes from a
  table of 1024 entries addressed by the 10 most significant bits of @x@, and
  is rounded to 10 fractional bits. Its product with the other operand is
  rounded to the precision of the first order sum (ties towards negative
  infinity) and subtracted.

* Finally the sum is rounded to the output width (ties up).

These details were validated against Xilinx's bit-accurate C model of the IP,
see @test\/Test\/Cores\/Xilinx\/Dds.hs@, and against the IP itself in Vivado's
simulator.

__NB__: None of the functions in this module are synthesizable.
-}

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Dds.Internal
  ( -- * Configuration
    Config(..)
  , defConfig
  , NoiseShaping(..)
  , AmplitudeMode(..)
  , MemoryType(..)
  , PartsPresent(..)
  , PhaseSource(..)
    -- * Resolved parameters
  , Params(..)
  , mkParams
  , angleWidth
  , halfStepOffset
  , amplitude
  , checkWidths
  , blockRom
  , minLatency
  , maxLatency
    -- * Models
  , accumulate
  , tableEntry
  , modelSinCos
    -- * Wizard options
  , partsPresentToTcl
  , noiseShapingToTcl
  , amplitudeModeToTcl
  , memoryTypeToTcl
  , phaseSourceToTcl
    -- * Constants (exported for testing)
  , tableQuirk
  ) where

import Prelude

import Data.Bits (shiftL, shiftR, (.&.))
import Language.Haskell.TH.Syntax (Lift)

-- | Which parts of the DDS the IP implements, see /Configuration Options/ in
-- PG141.
data PartsPresent
  = PhaseGeneratorAndSinCosLut
    -- ^ A complete DDS: a phase generator followed by a SIN\/COS lookup table
  | PhaseGeneratorOnly
    -- ^ Only the phase generator: an accumulator and a phase offset adder
  | SinCosLutOnly
    -- ^ Only the SIN\/COS lookup table, which converts a phase to its sine and
    -- cosine
  deriving (Eq, Show, Lift)

-- | How the phase increment or the phase offset is set, see /Phase Increment
-- Programmability/ and /Phase Offset Programmability/ in PG141.
data PhaseSource
  = NoSource
    -- ^ Only for the phase offset: there is no phase offset
  | FixedSource
    -- ^ Fixed when the IP is generated
  | ProgrammableSource
    -- ^ Set through the CONFIG channel
  | StreamingSource
    -- ^ Taken from the input PHASE channel, one value per sample
  deriving (Eq, Show, Lift)

-- | Noise shaping, see /Noise Shaping/ in PG141.
data NoiseShaping
  = NoNoiseShaping
    -- ^ Phase truncation: the most significant bits of the phase address the
    -- lookup table.
  | TaylorSeriesCorrected
    -- ^ Taylor series correction: the lookup table has 2048 entries, and the
    -- IP corrects its values with the rest of the phase. Outputs of up to 21
    -- bits use a first order (linear) correction, wider outputs a second order
    -- one. Needs a phase width and an output width of at least 12 bits. See
    -- "Clash.Cores.Xilinx.Dds.Internal" for the details of the model.
  deriving (Eq, Show, Lift)

-- | Amplitude of the outputs, see /Amplitude Mode/ in PG141.
data AmplitudeMode
  = FullRange
    -- ^ The largest amplitude the output width allows: @1 - 2^-(w-2)@ for an
    -- output of @w@ bits
  | UnitCircle
    -- ^ An amplitude of exactly @1\/2@
  deriving (Eq, Show, Lift)

-- | Implementation of the lookup table, see /Memory Type/ in PG141. This does
-- not affect the results, but it does affect the minimum latency: block RAM
-- needs an extra cycle.
data MemoryType
  = AutoMemory
    -- ^ Let the IP decide
  | DistributedRom
    -- ^ Distributed memory (LUTs)
  | BlockRom
    -- ^ Block RAM
  deriving (Eq, Show, Lift)

-- | Customization of the Xilinx DDS Compiler IP. Options that do not apply to
-- a configuration (for instance the output options of a phase generator) are
-- ignored.
data Config = Config
  { noiseShaping :: !NoiseShaping
    -- ^ Noise shaping
  , amplitudeMode :: !AmplitudeMode
    -- ^ Amplitude of the sine and cosine
  , negativeSine :: !Bool
    -- ^ Negate the sine output
  , negativeCosine :: !Bool
    -- ^ Negate the cosine output
  , memoryType :: !MemoryType
    -- ^ Implementation of the lookup table
  } deriving (Eq, Show, Lift)

-- | Default customization options, matching the defaults of the IP:
--
-- * No noise shaping ('NoNoiseShaping')
-- * Full range amplitude ('FullRange')
-- * Sine and cosine not negated
-- * Lookup table memory type chosen by the IP ('AutoMemory')
defConfig :: Config
defConfig = Config
  { noiseShaping = NoNoiseShaping
  , amplitudeMode = FullRange
  , negativeSine = False
  , negativeCosine = False
  , memoryType = AutoMemory
  }

-- | A configuration of the IP with its widths
data Params = Params
  { parts :: !PartsPresent
  , phaseWidth :: !Int
    -- ^ Width of the phase accumulator, the phase increment and offset, and
    -- the phase input of the lookup table
  , outputWidth :: !Int
    -- ^ Width of the sine and cosine outputs
  , incrementSource :: !PhaseSource
  , offsetSource :: !PhaseSource
  , config :: !Config
  } deriving (Eq, Show)

-- | Bundle a configuration of the IP
mkParams ::
  PartsPresent ->
  -- | Phase increment
  PhaseSource ->
  -- | Phase offset
  PhaseSource ->
  Config ->
  -- | Phase width
  Int ->
  -- | Output width
  Int ->
  Params
mkParams p inc off cfg pw ow = Params
  { parts = p
  , phaseWidth = pw
  , outputWidth = ow
  , incrementSource = if p == SinCosLutOnly then FixedSource else inc
  , offsetSource = if p == SinCosLutOnly then NoSource else off
  , config = cfg
  }

-- | Width of the address of the lookup table: the number of most significant
-- bits of the phase that select a table entry.
angleWidth :: Params -> Int
angleWidth Params{..}
  | taylor config = 11
  | otherwise = case parts of
  SinCosLutOnly -> phaseWidth
  _ -> min phaseWidth outputWidth

-- | Whether the table entries are taken half an address step after the
-- address. See the module documentation.
halfStepOffset :: Params -> Bool
halfStepOffset Params{..} =
  not (taylor config) && parts /= SinCosLutOnly && outputWidth <= phaseWidth

-- | Whether a configuration uses Taylor series correction
taylor :: Config -> Bool
taylor cfg = noiseShaping cfg == TaylorSeriesCorrected

-- | Amplitude of the sine and cosine, in units of the least significant bit of
-- the output.
amplitude :: Params -> Integer
amplitude Params{..} = case amplitudeMode config of
  FullRange -> (1 `shiftL` (outputWidth - 1)) - 2
  UnitCircle -> 1 `shiftL` (outputWidth - 2)

-- | Table entries that do not round as the formula says: the output width,
-- the address width, the address in the first quadrant, and the correct value
-- of the sine at that address. It is the cosine at the address that is a
-- quarter of a circle further.
--
-- For the one entry here, the exact value is @8299645.4999997...@. Xilinx's C
-- model rounds it up, and so does the IP.
tableQuirk :: [(Int, Int, Integer, Integer)]
tableQuirk = [(26, 16, 2607, 8299646)]

-- | Entry of the sine table: the sine at an address, as a signed integer, for
-- amplitude @A@ and an address width @a@: @round(A * sin(2 pi (k + o) \/ 2^a))@.
tableEntry :: Params -> Integer -> Integer
tableEntry p k
  | quadrant == 0 = quarter r
  | quadrant == 1 = quarter (q - r)
  | quadrant == 2 = negate (quarter r)
  | otherwise = negate (quarter (q - r))
 where
  aw = angleWidth p
  n = 1 `shiftL` aw :: Integer
  q = n `shiftR` 2
  -- With the half step offset, the entries at k and q - 1 - k of the first
  -- quadrant mirror each other, so mirror around q - 1/2 instead.
  half = halfStepOffset p
  kk = k .&. (n - 1)
  quadrant
    | aw < 2 = 0
    | otherwise = kk `shiftR` (aw - 2)
  r0 = kk .&. (q - 1)
  r | half && odd quadrant = r0 + 1
    | otherwise = r0
  -- The sine of address @j + o@ (for @0 <= j <= q@), computed directly from
  -- the formula
  quarter j
    | aw < 2 = direct kk
    | otherwise = case lookup j quirks of
        Just v -> v
        Nothing -> direct j
  quirks =
    [ (k0, v) | (w, a, k0, v) <- tableQuirk, w == outputWidth p, a == aw, not half
              , amplitudeMode (config p) == FullRange ]
  direct j =
    let x = fromInteger (amplitude p) * sin (2 * pi * (fromInteger j + off) / fromInteger n) :: Double
    in floor (x + 0.5)
  off = if half then 0.5 else 0

-- | The sine and cosine of a phase (unsigned, @phaseWidth@ bits), as signed
-- integers. Returns @(cosine, sine)@, negated if configured.
modelSinCos :: Params -> Integer -> (Integer, Integer)
modelSinCos p@Params{..} phase
  | taylor config = let (s, c) = taylorSinCos p ph in (c, s)
  | otherwise = (neg (negativeCosine config) c0, neg (negativeSine config) s0)
 where
  aw = angleWidth p
  ph = phase .&. ((1 `shiftL` phaseWidth) - 1)
  k = ph `shiftR` (phaseWidth - aw)
  n = 1 `shiftL` aw :: Integer
  s0 = tableEntry p k
  c0 = tableEntry p (k + n `shiftR` 2)
  neg b v = if b then negate v else v

-- | Sine and cosine with Taylor series correction, see the module
-- documentation. Takes a phase of @phaseWidth@ bits, returns @(sine, cosine)@.
-- Negation is included: the IP negates the table values (and the multiplier
-- operands it derives from them), and corrects the signs of the products,
-- which does not give the same rounding as negating the result.
taylorSinCos :: Params -> Integer -> (Integer, Integer)
taylorSinCos p@Params{..} phase = (sinOut, cosOut)
 where
  sign b = if b then -1 else 1 :: Integer
  sgnS = sign (negativeSine config)
  sgnC = sign (negativeCosine config)
  -- The tables as the IP stores them
  sq' = sgnS * sq
  cq' = sgnC * cq
  -- @sgnS * sin = sq' + sgnS * sgnC * cq' * d - sq' * d^2 / 2@, and likewise
  -- for the cosine
  sinOut = stage sq' (sgnS * sgnC * operand cq') (operand sq')
  cosOut = stage cq' (negate (sgnS * sgnC) * operand sq') (operand cq')
  e = phaseWidth - 11
  j = phase `shiftR` e
  -- The phase error relative to the centre of the table entry, as a signed
  -- fraction of an entry with 17 bits (rounded down)
  xi = (phase .&. ((1 `shiftL` e) - 1)) - (1 `shiftL` (e - 1))
  x17 = if e <= 17 then xi `shiftL` (17 - e) else xi `shiftR` (e - 17)
  -- The phase error in radians (times 1024), rounded down to 14 fractional
  -- bits: the product with pi rounded down to 15 fractional bits
  dq = (x17 * 102943) `shiftR` 18
  second = outputWidth >= 22
  (sq, cq) = taylorTable p j
  -- The table values as 18-bit multiplier operands: the 18 most significant
  -- bits, or the value with a one appended after its least significant bit
  kk = 18 - outputWidth
  operand t
    | kk > 0 = (t `shiftL` kk) + (1 `shiftL` (kk - 1))
    | otherwise = t `shiftR` negate kk
  -- Half of the square of the phase error in radians (times 1024^2), for
  -- 10 bits of the phase error, rounded to 10 fractional bits
  xb = fromInteger (x17 `shiftR` 7) / 1024 :: Double
  q2 = floor ((xb * pi) * (xb * pi) / 2 * 1024 + 0.5) :: Integer
  s1 = 24 + kk
  stage t a1 a2 =
    let
      -- First order: drop 6 bits of the sum, rounding
      p1 = ((t `shiftL` s1) + a1 * dq + 32) `shiftR` 6
      -- Second order: subtract the product with the square of the phase
      -- error, rounded to the same precision (ties towards negative infinity)
      p2 | second = p1 + ((negate (a2 * q2) + 2048) `shiftR` 12)
         | otherwise = p1
    in (p2 + (1 `shiftL` (s1 - 7))) `shiftR` (s1 - 6)

-- | Sine and cosine at the centre of an entry of the Taylor series corrected
-- table: @round(A * K * sin(2 pi (j + 1/2) / 2048))@ and the cosine. For first
-- order correction, the IP scales the table by @K = 1 - (pi / 2048)^2 / 4@,
-- which halves the maximum error of the linear approximation.
taylorTable :: Params -> Integer -> (Integer, Integer)
taylorTable p@Params{..} j = (rnd (sin phc), rnd (cos phc))
 where
  phc = 2 * pi * (fromInteger j + 0.5) / 2048 :: Double
  k | outputWidth >= 22 = 1
    | otherwise = 1 - (pi / 2048) ^ (2 :: Int) / 4
  a = fromInteger (amplitude p) * k
  rnd x = floor (a * x + 0.5)

-- | One step of the phase generator: given the accumulated phase, the phase
-- increment, the phase offset and whether to resynchronize, return the new
-- accumulated phase and the output phase.
accumulate ::
  -- | Phase width
  Int ->
  -- | Accumulated phase
  Integer ->
  -- | Phase increment
  Integer ->
  -- | Phase offset
  Integer ->
  -- | Resynchronize
  Bool ->
  (Integer, Integer)
accumulate pw acc pinc poff resync = (acc', (acc' + poff) .&. mask)
 where
  mask = (1 `shiftL` pw) - 1
  acc' = (if resync then pinc else acc + pinc) .&. mask

-- | Whether the lookup table is implemented in block RAM. With 'AutoMemory',
-- the IP uses distributed memory for tables with an address of up to 5 bits
-- (see /Memory Type/ in PG141).
blockRom :: Params -> Bool
blockRom p = case memoryType (config p) of
  BlockRom -> True
  DistributedRom -> False
  AutoMemory -> angleWidth p > 5

-- | Whether a configuration uses second order Taylor series correction, which
-- needs a cycle more
secondOrder :: Params -> Bool
secondOrder p = taylor (config p) && outputWidth p >= 22

-- | Minimum latency the IP supports for a configuration, in clock cycles. The
-- IP accepts a latency of 0 for a SIN\/COS LUT in distributed memory, but it
-- then has no clock input; that is not supported here.
--
-- PG141 (/Latency Options/) lists the sources of latency; we found these
-- minimums by letting Vivado validate a wide range of configurations: the
-- phase generator takes a cycle, and so do a lookup table in block RAM and
-- second order Taylor series correction.
minLatency :: Params -> Int
minLatency p = case parts p of
  PhaseGeneratorOnly -> 1
  SinCosLutOnly -> max 1 (fromEnum (blockRom p) + fromEnum (secondOrder p))
  PhaseGeneratorAndSinCosLut -> 1 + fromEnum (blockRom p) + fromEnum (secondOrder p)

-- | Check that the IP supports the widths of a configuration (see /Hardware
-- Parameters/ in PG141, and the ranges Vivado enforces). Returns an error
-- message if it does not.
checkWidths :: Params -> Maybe String
checkWidths Params{..}
  | phaseWidth < 3 || phaseWidth > maxPhase =
      Just ("the phase width must be between 3 and " <> show maxPhase <> " bits")
  | parts /= PhaseGeneratorOnly && (outputWidth < 3 || outputWidth > 26) =
      Just "the output width must be between 3 and 26 bits"
  | taylorLut && (phaseWidth < 12 || outputWidth < 12) =
      Just "Taylor series correction needs a phase width and an output width of at least 12 bits"
  | parts == PhaseGeneratorAndSinCosLut && not taylorLut && min phaseWidth outputWidth > 16 =
      Just "the phase width or the output width must be at most 16 bits"
  | otherwise = Nothing
 where
  maxPhase = if parts == SinCosLutOnly then 16 else 48
  taylorLut = parts /= PhaseGeneratorOnly && taylor config

-- | Maximum latency the IP supports, in clock cycles
maxLatency :: Int
maxLatency = 21

partsPresentToTcl :: PartsPresent -> String
partsPresentToTcl = \case
  PhaseGeneratorAndSinCosLut -> "Phase_Generator_and_SIN_COS_LUT"
  PhaseGeneratorOnly -> "Phase_Generator_only"
  SinCosLutOnly -> "SIN_COS_LUT_only"

noiseShapingToTcl :: NoiseShaping -> String
noiseShapingToTcl = \case
  NoNoiseShaping -> "None"
  TaylorSeriesCorrected -> "Taylor_Series_Corrected"

amplitudeModeToTcl :: AmplitudeMode -> String
amplitudeModeToTcl = \case
  FullRange -> "Full_Range"
  UnitCircle -> "Unit_Circle"

memoryTypeToTcl :: MemoryType -> String
memoryTypeToTcl = \case
  AutoMemory -> "Auto"
  DistributedRom -> "Distributed_ROM"
  BlockRom -> "Block_ROM"

phaseSourceToTcl :: PhaseSource -> String
phaseSourceToTcl = \case
  NoSource -> "None"
  FixedSource -> "Fixed"
  ProgrammableSource -> "Programmable"
  StreamingSource -> "Streaming"
