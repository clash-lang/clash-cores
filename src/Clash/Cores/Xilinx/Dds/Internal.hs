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
angleWidth Params{..} = case parts of
  SinCosLutOnly -> phaseWidth
  _ -> min phaseWidth outputWidth

-- | Whether the table entries are taken half an address step after the
-- address. See the module documentation.
halfStepOffset :: Params -> Bool
halfStepOffset Params{..} = parts /= SinCosLutOnly && outputWidth <= phaseWidth

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
modelSinCos p@Params{..} phase = (neg (negativeCosine config) c, neg (negativeSine config) s)
 where
  aw = angleWidth p
  k = (phase .&. ((1 `shiftL` phaseWidth) - 1)) `shiftR` (phaseWidth - aw)
  n = 1 `shiftL` aw :: Integer
  s = tableEntry p k
  c = tableEntry p (k + n `shiftR` 2)
  neg b v = if b then negate v else v

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

-- | Minimum latency the IP supports for a configuration, in clock cycles. The
-- IP accepts a latency of 0 for a SIN\/COS LUT in distributed memory, but it
-- then has no clock input; that is not supported here.
--
-- PG141 (/Latency Options/) lists the sources of latency; we found these
-- minimums by letting Vivado validate a wide range of configurations: the
-- phase generator takes a cycle, and so does a lookup table in block RAM.
minLatency :: Params -> Int
minLatency p = case parts p of
  PhaseGeneratorOnly -> 1
  SinCosLutOnly -> 1
  PhaseGeneratorAndSinCosLut -> 1 + fromEnum (blockRom p)

-- | Check that the IP supports the widths of a configuration (see /Hardware
-- Parameters/ in PG141, and the ranges Vivado enforces). Returns an error
-- message if it does not.
checkWidths :: Params -> Maybe String
checkWidths Params{..}
  | phaseWidth < 3 || phaseWidth > maxPhase =
      Just ("the phase width must be between 3 and " <> show maxPhase <> " bits")
  | parts /= PhaseGeneratorOnly && (outputWidth < 3 || outputWidth > 26) =
      Just "the output width must be between 3 and 26 bits"
  | parts == PhaseGeneratorAndSinCosLut && min phaseWidth outputWidth > 16 =
      Just "the phase width or the output width must be at most 16 bits"
  | otherwise = Nothing
 where
  maxPhase = if parts == SinCosLutOnly then 16 else 48

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
