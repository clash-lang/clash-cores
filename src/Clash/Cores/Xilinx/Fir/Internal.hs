{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation model, configuration and IP instantiation for
"Clash.Cores.Xilinx.Fir". See there for the user facing API.

The arithmetic of a FIR filter is exact integer arithmetic, so the model of the
numbers is short: the sum of products, rounded to the output width. Most of
this module models the structure the IP picks for a set of coefficients, which
determines its latency and full precision output width. These rules were found
with Vivado, from the latency and widths it reports for thousands of
configurations of the IP on 7 series and UltraScale+ devices, and checked
against the IP in Vivado's simulator:

* The IP infers the sign of the coefficients from their values: they are
  unsigned if none of them is negative, whatever their declared type.

* The full precision output width is the data width plus
  @ceil (log2 (sum |c_i|))@, plus one for unsigned data with signed
  coefficients.

* The IP infers the structure of the coefficients ('inferStructure'):
  symmetric (or negative symmetric), half-band, Hilbert, or none. A symmetric
  structure uses a pre-adder to share a multiplication between two taps. A
  half-band or Hilbert structure also skips the taps that are zero. A Hilbert
  structure gives the IP a second output, which is not supported.

* The latency is the number of multiply-add units ('multiplyAdds'), plus five,
  plus one for a pre-adder, plus one for the centre tap of a half-band filter
  (except on UltraScale devices when the centre tap is a positive power of
  two), plus 2 or 6 if a multiplication needs 2 or 4 DSP slices
  ('dspsPerMultiply'), plus one for symmetric rounding, plus 4 for every extra
  column of DSP slices. The accumulator of a DSP slice has 48 bits: a wider
  full precision result needs two DSP slices per multiplication.

* For a sample period of more than one clock cycle, each multiply-add unit
  handles several coefficients, one per clock cycle, and the latency also
  includes that number of cycles. See 'latency' and t'Params' for the details.

__NB__: None of the functions in this module are synthesizable.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Fir.Internal
  ( -- * Configuration
    Config(..)
  , defConfig
  , Rounding(..)
  , CoefficientStructure(..)
  , DspSlice(..)
    -- * Numbers
  , FirNumber(..)
  , FirOutput
  , ByteWidth
    -- * Structure of the IP
  , Structure(..)
  , Params(..)
  , mkParams
  , inferStructure
  , multiplyAdds
  , dspsPerMultiply
  , fullPrecisionWidth
  , latency
  , checkParams
    -- * Simulation model
  , roundOutput
  , firOutputModel
  , firModel
  , firValidModel
    -- * Instantiating the IP
  , firInst
  , firOptions
  ) where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Data.Kind (Type)
import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

-- | Customization of the FIR Compiler IP. See 'defConfig' for the defaults.
data Config = Config
  { rounding :: !Rounding
    -- ^ How the IP rounds its output to the output width
  , coefficientStructure :: !CoefficientStructure
    -- ^ Whether the IP may exploit the structure of the coefficients, which
    -- affects its resource use and latency, but not its results
  , dspSlice :: !DspSlice
    -- ^ The DSP slices of the target device. They determine how many DSP
    -- slices a multiplication of wide operands needs, and thereby the latency.
  , dspColumnLength :: !(Maybe Int)
    -- ^ The number of DSP slices in a column of the target device, if that is
    -- fewer than the number of multiply-add units of the filter: the number of
    -- coefficients, or half that for symmetric coefficients, or a quarter for
    -- a half-band filter. The IP then spreads the filter over several columns,
    -- which adds 4 cycles of latency per extra column. 'Nothing' means that the filter fits in a
    -- single column. A column has 20 DSP slices per clock region on 7 series
    -- devices and 24 on UltraScale(+) devices; for example, 80 on a @xc7k70t@
    -- and 96 on a @xcku3p@.
  } deriving (Eq, Show)

-- | Default customization:
--
-- * 'FullPrecision' output
-- * 'Inferred' coefficient structure
-- * 'Dsp48E2' (UltraScale and UltraScale+) DSP slices
-- * The filter fits in a single column of DSP slices
defConfig :: Config
defConfig = Config
  { rounding = FullPrecision
  , coefficientStructure = Inferred
  , dspSlice = Dsp48E2
  , dspColumnLength = Nothing
  }

-- | How the IP rounds the full precision result to the output width, by
-- discarding least significant bits. The full precision width is given by
-- 'Clash.Cores.Xilinx.Fir.Explicit.fullPrecisionWidth'. See [PG149](https://docs.amd.com/r/en-US/pg149-fir-compiler),
-- /Output Rounding/. The descriptions below say what happens to a result that
-- lies exactly halfway between two output values (a tie).
data Rounding
  = FullPrecision
    -- ^ No rounding: the output width must equal the full precision width
  | TruncateLsbs
    -- ^ Discard the least significant bits: round towards negative infinity
  | NonSymmetricRoundingDown
    -- ^ Round to the nearest value, ties towards negative infinity
  | NonSymmetricRoundingUp
    -- ^ Round to the nearest value, ties towards positive infinity
  | SymmetricRoundingToZero
    -- ^ Round to the nearest value, ties towards zero. Adds a cycle of latency.
  | SymmetricRoundingToInfinity
    -- ^ Round to the nearest value, ties away from zero. Adds a cycle of
    -- latency.
  | ConvergentRoundingToEven
    -- ^ Round to the nearest value, ties to the even value
  | ConvergentRoundingToOdd
    -- ^ Round to the nearest value, ties to the odd value
  deriving (Eq, Show, Enum, Bounded)

roundingToTcl :: Rounding -> String
roundingToTcl = \case
  FullPrecision -> "Full_Precision"
  TruncateLsbs -> "Truncate_LSBs"
  NonSymmetricRoundingDown -> "Non_Symmetric_Rounding_Down"
  NonSymmetricRoundingUp -> "Non_Symmetric_Rounding_Up"
  SymmetricRoundingToZero -> "Symmetric_Rounding_to_Zero"
  SymmetricRoundingToInfinity -> "Symmetric_Rounding_to_Infinity"
  ConvergentRoundingToEven -> "Convergent_Rounding_to_Even"
  ConvergentRoundingToOdd -> "Convergent_Rounding_to_Odd"

-- | Whether the IP may exploit the structure of the coefficients. This
-- affects the number of DSP slices and the latency, not the results.
data CoefficientStructure
  = Inferred
    -- ^ The IP inspects the coefficients and exploits symmetry, negative
    -- symmetry and the zeros of half-band filters. A half-band filter has
    -- an odd number of symmetric coefficients @n@, with @n mod 4 == 3@ and
    -- @n >= 7@, of which every second one, counting from the centre tap, is
    -- zero (the centre tap itself may have any value).
    --
    -- The IP turns coefficients that form a Hilbert transformer into a
    -- Hilbert filter with two outputs, which is not supported: simulation
    -- reports an error. Use 'NegativeSymmetric' (or 'NonSymmetric') for
    -- them.
  | NonSymmetric
    -- ^ One multiply-add unit per coefficient
  | Symmetric
    -- ^ Symmetric coefficients, @c_i = c_(n-1-i)@: one multiply-add unit per
    -- pair of coefficients. Unlike 'Inferred', this does not exploit the zeros
    -- of a half-band filter.
  | NegativeSymmetric
    -- ^ Negative symmetric coefficients, @c_i = -c_(n-1-i)@ (except for the
    -- centre tap of an odd number of coefficients): one multiply-add unit per
    -- pair of coefficients.
  deriving (Eq, Show, Enum, Bounded)

coefficientStructureToTcl :: CoefficientStructure -> String
coefficientStructureToTcl = \case
  Inferred -> "Inferred"
  NonSymmetric -> "Non_Symmetric"
  Symmetric -> "Symmetric"
  NegativeSymmetric -> "Negative_Symmetric"

-- | The DSP slices of the target device. Their multipliers take a 25-bit
-- ('Dsp48E1') or 27-bit ('Dsp48E2') and an 18-bit operand. Wider
-- multiplications need two or four DSP slices, which adds latency.
data DspSlice
  = Dsp48E1
    -- ^ 7 series devices
  | Dsp48E2
    -- ^ UltraScale and UltraScale+ devices
  deriving (Eq, Show, Enum, Bounded)

------------------------------------------------------------------------------
-- Numbers
------------------------------------------------------------------------------

-- | Types of the data, the coefficients and the output of the filter:
-- 'Signed' and 'Unsigned'. Its methods are used by the simulation model and
-- the IP instantiation; you should not need them.
class (forall n. KnownNat n => Integral (a n)) => FirNumber (a :: Nat -> Type) where
  -- | @Signed@ or @Unsigned@, the value of the IP's @Data_Sign@ parameter
  numberSign :: String

  -- | Whether the type is signed
  numberIsSigned :: Bool

  -- | Wraps around if the value does not fit
  numberFromInteger :: KnownNat n => Integer -> a n

  -- | 'pack' the number after extending it to @m@ bits: sign extension for
  -- 'Signed', zero extension for 'Unsigned'
  numberToPaddedBitVector :: (KnownNat n, KnownNat m) => a n -> BitVector m

  -- | 'unpack' the @n@ least significant bits
  numberFromPaddedBitVector :: (KnownNat n, KnownNat m) => BitVector m -> a n

instance FirNumber Signed where
  numberSign = "Signed"
  numberIsSigned = True
  numberFromInteger = fromInteger
  numberToPaddedBitVector = pack . resize
  numberFromPaddedBitVector = unpack . resize

instance FirNumber Unsigned where
  numberSign = "Unsigned"
  numberIsSigned = False
  numberFromInteger = fromInteger
  numberToPaddedBitVector = pack . resize
  numberFromPaddedBitVector = unpack . resize

-- | Type of the output of a filter with data of type @a@ and coefficients of
-- type @c@: 'Unsigned' if both are 'Unsigned', 'Signed' otherwise.
type family FirOutput (a :: Nat -> Type) (c :: Nat -> Type) :: Nat -> Type where
  FirOutput Unsigned Unsigned = Unsigned
  FirOutput a c = Signed

-- | Width of a number padded to whole bytes, as on the AXI4-Stream interfaces
-- of the IP
type ByteWidth (n :: Nat) = 8 * DivRU n 8

------------------------------------------------------------------------------
-- Structure of the IP
------------------------------------------------------------------------------

-- | The structure the IP uses for a set of coefficients
data Structure
  = NonSymmetricStructure
    -- ^ One multiply-add unit per coefficient
  | SymmetricStructure
    -- ^ The coefficients are symmetric or negative symmetric: @c_i = c_(n-1-i)@
    -- for all @i@, or @c_i = -c_(n-1-i)@ for all @i@ except the centre tap of
    -- an odd number of coefficients. A pre-adder adds (or subtracts) the
    -- samples of each pair of taps before the multiplication.
  | HalfBandStructure
    -- ^ A symmetric half-band filter: an odd number of coefficients @n@, with
    -- @n mod 4 == 3@ and @n >= 7@, of which every second one, counting from
    -- the centre tap, is zero (the centre tap itself may have any value).
  | HilbertStructure
    -- ^ A negative symmetric Hilbert transformer: like a half-band filter, but
    -- negative symmetric, with a centre tap of zero. The IP then has two
    -- outputs, which is not supported.
  deriving (Eq, Show)

-- | The structure the IP uses for a list of coefficients, see 'Structure'.
-- Gives 'Nothing' if the coefficients do not have the structure the
-- configuration demands; Vivado rejects such a configuration.
inferStructure :: CoefficientStructure -> [Integer] -> Maybe Structure
inferStructure cfg cs = case cfg of
  NonSymmetric -> Just NonSymmetricStructure
  Symmetric
    | symmetric -> Just SymmetricStructure
    | otherwise -> Nothing
  NegativeSymmetric
    | negSymmetric -> Just SymmetricStructure
    | otherwise -> Nothing
  Inferred
    | halfBandLike && symmetric -> Just HalfBandStructure
    | halfBandLike && negSymmetric && centre == 0 -> Just HilbertStructure
    | symmetric || negSymmetric -> Just SymmetricStructure
    | otherwise -> Just NonSymmetricStructure
 where
  n = P.length cs
  m = n `div` 2
  pairs = P.take m (P.zip cs (P.reverse cs))
  symmetric = P.all (\(a, b) -> a == b) pairs
  negSymmetric = P.all (\(a, b) -> a == negate b) pairs
  centre = cs P.!! m
  halfBandLike =
       n >= 7
    && n `mod` 4 == 3
    && P.and [c == 0 | (i, c) <- P.zip [0 ..] cs, i /= m, P.even (i - m)]

-- | Number of multiply-add units the IP uses for a structure and a number of
-- coefficients. This is the number the IP reports as @C_NUM_MADDS@.
multiplyAdds :: Structure -> Int -> Int
multiplyAdds structure n = case structure of
  NonSymmetricStructure -> n
  SymmetricStructure -> (n + 1) `div` 2
  HalfBandStructure -> (n + 1) `div` 4
  HilbertStructure -> (n + 1) `div` 4

-- | Number of DSP slices of a single multiplication: 1, 2 or 4. A DSP slice
-- multiplies a wide operand (25 bits for 'Dsp48E1', 27 bits for 'Dsp48E2') by
-- an 18-bit operand. An unsigned operand needs an extra bit, and so does the
-- output of a pre-adder. The IP assigns the data and the coefficient to
-- either port. If one doesn't fit, it splits one of them in two (2 slices) or
-- both (4 slices). An operand of @w@ bits can be split over two ports of @p@
-- bits if @w <= 2*p - 1@; the output of a pre-adder if @w <= 2*p - 2@.
dspsPerMultiply ::
  DspSlice ->
  Structure ->
  -- | Data width
  Int ->
  -- | Data is signed
  Bool ->
  -- | Coefficient width
  Int ->
  -- | Coefficients are signed (inferred from their values)
  Bool ->
  Int
dspsPerMultiply dsp structure dw dSigned cw cSigned
  | fits1 = 1
  | fits2 = 2
  | otherwise = 4
 where
  wide = case dsp of
    Dsp48E1 -> 25
    Dsp48E2 -> 27
  narrow = 18 :: Int
  preAdd = structure /= NonSymmetricStructure
  a = dw + extraBit (not dSigned) + extraBit preAdd
  b = cw + extraBit (not cSigned)
  extraBit x = if x then 1 else 0
  splitMax p = 2 * p - 1 - extraBit preAdd
  fits1 = (a <= wide && b <= narrow) || (a <= narrow && b <= wide)
  fits2 =
       (a <= splitMax narrow && b <= wide)
    || (a <= splitMax wide && b <= narrow)
    || (b <= 2 * narrow - 1 && a <= wide)
    || (b <= 2 * wide - 1 && a <= narrow)

-- | Full precision output width of the IP: the data width plus
-- @ceil (log2 (sum |c_i|))@, plus one for unsigned data and signed
-- coefficients.
fullPrecisionWidth ::
  -- | Data width
  Int ->
  -- | Data is signed
  Bool ->
  -- | Coefficients
  [Integer] ->
  Int
fullPrecisionWidth dw dSigned cs =
  dw + clog2 (P.sum (P.map abs cs)) + (if not dSigned && P.any (< 0) cs then 1 else 0)
 where
  clog2 :: Integer -> Int
  clog2 x
    | x <= 1 = 0
    | otherwise = 1 + clog2 ((x + 1) `div` 2)

-- | All properties of a filter that follow from its configuration and
-- coefficients
data Params = Params
  { config :: Config
  , coefficients :: [Integer]
  , dataWidth :: Int
  , dataSigned :: Bool
  , coefficientWidth :: Int
  , coefficientTypeSigned :: Bool
    -- ^ The declared type of the coefficients. The IP infers the sign from the
    -- values, see 'coefficientsSigned'.
  , outputWidth :: Int
  , samplePeriod :: Int
    -- ^ The number of clock cycles per input sample the IP is configured for
  , structure :: Structure
    -- ^ The structure of the IP. 'NonSymmetricStructure' if the coefficients
    -- do not have the structure the configuration demands, see 'checkParams'.
  , structureOk :: Bool
  , coefficientsSigned :: Bool
    -- ^ Whether any coefficient is negative
  , fullWidth :: Int
    -- ^ 'fullPrecisionWidth'
  , madds :: Int
    -- ^ The number of multiply-add units: 'multiplyAdds' for a sample period
    -- of 1, fewer for longer sample periods (see 'oversampling').
  , oversampling :: Int
    -- ^ The number of coefficients (or pairs of coefficients) each
    -- multiply-add unit handles
  , cyclesPerSample :: Int
    -- ^ The number of clock cycles the IP needs per sample: it can accept a
    -- new sample every 'cyclesPerSample' cycles. This is 'oversampling', plus
    -- one for symmetric rounding, but at most 'samplePeriod'.
  , frontPadding :: Int
    -- ^ The number of zeros the IP effectively puts in front of the
    -- coefficients, which delays the output by as many samples. When the
    -- multiply-add units have more room than the coefficients need
    -- (@'madds' * 'oversampling' > 'multiplyAdds'@), the IP pads the
    -- coefficients with zeros. It pads non-symmetric coefficients at the end,
    -- but symmetric coefficients at both ends, to keep them symmetric: one zero
    -- at each end per padded pair, or two for a half-band filter, of which only
    -- every second coefficient is in a multiply-add unit.
  , dsps :: Int
    -- ^ 'dspsPerMultiply'
  } deriving (Show)

-- | Compute the properties of a filter
mkParams ::
  Config ->
  -- | Coefficients
  [Integer] ->
  -- | Data width
  Int ->
  -- | Data is signed
  Bool ->
  -- | Coefficient width
  Int ->
  -- | Coefficient type is signed
  Bool ->
  -- | Output width
  Int ->
  -- | Sample period
  Int ->
  Params
mkParams config coefficients dataWidth dataSigned coefficientWidth coefficientTypeSigned outputWidth samplePeriod =
  Params{..}
 where
  inferred = inferStructure (coefficientStructure config) coefficients
  structure = fromMaybe NonSymmetricStructure inferred
  structureOk = isJust inferred
  coefficientsSigned = P.any (< 0) coefficients
  fullWidth = fullPrecisionWidth dataWidth dataSigned coefficients
  -- With a sample period of p clock cycles, each multiply-add unit can handle
  -- up to p coefficients (or pairs). The IP uses as few multiply-add units as
  -- that allows, and spreads the coefficients over them evenly.
  maddsP1 = multiplyAdds structure (P.length coefficients)
  madds = divRU maddsP1 (max 1 samplePeriod)
  oversampling = divRU maddsP1 (max 1 madds)
  divRU a b = (a + b - 1) `div` b
  symmetricRounding =
    rounding config `P.elem` [SymmetricRoundingToZero, SymmetricRoundingToInfinity]
  cyclesPerSample =
    min (max 1 samplePeriod) (oversampling + (if symmetricRounding then 1 else 0))
  padding = madds * oversampling - maddsP1
  frontPadding = case structure of
    NonSymmetricStructure -> 0
    SymmetricStructure -> padding
    HalfBandStructure -> 2 * padding
    HilbertStructure -> 2 * padding
  -- The accumulator of a DSP slice has 48 bits. If the full precision result
  -- does not fit, the IP splits multiplications that would fit a single DSP
  -- slice over two, each with a narrower product.
  dsps = case dspsPerMultiply (dspSlice config) structure dataWidth dataSigned coefficientWidth coefficientsSigned of
    1 | fullWidth > 48 -> 2
    d -> d

-- | Latency of the IP in clock cycles, as reported by Vivado (@C_LATENCY@):
-- from accepting a sample to giving the output for it
latency :: Params -> Int
latency Params{..} =
    madds
  + 5
  + (if structure == NonSymmetricStructure then 0 else 1)
  + (case dsps of { 1 -> 0; 2 -> 2; _ -> 6 })
  + (if rounding config `P.elem` [SymmetricRoundingToZero, SymmetricRoundingToInfinity] then 1 else 0)
  + 4 * (columns - 1)
  + (if samplePeriod <= 1 then singleCycle else multiCycle)
 where
  -- The fully parallel filter, which takes a sample every clock cycle
  singleCycle = if halfBandCentre then 1 else 0
  halfBandCentre = structure == HalfBandStructure && not centreIsShift
  -- A filter of which every multiply-add unit handles several coefficients,
  -- one per clock cycle, and accumulates their products
  -- A half-band filter (unless its centre tap is a shift), and a filter with
  -- several multiply-add units on a 7 series device, take a cycle more (but
  -- not two for both).
  multiCycle =
      oversampling
    + (if halfBandCentre || (dspSlice config == Dsp48E1 && madds > 1) then 1 else 0)
  -- On UltraScale devices, the IP implements a centre tap that is a positive
  -- power of two as a shift, which takes a cycle less
  centre = coefficients P.!! (P.length coefficients `div` 2)
  centreIsShift =
    dspSlice config == Dsp48E2 && centre > 0 && popCount centre == 1
  columns = case dspColumnLength config of
    Just l | l > 0 -> (madds + l - 1) `div` l
    _ -> 1

-- | Check that a filter is supported and that the delay @d@ in its type
-- equals its latency. Errors otherwise.
checkParams :: forall d. (KnownNat d, HasCallStack) => Params -> ()
checkParams params@Params{..}
  | P.length coefficients < 2 =
      err "the IP needs at least two coefficients"
  | samplePeriod < 1 =
      err "the sample period should be at least 1"
  | not structureOk =
      err $
           "the coefficients are not " <> show (coefficientStructure config)
  | structure == HilbertStructure =
      err $
           "the coefficients form a Hilbert transformer, which the IP gives two "
        <> "outputs: that is not supported. Use coefficientStructure = "
        <> "NegativeSymmetric instead."
  | not dataSigned && coefficientTypeSigned && not coefficientsSigned =
      err $
           "with unsigned data, signed coefficients need at least one negative "
        <> "coefficient: the IP infers the sign of the coefficients from their "
        <> "values, which makes the output unsigned. Use Unsigned coefficients."
  | rounding config == FullPrecision && outputWidth /= fullWidth =
      err $
           "the output width (" <> show outputWidth <> ") does not match the full "
        <> "precision output width of the IP (" <> show fullWidth <> ")"
  | rounding config /= FullPrecision && outputWidth >= fullWidth =
      err $
           "the output width (" <> show outputWidth <> ") should be less than "
        <> "the full precision output width of the IP (" <> show fullWidth
        <> ") when rounding"
  | natToNum @d /= lat =
      err $
           "the delay of the result (" <> show (natToNum @d :: Int) <> ") does "
        <> "not match the latency of the IP (" <> show lat <> ")"
  | otherwise = ()
 where
  lat = latency params
  err msg = withFrozenCallStack $ error $
    "Clash.Cores.Xilinx.Fir: " <> msg <> ". Configuration: " <> show params

------------------------------------------------------------------------------
-- Simulation model
------------------------------------------------------------------------------

-- | Round a full precision result of @w@ bits to @w - k@ bits, see 'Rounding'
roundOutput :: Rounding -> Int -> Integer -> Integer
roundOutput _ k y | k <= 0 = y
roundOutput mode k y = case mode of
  FullPrecision -> y
  TruncateLsbs -> q
  NonSymmetricRoundingDown -> nearest q
  NonSymmetricRoundingUp -> nearest (q + 1)
  SymmetricRoundingToZero -> nearest (if y >= 0 then q else q + 1)
  SymmetricRoundingToInfinity -> nearest (if y >= 0 then q + 1 else q)
  ConvergentRoundingToEven -> nearest (if P.even q then q else q + 1)
  ConvergentRoundingToOdd -> nearest (if P.odd q then q else q + 1)
 where
  (q, r) = y `divMod` bit k
  half = bit (k - 1)
  nearest tie
    | r < half = q
    | r > half = q + 1
    | otherwise = tie

-- | The output of a filter for the most recent samples (newest first): the
-- sum of products, rounded to the output width. The result is not wrapped to
-- the output width yet. The coefficients include the 'frontPadding'.
firOutputModel :: Params -> [Integer] -> Integer
firOutputModel Params{..} samples =
  roundOutput (rounding config) (fullWidth - outputWidth)
    (P.sum (P.zipWith (*) (P.replicate frontPadding 0 <> coefficients) samples))

-- | Simulation model of the IP for valid-qualified samples, see
-- 'Clash.Cores.Xilinx.Fir.Explicit.firValidWith'. Gives the IP's @TREADY@ and
-- its output, @Just@ when @TVALID@ is asserted. The IP:
--
-- * is not ready in the first enabled clock cycle;
--
-- * accepts a sample in an enabled clock cycle in which it is ready and the
--   sample is valid ('Just');
--
-- * is not ready in the 'cyclesPerSample' minus one enabled clock cycles
--   after it accepts a sample;
--
-- * gives the output for an accepted sample 'latency' enabled clock cycles
--   later, for a single clock cycle (or as long as the IP is disabled).
--
-- The history of the filter starts out as all zeros. The output includes the
-- delay of the 'frontPadding'.
firValidModel ::
  forall dom.
  KnownDomain dom =>
  Params ->
  Clock dom ->
  Enable dom ->
  Signal dom (Maybe Integer) ->
  (Signal dom Bool, Signal dom (Maybe Integer))
firValidModel params clk en x = (ready, delayChain (latency params) y)
 where
  started = delay clk en False (pure True)
  busy :: Signal dom Int
  busy = delay clk en 0 busy1
  ready = (&&) <$> started <*> ((== 0) <$> busy)
  accepted = (\r s -> r && isJust s) <$> ready <*> x
  busy1 = (\acc b -> if acc then cyclesPerSample params - 1 else max 0 (b - 1)) <$> accepted <*> busy

  history :: Signal dom [Integer]
  history = delay clk en initHistory history1
  initHistory = P.replicate (frontPadding params + P.length (coefficients params)) 0
  history1 = newHistory <$> accepted <*> x <*> history
  newHistory acc smp h = case smp of
    Just s | acc -> s : P.init h
    _ -> h

  y = (\acc h -> if acc then Just (firOutputModel params h) else Nothing) <$> accepted <*> history1

  delayChain :: Int -> Signal dom (Maybe Integer) -> Signal dom (Maybe Integer)
  delayChain 0 s = s
  delayChain i s = delayChain (i - 1) (delay clk en Nothing s)

-- | Simulation model of the IP for a sample every clock cycle, see
-- 'Clash.Cores.Xilinx.Fir.Explicit.firWith': 'firValidModel' with valid
-- samples only. The outputs for which the IP does not assert @TVALID@, those
-- of the first cycles and of the sample in the first enabled cycle, are
-- undefined.
firModel ::
  forall dom.
  (KnownDomain dom, HasCallStack) =>
  Params ->
  Clock dom ->
  Enable dom ->
  Signal dom Integer ->
  Signal dom Integer
firModel params clk en x =
  fromMaybe und <$> snd (firValidModel params clk en (Just <$> x))
 where
  und = withFrozenCallStack $ errorX "Clash.Cores.Xilinx.Fir: initial value undefined"

------------------------------------------------------------------------------
-- Instantiating the IP
------------------------------------------------------------------------------

-- | Options of the FIR Compiler. Note that Clash evaluates these when
-- generating HDL, so they must be simple. The coefficients go straight into an
-- t'IntegerVec'.
firOptions ::
  forall a c wd wc wo n.
  ( FirNumber a
  , FirNumber c
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  ) =>
  Config ->
  -- | Sample period
  Integer ->
  Vec n (c wc) ->
  Vec 30 (OptionName, XilinxWizardOption)
firOptions cfg samplePeriod coefs =
     ("CONFIG.Filter_Type",                  StrOpt "Single_Rate")
  :> ("CONFIG.Rate_Change_Type",             StrOpt "Integer")
  :> ("CONFIG.Interpolation_Rate",           IntegerOpt 1)
  :> ("CONFIG.Decimation_Rate",              IntegerOpt 1)
  :> ("CONFIG.Number_Channels",              IntegerOpt 1)
  :> ("CONFIG.Number_Paths",                 IntegerOpt 1)
  :> ("CONFIG.RateSpecification",            StrOpt "Input_Sample_Period")
  :> ("CONFIG.SamplePeriod",                 IntegerOpt samplePeriod)
  :> ("CONFIG.CoefficientSource",            StrOpt "Vector")
  :> ("CONFIG.CoefficientVector",            IntegerVecOpt (IntegerVec coefs))
  :> ("CONFIG.Coefficient_Sets",             IntegerOpt 1)
  :> ("CONFIG.Coefficient_Reload",           BoolOpt False)
  :> ("CONFIG.Quantization",                 StrOpt "Integer_Coefficients")
  :> ("CONFIG.Coefficient_Width",            IntegerOpt (natToNum @wc))
  :> ("CONFIG.Coefficient_Fractional_Bits",  IntegerOpt 0)
  :> ("CONFIG.Coefficient_Structure",        StrOpt (coefficientStructureToTcl (coefficientStructure cfg)))
  :> ("CONFIG.Data_Sign",                    StrOpt (numberSign @a))
  :> ("CONFIG.Data_Width",                   IntegerOpt (natToNum @wd))
  :> ("CONFIG.Data_Fractional_Bits",         IntegerOpt 0)
  :> ("CONFIG.Output_Rounding_Mode",         StrOpt (roundingToTcl (rounding cfg)))
  :> ("CONFIG.Output_Width",                 IntegerOpt (natToNum @wo))
  :> ("CONFIG.Filter_Architecture",          StrOpt "Systolic_Multiply_Accumulate")
  :> ("CONFIG.Optimization_Goal",            StrOpt "Area")
  :> ("CONFIG.Multi_Column_Support",         StrOpt "Automatic")
  :> ("CONFIG.Has_ACLKEN",                   BoolOpt True)
  :> ("CONFIG.Has_ARESETn",                  BoolOpt False)
  :> ("CONFIG.M_DATA_Has_TREADY",            BoolOpt False)
     -- Without the FIFO, the latency of the IP equals the latency it reports
  :> ("CONFIG.S_DATA_Has_FIFO",              BoolOpt False)
  :> ("CONFIG.S_DATA_Has_TUSER",             StrOpt "Not_Required")
  :> ("CONFIG.M_DATA_Has_TUSER",             StrOpt "Not_Required")
  :> Nil
{-# INLINE firOptions #-}

-- | Instantiate the FIR Compiler. It is always configured as:
--
-- * A single rate, single channel filter
-- * Non-blocking AXI4-Stream interfaces: no FIFO on the input, no @TREADY@ on
--   the output
-- * A clock enable (@aclken@), no reset
--
-- Takes the sample period, the input's @TVALID@ and @TDATA@. Gives the input's
-- @TREADY@ and the output's @TVALID@ and @TDATA@.
firInst ::
  forall a c wd wc wo n dom.
  ( KnownDomain dom
  , FirNumber a
  , FirNumber c
  , KnownNat wd
  , KnownNat wc
  , KnownNat wo
  , KnownNat n
  , HasCallStack
  ) =>
  Config ->
  -- | Sample period
  Integer ->
  Vec n (c wc) ->
  Clock dom ->
  Enable dom ->
  -- | Input @TVALID@
  Signal dom Bool ->
  -- | Input @TDATA@
  Signal dom (a wd) ->
  -- | Input @TREADY@, output @TVALID@ and @TDATA@
  (Signal dom Bool, Signal dom Bool, Signal dom (BitVector (ByteWidth wo)))
firInst cfg samplePeriod coefs clk en valid x =
  (bitToBool <$> unPort tready, bitToBool <$> unPort tvalid, unPort tdata)
 where
  (tready, tvalid, tdata) = go

  go ::
    ( Port "s_axis_data_tready" dom Bit
    , Port "m_axis_data_tvalid" dom Bit
    , Port "m_axis_data_tdata" dom (BitVector (ByteWidth wo))
    )
  go =
    instWithXilinxWizard
      (instConfig "fir")
      XilinxWizard
        { wiz_name = "fir_compiler"
        , wiz_vendor = "xilinx.com"
        , wiz_library = "ip"
        , wiz_version = "7.2"
        , wiz_options = firOptions @a @c @wd @wc @wo cfg samplePeriod coefs
        }
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_data_tvalid" (boolToBit <$> valid))
      -- The IP ignores the padding of the data, but follow the AXI4-Stream
      -- convention of the IP anyway: sign extension for signed data.
      (Port @"s_axis_data_tdata"
        (numberToPaddedBitVector <$> x :: Signal dom (BitVector (ByteWidth wd))))
{-# INLINE firInst #-}
