{-|
Copyright  :  (C) 2026,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation models and IP instantiation for "Clash.Cores.Xilinx.Fft".
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Fft.Internal
  ( -- * Configuration
    Config(..)
  , defConfig
  , Rounding(..)
  , OutputOrdering(..)
  , ComplexMultiplier(..)
  , ButterflyArithmetic(..)
  , Scaling(..)
  , KnownScaling(..)
  , Direction(..)
  , FrameConfig(..)
  , Groups
  , scaleByN
  , FftOutput(..)
  , ByteWidth
  , KnownFft
    -- * Arithmetic
  , ArithScaling(..)
  , FrameResult(..)
  , fftFrameInteger
  , fftFrame
  , twiddle
  , bitReverse
    -- * Cycle-accurate simulation model
  , fftModel
    -- * Instantiating the IP
  , fftSynth
  ) where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Data.Complex (Complex(..))
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)

import Clash.Signal.Internal (Signal((:-)))

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

-- | How the IP drops the least significant bits at the output of the
-- butterflies, see PG109, "Rounding Implementation"
data Rounding
  = Truncation
    -- ^ Round towards minus infinity
  | ConvergentRounding
    -- ^ Round to the nearest value, ties to even
  deriving (Eq, Show)

-- | Order of the output samples
data OutputOrdering
  = BitReversedOrder
    -- ^ Output sample @i@ of a frame is the frequency (or time) index @k@ with
    -- the bits of @i@ reversed
  | NaturalOrder
    -- ^ The output samples come in the order @k = 0, 1, ..., N-1@. The IP
    -- needs a reorder buffer for this, which increases its latency.
  deriving (Eq, Show)

-- | Implementation of the complex multipliers, see PG109, "Optimize Options".
-- It does not change the results, but it can change the latency.
data ComplexMultiplier
  = CmpyLuts
    -- ^ Use CLB logic
  | CmpyResources
    -- ^ Use three real multipliers in DSP slices (the IP's default)
  | CmpyPerformance
    -- ^ Use four real multipliers in DSP slices
  deriving (Eq, Show)

-- | Implementation of the butterflies, see PG109, "Optimize Options". It does
-- not change the results, but it can change the latency.
data ButterflyArithmetic
  = ButterflyLuts
    -- ^ Use CLB logic (the IP's default)
  | ButterflyDsps
    -- ^ Use DSP slices
  deriving (Eq, Show)

-- | Customize the Fast Fourier Transform IP. The options marked as such only
-- influence how the IP is implemented, and therefore its latency, but not the
-- results it computes.
data Config = Config
  { phaseFactorWidth :: !Int
    -- ^ Width of the phase factors (twiddle factors), 8 to 34 bits
  , rounding :: !Rounding
    -- ^ Rounding at the output of the butterflies
  , outputOrdering :: !OutputOrdering
    -- ^ Order of the output samples
  , complexMultiplier :: !ComplexMultiplier
    -- ^ Implementation of the complex multipliers (implementation only)
  , butterflyArithmetic :: !ButterflyArithmetic
    -- ^ Implementation of the butterflies (implementation only)
  } deriving (Eq, Show)

-- | Default customization: 16-bit phase factors, 'Truncation',
-- 'BitReversedOrder', and the IP's defaults for the implementation options
defConfig :: Config
defConfig =
  Config
    { phaseFactorWidth = 16
    , rounding = Truncation
    , outputOrdering = BitReversedOrder
    , complexMultiplier = CmpyResources
    , butterflyArithmetic = ButterflyLuts
    }

-- | Direction of a transform
data Direction
  = Forward
    -- ^ The forward transform: @X(k) = sum x(n) exp(-2 pi j n k / N)@
  | Inverse
    -- ^ The inverse transform, without the factor @1/N@:
    -- @x(n) = sum X(k) exp(2 pi j n k / N)@
  deriving (Eq, Show, Generic, NFDataX, BitPack, ShowX)

-- | Number of radix-2² stages ("groups") of a transform of length @2^nfft@: the
-- number of entries in a scaling schedule
type Groups (nfft :: Nat) = DivRU nfft 2

-- | Width of a field padded to whole bytes, as on the AXI4-Stream interfaces
-- of the IP
type ByteWidth (n :: Nat) = 8 * DivRU n 8

-- | How the IP keeps the values in range. See PG109, "Finite Word Length
-- Considerations".
data Scaling
  = Unscaled
    -- ^ Full precision: the output has @bw + nfft + 1@ bits
  | Scaled
    -- ^ The output is shifted right after each pair of radix-2 stages, as set
    -- by a scaling schedule per frame. Values that do not fit wrap around,
    -- which the IP reports as an overflow.
  | BlockFloatingPoint
    -- ^ The IP scales the frame so that its largest value fits, and reports
    -- the amount of scaling as a block exponent

-- | Term-level version of 'Scaling', for the arithmetic model
data ArithScaling
  = ArithUnscaled
  | ArithScaled [Int]
    -- ^ The scaling schedule: shift per group, group 0 first
  | ArithBlockFloatingPoint
  deriving (Eq, Show)

-- | Properties of a 'Scaling' option. You should not need its methods.
class
  ( NFDataX (FrameStatus s), ShowX (FrameStatus s), Show (FrameStatus s)
  , Eq (FrameStatus s), BitPack (FrameStatus s), KnownNat (BitSize (FrameStatus s))
  ) => KnownScaling (s :: Scaling) where
  -- | Width of the output samples, for input samples of width @bw@
  type OutputWidth s (nfft :: Nat) (bw :: Nat) :: Nat

  -- | Scaling schedule of a frame: for 'Scaled', the number of bits to shift
  -- the data right after each group of two radix-2 stages (the first group
  -- first). The last group of a transform with an odd @nfft@ consists of a
  -- single radix-2 stage, and its shift must be 0 or 1.
  type ScaleSchedule s (nfft :: Nat) :: Type

  -- | Status of an output sample: for 'Scaled', whether the frame overflowed
  -- (only 'True' on the last sample of the frame); for 'BlockFloatingPoint',
  -- the block exponent of the frame
  type FrameStatus s :: Type

  -- | Width of the IP's @s_axis_config_tdata@
  type ConfigWidth s (nfft :: Nat) :: Nat

  -- | Width of the status fields in the IP's @m_axis_data_tuser@, after
  -- @XK_INDEX@
  type StatusWidth s :: Nat

  -- | Value of the IP's @scaling_options@ parameter
  scalingOption :: String

  -- | Whether the IP has a status channel (and the @OVFLO@ option)
  hasStatusChannel :: Bool

  -- | Scaling schedule for the arithmetic model
  arithScaling :: KnownNat nfft => ScaleSchedule s nfft -> ArithScaling

  -- | Status from the arithmetic model: block exponent, and whether the frame
  -- overflowed and the sample is the last one
  mkStatus :: Int -> Bool -> FrameStatus s

  -- | The SCALE_SCH field of the configuration channel, which is empty
  -- except for 'Scaled'
  configWord ::
    KnownNat nfft =>
    Bit ->
    ScaleSchedule s nfft ->
    BitVector (ConfigWidth s nfft)

  -- | Status from the status fields in @m_axis_data_tuser@ and @tlast@
  statusFromTuser :: BitVector (StatusWidth s) -> Bool -> FrameStatus s

instance KnownScaling 'Unscaled where
  type OutputWidth 'Unscaled nfft bw = bw + nfft + 1
  type ScaleSchedule 'Unscaled nfft = ()
  type FrameStatus 'Unscaled = ()
  type ConfigWidth 'Unscaled nfft = 8
  type StatusWidth 'Unscaled = 0
  scalingOption = "unscaled"
  hasStatusChannel = False
  arithScaling () = ArithUnscaled
  mkStatus _ _ = ()
  configWord fwd () = resize (pack fwd)
  statusFromTuser _ _ = ()

instance KnownScaling 'Scaled where
  type OutputWidth 'Scaled nfft bw = bw
  type ScaleSchedule 'Scaled nfft = Vec (Groups nfft) (Index 4)
  type FrameStatus 'Scaled = Bool
  type ConfigWidth 'Scaled nfft = ByteWidth (1 + 2 * Groups nfft)
  type StatusWidth 'Scaled = 8
  scalingOption = "scaled"
  hasStatusChannel = True
  arithScaling sch = ArithScaled (P.map fromEnum (toList sch))
  mkStatus _ ovf = ovf
  configWord fwd sch = resize (pack (reverse sch) ++# pack fwd)
  statusFromTuser tuser lst = lst && testBit tuser 0

instance KnownScaling 'BlockFloatingPoint where
  type OutputWidth 'BlockFloatingPoint nfft bw = bw
  type ScaleSchedule 'BlockFloatingPoint nfft = ()
  type FrameStatus 'BlockFloatingPoint = Unsigned 5
  type ConfigWidth 'BlockFloatingPoint nfft = 8
  type StatusWidth 'BlockFloatingPoint = 8
  scalingOption = "block_floating_point"
  hasStatusChannel = True
  arithScaling () = ArithBlockFloatingPoint
  mkStatus e _ = fromIntegral e
  configWord fwd () = resize (pack fwd)
  statusFromTuser tuser _ = unpack (resize tuser)

-- | Configuration of a single frame
data FrameConfig (s :: Scaling) (nfft :: Nat) = FrameConfig
  { direction :: Direction
    -- ^ Forward or inverse transform
  , scaleSchedule :: ScaleSchedule s nfft
    -- ^ Scaling schedule, only used for 'Scaled'
  }

deriving instance Show (ScaleSchedule s nfft) => Show (FrameConfig s nfft)
deriving instance Eq (ScaleSchedule s nfft) => Eq (FrameConfig s nfft)
deriving instance Generic (FrameConfig s nfft)
deriving instance NFDataX (ScaleSchedule s nfft) => NFDataX (FrameConfig s nfft)
deriving instance ShowX (ScaleSchedule s nfft) => ShowX (FrameConfig s nfft)

-- | The scaling schedule that scales by @1/N@, which never overflows for
-- radix-2 based transforms of length @N@ with an input that fits: a shift of
-- 2 in every group, except 1 in the last group when @nfft@ is odd. This is
-- what the IP uses after a reset.
--
-- PG109 recommends a shift of 3 in the first group to avoid overflow
-- completely.
scaleByN :: forall nfft. KnownNat nfft => Vec (Groups nfft) (Index 4)
scaleByN = imap groupShift (repeat ())
 where
  groupShift i ()
    | P.odd (natToNum @nfft :: Int) && fromEnum i == natToNum @(Groups nfft) - 1 = 1
    | otherwise = 2

-- | An output sample of the transform
data FftOutput (s :: Scaling) (nfft :: Nat) (w :: Nat) = FftOutput
  { index :: Index (2 ^ nfft)
    -- ^ Index @k@ of the sample in the transform (@XK_INDEX@)
  , value :: Complex (Signed w)
    -- ^ The value of the sample. For a frame that overflowed (see
    -- 'FrameStatus'), the IP's values are unusable, and the simulation model
    -- gives undefined values.
  , lastSample :: Bool
    -- ^ Whether this is the last sample of a frame
  , status :: FrameStatus s
    -- ^ See 'FrameStatus'
  } deriving (Generic)

deriving instance (KnownNat nfft, KnownNat w, KnownScaling s) => Show (FftOutput s nfft w)
deriving instance (KnownNat nfft, KnownNat w, KnownScaling s) => Eq (FftOutput s nfft w)
deriving instance (KnownNat nfft, KnownNat w, KnownScaling s) => NFDataX (FftOutput s nfft w)
deriving instance (KnownNat nfft, KnownNat w, KnownScaling s) => ShowX (FftOutput s nfft w)
deriving instance
  ( KnownNat nfft, KnownNat w, KnownScaling s
  , KnownNat (BitSize (Index (2 ^ nfft))), KnownNat (BitSize (FrameStatus s))
  ) => BitPack (FftOutput s nfft w)

-- | Constraints on the type-level parameters of the FFT, all of which hold for
-- valid parameters
type KnownFft s nfft bw =
  ( KnownScaling s
  , KnownNat nfft
  , KnownNat bw
  , 3 <= nfft
  , nfft <= 16
  , 8 <= bw
  , bw <= 34
  , KnownNat (2 ^ nfft)
  , 1 <= 2 ^ nfft
  , KnownNat (OutputWidth s nfft bw)
  , KnownNat (ByteWidth bw)
  , KnownNat (ByteWidth (OutputWidth s nfft bw))
  , KnownNat (ByteWidth nfft)
  , KnownNat (ConfigWidth s nfft)
  , KnownNat (StatusWidth s)
  , KnownNat (Groups nfft)
  )

------------------------------------------------------------------------------
-- Arithmetic
------------------------------------------------------------------------------

-- | The result of a transform of one frame by 'fftFrameInteger'
data FrameResult = FrameResult
  { frSamples :: [(Integer, Integer)]
    -- ^ The output samples in natural order, as integers
  , frBlockExponent :: Int
    -- ^ Block exponent, for 'ArithBlockFloatingPoint'
  , frOverflow :: Bool
    -- ^ Whether the frame overflowed, for 'ArithScaled'
  } deriving (Eq, Show)

-- | The phase factor @W_l^e = exp(-2 pi j e / l)@ for a forward transform, or
-- its conjugate for an inverse transform, quantized to @tw@ bits with @tw - 1@
-- fractional bits by rounding to the nearest value. The values @1@ and @j@ do
-- not fit, but the IP never multiplies by @1@, and multiplying by the
-- out-of-range value @j@ instead (for an inverse transform) is exact, like
-- multiplying by @-j@.
twiddle :: Int -> Direction -> Int -> Int -> (Integer, Integer)
twiddle tw dir e l = (q (cos theta), q (sign * sin theta))
 where
  theta = 2 * pi * fromIntegral e / fromIntegral l :: Double
  sign = case dir of Forward -> -1; Inverse -> 1
  q v = round (v * fromInteger (1 `shiftL` (tw - 1)))

-- | Reverse the lowest @m@ bits of an index
bitReverse :: Int -> Int -> Int
bitReverse m i = P.foldl (\acc b -> 2 * acc + (if testBit i b then 1 else 0)) 0 [0 .. m - 1]

{- | Model of the IP's arithmetic for the /Pipelined, Streaming I\/O/
architecture, for one frame of @2^nfft@ samples of @bw@ bits, as integers in
natural order. Gives the output in natural order.

The architecture is a decimation-in-frequency FFT, built from radix-2²
groups: two radix-2 butterfly stages, where the second one multiplies a quarter
of its inputs by @-j@ (or @j@ for an inverse transform), followed by a complex
multiplication with the phase factors @W_l^(n (k1 + 2 k2))@ of a group of
length @l@. See He and Torkelson, "A New Approach to Pipeline FFT Processor"
(1996). When @nfft@ is odd, the last group is a single radix-2 stage.

The butterflies are exact. The complex multiplier keeps 6 (unscaled) or 5
(scaled) fractional bits below the precision of its input, rounding to the
nearest value with ties towards minus infinity (PG109: "a simple rounding
constant [...] round-towards-minus-infinity"). At the end of each group, the
IP drops these bits, and for 'ArithScaled' also the bits of the group's
scaling, by 'Truncation' or 'ConvergentRounding'. The last radix-2² group,
which has no phase factors, is an exception: it sees at most two fractional
bits, so a shift by three bits first truncates one bit.

With 'ArithScaled', a group's result overflows if it does not fit in @bw@ bits
when truncated. It then wraps around, and the frame overflowed. Otherwise, it
saturates if rounding up makes it too large. With 'ArithBlockFloatingPoint', the IP computes
the unscaled transform, and shifts it right by the smallest number of bits for
which all values, truncated, fit in @bw@ bits: the block exponent. The shift
always uses convergent rounding, and saturates if rounding up makes a value
too large.

We found the rounding details with Xilinx's C model of the IP, and confirmed
them against it; see @test/Test/Cores/Xilinx/Fft.hs@.
-}
fftFrameInteger ::
  -- | Input width @bw@
  Int ->
  -- | Phase factor width
  Int ->
  Rounding ->
  ArithScaling ->
  Direction ->
  -- | Input samples, natural order. The length must be a power of two.
  [(Integer, Integer)] ->
  FrameResult
fftFrameInteger bw tw rnd scaling dir xs =
  case scaling of
    ArithUnscaled ->
      FrameResult (natural (transform 6 (P.repeat 0) False)) 0 False
    ArithScaled sch ->
      let ys = transform 5 sch True
          ovf = P.or [o | (_, o) <- ys]
      in FrameResult (natural ys) 0 ovf
    ArithBlockFloatingPoint ->
      let ys = natural (transform 6 (P.repeat 0) False)
          vals = P.concat [[a, b] | (a, b) <- ys]
          e = blockExponent 0
          blockExponent s
            | P.all (fits . (`shiftR` s)) vals = s
            | otherwise = blockExponent (s + 1)
          -- Rounding up can give a value just out of range, which saturates
          norm = P.min hi . convergent e
      in FrameResult [(norm a, norm b) | (a, b) <- ys] e False
 where
  n = P.length xs
  m = log2 n
  log2 k = if k <= 1 then 0 else 1 + log2 (k `div` 2)

  lo = negate (1 `shiftL` (bw - 1))
  hi = (1 `shiftL` (bw - 1)) - 1
  fits v = lo <= v && v <= hi
  wrapBw v = ((v - lo) `mod` (1 `shiftL` bw)) + lo

  natural ys =
    let arr = Map.fromList (P.zip [bitReverse m p | p <- [0 ..]] (P.map fst ys))
    in Map.elems arr

  dropBits = case rnd of
    Truncation -> \k v -> v `shiftR` k
    ConvergentRounding -> convergent

  -- Run all groups. Gives the values in bit-reversed order, with a flag that
  -- tells whether a group's result did not fit for any of the values that
  -- contributed to them.
  transform :: Int -> [Int] -> Bool -> [((Integer, Integer), Bool)]
  transform g sch wrap = go 0 sch [(x, False) | x <- xs]
   where
    go :: Int -> [Int] -> [((Integer, Integer), Bool)] -> [((Integer, Integer), Bool)]
    go _ _ [] = []
    go grp ks ys
      | l == 1 = ys
      | [(a, oa), (b, ob)] <- ys =
          let o = oa || ob
          in [finish k (addC a b) o, finish k (subC a b) o]
      | otherwise =
          let (q0, rest0) = P.splitAt (l `div` 4) ys
              (q1, rest1) = P.splitAt (l `div` 4) rest0
              (q2, q3) = P.splitAt (l `div` 4) rest1
              -- Stage 1
              sum0 = P.zipWith (lift2 addC) q0 q2
              diff0 = P.zipWith (lift2 subC) q0 q2
              sum1 = P.zipWith (lift2 addC) q1 q3
              diff1 = P.map (lift1 mulJ) (P.zipWith (lift2 subC) q1 q3)
              -- Stage 2, in the order of k1 + 2 k2 = 0, 2, 1, 3
              blocks =
                [ (P.zipWith (lift2 addC) sum0 sum1, 0)
                , (P.zipWith (lift2 subC) sum0 sum1, 2)
                , (P.zipWith (lift2 addC) diff0 diff1, 1)
                , (P.zipWith (lift2 subC) diff0 diff1, 3)
                ]
              twiddled =
                [ P.zipWith (\n3 (v, o) -> phaseFactor l (n3 * f) v o) [0 ..] blk
                | (blk, f) <- blocks ]
          in P.concatMap (go (grp + 1) ks') twiddled
     where
      l = P.length ys
      k = case ks of (k0 : _) -> k0; [] -> 0
      ks' = P.drop 1 ks

      -- Multiply by a phase factor and drop the extra bits
      phaseFactor :: Int -> Int -> (Integer, Integer) -> Bool -> ((Integer, Integer), Bool)
      phaseFactor len e (xr, xi) o
        | len == 4 =
            -- The last radix-2² group, without phase factors
            let pre = P.max 0 (k - 2)
            in finish (P.min k 2) (xr `shiftR` pre, xi `shiftR` pre) o
        | e == 0 = finish k (xr, xi) o
        | otherwise =
            let (wr, wi) = twiddle tw dir e len
                d = tw - 1 - g
                mulRound p = (p + (1 `shiftL` (d - 1)) - 1) `shiftR` d
                pr = mulRound (xr * wr - xi * wi)
                pim = mulRound (xr * wi + xi * wr)
            in finish (g + k) (pr, pim) o

      -- Drop @sh@ bits of a value, and limit it to @bw@ bits if needed
      finish :: Int -> (Integer, Integer) -> Bool -> ((Integer, Integer), Bool)
      finish sh (a, b) o =
        let (a', oa) = limit sh a
            (b', ob) = limit sh b
        in ((a', b'), o || oa || ob)

      -- A value overflows if it does not fit when truncated. Rounding up can
      -- give a value just out of range, which saturates.
      limit :: Int -> Integer -> (Integer, Bool)
      limit sh v
        | not wrap = (r, False)
        | fits (v `shiftR` sh) = (P.min hi r, False)
        | otherwise = (wrapBw r, True)
       where
        r = dropBits sh v

  addC (a, b) (c, d) = (a + c, b + d)
  subC (a, b) (c, d) = (a - c, b - d)
  -- Multiply by -j (forward) or j (inverse)
  mulJ (a, b) = case dir of
    Forward -> (b, negate a)
    Inverse -> (negate b, a)
  lift2 f (a, oa) (b, ob) = (f a b, oa || ob)
  lift1 f (a, o) = (f a, o)


-- | Drop @k@ bits with convergent rounding: to the nearest value, ties to even
convergent :: Int -> Integer -> Integer
convergent k v
  | k <= 0 = v
  | r > h || (r == h && P.odd q) = q + 1
  | otherwise = q
 where
  q = v `shiftR` k
  r = v - (q `shiftL` k)
  h = 1 `shiftL` (k - 1)

-- | 'fftFrameInteger' on typed samples, for frames of @2^nfft@ samples. Gives
-- the output in natural order, the block exponent, and whether the frame
-- overflowed.
fftFrame ::
  forall s nfft bw.
  KnownFft s nfft bw =>
  Config ->
  FrameConfig s nfft ->
  [Complex (Signed bw)] ->
  ([Complex (Signed (OutputWidth s nfft bw))], Int, Bool)
fftFrame Config{phaseFactorWidth, rounding} FrameConfig{direction, scaleSchedule} xs =
  ( [fromInteger a :+ fromInteger b | (a, b) <- frSamples r]
  , frBlockExponent r
  , frOverflow r )
 where
  r =
    fftFrameInteger
      (natToNum @bw) phaseFactorWidth rounding
      (arithScaling @s @nfft scaleSchedule) direction
      [(toInteger a, toInteger b) | a :+ b <- xs]

------------------------------------------------------------------------------
-- Cycle-accurate simulation model
------------------------------------------------------------------------------

-- | State of 'fftModel'
data ModelState s nfft bw = ModelState
  { msTime :: !Int
    -- ^ Current clock cycle
  , msInReset :: !Bool
    -- ^ The IP's internal (registered) reset
  , msSinceReset :: !Int
    -- ^ Number of cycles since the internal reset was deasserted
  , msPrev :: !Int
    -- ^ Cycle in which the processing core took the previous sample
  , msIndex :: !Int
    -- ^ Index of the next sample in its frame
  , msFrameStart :: !Int
    -- ^ Cycle in which the processing core took the first sample of the
    -- current frame
  , msFrameConfig :: FrameConfig s nfft
  , msFrame :: [Complex (Signed bw)]
    -- ^ Samples of the current frame, in reverse order
  , msScheduled :: Map.Map Int (FftOutput s nfft (OutputWidth s nfft bw))
    -- ^ Output samples, by the cycle in which the IP outputs them
  , msBroken :: Maybe String
    -- ^ The input violated the requirements of the model
  }

{- | Cycle-accurate model of the IP, as configured by 'fftSynth', with latency
@d@: the number of clock cycles from the IP accepting the first sample of a
frame, while idle, to the first output sample of that frame.

We measured its timing by simulating the IP in Vivado's simulator:

* @s_axis_data_tready@ is deasserted from the cycle after @aresetn@ is
  asserted, until three cycles after the cycle in which it is deasserted again.
  Otherwise it is always asserted: the IP's output is never stalled.

* The processing core of the IP takes the first sample of a frame, accepted in
  cycle @a@, in cycle @a + 6@, unless it took the last sample of the previous
  frame in cycle @a + 4@ or later: it then takes it in the next cycle. The
  remaining samples of the frame must come in consecutive cycles; the
  processing core takes them in consecutive cycles too.

* The output sample with position @p@ in a frame comes out @d - 6 + p@ cycles
  after the processing core took the first sample of the frame.

* The configuration accompanying the first sample of a frame applies to that
  frame.

* Output samples that are on their way out when the reset is asserted still
  come out in the next cycle.
-}
fftModel ::
  forall d s nfft bw dom.
  (KnownFft s nfft bw, KnownNat d, HasCallStack) =>
  Config ->
  -- | Reset, active high
  Signal dom Bool ->
  Signal dom (FrameConfig s nfft) ->
  Signal dom (Maybe (Complex (Signed bw))) ->
  Signal dom (Bool, Maybe (FftOutput s nfft (OutputWidth s nfft bw)))
fftModel cfg@Config{outputOrdering} = go initState
 where
  n = natToNum @(2 ^ nfft) :: Int
  m = natToNum @nfft :: Int
  -- Cycles from the processing core taking the first sample of a frame to
  -- the first output sample
  coreLatency = natToNum @d - 6 :: Int

  initState =
    ModelState
      { msTime = 0
      , msInReset = True
      , msSinceReset = 0
      , msPrev = minBound `div` 2
      , msIndex = 0
      , msFrameStart = 0
      , msFrameConfig = errorX "No frame"
      , msFrame = []
      , msScheduled = Map.empty
      , msBroken = Nothing
      }

  resetState st =
    initState{msTime = msTime st, msInReset = True}

  go st (rst :- rsts) (fc :- fcs) (x :- xs) =
    out :- go st'' rsts fcs xs
   where
    t = msTime st
    (scheduled, rest) =
      case Map.lookup t (msScheduled st) of
        Nothing -> (Nothing, msScheduled st)
        Just o -> (Just o, Map.delete t (msScheduled st))
    outSample = case msBroken st of
      Nothing -> scheduled
      Just msg -> deepErrorX msg

    sinceReset = msSinceReset st + 1
    ready = not (msInReset st) && sinceReset > 2

    out = (ready, outSample)

    st'
      | msInReset st = resetState st
      | otherwise = accept st{msScheduled = rest, msSinceReset = sinceReset}

    st'' = st'{msTime = t + 1, msInReset = rst}

    accept s
      | not ready = s
      | Just v <- x = takeSample s v
      | msIndex s /= 0 =
          s{msBroken = Just $
              "Clash.Cores.Xilinx.Fft: the simulation model requires the samples "
                <> "of a frame to come in consecutive clock cycles, but sample "
                <> show (msIndex s) <> " of a frame did not come in cycle "
                <> show t <> ". The model's output is undefined from now on, "
                <> "until the next reset."}
      | otherwise = s

    takeSample s v
      | msIndex s == 0 =
          let c = if t + 5 <= msPrev s + 1 then msPrev s + 1 else t + 6
          in continue s{msPrev = c, msFrameStart = c, msFrameConfig = fc, msFrame = [v]}
      | otherwise =
          continue s{msPrev = msPrev s + 1, msFrame = v : msFrame s}

    continue s
      | msIndex s + 1 < n = s{msIndex = msIndex s + 1}
      | otherwise =
          s { msIndex = 0
            , msFrame = []
            , msScheduled = Map.union (msScheduled s) (Map.fromList (frameOutputs s))
            }

    frameOutputs s =
      [ ( msFrameStart s + coreLatency + p
        , FftOutput
            { index = fromIntegral k
            , value = if ovf then overflowed else resultMap Map.! k
            , lastSample = p == n - 1
            , status = mkStatus @s blkExp (ovf && p == n - 1)
            } )
      | p <- [0 .. n - 1]
      , let k = case outputOrdering of
                  NaturalOrder -> p
                  BitReversedOrder -> bitReverse m p
      ]
     where
      (results, blkExp, ovf) = fftFrame cfg (msFrameConfig s) (P.reverse (msFrame s))
      resultMap = Map.fromList (P.zip [0 ..] results)

  overflowed =
    deepErrorX "Clash.Cores.Xilinx.Fft: this frame overflowed, so the IP's output is unusable"
{-# OPAQUE fftModel #-}

------------------------------------------------------------------------------
-- Instantiating the IP
------------------------------------------------------------------------------

-- | Instantiate the IP, and the logic around it that counts the samples of a
-- frame and passes the configuration of a frame with its first sample. The
-- IP is configured as:
--
-- * /Pipelined, Streaming I\/O/ architecture, a single channel, a fixed
--   transform length, fixed-point data
-- * Non-realtime throttle scheme, with the output always ready
-- * No clock enable, a synchronous reset (@aresetn@)
-- * @XK_INDEX@, and @OVFLO@ for 'Scaled'
-- * No cyclic prefix insertion
fftSynth ::
  forall s nfft bw dom.
  (KnownDomain dom, KnownFft s nfft bw, HasCallStack) =>
  Config ->
  Clock dom ->
  Reset dom ->
  Signal dom (FrameConfig s nfft) ->
  Signal dom (Maybe (Complex (Signed bw))) ->
  (Signal dom Bool, Signal dom (Maybe (FftOutput s nfft (OutputWidth s nfft bw))))
fftSynth cfg clk rst frameCfg input = (dataReady, output)
 where
  dataReady = bitToBool <$> unPort dataTready
  accepted = (\r i -> r && isJust i) <$> dataReady <*> input

  -- Position of the next sample in its frame
  position :: Signal dom (Index (2 ^ nfft))
  position = register clk rst enableGen 0 (next <$> accepted <*> position)
  next acc p
    | acc = satSucc SatWrap p
    | otherwise = p

  firstSample = (\acc p -> acc && p == 0) <$> accepted <*> position

  configData =
    (\FrameConfig{direction, scaleSchedule} ->
        configWord @s @nfft (boolToBit (direction == Forward)) scaleSchedule)
      <$> frameCfg

  inputData = maybe 0 packSample <$> input
  packSample :: Complex (Signed bw) -> BitVector (2 * ByteWidth bw)
  packSample (re :+ im) = pack (resize im :: Signed (ByteWidth bw)) ++# pack (resize re :: Signed (ByteWidth bw))

  output = toOutput <$> bitToBool <$> unPort outTvalid <*> unPort outTdata <*> unPort outTuser <*> (bitToBool <$> unPort outTlast)

  toOutput ::
    Bool ->
    BitVector (2 * ByteWidth (OutputWidth s nfft bw)) ->
    BitVector (ByteWidth nfft + StatusWidth s) ->
    Bool ->
    Maybe (FftOutput s nfft (OutputWidth s nfft bw))
  toOutput False _ _ _ = Nothing
  toOutput True tdata tuser lst =
    Just FftOutput
      { index = unpack (resize xkIndex)
      , value = unpack (resize re) :+ unpack (resize im)
      , lastSample = lst
      , status = statusFromTuser @s statusBits lst
      }
   where
    (im, re) = split @_ @(ByteWidth (OutputWidth s nfft bw)) tdata
    (statusBits, xkIndex) = split @_ @(StatusWidth s) @(ByteWidth nfft) tuser

  ( _configTready, dataTready, outTdata, outTuser, outTvalid, outTlast ) =
    if hasStatusChannel @s then withStatus else withoutStatus

  withStatus ::
    ( Port "s_axis_config_tready" dom Bit
    , Port "s_axis_data_tready" dom Bit
    , Port "m_axis_data_tdata" dom (BitVector (2 * ByteWidth (OutputWidth s nfft bw)))
    , Port "m_axis_data_tuser" dom (BitVector (ByteWidth nfft + StatusWidth s))
    , Port "m_axis_data_tvalid" dom Bit
    , Port "m_axis_data_tlast" dom Bit
    )
  withStatus =
    instWithXilinxWizard
      (instConfig "fft")
      (fftWizard @s @nfft @bw cfg)
      (ClockPort @"aclk" clk)
      (ResetPort @"aresetn" @'ActiveLow rst)
      (Port @"s_axis_config_tdata" configData)
      (Port @"s_axis_config_tvalid" (boolToBit <$> firstSample))
      (Port @"s_axis_data_tdata" inputData)
      (Port @"s_axis_data_tvalid" (boolToBit . isJust <$> input))
      (Port @"s_axis_data_tlast" (pure 0 :: Signal dom Bit))
      (Port @"m_axis_data_tready" (pure 1 :: Signal dom Bit))
      (Port @"m_axis_status_tready" (pure 1 :: Signal dom Bit))

  withoutStatus ::
    ( Port "s_axis_config_tready" dom Bit
    , Port "s_axis_data_tready" dom Bit
    , Port "m_axis_data_tdata" dom (BitVector (2 * ByteWidth (OutputWidth s nfft bw)))
    , Port "m_axis_data_tuser" dom (BitVector (ByteWidth nfft + StatusWidth s))
    , Port "m_axis_data_tvalid" dom Bit
    , Port "m_axis_data_tlast" dom Bit
    )
  withoutStatus =
    instWithXilinxWizard
      (instConfig "fft")
      (fftWizard @s @nfft @bw cfg)
      (ClockPort @"aclk" clk)
      (ResetPort @"aresetn" @'ActiveLow rst)
      (Port @"s_axis_config_tdata" configData)
      (Port @"s_axis_config_tvalid" (boolToBit <$> firstSample))
      (Port @"s_axis_data_tdata" inputData)
      (Port @"s_axis_data_tvalid" (boolToBit . isJust <$> input))
      (Port @"s_axis_data_tlast" (pure 0 :: Signal dom Bit))
      (Port @"m_axis_data_tready" (pure 1 :: Signal dom Bit))
{-# INLINE fftSynth #-}

-- | Options of the Fast Fourier Transform IP, see 'fftSynth'
fftWizard ::
  forall s nfft bw.
  KnownFft s nfft bw =>
  Config ->
  XilinxWizard 20
fftWizard Config{..} =
  XilinxWizard
    { wiz_name = "xfft"
    , wiz_vendor = "xilinx.com"
    , wiz_library = "ip"
    , wiz_version = "9.1"
    , wiz_options =
           ("CONFIG.channels",                StrOpt "1")
        :> ("CONFIG.transform_length",        IntegerOpt (natToNum @(2 ^ nfft)))
        :> ("CONFIG.implementation_options",  StrOpt "pipelined_streaming_io")
        :> ("CONFIG.run_time_configurable_transform_length", BoolOpt False)
        :> ("CONFIG.data_format",             StrOpt "fixed_point")
        :> ("CONFIG.input_width",             IntegerOpt (natToNum @bw))
        :> ("CONFIG.phase_factor_width",      IntegerOpt (toInteger phaseFactorWidth))
        :> ("CONFIG.scaling_options",         StrOpt (scalingOption @s))
        :> ("CONFIG.rounding_modes",          StrOpt roundingOption)
        :> ("CONFIG.aclken",                  BoolOpt False)
        :> ("CONFIG.aresetn",                 BoolOpt True)
        :> ("CONFIG.ovflo",                   BoolOpt (scalingOption @s == "scaled"))
        :> ("CONFIG.xk_index",                BoolOpt True)
        :> ("CONFIG.throttle_scheme",         StrOpt "nonrealtime")
        :> ("CONFIG.output_ordering",         StrOpt orderingOption)
        :> ("CONFIG.cyclic_prefix_insertion", BoolOpt False)
        :> ("CONFIG.complex_mult_type",       StrOpt cmpyOption)
        :> ("CONFIG.butterfly_type",          StrOpt butterflyOption)
        :> ("CONFIG.memory_options_hybrid",   BoolOpt False)
        :> ("CONFIG.target_clock_frequency",  StrOpt "250")
        :> Nil
    }
 where
  roundingOption = case rounding of
    Truncation -> "truncation"
    ConvergentRounding -> "convergent_rounding"
  orderingOption = case outputOrdering of
    BitReversedOrder -> "bit_reversed_order"
    NaturalOrder -> "natural_order"
  cmpyOption = case complexMultiplier of
    CmpyLuts -> "use_luts"
    CmpyResources -> "use_mults_resources"
    CmpyPerformance -> "use_mults_performance"
  butterflyOption = case butterflyArithmetic of
    ButterflyLuts -> "use_luts"
    ButterflyDsps -> "use_xtremedsp_slices"
{-# INLINE fftWizard #-}
