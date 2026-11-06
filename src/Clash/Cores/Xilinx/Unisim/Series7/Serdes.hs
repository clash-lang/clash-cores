{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Input and output serializers/deserializers of 7 series devices (Artix-7,
Kintex-7, Virtex-7, Spartan-7 and Zynq-7000): @ISERDESE2@ and @OSERDESE2@ in
networking mode. For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/ISERDESE2)
and the /7 Series FPGAs SelectIO Resources/ user guide (UG471).

For the UltraScale counterparts see "Clash.Cores.Xilinx.Unisim.Serdes". The 7
series DDR registers (@IDDR@ and @ODDR@) are 'Clash.Xilinx.DDR.iddr' and
'Clash.Xilinx.DDR.oddr' in "Clash.Xilinx.DDR".

= Clocks and domains

A SERDES primitive involves three domains:

  * @domLine@, the domain of the serial signal. It has one sample per bit, so
    its period is the bit time.

  * @domClk@, the domain of the high-speed clock @CLK@. With a 'DDR' data rate,
    bits are transferred on both edges of this clock, so its period is twice
    the bit time. With an 'SDR' data rate, bits are transferred on its rising
    edges only, so its period is the bit time.

  * @domDiv@, the domain of the divided clock @CLKDIV@ and of the parallel
    data. Its period is /width/ times the bit time, where /width/ is the number
    of bits in a word.

These relations are enforced on the domains' periods. @CLK@ and @CLKDIV@ must
be phase aligned, for example by generating them with the same MMCM or PLL, or
with a @BUFIO@ and a @BUFR@ (UG471 lists the allowed clocking topologies). The
simulation models assume their rising edges coincide: they don't look at the
clocks, but map /width/ consecutive samples of the serial signal to one sample
of the parallel data.

The descriptions of the models count samples the way Clash's test bench clocks
and clash-prelude's DDR primitives ("Clash.Xilinx.DDR") do: sample 0 of every
domain precedes the first clock edge, and the first edges of all clocks
coincide. Hence sample /k/ (for /k/ > 0) of the parallel signal starts at the
same time as sample @width * (k - 1) + 1@ of the serial signal.

Words are represented as @'Vec' width 'Bit'@ ordered in time: the head of the
vector is the first bit on the serial line. The primitives use opposite bit
orders on their parallel ports (the first bit is @D1@ of an @OSERDESE2@, but
@Q8@ of an 8-bit @ISERDESE2@); the wrappers take care of that.

= Simulation

The UNISIM simulation models of these primitives are encrypted. The latencies,
bit order, reset behavior and bitslip behavior of the Clash models were
determined by simulating the UNISIM models in Vivado's simulator as black
boxes, with phase-aligned clocks, and are cycle-accurate under these
conditions:

  * The reset is asserted for at least two cycles of @CLKDIV@, as UG471
    prescribes. Shorter resets leave the primitives in states the models don't
    reproduce.

  * The primitives are reset before use, after their clocks are stable. At the
    start of a simulation, the UNISIM models hold their outputs at their initial
    values during the global set/reset (the first 100 ns); assert the reset at
    least until then. The Clash models output 0 before the first reset, where
    the UNISIM model of @ISERDESE2@ outputs undefined values when its clocks
    didn't run during the global set/reset (as with Clash's test bench clocks).

  * Inputs change after the clock edges at which they are sampled, as they do
    in hardware. Simulating Verilog UNISIM models from VHDL registers without a
    clock-to-output delay is prone to delta-cycle races.

= Not supported

Width expansion (@DATA_WIDTH@ 10 and 14, with a slave @SERDES_MODE@), the
@MEMORY@, @MEMORY_DDR3@, @MEMORY_QDR@ and @OVERSAMPLE@ interface types,
dynamic clock inversion, clock enables (@CE1@, @CE2@, @OCE@, @TCE@), the
@ISERDESE2@ feedback input (@OFB_USED@) and the 3-state path of @OSERDESE2@
(@T1@ to @T4@, @TQ@, @TFB@, @TBYTEIN@, @TBYTEOUT@) are not supported.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Cores.Xilinx.Unisim.Series7.Serdes
  ( -- * Deserializer
    iserdese2
  , Iserdese2Config (..)
  , iserdese2DefaultConfig
  , IserdesInput (..)
    -- * Serializer
  , oserdese2
  , Oserdese2Config (..)
  , oserdese2DefaultConfig
  , OserdesOutput (..)
    -- * Common
  , DataRate (..)
  , KnownDataRate (..)
  , BitsPerClock
  , SerdesWidth
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal ((:-)))
import Clash.Sized.Vector (unsafeFromList)

import Data.Kind (Constraint)
import qualified Data.List as L
import Data.Maybe (fromMaybe)

import Clash.Cores.Xilinx.Unisim.Internal (unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (ClockPort (..), Param (..), Port (..), ResetPort (..), inst, unPort)

-- | Data rate of a SERDES (@DATA_RATE@, @DATA_RATE_OQ@). Used as a type-level
-- parameter, e.g. @'iserdese2' \@DDR \@8@.
data DataRate
  = -- | Single data rate: one bit per cycle of @CLK@
    SDR
  | -- | Double data rate: one bit per edge of @CLK@
    DDR
  deriving (Show, Eq, Generic, NFDataX)

-- | Data rates known at the type level
class KnownDataRate (rate :: DataRate) where
  -- | The data rate as a value
  dataRateVal :: DataRate

instance KnownDataRate 'SDR where
  dataRateVal = SDR

instance KnownDataRate 'DDR where
  dataRateVal = DDR

-- | Number of bits transferred per cycle of @CLK@
type family BitsPerClock (rate :: DataRate) :: Nat where
  BitsPerClock 'SDR = 1
  BitsPerClock 'DDR = 2

{- | Supported word widths of the SERDES primitives (without width expansion):
2 to 8 for 'SDR', and 4, 6 and 8 for 'DDR'.
-}
type family SerdesWidth (rate :: DataRate) (width :: Nat) :: Constraint where
  SerdesWidth 'SDR 2 = ()
  SerdesWidth 'SDR 3 = ()
  SerdesWidth 'SDR 4 = ()
  SerdesWidth 'SDR 5 = ()
  SerdesWidth 'SDR 6 = ()
  SerdesWidth 'SDR 7 = ()
  SerdesWidth 'SDR 8 = ()
  SerdesWidth 'DDR 4 = ()
  SerdesWidth 'DDR 6 = ()
  SerdesWidth 'DDR 8 = ()
  SerdesWidth 'SDR n =
    TypeError ('Text "The data width of an SDR SERDES must be 2 to 8, not " ':<>: 'ShowType n)
  SerdesWidth 'DDR n =
    TypeError ('Text "The data width of a DDR SERDES must be 4, 6 or 8, not " ':<>: 'ShowType n)

dataRateString :: DataRate -> String
dataRateString = \case
  SDR -> "SDR"
  DDR -> "DDR"

-- | Where the serial input of an 'iserdese2' comes from
data IserdesInput
  = -- | An input buffer (@D@, @IOBDELAY = \"NONE\"@)
    InputD
  | -- | An 'Clash.Cores.Xilinx.Unisim.Series7.Delay.idelaye2' (@DDLY@,
    -- @IOBDELAY = \"IFD\"@)
    InputDdly
  deriving (Show, Eq, Generic, NFDataX)

-- | Configuration of 'iserdese2'
newtype Iserdese2Config = Iserdese2Config
  { iserdese2Input :: IserdesInput
  -- ^ Where the serial input comes from
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'iserdese2': input from an input buffer
iserdese2DefaultConfig :: Iserdese2Config
iserdese2DefaultConfig = Iserdese2Config{iserdese2Input = InputD}

-- | Timing of the 'iserdese2' simulation model. Time is counted in cycles of
-- @CLK@; cycle @clocks * k@ is at rising edge /k/ of @CLKDIV@.
data IserdesTiming = IserdesTiming
  { itClocks :: !Int
  -- ^ Cycles of @CLK@ per cycle of @CLKDIV@
  , itBits :: !Int
  -- ^ Bits per cycle of @CLK@
  , itSample :: !Int
  -- ^ Cycle (relative to a @CLKDIV@ edge) at which the parallel output samples
  -- the internal word register
  , itSlip :: !Int
  -- ^ Cycles after the @CLKDIV@ edge at which a bitslip or reset is sampled
  -- until it reaches the high-speed logic
  , itLatency :: !Int
  -- ^ Index of the first bit of a word loaded at cycle /t/, relative to
  -- @bits * t@
  , itResetEnd :: !Int
  -- ^ Cycles after the @CLKDIV@ edge following a reset sample until which the
  -- high-speed logic is held in reset
  }

iserdesTiming :: DataRate -> Int -> IserdesTiming
iserdesTiming rate width =
  IserdesTiming
    { itClocks = clocks
    , itBits = bits
    , itSample = max 0 (clocks - 3)
    , itSlip = if clocks >= 3 then 2 * clocks - 2 else 3
    , itLatency = latency
    , itResetEnd = if clocks >= 3 then 2 * clocks + 1 else 7
    }
 where
  bits = case rate of SDR -> 1; DDR -> 2
  clocks = width `div` bits
  latency = case (rate, width) of
    (SDR, 2) -> 5
    (SDR, _) -> 3 * width - 2
    (DDR, 4) -> 11
    (DDR, 6) -> 15
    (DDR, _) -> 21

-- | State of the high-speed logic in the 'iserdese2' model
data IserdesState = IserdesState
  { isCount :: !Int
  -- ^ Counter that loads a word into the word register when it is 0
  , isHalf :: !Int
  -- ^ DDR only: whether words are taken one bit earlier (bitslip)
  , isWord :: [Bit]
  -- ^ Word register
  , isInReset :: !Bool
  -- ^ Whether the previous cycle was in reset
  , isHolds :: [Int]
  -- ^ Cycles at which the counter holds (bitslip)
  , isDeferred :: Maybe Int
  -- ^ A hold deferred by a cycle because it coincided with a load
  , isHalfChanges :: [(Int, Int)]
  -- ^ Cycles at which 'isHalf' changes, and the new value
  , isResets :: [(Int, Int)]
  -- ^ Intervals of cycles in reset
  , isSlipHalf :: !Int
  -- ^ Value of 'isHalf' after all scheduled changes
  }

{- | Simulation model of 'iserdese2'. The high-speed logic is simulated per
cycle of @CLK@: a counter loads a word into a register once per cycle of
@CLKDIV@, and the parallel output samples that register. A bitslip makes the
counter hold for a cycle, so that words are loaded one bit (SDR) later, or, in
DDR mode, alternately takes words one bit earlier and makes the counter hold.
When the load moves past the point where the output samples the register, a
word is output twice. A reset realigns the counter.
-}
iserdesModel ::
  forall width domLine domDiv.
  (KnownNat width, KnownDomain domDiv) =>
  DataRate ->
  Reset domDiv ->
  Signal domDiv Bool ->
  Signal domLine Bit ->
  Signal domDiv (Vec width Bit)
iserdesModel rate rst slip0 line0 =
  -- Sample 0 of the inputs precedes the first clock edge; the output follows
  -- the edges a sample later, as registers do
  go 0 st0 [] True Nothing (dropS (unsafeToActiveHigh rst)) (dropS slip0) (dropS line0)
 where
  dropS (_ :- xs) = xs
  width = natToNum @width :: Int
  IserdesTiming clocks bits sampleAt slipAt latency resetEnd = iserdesTiming rate width
  zeros = L.replicate width 0
  st0 = IserdesState 0 0 zeros False [] Nothing [] [] 0
  -- Bits of the serial line kept: enough for the loads of one CLKDIV cycle
  histLen = latency + width + 2

  -- Step /k/: @prev2@ is the reset at sample @k - 2@, @mPrev@ the reset and
  -- bitslip at sample @k - 1@, @hist@ the serial samples before @width * k@ in
  -- reverse order.
  go ::
    Int ->
    IserdesState ->
    [Bit] ->
    Bool ->
    Maybe (Bool, Bool) ->
    Signal domDiv Bool ->
    Signal domDiv Bool ->
    Signal domLine Bit ->
    Signal domDiv (Vec width Bit)
  go !k st hist prev2 mPrev rs ss line = out :- rest
   where
    st1 = case mPrev of
      Nothing -> st
      Just (r, s) -> schedule (k - 1) prev2 r s st
    (st2, word) = runCycles k hist st1
    rPrev = maybe True fst mPrev
    out
      | clocks >= 3 && prev2 = repeat 0
      | otherwise = unsafeFromList word
    rest = case (rs, ss) of
      (r :- rs1, s :- ss1) ->
        let (new, line1) = takeSignal width line
            hist1 = L.take histLen (L.reverse new L.++ hist)
         in go (k + 1) (prune (clocks * (k + 1)) st2) hist1 rPrev (Just (r, s)) rs1 ss1 line1

  -- Bit with index /i/ of the serial line, given the history at step /k/
  bitAt :: Int -> [Bit] -> Int -> Bit
  bitAt k hist i
    | i < 0 = 0
    | otherwise = fromMaybe 0 (listToMaybe' (L.drop (width * k - 1 - i) hist))

  listToMaybe' (x : _) = Just x
  listToMaybe' [] = Nothing

  -- Handle the reset and bitslip of sample /j/
  schedule :: Int -> Bool -> Bool -> Bool -> IserdesState -> IserdesState
  schedule j rPrev r s st
    | r =
        st
          { isResets = isResets st L.++ [(start, clocks * (j + 1) + resetEnd)]
          , isSlipHalf = 0
          , isHolds = L.filter (< start) (isHolds st)
          , isHalfChanges = L.filter ((< start) . fst) (isHalfChanges st)
          }
    | s && not rPrev = case rate of
        SDR -> st{isHolds = isHolds st L.++ [slipTime]}
        DDR
          | isSlipHalf st == 0 ->
              st{isHalfChanges = isHalfChanges st L.++ [(slipTime, 1)], isSlipHalf = 1}
          | otherwise ->
              st
                { isHalfChanges = isHalfChanges st L.++ [(slipTime, 0)]
                , isSlipHalf = 0
                , isHolds = isHolds st L.++ [slipTime]
                }
    | otherwise = st
   where
    start = clocks * j + slipAt
    slipTime = clocks * (j + 1) + slipAt

  -- Simulate the CLK cycles of step /k/; returns the word sampled by the output
  runCycles :: Int -> [Bit] -> IserdesState -> (IserdesState, [Bit])
  runCycles k hist st0' = goC 0 st0' zeros
   where
    goC !i st w
      | i == clocks = (st, w)
      | otherwise = goC (i + 1) st' (if i == sampleAt then isWord st' else w)
     where
      st' = cycle1 (clocks * k + i) st

    cycle1 :: Int -> IserdesState -> IserdesState
    cycle1 t st
      | L.any (\(a, b) -> a <= t && t < b) (isResets st) =
          let rstWord
                | clocks == 2 = zeros
                | not (isInReset st) && isCount st == 0 = zeros
                | otherwise = isWord st
           in st
                { isCount = (t + 1) `mod` clocks
                , isHalf = 0
                , isWord = rstWord
                , isInReset = True
                }
      | otherwise =
          st
            { isCount = count1
            , isHalf = half
            , isWord = word
            , isInReset = False
            , isDeferred = deferred1
            }
     where
      half = fromMaybe (isHalf st) (L.lookup t (isHalfChanges st))
      word
        | isCount st == 0 =
            [bitAt k hist (bits * t - half - latency + n) | n <- [0 .. width - 1]]
        | otherwise = isWord st
      hold = t `L.elem` isHolds st || isDeferred st == Just t
      next = (isCount st + 1) `mod` clocks
      (count1, deferred1)
        | hold && isCount st == 0 = (next, Just (t + 1))
        | hold = (isCount st, Nothing)
        | otherwise = (next, isDeferred st)

  -- Forget events before cycle /t/
  prune :: Int -> IserdesState -> IserdesState
  prune t st =
    st
      { isHolds = L.filter (>= t) (isHolds st)
      , isHalfChanges = L.filter ((>= t) . fst) (isHalfChanges st)
      , isResets = L.filter ((> t) . snd) (isResets st)
      , isDeferred = if maybe False (< t) (isDeferred st) then Nothing else isDeferred st
      }

{- | Input deserializer (@ISERDESE2@ in @NETWORKING@ mode): deserializes a
serial signal into words of /width/ bits, at the given data rate. Example:

> words = iserdese2 @DDR @8 iserdese2DefaultConfig clk clkDiv rst bitslip serial

Let @b0, b1, b2, ...@ be the samples of the serial signal. After a reset,
sample /k/ of the output is @b(width * k - L), ..., b(width * k - L + width - 1)@,
where the latency /L/ is:

+-----------+---+----+----+----+----+----+----+
| width     | 2 | 3  | 4  | 5  | 6  | 7  | 8  |
+===========+===+====+====+====+====+====+====+
| 'SDR'     | 4 | 6  | 9  | 12 | 15 | 18 | 21 |
+-----------+---+----+----+----+----+----+----+
| 'DDR'     |   |    | 10 |    | 14 |    | 20 |
+-----------+---+----+----+----+----+----+----+

Words aren't aligned to @CLKDIV@; use the bitslip to align them to word
boundaries in the data, for example with a training pattern.

__Bitslip__ (@BITSLIP@): asserting the bitslip for one cycle of @CLKDIV@ shifts
the word boundary. In 'SDR' mode, each bitslip delays the word boundary by one
bit: the word starts one bit later. In 'DDR' mode, bitslips alternately move
the word boundary one bit earlier and three bits later (UG471: \"shift right by
one and shift left by three\"). After /width/ bitslips, the boundary is back
where it started. A bitslip in sample /j/ shows on the output from sample
@j + 2@ or @j + 3@ on, depending on the bitslip state. When the boundary moves past the point where words
are handed over to @CLKDIV@, a word is output twice (or, in 'DDR' mode, once
every /width/ bitslips, skipped). Don't assert the bitslip in two consecutive
cycles. Bitslips during a reset or in the cycle after it are ignored.

__Reset__ (@RST@): active-high and asynchronous, it must be deasserted
synchronously to @CLKDIV@ and be asserted for at least two cycles of
@CLKDIV@. It resets the bitslip logic. Let the reset be asserted from sample
/s/ up to and including sample /e/. Then:

  * Width 2 'SDR' and width 4 'DDR' (two @CLK@ cycles per @CLKDIV@ cycle):
    samples @s + 1@ up to and including @e + 3@ are 0. If a bitslip follows the
    reset closely, the word output twice can be 0 too.

  * Other widths: samples @s + 1@ up to and including @e + 1@ are 0, and sample
    @e + 2@ repeats sample /s/ (or is 0, for some bitslip states).

From then on, the output follows the description above with the word boundary
in its initial position.

@CLKB@ is connected to @CLK@ and inverted in the primitive
(@IS_CLKB_INVERTED = 1@). The initial and reset values of the input registers
(@INIT_Q1@ to @INIT_Q4@, @SRVAL_Q1@ to @SRVAL_Q4@) are 0. The combinational
output (@O@) isn't exposed.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/ISERDESE2)
and UG471.
-}
iserdese2 ::
  forall rate width domLine domClk domDiv.
  ( KnownDataRate rate
  , KnownNat width
  , SerdesWidth rate width
  , KnownDomain domLine
  , KnownDomain domClk
  , KnownDomain domDiv
  , DomainPeriod domClk ~ (BitsPerClock rate * DomainPeriod domLine)
  , DomainPeriod domDiv ~ (width * DomainPeriod domLine)
  ) =>
  Iserdese2Config ->
  -- | High-speed clock (@CLK@)
  Clock domClk ->
  -- | Divided clock (@CLKDIV@)
  Clock domDiv ->
  -- | Reset (@RST@)
  Reset domDiv ->
  -- | Bitslip (@BITSLIP@)
  Signal domDiv Bool ->
  -- | Serial input (@D@ or @DDLY@)
  Signal domLine Bit ->
  -- | Words, ordered in time
  Signal domDiv (Vec width Bit)
iserdese2 config clk clkDiv rst bitslip d
  | clashSimulation = iserdesModel (dataRateVal @rate) rst bitslip d
  | otherwise = toWord <$> bundle qs
 where
  width = natToNum @width :: Int
  zero = pure 0 :: Signal domLine Bit
  (dPort, ddlyPort, iobDelay) = case iserdese2Input config of
    InputD -> (d, zero, "NONE")
    InputDdly -> (zero, d, "IFD")

  -- The first bit is on Q<width>
  toWord :: Vec 8 Bit -> Vec width Bit
  toWord q = map (\i -> q !! (width - 1 - fromEnum i)) indicesI

  qs :: Vec 8 (Signal domDiv Bit)
  qs = unPort q1 :> unPort q2 :> unPort q3 :> unPort q4 :> unPort q5 :> unPort q6
    :> unPort q7 :> unPort q8 :> Nil

  (q1, q2, q3, q4, q5, q6, q7, q8, _o, _shiftOut1, _shiftOut2) = go
  go ::
    ( Port "Q1" domDiv Bit
    , Port "Q2" domDiv Bit
    , Port "Q3" domDiv Bit
    , Port "Q4" domDiv Bit
    , Port "Q5" domDiv Bit
    , Port "Q6" domDiv Bit
    , Port "Q7" domDiv Bit
    , Port "Q8" domDiv Bit
    , Port "O" domLine Bit
    , Port "SHIFTOUT1" domDiv Bit
    , Port "SHIFTOUT2" domDiv Bit
    )
  go =
    inst
      (unisim "ISERDESE2")
      (Param @"DATA_RATE" @String (dataRateString (dataRateVal @rate)))
      (Param @"DATA_WIDTH" @Integer (toInteger width))
      (Param @"DYN_CLKDIV_INV_EN" @String "FALSE")
      (Param @"DYN_CLK_INV_EN" @String "FALSE")
      (Param @"INIT_Q1" @Bit 0)
      (Param @"INIT_Q2" @Bit 0)
      (Param @"INIT_Q3" @Bit 0)
      (Param @"INIT_Q4" @Bit 0)
      (Param @"INTERFACE_TYPE" @String "NETWORKING")
      (Param @"IOBDELAY" @String iobDelay)
      (Param @"IS_CLKB_INVERTED" @Bit 1)
      (Param @"IS_CLKDIVP_INVERTED" @Bit 0)
      (Param @"IS_CLKDIV_INVERTED" @Bit 0)
      (Param @"IS_CLK_INVERTED" @Bit 0)
      (Param @"IS_D_INVERTED" @Bit 0)
      (Param @"IS_OCLKB_INVERTED" @Bit 0)
      (Param @"IS_OCLK_INVERTED" @Bit 0)
      (Param @"NUM_CE" @Integer 1)
      (Param @"OFB_USED" @String "FALSE")
      (Param @"SERDES_MODE" @String "MASTER")
      (Param @"SRVAL_Q1" @Bit 0)
      (Param @"SRVAL_Q2" @Bit 0)
      (Param @"SRVAL_Q3" @Bit 0)
      (Param @"SRVAL_Q4" @Bit 0)
      (Port @"BITSLIP" (boolToBit <$> bitslip))
      (Port @"CE1" (pure 1 :: Signal domDiv Bit))
      (Port @"CE2" (pure 1 :: Signal domDiv Bit))
      (ClockPort @"CLK" clk)
      (ClockPort @"CLKB" clk)
      (ClockPort @"CLKDIV" clkDiv)
      (Port @"CLKDIVP" (pure 0 :: Signal domDiv Bit))
      (Port @"D" dPort)
      (Port @"DDLY" ddlyPort)
      (Port @"DYNCLKDIVSEL" (pure 0 :: Signal domDiv Bit))
      (Port @"DYNCLKSEL" (pure 0 :: Signal domDiv Bit))
      (Port @"OCLK" (pure 0 :: Signal domClk Bit))
      (Port @"OCLKB" (pure 0 :: Signal domClk Bit))
      (Port @"OFB" zero)
      (ResetPort @"RST" @'ActiveHigh rst)
      (Port @"SHIFTIN1" (pure 0 :: Signal domDiv Bit))
      (Port @"SHIFTIN2" (pure 0 :: Signal domDiv Bit))

-- | Which output of an 'oserdese2' carries the serial data
data OserdesOutput
  = -- | To an output buffer (@OQ@)
    OutputOq
  | -- | To an 'Clash.Cores.Xilinx.Unisim.Series7.Delay.odelaye2' or the
    -- feedback input of an @ISERDESE2@ (@OFB@)
    OutputOfb
  deriving (Show, Eq, Generic, NFDataX)

-- | Configuration of 'oserdese2'
data Oserdese2Config = Oserdese2Config
  { oserdese2ResetValue :: Bit
  -- ^ Value of the serial output while the primitive is in reset, and at the
  -- start of a simulation (@SRVAL_OQ@, @INIT_OQ@)
  , oserdese2Output :: OserdesOutput
  -- ^ Which output carries the serial data
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'oserdese2': a serial output of 0 during reset,
-- to an output buffer
oserdese2DefaultConfig :: Oserdese2Config
oserdese2DefaultConfig =
  Oserdese2Config
    { oserdese2ResetValue = 0
    , oserdese2Output = OutputOq
    }

{- | Output serializer (@OSERDESE2@): serializes words of /width/ bits onto a
serial signal, at the given data rate. Example:

> serial = oserdese2 @SDR @4 oserdese2DefaultConfig clk clkDiv rst words

Let @w(k)@ be sample /k/ of the input. Then bit /i/ of @w(k)@ is sample
@width * k + L + i@ of the serial output, where the latency /L/ is:

+-----------+---+----+----+----+----+----+----+
| width     | 2 | 3  | 4  | 5  | 6  | 7  | 8  |
+===========+===+====+====+====+====+====+====+
| 'SDR'     | 2 | 4  | 5  | 5  | 6  | 6  | 7  |
+-----------+---+----+----+----+----+----+----+
| 'DDR'     |   |    | 3  |    | 7  |    | 9  |
+-----------+---+----+----+----+----+----+----+

__Reset__ (@RST@): active-high and asynchronous, it must be deasserted
synchronously to @CLKDIV@ and be asserted for at least two cycles of
@CLKDIV@. Let the reset be asserted from sample /s/ of @CLKDIV@ up to and
including sample /e/. Then serial samples @width * (s - 1) + 2@ up to and
including @width * e + 2@ are 'oserdese2ResetValue', and the samples after
that are 0 up to the first bit of word @e + 1@ (width 2 'SDR' and width 4
'DDR': word @e + 2@), which is output as described above.

The output takes 'oserdese2ResetValue' at the start of the simulation; reset
the primitive before use.

For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/OSERDESE2)
and UG471.
-}
oserdese2 ::
  forall rate width domLine domClk domDiv.
  ( KnownDataRate rate
  , KnownNat width
  , SerdesWidth rate width
  , KnownDomain domLine
  , KnownDomain domClk
  , KnownDomain domDiv
  , DomainPeriod domClk ~ (BitsPerClock rate * DomainPeriod domLine)
  , DomainPeriod domDiv ~ (width * DomainPeriod domLine)
  ) =>
  Oserdese2Config ->
  -- | High-speed clock (@CLK@)
  Clock domClk ->
  -- | Divided clock (@CLKDIV@)
  Clock domDiv ->
  -- | Reset (@RST@)
  Reset domDiv ->
  -- | Words, ordered in time (@D1@ to @D8@)
  Signal domDiv (Vec width Bit) ->
  -- | Serial output (@OQ@ or @OFB@)
  Signal domLine Bit
oserdese2 config clk clkDiv rst ws
  | clashSimulation = oserdesModel (dataRateVal @rate) (oserdese2ResetValue config) rst ws
  | otherwise = case oserdese2Output config of
      OutputOq -> unPort oq
      OutputOfb -> unPort ofb
 where
  width = natToNum @width :: Int
  rate = dataRateVal @rate
  rstVal = oserdese2ResetValue config

  -- D1 is the first bit, unused inputs are 0
  ds :: Vec 8 (Signal domDiv Bit)
  ds = unbundle (toD <$> ws)
  toD :: Vec width Bit -> Vec 8 Bit
  toD w = map (\i -> if fromEnum i < width then w !! min (fromEnum i) (width - 1) else 0) indicesI

  -- The 3-state path isn't supported. Its configuration follows UG471's table of
  -- valid combinations.
  (rateTq, tristateWidth) = case (rate, width) of
    (DDR, 4) -> ("DDR", 4)
    _ -> ("SDR", 1 :: Integer)

  (oq, ofb, _tq, _tfb, _tbyteOut, _shiftOut1, _shiftOut2) = go
  go ::
    ( Port "OQ" domLine Bit
    , Port "OFB" domLine Bit
    , Port "TQ" domLine Bit
    , Port "TFB" domLine Bit
    , Port "TBYTEOUT" domLine Bit
    , Port "SHIFTOUT1" domDiv Bit
    , Port "SHIFTOUT2" domDiv Bit
    )
  go =
    inst
      (unisim "OSERDESE2")
      (Param @"DATA_RATE_OQ" @String (dataRateString rate))
      (Param @"DATA_RATE_TQ" @String rateTq)
      (Param @"DATA_WIDTH" @Integer (toInteger width))
      (Param @"INIT_OQ" @Bit rstVal)
      (Param @"INIT_TQ" @Bit 0)
      (Param @"SERDES_MODE" @String "MASTER")
      (Param @"SRVAL_OQ" @Bit rstVal)
      (Param @"SRVAL_TQ" @Bit 0)
      (Param @"TBYTE_CTL" @String "FALSE")
      (Param @"TBYTE_SRC" @String "FALSE")
      (Param @"TRISTATE_WIDTH" @Integer tristateWidth)
      (ClockPort @"CLK" clk)
      (ClockPort @"CLKDIV" clkDiv)
      (Port @"D1" (ds !! (0 :: Int)))
      (Port @"D2" (ds !! (1 :: Int)))
      (Port @"D3" (ds !! (2 :: Int)))
      (Port @"D4" (ds !! (3 :: Int)))
      (Port @"D5" (ds !! (4 :: Int)))
      (Port @"D6" (ds !! (5 :: Int)))
      (Port @"D7" (ds !! (6 :: Int)))
      (Port @"D8" (ds !! (7 :: Int)))
      (Port @"OCE" (pure 1 :: Signal domDiv Bit))
      (ResetPort @"RST" @'ActiveHigh rst)
      (Port @"SHIFTIN1" (pure 0 :: Signal domDiv Bit))
      (Port @"SHIFTIN2" (pure 0 :: Signal domDiv Bit))
      (Port @"T1" (pure 0 :: Signal domDiv Bit))
      (Port @"T2" (pure 0 :: Signal domDiv Bit))
      (Port @"T3" (pure 0 :: Signal domDiv Bit))
      (Port @"T4" (pure 0 :: Signal domDiv Bit))
      (Port @"TBYTEIN" (pure 0 :: Signal domDiv Bit))
      (Port @"TCE" (pure 0 :: Signal domDiv Bit))

-- | Latency of 'oserdese2': the serial sample (relative to the @CLKDIV@ edge at
-- which a word is presented) carrying its first bit
oserdesLatency :: DataRate -> Int -> Int
oserdesLatency rate width = case (rate, width) of
  (SDR, 2) -> 3
  (SDR, 3) -> 6
  (SDR, 4) -> 8
  (SDR, 5) -> 9
  (SDR, 6) -> 11
  (SDR, 7) -> 12
  (SDR, _) -> 14
  (DDR, 4) -> 6
  (DDR, 6) -> 12
  (DDR, _) -> 16

{- | Simulation model of 'oserdese2'. At step /k/ it outputs serial samples
@width * k@ up to @width * k + width - 1@, using the words of samples @k - 1@
and @k - 2@, the reset of sample @k - 1@, the reset of sample /k/ (for all but
the first serial sample: the reset is asynchronous) and the last sample before
/k/ at which the reset was asserted.
-}
oserdesModel ::
  forall width domLine domDiv.
  (KnownNat width, KnownDomain domDiv) =>
  DataRate ->
  -- | Reset value
  Bit ->
  Reset domDiv ->
  Signal domDiv (Vec width Bit) ->
  Signal domLine Bit
oserdesModel rate rstVal rst ws0 =
  -- Sample 0 precedes the first clock edge
  rstVal :- go 0 (-1) True zeros zeros (dropS (unsafeToActiveHigh rst)) (dropS ws0)
 where
  dropS (_ :- xs) = xs
  width = natToNum @width :: Int
  latency = oserdesLatency rate width
  zeros = L.replicate width 0
  twoClocks = case rate of SDR -> width == 2; DDR -> width == 4

  -- @lastRst@: the last sample before /k/ at which the reset was asserted,
  -- @rPrev@: whether that is @k - 1@, @w1@ and @w2@: the words of samples
  -- @k - 1@ and @k - 2@
  go ::
    Int -> Int -> Bool -> [Bit] -> [Bit] ->
    Signal domDiv Bool -> Signal domDiv (Vec width Bit) -> Signal domLine Bit
  go !k !lastRst rPrev w1 w2 (r :- rs) ~(w :- ws) =
    prependSignal (L.map serialSample [0 .. width - 1]) $
      go (k + 1) (if r then k else lastRst) r (toList w) w1 rs ws
   where
    firstValid = (if twoClocks then lastRst + 2 else lastRst + 1) * width + latency
    serialSample i
      | i >= 1 && r = rstVal
      | i <= 1 && rPrev = rstVal
      | n >= (lastRst + 1) * width + 2 && n < firstValid = 0
      | otherwise = dataBit (n - latency)
     where
      n = width * k + i

    -- Bit /m/ of the concatenated words; only words @k - 1@ and @k - 2@ are
    -- needed
    dataBit m
      | m < 0 = 0
      | otherwise =
          let (j, b) = m `divMod` width
           in if j == k - 1
                then w1 L.!! b
                else if j == k - 2 then w2 L.!! b else 0

-- | Take the first /n/ samples of a signal, and return the rest
takeSignal :: Int -> Signal dom a -> ([a], Signal dom a)
takeSignal n0 xs0 = go n0 [] xs0
 where
  go 0 acc xs = (L.reverse acc, xs)
  go i acc (x :- xs) = go (i - 1) (x : acc) xs

-- | Prepend the values of a list to a signal
prependSignal :: [a] -> Signal dom a -> Signal dom a
prependSignal xs s = L.foldr (:-) s xs
