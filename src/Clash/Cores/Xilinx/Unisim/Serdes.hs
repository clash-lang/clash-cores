{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Input and output serializers/deserializers of UltraScale and UltraScale+
devices (@ISERDESE3@ and @OSERDESE3@ in component mode). For
more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/I/O-Components)
and the /UltraScale Architecture SelectIO Resources/ user guide (UG571).

= Clocks and domains

A SERDES primitive involves three domains:

  * @domLine@, the domain of the serial signal. It has one sample per bit, so
    its period is the bit time.

  * @domClk@, the domain of the high-speed clock @CLK@. Bits are transferred
    on both edges of this clock, so its period is twice the bit time.

  * @domDiv@, the domain of the divided clock @CLKDIV@ and of the parallel
    data. Its period is /width/ times the bit time, where /width/ (4 or 8) is
    the number of bits in a word.

These relations are enforced on the domains' periods. @CLK@ and @CLKDIV@ must
be phase aligned, for example by generating them with two 'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgceDiv's
fed by the same clock (UG571 lists the allowed clocking topologies). The
simulation models assume this alignment: they don't look at the clocks, but
map /width/ consecutive samples of the serial signal to one sample of the
parallel data.

Words are represented as @'Vec' width 'Bit'@ ordered in time: the head of the
vector is the first bit on the serial line. Note that this is the reverse of
the primitive's ports, where bit 0 of @Q@ and @D@ is the first bit, and that
'pack' turns the head of a vector into the most significant bit.

The latencies and the behavior during and after a reset are those of the UNISIM
simulation models of the primitives, which are the same for UltraScale and
UltraScale+ devices.
-}
module Clash.Cores.Xilinx.Unisim.Serdes
  ( -- * Deserializer
    iserdese3
    -- * Serializer
  , oserdese3
  , Oserdese3Config (..)
  , oserdese3DefaultConfig
    -- * Common
  , SerdesWidth
  , SimDevice (..)
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal ((:-)))
import Clash.Sized.Vector (unsafeFromList)

import Data.Kind (Constraint)
import qualified Data.List as L

import Clash.Cores.Xilinx.Unisim.Internal (SimDevice (..), simDeviceString, unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (ClockPort (..), Param (..), Port (..), ResetPort (..), inst, unPort)

-- | Supported word widths of the SERDES primitives: 4 and 8
type family SerdesWidth (width :: Nat) :: Constraint where
  SerdesWidth 4 = ()
  SerdesWidth 8 = ()
  SerdesWidth n =
    TypeError ('Text "The data width of a SERDES must be 4 or 8, not " ':<>: 'ShowType n)

{- | Input deserializer (@ISERDESE3@): deserializes
a serial signal sampled on both edges of @CLK@ into words of /width/ bits.

Let @b0, b1, b2, ...@ be the samples of the serial signal, where sample @b(width * k)@
is at rising edge /k/ of @CLKDIV@. Then sample /k/ of the output is
@b(width * (k - 2) + 1), ..., b(width * (k - 1))@: a word consists of the bits
received after one edge of @CLKDIV@ up to and including the next one, and is
output two cycles of @CLKDIV@ after its first bit.

The reset (@RST@) is active-high, asynchronous, and must be deasserted
synchronously to @CLKDIV@. While it is asserted, and for 5 (width 4) or 4
(width 8) cycles of @CLKDIV@ after it is deasserted, the output is 0. Reset the
primitive after its clocks are stable, as UG571 prescribes.

The primitive's FIFO is disabled (@FIFO_ENABLE = \"FALSE\"@), @CLK_B@ is
connected to @CLK@ and inverted in the primitive (@IS_CLK_B_INVERTED = 1@).
@IDDR_MODE@ is not supported; use 'Clash.Cores.Xilinx.Unisim.Ddr.iddre1' instead.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/ISERDESE3).
-}
iserdese3 ::
  forall width domLine domClk domDiv.
  ( KnownNat width
  , SerdesWidth width
  , KnownDomain domLine
  , KnownDomain domClk
  , KnownDomain domDiv
  , DomainPeriod domClk ~ (2 * DomainPeriod domLine)
  , DomainPeriod domDiv ~ (width * DomainPeriod domLine)
  ) =>
  -- | Device family (@SIM_DEVICE@)
  SimDevice ->
  -- | High-speed clock (@CLK@)
  Clock domClk ->
  -- | Divided clock (@CLKDIV@)
  Clock domDiv ->
  -- | Reset (@RST@)
  Reset domDiv ->
  -- | Serial input (@D@)
  Signal domLine Bit ->
  -- | Words, ordered in time
  Signal domDiv (Vec width Bit)
iserdese3 simDevice clk clkDiv rst d
  | clashSimulation = sim
  | otherwise = toWord . unpack . resize <$> unPort q
 where
  width = natToNum @width :: Int

  -- Number of cycles after a reset in which the output is still 0
  inReset
    | width == 4 = resetWindow d5 clkDiv rst
    | otherwise = resetWindow d4 clkDiv rst

  sim =
    mux
      inReset
      (pure (repeat 0))
      (delay clkDiv enableGen (repeat 0) (delay clkDiv enableGen (repeat 0) (toWords d)))

  -- Words of the bits received after a CLKDIV edge up to and including the
  -- next one
  toWords :: Signal domLine Bit -> Signal domDiv (Vec width Bit)
  toWords (_ :- bs) = goWords bs
   where
    goWords xs = word :- goWords rest
     where
      (word, rest) = takeSignal xs

  -- Q[0] is the first bit, 'unpack' puts it at the end of the vector
  toWord :: Vec width Bit -> Vec width Bit
  toWord = reverse

  (q, _fifoEmpty, _internalDivClk) = go
  go ::
    ( Port "Q" domDiv (BitVector 8)
    , Port "FIFO_EMPTY" domDiv Bit
    , Port "INTERNAL_DIVCLK" domDiv Bit
    )
  go =
    inst
      (unisim "ISERDESE3")
      (Param @"DATA_WIDTH" @Integer (natToNum @width))
      (Param @"FIFO_ENABLE" @String "FALSE")
      (Param @"FIFO_SYNC_MODE" @String "FALSE")
      (Param @"IDDR_MODE" @String "FALSE")
      (Param @"IS_CLK_B_INVERTED" @Bit 1)
      (Param @"IS_CLK_INVERTED" @Bit 0)
      (Param @"IS_RST_INVERTED" @Bit 0)
      (Param @"SIM_DEVICE" @String (simDeviceString simDevice))
      (ClockPort @"CLK" clk)
      (ClockPort @"CLK_B" clk)
      (ClockPort @"CLKDIV" clkDiv)
      (Port @"D" d)
      (ClockPort @"FIFO_RD_CLK" clkDiv)
      (Port @"FIFO_RD_EN" (pure 0 :: Signal domDiv Bit))
      (ResetPort @"RST" @'ActiveHigh rst)

-- | Configuration of 'oserdese3'
data Oserdese3Config = Oserdese3Config
  { oserdese3SimDevice :: SimDevice
  -- ^ Device family (@SIM_DEVICE@)
  , oserdese3Init :: Bit
  -- ^ Value of the serial output while the primitive is in reset (@INIT@)
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'oserdese3': an UltraScale device, and a serial
-- output of 0 during reset
oserdese3DefaultConfig :: Oserdese3Config
oserdese3DefaultConfig =
  Oserdese3Config
    { oserdese3SimDevice = UltraScale
    , oserdese3Init = 0
    }

{- | Output serializer (@OSERDESE3@): serializes words of /width/ bits onto a
serial signal, transmitting on both edges of @CLK@.

Let @w(k)@ be sample /k/ of the input, the word at rising edge /k/ of
@CLKDIV@, and let sample @width * k@ of the serial output be the one at that
edge. Then bit /i/ of @w(k)@ is sample @width * k + width - 1 + i@ of the serial
output: the first bit of a word is output /width - 1/ bit times after the
@CLKDIV@ edge at which the word is sampled.

The reset (@RST@) is active-high, asynchronous, and must be deasserted
synchronously to @CLKDIV@. The serial output takes the value @INIT@ as soon as
the reset is asserted (truncating the word being transmitted), and keeps it
until the word sampled 4 cycles of @CLKDIV@ after the reset is deasserted. Put
in terms of samples: if the reset is asserted at sample /m/ of @CLKDIV@, bits
2 and up of word @m - 2@, and all bits of words @m - 1@ up to and including
@m + 3@, are replaced by @INIT@. Reset the primitive after its clocks are
stable, as UG571 prescribes.

The 3-state path (@T@, @T_OUT@) is not supported: @T@ is tied to 0. Neither
are @ODDR_MODE@ and the bypass modes; use 'Clash.Cores.Xilinx.Unisim.Ddr.oddre1'
instead of @ODDR_MODE@.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/OSERDESE3).
-}
oserdese3 ::
  forall width domLine domClk domDiv.
  ( KnownNat width
  , SerdesWidth width
  , KnownDomain domLine
  , KnownDomain domClk
  , KnownDomain domDiv
  , DomainPeriod domClk ~ (2 * DomainPeriod domLine)
  , DomainPeriod domDiv ~ (width * DomainPeriod domLine)
  ) =>
  Oserdese3Config ->
  -- | High-speed clock (@CLK@)
  Clock domClk ->
  -- | Divided clock (@CLKDIV@)
  Clock domDiv ->
  -- | Reset (@RST@)
  Reset domDiv ->
  -- | Words, ordered in time (@D@)
  Signal domDiv (Vec width Bit) ->
  -- | Serial output (@OQ@)
  Signal domLine Bit
oserdese3 config clk clkDiv rst ws
  | clashSimulation = sim
  | otherwise = unPort oq
 where
  initVal = oserdese3Init config

  -- After CLKDIV edge /m/ (samples @width * m + 1@ up to and including
  -- @width * (m + 1)@ of the serial output) the serializer transmits bits 2 and
  -- up of word @m - 1@, followed by bits 0 and 1 of word /m/. A reset at sample
  -- /r/ replaces the former for @m - 4 <= r <= m + 1@, and the latter for
  -- @m - 3 <= r <= m + 1@.
  sim :: Signal domLine Bit
  sim = initVal :- go (repeat initVal) (L.replicate 4 False) (unsafeToActiveHigh rst) ws

  go ::
    Vec width Bit ->
    [Bool] ->
    Signal domDiv Bool ->
    Signal domDiv (Vec width Bit) ->
    Signal domLine Bit
  go prev hist (r :- rs@(~(rNext :- _))) (w :- ws0) =
    prependSignal (L.map pick (L.zip [0 ..] (toList prev L.++ toList w)))
      (go w (L.take 4 (r : hist)) rs ws0)
   where
    -- hist = [r(m-1), r(m-2), r(m-3), r(m-4)]
    resetOld = rNext || r || L.or hist
    resetNew = rNext || r || L.or (L.take 3 hist)
    pick :: (Int, Bit) -> Maybe Bit
    pick (i, b)
      | i < 2 = Nothing -- bits 0 and 1 of the previous word: already sent
      | i < width = Just (if resetOld then initVal else b)
      | i < width + 2 = Just (if resetNew then initVal else b)
      | otherwise = Nothing

  width = natToNum @width :: Int

  dPort :: Signal domDiv (BitVector 8)
  dPort = resize . pack . reverse <$> ws

  (oq, _tOut) = go'
  go' :: (Port "OQ" domLine Bit, Port "T_OUT" domLine Bit)
  go' =
    inst
      (unisim "OSERDESE3")
      (Param @"DATA_WIDTH" @Integer (natToNum @width))
      (Param @"INIT" @Bit initVal)
      (Param @"IS_CLKDIV_INVERTED" @Bit 0)
      (Param @"IS_CLK_INVERTED" @Bit 0)
      (Param @"IS_RST_INVERTED" @Bit 0)
      (Param @"ODDR_MODE" @String "FALSE")
      (Param @"OSERDES_D_BYPASS" @String "FALSE")
      (Param @"OSERDES_T_BYPASS" @String "FALSE")
      (Param @"SIM_DEVICE" @String (simDeviceString (oserdese3SimDevice config)))
      (ClockPort @"CLK" clk)
      (ClockPort @"CLKDIV" clkDiv)
      (Port @"D" dPort)
      (ResetPort @"RST" @'ActiveHigh rst)
      (Port @"T" (pure 0 :: Signal domDiv Bit))

-- | Whether the reset is asserted at the current sample or at one of the
-- previous /n/ samples
resetWindow ::
  forall n dom.
  (KnownNat n, KnownDomain dom) =>
  SNat n ->
  Clock dom ->
  Reset dom ->
  Signal dom Bool
resetWindow SNat clk rst = (||) <$> r <*> (or <$> hist)
 where
  r = unsafeToActiveHigh rst
  hist :: Signal dom (Vec n Bool)
  hist = delay clk enableGen (repeat False) ((+>>) <$> r <*> hist)

-- | Take the first /n/ samples of a signal, and return the rest
takeSignal :: forall n dom a. (KnownNat n) => Signal dom a -> (Vec n a, Signal dom a)
takeSignal xs0 = (unsafeFromList (L.reverse acc), rest)
 where
  (acc, rest) = go (natToNum @n :: Int) [] xs0
  go 0 acc0 xs = (acc0, xs)
  go i acc0 (x :- xs) = go (i - 1) (x : acc0) xs

-- | Prepend the 'Just' values of a list to a signal
prependSignal :: [Maybe a] -> Signal dom a -> Signal dom a
prependSignal [] s = s
prependSignal (Nothing : xs) s = prependSignal xs s
prependSignal (Just x : xs) s = x :- prependSignal xs s
