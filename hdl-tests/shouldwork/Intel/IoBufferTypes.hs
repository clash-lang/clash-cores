module IoBufferTypes where

import Clash.Explicit.Prelude

import Clash.Cores.I2c (I2cIn, I2cOut)
import Clash.Cores.Intel.IO

import IntelTestCommon

type Dom = System

-- | Number of cycles per test bench
nCycles :: Int
nCycles = 200

{- | Stimuli for two drivers sharing a wire: which one drives (or none at
all, if the wire may float), and the bits they drive.
-}
type DriverStimuli = (BitVector 2, Bit, Bit)

genDriverStimuli :: Int -> [DriverStimuli]
genDriverStimuli =
  evalG (listOf nCycles ((,,) <$> genBV <*> genBit <*> genBit))

{- | Values to drive onto a wire shared by two buffers, picked by random bits:
never both at the same time, and none at all only if the wire may float (it
has a pull resistor).
-}
drivers :: Bool -> DriverStimuli -> (Maybe Bit, Maybe Bit)
drivers mayFloat (sel, a, b) = case sel of
  0 -> (Just a, Nothing)
  1 -> (Nothing, Just b)
  2 | mayFloat -> (Nothing, Nothing)
    | otherwise -> (Nothing, Just b)
  _ -> (Just a, Nothing)

-- | Configuration with non-default parameters
config :: IoBufferConfig
config =
  IoBufferConfig
    { ioStandard = Just "3.3-V LVTTL"
    , ioCurrentStrength = Just "8MA"
    , ioSlewRate = Just 1
    , ioBusHold = Just False
    , ioTermination = Just "SERIES 50 OHMS"
    , ioLocation = Nothing
    }

-- | Configuration with non-default parameters for differential buffers
diffConfig :: IoBufferConfig
diffConfig =
  defaultIoBufferConfig
    { ioStandard = Just "LVDS"
    , ioTermination = Just "DIFFERENTIAL"
    }

-- | A wire shared by several buffers
type Wire ds = BiSignalIn ds Dom 1

{- | Two 'iobuf's sharing a single wire. Returns what both of them read from
the wire. The first buffer uses the given configuration, the second one the
default. The wire is always driven by one of them.
-}
dutIobufFloating :: IoBufferConfig -> Clock Dom -> Signal Dom DriverStimuli -> Signal Dom (Bit, Bit)
dutIobufFloating cfg _clk stim = bundle (strengthen oA, strengthen oB)
 where
  (outA, outB) = unbundle (drivers False <$> stim)
  wire :: Wire 'Floating
  wire = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wireA :> wireB :> Nil))
  (wireA, oA) = iobuf cfg wire outA
  (wireB, oB) = iobuf defaultIoBufferConfig wire outB

-- | Like 'dutIobufFloating', with a pull-up resistor, so that neither buffer
-- drives the wire in some cycles.
dutIobufPullUp :: IoBufferConfig -> Clock Dom -> Signal Dom DriverStimuli -> Signal Dom (Bit, Bit)
dutIobufPullUp cfg _clk stim = bundle (strengthen oA, strengthen oB)
 where
  (outA, outB) = unbundle (drivers True <$> stim)
  wire :: Wire 'PullUp
  wire =
    veryUnsafeToBiSignalIn
      (mergeBiSignalOuts (wireA :> wireB :> pullUp wire :> Nil))
  (wireA, oA) = iobuf cfg wire outA
  (wireB, oB) = iobuf defaultIoBufferConfig wire outB

{- | An 'outbufTri' driving random bits, or high impedance in a quarter of
the cycles. Clash simulates high impedance as undefined, so those cycles
aren't checked.
-}
dutOutbufTri :: Clock Dom -> Signal Dom DriverStimuli -> Signal Dom Bit
dutOutbufTri _clk stim = outbufTri config (go <$> stim)
 where
  go (sel, a, _b) = if sel == 2 then Nothing else Just a

{- | An 'opndrn' and an 'iobuf' sharing a wire with a pull-up resistor. The
'iobuf' only drives the wire when 'opndrn' releases it. Returns what the
'iobuf' reads.
-}
dutOpndrn :: Clock Dom -> Signal Dom DriverStimuli -> Signal Dom Bit
dutOpndrn _clk stim = strengthen o
 where
  (inA, outB) = unbundle (go <$> stim)
  go (sel, _a, b) = case sel of
    0 -> (0, Nothing)
    1 -> (1, Just b)
    _ -> (1, Nothing)
  wire :: Wire 'PullUp
  wire =
    veryUnsafeToBiSignalIn
      (mergeBiSignalOuts (wireA :> wireB :> pullUp wire :> Nil))
  wireA = opndrn wire inA
  (wireB, o) = iobuf defaultIoBufferConfig wire outB

{- | Random data through an 'outbufDiff', and back through an 'inbufDiff'.
Returns the positive and negative output of the 'outbufDiff', and the output
of the 'inbufDiff'. The last stimulus bit drives the negative input of the
'inbufDiff' instead, if the first one is set, to check what it does for equal
inputs.
-}
dutOutbufDiff :: Clock Dom -> Signal Dom DriverStimuli -> Signal Dom (Bit, Bit, Bit)
dutOutbufDiff _clk stim = bundle (p, n, inbufDiff diffConfig p n1)
 where
  (sel, a, b) = unbundle stim
  (p, n) = outbufDiff diffConfig a
  n1 = mux ((== 3) <$> sel) b n

{- | Two 'iobufDiff's sharing a differential pair. Returns what both of them
read from the pair. The pair is always driven by one of them.
-}
dutIobufDiff :: Clock Dom -> Signal Dom DriverStimuli -> Signal Dom (Bit, Bit)
dutIobufDiff _clk stim = bundle (oA, oB)
 where
  (outA, outB) = unbundle (drivers False <$> stim)
  wireP = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wirePA :> wirePB :> Nil))
  wireN = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wireNA :> wireNB :> Nil))
  (wirePA, wireNA, oA) = iobufDiff diffConfig wireP wireN outA
  (wirePB, wireNB, oB) = iobufDiff defaultIoBufferConfig wireP wireN outB

{- | The example in the documentation of "Clash.Cores.Intel.IO", to make sure
it compiles.
-}
i2cPads ::
  (KnownDomain dom) =>
  "SCL" ::: BiSignalIn 'PullUp dom 1 ->
  "SDA" ::: BiSignalIn 'PullUp dom 1 ->
  Signal dom I2cOut ->
  ( "SCL" ::: BiSignalOut 'PullUp dom 1
  , "SDA" ::: BiSignalOut 'PullUp dom 1
  , Signal dom I2cIn
  )
i2cPads sclPad sdaPad i2cOut = (sclPadOut, sdaPadOut, bundle (scl, sda))
 where
  (sclO, sclOen, sdaO, sdaOen) = unbundle i2cOut
  (sclPadOut, scl) = iobuf defaultIoBufferConfig sclPad (openDrain <$> sclO <*> sclOen)
  (sdaPadOut, sda) = iobuf defaultIoBufferConfig sdaPad (openDrain <$> sdaO <*> sdaOen)

  -- The output enables of the I2C core are active low: True releases the line
  openDrain o oen = if oen then Nothing else Just o
