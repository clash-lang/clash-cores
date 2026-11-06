{-# LANGUAGE AllowAmbiguousTypes #-}
module IoBufferTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench (clockToDiffClock)

import Clash.Cores.I2c (I2cIn, I2cOut)
import Clash.Cores.Xilinx.Unisim.IoBuffer
import Clash.Signal.BiSignal (HasBiSignalDefault, SBiSignalDefault)
import Data.Reflection (Given)
import UnisimTestCommon

-- | Domain of the test bench
type A = XilinxSystem

createDomain vXilinxSystem{vName="Div2", vPeriod=2 * hzToPeriod 100e6}

-- | Random data through an 'obufds', and back through an 'ibufds'. Returns
-- the positive and negative output of the 'obufds', and the output of the
-- 'ibufds'.
dutObufds :: Clock A -> Signal A (BitVector 3)
dutObufds clk = pack <$> bundle (p, n, ibufds p n)
 where
  (p, n) = obufds (genTestData randomSeed clk)

-- | A counter clocked by a clock, observed from domain 'A'. See
-- "ClockBufferTypes".
observe ::
  (KnownDomain dom) =>
  Clock A ->
  Clock dom ->
  Signal A (Unsigned 8)
observe clkA clkB =
  delay clkA enableGen 0 (unsafeSynchronizer clkB clkA cnt)
 where
  cnt = delay clkB enableGen 0 (cnt + 1)

-- | Counters clocked by both outputs of an 'ibufdsGte4' or 'ibufdsGte3',
-- observed from 'A'
dutIbufdsGte4 ::
  forall div domDiv.
  ( KnownDomain domDiv
  , 1 <= div
  , div <= 2
  , DomainPeriod domDiv ~ div * DomainPeriod A
  ) =>
  -- | Use 'ibufdsGte3' instead of 'ibufdsGte4'
  Bool ->
  SNat div ->
  Clock A ->
  Signal A (Unsigned 8, Unsigned 8)
dutIbufdsGte4 gte3 divider clkA = bundle (observe clkA o, observe clkA odiv2)
 where
  (o, odiv2)
    | gte3 = ibufdsGte3 @div @A @domDiv divider (clockToDiffClock clkA)
    | otherwise = ibufdsGte4 @div @A @domDiv divider (clockToDiffClock clkA)

-- | Clear the output during the first 16 cycles. The Verilog UNISIM models put
-- all outputs in high impedance during the first 100 ns of the simulation
-- (@glbl.GTS@), so we can't check them yet.
warmUp :: (NFDataX a) => a -> Clock A -> Signal A a -> Signal A a
warmUp dflt clk = mux (cycleCount @A @32 clk .<. 16) (pure dflt)

{- | Values to drive onto a wire shared by two buffers, picked by random bits:
never both at the same time, and none at all only if the wire may float (it
has a pull resistor).
-}
drivers :: Bool -> BitVector 2 -> Bit -> Bit -> (Maybe Bit, Maybe Bit)
drivers mayFloat sel a b = case sel of
  0 -> (Just a, Nothing)
  1 -> (Nothing, Just b)
  2 | mayFloat -> (Nothing, Nothing)
    | otherwise -> (Nothing, Just b)
  _ -> (Just a, Nothing)

-- | Random drivers for two buffers sharing a wire, see 'drivers'
randomDrivers :: Bool -> Clock A -> Signal A (Maybe Bit, Maybe Bit)
randomDrivers mayFloat clk = go <$> genTestData randomSeed clk
 where
  go (sel, a, b) = drivers mayFloat sel a b

-- | Configuration with non-default attributes for 'iobuf'
iobufConfig :: IobufConfig
iobufConfig =
  IobufConfig
    { iobufDrive = 8
    , iobufSlew = Fast
    , iobufIoStandard = "LVCMOS18"
    , iobufLowPower = False
    }

{- | Two 'iobuf's sharing a single wire. Returns what both of them read from
the wire. The first buffer uses the given configuration, the second one the
default. If @mayFloat@ is set, neither buffer drives the wire in some cycles,
which should make it read the level of its pull resistor.
-}
dutIobuf ::
  forall ds.
  (HasBiSignalDefault ds, Given (SBiSignalDefault ds)) =>
  Bool ->
  IobufConfig ->
  Clock A ->
  Signal A (Bit, Bit)
dutIobuf mayFloat config clk = warmUp (0, 0) clk (bundle (oA, oB))
 where
  (outA, outB) = unbundle (randomDrivers mayFloat clk)
  wire = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wireA :> wireB :> Nil))
  (wireA, oA) = iobuf @ds config wire outA
  (wireB, oB) = iobuf iobufDefaultConfig wire outB

-- | Configuration with non-default attributes for 'obuft'
obuftConfig :: ObuftConfig
obuftConfig =
  ObuftConfig
    { obuftDrive = 4
    , obuftSlew = Medium
    , obuftIoStandard = "LVCMOS12"
    }

{- | An 'obuft' and an 'iobuf' sharing a single wire. Returns what the 'iobuf'
reads from the wire. If @mayFloat@ is set, neither buffer drives the wire in
some cycles, which should make it read the level of its pull resistor.
-}
dutObuft ::
  forall ds.
  (HasBiSignalDefault ds, Given (SBiSignalDefault ds)) =>
  Bool ->
  ObuftConfig ->
  Clock A ->
  Signal A Bit
dutObuft mayFloat config clk = warmUp 0 clk o
 where
  (outA, outB) = unbundle (randomDrivers mayFloat clk)
  wire = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wireA :> wireB :> Nil))
  wireA = obuft @ds config wire outA
  (wireB, o) = iobuf iobufDefaultConfig wire outB

-- | Configuration with non-default attributes for 'iobufds'
iobufdsConfig :: IobufdsConfig
iobufdsConfig =
  IobufdsConfig
    { iobufdsSlew = Fast
    , iobufdsIoStandard = "DIFF_SSTL12"
    , iobufdsLowPower = False
    , iobufdsDiffTerm = True
    }

{- | Two 'iobufds's sharing a differential pair. Returns what both of them read
from the pair. The first buffer uses 'iobufdsConfig', the second one the
default. The pair is always driven by one of them.
-}
dutIobufds :: Clock A -> Signal A (Bit, Bit)
dutIobufds clk = warmUp (0, 0) clk (bundle (oA, oB))
 where
  (outA, outB) = unbundle (randomDrivers False clk)
  wireP = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wirePA :> wirePB :> Nil))
  wireN = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wireNA :> wireNB :> Nil))
  (wirePA, wireNA, oA) = iobufds iobufdsConfig wireP wireN outA
  (wirePB, wireNB, oB) = iobufds iobufdsDefaultConfig wireP wireN outB

-- | Configuration with non-default attributes for 'obuftds'
obuftdsConfig :: ObuftdsConfig
obuftdsConfig =
  ObuftdsConfig
    { obuftdsSlew = Fast
    , obuftdsIoStandard = "DIFF_HSTL_I_18"
    }

{- | An 'obuftds' and an 'iobufds' sharing a differential pair. Returns what
the 'iobufds' reads from the pair. The pair is always driven by one of them.
-}
dutObuftds :: Clock A -> Signal A Bit
dutObuftds clk = warmUp 0 clk o
 where
  (outA, outB) = unbundle (randomDrivers False clk)
  wireP = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wirePA :> wirePB :> Nil))
  wireN = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wireNA :> wireNB :> Nil))
  (wirePA, wireNA) = obuftds obuftdsConfig wireP wireN outA
  (wirePB, wireNB, o) = iobufds iobufdsDefaultConfig wireP wireN outB

{- | The example in the documentation of "Clash.Cores.Xilinx.Unisim.IoBuffer",
to make sure it compiles.
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
  (sclPadOut, scl) = iobuf iobufDefaultConfig sclPad (openDrain <$> sclO <*> sclOen)
  (sdaPadOut, sda) = iobuf iobufDefaultConfig sdaPad (openDrain <$> sdaO <*> sdaOen)

  -- The output enables of the I2C core are active low: True releases the line
  openDrain o oen = if oen then Nothing else Just o
