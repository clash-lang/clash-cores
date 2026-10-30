{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-top-binds #-}

-- | Haskell-only tests of the simulation models of the AXI XPM FIFOs. Their
-- cycle accuracy is tested against the real macros in @hdl-tests@; these
-- tests check FIFO properties: no data is lost, duplicated, or reordered,
-- under random back pressure.
module Test.Cores.Xilinx.Xpm.FifoAxi (tests) where

import Clash.Explicit.Prelude

import qualified Prelude as P

import Hedgehog (Gen, Property, forAll, property, (===))
import Protocols.Experimental.Axi4.Stream
import Protocols.Experimental.Hedgehog (defExpectOptions, idWithModel)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Cores.Xilinx.Xpm.Fifo.Axif
import Clash.Cores.Xilinx.Xpm.Fifo.Axil
import Clash.Cores.Xilinx.Xpm.Fifo.Axis

createDomain vXilinxSystem{vName="P3", vPeriod=3000}
createDomain vXilinxSystem{vName="P7", vPeriod=7000}

type Conf = 'Axi4StreamConfig 2 1 1

-- | Stream of transfers. The last transfer has '_tlast' set, which packet
-- mode needs to get all data out.
genStream :: Gen [Axi4StreamM2S Conf (BitVector 2)]
genStream = do
  xs <- Gen.list (Range.linear 0 100) genTransfer
  pure (P.zipWith (\i x -> if i == P.length xs then x{_tlast = True} else x) [1..] xs)
 where
  genTransfer =
    Axi4StreamM2S
      <$> traverse (const (Gen.integral Range.linearBounded)) (repeat ())
      <*> traverse (const Gen.bool) (repeat ())
      <*> traverse (const Gen.bool) (repeat ())
      <*> Gen.bool
      <*> Gen.integral Range.linearBounded
      <*> Gen.integral Range.linearBounded
      <*> Gen.integral Range.linearBounded

prop_axisId :: XpmFifoAxisConfig 16 -> Property
prop_axisId cfg =
  idWithModel defExpectOptions genStream id
    (xpmFifoAxisC @16 @Conf @(BitVector 2) cfg (clockGen @P3) resetGen)

-- | Source for one channel: offers the given values in order, each after
-- waiting the given number of cycles. Keeps offering a value until it is
-- accepted. Starts after 50 cycles: with independent clocks, the FIFOs
-- briefly assert ready during their power-on reset (and drop the data).
source :: (KnownDomain dom, NFDataX a) => Clock dom -> [(Int, a)] -> Signal dom Bool -> Signal dom (Maybe a)
source clk vals0 ready = mealy clk noReset enableGen go (startup vals0) ready
 where
  startup [] = []
  startup ((w, a):rest) = (w + 50, a):rest

  go [] _ = ([], Nothing)
  go vals@((w, a):rest) rdy
    | w > 0 = ((w - 1, a):rest, Nothing)
    | rdy = (rest, Just a)
    | otherwise = (vals, Just a)

-- | Values accepted on a channel
accepted :: (KnownDomain dom, NFDataX a) => Int -> Signal dom (Maybe a) -> Signal dom Bool -> [a]
accepted n dat rdy = [a | (Just a, True) <- sampleN n (bundle (dat, rdy))]

genWaits :: Gen [(Int, Unsigned 16)]
genWaits = do
  n <- Gen.int (Range.linear 0 50)
  ws <- Gen.list (Range.singleton n) (Gen.frequency [(3, pure 0), (1, Gen.int (Range.linear 1 5))])
  pure (P.zip ws [0..])

genReadies :: Gen [Bool]
genReadies = Gen.list (Range.singleton 1500) (Gen.frequency [(2, pure True), (1, pure False)])

readySig :: KnownDomain dom => [Bool] -> Signal dom Bool
readySig rs = fromList (rs <> P.repeat True)

-- | The AXI4-Lite FIFO with independent clocks delivers all transfers on all
-- channels, in order. Relies on the power-on reset of the FIFO: a reset
-- asserted while the sources are active would (legitimately) drop data.
prop_axilAsync :: Property
prop_axilAsync = property $ do
  aw <- forAll genWaits
  w <- forAll genWaits
  b <- forAll genWaits
  ar <- forAll genWaits
  r <- forAll genWaits
  rdyAw <- forAll genReadies
  rdyW <- forAll genReadies
  rdyB <- forAll genReadies
  rdyAr <- forAll genReadies
  rdyR <- forAll genReadies
  let
    clkS = clockGen @P3
    clkM = clockGen @P7
    cfg = (xpmFifoAxilConfig d16 d16)
      { axilWriteAddressDepth = 16, axilWriteResponseDepth = 16, axilReadAddressDepth = 16 }
    sM2S = Axi4LiteM2S
      <$> source clkS (fmap (fmap mkAddr) aw) (axilAwReady <$> sS2M)
      <*> source clkS (fmap (fmap mkData) w) (axilWReady <$> sS2M)
      <*> readySig rdyB
      <*> source clkS (fmap (fmap mkAddr) ar) (axilArReady <$> sS2M)
      <*> readySig rdyR
    mS2M = Axi4LiteS2M
      <$> readySig rdyAw
      <*> readySig rdyW
      <*> source clkM (fmap (fmap mkResp) b) (axilBReady <$> mM2S)
      <*> readySig rdyAr
      <*> source clkM (fmap (fmap mkRData) r) (axilRReady <$> mM2S)
    (sS2M, mM2S, _) = xpmFifoAxilAsync @16 @4 cfg clkS clkM noReset sM2S mS2M
    nS = 2000
    nM = 1000
  accepted nM (axilAw <$> mM2S) (axilAwReady <$> mS2M) === fmap (mkAddr . snd) aw
  accepted nM (axilW <$> mM2S) (axilWReady <$> mS2M) === fmap (mkData . snd) w
  accepted nS (axilB <$> sS2M) (axilBReady <$> sM2S) === fmap (mkResp . snd) b
  accepted nM (axilAr <$> mM2S) (axilArReady <$> mS2M) === fmap (mkAddr . snd) ar
  accepted nS (axilR <$> sS2M) (axilRReady <$> sM2S) === fmap (mkRData . snd) r
 where
  mkAddr n = Axi4LiteAddress (pack n) (resize (pack n))
  mkData n = Axi4LiteWriteData (pack (n, n + 1)) (resize (pack n))
  mkResp n = unpack (resize (pack n)) :: AxiResp
  mkRData n = Axi4LiteReadData (pack (n, complement n)) (mkResp n)

type AxiConf = 'Axi4Config 2 16 2 1 1 1 1 1

-- | The AXI4 FIFO with a common clock delivers all transfers on all channels,
-- in order
prop_axif :: Property
prop_axif = property $ do
  aw <- forAll genWaits
  w <- forAll genWaits
  b <- forAll genWaits
  ar <- forAll genWaits
  r <- forAll genWaits
  rdyAw <- forAll genReadies
  rdyW <- forAll genReadies
  rdyB <- forAll genReadies
  rdyAr <- forAll genReadies
  rdyR <- forAll genReadies
  let
    clk = clockGen @P3
    cfg = (xpmFifoAxifConfig d16 d32)
      { axifWriteAddressDepth = 16, axifWriteResponseDepth = 32, axifReadAddressDepth = 16 }
    sM2S = Axi4M2S
      <$> source clk (fmap (fmap mkAddr) aw) (axiAwReady <$> sS2M)
      <*> source clk (fmap (fmap mkData) w) (axiWReady <$> sS2M)
      <*> readySig rdyB
      <*> source clk (fmap (fmap mkAddr) ar) (axiArReady <$> sS2M)
      <*> readySig rdyR
    mS2M = Axi4S2M
      <$> readySig rdyAw
      <*> readySig rdyW
      <*> source clk (fmap (fmap mkResp) b) (axiBReady <$> mM2S)
      <*> readySig rdyAr
      <*> source clk (fmap (fmap mkRData) r) (axiRReady <$> mM2S)
    (sS2M, mM2S, _) = xpmFifoAxif @AxiConf cfg clk noReset sM2S mS2M
    n = 2000
  accepted n (axiAw <$> mM2S) (axiAwReady <$> mS2M) === fmap (mkAddr . snd) aw
  accepted n (axiW <$> mM2S) (axiWReady <$> mS2M) === fmap (mkData . snd) w
  accepted n (axiB <$> sS2M) (axiBReady <$> sM2S) === fmap (mkResp . snd) b
  accepted n (axiAr <$> mM2S) (axiArReady <$> mS2M) === fmap (mkAddr . snd) ar
  accepted n (axiR <$> sS2M) (axiRReady <$> sM2S) === fmap (mkRData . snd) r
 where
  mkAddr k = unpack (resize (pack (k, complement k, k)))
  mkData k = unpack (resize (pack (complement k, k, k)))
  mkResp k = unpack (resize (pack (k, k)))
  mkRData k = unpack (resize (pack (k `rotateL` 3, k, k)))

tests :: TestTree
tests = testGroup "XPM AXI FIFOs"
  [ testPropertyNamed
      "xpmFifoAxisC is an identity"
      "prop_axisId"
      (prop_axisId (xpmFifoAxisConfig d16))
  , testPropertyNamed
      "xpmFifoAxisC in packet mode is an identity"
      "prop_axisId_packet"
      (prop_axisId (xpmFifoAxisConfig d16){axisPacketFifo = True})
  , testPropertyNamed
      "xpmFifoAxilAsync delivers all transfers in order"
      "prop_axilAsync"
      prop_axilAsync
  , testPropertyNamed
      "xpmFifoAxif delivers all transfers in order"
      "prop_axif"
      prop_axif
  ]
