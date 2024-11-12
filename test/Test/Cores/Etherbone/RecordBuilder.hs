{-# OPTIONS -fplugin=Protocols.Plugin #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cores.Etherbone.RecordBuilder (
  tests,
) where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Clash.Prelude as C
import Data.Maybe
import Protocols
import qualified Protocols.Df as Df
import Prelude
import Protocols.PacketStream
import Clash.Cores.Etherbone.Base
import Protocols.Hedgehog hiding (Test)
import Test.Cores.Etherbone.Internal
import Clash.Cores.Etherbone.RecordProcessor (Bypass(..))
import Clash.Cores.Etherbone.RecordBuilder (recordTxMetaMap, ebTxMeta)
import Clash.Cores.Etherbone.RecordBuilder (recordBuilderC)
import Data.Foldable
import qualified Data.Bifunctor as B

-- The current @WithModel@ helper functions do not support @CSignal@. To still
-- be able to use these functions, the bypass (@CSignal@) data should be
-- constant. This can only be the case for only writes or only reads.

{-
prop_recordBuilder_writes :: forall dat . Property
prop_recordBuilder_writes = property $ do
  hdr <- forAll $ genRecordHeader (Range.linear 1 32) (Range.singleton 0)

  let
    bypass = repeat $ Just (Bypass @AddrWidth hdr Nothing False)
    RecordHeader{..} = hdr

    wCount = fromIntegral _wCount

    inp = replicate (wCount-1) (dat False) <> [dat True] <> repeat Df.NoData
      where dat l = Df.Data $ WishboneResult @WBData Nothing l

    cfgDat'
      | _wca      = inp
      | otherwise = repeat Df.NoData
    wbmDat'
      | not _wca  = inp
      | otherwise = repeat Df.NoData

    setLast _ []     = []
    setLast f [x]    = [f x]
    setLast f (x:xs) = x : setLast f xs

    setResLast Df.NoData = Df.NoData
    setResLast (Df.Data x) = Df.Data $ x {_resLast = True}
    setPSLast Df.NoData = Df.NoData
    setPSLast (Df.Data x)  = Df.Data $ x {_last = Just maxBound}

    cfgDat = setLast setResLast cfgDat'
    wbmDat = setLast setResLast wbmDat'

    -- model :: ([WishboneResult WBData], [WishboneResult WBData]) -> [PacketStreamM2S DataWidth EBHeader]
    -- model _ = setLast setPSLast pkts
    --   where
    --     dat = replicate wCount (0 :: WBData) <> [C.pack hdr']
    --     hdr' = recordTxMetaMap hdr
    --     meta = ebTxMeta (C.SNat @DataWidth) (C.SNat @AddrWidth)

    --     pkts = map (\x -> PacketStreamM2S x Nothing meta False) (map C.bitCoerce dat)

    -- ckt
    --   :: Circuit (Df.Df dom (WishboneResult WBData), Df.Df dom (WishboneResult WBData))
    --              (PacketStream dom DataWidth EBHeader)
    -- ckt = Circuit go
    --   where
    --     go ((cfgF, wbmF), oBwd) = ((cfgB, wbmB), iBwd)
    --       where
    --         ((_, cfgB, wbmB), iBwd) = toSignals (recordBuilderC @_ @_ @DataWidth @WBData) ((pure Nothing, cfgF, wbmF), oBwd)

    res' = go ((C.fromList bypass, C.fromList cfgDat, C.fromList wbmDat), pure $ PacketStreamS2M True)
      where
        go = toSignals (C.withClockResetEnable C.clockGen C.resetGen C.enableGen (recordBuilderC @C.System))
    res = map cleanup $ C.sampleN (32) $ (C.bundle . B.first C.bundle) res'
    cleanup ((_, c, w), ps) = (c, w, ps)
  footnote $ show res

  assert False
-}

dfToSignalUnsafeC :: Circuit (Df.Df dom a) (CSignal dom (Maybe a))
dfToSignalUnsafeC = Circuit go
  where
    go (fwd, _) = (pure $ Ack True, Df.dataToMaybe <$> fwd)


prop_recordBuilder_writes :: Property
prop_recordBuilder_writes =
  idWithModelSingleDomain
    defExpectOptions
    genInp
    (C.exposeClockResetEnable recordBuilderModel)
    (C.exposeClockResetEnable (ckt @C.System))
  where
    ckt
      :: Circuit ( Df.Df dom (Bypass AddrWidth)
                 , Df.Df dom (WishboneResult WBData)
                 , Df.Df dom (WishboneResult WBData))
                 (PacketStream dom DataWidth EBHeader)
    ckt = circuit $ \(bp, cfg, wbm) -> do
      bpS <- dfToSignalUnsafeC -< bp
      recordBuilderC -< (bpS, cfg, wbm)


recordBuilderModel
  :: ([Bypass AddrWidth], [WishboneResult WBData], [WishboneResult WBData])
  -> [PacketStreamM2S DataWidth EBHeader]
recordBuilderModel (bypass, cfg, wbm) = out
  where
    hdr = _bpHeader $ head bypass
    base = _bpBase $ head bypass

    wCount = fromIntegral $ _wCount hdr
    rCount = fromIntegral $ _rCount hdr

    ebHdr = ebTxMeta (C.SNat @DataWidth) (C.SNat @AddrWidth)
    txHdr = recordTxMetaMap hdr

    outDat
      | wCount > 0 && rCount > 0 = error "This model assumes only reads or writes."
      | wCount > 0 = replicate (wCount+1) 0 <> [C.pack txHdr]
      | rCount > 0 = [C.pack txHdr] <> [fromJust base] <> replicate rCount readVal
      | otherwise = error "This model assumes at least one read or write"

    pkt dat = PacketStreamM2S (C.bitCoerce dat) Nothing ebHdr False
    out' = map pkt outDat

    lst = _resLast (last cfg) || _resLast (last wbm)
    out = init out' <> [(last out') {_last=l}]
      where l = if lst then Just maxBound else Nothing


  

-- go :: (Fwd
--    (CSignal Any (Maybe (Bypass Any)), Df Any (WishboneResult Any),
--     Df Any (WishboneResult Any)),
--  Bwd (PacketStream Any Any EBHeader))

  -- idWithModelSingleDomain
  --   defExpectOptions
  --   genInp
  --   (C.exposeClockResetEnable model)
  --   (C.exposeClockResetEnable (ckt @C.System))


{-
prop_recordBuilder_writes :: Property
prop_recordBuilder_writes = property $ do
  hdr <- forAll $ genRecordHeader (Range.linear 1 32) (Range.singleton 0)

  let
    bypass = repeat $ Just (Bypass hdr Nothing False)
    RecordHeader{_wCount, _wca} = hdr

    wCount = fromIntegral _wCount

    cfgDat'
      | _wca      = replicate wCount (WishboneResult Nothing False)
      | otherwise = []
    wbmDat'
      | not _wca  = replicate wCount (WishboneResult Nothing False)
      | otherwise = []

    setLast _ []     = []
    setLast f [x]    = [f x]
    setLast f (x:xs) = x : setLast f xs

    setResLast x = x {_resLast = True}
    setPSLast x  = x {_last = Just maxBound}

    cfgDat = setLast setResLast cfgDat'
    wbmDat = setLast setResLast wbmDat'

    genInp :: Gen ([WishboneResult WBData], [WishboneResult WBData])
    genInp = pure (cfgDat, wbmDat)

    model :: ([WishboneResult WBData], [WishboneResult WBData]) -> [PacketStreamM2S DataWidth EBHeader]
    model _ = setLast setPSLast pkts
      where
        dat = replicate wCount (0 :: WBData) <> [C.pack hdr']
        hdr' = recordTxMetaMap hdr
        meta = ebTxMeta (C.SNat @DataWidth) (C.SNat @AddrWidth)

        pkts = map (\x -> PacketStreamM2S x Nothing meta False) (map C.bitCoerce dat)

    ckt
      :: Circuit (Df.Df dom (WishboneResult WBData), Df.Df dom (WishboneResult WBData))
                 (PacketStream dom DataWidth EBHeader)
    ckt = Circuit go
      where
        go ((cfgF, wbmF), oBwd) = ((cfgB, wbmB), iBwd)
          where
            ((_, cfgB, wbmB), iBwd) = toSignals (recordBuilderC @_ @_ @DataWidth @WBData) ((C.fromList bypass, cfgF, wbmF), oBwd)

  idWithModelSingleDomain
    defExpectOptions
    genInp
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable (ckt @C.System))

-}

tests :: TestTree
tests = $(testGroupGenerator)
