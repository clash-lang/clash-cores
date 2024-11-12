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


testBaseAddr :: C.BitVector AddrWidth
testBaseAddr = 0xdeadbeef

genRecordBuilderInput :: C.Unsigned 8 -> C.Unsigned 8 -> Gen [(Bypass AddrWidth, WishboneResult WBData)]
genRecordBuilderInput wMax rMax = do
  hdr <- genRecordHeader (Range.linear 0 wMax) (Range.linear 0 rMax)

  let
    wCount = fromIntegral $ _wCount hdr
    rCount = fromIntegral $ _rCount hdr

    bypassW
      | wCount > 0 = replicate (wCount+1) Nothing
      | otherwise  = []
    bypassR
      | rCount > 0 = [Just testBaseAddr] <> replicate rCount Nothing
      | otherwise  = []
    bypass = map (\x -> Bypass hdr x False) (bypassW <> bypassR)
    
    wbResW = replicate wCount $ WishboneResult Nothing False
    wbResR = replicate rCount $ WishboneResult (Just readVal) False
    wbRes' = wbResW <> wbResR
    wbRes = init wbRes' <> [(last wbRes') {_resLast=True}]

    out = zip bypass wbRes
  pure out

recordBuilderTest :: C.Unsigned 8 -> C.Unsigned 8 -> Property
recordBuilderTest wMax rMax =
  idWithModelSingleDomain
    defExpectOptions {eoTrace=False}
    (genRecordBuilderInput wMax rMax)
    (C.exposeClockResetEnable recordBuilderModel)
    (C.exposeClockResetEnable (ckt @C.System))
  where
    -- In the real implementation it is impossible for bypass signals to be
    -- received _later_ than their corresponding wishboneResult signal. To
    -- ensure this in simulation, the bypass and result lines are combined in a
    -- single Df so that they receive the same backpressure.
    -- This is not totaly identical to the real situation, but good enough for a
    -- simple unit test.
    ckt ::
      ( C.HiddenClockResetEnable dom
      , C.KnownNat dataWidth
      , C.KnownNat addrWidth
      , C.BitPack dat
      , C.BitSize dat ~ dataWidth C.* 8
      , 4 C.<= dataWidth
      )
      => Circuit ( Df.Df dom (Bypass addrWidth, WishboneResult dat) )
                 (PacketStream dom dataWidth EBHeader)
    ckt = circuit $ \inp -> do
      [inp0, inp1] <- Df.fanout -< inp
      bp <- Df.fst -< inp0
      wb <- Df.snd -< inp1
      [cfg, wbm] <- Df.fanout -< wb
      recordBuilderC -< (bp, cfg, wbm)

recordBuilderModel
  :: [(Bypass AddrWidth, WishboneResult WBData)]
  -> [PacketStreamM2S DataWidth EBHeader]
recordBuilderModel inp = out
  where
    bypass = map fst inp
    wb = map snd inp

    hdr = _bpHeader $ head bypass
    base = _bpBase $ head bypass

    wCount = fromIntegral $ _wCount hdr
    rCount = fromIntegral $ _rCount hdr

    ebHdr = ebTxMeta (C.SNat @DataWidth) (C.SNat @AddrWidth)
    txHdr = recordTxMetaMap hdr

    -- The generator and 'all inputs in one Df' format does not support
    -- combined reads and writes. This requires a different number of bypass
    -- signals than wishboneResult signals. And splitting them into separate Dfs
    -- has the issue of unrealisitc amount of backpressure on the bypass line.
    outDat
      | wCount > 0 && rCount > 0 = error "This model assumes only reads or writes."
      | wCount > 0 = replicate (wCount+1) 0 <> [C.pack txHdr]
      | rCount > 0 = [C.pack txHdr] <> [fromJust base] <> replicate rCount readVal
      | otherwise = error "This model assumes at least one read or write"

    pkt dat = PacketStreamM2S (C.bitCoerce dat) Nothing ebHdr False
    out' = map pkt outDat

    lst = _resLast (last wb)
    out = init out' <> [(last out') {_last=l}]
      where l = if lst then Just maxBound else Nothing

prop_recordBuilder_writes :: Property
prop_recordBuilder_writes = recordBuilderTest 32 0

prop_recordBuilder_reads :: Property
prop_recordBuilder_reads = recordBuilderTest 0 32

-- prop_recordBuilder_writesAndReads :: Property
-- prop_recordBuilder_writesAndReads = recordBuilderTest 32 32


tests :: TestTree
tests = $(testGroupGenerator)
