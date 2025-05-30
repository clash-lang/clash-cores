{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cores.Etherbone.RecordBuilder (
  tests,
) where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import Hedgehog
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
import Clash.Cores.Etherbone.RecordBuilder (hdrRx2Tx, ebTxMeta, recordBuilderC)

-- TODO: Add multi-record testing

testBaseAddr :: C.BitVector AddrWidth
testBaseAddr = 0xdeadbeef

genRecordBuilderInput :: C.Unsigned 8 -> C.Unsigned 8 -> Gen [(Bypass AddrWidth, WishboneResult WBData)]
genRecordBuilderInput wMax rMax = do
  hdr <- genRecordHeader (Range.linear 0 wMax) (Range.linear 0 rMax)

  let
    wCount = fromIntegral $ _wCount hdr
    rCount = fromIntegral $ _rCount hdr

    wbResW = replicate wCount $ WishboneResult Nothing False False
    wbResR = replicate rCount $ WishboneResult (Just readVal) False False
    wbRes' = wbResW <> wbResR
    wbRes = init wbRes' <> [(last wbRes') {_resEOR=True, _resEOP=True}]

    -- The bypass line is always kept constant. The base address is only used if
    -- there are reads.
    bypass = repeat $ Bypass hdr (Just testBaseAddr) False
  pure $ zip bypass wbRes

recordBuilderTest :: C.Unsigned 8 -> C.Unsigned 8 -> Property
recordBuilderTest wMax rMax =
  idWithModelSingleDomain
    defExpectOptions {eoTrace=False}
    (genRecordBuilderInput wMax rMax)
    (C.exposeClockResetEnable recordBuilderModel)
    (C.exposeClockResetEnable (ckt @C.System))
  where
    -- The @withModel@ functions do not support @CSignal@. Additionally, the
    -- bypass signal and the @WishboneResult@ are always related. So they are
    -- put in the same @Df@ signal, where the backwards signal on bypass is
    -- being dropped.
    ckt :: forall dom addrWidth dataWidth dat .
      ( C.HiddenClockResetEnable dom
      , C.KnownNat addrWidth
      , C.KnownNat dataWidth
      , C.BitPack dat
      , C.BitSize dat ~ dataWidth C.* 8
      , 4 C.<= dataWidth
      )
      => Circuit (Df.Df dom (Bypass addrWidth, WishboneResult dat))
                 (PacketStream dom dataWidth EBHeader)
    ckt = circuit $ \input -> do
      [in0, in1] <- Df.fanout -< input
      wb <- Df.snd -< in0
      bp <- bypassC <| Df.fst -< in1

      [wb0, wb1] <- Df.fanout -< wb

      recordBuilderC -< (bp, wb0, wb1)
      where
        bypassC ::
          Circuit (Df.Df dom (Bypass addrWidth))
                  (CSignal dom (Maybe(Bypass addrWidth)))
        bypassC = Circuit $ C.unbundle . fmap go . C.bundle
          where
            go (dat, _) = (Ack True, dat)

recordBuilderModel
  :: [(Bypass AddrWidth, WishboneResult WBData)]
  -> [PacketStreamM2S DataWidth EBHeader]
recordBuilderModel [] = error "recordBuilderModel: No input data"
recordBuilderModel inp@((fst -> bypassHead): _) = out
  where
    (_, wb) = unzip inp

    hdr = _bpHeader bypassHead
    base = _bpBase bypassHead

    wCount = fromIntegral $ _wCount hdr
    rCount = fromIntegral $ _rCount hdr

    ebHdr = ebTxMeta (C.SNat @DataWidth) (C.SNat @AddrWidth)
    txHdr = hdrRx2Tx hdr

    outDat
      | wCount > 0 && rCount > 0
        = replicate (wCount+1) 0 <> [C.pack txHdr]
        <> [fromJust base] <> replicate rCount readVal
      | wCount > 0 = replicate (wCount+1) 0 <> [C.pack txHdr]
      | rCount > 0 = [C.pack txHdr] <> [fromJust base] <> replicate rCount readVal
      | otherwise = error "This model assumes at least one read or write"

    pkt dat = PacketStreamM2S (C.bitCoerce dat) Nothing ebHdr False
    out' = map pkt outDat

    lst = _resEOP (last wb)
    out = init out' <> [(last out') {_last=l}]
      where l = if lst then Just maxBound else Nothing

prop_recordBuilder_writes :: Property
prop_recordBuilder_writes = recordBuilderTest 32 0

prop_recordBuilder_reads :: Property
prop_recordBuilder_reads = recordBuilderTest 0 32

prop_recordBuilder_writesAndReads :: Property
prop_recordBuilder_writesAndReads = recordBuilderTest 32 32


tests :: TestTree
tests = $(testGroupGenerator)
