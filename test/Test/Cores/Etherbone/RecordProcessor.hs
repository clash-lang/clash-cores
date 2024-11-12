{-# OPTIONS -fplugin=Protocols.Plugin #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cores.Etherbone.RecordProcessor (
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
import Clash.Cores.Etherbone.RecordProcessor (recordProcessorC, Bypass(..))
import Test.Cores.Etherbone.Internal

genRecordProcessorInput :: Gen [PacketStreamM2S DataWidth RecordHeader]
genRecordProcessorInput = do
  hdr <- genRecordHeader (Range.linear 0 32) (Range.linear 0 32)

  readBase' :: WBData <- Gen.integral Range.linearBounded
  writeBase' :: WBData <- Gen.integral Range.linearBounded

  let
    RecordHeader{..} = hdr

    readBase
      | _rCount > 0 = [readBase']
      | otherwise   = []
    writeBase
      | _wCount > 0 = [writeBase']
      | otherwise   = []

    readVals = replicate (fromIntegral _rCount) readVal
    writeVals = replicate (fromIntegral _wCount) writeVal

    rawData = writeBase <> writeVals <> readBase <> readVals

    pkt :: WBData -> PacketStreamM2S DataWidth RecordHeader
    pkt dat = PacketStreamM2S (C.bitCoerce dat) Nothing hdr False

    stream' = map pkt rawData
    stream = setLast stream'
    setLast [] = []
    setLast xs = init xs <> [(last xs) {_last = Just 4}]

  pure stream

prop_genRecordProcessorInput :: Property
prop_genRecordProcessorInput = property $ do
  stream <- forAll genRecordProcessorInput
  let
    wCount = _wCount $ _meta $ head stream
    rCount = _rCount $ _meta $ head stream

  footnote ("rCount:" <> show rCount)
  footnote ("wCount:" <> show wCount)
  footnote ("stream Length:" <> show (length stream))

  -- Validate the length
  assert $ length stream == ( fromIntegral wCount + fromEnum (wCount > 0) +
                              fromIntegral rCount + fromEnum (rCount > 0) )
  -- Validate _last
  assert $ _last (last stream) == Just maxBound

prop_recordProcessor_wishbone :: Property
prop_recordProcessor_wishbone = withTests 1000 $ do
  idWithModelSingleDomain @C.System
    defExpectOptions
    genRecordProcessorInput
    (C.exposeClockResetEnable (snd . recordProcessorModel))
    (C.exposeClockResetEnable (ckt @C.System @DataWidth @AddrWidth @DataWidth @WBData))
  where
    ckt :: forall dom dataWidth addrWidth selWidth dat . 
      ( C.HiddenClockResetEnable dom
      , C.KnownNat dataWidth
      , C.KnownNat addrWidth
      , C.BitPack dat
      , C.BitSize dat ~ dataWidth C.* 8
      , selWidth ~ dataWidth
      , Show dat
      )
      => Circuit (PacketStream dom dataWidth RecordHeader)
                 (Df.Df dom (WishboneOperation addrWidth selWidth dat))
    ckt = Circuit go
      where
        go (iFwd, oBwd) = (oFwd, snd iBwd)
          where
            (oFwd, iBwd) = toSignals recordProcessorC (iFwd, (pure (), oBwd))


prop_recordProcessor_bypass :: Property
prop_recordProcessor_bypass = withTests 1000 $ property $ do
  inputs' <- forAll genRecordProcessorInput

  let
    ckt :: forall dom dataWidth addrWidth dat . 
      ( C.HiddenClockResetEnable dom
      , C.KnownNat dataWidth
      , C.KnownNat addrWidth
      , C.BitPack dat
      , C.BitSize dat ~ dataWidth C.* 8
      , C.Show dat
      )
      => Circuit (PacketStream dom dataWidth RecordHeader)
                 (CSignal dom (Maybe (Bypass addrWidth)))
    ckt = Circuit go
      where
        go (iFwd, oBwd) = (oFwd, fst iBwd)
          where
            (oFwd, iBwd) = toSignals (recordProcessorC @_ @_ @_ @dat @dataWidth) (iFwd, (oBwd, pure $ Ack True))

    inputs = map Just inputs'
    inputsS = zip inputs (repeat ())

    res = take (length inputs) $ C.simulate (C.bundle . go . C.unbundle) inputsS
      where
        go = toSignals (C.withClockResetEnable C.clockGen C.resetGen C.enableGen (ckt @C.System @DataWidth @AddrWidth @WBData))

    bypass = map snd res

    modelBypass = fst $ (recordProcessorModel @DataWidth @AddrWidth @WBData) inputs'
  footnote $ "Circit output: " <> show bypass
  footnote $ "Model output:  " <> show modelBypass

  assert $ length bypass == length modelBypass
  assert $ bypass == modelBypass


recordProcessorModel :: forall dataWidth addrWidth dat selWidth .
  ( C.KnownNat dataWidth
  , C.KnownNat addrWidth
  , C.BitPack dat
  , C.BitSize dat ~ dataWidth C.* 8
  , selWidth ~ dataWidth
  , Show dat
  )
  => [PacketStreamM2S dataWidth RecordHeader]
  -> ([Maybe (Bypass addrWidth)], [WishboneOperation addrWidth selWidth dat])
recordProcessorModel inputs = (bypass, wbmInput)
  where
    hdr = _meta $ head inputs
    wCount = fromIntegral $ _wCount hdr
    rCount = _rCount hdr

    writeBase
      | wCount > 0 = C.resize $ C.pack $ _data $ head inputs
      | otherwise  = 0
    inputWrites = take wCount $ tail inputs
    inputReads
      | wCount > 0 = drop (wCount + 2) inputs
      | otherwise  = tail inputs

    createBypass input i = Bypass (_meta input) readBase (_abort input)
      where
        readBase
          | rCount > 0 && i == iBase = Just $ addrConv input
          | otherwise = Nothing
        iBase
          | wCount > 0 = wCount + 1
          | otherwise  = 0
        addrConv = C.resize . C.pack . _data

    createWriteInput input i =
      WishboneOperation addr (Just dat) (C.resize $ _byteEn hdr) (isJust $ _last input) (_abort input) False space
      where
        addr
          | _wff hdr  = writeBase
          | otherwise = writeBase + (i * 4)
        dat = C.bitCoerce $ _data input
        space
          | _wca hdr  = ConfigAddressSpace
          | otherwise = WishboneAddressSpace

    createReadInput input =
      WishboneOperation addr Nothing (C.resize $ _byteEn hdr) (isJust $ _last input) (_abort input) False space
      where
        addr = C.resize $ C.pack $ _data input
        space
          | _rca hdr  = ConfigAddressSpace
          | otherwise = WishboneAddressSpace

    wbmInput' = zipWith createWriteInput inputWrites [0..]
      <> map createReadInput inputReads
    
    -- Set the last operation to drop Cyc. Since the last field is always
    -- @_last@, this is asserted to prevent a lock-up
    wbmInput = init wbmInput' <> [(last wbmInput') {_dropCyc = True}]

    bypass = map Just $ zipWith createBypass inputs [0..]


tests :: TestTree
tests = $(testGroupGenerator)
