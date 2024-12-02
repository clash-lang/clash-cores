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
import Clash.Cores.Etherbone.RecordProcessor (recordProcessorC)
import Test.Cores.Etherbone.Internal

-- TODO: Add multi-record tests

genRecordProcessorInput :: Gen [PacketStreamM2S DataWidth (Bool, RecordHeader)]
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

    pkt :: WBData -> PacketStreamM2S DataWidth (Bool, RecordHeader)
    pkt dat = PacketStreamM2S (C.bitCoerce dat) Nothing (False, hdr) False

    stream' = map pkt rawData
    stream = setLast stream'
      where
        setLast [] = []
        setLast xs = init xs <> [lastPs (last xs)]
        lastPs x = x {_last = Just maxBound, _meta = (True, snd $ _meta x)}

  pure stream

prop_genRecordProcessorInput :: Property
prop_genRecordProcessorInput = property $ do
  stream <- forAll genRecordProcessorInput
  let
    hdr = snd $ _meta $ head stream
    wCount = _wCount hdr
    rCount = _rCount hdr

  footnote ("rCount:" <> show rCount)
  footnote ("wCount:" <> show wCount)
  footnote ("stream Length:" <> show (length stream))

  -- Validate the length
  assert $ length stream == ( fromIntegral wCount + fromEnum (wCount > 0) +
                              fromIntegral rCount + fromEnum (rCount > 0) )
  -- Validate _last
  assert $ _last (last stream) == Just maxBound

-- Check whether the WishboneOperations are formatted correctly. Also crudely
-- checks the bypass signal to see if there is a base address when needed
prop_recordProcessor_wishbone :: Property
prop_recordProcessor_wishbone =
  idWithModelSingleDomain @C.System
    defExpectOptions {eoSampleMax = 256}
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
      => Circuit (PacketStream dom dataWidth (Bool, RecordHeader))
                 (Df.Df dom (WishboneOperation addrWidth selWidth dat))
    ckt = Circuit go
      where
        go (iFwd, oBwd) = (iBwd, snd oFwd)
          where
            (iBwd, oFwd) = toSignals recordProcessorC (iFwd, (pure (), oBwd))

-- Check whether the Bypass data returned is formatted correctly. This does not
-- fully test the backpressure behaviour.
prop_recordProcessor_bypass :: Property
prop_recordProcessor_bypass = property $ do
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
      => Circuit (PacketStream dom dataWidth (Bool, RecordHeader))
                 (CSignal dom (Maybe(Bypass addrWidth)))
    ckt = Circuit go
      where
        go (iFwd, oBwd) = (iBwd, fst oFwd)
          where
            (iBwd, oFwd) = toSignals (recordProcessorC @_ @_ @_ @dat @dataWidth) (iFwd, (oBwd, pure $ Ack True))

    inputs = map Just inputs'
    inputsS = zip inputs (repeat ())

    res = take (length inputs) $ C.simulate (C.bundle . go . C.unbundle) inputsS
      where
        go = toSignals (C.withClockResetEnable C.clockGen C.resetGen C.enableGen (ckt @C.System @DataWidth @AddrWidth @WBData))

    bypass = map (fromJust . snd) res

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
  => [PacketStreamM2S dataWidth (Bool, RecordHeader)]
  -> ([Bypass addrWidth], [WishboneOperation addrWidth selWidth dat])
recordProcessorModel inputs = (bypass, wbmInput)
  where
    meta = _meta $ head inputs
    hdr = snd meta
    wCount = fromIntegral $ _wCount hdr
    rCount = _rCount hdr

    writeBase
      | wCount > 0 = C.resize $ C.pack $ _data $ head inputs
      | otherwise  = 0
    inputWrites = take wCount $ tail inputs
    inputReads
      | wCount > 0 = drop (wCount + 2) inputs
      | otherwise  = tail inputs

    createBypass input i = Bypass (snd $ _meta input) readBase (_abort input)
      where
        readBase
          | rCount > 0 && i == iBase = Just $ addrConv input
          | otherwise = Nothing
        iBase
          | wCount > 0 = wCount + 1
          | otherwise  = 0
        addrConv = C.resize . C.pack . _data

    createWriteInput input i
      = WishboneOperation
        { _opAddr = addr
        , _opDat = Just dat
        , _opSel = C.resize $ _byteEn hdr
        , _opDropCyc = False
        , _opAddrSpace = space
        , _opEOR = isJust $ _last input
        -- XXX: Only one record per packet is tested here. So _opEOR = _opEOP
        , _opEOP = isJust $ _last input
        , _opAbort = _abort input
        }
      where
        addr
          | _writeFifo hdr = writeBase
          | otherwise      = writeBase + (i * 4)
        dat = C.bitCoerce $ _data input
        space
          | _writeConfigAddr hdr = ConfigAddressSpace
          | otherwise            = WishboneAddressSpace

    createReadInput input
      = WishboneOperation
        { _opAddr = addr
        , _opDat = Nothing
        , _opSel = C.resize $ _byteEn hdr
        , _opDropCyc = False
        , _opAddrSpace = space
        , _opEOR = isJust $ _last input
        -- XXX: Only one record per packet is tested here. So _opEOR = _opEOP
        , _opEOP = isJust $ _last input
        , _opAbort = _abort input
        }
      where
        addr = C.resize $ C.pack $ _data input
        space
          | _readConfigAddr hdr = ConfigAddressSpace
          | otherwise           = WishboneAddressSpace

    wbmInput'
      = zipWith createWriteInput inputWrites [0..]
      <> map createReadInput inputReads

    -- Set the last operation to drop Cyc.
    -- The last fragment should always drop CYC to prevent a lock-up.
    wbmInput = init wbmInput' <> [(last wbmInput') {_opDropCyc = True}]

    bypass = zipWith createBypass inputs [0..]

tests :: TestTree
tests = $(testGroupGenerator)
