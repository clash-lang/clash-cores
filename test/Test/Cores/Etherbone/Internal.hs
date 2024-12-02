{-# LANGUAGE NamedFieldPuns #-}

module Test.Cores.Etherbone.Internal where
import qualified Clash.Prelude as C
import Hedgehog
import Clash.Cores.Etherbone.Base
import Prelude
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type WBData = C.BitVector 32
type DataWidth = 4
type AddrWidth = 32

-- Read / write values used in the tests
readVal :: WBData
readVal = 0x55555555
writeVal :: WBData
writeVal = 0xaaaaaaaa

genRecordHeader
  :: Range.Range (C.Unsigned 8)
  -> Range.Range (C.Unsigned 8)
  -> Gen RecordHeader
genRecordHeader wRange rRange = do
  _wCount' :: C.Unsigned 8 <- Gen.integral wRange
  _rCount' :: C.Unsigned 8 <- Gen.integral rRange

  -- Ensure that there is at least one read or one write
  ( _rCount, _wCount ) <- Gen.filter (\(r, w) -> (r+w) > 0) $ do
    return (_rCount', _wCount')

  _cyc <- Gen.bool
  _readConfigAddr <- Gen.bool
  _writeConfigAddr <- Gen.bool
  _writeFifo <- Gen.bool

  let
    _baseConfigAddr = False
    _readFifo = False
    _res0 = 0
    _res1 = 0
    _byteEn = C.resize $ C.pack $ C.replicate (C.SNat @DataWidth) (1::C.Bit)

  pure RecordHeader
      { _baseConfigAddr
      , _readConfigAddr
      , _readFifo
      , _res0
      , _cyc
      , _writeConfigAddr
      , _writeFifo
      , _res1
      , _byteEn
      , _wCount
      , _rCount
      }
