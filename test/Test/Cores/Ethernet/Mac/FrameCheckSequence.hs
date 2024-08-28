{-# OPTIONS_GHC -Wno-orphans #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}

module Test.Cores.Ethernet.Mac.FrameCheckSequence where

-- base
import Control.Monad
import qualified Data.List as L
import Prelude

-- clash
import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import qualified Clash.Prelude as C

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- clash-protocols
import Protocols
import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog
import Protocols.Hedgehog

-- ethernet
import Clash.Cores.Ethernet.Mac.FrameCheckSequence


$(deriveHardwareCrc Crc32_ethernet C.d8 C.d1)
$(deriveHardwareCrc Crc32_ethernet C.d8 C.d2)
$(deriveHardwareCrc Crc32_ethernet C.d8 C.d4)
$(deriveHardwareCrc Crc32_ethernet C.d8 C.d8)

genVec :: (C.KnownNat n, 1 C.<= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

modelInsert
  :: C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth ()]
modelInsert fragments = insertCrc =<< chunkByPacket fragments

packetToCrcInp
  :: C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth ()] -> [C.BitVector 8]
packetToCrcInp packet = C.head . _data <$> (chopPacket =<< packet)

insertCrc
  :: forall (dataWidth :: C.Nat)
  .  C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth ()]
insertCrc =  upConvert . go . downConvert
  where
    go :: [PacketStreamM2S 1 ()] -> [PacketStreamM2S 1 ()]
    go pkt = pkt''
      where
        crcInp = C.head . _data <$> pkt
        softwareCrc = mkSoftwareCrc Crc32_ethernet C.d8
        crc = digest $ L.foldl' feed softwareCrc  crcInp
        crc' = C.singleton . C.v2bv <$> (C.toList . C.reverse . C.unconcat C.d8 . C.bv2v $ crc)
        lastfmnt = L.last pkt
        pkt' = init pkt L.++ [lastfmnt {_last = Nothing}] L.++ fmap (\dat -> lastfmnt {_data = dat, _last = Nothing}) crc'
        pkt'' = init pkt' ++ [(last pkt'){_last = Just 0}]

-- | Test the fcsinserter
fcsinserterTest
  :: forall n
  .  (1 C.<= n, HardwareCrc Crc32_ethernet 8 n)
  => C.SNat n -> Property
fcsinserterTest C.SNat =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genValidPackets (Range.linear 1 4) (Range.linear 0 30) Abort)
    (C.exposeClockResetEnable modelInsert)
    (C.exposeClockResetEnable (fcsInserterC @C.System @n))

prop_fcsinserter_d1, prop_fcsinserter_d2, prop_fcsinserter_d4, prop_fcsinserter_d8 :: Property
prop_fcsinserter_d1 = fcsinserterTest C.d1
prop_fcsinserter_d2 = fcsinserterTest C.d2
prop_fcsinserter_d4 = fcsinserterTest C.d4
prop_fcsinserter_d8 = fcsinserterTest C.d8

modelValidate
  :: C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth ()]
modelValidate fragments = validateCrc =<< chunkByPacket  fragments

validateCrc
  :: forall (dataWidth :: C.Nat)
  .  C.KnownNat dataWidth
  => 1 C.<= dataWidth
  => [PacketStreamM2S dataWidth ()] -> [PacketStreamM2S dataWidth ()]
validateCrc packet = L.init packet ++ [lastPacketSetAbort]
  where
    lastFragment = last packet
    softwareCrc = mkSoftwareCrc Crc32_ethernet C.d8
    crcBytes =  digest $ L.foldl' feed softwareCrc $ packetToCrcInp packet
    valid = C.complement crcBytes == residue Crc32_ethernet

    lastPacketSetAbort = lastFragment {
      _abort = not valid || _abort lastFragment
    }

-- | Test the fcsvalidator
fcsvalidatorTest
  :: forall n
  .  (1 C.<= n, HardwareCrc Crc32_ethernet 8 n)
  => C.SNat n -> Property
fcsvalidatorTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    inpPackets''  -- Input packets
    (C.exposeClockResetEnable modelValidate)                                             -- Desired behaviour of FcsInserter
    (C.exposeClockResetEnable ckt)                                               -- Implementation of FcsInserter
    (===)                                                                        -- Property to test
  where
    ckt
      :: forall (dom :: C.Domain) (dataWidth :: C.Nat)
      .  C.KnownDomain dom
      => 1 C.<= dataWidth
      => C.HiddenClockResetEnable dom
      => HardwareCrc Crc32_ethernet 8 dataWidth
      => Circuit
        (PacketStream dom dataWidth ())
        (PacketStream dom dataWidth ())
    ckt = fcsValidatorC

    -- This generates the packets
    inpPackets'' = join <$> genPackets'

    genPackets' = do
      fragments <- insertCrc . fullPackets <$> Gen.list (Range.linear 1 100) genPackets
      let packets = chunkByPacket fragments
      forM packets $ \packet -> do
        poison <- Gen.enumBounded :: Gen Bool
        let lastfmnt = last packet
        return $
          if poison
          then init packet L.++ [lastfmnt {_last = Nothing}] L.++ [lastfmnt]
          else packet

    genPackets =
      PacketStreamM2S <$>
      genVec @n Gen.enumBounded <*>
      Gen.maybe Gen.enumBounded <*>
      Gen.enumBounded <*>
      Gen.enumBounded

prop_fcsvalidator_d1, prop_fcsvalidator_d2, prop_fcsvalidator_d4, prop_fcsvalidator_d8 :: Property
prop_fcsvalidator_d1 = fcsvalidatorTest C.d1
prop_fcsvalidator_d2 = fcsvalidatorTest C.d2
prop_fcsvalidator_d4 = fcsvalidatorTest C.d4
prop_fcsvalidator_d8 = fcsvalidatorTest C.d8

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
