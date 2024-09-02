{-# LANGUAGE NumericUnderscores #-}

module Test.Cores.Ethernet.Mac.PaddingInserter (
  tests,
) where

import Clash.Cores.Ethernet.Mac.PaddingInserter

import Clash.Prelude

import qualified Data.List as L

import Hedgehog (Property)
import qualified Hedgehog.Range as Range

import Protocols.Hedgehog
import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

paddingInserterModel ::
  forall (dataWidth :: Nat).
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Int ->
  [PacketStreamM2S dataWidth ()] ->
  [PacketStreamM2S dataWidth ()]
paddingInserterModel padBytes fragments =
  L.concatMap
    (upConvert . fullPackets . insertPadding)
    (chunkByPacket $ downConvert fragments)
 where
  padding =
    PacketStreamM2S
      { _data = repeat 0x00
      , _last = Nothing
      , _meta = ()
      , _abort = False
      }

  insertPadding xs =
    L.init xs
      L.++ ( (L.last xs){_last = Nothing}
              : L.replicate (max 0 (padBytes - L.length xs)) padding
           )

-- | Test the padding inserter.
paddingInserterTest ::
  forall dataWidth padBytes.
  (KnownNat padBytes) =>
  (1 <= dataWidth) =>
  (1 <= padBytes) =>
  SNat dataWidth ->
  SNat padBytes ->
  Property
paddingInserterTest SNat padBytes =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets (Range.linear 1 10) Abort (genValidPacket (pure ()) (Range.linear 0 10)))
    (exposeClockResetEnable (paddingInserterModel $ natToNum @padBytes))
    (exposeClockResetEnable (paddingInserterC @dataWidth padBytes))

-- | dataWidth ~ padBytes
prop_paddinginserter_d1 :: Property
prop_paddinginserter_d1 = paddingInserterTest d1 d1

-- | dataWidth % padBytes ~ 0
prop_paddinginserter_d2 :: Property
prop_paddinginserter_d2 = paddingInserterTest d2 d26

-- | dataWidth % padBytes > 0
prop_paddinginserter_d7 :: Property
prop_paddinginserter_d7 = paddingInserterTest d7 d26

-- | dataWidth > padBytes
prop_paddinginserter_d20 :: Property
prop_paddinginserter_d20 = paddingInserterTest d20 d10

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
