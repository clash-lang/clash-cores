{-# LANGUAGE NumericUnderscores #-}

module Test.Cores.Ethernet.Mac.Preamble (
  tests,
) where

import Clash.Cores.Ethernet.Mac.Preamble

import Clash.Prelude

import qualified Data.List as L

import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Protocols.Hedgehog
import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

preambleStripperModel :: [PacketStreamM2S 1 ()] -> [PacketStreamM2S 1 ()]
preambleStripperModel packets = L.concatMap go (chunkByPacket packets)
 where
  go [] = []
  go (x : xs)
    | head (_data x) == 0xD5 = xs
    | otherwise = go xs

prop_preamble_stripper :: Property
prop_preamble_stripper =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoStopAfterEmpty = 1000}
    (genPackets (Range.linear 1 10) Abort genPkt)
    (exposeClockResetEnable preambleStripperModel)
    (exposeClockResetEnable preambleStripperC)
 where
  genPkt am =
    Gen.choice
      [ -- Random valid packet
        genValidPacket (pure ()) (Range.linear 0 20) am
      , -- Valid packet with SFD set somewhere
        do
          packet <- genValidPacket (pure ()) (Range.linear 0 20) am
          idx <- Gen.int (Range.linear 0 (L.length packet - 1))
          pure $
            L.zipWith
              (\f i -> if i == idx then f{_data = singleton 0xD5} else f)
              packet
              [0 ..]
      ]

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
