{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cores.Ethernet.Rgmii where

import Clash.Prelude
import qualified Clash.Explicit.DDR as DDR

import Clash.Cores.Ethernet.Rgmii
import Data.Maybe (catMaybes, isNothing)
import qualified Prelude as P
import Protocols
import Protocols.PacketStream
import Test.Tasty
import Test.Tasty.HUnit

createDomain vSystem{vName = "RgmiiDom", vPeriod = 8000}
createDomain vSystem{vName = "RgmiiDdr", vPeriod = 4000}

iddr ::
  (NFDataX a, BitPack a) =>
  Clock RgmiiDom -> Reset RgmiiDom -> Enable RgmiiDom ->
  Signal RgmiiDdr a -> Signal RgmiiDom (a, a)
iddr clk rst en = DDR.ddrIn clk rst en (unpack 0, unpack 0, unpack 0)

oddr ::
  (NFDataX a, BitPack a) =>
  Clock RgmiiDom -> Reset RgmiiDom -> Enable RgmiiDom ->
  Signal RgmiiDom (a, a) -> Signal RgmiiDdr a
oddr clk rst en = DDR.ddrOut clk rst en (unpack 0)

stream :: (NFDataX a) => [a] -> a -> Signal dom a
stream xs idle = fromList (xs P.++ P.repeat idle)

initialReset :: Reset RgmiiDom
initialReset = unsafeFromActiveHigh $ stream [True, True] False

channel :: [Maybe (BitVector 8, Bool)] -> RgmiiChannel RgmiiDom RgmiiDdr
channel samples = RgmiiChannel clockGen (stream ctl 0) (stream dat 0)
 where
  ctl = P.concatMap (maybe [0, 0] (\(_, err) -> [1, boolToBit (not err)])) samples
  dat = P.concatMap (maybe [0, 0] (\(byte, _) -> let (hi, lo) = split byte in [lo, hi])) samples

receive :: Reset RgmiiDom -> [Maybe (BitVector 8, Bool)] -> [Maybe (PacketStreamM2S 1 ())]
receive resetIn samples = sampleN_lazy 40 output
 where
  (_, output) = withClockResetEnable clockGen resetIn enableGen $
    toSignals (unsafeRgmiiRxC id iddr) (channel samples, pure (PacketStreamS2M True))

transfer :: BitVector 8 -> Maybe (Index 2) -> Bool -> PacketStreamM2S 1 ()
transfer byte lastByte err = PacketStreamM2S (singleton byte) lastByte () err

transmit :: [Maybe (PacketStreamM2S 1 ())] -> [Maybe (BitVector 8, Bool)]
transmit samples = P.zipWith decode ctl dat
 where
  (_, output) = withClockResetEnable clockGen initialReset enableGen $
    toSignals (rgmiiTxC id oddr) (stream (P.replicate 4 Nothing P.++ samples) Nothing, pure ())
  ctl = pairs $ P.drop 1 $ sampleN_lazy 80 $ rgmiiCtl output
  dat = pairs $ P.drop 1 $ sampleN_lazy 80 $ rgmiiData output
  pairs (a : b : rest) = (a, b) : pairs rest
  pairs _ = []
  decode (dv, ctlFall) (lo, hi)
    | dv == 1 = Just (hi ++# lo, ctlFall == 0)
    | otherwise = Nothing

testReceive :: TestTree
testReceive = testGroup "RX"
  [ testCase "single-byte packet" $
      assertEqual "last byte remains valid"
        [transfer 0xA1 (Just 1) False]
        (catMaybes $ receive initialReset $ P.replicate 4 Nothing P.++ [Just (0xA1, False)])
  , testCase "consecutive packets" $
      assertEqual "nibbles and packet boundaries"
        [ transfer 0xA1 Nothing False
        , transfer 0xB2 Nothing False
        , transfer 0xC3 (Just 1) False
        , transfer 0xD4 (Just 1) False
        ]
        (catMaybes $ receive initialReset $ P.replicate 4 Nothing P.++
          [Just (0xA1, False), Just (0xB2, False), Just (0xC3, False), Nothing, Just (0xD4, False)])
  , testGroup "error position"
      [ testCase (P.show position) $
          let errors = [position == i | i <- [0 :: Int .. 2]]
              bytes = [0xA1, 0xB2, 0xC3]
           in assertEqual "abort stays with the corresponding byte"
                (P.zipWith3 transfer bytes [Nothing, Nothing, Just 1] errors)
                (catMaybes $ receive initialReset $
                  P.replicate 4 Nothing P.++ P.map Just (P.zip bytes errors))
      | position <- [0 :: Int .. 2]
      ]
  , testCase "reset during reception" $ do
      let resetIn = unsafeFromActiveHigh $ stream
            ([True, True] P.++ P.replicate 6 False P.++ [True, True, True]) False
          samples = P.replicate 4 Nothing P.++ P.replicate 7 (Just (0xA1, True)) P.++
            P.replicate 4 Nothing P.++ [Just (0xB2, False)]
          received = P.drop 8 $ receive resetIn samples
      assertEqual "reset flushes in-flight data and errors"
        [transfer 0xB2 (Just 1) False] (catMaybes received)
  ]

testTransmit :: TestTree
testTransmit = testGroup "TX"
  [ testCase "full throughput and byte order" $ do
      let sent = transmit
            [ Just $ transfer 0xA1 Nothing False
            , Just $ transfer 0xB2 Nothing True
            , Just $ transfer 0xC3 (Just 1) False
            ]
      assertEqual "one byte per cycle, error on the middle byte"
        [Just (0xA1, False), Just (0xB2, True), Just (0xC3, False), Nothing]
        (P.take 4 $ P.dropWhile isNothing sent)
  , testGroup "empty terminator"
      [ testCase (P.show (dataAbort, emptyAbort)) $
          assertEqual "terminator consumes no byte and preserves abort"
            [(0xA1, dataAbort || emptyAbort)]
            (catMaybes $ transmit
              [ Just $ transfer 0xA1 Nothing dataAbort
              , Just $ transfer (errorX "empty data") (Just 0) emptyAbort
              ])
      | dataAbort <- [False, True], emptyAbort <- [False, True]
      ]
  , testCase "empty packet" $
      assertEqual "empty packets emit no data" [] $
        catMaybes $ transmit [Just $ transfer (errorX "empty data") (Just 0) True]
  , testCase "packet after an empty terminator" $
      assertEqual "following packet is preserved"
        [(0xA1, True), (0xB2, False)] $
        catMaybes $ transmit
          [ Just $ transfer 0xA1 Nothing False
          , Just $ transfer (errorX "empty data") (Just 0) True
          , Nothing
          , Just $ transfer 0xB2 (Just 1) False
          ]
  ]

tests :: TestTree
tests = testGroup "RGMII" [testReceive, testTransmit]
