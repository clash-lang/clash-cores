{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cores.Ethernet.Rgmii where

import Clash.Prelude
import qualified Clash.Explicit.DDR as DDR

import Clash.Cores.Ethernet.Rgmii
import Data.Maybe (catMaybes, isJust, isNothing)
import qualified Prelude as P
import Protocols
import Protocols.PacketStream
import Test.Tasty
import Test.Tasty.HUnit

createDomain vSystem{vName = "RgmiiDom", vPeriod = 8000}
createDomain vSystem{vName = "RgmiiDdr", vPeriod = 4000}

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
receive resetIn samples = receiveSafe resetIn samples []

receiveSafe ::
  Reset RgmiiDom -> [Maybe (BitVector 8, Bool)] -> [Bool] ->
  [Maybe (PacketStreamM2S 1 ())]
receiveSafe resetIn samples ready = sampleN_lazy 100 output
 where
  (_, output) = withClockResetEnable clockGen resetIn enableGen $
    toSignals (rgmiiRxC id DDR.ddrIn)
      (channel samples, PacketStreamS2M <$> stream ready True)

accepted :: [Bool] -> [Maybe a] -> [a]
accepted ready output = catMaybes $
  P.zipWith (\r x -> if r then x else Nothing) (ready P.++ P.repeat True) output

assertStable :: [Bool] -> [Maybe (PacketStreamM2S 1 ())] -> Assertion
assertStable ready output = sequence_
  [ assertEqual ("stalled transfer at cycle " P.++ P.show i) curr next
  | (i, (r, curr, next)) <- P.zip [0 :: Int ..] $
      P.zip3 (ready P.++ P.repeat True) output (P.drop 1 output)
  , not r, isJust curr
  ]

transfer :: BitVector 8 -> Maybe (Index 2) -> Bool -> PacketStreamM2S 1 ()
transfer byte lastByte err = PacketStreamM2S (singleton byte) lastByte () err

transmit :: [Maybe (PacketStreamM2S 1 ())] -> [Maybe (BitVector 8, Bool)]
transmit samples = P.zipWith decode ctl dat
 where
  (_, output) = withClockResetEnable clockGen initialReset enableGen $
    toSignals (rgmiiTxC @RgmiiDom @RgmiiDdr id DDR.ddrOut)
      (stream (P.replicate 4 Nothing P.++ samples) Nothing, pure ())
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
        [transfer 0xA1 Nothing False, transfer 0 (Just 0) False]
        (catMaybes $ receive initialReset $ P.replicate 4 Nothing P.++ [Just (0xA1, False)])
  , testCase "consecutive packets" $
      assertEqual "nibbles and packet boundaries"
        [ transfer 0xA1 Nothing False
        , transfer 0xB2 Nothing False
        , transfer 0xC3 Nothing False
        , transfer 0 (Just 0) False
        , transfer 0xD4 Nothing False
        , transfer 0 (Just 0) False
        ]
        (catMaybes $ receive initialReset $ P.replicate 4 Nothing P.++
          [Just (0xA1, False), Just (0xB2, False), Just (0xC3, False), Nothing, Just (0xD4, False)])
  , testGroup "error position"
      [ testCase (P.show position) $
          let errors = [position == i | i <- [0 :: Int .. 2]]
              bytes = [0xA1, 0xB2, 0xC3]
           in assertEqual "abort stays with the corresponding byte"
                (P.zipWith3 transfer bytes (P.repeat Nothing) errors P.++
                  [transfer 0 (Just 0) False])
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
        [transfer 0xB2 Nothing False, transfer 0 (Just 0) False] (catMaybes received)
  ]

testReceiveBackpressure :: TestTree
testReceiveBackpressure = testGroup "RX with backpressure"
  [ testCase "packets end with empty transfers" $
      assertEqual "data, errors, and packet boundaries"
        [ transfer 0xA1 Nothing False
        , transfer 0xB2 Nothing True
        , transfer 0xC3 Nothing False
        , end False
        , transfer 0xD4 Nothing False
        , end False
        ]
        (catMaybes $ receiveSafe initialReset samples [])
  , testGroup "stall data transfer"
      [ testCase (P.show (position, duration)) $ do
          let cycleIn = dataCycles P.!! position
              ready = P.replicate cycleIn True P.++ P.replicate duration False
              output = receiveSafe initialReset samples ready
          assertEqual "accepted prefix, abort, and recovery"
            (P.take (position + 1) firstPacket P.++
              [end True, transfer 0xD4 Nothing False, end False])
            (accepted ready output)
          assertStable ready output
      | position <- [0 :: Int .. 2], duration <- [1, 4]
      ]
  , testCase "abort terminator waits for acceptance" $ do
      let cycleIn = firstDataCycle
          ready = P.replicate cycleIn True P.++ [False, False, True] P.++
            P.replicate 4 False
          output = receiveSafe initialReset samples ready
      assertEqual "exactly one abort is accepted"
        [transfer 0xA1 Nothing False, end True, transfer 0xD4 Nothing False, end False]
        (accepted ready output)
      assertEqual "abort remains asserted throughout the stall"
        (P.replicate 5 $ Just $ end True)
        (P.take 5 $ P.drop (cycleIn + 3) output)
      assertStable ready output
  , testGroup "stall across another packet"
      [ testCase (if stallEnd then "normal terminator" else "data") $ do
          let input = P.replicate 4 Nothing P.++
                P.map (\b -> Just (b, False)) [0xA1, 0xB2, 0xC3] P.++
                P.replicate 12 Nothing P.++
                P.map (\b -> Just (b, False)) [0xD1, 0xD2, 0xD3, 0xD4, 0xD5] P.++
                P.replicate 12 Nothing P.++ [Just (0xE1, False)]
              baseline = receiveSafe initialReset input []
              cycleIn = if stallEnd then firstDataCycle + 3 else firstDataCycle
              release = case [i | (i, Just p) <- P.zip [0..] baseline,
                _last p == Nothing, head (_data p) == 0xD3] of
                  i : _ -> i
                  [] -> error "missing recovery packet"
              ready = P.replicate cycleIn True P.++ P.replicate (release - cycleIn) False
              output = receiveSafe initialReset input ready
              prefix = if stallEnd
                then P.map (\b -> transfer b Nothing False) [0xA1, 0xB2, 0xC3] P.++ [end False]
                else [transfer 0xA1 Nothing False, end True]
          assertEqual "the partially discarded packet is not resumed"
            (prefix P.++ [transfer 0xE1 Nothing False, end False])
            (accepted ready output)
          assertStable ready output
      | stallEnd <- [False, True]
      ]
  , testCase "idle output does not inspect ready" $ do
      let baseline = receiveSafe initialReset samples []
          ready = P.map (maybe (errorX "ready while idle") (const True)) baseline
      assertEqual "undefined idle backpressure is ignored"
        baseline (receiveSafe initialReset samples ready)
  , testCase "reset clears a pending abort" $ do
      let cycleIn = firstDataCycle
          ready = P.replicate cycleIn True P.++ [False, True] P.++ P.replicate 6 False
          resetCycle = cycleIn + 3
          resetIn = unsafeFromActiveHigh $ stream
            ([True, True] P.++ P.replicate (resetCycle - 2) False P.++ [True, True]) False
          output = receiveSafe resetIn samples ready
      assertEqual "reset suppresses transfers" [Nothing, Nothing]
        (P.take 2 $ P.drop resetCycle output)
      assertEqual "no stale abort after reset"
        [transfer 0xD4 Nothing False, end False]
        (catMaybes $ P.drop resetCycle output)
  ]
 where
  samples = P.replicate 4 Nothing P.++
    [Just (0xA1, False), Just (0xB2, True), Just (0xC3, False)] P.++
    P.replicate 12 Nothing P.++ [Just (0xD4, False)]
  firstPacket =
    [transfer 0xA1 Nothing False, transfer 0xB2 Nothing True, transfer 0xC3 Nothing False]
  dataCycles = [i | (i, Just p) <- P.zip [0 :: Int ..] $ receiveSafe initialReset samples [],
    _last p == Nothing]
  firstDataCycle = case dataCycles of
    i : _ -> i
    [] -> error "missing received data"
  end = transfer 0 (Just 0)

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
tests = testGroup "RGMII" [testReceive, testReceiveBackpressure, testTransmit]
