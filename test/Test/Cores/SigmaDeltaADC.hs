{- |
  Copyright   :  (C) 2026, Rowan Goemans
  License     :  BSD2 (see the file LICENSE)

  Compare the ADC against an integer block-sum model, including its latency.
-}
module Test.Cores.SigmaDeltaADC (tests) where

import Clash.Cores.SigmaDeltaADC
import qualified Clash.Prelude as C
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude

type Adc n =
  C.Clock C.System ->
  C.Reset C.System ->
  C.Enable C.System ->
  C.Signal C.System (C.BitVector n) ->
  ( C.Signal C.System (C.BitVector n)
  , C.Signal C.System (C.BitVector 3)
  , C.Signal C.System Bool
  )

serial :: Adc 1
serial = C.exposeClockResetEnable (sigmaDeltaADC @3 @1 @1 @0 (C.SNat @4) (C.SNat @1))

unfiltered :: Adc 1
unfiltered = C.exposeClockResetEnable (sigmaDeltaADC @3 @1 @0 @0 (C.SNat @4) (C.SNat @0))

parallel :: Adc 4
parallel = C.exposeClockResetEnable (sigmaDeltaADC @3 @1 @2 @3 (C.SNat @4) (C.SNat @2))

-- Smallest supported accumulator relative to a four-bit input word.
minimumWidth :: Adc 4
minimumWidth = C.exposeClockResetEnable (sigmaDeltaADC @3 @0 @0 @3 (C.SNat @3) (C.SNat @0))

runAdc ::
  (C.KnownNat n) =>
  Adc n ->
  [Bool] ->
  [Bool] ->
  [C.BitVector n] ->
  [(C.BitVector n, C.BitVector 3, Bool)]
runAdc adc resets enables input = C.sampleN (length input) (C.bundle adcOutput)
 where
  adcOutput =
    adc
      C.clockGen
      (C.unsafeFromActiveHigh (C.fromList (resets ++ repeat False)))
      (C.toEnable (C.fromList (enables ++ repeat True)))
      (C.fromList (input ++ repeat 0))

chunks :: Int -> [a] -> [[a]]
chunks n xs
  | length prefix == n = prefix : chunks n suffix
  | otherwise = []
 where
  (prefix, suffix) = splitAt n xs

-- Independent arithmetic model: saturate and truncate each conversion,
-- then average complete groups of conversions using unbounded integers.
reference :: (C.KnownNat n) => Int -> Int -> Int -> [C.BitVector n] -> [Integer]
reference width extra depth =
  map ((`div` (2 ^ depth)) . sum)
    . chunks (2 ^ depth)
    . map ((`div` (2 ^ extra)) . min (2 ^ (3 + extra) - 1) . sum . map (toInteger . C.popCount))
    . chunks (2 ^ (3 + extra) `div` width)

checkModel ::
  (C.KnownNat n) =>
  Adc n -> Int -> Int -> Int -> [C.BitVector n] -> Property
checkModel adc width extra depth input =
  counterexample ("actual: " ++ show actual ++ "\nexpected: " ++ show expected) $
    actual === expected
 where
  block = 2 ^ (3 + extra + depth) `div` width
  expectedValues = reference width extra depth input
  inputWithFlush = input ++ [0, 0]
  actual = runAdc adc [] [] inputWithFlush
  expected = zipWith3 (,,) (0 : init inputWithFlush) values ready
  values =
    [ if i < block + 1
        then 0
        else fromInteger (expectedValues !! ((i - 1) `div` block - 1))
    | i <- [0 .. length inputWithFlush - 1]
    ]
  ready = [i > 1 && (i - 1) `mod` block == 0 | i <- [0 .. length inputWithFlush - 1]]

validValues :: [(a, b, Bool)] -> [b]
validValues xs = [value | (_, value, True) <- xs]

tests :: TestTree
tests =
  testGroup
    "SigmaDeltaADC"
    [ testProperty "serial samples, full first window and aligned ready" $
        forAll (vectorOf 128 (elements [0, 1])) (checkModel serial 1 1 1)
    , testProperty "zero filter depth passes every complete conversion" $
        forAll (vectorOf 128 (elements [0, 1])) (checkModel unfiltered 1 1 0)
    , testProperty "four samples per cycle and four-conversion average" $
        forAll (vectorOf 128 (elements [0 .. 15])) (checkModel parallel 4 1 2)
    , testProperty "minimum accumulator width and no discarded bits" $
        forAll (vectorOf 128 (elements [0 .. 15])) (checkModel minimumWidth 4 0 0)
    , testCase "zero, half and full scale, including first result" $ do
        validValues (runAdc serial [] [] (replicate 130 0)) @?= replicate 4 0
        validValues (runAdc serial [] [] (take 130 (cycle [0, 1]))) @?= replicate 4 4
        validValues (runAdc serial [] [] (replicate 130 1)) @?= replicate 4 7
        validValues (runAdc parallel [] [] (replicate 66 15)) @?= replicate 4 7
    , testProperty "last sample in each conversion is included" $
        checkModel unfiltered 1 1 0 (concat (replicate 4 (replicate 14 0 ++ [1, 1])))
    , testCase "reset discards a partial conversion and filter window" $ do
        let resetAt = 27
            resetCycles = 3
            suffix = take 100 (cycle [0, 1])
            input = replicate (resetAt + resetCycles) 1 ++ suffix
            resets = replicate resetAt False ++ replicate resetCycles True
            afterReset = drop (resetAt + resetCycles) (runAdc serial resets [] input)
        afterReset @?= runAdc serial [] [] suffix
    , testProperty "enable pauses feedback, conversion, filter and valid flag" $
        forAll (vectorOf 160 arbitrary) $ \enables ->
          forAll (vectorOf 160 (elements [0 .. 15])) $ \input ->
            let activeInput = [x | (x, True) <- zip input enables]
                actual = [x | (x, True) <- zip (runAdc parallel [] enables input) enables]
             in actual === runAdc parallel [] [] activeInput
    , testCase "sample-rate helper includes parallel input and filter depth" $ do
        sigmaDeltaSampleRate (C.SNat @12) (C.SNat @2) (C.SNat @4) 100000000 @?= 24414.0625
        sigmaDeltaSampleRate (C.SNat @4) (C.SNat @0) (C.SNat @1) 1600 @?= 100
    ]
