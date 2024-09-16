{-# language FlexibleContexts #-}
{-# language NumericUnderscores #-}
{-# language RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Test.Cores.Ethernet.InternetChecksum where

-- base
import Data.Maybe
import qualified Data.List as L
import Data.Proxy
import Numeric ( showHex )

-- clash-prelude
import Clash.Prelude

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog ( HedgehogTestLimit(HedgehogTestLimit) )
import Test.Tasty.Hedgehog.Extra ( testProperty )
import Test.Tasty.TH ( testGroupGenerator )

-- ethernet
import Clash.Cores.Ethernet.InternetChecksum

import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

import qualified Clash.Sized.Vector as Vec


uncurryS ::
  (Signal dom a -> Signal dom b -> Signal dom c)
  -> (Signal dom (a, b) -> Signal dom c)
uncurryS f a = f (fst <$> a) (snd <$> a)

genVec :: (KnownNat n, 1 <= n) => Gen a -> Gen (Vec n a)
genVec gen = sequence (repeat gen)

genWord :: Gen (BitVector 16)
genWord = pack <$> genVec Gen.bool

genWordVec ::  (KnownNat n, 1 <= n) => Gen (Vec n (BitVector 16))
genWordVec = genVec genWord

-- functions used to print the intermediate state for debugging
showAsHex :: [BitVector 16] -> [String]
showAsHex = fmap (showSToString . Numeric.showHex . toInteger)
  where
    showSToString showS = showS ""

showComplementAsHex :: [BitVector 16] -> [String]
showComplementAsHex = showAsHex . fmap complement

flipBit :: Int -> Int -> [(Bool, Maybe (BitVector 16))] -> [(Bool, Maybe (BitVector 16))]
flipBit listIndex bitIndex bitList = replaceAtIndex listIndex newWord bitList
  where
    replaceAtIndex :: Int -> a -> [a] -> [a]
    replaceAtIndex n item ls = a L.++ (item : L.drop 1 b) where (a, b) = L.splitAt n ls

    newWord = fb <$> (bitList L.!! listIndex)

    fb Nothing = Nothing
    fb (Just word) = Just (complementBit word bitIndex)

checkZeroAfterReset :: Int -> [(Bool, Maybe a)] -> [BitVector 16] -> Bool
checkZeroAfterReset _ [] _ = True
checkZeroAfterReset _ _ [] = False
checkZeroAfterReset d ((True, _):xs) yl@(_:ys) =
  checkZeroAfterdelayCycles d yl && checkZeroAfterReset d xs ys
  where
    checkZeroAfterdelayCycles :: Int -> [BitVector 16] -> Bool
    checkZeroAfterdelayCycles _ [] = False
    checkZeroAfterdelayCycles 0 (z:_) = z == 0x0
    checkZeroAfterdelayCycles r (_:zs) = checkZeroAfterdelayCycles (r-1) zs
checkZeroAfterReset d (_:xs) (_:ys) = checkZeroAfterReset d xs ys

extendInput :: Int -> [(Bool, Maybe x)] -> [(Bool, Maybe x)]
extendInput delayCycles input = input L.++ L.replicate delayCycles (False, Nothing)

-- | Pure implementation of the RFC1079 internet checksum. Takes complement of
-- final outcome, unlike some components!
pureInternetChecksum :: Foldable t => t (BitVector 16) -> BitVector 16
pureInternetChecksum = complement . fromInteger . L.foldr (pureOnesComplementAdd . toInteger) 0

-- | Pure 16-bit one's complement sum for integers. Assumes that @a@ can store
-- large enough integers. Use something like `Int`, not `BitVector 16`.
pureOnesComplementAdd :: Integral a => a -> a -> a
pureOnesComplementAdd a b = (a + b) `mod` 65_536 + (a + b) `div` 65_536

alignTo :: Int -> a -> [a] -> [a]
alignTo n a xs = xs L.++ L.replicate (n - mod (L.length xs) n) a

{- |
Like 'pureInternetChecksum', but over a packet stream.
Assumes that there is only one packet in the input stream.
-}
calculateChecksum ::
  forall dataWidth meta.
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  [PacketStreamM2S dataWidth meta] ->
  BitVector 16
calculateChecksum fragments = checksum
 where
  dataToList PacketStreamM2S{..} = L.take validData $ Vec.toList _data
   where
    validData = 1 + fromIntegral (fromMaybe maxBound _last)
  checksum =
    pureInternetChecksum $
      fmap (pack . Vec.unsafeFromList @2) $
        chopBy 2 $
          alignTo 2 0x00 $
            L.concatMap dataToList fragments

-- Tests the one's complement sum
prop_onescomplementadd :: Property
prop_onescomplementadd = property $ do
  a <- forAll $ Gen.int (Range.linear 0 65_536)
  b <- forAll $ Gen.int (Range.linear 0 65_536)
  let c = pureOnesComplementAdd a b
  onesComplementAdd (fromIntegral a) (fromIntegral b) === fromIntegral c

-- Checks whether the checksum succeeds
prop_checksum_succeed :: Property
prop_checksum_succeed =
  property $ do
    let genInputList range = Gen.list range $ (,) False <$> Gen.maybe genWord

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = L.length input

    let checkSum = complement $ L.last $ L.take (size + 1) $
                    simulate @System (uncurryS internetChecksum) input
        input' = input L.++ [(False, Just checkSum)]
        checkSum' = L.last $ L.take (size + 2) $
                      simulate @System (uncurryS internetChecksum) input'

    checkSum' === 0xFFFF

-- | Flips a random bit and checks whether the checksum actually fails
prop_checksum_fail :: Property
prop_checksum_fail =
  property $ do
    let genInputList range = Gen.list range $ (,) False <$> (Just <$> genWord)

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = L.length input

    randomIndex <- forAll $ Gen.int (Range.linear 0 (size - 1))
    randomBitIndex <- forAll $ Gen.int (Range.linear 0 (16 - 1))

    let checkSum = complement $ L.last $ L.take (size + 1) $
                    simulate @System (uncurryS internetChecksum) input
        input' = flipBit randomIndex randomBitIndex $ input L.++ [(False, Just checkSum)]
        checkSum' = L.last $ L.take (size + 2) $ simulate @System (uncurryS internetChecksum) input'

    checkSum' /== 0xFFFF

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_specific_values :: Property
prop_checksum_specific_values =
  property $ do
    let input = (False,) . Just <$> [0x4500, 0x0073, 0x0000, 0x4000, 0x4011, 0x0000, 0xc0a8, 0x0001, 0xc0a8, 0x00c7]
        size = L.length input
        result = L.take (size + 1) $
          simulate @System (uncurryS internetChecksum) input
        checkSum = L.last result

    footnoteShow $ showAsHex result
    complement checkSum === 0xb861

-- | testing whether the value returns to 0 after a reset
prop_checksum_reset :: Property
prop_checksum_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) ((,) <$> Gen.bool <*> Gen.maybe genWord)

    input <- forAll genInputList
    let size = L.length input
        result = L.take (size + 1) $
          simulate @System (uncurryS internetChecksum) input

    footnoteShow $ showAsHex result
    assert $ checkZeroAfterReset 1 input result

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_reduce_specific_values :: Property
prop_checksum_reduce_specific_values =
  property $ do
    let input = (False,) . Just <$> [
          0x4500 :> 0x0073 :> 0x0000 :> Nil,
          0x4000 :> 0x4011 :> 0xc0a8 :> Nil,
          0x0001 :> 0xc0a8 :> 0x00c7 :> Nil
          ]
        size = L.length input
        result = L.take (size + 1) $
          simulate @System (uncurryS reduceToInternetChecksum) input
        checkSum = L.last result

    footnote $ "full output: " L.++ show (showAsHex result)
    checkSum === 0x479e

prop_checksum_reduce_succeed :: Property
prop_checksum_reduce_succeed =
  property $ do
    let genInputList range = Gen.list range ((,) False <$> Gen.maybe (genWordVec @5))

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = L.length input

    let result = simulate @System (uncurryS reduceToInternetChecksum) input
        checkSum = complement $ L.last $ L.take (size + 1) result
        input' = input L.++ [(False, Just (checkSum :> 0x0 :> 0x0 :> 0x0 :> 0x0 :> Nil))]
        checkSum' = L.last $ L.take (size + 2) $
          simulate @System (uncurryS reduceToInternetChecksum) input'

    checkSum' === 0xFFFF

prop_checksum_reduce_reset :: Property
prop_checksum_reduce_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) ((,) <$> Gen.bool <*>  Gen.maybe (genWordVec @5))

    input <- forAll genInputList
    let size = L.length input
        result = L.take (size + 1) $ simulate @System (uncurryS reduceToInternetChecksum) input

    assert $ checkZeroAfterReset 1 input result

-- | testing the example from wikipedia: https://en.wikipedia.org/wiki/Internet_checksum
prop_checksum_pipeline_specific_values :: Property
prop_checksum_pipeline_specific_values =
  property $ do
    let input = (False,) . Just <$> [
          0x4500 :> 0x0073 :> 0x0000 :> Nil,
          0x4000 :> 0x4011 :> 0xc0a8 :> Nil,
          0x0001 :> 0xc0a8 :> 0x00c7 :> Nil
          ]
        delayCycles = fromInteger $ natVal (Proxy :: Proxy (InternetChecksumLatency 4)) + 1
        size = L.length input
        result = L.take (size + delayCycles) $
          simulate @System (uncurryS pipelinedInternetChecksum) (extendInput delayCycles input)
        checkSum = L.last result

    footnote $ "full output: " L.++ show (showAsHex result)
    checkSum === 0x479e

prop_checksum_pipeline_succeed :: Property
prop_checksum_pipeline_succeed =
  property $ do
    let genInputList range = Gen.list range ((False,) <$> Gen.maybe (genWordVec @5))
        delayCycles = fromInteger $ natVal (Proxy :: Proxy (InternetChecksumLatency 5))

    input <- forAll $ genInputList (Range.linear 1 100)
    let size = L.length input

    let result = simulate @System (uncurryS pipelinedInternetChecksum) (extendInput delayCycles input)
        checkSum = complement $ L.last $ L.take (size + delayCycles) result
        input' = input
          L.++ [(False, Just (checkSum :> 0x0 :> 0x0 :> 0x0 :> 0x0 :> Nil))]
          L.++ extendInput delayCycles input
        checkSum' = L.last $ L.take (size + delayCycles + 1) $
          simulate @System (uncurryS pipelinedInternetChecksum) input'

    footnoteShow $ showAsHex $ L.take (size + delayCycles) result

    checkSum' === 0xFFFF

prop_checksum_pipeline_reset :: Property
prop_checksum_pipeline_reset =
  property $ do
    let genInputList = Gen.list (Range.linear 1 100) ((,) <$> Gen.bool <*> Gen.maybe (genWordVec @7))
        delayCycles = fromInteger $ natVal (Proxy :: Proxy (InternetChecksumLatency 7))
    input <- forAll genInputList
    let size = L.length input
        result = L.take (size + delayCycles) $
          simulate @System (uncurryS pipelinedInternetChecksum)
          (input L.++ extendInput delayCycles input)

    footnoteShow $ showAsHex result

    assert $ checkZeroAfterReset delayCycles input result

tests :: TestTree
tests =
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1_000))
  $(testGroupGenerator)
