module Test.Cores.Sgmii.BitSlip where

import Clash.Cores.Sgmii.BitSlip
import Clash.Cores.Sgmii.Common
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Data.List (find, isInfixOf)
import Data.Maybe (isJust, isNothing)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cores.LineCoding.Lc8b10b
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Version of 'bitSlip' that also outputs the current state, used to check
--   if the correct state has been reached
bitSlipSim ::
  forall dom.
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (C.BitVector 10) ->
  C.Signal dom (BitSlipState, C.BitVector 10, Status)
bitSlipSim cg =
  C.bundle $
    C.mooreB
      bitSlipT
      bitSlipO
      (BSFail (0, 0) (C.repeat 0) (C.repeat 0))
      (cg, pure Ok)

-- | Check that if 'bitSlip' moves into 'BSOk', the index is non-zero as it
--   needs to be over code group boundaries due to 'checkBitSequence'
prop_bitSlipNoBSOk :: H.Property
prop_bitSlipNoBSOk = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp <-
    H.forAll
      ( Gen.list
          (Range.singleton simDuration)
          (Gen.filter checkBitSequence genDefinedBitVector)
      )
  let simOut =
        map f $
          drop 1 $
            C.sampleN
              (simDuration + 1)
              (bitSlipSim @C.System (C.fromList (0 : inp)))
       where
        f (s, _, _) = s

  H.assert $ isNothing $ find g simOut
 where
  g (BSOk _ 0) = True
  g _ = False

-- | Check that with the comma at the third index, the output is equal to a
--   shifted version of the input, and the comma is actually at the third index
prop_bitSlipInOutCorrect :: H.Property
prop_bitSlipInOutCorrect = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 10 100))

  inp1 <-
    H.forAll
      ( Gen.list
          (Range.singleton simDuration)
          ( Gen.filter
              (\a -> isValidCodeGroup a && checkBitSequence a)
              genDefinedBitVector
          )
      )
  let inp2 = concatMap (\a -> [0b0101111100, a]) inp1

      simOut =
        drop 4 $
          C.sampleN
            (length inp2 + 1)
            (bitSlipSim @C.System (C.fromList (0 : inp2)))

      expected = take (length simOut) $ drop 1 inp2

  map f simOut H.=== expected
  H.assert $ isJust $ find g simOut
 where
  f (_, cg, _) = cg

  g (BSOk _ 0, _, _) = True
  g _ = False

-- | Bits of a code group in transmission order, bit 0 first
codeGroupBits :: C.BitVector 10 -> [C.Bit]
codeGroupBits cg = [cg C.! i | i <- [0 .. 9 :: Int]]

-- | Chop a bit stream into 10-bit words, the first bit at bit 0
toWords :: [C.Bit] -> [C.BitVector 10]
toWords bits = case splitAt 10 bits of
  (w, rest)
    | length w == 10 ->
        foldr (\(i, b) acc -> if b == 1 then C.setBit acc i else acc) 0 (zip [0 ..] w)
          : toWords rest
    | otherwise -> []

-- | A stream of code groups that arrives shifted by any number of bits is
--   realigned: after training, the output equals the original stream
prop_bitSlipShifted :: H.Property
prop_bitSlipShifted = H.property $ do
  shift <- H.forAll (Gen.integral (Range.linear 0 9))
  inp1 <-
    H.forAll
      ( Gen.list
          (Range.singleton 60)
          ( Gen.filter
              (\a -> isValidCodeGroup a && checkBitSequence a)
              genDefinedBitVector
          )
      )
  let
    stream = concatMap (\a -> [0b0101111100, a]) inp1
    shifted = toWords (replicate shift 0 ++ concatMap codeGroupBits stream)
    simOut =
      map f $
        C.sampleN (length shifted + 1) (bitSlipSim @C.System (C.fromList (0 : shifted)))
    -- Training takes eight commas, one every other word; the output lags the
    -- input by one word. Compare a stretch well inside the simulation.
    settled = drop 20 simOut
    expected = take (length stream - 40) (drop 30 stream)
  H.annotateShow settled
  H.assert $ expected `isInfixOf` settled
 where
  f (_, cg, _) = cg

tests :: TestTree
tests = $(testGroupGenerator)
