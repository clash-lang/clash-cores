{- |
  Copyright   :  (C) 2024-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  8b/10b encoding and decoding tests. Besides the property tests, the coder is
  compared exhaustively against the former table-based implementation in
  "Test.Cores.LineCoding.Lc8b10b.Reference".
-}
module Test.Cores.LineCoding.Lc8b10b where

import Clash.Cores.LineCoding.Internal (nextDisparity)
import Clash.Cores.LineCoding.Lc8b10b
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Control.Monad (forM_, unless, when)
import Data.Maybe (isNothing)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Test.Cores.LineCoding.Lc8b10b.Reference as Ref
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Check if a 'BitVector' does not contain a sequence of bits with the same
--   value for 5 or more indices consecutively
checkBitSequence :: C.BitVector 10 -> Bool
checkBitSequence cg =
  isNothing $
    C.elemIndex True $
      C.map (f . C.pack) $
        C.windows1d C.d5 $
          C.bv2v cg
 where
  f a = a == 0b11111 || a == 0b00000

-- | Function that checks whether a code group corresponds to a valid symbol
isValidCodeGroup :: C.BitVector 10 -> Bool
isValidCodeGroup cg =
  isValidSymbol (snd $ decode8b10b True cg)
    || isValidSymbol (snd $ decode8b10b False cg)

-- | Function that creates a list with a given range that contains a list of
--   running disparities and 'Symbol8b10b's
genSymbol8b10bs :: H.Range Int -> H.Gen [(Bool, Symbol8b10b)]
genSymbol8b10bs range = do
  n <- Gen.int range
  genSymbol8b10bs1 n False

-- | Recursive function to generate a list of 'Symbol8b10b's with the correct
--   running disparity
genSymbol8b10bs1 :: Int -> Bool -> H.Gen [(Bool, Symbol8b10b)]
genSymbol8b10bs1 0 _ = pure []
genSymbol8b10bs1 n rd = do
  (rdNew, dw) <- genSymbol8b10b rd
  ((rdNew, dw) :) <$> genSymbol8b10bs1 (pred n) rdNew

-- | Generate a 'Symbol8b10b' by creating a 'BitVector' of length 10 and
--   decoding it with the 'decode8b10b' function
genSymbol8b10b :: Bool -> H.Gen (Bool, Symbol8b10b)
genSymbol8b10b rd = Gen.filter f $ decode8b10b rd <$> genDefinedBitVector
 where
  f (_, dw) = isDw dw

-- | Whether a 'Symbol8b10b' is a 'RdError'
isRdError :: Symbol8b10b -> Bool
isRdError (RdError _) = True
isRdError _ = False

-- | Every defined symbol (all data words, and the control words the reference
--   encoder defines) with the running disparity before it, the running
--   disparity after it and its code group
encodings :: [((Bool, Symbol8b10b), (Bool, C.BitVector 10))]
encodings =
  [ ((rd, sym), encode8b10b rd sym)
  | rd <- [False, True]
  , w <- [minBound .. maxBound]
  , cw <- [False, True]
  , let sym = if cw then Cw w else Dw w
  , let (undefinedCw, _, _) = Ref.referenceEncode cw rd w
  , not undefinedCw
  ]

-- | The (running disparity, code group) pairs the encoder produces
validPairs :: [(Bool, C.BitVector 10)]
validPairs = [(rd, cg) | ((rd, _), (_, cg)) <- encodings]

-- Check if the output of 'decode8b10b' is a valid value for a given value from
-- 'encode8b10b'
prop_decode8b10bCheckValid :: H.Property
prop_decode8b10bCheckValid = H.withTests 1000 $ H.property $ do
  inp <- H.forAll genDefinedBitVector

  H.assert $
    isValidSymbol $
      snd $
        decode8b10b True $
          snd $
            encode8b10b True (Dw inp)
  H.assert $
    isValidSymbol $
      snd $
        decode8b10b False $
          snd $
            encode8b10b False (Dw inp)

-- | Encode and then decode a valid input and check whether it is the same
prop_encodeDecode8b10b :: H.Property
prop_encodeDecode8b10b = H.withTests 1000 $ H.property $ do
  inp <- H.forAll genDefinedBitVector
  roundTrip False inp H.=== Dw inp
  roundTrip True inp H.=== Dw inp
 where
  roundTrip rd inp = snd $ decode8b10b rd $ snd $ encode8b10b rd $ Dw inp

-- | Decode a valid code group and encode the result: this gives the code group
--   and the same new running disparity back
prop_decodeEncode8b10b :: H.Property
prop_decodeEncode8b10b = H.withTests 1000 $ H.property $ do
  inp <- H.forAll (Gen.filter isValidCodeGroup genDefinedBitVector)
  propertyForRd False inp
  propertyForRd True inp
 where
  propertyForRd rd inp = do
    H.annotateShow rd
    let (rdNew, sym) = decode8b10b rd inp
    when (isValidSymbol sym) $
      encode8b10b rd sym H.=== (rdNew, inp)

-- | The encoder gives the same new running disparity and code group as the
--   reference for every defined symbol, and leaves the running disparity
--   unchanged with code group 0 for undefined control words
case_encoderMatchesReference :: Assertion
case_encoderMatchesReference = forM_ [False, True] $ \rd ->
  forM_ [minBound .. maxBound] $ \w ->
    forM_ [False, True] $ \cw -> do
      let sym = if cw then Cw w else Dw w
          (undefinedCw, refRd, refCg) = Ref.referenceEncode cw rd w
          expected = if undefinedCw then (rd, 0) else (refRd, refCg)
      assertEqual (show (rd, sym)) expected (encode8b10b rd sym)

-- | The encoder produces a different code group for every symbol at a given
--   running disparity
case_encoderInjective :: Assertion
case_encoderInjective = forM_ [False, True] $ \rd -> do
  let cgs = [cg | ((rd', _), (_, cg)) <- encodings, rd' == rd]
  assertEqual (show rd) (length cgs) (length (uniq cgs))
 where
  uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | The decoder inverts the encoder, including the new running disparity, for
--   every code group the encoder produces, and agrees with the reference
--   decoder there
case_decoderInvertsEncoder :: Assertion
case_decoderInvertsEncoder = forM_ encodings $ \((rd, sym), (rdNew, cg)) -> do
  let name = show (rd, cg)
  assertEqual name (rdNew, sym) (decode8b10b rd cg)
  let (rdEr, cgEr, refCw, refRd, refW) = Ref.referenceDecode rd cg
  assertEqual (name ++ ": reference") (False, False, isCw sym, rdNew, fromSymbol sym) $
    (rdEr, cgEr, refCw, refRd, refW)

-- | Every pair of running disparity and code group that the encoder does not
--   produce decodes to an error symbol: 'RdError' when the code group is valid
--   at the other running disparity, 'DwError' otherwise. The new running
--   disparity follows from the disparity of the code group.
case_decoderRejectsInvalid :: Assertion
case_decoderRejectsInvalid = forM_ [False, True] $ \rd ->
  forM_ [minBound .. maxBound] $ \cg ->
    unless ((rd, cg) `elem` validPairs) $ do
      let (rdNew, sym) = decode8b10b rd cg
          name = show (rd, cg)
      assertBool (name ++ ": accepted") (not (isValidSymbol sym))
      assertEqual (name ++ ": error class") ((not rd, cg) `elem` validPairs) $
        isRdError sym
      assertEqual (name ++ ": new running disparity") (nextDisparity rd cg) rdNew

-- | The reference decoder accepts everything the decoder accepts. The
--   reference also accepts 144 pairs that no encoder produces (46 code groups
--   at the wrong running disparity and 98 that are not code groups at all);
--   those are the errors in the former decoding table.
case_decoderNotMoreLenientThanReference :: Assertion
case_decoderNotMoreLenientThanReference = do
  forM_ [False, True] $ \rd -> forM_ [minBound .. maxBound] $ \cg -> do
    let (rdEr, cgEr, _, _, _) = Ref.referenceDecode rd cg
    when (isValidSymbol (snd (decode8b10b rd cg))) $
      assertBool (show (rd, cg) ++ ": reference rejects") (not rdEr && not cgEr)
  length referenceOnly @?= 144
 where
  referenceOnly =
    [ (rd, cg)
    | rd <- [False, True]
    , cg <- [minBound .. maxBound]
    , let (rdEr, cgEr, _, _, _) = Ref.referenceDecode rd cg
    , not rdEr && not cgEr
    , not (isValidSymbol (snd (decode8b10b rd cg)))
    ]

tests :: TestTree
tests = $(testGroupGenerator)
