{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>,
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

3b/4b encoding and decoding tests. The input spaces are tiny, so every test is
exhaustive.
-}
module Test.Cores.LineCoding.Lc3b4b where

import Clash.Cores.LineCoding.Lc3b4b
import qualified Clash.Prelude as C
import Control.Monad (forM_, unless, when)
import Data.Bits (popCount)
import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Prelude

-- | Every input with its encoding. The alternate form flag only matters for
-- data words with value 7, so it is only varied there.
encodings :: [((Bool, Bool, Bool, C.BitVector 3), (Bool, C.BitVector 4))]
encodings =
  [ ((cw, alt, rd, y), encode3b4b cw alt rd y)
  | cw <- [False, True]
  , alt <- [False, True]
  , rd <- [False, True]
  , y <- [minBound .. maxBound]
  , not alt || (not cw && y == 7)
  ]

-- | The alternate form flag changes data words with value 7 and nothing else
case_alternateFormOnlyForData7 :: Assertion
case_alternateFormOnlyForData7 = forM_ [False, True] $ \cw ->
  forM_ [False, True] $ \rd ->
    forM_ [minBound .. maxBound] $ \y -> do
      let primary = encode3b4b cw False rd y
          alternate = encode3b4b cw True rd y
      if not cw && y == 7
        then assertBool (show (cw, rd, y)) (primary /= alternate)
        else assertEqual (show (cw, rd, y)) primary alternate

-- | Decoding an encoding gives back the value and the running disparity
case_roundTrip :: Assertion
case_roundTrip = forM_ encodings $ \((cw, alt, rd, y), (rdNew, code)) ->
  assertEqual (show (cw, alt, rd, y)) (Just (rdNew, y)) (decode3b4b cw rd code)

-- | Code groups have at most two more ones than zeros or the other way around,
-- a code group never has the same sign of disparity as the running disparity
-- before it, and the new running disparity follows from the code group
case_disparity :: Assertion
case_disparity = forM_ encodings $ \((cw, alt, rd, y), (rdNew, code)) -> do
  let ones = popCount code
      name = show (cw, alt, rd, y)
  assertBool (name ++ ": disparity out of range") (ones >= 1 && ones <= 3)
  unless rd $ assertBool (name ++ ": negative after negative") (ones >= 2)
  when rd $ assertBool (name ++ ": positive after positive") (ones <= 2)
  assertEqual (name ++ ": new running disparity") rdNew $
    if ones == 2 then rd else ones > 2

-- | The decoder rejects exactly the code groups that the encoder never produces
-- for that control word flag and running disparity
case_decoderRejectsInvalid :: Assertion
case_decoderRejectsInvalid = forM_ [False, True] $ \cw ->
  forM_ [False, True] $ \rd ->
    forM_ [minBound .. maxBound] $ \code ->
      assertEqual (show (cw, rd, code)) (code `elem` valid cw rd) $
        isJust (decode3b4b cw rd code)
 where
  valid cw rd =
    [code | ((cw', _, rd', _), (_, code)) <- encodings, cw' == cw, rd' == rd]

tests :: TestTree
tests = $(testGroupGenerator)
