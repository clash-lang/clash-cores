{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>,
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

5b/6b encoding and decoding tests. The input spaces are tiny, so every test is
exhaustive.
-}
module Test.Cores.LineCoding.Lc5b6b where

import Clash.Cores.LineCoding.Lc5b6b
import qualified Clash.Prelude as C
import Control.Monad (forM_, unless, when)
import Data.Bits (popCount)
import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Prelude

-- | Every input that has an encoding, with its encoding
encodings :: [((Bool, Bool, C.BitVector 5), (Bool, C.BitVector 6))]
encodings =
  [ ((cw, rd, x), enc)
  | cw <- [False, True]
  , rd <- [False, True]
  , x <- [minBound .. maxBound]
  , Just enc <- [encode5b6b cw rd x]
  ]

-- | All data words and exactly the five control words of IEEE 802.3 exist
case_definedInputs :: Assertion
case_definedInputs = forM_ [False, True] $ \rd ->
  forM_ [minBound .. maxBound] $ \x -> do
    assertBool ("D." ++ show x) (isJust (encode5b6b False rd x))
    assertEqual ("K." ++ show x) (x `elem` [23, 27, 28, 29, 30]) $
      isJust (encode5b6b True rd x)

-- | Decoding an encoding gives back the value, the running disparity and, for
-- @K.28@ only, the control flag
case_roundTrip :: Assertion
case_roundTrip = forM_ encodings $ \((cw, rd, x), (rdNew, code)) ->
  assertEqual (show (cw, rd, x)) (Just (cw && x == 28, rdNew, x)) $
    decode5b6b rd code

-- | Code groups have at most two more ones than zeros or the other way around,
-- a code group never has the same sign of disparity as the running disparity
-- before it, and the new running disparity follows from the code group
case_disparity :: Assertion
case_disparity = forM_ encodings $ \((cw, rd, x), (rdNew, code)) -> do
  let ones = popCount code
      name = show (cw, rd, x)
  assertBool (name ++ ": disparity out of range") (ones >= 2 && ones <= 4)
  unless rd $ assertBool (name ++ ": negative after negative") (ones >= 3)
  when rd $ assertBool (name ++ ": positive after positive") (ones <= 3)
  assertEqual (name ++ ": new running disparity") rdNew $
    if ones == 3 then rd else ones > 3

-- | The decoder rejects exactly the code groups that the encoder never produces
-- at that running disparity
case_decoderRejectsInvalid :: Assertion
case_decoderRejectsInvalid = forM_ [False, True] $ \rd ->
  forM_ [minBound .. maxBound] $ \code ->
    assertEqual (show (rd, code)) (code `elem` valid rd) $
      isJust (decode5b6b rd code)
 where
  valid rd = [code | ((_, rd', _), (_, code)) <- encodings, rd' == rd]

tests :: TestTree
tests = $(testGroupGenerator)
