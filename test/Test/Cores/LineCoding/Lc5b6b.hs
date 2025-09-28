{- |
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  5b/6b encoding and decoding tests
-}
module Test.Cores.LineCoding.Lc5b6b where

import Clash.Cores.LineCoding.Lc5b6b
import Clash.Hedgehog.Sized.BitVector
import Data.Maybe (fromJust)
import qualified Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

prop_encodeDecode5b6b :: H.Property
prop_encodeDecode5b6b = H.withTests 1000 $ H.property $ do
  inp <- H.forAll genDefinedBitVector
  roundTrip False inp H.=== inp
  roundTrip True inp H.=== inp
 where
  roundTrip rd inp =
    snd $ fromJust $ decode5b6b rd $ snd $ fromJust $ encode5b6b rd inp

tests :: TestTree
tests = $(testGroupGenerator)
