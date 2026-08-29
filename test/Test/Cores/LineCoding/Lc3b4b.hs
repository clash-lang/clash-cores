{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

3b/4b encoding and decoding tests
-}
module Test.Cores.LineCoding.Lc3b4b where

import Clash.Cores.LineCoding.Lc3b4b
import Clash.Hedgehog.Sized.BitVector
import Data.Maybe (fromJust)
import qualified Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

prop_encodeDecode3b4b :: H.Property
prop_encodeDecode3b4b = H.withTests 1000 $ H.property $ do
  inp <- H.forAll genDefinedBitVector
  roundTrip False inp H.=== inp
  roundTrip True inp H.=== inp
 where
  roundTrip rd inp =
    snd $
      fromJust $
        decode3b4b False rd $
          snd $
            fromJust $
              encode3b4b False False rd inp

tests :: TestTree
tests = $(testGroupGenerator)
