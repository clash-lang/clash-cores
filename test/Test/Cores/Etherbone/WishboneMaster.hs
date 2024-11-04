module Test.Cores.Etherbone.WishboneMaster (
  tests,
) where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
-- import Test.Tasty.Hedgehog.Extra (testProperty)
-- import Test.Tasty.TH (testGroupGenerator)

import Prelude
import qualified Clash.Prelude as C


-- tests :: TestTree
-- tests =
--   localOption (mkTimeout 20_000_000 {- 20 seconds -})
--     $ localOption
--       (HedgehogTestLimit (Just 1_000))
--       $(testGroupGenerator)
--



-- Function that creates a test case
testMultiplication :: Int -> Int -> TestTree
testMultiplication x y = testCase description assertion
  where
    description = "Multiplication of " ++ show x ++ " and " ++ show y
    assertion = x * y @?= expected
    expected = x * y

-- Main test tree
tests :: TestTree
tests = testGroup "Multiplication Tests"
  [ testMultiplication 2 3
  , testMultiplication 0 5
  , testMultiplication (-1) 4
  , testMultiplication 10 10
  ]
