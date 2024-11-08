module Test.Cores.Etherbone (
  tests,
) where

import Test.Tasty
import qualified Test.Cores.Etherbone.WishboneMaster
import qualified Test.Cores.Etherbone.RecordProcessor

tests :: TestTree
tests =
  testGroup
    "Etherbone"
    [ Test.Cores.Etherbone.WishboneMaster.tests
    , Test.Cores.Etherbone.RecordProcessor.tests
    ]
