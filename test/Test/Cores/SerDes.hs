module Test.Cores.SerDes where

import Clash.Prelude
import Clash.Cores.SerDes
import Data.Maybe (catMaybes)
import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit

data ComplexObject
  = ByteAlignedObject (BitVector 7) (BitVector 24)
  | NonByteAlignedObject (BitVector 4) (BitVector 32)
 deriving (Generic, Eq, BitPack, NFDataX, Show)

tests :: TestTree
tests = testGroup
  "SerDes tests"
  [ testCase "Test serialize" serTest
  , testCase "Test deserialize" deserTest
  , testCase "Test e2e" e2eTest
  ]


e2eTest :: Assertion
e2eTest = do
  assertEqual "outputsEqual" expected actual
 where
  input = NonByteAlignedObject 3 3
  inputS = fromList $ [Nothing, Nothing, Just input] <> List.repeat Nothing

  expected = input
  actual =
    List.head
      $ catMaybes
      $ sampleN @System 500
      $ withClockResetEnable clockGen resetGen enableGen
      $ deserialize @_ @7 (serialize (pure True) inputS)


serTest :: Assertion
serTest = do
  assertEqual "serializeEqualToBitPack" expected actual
 where
  input = NonByteAlignedObject 5 5
  inputS = fromList $ [Nothing, Nothing, Just input] <> List.repeat Nothing

  expected = toList $ ((unpack . resize . pack) input :: Vec 5 (BitVector 8)) :: [BitVector 8]
  actual =
    catMaybes
      $ sampleN @System 500
      $ withClockResetEnable clockGen resetGen enableGen
      $ serialize (pure True) inputS


deserTest :: Assertion
deserTest = do
  assertEqual "deserEqualToBitPack" expected actual
 where
  input = NonByteAlignedObject 5 5
  inputAsList = toList $ fmap Just $ ((unpack . resize . pack) input :: Vec 5 (BitVector 8))
  inputS = fromList $ [Nothing] <> inputAsList <> List.repeat Nothing

  expected = input
  actual =
    List.head
      $ catMaybes
      $ sampleN @System 500
      $ withClockResetEnable clockGen resetGen enableGen
      $ deserialize inputS

