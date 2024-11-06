{-# LANGUAGE NamedFieldPuns #-}

module Test.Cores.Etherbone.WishboneMaster (
  tests,
) where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Cores.Etherbone.WishboneMaster
import qualified Clash.Prelude as C
import Data.Maybe
import Protocols
import qualified Protocols.Df as Df
import Protocols.Wishbone
import Prelude

type WBData = C.BitVector 32

genWishboneMasterInput :: Gen (WishboneMasterInput 32 4 WBData)
genWishboneMasterInput = do
  _inAddr :: C.BitVector 32 <- Gen.integral Range.linearBounded
  _inDat :: Maybe WBData <- Gen.maybe $ Gen.integral Range.linearBounded
  _inLast <- Gen.bool
  _dropCyc <- Gen.bool
  let
    _inSel = 0xf :: C.BitVector 4
    input =
      WishboneMasterInput
        { _inAddr
        , _inDat
        , _inSel
        , _inLast
        , _dropCyc
        }
  pure input

prop_wishboneMasterT :: Property
prop_wishboneMasterT = property $ do
  input <- forAll genWishboneMasterInput
  wbTime :: Int <- forAll $ Gen.integral (Range.linear 1 10)
  ackTime :: Int <- forAll $ Gen.integral (Range.linear 0 10)
  let
    beginWait = 5
    readData = 0x55555555

    wbTime' = wbTime + 1
    ackTime' = ackTime + 1

    -- Proper way to test this would be to create a model of the wishbone
    -- system. Instead I generate input 'signals' that are properly timed.
    input' = replicate beginWait Df.NoData <> replicate wbTime' (Df.Data input) <> replicate ackTime' Df.NoData
    ackInput = replicate beginWait True <> replicate wbTime' False <> replicate (ackTime' - 1) False <> [True]
    wbAckInput = replicate beginWait False <> replicate (wbTime' - 1) False <> [True] <> replicate ackTime' False

    finalInput = map mapInput $ zip3 input' ackInput wbAckInput

    (outStates, out) = foldl fn ([WaitForOp False], []) finalInput
    fn (states, results) inp = (states ++ [state], results ++ [result])
     where
      (state, result) = wishboneMasterT (last states) inp

    mapInput (x, ack, wbAck) =
      (False, (x, (Ack ack, (emptyWishboneS2M @WBData){readData = readData, acknowledge = wbAck}, ())))

    -- (Ack False,
    --   (NoData, WishboneM2S [  CYC  STB !WE, ADR = 0b0000_0000_0000_0000_0000_0000_0000_0000, DAT = undefined, SEL = 0b1111, CTE = classic, BTE = linear ],Just 0
    --   )
    -- )
    getWb (_, (_, x, _)) = x
    getOutDat (_, (Df.Data x, _, _)) = x
    getOutDat (_, (_, _, _)) = error "No data at the expected cycle"
  footnote (show input')
  footnote (show outStates)
  footnote (show out)

  -- assert (last (fst res) == WaitForOp (not $ _dropCyc input))
  -- Check if it passes the correct states
  assert $ outStates !! beginWait == WaitForOp False
  assert $ outStates !! (beginWait + 1) == Busy
  assert $ last outStates == WaitForOp (not $ _dropCyc input)

  -- Check if dropCyc works as expected
  assert $ busCycle (getWb $ last out) == not (_dropCyc input)

  -- Check if data was read from the wishbone bus if a read op was submitted,
  -- otherwise check if the returned data was 'Nothing'
  assert $
    _outDat (getOutDat $ out !! (beginWait + wbTime'))
      == if isJust (_inDat input) then Nothing else Just readData

  -- Check if last was set correctly
  assert $ _outLast (getOutDat $ out !! (beginWait + wbTime')) == _inLast input

tests :: TestTree
tests = $(testGroupGenerator)
