{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ScFifoTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Fifo

import IntelTestCommon

import qualified Prelude as P

-- | Like 'System', but with a synchronous reset
createDomain vSystem{vName = "DomSync", vResetKind = Synchronous}

-- | Number of cycles per test bench
nCycles :: Int
nCycles = 1000

-- | Stimuli: reset, write request and read request
type Stimuli = (Bool, Maybe (BitVector 8), Bool)

{- | Random stimuli, in phases of 30 cycles that mostly write, mostly read, or
do both, so that the FIFO fills up and drains.
-}
genStimuli :: Int -> [Stimuli]
genStimuli = evalG $ do
  phases <- listOf (nCycles `div` 30 + 1) $ do
    (pw, pr) <- elements [(80, 20), (20, 80), (50, 50), (90, 90)]
    listOf 30 $ do
      r <- chance 1
      w <- chance pw
      d <- genBV
      rd <- chance pr
      pure (r, if w then Just d else Nothing, rd)
  pure ([(True, Nothing, False) | _ <- [1 .. 3 :: Int]] <> P.take nCycles (P.concat phases))

type Out = (BitVector 8, Bool, Bool, Bool, Bool, BitVector 8)

-- | The outputs of an 'scfifo', with 'usedWords' extended to 8 bits
outputs :: (KnownNat n, 1 <= n, KnownNat (CLog 2 n)) => ScFifoOut dom n (BitVector 8) -> Signal dom Out
outputs ScFifoOut{..} =
  bundle (fifoData, isFull, isEmpty, isAlmostFull, isAlmostEmpty, pack . (resize :: _ -> Unsigned 8) <$> usedWords)

-- | An 'scfifo' with the given configuration
dut ::
  forall n dom.
  (KnownDomain dom, KnownNat n, 2 <= n, KnownNat (CLog 2 n)) =>
  ScFifoConfig ->
  SNat n ->
  Clock dom ->
  Signal dom Stimuli ->
  Signal dom Out
dut cfg n clk stim =
  outputs (scfifo (SSymbol @"Cyclone V") cfg n clk (unsafeFromActiveHigh rst) wr rd)
 where
  (rst, wr, rd) = unbundle stim

{- | Like 'dut', but only requests a write when the FIFO isn't full and a
read when it isn't empty, for FIFOs without overflow and underflow checking.
-}
dutUnchecked ::
  forall n dom.
  (KnownDomain dom, KnownNat n, 2 <= n, KnownNat (CLog 2 n)) =>
  ScFifoConfig ->
  SNat n ->
  Clock dom ->
  Signal dom Stimuli ->
  Signal dom Out
dutUnchecked cfg n clk stim = outputs fifo
 where
  (rst, wr, rd) = unbundle stim
  fifo = scfifo (SSymbol @"Cyclone IV E") cfg n clk (unsafeFromActiveHigh rst) wr1 rd1
  wr1 = mux (isFull fifo) (pure Nothing) wr
  rd1 = rd .&&. fmap not (isEmpty fifo)

normalArea, normalSpeed, showAheadArea, showAheadSpeed :: ScFifoConfig
normalArea = defScFifoConfig{almostFullValue = 6, almostEmptyValue = 2}
normalSpeed = defScFifoConfig{outputRegister = True, almostFullValue = 1, almostEmptyValue = 1}
showAheadArea = defScFifoConfig{readMode = ShowAhead, almostFullValue = 3, almostEmptyValue = 5}
showAheadSpeed =
  defScFifoConfig
    { readMode = ShowAhead
    , outputRegister = True
    , almostFullValue = 8
    , almostEmptyValue = 8
    }

unchecked :: ScFifoConfig -> ScFifoConfig
unchecked cfg = cfg{overflowChecking = False, underflowChecking = False}
