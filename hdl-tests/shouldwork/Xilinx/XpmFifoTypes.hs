{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test infrastructure for "XpmFifoSync". It drives the FIFO with random write and read requests, the density of which
-- varies over time so the FIFO regularly fills up and drains, and with resets
-- at fixed cycles. All outputs of the FIFO are compared, cycle by cycle, with
-- outputs of the Haskell simulation model.
module XpmFifoTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (BitVector(BV), fromInteger#)
import Language.Haskell.TH (ExpQ, litT, numTyLit)
import qualified Prelude as P
import Language.Haskell.TH.Syntax (lift)

import Clash.Cores.Xilinx.Xpm.Fifo

import XpmTestCommon

-- | Data stored in the FIFO
type Dat = BitVector 9

-- | Free running counter, used to schedule test phases and resets
counter :: KnownDomain dom => Clock dom -> Signal dom (Unsigned 16)
counter clk = cnt
 where
  cnt = delay clk enableGen 0 (cnt + 1)

-- | Probability (out of 256) of a request in each of four phases of 64
-- cycles. Phase one fills the FIFO, phase three drains it.
phaseThreshold :: Vec 4 (Unsigned 8) -> Unsigned 16 -> Unsigned 8
phaseThreshold ts cnt = ts !! (truncateB (cnt `shiftR` 6) :: Unsigned 2)

-- | Random write requests, starting at the given cycle
writeStimulus ::
  KnownDomain dom =>
  Unsigned 16 ->
  Clock dom ->
  Signal dom (Maybe Dat)
writeStimulus start clk = go <$> counter clk <*> genTestData randomSeed clk
 where
  go :: Unsigned 16 -> BitVector 17 -> Maybe Dat
  go cnt rnd
    | cnt >= start
    , unpack (truncateB rnd) < phaseThreshold (230 :> 128 :> 40 :> 180 :> Nil) cnt
    = Just (truncateB (rnd `shiftR` 8))
    | otherwise = Nothing

-- | Random read requests
readStimulus ::
  KnownDomain dom =>
  Clock dom ->
  Signal dom Bool
readStimulus clk = go <$> counter clk <*> genTestData (randomSeed + 1) clk
 where
  go :: Unsigned 16 -> BitVector 8 -> Bool
  go cnt rnd = unpack rnd < phaseThreshold (40 :> 128 :> 230 :> 180 :> Nil) cnt

-- | Reset, asserted for three cycles at the first given cycle, and for a single
-- cycle at the second given cycle and four cycles after that. Driven by a
-- register.
resetStimulus ::
  KnownDomain dom =>
  Unsigned 16 ->
  Unsigned 16 ->
  Clock dom ->
  Reset dom
resetStimulus r0 r1 clk =
  unsafeFromActiveHigh (delay clk enableGen False (go <$> counter clk))
 where
  go cnt = (cnt >= r0 && cnt < r0 + 3) || cnt == r1 || cnt == r1 + 4

-- | Write side outputs
type WriteOut depth = (Bool, Bool, Bool, Unsigned (depth + 1), Bool, Bool, Bool)

-- | Read side outputs
type ReadOut depth = (Dat, Bool, Bool, Bool, Unsigned (depth + 1), Bool, Bool, Bool)

-- | Constraints needed to work with 'WriteOut' and 'ReadOut'
type KnownOuts depth =
  ( KnownNat depth, 4 <= depth, depth <= 22
  , KnownNat (BitSize (WriteOut depth))
  , KnownNat (BitSize (ReadOut depth))
  , KnownNat (BitSize (WriteOut depth, ReadOut depth)) )

-- | Write side outputs, packed
packWrite ::
  KnownNat depth =>
  XpmFifoOut write read depth a ->
  Signal write (BitVector (BitSize (WriteOut depth)))
packWrite XpmFifoOut{..} =
  pack <$> bundle
    ( isFull, isAlmostFull, isProgFull, writeCount, isOverflow, writeAck
    , writeResetBusy )

-- | Read side outputs, packed
packRead ::
  KnownNat depth =>
  XpmFifoOut write read depth Dat ->
  Signal read (BitVector (BitSize (ReadOut depth)))
packRead XpmFifoOut{..} =
  pack <$> bundle
    ( fifoData, isEmpty, isAlmostEmpty, isProgEmpty, readCount, isUnderflow
    , isDataValid, readResetBusy )

-- | Like 'listToVecTH', but renders the vector as a single 'BitVector'
-- literal. This prevents GHC from running into its reduction depth limit for
-- vectors with more than a thousand elements.
bitVecToVecTH :: forall m . KnownNat m => [BitVector m] -> ExpQ
bitVecToVecTH xs =
  [| bitCoerce (fromInteger# $(lift mask) $(lift val) :: BitVector $(litT (numTyLit (n * m))))
       :: Vec $(litT (numTyLit n)) (BitVector $(litT (numTyLit m))) |]
 where
  n = toInteger (P.length xs)
  m = natToNum @m :: Integer
  mask = P.foldl (\acc (BV msk _) -> acc * 2 P.^ m + msk) 0 xs
  val = toInteger (P.foldl (\acc (BV _ v) -> acc * 2 P.^ m + v) 0 xs)

-- * Synchronous FIFO

syncFifo ::
  forall depth dom .
  (KnownOuts depth, KnownDomain dom) =>
  XpmFifoConfig depth ->
  Clock dom ->
  Signal dom (BitVector (BitSize (WriteOut depth, ReadOut depth)))
syncFifo cfg clk = pack <$> bundle (unpack <$> packWrite out :: Signal dom (WriteOut depth), unpack <$> packRead out :: Signal dom (ReadOut depth))
 where
  out =
    xpmFifoSyncWith cfg clk (resetStimulus 300 600 clk)
      (writeStimulus 0 clk) (readStimulus clk)

syncTb ::
  forall depth n dom .
  ( KnownOuts depth, KnownDomain dom
  , KnownNat n, 1 <= n ) =>
  XpmFifoConfig depth ->
  Vec n (BitVector (BitSize (WriteOut depth, ReadOut depth))) ->
  Signal dom Bool
syncTb cfg expectedDat = done
 where
  done =
    outputVerifierWith
      (\c r -> assertBitVector c r $(lift $ "outputVerifier (seed:" <> show randomSeed <> ")"))
      clk clk noReset expectedDat (syncFifo cfg clk)
  clk :: Clock dom
  clk = tbClockGen (not <$> done)

syncExpected ::
  forall depth dom .
  (KnownOuts depth, KnownDomain dom) =>
  XpmFifoConfig depth ->
  Clock dom ->
  Int ->
  ExpQ
syncExpected cfg clk samples = bitVecToVecTH (sampleN samples (syncFifo cfg clk))

-- * Configurations

-- | Standard read mode, read latency 1, all optional outputs
syncCfg0 :: XpmFifoConfig 4
syncCfg0 = (defXpmFifoConfig @4)
  { fifoMemoryType = Block
  , advFeatures = allAdvFeatures
  }

-- | FWFT, full flags reset to 'True'
syncCfg1 :: XpmFifoConfig 4
syncCfg1 = (defXpmFifoConfig @4)
  { fifoMemoryType = Distributed
  , readMode = Fwft
  , fullResetValue = True
  , progFullThresh = 7
  , progEmptyThresh = 6
  , advFeatures = allAdvFeatures
  }

-- | Read latency 3, default optional outputs
syncCfg2 :: XpmFifoConfig 5
syncCfg2 = (defXpmFifoConfig @5)
  { readMode = Std 3
  , progFullThresh = 20
  , progEmptyThresh = 5
  }

-- | Read latency 2, UltraRAM
syncCfg3 :: XpmFifoConfig 4
syncCfg3 = (defXpmFifoConfig @4)
  { fifoMemoryType = Ultra
  , readMode = Std 2
  , fullResetValue = True
  , progFullThresh = 12
  , progEmptyThresh = 4
  , advFeatures = allAdvFeatures
  }

-- | FWFT, only data valid
syncCfg4 :: XpmFifoConfig 5
syncCfg4 = (defXpmFifoConfig @5)
  { fifoMemoryType = Block
  , readMode = Fwft
  , advFeatures = noAdvFeatures{enDataValid = True}
  }

-- | Read latency 4
syncCfg5 :: XpmFifoConfig 4
syncCfg5 = (defXpmFifoConfig @4)
  { fifoMemoryType = Distributed
  , readMode = Std 4
  , progFullThresh = 3
  , progEmptyThresh = 13
  , advFeatures = allAdvFeatures
  }

-- | Larger FIFO, FWFT
syncCfg6 :: XpmFifoConfig 6
syncCfg6 = (defXpmFifoConfig @6)
  { readMode = Fwft
  , progFullThresh = 40
  , progEmptyThresh = 20
  , advFeatures = allAdvFeatures
  }
