{-# LANGUAGE RecordWildCards #-}

module DcFifoTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Fifo

import IntelTestCommon

import qualified Prelude as P

-- | Read domains, faster and slower than 'System'
createDomain vSystem{vName = "R7", vPeriod = 7000}
createDomain vSystem{vName = "R25", vPeriod = 25000}

-- | Stimuli of the write domain: reset and write request
type WStim = (Bool, Maybe (BitVector 8))

-- | Stimuli of the read domain: read request
type RStim = Bool

{- | Random stimuli for a test of the given duration (in nanoseconds), with a
write clock of 10 ns and a read clock of the given period (in nanoseconds).
The write side writes and the read side reads in phases of different
intensities, so that the FIFO fills up and drains. The write side resets the
FIFO now and then, for five cycles. Both end with idle cycles.
-}
genStimuli :: Int -> Int -> Int -> ([WStim], [RStim])
genStimuli duration rPeriod = evalG $ do
  ws <- P.concat <$> listOf nPhases (do
    pw <- elements [10, 50, 90, 100]
    rst <- chance 10
    if rst
      then pure (P.replicate 5 (True, Nothing))
      else listOf 30 ((\w d -> (False, if w then Just d else Nothing)) <$> chance pw <*> genBV))
  rs <- P.concat <$> listOf nPhases (do
    pr <- elements [10, 50, 90, 100]
    listOf (300 `div` rPeriod) (chance pr))
  pure
    ( P.replicate 3 (True, Nothing) <> P.take nW (ws <> P.repeat (False, Nothing)) <> P.replicate 30 (False, Nothing)
    , P.take nR (rs <> P.repeat False) <> P.replicate (300 `div` rPeriod) False )
 where
  nW = duration `div` 10
  nR = duration `div` rPeriod
  nPhases = duration `div` 50 + 1

type WOut = (Bool, Bool, BitVector 8)
type ROut = (BitVector 8, Bool, Bool, BitVector 8)

-- | A 'dcfifo' with the given configuration, with 'Unsigned 8' counts
dut ::
  forall depth read.
  (KnownDomain read, KnownNat depth, 2 <= depth, depth <= 8) =>
  DcFifoConfig ->
  SNat depth ->
  Clock System ->
  Clock read ->
  Signal System WStim ->
  Signal read RStim ->
  (Signal System WOut, Signal read ROut)
dut cfg depth wClk rClk wStim rd =
  ( bundle (isWriteFull, isWriteEmpty, ext <$> writeUsedWords)
  , bundle (readData, isReadEmpty, isReadFull, ext <$> readUsedWords) )
 where
  (rst, wr) = unbundle wStim
  DcFifoOut{..} =
    dcfifo (SSymbol @"Cyclone V") cfg depth wClk (unsafeFromActiveHigh rst) rClk wr rd
  ext :: Unsigned depth -> BitVector 8
  ext = pack . (resize :: Unsigned depth -> Unsigned 8)

{- | Like 'dut', but only requests a write when the FIFO isn't full and a
read when it isn't empty, for FIFOs without overflow and underflow checking.
-}
dutUnchecked ::
  forall depth read.
  (KnownDomain read, KnownNat depth, 2 <= depth, depth <= 8) =>
  DcFifoConfig ->
  SNat depth ->
  Clock System ->
  Clock read ->
  Signal System WStim ->
  Signal read RStim ->
  (Signal System WOut, Signal read ROut)
dutUnchecked cfg depth wClk rClk wStim rd =
  ( bundle (isWriteFull, isWriteEmpty, ext <$> writeUsedWords)
  , bundle (readData, isReadEmpty, isReadFull, ext <$> readUsedWords) )
 where
  (rst, wr) = unbundle wStim
  DcFifoOut{..} =
    dcfifo (SSymbol @"Cyclone IV E") cfg depth wClk (unsafeFromActiveHigh rst) rClk wr1 rd1
  wr1 = mux isWriteFull (pure Nothing) wr
  rd1 = rd .&&. fmap not isReadEmpty
  ext :: Unsigned depth -> BitVector 8
  ext = pack . (resize :: Unsigned depth -> Unsigned 8)
