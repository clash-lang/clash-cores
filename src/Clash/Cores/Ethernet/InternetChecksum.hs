{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Functions for computing the RFC1071 internet checksum.
-}
module Clash.Cores.Ethernet.InternetChecksum (
  onesComplementAdd,
  calculateChecksumC,
  internetChecksum,
  reduceToInternetChecksum,
  pipelinedInternetChecksum,
  InternetChecksumLatency,
) where

import Clash.Prelude

import Clash.Signal.Extra ( registerN )
import Clash.Sized.Vector.Extra ( PipelineLatency, foldPipeline, takeLe )

import qualified Data.Coerce as Coerce
import Data.Maybe
import Data.Type.Equality ((:~:)(Refl))

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream

{- |
Computes the one's complement sum of two 16-bit numbers. An important property
of this function is that it never produces @0x0000@ (positive zero) as a result.

=== __doctests setup__
>>> import Clash.Prelude

=== Examples

>>> onesComplementAdd 0x0001 0x0004 == 0x0005
True
>>> onesComplementAdd 0x1111 0xEEEE == 0xFFFF
True
>>> onesComplementAdd 0x1112 0xEEEE == 0x0001
True
-}
onesComplementAdd :: BitVector 16 -> BitVector 16 -> BitVector 16
onesComplementAdd a b = carry + truncated
  where
    c :: BitVector 17 = add a b
    (zeroExtend -> carry, truncated) = split c

-- | computes the un-complimented internet checksum of a stream of 16-bit words according to https://datatracker.ietf.org/doc/html/rfc1071
-- The checksum and reset are delayed by one clock cycle.
-- Keep in mind that if "reset" is True in the input tuple, the checksum is
-- reset to 0 the next cycle so the value of the bitvector is disgarded
internetChecksum
  :: forall (dom :: Domain).
  HiddenClockResetEnable dom
  => Signal dom Bool
  -- ^ Reset signal, resets the checksum to 0 the next cycle
  -> Signal dom (Maybe (BitVector 16))
  -- ^ Input data which gets added to the checksum
  -> Signal dom (BitVector 16)
 -- ^ Resulting checksum
internetChecksum reset inputM = checkSumWithCarry
  where
    inp = fromMaybe 0 <$> inputM

    checkSum :: Signal dom (BitVector 17)
    checkSum = register 0 $ mux reset 0 nextCheckSum

    (fmap zeroExtend -> carry, truncated) = unbundle $ split <$> checkSum

    checkSumWithCarry = carry + truncated
    nextCheckSum = add <$> inp <*> checkSumWithCarry

-- | Computes the internetChecksum of a vector of 16 bit words. Compared to
-- internetChecksum this is quicker as you can load multiple words per cycle
reduceToInternetChecksum ::
  forall (dom :: Domain) (width :: Nat).
  HiddenClockResetEnable dom
  => 1 <= width
  => KnownNat width
  => Signal dom Bool
  -- ^ Reset signal, resets the checksum to 0 the next cycle
  -> Signal dom (Maybe (Vec width (BitVector 16)))
  -- ^ Input data which gets added to the checksum
  -> Signal dom (BitVector 16)
  -- ^ Resulting checksum
reduceToInternetChecksum reset inputM = checkSum
  where
    checkSum = register 0 $ mux reset 0 checksumResult
    input = fromMaybe (repeat 0) <$> inputM
    checksumResult = fold onesComplementAdd <$> toSum
    toSum = (++) <$> (singleton <$> checkSum) <*> input

-- | Computes the internetChecksum of a vector of 16 bit words. Same as reduceToInternetChecksum
-- but with registers between each layer of the fold. Thus the critical path is shorter, but the
-- latency is higher. The latency is equal to PipelinedICLatency width.
pipelinedInternetChecksum ::
  forall (width :: Nat) (dom :: Domain).
  HiddenClockResetEnable dom
  => 1 <= width
  => KnownNat width
  => Signal dom Bool
  -- ^ Reset signal, resets the checksum to 0 the next cycle
  -> Signal dom (Maybe (Vec width (BitVector 16)))
  -- ^ Input data which gets added to the checksum
  -> Signal dom (BitVector 16)
  -- ^ Resulting checksum, the latency between input and output is PipelinedICLatency width
pipelinedInternetChecksum resetInp inputM = checkSum
  where
    checkSum = register 0 $ mux reset 0 checksumResult
    input = fromMaybe (repeat 0) <$> inputM
    checksumResult = onesComplementAdd <$> foldPipeline 0 onesComplementAdd input <*> checkSum
    reset = registerN (SNat :: SNat (PipelineLatency width)) False resetInp

-- | The latency of pipelinedInternetChecksum
type InternetChecksumLatency (n :: Nat) = CLog 2 n + 1

-- | State of 'calculateChecksumT1'
data ComputeChecksumState1 dataWidth
  =
  -- | Compute the checksum.
    Compute1
      { _counter1 :: Index (20 `DivRU` dataWidth) }
  -- | Consume the remainder of the packet.
  | Consume1
      { _latency1 :: Index 2--Index (1 + CLog 2 (dataWidth `DivRU` 2))
      -- ^ Number of clock cycles before the checksum is ready to read
      , _sent1 :: Bool
      -- ^ Whether we have transmitted the checksum
      }
  deriving (Generic, NFDataX, Show, ShowX)

{- |
Transition function of 'calculateChecksumC' in case @dataWidth < 20@
and @dataWidth@ is even.
-}
calculateChecksumT1 ::
  forall dataWidth meta width.
  (KnownNat dataWidth) =>
  1 <= dataWidth
  => dataWidth + 1 <= 20
  => 2 * width ~ dataWidth
  => ComputeChecksumState1 dataWidth
  -> (Maybe (PacketStreamM2S dataWidth meta), Ack, BitVector 16)
  -> (ComputeChecksumState1 dataWidth,
      ( Df.Data (BitVector 16), PacketStreamS2M, Bool, Maybe (Vec width (BitVector 16))
      )
     )
calculateChecksumT1 st@Compute1{..} (fwdIn, _, _) = (nextSt, (Df.NoData, PacketStreamS2M True, False, checksumIn))
 where
  nextSt = case (fwdIn, _counter1 == 0) of
    (Just _, True) -> Consume1 maxBound False
    (Just _, False) -> Compute1 (_counter1 - 1)
    (Nothing, _) -> st

  checksumIn = bitCoerce . _data <$> fwdIn

calculateChecksumT1 Consume1{..} (fwdIn, bwdIn, csum) = (nextSt, (dataOut, PacketStreamS2M outReady, rstChecksum, Nothing))
 where
  sendEn = _latency1 == 0 && not _sent1
  dataOut = if sendEn then Df.Data (complement csum) else Df.NoData

  rstChecksum = sendEn && Coerce.coerce bwdIn

  outReady = case fwdIn of
    Nothing -> True
    Just pkt -> isNothing (_last pkt) || _sent1 || rstChecksum

  nextSt = case (_latency1 == 0, _sent1, fwdIn) of
    (True, True, Just pkt) | isJust (_last pkt) ->
      Compute1 maxBound
    (True, s, _) ->
      Consume1 0 (s || rstChecksum)
    (False, _, _) ->
      Consume1 (_latency1 - 1) False

data ComputeChecksumState2 dataWidth
  =
  -- | Compute the checksum.
    Compute2
      { _counter2 :: Index (20 `DivRU` dataWidth)
      , _buffer2 :: Vec dataWidth (BitVector 8)
      , _checksumEn2 :: Bool
      }
  -- | Consume the remainder of the packet.
  | Consume2
      { _latency2 :: Index 7--Index (1 + CLog 2 (dataWidth `DivRU` 2))
      -- ^ Number of clock cycles before the checksum is ready to read
      , _sent2 :: Bool
      -- ^ Whether we have transmitted the checksum
      }
  deriving (Generic, NFDataX, Show, ShowX)

{- |
Transition function of 'calculateChecksumC' in case @dataWidth < 20@
and @dataWidth@ is odd.
-}
-- TODO: is inefficient and does NOT work if 10 < dataWidth < 20
calculateChecksumT2 ::
  forall dataWidth meta.
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  dataWidth + 1 <= 20 =>
  ComputeChecksumState2 dataWidth
  -> (Maybe (PacketStreamM2S dataWidth meta), Ack, BitVector 16)
  -> (ComputeChecksumState2 dataWidth,
      ( Df.Data (BitVector 16), PacketStreamS2M, Bool, Maybe (Vec dataWidth (BitVector 16))
      )
     )
calculateChecksumT2 st@Compute2{..} (fwdIn, _, _) = (nextSt, (Df.NoData, PacketStreamS2M True, False, checksumIn))
 where
  nextSt = case (fwdIn, _counter2 == 0) of
    (Just _, True) -> Consume2 maxBound False
    (Just _, False) -> Compute2 (_counter2 - 1) nextBuffer nextEn
    (Nothing, _) -> st

  nextBuffer = case fwdIn of
    Just transferIn | not _checksumEn2 -> _data transferIn
    _ -> _buffer2

  nextEn = case fwdIn of
    Nothing -> _checksumEn2
    Just _ -> case sameNat d0 (SNat @(Mod 20 (dataWidth * 2))) of
      Just Refl -> not _checksumEn2
      _ -> not _checksumEn2 || _counter2 == 1

  checksumIn =
    if _checksumEn2
      then go . _data <$> fwdIn
      else Nothing

  go dat = case sameNat d0 (SNat @(20 `Mod` (2 * dataWidth))) of
    Just Refl -> bitCoerce (_buffer2 ++ dat)
    Nothing -> if _counter2 == 0
      then case compareSNat (SNat @(Mod 20 (2 * dataWidth))) (SNat @(2 * dataWidth)) of
        SNatLE -> case (compareSNat (SNat @(Mod 20 dataWidth)) (SNat @(dataWidth + dataWidth)),compareSNat (SNat @(Mod 20 dataWidth)) (SNat @dataWidth))  of
          (SNatLE, SNatLE) -> bitCoerce (takeLe (SNat @(20 `Mod` dataWidth)) dat ++ repeat @(dataWidth + dataWidth - 20 `Mod` dataWidth ) 0x00)
          _ -> clashCompileError "calculateChecksumT2: absurd1"
        SNatGT -> clashCompileError "calculateChecksumT2: absurd2"
      else bitCoerce (_buffer2 ++ dat)

calculateChecksumT2 Consume2{..} (fwdIn, bwdIn, csum) = (nextSt, (dataOut, PacketStreamS2M outReady, rstChecksum, Nothing))
 where
  sendEn = _latency2 == 0 && not _sent2
  dataOut = if sendEn then Df.Data (complement csum) else Df.NoData

  rstChecksum = sendEn && Coerce.coerce bwdIn

  outReady = case fwdIn of
    Nothing -> True
    Just pkt -> isNothing (_last pkt) || _sent2 || rstChecksum

  nextSt = case (_latency2 == 0, _sent2, fwdIn) of
    (True, True, Just pkt) | isJust (_last pkt) ->
      Compute2 maxBound undefined False
    (True, s, _) ->
      Consume2 0 (s || rstChecksum)
    (False, _, _) ->
      Consume2 (_latency2 - 1) False

-- | State of 'calculatechecksumT3'.
data ComputeChecksumState3
  =
  -- | Compute the checksum.
    Compute3
  -- | Consume the remainder of the packet.
  | Consume3
      -- TODO this is incorect, but if the latency here is longer it doesn't matter for the test.
      -- Change this once we use the version of pipelinedInternetChecksum without residue
      { _latency3 :: Index 10--Index (1 + CLog 2 (dataWidth `DivRU` 2))
      -- ^ Number of clock cycles before the checksum is ready to read
      , _sent3 :: Bool
      -- ^ Whether we have transmitted the checksum
      }
  deriving (Generic, NFDataX, Show, ShowX)

-- | Transition function of 'calculateChecksumC' in case @20 <= dataWidth@.
calculateChecksumT3 ::
  forall dataWidth meta.
  (20 <= dataWidth) =>
  ComputeChecksumState3 ->
  (Maybe (PacketStreamM2S dataWidth meta), Ack, BitVector 16) ->
  ( ComputeChecksumState3
  , ( Df.Data (BitVector 16), PacketStreamS2M, Bool, Maybe (Vec 10 (BitVector 16))
    )
  )
calculateChecksumT3 Compute3 (fwdIn, _, _) = (nextSt, (Df.NoData, PacketStreamS2M True, False, checksumIn))
 where
  checksumIn = bitCoerce . takeLe d20 . _data <$> fwdIn

  nextSt = case fwdIn of
    Nothing -> Compute3
    _ -> Consume3 maxBound False
calculateChecksumT3 Consume3{..} (fwdIn, bwdIn, csum) = (nextSt, (dataOut, PacketStreamS2M outReady, rstChecksum, Nothing))
 where
  sendEn = _latency3 == 0 && not _sent3
  dataOut = if sendEn then Df.Data (complement csum) else Df.NoData

  rstChecksum = sendEn && Coerce.coerce bwdIn

  outReady = case fwdIn of
    Nothing -> True
    Just pkt -> isNothing (_last pkt) || _sent3 || rstChecksum

  nextSt = case (_latency3 == 0, _sent3, fwdIn) of
    (True, True, Just pkt) | isJust (_last pkt) ->
      Compute3
    (True, s, _) ->
      Consume3 0 (s || rstChecksum)
    (False, _, _) ->
      Consume3 (_latency3 - 1) False

{- |
Compute the Internet Checksum over the first @20@ bytes of a packet stream.
-}
calculateChecksumC ::
  forall dataWidth meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit (PacketStream dom dataWidth meta) (Df dom (BitVector 16))
calculateChecksumC = forceResetSanity |> fromSignals ckt
 where
  ckt (fwdInS, bwdInS) = case compareSNat d1 (SNat @(dataWidth `DivRU` 2)) of
    SNatGT -> clashCompileError "calculateChecksumC: absurd1"
    SNatLE -> case compareSNat d20 (SNat @dataWidth) of
      SNatLE -> (bwdOut, fwdOut)
       where
        csum = pipelinedInternetChecksum @10 rst csumInp
        (fwdOut, bwdOut, rst, csumInp) = mealyB calculateChecksumT3 Compute3 (fwdInS, bwdInS, csum)
      SNatGT -> case (sameNat (SNat @(dataWidth `Mod` 2)) d0, sameNat (SNat @(dataWidth `Mod` 2)) d1) of
        (Just Refl, _) -> case sameNat (SNat @(2 * Div (dataWidth + 1) 2)) (SNat @dataWidth) of
          Nothing -> clashCompileError "ecalculateChecksumC: absurd2"
          Just Refl -> (bwdOut, fwdOut)
        -- Even
            where
              csum = pipelinedInternetChecksum @(dataWidth `DivRU` 2) rst csumInp
              (fwdOut, bwdOut, rst, csumInp) = mealyB calculateChecksumT1 (Compute1 maxBound) (fwdInS, bwdInS, csum)
        (_, Just Refl) -> (bwdOut, fwdOut)
        -- Odd
         where
          csum = pipelinedInternetChecksum @dataWidth rst csumInp
          (fwdOut, bwdOut, rst, csumInp) = mealyB calculateChecksumT2 (Compute2 maxBound (deepErrorX "calculateChecksumC: Absurd3") False) (fwdInS, bwdInS, csum)
        _ -> clashCompileError "calculateChecksumC: absurd4"
