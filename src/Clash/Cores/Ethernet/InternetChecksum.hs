{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

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
import Data.Type.Bool (If)
import Data.Type.Equality ((:~:)(Refl), type (==))

import GHC.TypeLits.KnownNat (KnownBool)

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
    reset = registerN (SNat @(PipelineLatency width)) False resetInp

-- | The latency of pipelinedInternetChecksum
type InternetChecksumLatency (n :: Nat) = CLog 2 n + 1

-- | State of 'calculateChecksumT'
data ComputeChecksumState dataWidth
  =
  -- | Compute the checksum.
    Compute
      { _counter :: Index (20 `DivRU` dataWidth)
      -- ^ Counts the number of transfers we still need to feed to the checksum engine
      , _buffer :: Vec (If (CmpNat dataWidth 20 == 'LT) (dataWidth `Mod` 2) 0) (Maybe (BitVector 8))
      -- ^ Contains 1 byte if @dataWidth < 20@ and @dataWidth@ is odd
      -- Otherwise, is emtpy.
      }
  -- | Consume the remainder of the packet.
  | Consume
      { _latency :: Index (1 + CLog 2 (If (CmpNat dataWidth 20 == 'LT) (dataWidth `DivRU` 2) 10))
      -- ^ Number of clock cycles before the checksum is ready to read.
      -- If @dataWidth >= 20@, then this is capped at @1 + CLog 2 10 = 5@ clock cycles.
      , _sent :: Bool
      -- ^ Whether we have transmitted the checksum
      }
  deriving (Generic, Show, ShowX)

instance
  (KnownNat dataWidth, KnownBool (CmpNat dataWidth 20 == LT)) =>
  NFDataX (ComputeChecksumState dataWidth)

-- | Transition function of 'calculateChecksumC'.
calculateChecksumT ::
  forall dataWidth meta width.
  (KnownBool (CmpNat dataWidth 20 == 'LT)) =>
  (KnownNat dataWidth) =>
  (KnownNat width) =>
  (1 <= dataWidth) =>
  (1 <= width) =>
  (20 `Mod` dataWidth <= dataWidth) =>
  (width ~ If (CmpNat dataWidth 20 == 'LT) (dataWidth `DivRU` 2) 10) =>
  ComputeChecksumState dataWidth ->
  (Maybe (PacketStreamM2S dataWidth meta), Ack, BitVector 16) ->
  ( ComputeChecksumState dataWidth
  , ( Df.Data (BitVector 16)
    , PacketStreamS2M
    , Bool
    , Maybe (Vec width (BitVector 16))
    )
  )
calculateChecksumT st@Compute{..} (fwdIn, _, _) = (nextSt, (Df.NoData, PacketStreamS2M True, False, checksumIn))
 where
  nextSt = case (fwdIn, _counter == 0) of
    (Just _, True) -> Consume maxBound False
    (Just _, False) -> Compute (_counter - 1) nextBuffer
    (Nothing, _) -> st

  nextBuffer = case (cmpNat (SNat @dataWidth) d20, sameNat d1 (SNat @(dataWidth `Mod` 2))) of
    -- If @dataWidth < 20@ with @dataWidth@ odd, we might need to buffer the last byte.
    (LTI, Just Refl) -> case (fwdIn, _buffer) of
      (Just transferIn, Nothing :> Nil) ->
        leToPlus @1 @dataWidth $ singleton (Just $ last (_data transferIn))
      (Just _, Just _ :> Nil) ->
        singleton Nothing
      _ ->
        _buffer
    -- Should always be Nil, but the type checker does not know that.
    (_, _) -> repeat Nothing


  mod20 = SNat @(20 `Mod` dataWidth)

  checksumIn = go . _data <$> fwdIn

  go :: Vec dataWidth (BitVector 8) -> Vec width (BitVector 16)
  go dat = case (cmpNat (SNat @dataWidth) d20, sameNat d1 (SNat @(dataWidth `Mod` 2))) of
    -- @dataWidth >= 20@
    (EQI, _) -> bitCoerce $ takeLe d20 dat
    (GTI, _) -> case compareSNat d20 (SNat @dataWidth) of
      -- Not sure why the constraint solver does not emit a @20 <= dataWidth@
      -- in this branch.
      SNatLE -> bitCoerce $ takeLe d20 dat
      SNatGT -> clashCompileError "calculateChecksumT: absurd 1"
    -- @dataWidth < 20@, @dataWidth@ even
    (LTI, Nothing) -> case sameNat (SNat @(2 * width)) (SNat @dataWidth) of
      Nothing -> clashCompileError "calculateChecksumT: absurd 2"
      Just Refl -> bitCoerce $ case (sameNat d0 mod20, _counter == 0) of
        (Nothing, True) ->
          takeLe mod20 dat ++ repeat @(dataWidth - 20 `Mod` dataWidth) 0x00
        _ ->
          dat
    -- @dataWidth < 20@, @dataWidth@ odd
    (LTI, Just Refl) -> case sameNat (SNat @(2 * width - 1)) (SNat @dataWidth) of
      Nothing -> clashCompileError "calculateChecksumT: absurd 3"
      Just Refl -> bitCoerce $ case (sameNat d0 mod20, _counter == 0, _buffer) of
        (Nothing, True, Nothing :> Nil) ->
          takeLe mod20 dat ++ repeat @(dataWidth - 20 `Mod` dataWidth + 1) 0x00
        (_, _, Nothing :> Nil) ->
          takeLe (SNat @(dataWidth - 1)) dat ++ (0x00 :> 0x00 :> Nil)
        (Nothing, True, Just buf :> Nil) ->
          buf :> takeLe mod20 dat ++ repeat @(dataWidth - 20 `Mod` dataWidth) 0x00
        (_, _, Just buf :> Nil) ->
          buf :> dat
        _ -> deepErrorX "calculateChecksumT: absurd non-singleton Vec"

calculateChecksumT Consume{..} (fwdIn, bwdIn, csum) = (nextSt, (dataOut, PacketStreamS2M outReady, rstChecksum, Nothing))
 where
  sendEn = _latency == 0 && not _sent
  dataOut = if sendEn then Df.Data (complement csum) else Df.NoData

  rstChecksum = sendEn && Coerce.coerce bwdIn

  outReady = case fwdIn of
    Nothing -> True
    Just pkt -> isNothing (_last pkt) || _sent || rstChecksum

  nextSt = case (_latency == 0, _sent, fwdIn) of
    (True, True, Just pkt) | isJust (_last pkt) ->
      Compute maxBound (repeat Nothing)
    (True, s, _) ->
      Consume 0 (s || rstChecksum)
    (False, _, _) ->
      Consume (_latency - 1) False

{- |
Compute the Internet Checksum over the first @20@ bytes of a packet stream.
-}
calculateChecksumC ::
  forall dataWidth meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownBool (CmpNat dataWidth 20 == LT)) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit (PacketStream dom dataWidth meta) (Df dom (BitVector 16))
calculateChecksumC = forceResetSanity |> fromSignals ckt
 where
  ckt (fwdInS, bwdInS) = case
    ( compareSNat (SNat @(20 `Mod` dataWidth)) (SNat @dataWidth)
    , compareSNat d1 (SNat @(If (CmpNat dataWidth 20 == LT) (Div (dataWidth + 1) 2) 10))
    ) of
    (SNatLE, SNatLE) -> (bwdOut, fwdOut)
     where
      csum = pipelinedInternetChecksum rst csumInp
      (fwdOut, bwdOut, rst, csumInp) = mealyB calculateChecksumT (Compute maxBound (repeat Nothing)) (fwdInS, bwdInS, csum)
    _ -> clashCompileError "absurd"
 