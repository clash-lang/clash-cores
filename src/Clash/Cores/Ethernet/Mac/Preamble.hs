{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides components which insert and strip the Ethernet preamble.
-}
module Clash.Cores.Ethernet.Mac.Preamble (
  preambleInserterC,
  preambleStripperC,
) where

import Clash.Prelude

import Data.Maybe (isJust, isNothing)

import Protocols (Circuit, fromSignals, (|>))
import Protocols.PacketStream

-- | Ethernet start frame delimiter (SFD), least significant bit first.
startFrameDelimiter :: BitVector 8
startFrameDelimiter = 0xD5

-- | The size of the Ethernet preamble + SFD.
type Preamble = Vec 8 (BitVector 8)

-- | The actual preamble, each byte ordered least significant bit first.
preamble :: Preamble
preamble = replicate d7 0x55 :< 0xD5

{- |
Prepends the Ethernet preamble and SFD to each packet in the packet stream.
The bytes are ordered least significant bit first:

>>> import Clash.Prelude
>>> preamble = 0x55 :> 0x55 :> 0x55 :> 0x55 :> 0x55 :> 0x55 :> 0x55 :> 0xD5 :> Nil

Inherits latency and throughput from `packetizerC`.
-}
preambleInserterC ::
  forall dataWidth dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
preambleInserterC = packetizerC id (const preamble)

-- | State of 'preambleStripperC'.
data PreambleStripperState
  = ValidateSfd
  | Forward
  deriving (Generic, NFDataX, Show, ShowX)

{- |
Strips each packet in the incoming packet stream of the preamble and SFD.
After a valid SFD has been detected, all incoming transfers are forwarded
until '_last' is asserted.

This component provides zero latency and full throughput.

__NB__: assumes that the SFD is byte-aligned.
-}
preambleStripperC ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  Circuit (PacketStream dom 1 ()) (PacketStream dom 1 ())
preambleStripperC = forceResetSanity |> fromSignals (mealyB go ValidateSfd)
 where
  go ValidateSfd (Just PacketStreamM2S{..}, _) =
    (nextSt, (PacketStreamS2M True, Nothing))
   where
    nextSt
      | isNothing _last && head _data == startFrameDelimiter = Forward
      | otherwise = ValidateSfd
  go Forward (Just transferIn, bwdIn) = (nextSt, (bwdIn, Just transferIn))
   where
    nextSt
      | isJust (_last transferIn) && _ready bwdIn = ValidateSfd
      | otherwise = Forward
  go st (Nothing, _) = (st, (PacketStreamS2M True, Nothing))
