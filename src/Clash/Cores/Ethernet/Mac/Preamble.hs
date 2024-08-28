{-# OPTIONS_HADDOCK hide #-}

{-|
Module      : Clash.Cores.Ethernet.Mac.Preamble
Description : Provides components which insert and strip the Ethernet preamble.
-}
module Clash.Cores.Ethernet.Mac.Preamble (
  preambleInserterC,
  preambleStripperC,
) where

import Clash.Prelude

import Protocols ((|>), Circuit)
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

{- |
Strips the incoming packet stream of the Ethernet preamble and SFD (Start Frame
Delimiter), 8 bytes in total. Drops a packet if the SFD is not correct.
Does not check if the preamble itself matches for efficiency reasons.

Inherits latency and throughput from `depacketizerC`.
-}
preambleStripperC ::
  forall dataWidth dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
preambleStripperC =
  -- Only put the SFD in the metadata for efficiency reasons.
  depacketizerC (\(p :: Preamble) _ -> last p)
    |> filterMeta (== startFrameDelimiter)
    |> mapMeta (const ())
