{-|
Module      : Clash.Cores.Ethernet.Mac.Preamble
Description : Provides circuits to insert and strip the ethernet preamble.
-}
module Clash.Cores.Ethernet.Mac.Preamble
  ( preambleInserterC
  , preambleStripperC
  ) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream


-- | A vector of 8 bytes, which is the size of the Ethernet preamble + start frame delimiter.
type Preamble = Vec 8 (BitVector 8)

-- | The actual preamble, each byte ordered least significant bit first.
preamble :: Preamble
preamble = replicate d7 0x55 :< 0xD5

-- | Ethernet start frame delimiter (SFD), least significant bit first.
startFrameDelimiter :: BitVector 8
startFrameDelimiter = 0xD5

-- | Prepends the ethernet preamble to the packet stream, for each individual packet.
preambleInserterC
  :: forall dataWidth dom.
  HiddenClockResetEnable dom =>
  KnownNat dataWidth =>
  1 <= dataWidth =>
  Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
preambleInserterC = packetizerC id (const preamble)

-- | Strips the incoming PacketStream of the preamble and SFD. Drops a packet only
--   if the SFD is not correct, the circuit does not check if the preamble itself
--   matches for efficiency reasons.
preambleStripperC
  :: forall dataWidth dom.
  HiddenClockResetEnable dom =>
  KnownNat dataWidth =>
  1 <= dataWidth =>
  Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
preambleStripperC =
  -- Only put the SFD in the metadata for efficiency reasons.
  depacketizerC (\(p :: Preamble) _ -> last p) |>
  filterMeta (== startFrameDelimiter) |>
  mapMeta (const ())
