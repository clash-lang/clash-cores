{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Receive and transmit paths between the LVDS pins and 10-bit code groups
-}
module Kcu105.Sgmii.Serdes where

import Clash.Explicit.Prelude
import Kcu105.Sgmii.Domains
import Kcu105.Sgmii.Gearbox
import Kcu105.Sgmii.Primitives

-- | Reverse the bit order of a nibble
reverseNibble :: BitVector 4 -> BitVector 4
reverseNibble = v2bv . reverse . bv2v

-- | Resynchronise a quasi-static value from the code group clock to the SERDES
--   clock. A change caught halfway is corrected one cycle later.
quasiStatic ::
  (NFDataX a) =>
  Clock Pcs125 ->
  Clock Serdes312 ->
  Reset Serdes312 ->
  a ->
  Signal Pcs125 a ->
  Signal Serdes312 a
quasiStatic clkPcs clkDiv rstDiv dflt =
  register clkDiv rstDiv enableGen dflt . unsafeSynchronizer clkPcs clkDiv

-- | Receive path: differential input buffer, variable delay, deserializer,
-- gearbox and clock crossing. Returns the code groups, the delay tap the
-- primitive reports and the sticky FIFO errors.
rxPath ::
  Clock Serdes625 ->
  Clock Serdes312 ->
  Reset Serdes312 ->
  Clock Pcs125 ->
  Reset Pcs125 ->
  -- | Requested delay tap (quasi-static)
  Signal Pcs125 (Unsigned 9) ->
  -- | Reverse the bit order of each nibble (quasi-static)
  Signal Pcs125 Bool ->
  -- | P channel
  Signal Line1250 Bit ->
  -- | N channel
  Signal Line1250 Bit ->
  (Signal Pcs125 (BitVector 10), Signal Pcs125 (Unsigned 9), Signal Pcs125 FifoErrors)
rxPath clkSer clkDiv rstDiv clkPcs rstPcs tapReq rev rxP rxN = (cg, tapOut, errors)
 where
  line = ibufds rxP rxN
  tapReqDiv = quasiStatic clkPcs clkDiv rstDiv 0 tapReq
  tapPrev = register clkDiv rstDiv enableGen 0 tapReqDiv
  load = tapReqDiv ./=. tapPrev
  (delayed, tapCur) = idelaye3 clkDiv rstDiv tapReqDiv load line
  nibble = iserdese3 clkSer clkDiv rstDiv delayed
  revDiv = quasiStatic clkPcs clkDiv rstDiv False rev
  nibble' = mux revDiv (reverseNibble <$> nibble) nibble
  pairs = rxGearbox clkDiv rstDiv nibble'
  (cg, errors) = rxCrossing clkDiv rstDiv clkPcs rstPcs pairs
  tapOut = register clkPcs rstPcs enableGen 0 (unsafeSynchronizer clkDiv clkPcs tapCur)

-- | Transmit path: clock crossing, gearbox, serializer and differential output
--   buffer. Returns the P and N channel and the sticky FIFO errors.
txPath ::
  Clock Serdes625 ->
  Clock Serdes312 ->
  Reset Serdes312 ->
  Clock Pcs125 ->
  Reset Pcs125 ->
  -- | Reverse the bit order of each nibble (quasi-static)
  Signal Pcs125 Bool ->
  -- | Code groups
  Signal Pcs125 (BitVector 10) ->
  (Signal Line1250 Bit, Signal Line1250 Bit, Signal Pcs125 FifoErrors, Signal Pcs125 (Unsigned 16))
txPath clkSer clkDiv rstDiv clkPcs rstPcs rev cg = (txP, txN, errors, starved)
 where
  (nibble, errors, starved) = txCrossing clkPcs rstPcs clkDiv rstDiv cg
  revDiv = quasiStatic clkPcs clkDiv rstDiv False rev
  nibble' = mux revDiv (reverseNibble <$> nibble) nibble
  line = oserdese3 clkSer clkDiv rstDiv nibble'
  (txP, txN) = obufds line
