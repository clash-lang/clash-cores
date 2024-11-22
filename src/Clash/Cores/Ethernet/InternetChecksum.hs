{-# LANGUAGE ViewPatterns #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Functions for computing the RFC1071 internet checksum.
-}
module Clash.Cores.Ethernet.InternetChecksum (
  onesComplementAdd,
  internetChecksum,
) where

import Clash.Prelude

import Data.Maybe (isJust)

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

{- |
Computes the un-complemented internet checksum of a stream of 16-bit words
according to [IETF RFC 1071](https://datatracker.ietf.org/doc/html/rfc1071).

The checksum is delayed by one clock cycle.
Keep in mind that if the reset input is @True@, the checksum is
reset to @0@ the next cycle so the the input data is thrown away.
-}
internetChecksum ::
  forall (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  -- | Reset signal, resets the checksum to 0 the next cycle
  Signal dom Bool ->
  -- | Input data which gets added to the checksum
  Signal dom (Maybe (BitVector 16)) ->
  -- | Resulting un-complemented checksum
  Signal dom (BitVector 16)
internetChecksum reset inputM = checksum
 where
  enable = reset .||. isJust <$> inputM

  checksum :: Signal dom (BitVector 16)
  checksum = regEn 0 enable $ mux reset 0 nextChecksum

  nextChecksum = liftA2 onesComplementAdd (fromJustX <$> inputM) checksum
