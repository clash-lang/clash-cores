{-# LANGUAGE ViewPatterns #-}

{-|
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

import Data.Maybe

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
