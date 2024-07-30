{-# LANGUAGE RankNTypes #-}

-- | Functions to generate signals for SPI devices. The function used to
-- generate a signal determines if the signal is lawful or not (with regards
-- to respecting busy and acknowledgement signals).
--
module Test.Cores.Internal.Signals
  ( GenMaster
  , GenSlave
  , masterLawfulSignal
  , masterLawlessSignal
  , slaveLawfulSignal
  , slaveLawlessSignal
  ) where

import qualified Data.List as List (cycle, head, tail)

import qualified Clash.Explicit.Prelude as E (moore, mooreB)
import           Clash.Prelude
-- import           Clash.Cores.SPI


type GenMaster n =
     forall dom
   . KnownDomain dom
  => Clock dom
  -> Reset dom
  -> [BitVector n]
  -> Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Maybe (BitVector n))

type GenSlave n =
     forall dom
   . KnownDomain dom
  => Clock dom
  -> Reset dom
  -> [BitVector n]
  -> Signal dom Bool
  -> Signal dom (BitVector n)

-- | A device which gives data to the SPI master, while respecting
-- busy and acknowledgement signals from the device.
--
masterLawfulSignal :: GenMaster n
masterLawfulSignal clk rst vals ack busy =
  E.mooreB clk rst enableGen go out
    (List.head vals, List.tail (List.cycle vals), True)
    (ack, busy)
 where
  go (x, xs, _) (isAck, isBusy)
    | isAck     = (List.head xs, List.tail xs, isBusy)
    | otherwise = (x, xs, isBusy)

  out (x, _, b) = if b then Nothing else Just x

-- | A device which gives data to the SPI master. This function does not
-- wait for the acknowledgement signal before changing the input, which
-- should result in incorrect data transfer.
--
masterLawlessSignal :: GenMaster n
masterLawlessSignal clk rst vals _ =
  E.mooreB clk rst enableGen go out
    (List.head vals, List.tail (List.cycle vals), True)
 where
  go (_, xs, _) isBusy =
    (List.head xs, List.tail xs, isBusy)

  out (x, _, b) =
    if b then Nothing else Just x

-- | A device which gives data to an SPI slave, while respecting
-- acknowledgement signals from the device.
--
slaveLawfulSignal :: GenSlave n
slaveLawfulSignal clk rst vals =
  E.moore clk rst enableGen go fst
    (List.head vals, List.tail (List.cycle vals))
 where
  go (x, xs) isAck =
    if isAck then (List.head xs, List.tail xs) else (x, xs)

-- | A device which gives data to an SPI slave. This function does not
-- wait for the acknowledgement signal before changing the input, which
-- should result in incorrect data transfer.
--
slaveLawlessSignal :: GenSlave n
slaveLawlessSignal clk rst vals =
  E.moore clk rst enableGen go fst
    (List.head vals, List.tail (List.cycle vals))
 where
  go (_, xs) _ = (List.head xs, List.tail xs)

