{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides a highly configurable ARP table.
-}
module Clash.Cores.Ethernet.Arp.ArpTable (
  arpTableC,
) where

import Clash.Cores.Ethernet.Arp.ArpTypes (ArpEntry (..), ArpLookup, ArpResponse (..))
import Clash.Cores.Ethernet.IP.IPv4Types (IPv4Address)

import Clash.Prelude

import Clash.Signal.Extra (timer)

import Data.Maybe (isJust)

import Protocols (Ack (..), Circuit (..), Df)
import qualified Protocols.Df as Df

-- | State of 'arpTableT'.
data ArpTableState depth
  = -- | The ARP table is currently serving insertion and lookup requests
    Active
      { _bramValid :: Bool
      -- ^ Whether the output of the blockram contains valid data
      }
  | -- | The ARP table is decrementing the timers of all entries,
    -- and therefore cannot currently accept any insertion or lookup requests.
    Invalidating
      { _writeAddr :: Unsigned depth
      -- ^ The timer of the entry at this address will be decremented
      }
  deriving (Generic, Show, ShowX, NFDataX)

-- | State transition function of 'arpTableC'.
arpTableT ::
  forall
    (depth :: Nat)
    (maxAgeSeconds :: Nat).
  (KnownNat depth) =>
  (KnownNat maxAgeSeconds) =>
  (1 <= depth) =>
  ArpTableState depth ->
  ( Bool
  , (ArpEntry, Index (maxAgeSeconds + 1))
  , Maybe (ArpEntry, Unsigned depth)
  , Maybe (IPv4Address, Unsigned depth)
  , Bool
  ) ->
  ( ArpTableState depth
  , ( Ack
    , Unsigned depth
    , Maybe (Unsigned depth, (ArpEntry, Index (maxAgeSeconds + 1)))
    , Maybe ArpResponse
    )
  )
-- If the reset is on, go back to the initial state
-- and don't acknowledge or send out data.
arpTableT _ (True, _, _, _, _) =
  (Active False, (Ack False, 0, Nothing, Nothing))
arpTableT Active{..} (_, (arpEntry, secsLeft), insertion, lookupReq, secondPassed) =
  (nextSt, (Ack True, readAddr, writeCmd, arpRespOut))
 where
  writeCmd = (\(entry, hash) -> (hash, (entry, maxBound))) <$> insertion

  arpRespOut = case (_bramValid, lookupReq) of
    (True, Just (ip, _)) -> Just (arpResp ip)
    _ -> Nothing

  -- It is possible that the IP stored in the entry is not the same as the
  -- lookup IP. This happens due to hash collisions.
  arpResp lookupIP =
    if secsLeft == 0 || lookupIP /= _arpIP arpEntry
      then ArpEntryNotFound
      else ArpEntryFound (_arpMac arpEntry)

  (nextSt, readAddr)
    | secondPassed = (Invalidating maxBound, maxBound)
    | otherwise = (Active (isJust lookupReq && not _bramValid), maybe 0 snd lookupReq)
arpTableT Invalidating{..} (_, (arpEntry, secsLeft), _, _, _) =
  (nextSt, (Ack False, writeAddr', writeCmd, Nothing))
 where
  writeCmd = Just (_writeAddr, (arpEntry, satPred SatBound secsLeft))
  writeAddr' = _writeAddr - 1
  nextSt
    | _writeAddr == 0 = Active False
    | otherwise = Invalidating writeAddr'

{- |
ARP table that stores @2^depth@ entries in block RAM. @maxAgeSeconds@ is the
number of seconds before the entry will be removed from the table (lazily).

Every second, the ARP table is unable to handle insertion and lookup requests
for @2^depth@ clock cycles, because it needs to decrease the timers of the
entries. During this period, the component will assert backpressure. Note
that this implies that the component will not work correctly when the size of
the ARP table is bigger than the clock frequency.

An entry may be evicted sooner than expected from the cache due to hash collisions;
entries are addressed by taking the last @depth@ bits of their corresponding IPv4
address. By increasing the number of entries in the table, the chance of IPv4
addresses colliding is lower. For example, the addresses @10.0.0.1@ and
@192.168.0.1@ will collide if the depth is small.

__NB__: the timeout is inaccurate for up to one second, because the circuit uses a
constant counter for efficiency reasons.
-}
arpTableC ::
  forall
    (dom :: Domain)
    (depth :: Nat)
    (maxAgeSeconds :: Nat).
  (HiddenClockResetEnable dom) =>
  (1 <= depth) =>
  -- | The table will contain @2^depth@ entries
  SNat depth ->
  -- | Entries are evicted from the table this many seconds after being inserted
  SNat maxAgeSeconds ->
  -- | (Lookup request, Insertion request)
  Circuit (ArpLookup dom, Df dom ArpEntry) ()
arpTableC SNat SNat = Circuit (hideReset ckt)
 where
  ckt reset ((lookupReq, insertReq), ()) = ((arpResp, outReady), ())
   where
    -- The underlying blockram.
    tableEntry =
      blockRam1
        NoClearOnReset
        (SNat @(2 ^ depth))
        (deepErrorX "arpTableC: initial blockram", 0)
        readAddr
        writeCmd

    -- Hashes of the IPv4 addresses, used to address the blockram.
    -- We simply take the last @depth@ bits of the IPv4 address.
    lookupWithHash :: Signal dom (Maybe (IPv4Address, Unsigned depth))
    lookupWithHash =
      fmap (\ipAddr -> (ipAddr, resize $ bitCoerce ipAddr))
        <$> lookupReq

    insertionWithHash :: Signal dom (Maybe (ArpEntry, Unsigned depth))
    insertionWithHash =
      fmap (\entry -> (entry, resize $ bitCoerce (_arpIP entry)))
        <$> (Df.dataToMaybe <$> insertReq)

    readAddr :: Signal dom (Unsigned depth)
    writeCmd :: Signal dom (Maybe (Unsigned depth, (ArpEntry, Index (maxAgeSeconds + 1))))
    (outReady, readAddr, writeCmd, arpResp) =
      unbundle (mealy arpTableT (Active False) input)

    input =
      bundle
        ( unsafeToActiveHigh reset
        , tableEntry
        , insertionWithHash
        , lookupWithHash
        , timer (SNat @(10 ^ 12))
        )
