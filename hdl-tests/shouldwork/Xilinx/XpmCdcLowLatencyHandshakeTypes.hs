module XpmCdcLowLatencyHandshakeTypes where

import Clash.Cores.Xilinx.Xpm.Cdc.LowLatencyHandshake
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Data.Proxy
import Language.Haskell.TH.Lib

-- | Offers 1, 2, 3, ... to the destination domain. After every transfer, it
-- waits zero, one, or two cycles before offering the next value.
srcFsm ::
  forall src .
  KnownDomain src =>
  Clock src ->
  -- | @src_ready@
  Signal src Bool ->
  -- | (@src_in@, @src_valid@)
  Signal src (Unsigned 8, Bool)
srcFsm clk = mealy clk noReset enableGen go (1, 0)
 where
  go :: (Unsigned 8, Index 3) -> Bool -> ((Unsigned 8, Index 3), (Unsigned 8, Bool))
  go (n, 0) ready
    | ready     = ((n + 1, fromIntegral (n `mod` 3)), (n, True))
    | otherwise = ((n,     0),                        (n, True))
  go (n, w) _   = ((n,     w - 1),                    (n, False))
{-# NOINLINE srcFsm #-}

-- | Accepts data in three out of every five cycles
dstFsm ::
  forall dst .
  KnownDomain dst =>
  Clock dst ->
  -- | (@dest_valid@, @dest_out@)
  Signal dst (Bool, Unsigned 8) ->
  -- | (@dest_ready@, accepted data)
  Signal dst (Bool, Maybe (Unsigned 8))
dstFsm clk = mealy clk noReset enableGen go 0
 where
  go :: Index 5 -> (Bool, Unsigned 8) -> (Index 5, (Bool, Maybe (Unsigned 8)))
  go cnt (valid, dat) = (satSucc SatWrap cnt, (ready, accepted))
   where
    ready = cnt < 3
    accepted = if valid && ready then Just dat else Nothing
{-# NOINLINE dstFsm #-}

-- | Composition of 'srcFsm' and 'dstFsm'
top ::
  forall srcStages dstStages src dst .
  ( KnownDomain src
  , KnownDomain dst
  , 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  ) =>
  XpmCdcLowLatencyHandshakeConfig srcStages dstStages ->
  Clock src ->
  Clock dst ->
  Signal dst (Maybe (Unsigned 8))
top opts clkSrc clkDst = maybeDat
 where
  (srcIn, srcValid) = unbundle $ srcFsm clkSrc srcReady

  (destOut, destValid, srcReady) =
    xpmCdcLowLatencyHandshakeWith opts clkSrc clkDst srcIn srcValid destReady

  (destReady, maybeDat) = unbundle $ dstFsm clkDst $ bundle (destValid, destOut)
{-# NOINLINE top #-}

tb ::
  forall a b srcStages dstStages n .
  ( KnownNat n, 1 <= n
  , 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  , KnownDomain a
  , KnownDomain b
  ) =>
  Proxy a -> Proxy b ->
  XpmCdcLowLatencyHandshakeConfig srcStages dstStages ->
  -- | Expected data
  Vec n (Maybe (Unsigned 8)) ->
  Signal b Bool
tb Proxy Proxy opts expectedDat = done
 where
  actual = top opts clkA clkB

  done = outputVerifier' clkB noReset expectedDat actual

  -- Testbench clocks
  clkA :: Clock a
  clkA = tbClockGen (not <$> unsafeSynchronizer clkB clkA done)
  clkB :: Clock b
  clkB = tbClockGen (not <$> done)

expected ::
  forall a b srcStages dstStages samples .
  ( KnownDomain a
  , KnownDomain b
  , 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  ) =>
  Proxy a ->
  Proxy b ->
  XpmCdcLowLatencyHandshakeConfig srcStages dstStages ->
  SNat samples ->
  ExpQ
expected Proxy Proxy opts SNat = listToVecTH out1
 where
  out0 = top opts (clockGen @a) (clockGen @b)
  out1 = sampleN (natToNum @samples) out0
