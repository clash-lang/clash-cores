{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- | Test benches and scans for "VirtualJtag". They are defined here, as the
Template Haskell in "VirtualJtag" uses them to create reference traces.
-}
module VirtualJtagDuts where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Data.Maybe (isJust)
import Data.Word (Word64)

import Clash.Cores.Intel.Debug.VirtualJtag
import Clash.Cores.Intel.Debug.VirtualJtag.Internal (ModelCycle(..), TapState(..), hostTmsTdi, modelCycles)
import Clash.Explicit.Testbench (tbClockGen)

import IntelIpSim (simLibraries)
import IpTestCommon (randoms)

-- | Domain of TCK. The test bench samples the outputs of @sld_virtual_jtag@ at
-- the falling edge of TCK, when they are stable: they change after the rising
-- edges.
createDomain vSystem{vName="Tck", vPeriod=100000, vActiveEdge=Falling}

-- | Domain of the test bench's own clock, see 'virtualJtagTB'
createDomain vSystem{vName="Slow", vPeriod=1000000}

{- | Expected outputs at the falling edge of every cycle of TCK in HDL
simulation, until the simulation model stops TCK: the virtual state, whether
the TAP controller is in Test-Logic-Reset, and, if they are to be checked, TDI
and the virtual instruction register.

The Clash model gives the values that a design clocked by TCK sees at the
rising edge at the end of a cycle. The simulation models register TDI and the
virtual instruction register at that rising edge, and (by delta cycle
ordering) a design clocked by TCK sees the new values at that same edge. So
at the falling edge, TDI and the virtual instruction register are the values
of the model of the previous cycle. In Test-Logic-Reset, the simulation models
reset them right away. Before the first edge, they are undefined, and the
state of the TAP controller depends on the simulator: Vivado sees the initial
change of TCK to high as a rising edge, Icarus doesn't.
-}
expectedTrace ::
  forall n.
  KnownNat n =>
  [VirtualJtagScan] ->
  [(Maybe VirtualJtagState, Maybe Bool, Maybe (Bit, BitVector n))]
expectedTrace scans = P.zipWith3 entry [0 :: Int ..] cs (undefined : cs)
 where
  w = natToNum @n
  cs = P.take (P.length (hostTmsTdi w scans)) (modelCycles w scans)
  entry j c prev =
    ( mcState c
    , if j == 0 then Nothing else Just tlr
    , if | j == 0 -> Nothing
         | tlr -> Just (0, 0)
         | otherwise -> Just (mcTdi prev, fromInteger (mcIrIn prev))
    )
   where
    tlr = mcTap c == TestLogicReset

-- | Number of cycles of the test bench's clock after which the simulation
-- model must be done: TCK has a period of 100 ns, and the model stops it for a
-- millisecond for every 'Idle' 1.
timeoutCycles :: Int -> [VirtualJtagScan] -> Integer
timeoutCycles w scans =
  toInteger (P.length (hostTmsTdi w scans)) `P.div` 10 + 1000 * idle + 100
 where
  idle = P.sum [toInteger d | Idle d <- scans]

-- | Test bench: replays the expected trace on TCK, and checks that TCK
-- delivers all of its cycles before a timeout on the test bench's own clock.
-- The design returns the virtual instruction register as @ir_out@, and TDI as
-- TDO.
virtualJtagTB ::
  forall n m k.
  (KnownNat n, 1 <= n, n <= 24, KnownNat m, KnownNat k, 1 <= k) =>
  Vec m VirtualJtagScan ->
  Integer ->
  MemBlob k (BitSize (Maybe VirtualJtagState, Maybe Bool, Maybe (Bit, BitVector n))) ->
  Signal Slow Bool
virtualJtagTB scans timeout blob = simLibraries <$> done
 where
  vj = virtualJtag @Tck @n Nothing scans (vjIrIn vj) (vjTdi vj)
  tck = vjTck vj
  tdi = vjTdi vj
  irIn = vjIrIn vj
  state = vjState vj
  tlr = vjTestLogicReset vj

  -- Cycles of TCK, saturating at k: after the last cycle
  cnt :: Signal Tck (Index (k + 1))
  cnt = register tck noReset enableGen 0 (satSucc SatBound <$> cnt)
  expected = unpack . asyncRomBlob blob . fromIntegral . min (natToNum @(k - 1)) <$> cnt
  (expState, expTlr, expTdiIr) = unbundle expected
  inTrace = (< natToNum @k) <$> cnt

  actualTlr = mask <$> expTlr <*> tlr
  actualTdiIr = mask <$> expTdiIr <*> bundle (tdi, irIn)
  mask :: Maybe a -> b -> Maybe b
  mask e a = if isJust e then Just a else Nothing
  actual = bundle (state, actualTlr, actualTdiIr)
  expected1 = bundle (expState, expTlr, expTdiIr)
  checked = mux inTrace actual expected1
  finished =
    assert tck noReset "virtualJtag" checked expected1
      ((== maxBound) <$> cnt)

  -- The test bench's own clock waits for TCK to deliver all its cycles
  clk = tbClockGen (not <$> done)
  finishedSlow = unsafeSynchronizer tck clk finished
  timer :: Signal Slow (Unsigned 32)
  timer = register clk noReset enableGen 0 (timer + 1)
  timedOut = (== fromInteger timeout) <$> timer
  -- Delayed by a cycle, so that the assertion sees a time out before the
  -- simulation stops
  done0 = register clk noReset enableGen False (finishedSlow .||. timedOut)
  done =
    assert clk noReset "TCK delivered all cycles" (timedOut .&&. (not <$> finishedSlow)) (pure False)
      done0

-- * Scans

-- | Random scans, starting with an IR scan: IR scans, DR scans of random
-- lengths (some of which are a multiple of four), and some idle time
randomScans :: Int -> Int -> Word64 -> [VirtualJtagScan]
randomScans w n seed = IrScan (bits w r0) : P.take n (go rs)
 where
  r0 : rs = randoms seed
  bits :: Int -> Word64 -> Integer
  bits l r = toInteger r `P.mod` (2 P.^ l)
  go (k : r : rest) = case k `P.mod` 16 of
    0 -> Idle 1 : go rest
    t | t < 6 -> IrScan (bits w r) : go rest
      | otherwise ->
          let l = 1 + fromIntegral ((k `shiftR` 8) `P.mod` 40)
          in DrScan l (bits (P.fromIntegral l) r) : go rest
  go _ = []
