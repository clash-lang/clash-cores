{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE RecordWildCards #-}

module XpmFifoAxisTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Maybe (isJust)
import Data.Proxy
import Language.Haskell.TH.Lib (ExpQ)
import Protocols.Experimental.Axi4.Stream

import Clash.Cores.Xilinx.Xpm.Fifo.Axis

import XpmFifoTestCommon

-- | Stream configuration used by most tests: two bytes of data, a 3 bit ID, a
-- 2 bit destination, and a 2 bit user field
type Conf = 'Axi4StreamConfig 2 3 2
type User = BitVector 2

-- | Stream configuration with zero width ID and destination and a zero width
-- user field. The wrapper should pad them.
type ConfNarrow = 'Axi4StreamConfig 1 0 0
type UserNarrow = ()

-- | Turn a counter value into a stream payload. All fields are derived from
-- the counter, so the fields of different transfers differ.
mkPayload ::
  forall conf user .
  ( KnownAxi4StreamConfig conf
  , BitPack user
  ) =>
  Unsigned 16 -> Bool -> Axi4StreamM2S conf user
mkPayload n lst = Axi4StreamM2S
  { _tdata = map fromIntegral (iterateI (`rotateL` 3) n)
  , _tkeep = map (testBit n) (iterateI (+ 1) 0)
  , _tstrb = map (testBit n) (iterateI (+ 1) 5)
  , _tlast = lst
  , _tid = fromIntegral (n `rotateR` 4)
  , _tdest = fromIntegral (n `rotateR` 8)
  , _tuser = unpack (resize (pack (n `rotateR` 12)))
  }

packPayload ::
  ( KnownAxi4StreamConfig conf
  , BitPack user
  ) =>
  Bool -> Axi4StreamM2S conf user -> BitVector 128
packPayload x Axi4StreamM2S{..} =
      field x 0 _tdata
  .|. field x 64 _tkeep
  .|. field x 72 _tstrb
  .|. field x 80 _tlast
  .|. field x 81 _tid
  .|. field x 89 _tdest
  .|. field x 97 _tuser

-- | Source of AXI4-Stream transfers, carrying a counter. Whether it offers
-- data is random. Once it offers data, it keeps offering it until it is
-- accepted, as AXI requires. Also generates the reset for the FIFO.
source ::
  forall conf user dom .
  ( KnownDomain dom
  , KnownAxi4StreamConfig conf
  , BitPack user
  ) =>
  Stimuli ->
  Clock dom ->
  -- | @s_axis_tready@
  Signal dom Axi4StreamS2M ->
  (Reset dom, Signal dom (Maybe (Axi4StreamM2S conf user)))
source stim@Stimuli{..} clk ready = (rst, out)
 where
  cycleCount = cycleCounter clk
  rnd = lfsr clk 0xACE1
  rst = unsafeFromActiveHigh (delay clk enableGen True (inReset stim <$> cycleCount))

  out = mealy clk noReset enableGen go (False, 0, False) (bundle (ready, rnd, cycleCount))

  go (pend, n, lst) (Axi4StreamS2M rdy, r, c) = ((pend', n', lst'), o)
   where
    o = if pend then Just (mkPayload n lst) else Nothing
    accepted = pend && rdy
    n1 = if accepted then n + 1 else n
    -- Offer new data with a probability depending on the phase
    offer = chance (phaseOf stWritePhases c) r
    (pend', n', lst')
      | pend && not rdy = (pend, n, lst)
      | otherwise = (offer, n1, testBit r 9 && testBit r 10)

-- | Sink: sets @m_axis_tready@ randomly
sink ::
  forall dom .
  KnownDomain dom =>
  Stimuli ->
  Clock dom ->
  Signal dom Axi4StreamS2M
sink Stimuli{..} clk = Axi4StreamS2M <$> (chance <$> (phaseOf stReadPhases <$> cycleCount) <*> rnd)
 where
  cycleCount = cycleCounter clk
  rnd = lfsr clk 0x1D0F

-- | Everything observed in the write domain
type WrObs depth = (Bool, XpmFifoWriteStatus depth)

packWrObs :: (KnownNat depth, 1 <= depth) => Bool -> WrObs depth -> BitVector 128
packWrObs x (rdy, XpmFifoWriteStatus{..}) =
      field x 0 rdy
  .|. field x 1 progFull
  .|. field x 2 almostFull
  .|. field x 3 writeDataCount

-- | Everything observed in the read domain
type RdObs depth conf user = (Maybe (Axi4StreamM2S conf user), XpmFifoReadStatus depth)

packRdObs ::
  ( KnownNat depth, 1 <= depth
  , KnownAxi4StreamConfig conf
  , BitPack user
  ) =>
  Bool -> RdObs depth conf user -> BitVector 128
packRdObs x (dat, XpmFifoReadStatus{..}) =
      field x 0 progEmpty
  .|. field x 1 almostEmpty
  .|. field x 2 readDataCount
  .|. field x 26 (isJust dat)
  .|. (payload `shiftL` 27)
 where
  payload0 = maybe (deepErrorX "no data") (packPayload x) dat
  payload = if x then packX payload0 else payload0

-- | Test circuit, common clock
topSync ::
  forall depth conf user dom .
  ( KnownDomain dom
  , KnownNat depth, 16 <= depth, depth <= 4194304, FLog 2 depth ~ CLog 2 depth
  , KnownAxi4StreamConfig conf, 1 <= DataWidth conf, DataWidth conf <= 256
  , IdWidth conf <= 32, DestWidth conf <= 32
  , BitPack user, NFDataX user
  ) =>
  Stimuli ->
  XpmFifoAxisConfig depth ->
  Clock dom ->
  (Signal dom (WrObs depth), Signal dom (RdObs depth conf user))
topSync stim cfg clk = (bundle (_tready <$> sBwd, wrStatus), bundle (mOut, rdStatus))
 where
  (rst, sIn) = source stim clk sBwd
  mBwd = sink stim clk
  (sBwd, mOut, wrStatus, rdStatus) = xpmFifoAxis @depth @conf @user cfg clk rst sIn mBwd
{-# NOINLINE topSync #-}

-- | Test circuit, independent clocks
topAsync ::
  forall depth conf user s m .
  ( KnownDomain s, KnownDomain m
  , KnownNat depth, 16 <= depth, depth <= 4194304, FLog 2 depth ~ CLog 2 depth
  , KnownAxi4StreamConfig conf, 1 <= DataWidth conf, DataWidth conf <= 256
  , IdWidth conf <= 32, DestWidth conf <= 32
  , BitPack user, NFDataX user
  ) =>
  Stimuli ->
  XpmFifoAxisConfig depth ->
  Clock s ->
  Clock m ->
  (Signal s (WrObs depth), Signal m (RdObs depth conf user))
topAsync stim cfg clkS clkM = (bundle (_tready <$> sBwd, wrStatus), bundle (mOut, rdStatus))
 where
  (rst, sIn) = source stim clkS sBwd
  mBwd = sink stim clkM
  (sBwd, mOut, wrStatus, rdStatus) = xpmFifoAxisAsync @depth @conf @user cfg clkS clkM rst sIn mBwd
{-# NOINLINE topAsync #-}

tbSync ::
  forall depth conf user dom nW nR .
  ( KnownDomain dom
  , KnownNat depth, 16 <= depth, depth <= 4194304, FLog 2 depth ~ CLog 2 depth
  , KnownAxi4StreamConfig conf, 1 <= DataWidth conf, DataWidth conf <= 256
  , IdWidth conf <= 32, DestWidth conf <= 32
  , BitPack user, NFDataX user
  , KnownNat nW, 1 <= nW, KnownNat nR, 1 <= nR
  ) =>
  Proxy dom ->
  Stimuli ->
  XpmFifoAxisConfig depth ->
  (MemBlob nW (128 + 128), MemBlob nR (128 + 128)) ->
  Signal dom Bool
tbSync Proxy stim cfg (expectedW, expectedR) = done
 where
  (wrObs, rdObs) = topSync @depth @conf @user stim cfg clk
  doneW = verify clk "write side" expectedW (packWrObs False <$> wrObs)
  doneR = verify clk "read side" expectedR (packRdObs False <$> rdObs)
  done = (&&) <$> doneW <*> doneR
  clk :: Clock dom
  clk = tbClockGen (not <$> done)

tbAsync ::
  forall depth conf user s m nW nR .
  ( KnownDomain s, KnownDomain m
  , KnownNat depth, 16 <= depth, depth <= 4194304, FLog 2 depth ~ CLog 2 depth
  , KnownAxi4StreamConfig conf, 1 <= DataWidth conf, DataWidth conf <= 256
  , IdWidth conf <= 32, DestWidth conf <= 32
  , BitPack user, NFDataX user
  , KnownNat nW, 1 <= nW, KnownNat nR, 1 <= nR
  ) =>
  Proxy s ->
  Proxy m ->
  Stimuli ->
  XpmFifoAxisConfig depth ->
  (MemBlob nW (128 + 128), MemBlob nR (128 + 128)) ->
  Signal m Bool
tbAsync Proxy Proxy stim cfg (expectedW, expectedR) = done
 where
  (wrObs, rdObs) = topAsync @depth @conf @user stim cfg clkS clkM
  doneW = verify clkS "write side" expectedW (packWrObs False <$> wrObs)
  doneR = verify clkM "read side" expectedR (packRdObs False <$> rdObs)
  done = (&&) <$> doneR <*> unsafeSynchronizer clkS clkM doneW
  clkS :: Clock s
  clkS = tbClockGen (not <$> unsafeSynchronizer clkM clkS done)
  clkM :: Clock m
  clkM = tbClockGen (not <$> done)

-- | Expected values for 'tbSync'. Simulates for the given duration.
expectedSync ::
  forall depth conf user dom .
  ( KnownDomain dom
  , KnownNat depth, 16 <= depth, depth <= 4194304, FLog 2 depth ~ CLog 2 depth
  , KnownAxi4StreamConfig conf, 1 <= DataWidth conf, DataWidth conf <= 256
  , IdWidth conf <= 32, DestWidth conf <= 32
  , BitPack user, NFDataX user
  ) =>
  Proxy dom ->
  Stimuli ->
  XpmFifoAxisConfig depth ->
  ExpQ
expectedSync Proxy stim cfg =
  expectedTH
    (samplesFor @dom stTestDuration (packWrObs True <$> wrObs))
    (samplesFor @dom stTestDuration (packRdObs True <$> rdObs))
 where
  Stimuli{stTestDuration} = stim
  (wrObs, rdObs) = topSync @depth @conf @user stim cfg (clockGen @dom)

-- | Expected values for 'tbAsync'. Simulates for the given duration.
expectedAsync ::
  forall depth conf user s m .
  ( KnownDomain s, KnownDomain m
  , KnownNat depth, 16 <= depth, depth <= 4194304, FLog 2 depth ~ CLog 2 depth
  , KnownAxi4StreamConfig conf, 1 <= DataWidth conf, DataWidth conf <= 256
  , IdWidth conf <= 32, DestWidth conf <= 32
  , BitPack user, NFDataX user
  ) =>
  Proxy s ->
  Proxy m ->
  Stimuli ->
  XpmFifoAxisConfig depth ->
  ExpQ
expectedAsync Proxy Proxy stim cfg =
  expectedTH
    (samplesFor @s stTestDuration (packWrObs True <$> wrObs))
    (samplesFor @m stTestDuration (packRdObs True <$> rdObs))
 where
  Stimuli{stTestDuration} = stim
  (wrObs, rdObs) = topAsync @depth @conf @user stim cfg (clockGen @s) (clockGen @m)
