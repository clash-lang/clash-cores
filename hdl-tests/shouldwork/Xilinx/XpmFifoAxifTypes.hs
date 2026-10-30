{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module XpmFifoAxifTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Proxy
import Language.Haskell.TH.Lib (ExpQ)

import Clash.Cores.Xilinx.Xpm.Fifo.Axif

import XpmFifoTestCommon

-- | Test configuration: 2 bit IDs, 12 bit addresses, 4 data bytes, and user
-- signals of various widths, including zero.
type Conf = 'Axi4Config 2 12 4 1 2 0 3 1

-- | Configuration with zero width IDs and user signals
type ConfNarrow = 'Axi4Config 0 7 1 0 0 0 0 0

type C c = (KnownAxi4Config c, 1 <= AxiAddrWidth c, 1 <= AxiDataBytes c)

mkAddress :: forall i a u . (KnownNat i, KnownNat a, KnownNat u) => Unsigned 16 -> Axi4Address i a u
mkAddress n = Axi4Address
  { axiId = resize (pack (n `rotateR` 11))
  , axiAddr = resize (pack (n `rotateL` 5))
  , axiLen = resize (pack (n `rotateR` 3))
  , axiSize = resize (pack (n `rotateR` 1))
  , axiBurst = resize (pack (n `rotateR` 6))
  , axiLock = msb (pack n)
  , axiCache = resize (pack (n `rotateR` 7))
  , axiProt = resize (pack (n `rotateR` 2))
  , axiQos = resize (pack (n `rotateR` 9))
  , axiRegion = resize (pack (n `rotateR` 12))
  , axiUser = resize (pack (n `rotateR` 13))
  }

mkWriteData :: forall b u . (KnownNat b, KnownNat u) => Unsigned 16 -> Axi4WriteData b u
mkWriteData n = Axi4WriteData
  { axiWData = resize (pack (n `rotateL` 3, complement n, n))
  , axiWStrb = resize (pack (n `rotateR` 7))
  , axiWLast = testBit n 2
  , axiWUser = resize (pack (n `rotateR` 4))
  }

mkResp :: Unsigned 16 -> AxiResp
mkResp n = unpack (resize (pack (n `rotateR` 1)))

mkWriteResponse :: forall i u . (KnownNat i, KnownNat u) => Unsigned 16 -> Axi4WriteResponse i u
mkWriteResponse n = Axi4WriteResponse
  { axiBId = resize (pack (n `rotateR` 5))
  , axiBResp = mkResp n
  , axiBUser = resize (pack (n `rotateR` 9))
  }

mkReadData :: forall i b u . (KnownNat i, KnownNat b, KnownNat u) => Unsigned 16 -> Axi4ReadData i b u
mkReadData n = Axi4ReadData
  { axiRId = resize (pack (n `rotateR` 3))
  , axiRData = resize (pack (complement n, n `rotateL` 9, n))
  , axiRResp = mkResp (n + 1)
  , axiRLast = testBit n 1
  , axiRUser = resize (pack (n `rotateR` 8))
  }

-- | AXI4 manager driving all channels randomly. Payloads are derived from
-- per-channel counters.
manager ::
  (KnownDomain dom, C c) =>
  Stimuli -> Clock dom -> Signal dom (Axi4S2M c) -> Signal dom (Axi4M2S c)
manager Stimuli{..} clk s2m =
  Axi4M2S
    <$> (fmap mkAddress <$> stream stWritePhases 0xACE1 clk (axiAwReady <$> s2m))
    <*> (fmap mkWriteData <$> stream stWritePhases 0x1234 clk (axiWReady <$> s2m))
    <*> readyGen stReadPhases 0xBEEF clk
    <*> (fmap mkAddress <$> stream stWritePhases 0x4321 clk (axiArReady <$> s2m))
    <*> readyGen stReadPhases 0x7777 clk

-- | AXI4 subordinate driving all channels randomly. Payloads are derived from
-- per-channel counters.
subordinate ::
  (KnownDomain dom, C c) =>
  Stimuli -> Clock dom -> Signal dom (Axi4M2S c) -> Signal dom (Axi4S2M c)
subordinate Stimuli{..} clk m2s =
  Axi4S2M
    <$> readyGen stReadPhases 0x1D0F clk
    <*> readyGen stReadPhases 0x2E1F clk
    <*> (fmap mkWriteResponse <$> stream stWritePhases 0x3F2F clk (axiBReady <$> m2s))
    <*> readyGen stReadPhases 0x4A3F clk
    <*> (fmap mkReadData <$> stream stWritePhases 0x5B4F clk (axiRReady <$> m2s))

-- | Pack an 'Axi4Address' into at most 64 bits
packAddress :: (KnownNat i, KnownNat a, KnownNat u) => Axi4Address i a u -> BitVector 64
packAddress = resize . pack

-- | Everything observed in the subordinate side's domain. Bit vectors are
-- reduced to 128 bits by xor-ing slices, which is good enough to detect
-- differences.
packS ::
  (C c, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Bool ->
  (Axi4S2M c, XpmFifoWriteStatus wdch, XpmFifoReadStatus rdch) ->
  BitVector 128
packS x (Axi4S2M{..}, wdch, rdch) =
      field x 0 axiAwReady
  .|. field x 1 axiWReady
  .|. fieldMaybe x 2 (resize @_ @_ @16 . pack <$> axiB)
  .|. field x 19 axiArReady
  .|. fieldMaybe x 20 (resize @_ @_ @64 . pack <$> axiR)
  .|. field x 85 (progFull wdch)
  .|. field x 86 (writeDataCount wdch)
  .|. field x 96 (progEmpty rdch)
  .|. field x 97 (readDataCount rdch)

-- | Everything observed in the manager side's domain
packM ::
  (C c, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Bool ->
  (Axi4M2S c, XpmFifoReadStatus wdch, XpmFifoWriteStatus rdch) ->
  BitVector 256
packM x (Axi4M2S{..}, wdch, rdch) =
      fieldMaybe x 0 (packAddress <$> axiAw)
  .|. field x 65 axiBReady
  .|. field x 66 axiRReady
  .|. field x 67 (progEmpty wdch)
  .|. field x 68 (readDataCount wdch)
  .|. field x 78 (progFull rdch)
  .|. field x 79 (writeDataCount rdch)
  .|. fieldMaybe x 128 (resize @_ @_ @60 . pack <$> axiW)
  .|. fieldMaybe x 189 (packAddress <$> axiAr)

type Obs c s m wdch rdch =
  ( Signal s (Axi4S2M c, XpmFifoWriteStatus wdch, XpmFifoReadStatus rdch)
  , Signal m (Axi4M2S c, XpmFifoReadStatus wdch, XpmFifoWriteStatus rdch) )

topSync ::
  forall c wdch rdch dom .
  (C c, KnownDomain dom, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Stimuli -> XpmFifoAxifConfig wdch rdch -> Clock dom -> Obs c dom dom wdch rdch
topSync stim cfg clk =
  ( bundle (sS2M, axifWriteDataWriteStatus st, axifReadDataReadStatus st)
  , bundle (mM2S, axifWriteDataReadStatus st, axifReadDataWriteStatus st) )
 where
  rst = stimuliReset stim clk
  sM2S = manager stim clk sS2M
  mS2M = subordinate stim clk mM2S
  (sS2M, mM2S, st) = xpmFifoAxif cfg clk rst sM2S mS2M
{-# NOINLINE topSync #-}

topAsync ::
  forall c wdch rdch s m .
  (C c, KnownDomain s, KnownDomain m, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Stimuli -> XpmFifoAxifConfig wdch rdch -> Clock s -> Clock m -> Obs c s m wdch rdch
topAsync stim cfg clkS clkM =
  ( bundle (sS2M, axifWriteDataWriteStatus st, axifReadDataReadStatus st)
  , bundle (mM2S, axifWriteDataReadStatus st, axifReadDataWriteStatus st) )
 where
  rst = stimuliReset stim clkS
  sM2S = manager stim clkS sS2M
  mS2M = subordinate stim clkM mM2S
  (sS2M, mM2S, st) = xpmFifoAxifAsync cfg clkS clkM rst sM2S mS2M
{-# NOINLINE topAsync #-}

tbSync ::
  forall c wdch rdch dom nS nM .
  ( C c, KnownDomain dom, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch
  , KnownNat nS, 1 <= nS, KnownNat nM, 1 <= nM ) =>
  Proxy dom -> Stimuli -> XpmFifoAxifConfig wdch rdch ->
  (MemBlob nS (128 + 128), MemBlob nM (256 + 256)) ->
  Signal dom Bool
tbSync Proxy stim cfg (expectedS, expectedM) = done
 where
  (obsS, obsM) = topSync @c stim cfg clk
  doneS = verify clk "subordinate side" expectedS (packS False <$> obsS)
  doneM = verify clk "manager side" expectedM (packM False <$> obsM)
  done = (&&) <$> doneS <*> doneM
  clk :: Clock dom
  clk = tbClockGen (not <$> done)

tbAsync ::
  forall c wdch rdch s m nS nM .
  ( C c, KnownDomain s, KnownDomain m, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch
  , KnownNat nS, 1 <= nS, KnownNat nM, 1 <= nM ) =>
  Proxy s -> Proxy m -> Stimuli -> XpmFifoAxifConfig wdch rdch ->
  (MemBlob nS (128 + 128), MemBlob nM (256 + 256)) ->
  Signal m Bool
tbAsync Proxy Proxy stim cfg (expectedS, expectedM) = done
 where
  (obsS, obsM) = topAsync @c stim cfg clkS clkM
  doneS = verify clkS "subordinate side" expectedS (packS False <$> obsS)
  doneM = verify clkM "manager side" expectedM (packM False <$> obsM)
  done = (&&) <$> doneM <*> unsafeSynchronizer clkS clkM doneS
  clkS :: Clock s
  clkS = tbClockGen (not <$> unsafeSynchronizer clkM clkS done)
  clkM :: Clock m
  clkM = tbClockGen (not <$> done)

expectedSync ::
  forall c wdch rdch dom .
  (C c, KnownDomain dom, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Proxy dom -> Stimuli -> XpmFifoAxifConfig wdch rdch -> ExpQ
expectedSync Proxy stim cfg =
  expectedTH
    (samplesFor @dom (stTestDuration stim) (packS True <$> obsS))
    (samplesFor @dom (stTestDuration stim) (packM True <$> obsM))
 where
  (obsS, obsM) = topSync @c stim cfg (clockGen @dom)

expectedAsync ::
  forall c wdch rdch s m .
  (C c, KnownDomain s, KnownDomain m, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Proxy s -> Proxy m -> Stimuli -> XpmFifoAxifConfig wdch rdch -> ExpQ
expectedAsync Proxy Proxy stim cfg =
  expectedTH
    (samplesFor @s (stTestDuration stim) (packS True <$> obsS))
    (samplesFor @m (stTestDuration stim) (packM True <$> obsM))
 where
  (obsS, obsM) = topAsync @c stim cfg (clockGen @s) (clockGen @m)
