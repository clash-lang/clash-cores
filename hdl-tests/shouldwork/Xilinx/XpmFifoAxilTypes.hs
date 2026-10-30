{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module XpmFifoAxilTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Proxy
import Language.Haskell.TH.Lib (ExpQ)

import Clash.Cores.Xilinx.Xpm.Fifo.Axil

import XpmFifoTestCommon

type AddrWidth = 13
type DataBytes = 4

mkAddress :: Unsigned 16 -> Axi4LiteAddress AddrWidth
mkAddress n = Axi4LiteAddress
  { axilAddr = resize (pack (n `rotateL` 5))
  , axilProt = resize (pack (n `rotateR` 2))
  }

mkWriteData :: Unsigned 16 -> Axi4LiteWriteData DataBytes
mkWriteData n = Axi4LiteWriteData
  { axilWData = pack (n `rotateL` 3, complement n)
  , axilWStrb = resize (pack (n `rotateR` 7))
  }

mkResp :: Unsigned 16 -> AxiResp
mkResp n = unpack (resize (pack (n `rotateR` 1)))

mkReadData :: Unsigned 16 -> Axi4LiteReadData DataBytes
mkReadData n = Axi4LiteReadData
  { axilRData = pack (complement n, n `rotateL` 9)
  , axilRResp = mkResp (n + 1)
  }

-- | AXI4-Lite manager driving all channels randomly. Address and data are
-- derived from per-channel counters.
manager ::
  KnownDomain dom =>
  Stimuli -> Clock dom -> Signal dom (Axi4LiteS2M DataBytes) -> Signal dom (Axi4LiteM2S AddrWidth DataBytes)
manager Stimuli{..} clk s2m =
  Axi4LiteM2S
    <$> (fmap mkAddress <$> stream stWritePhases 0xACE1 clk (axilAwReady <$> s2m))
    <*> (fmap mkWriteData <$> stream stWritePhases 0x1234 clk (axilWReady <$> s2m))
    <*> readyGen stReadPhases 0xBEEF clk
    <*> (fmap mkAddress <$> stream stWritePhases 0x4321 clk (axilArReady <$> s2m))
    <*> readyGen stReadPhases 0x7777 clk

-- | AXI4-Lite subordinate driving all channels randomly. Responses and data
-- are derived from per-channel counters.
subordinate ::
  KnownDomain dom =>
  Stimuli -> Clock dom -> Signal dom (Axi4LiteM2S AddrWidth DataBytes) -> Signal dom (Axi4LiteS2M DataBytes)
subordinate Stimuli{..} clk m2s =
  Axi4LiteS2M
    <$> readyGen stReadPhases 0x1D0F clk
    <*> readyGen stReadPhases 0x2E1F clk
    <*> (fmap mkResp <$> stream stWritePhases 0x3F2F clk (axilBReady <$> m2s))
    <*> readyGen stReadPhases 0x4A3F clk
    <*> (fmap mkReadData <$> stream stWritePhases 0x5B4F clk (axilRReady <$> m2s))

-- | Everything observed in the subordinate side's domain
packS ::
  (KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Bool ->
  (Axi4LiteS2M DataBytes, XpmFifoWriteStatus wdch, XpmFifoReadStatus rdch) ->
  BitVector 128
packS x (Axi4LiteS2M{..}, wdch, rdch) =
      field x 0 axilAwReady
  .|. field x 1 axilWReady
  .|. fieldMaybe x 2 axilB
  .|. field x 5 axilArReady
  .|. fieldMaybe x 6 axilR
  .|. field x 41 (progFull wdch)
  .|. field x 42 (writeDataCount wdch)
  .|. field x 66 (progEmpty rdch)
  .|. field x 67 (readDataCount rdch)

-- | Everything observed in the manager side's domain
packM ::
  (KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Bool ->
  (Axi4LiteM2S AddrWidth DataBytes, XpmFifoReadStatus wdch, XpmFifoWriteStatus rdch) ->
  BitVector 128
packM x (Axi4LiteM2S{..}, wdch, rdch) =
      fieldMaybe x 0 axilAw
  .|. fieldMaybe x 17 axilW
  .|. field x 54 axilBReady
  .|. fieldMaybe x 55 axilAr
  .|. field x 72 axilRReady
  .|. field x 73 (progEmpty wdch)
  .|. field x 74 (readDataCount wdch)
  .|. field x 98 (progFull rdch)
  .|. field x 99 (writeDataCount rdch)

type Obs s m wdch rdch =
  ( Signal s (Axi4LiteS2M DataBytes, XpmFifoWriteStatus wdch, XpmFifoReadStatus rdch)
  , Signal m (Axi4LiteM2S AddrWidth DataBytes, XpmFifoReadStatus wdch, XpmFifoWriteStatus rdch) )

topSync ::
  forall wdch rdch dom .
  (KnownDomain dom, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Stimuli -> XpmFifoAxilConfig wdch rdch -> Clock dom -> Obs dom dom wdch rdch
topSync stim cfg clk =
  ( bundle (sS2M, axilWriteDataWriteStatus st, axilReadDataReadStatus st)
  , bundle (mM2S, axilWriteDataReadStatus st, axilReadDataWriteStatus st) )
 where
  rst = stimuliReset stim clk
  sM2S = manager stim clk sS2M
  mS2M = subordinate stim clk mM2S
  (sS2M, mM2S, st) = xpmFifoAxil cfg clk rst sM2S mS2M
{-# NOINLINE topSync #-}

topAsync ::
  forall wdch rdch s m .
  (KnownDomain s, KnownDomain m, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Stimuli -> XpmFifoAxilConfig wdch rdch -> Clock s -> Clock m -> Obs s m wdch rdch
topAsync stim cfg clkS clkM =
  ( bundle (sS2M, axilWriteDataWriteStatus st, axilReadDataReadStatus st)
  , bundle (mM2S, axilWriteDataReadStatus st, axilReadDataWriteStatus st) )
 where
  rst = stimuliReset stim clkS
  sM2S = manager stim clkS sS2M
  mS2M = subordinate stim clkM mM2S
  (sS2M, mM2S, st) = xpmFifoAxilAsync cfg clkS clkM rst sM2S mS2M
{-# NOINLINE topAsync #-}

tbSync ::
  forall wdch rdch dom nS nM .
  ( KnownDomain dom, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch
  , KnownNat nS, 1 <= nS, KnownNat nM, 1 <= nM ) =>
  Proxy dom -> Stimuli -> XpmFifoAxilConfig wdch rdch ->
  (MemBlob nS (128 + 128), MemBlob nM (128 + 128)) ->
  Signal dom Bool
tbSync Proxy stim cfg (expectedS, expectedM) = done
 where
  (obsS, obsM) = topSync stim cfg clk
  doneS = verify clk "subordinate side" expectedS (packS False <$> obsS)
  doneM = verify clk "manager side" expectedM (packM False <$> obsM)
  done = (&&) <$> doneS <*> doneM
  clk :: Clock dom
  clk = tbClockGen (not <$> done)

tbAsync ::
  forall wdch rdch s m nS nM .
  ( KnownDomain s, KnownDomain m, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch
  , KnownNat nS, 1 <= nS, KnownNat nM, 1 <= nM ) =>
  Proxy s -> Proxy m -> Stimuli -> XpmFifoAxilConfig wdch rdch ->
  (MemBlob nS (128 + 128), MemBlob nM (128 + 128)) ->
  Signal m Bool
tbAsync Proxy Proxy stim cfg (expectedS, expectedM) = done
 where
  (obsS, obsM) = topAsync stim cfg clkS clkM
  doneS = verify clkS "subordinate side" expectedS (packS False <$> obsS)
  doneM = verify clkM "manager side" expectedM (packM False <$> obsM)
  done = (&&) <$> doneM <*> unsafeSynchronizer clkS clkM doneS
  clkS :: Clock s
  clkS = tbClockGen (not <$> unsafeSynchronizer clkM clkS done)
  clkM :: Clock m
  clkM = tbClockGen (not <$> done)

expectedSync ::
  forall wdch rdch dom .
  (KnownDomain dom, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Proxy dom -> Stimuli -> XpmFifoAxilConfig wdch rdch -> ExpQ
expectedSync Proxy stim cfg =
  expectedTH
    (samplesFor @dom (stTestDuration stim) (packS True <$> obsS))
    (samplesFor @dom (stTestDuration stim) (packM True <$> obsM))
 where
  (obsS, obsM) = topSync stim cfg (clockGen @dom)

expectedAsync ::
  forall wdch rdch s m .
  (KnownDomain s, KnownDomain m, KnownNat wdch, 1 <= wdch, KnownNat rdch, 1 <= rdch) =>
  Proxy s -> Proxy m -> Stimuli -> XpmFifoAxilConfig wdch rdch -> ExpQ
expectedAsync Proxy Proxy stim cfg =
  expectedTH
    (samplesFor @s (stTestDuration stim) (packS True <$> obsS))
    (samplesFor @m (stTestDuration stim) (packM True <$> obsM))
 where
  (obsS, obsM) = topAsync stim cfg (clockGen @s) (clockGen @m)
