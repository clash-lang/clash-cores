{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tests of "Clash.Cores.Xilinx.Fft" against the Fast Fourier Transform IP: each
test bench plays stimuli from a ROM (resets, frames with random gaps between
them, random directions and scaling schedules), and compares the output of the
IP with the output of the simulation model in every clock cycle. Where the
model's output sample is undefined, because its frame overflowed, only the
other fields of the output are compared.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Fft where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Complex (Complex(..))
import Data.Maybe (fromMaybe)

import Clash.Cores.Xilinx.Fft.Explicit

import Fft.TH

-- | Compared output of one clock cycle: whether the IP is ready, and whether
-- there is an output sample and its fields. The fields are zero if there is
-- no output sample.
type Compared s nfft bw =
  ( Bool
  , Bool
  , Index (2 ^ nfft)
  , Complex (Signed (OutputWidth s nfft bw))
  , Bool
  , FrameStatus s
  )

-- | Play the samples from a ROM and compare the output of the function under
-- test with the expected output
fftTB ::
  forall s nfft bw n.
  ( KnownFft s nfft bw
  , KnownNat n
  , 1 <= n
  , BitPack (ScaleSchedule s nfft)
  , KnownNat (BitSize (Sample s nfft bw))
  ) =>
  Dut s nfft bw ->
  MemBlob n (BitSize (Sample s nfft bw)) ->
  Signal XilinxSystem Bool
fftTB dut content = done
 where
  cnt :: Signal XilinxSystem (Index n)
  cnt = delay clk enableGen 0 (satSucc SatBound <$> cnt)
  romOut = unpack . asyncRomBlob content <$> cnt :: Signal XilinxSystem (Sample s nfft bw)
  stim = fst <$> romOut
  expected = snd <$> romOut
  rstIn = (\(r, _, _) -> r) <$> stim
  cfg = (\(_, (d, sch), _) -> FrameConfig d sch) <$> stim
  input = (\(_, _, x) -> x) <$> stim

  (ready, out) = dut clk (unsafeFromActiveHigh rstIn) cfg input

  actual = toCompared <$> ready <*> out
  checked = fill <$> actual <*> expected

  toCompared :: Bool -> Maybe (FftOutput s nfft (OutputWidth s nfft bw)) -> Compared s nfft bw
  toCompared r Nothing = (r, False, 0, 0 :+ 0, False, unpack 0)
  toCompared r (Just FftOutput{index, value, lastSample, status}) =
    (r, True, index, value, lastSample, status)

  -- The expected output, with the sample of the actual output where the
  -- expected one is undefined
  fill :: Compared s nfft bw -> Expected s nfft bw -> Compared s nfft bw
  fill _ (r, Nothing) = (r, False, 0, 0 :+ 0, False, unpack 0)
  fill (_, _, _, actualSample, _, _) (r, Just (i, s, l, st)) =
    (r, True, i, fromMaybe actualSample s, l, st)

  done0 = delay clk enableGen False ((== maxBound) <$> cnt)
  done = mux done0 done0 (assert clk rst "fftTB" actual checked done0)

  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# INLINE fftTB #-}

scaledN8 :: Dut 'Scaled 3 16
scaledN8 = scaledN8'
{-# OPAQUE scaledN8 #-}
{-# ANN scaledN8 (topAnn "scaledN8") #-}

scaledN8TB :: Signal XilinxSystem Bool
scaledN8TB = fftTB scaledN8 $(memBlobTH (Just 0) (samples scaledN8' (stimuli @'Scaled @3 @16 1 150 45)))
{-# ANN scaledN8TB (TestBench 'scaledN8) #-}

scaledN32Conv :: Dut 'Scaled 5 16
scaledN32Conv = scaledN32Conv'
{-# OPAQUE scaledN32Conv #-}
{-# ANN scaledN32Conv (topAnn "scaledN32Conv") #-}

scaledN32ConvTB :: Signal XilinxSystem Bool
scaledN32ConvTB = fftTB scaledN32Conv $(memBlobTH (Just 0) (samples scaledN32Conv' (stimuli @'Scaled @5 @16 2 50 118)))
{-# ANN scaledN32ConvTB (TestBench 'scaledN32Conv) #-}

scaledN64Natural :: Dut 'Scaled 6 16
scaledN64Natural = scaledN64Natural'
{-# OPAQUE scaledN64Natural #-}
{-# ANN scaledN64Natural (topAnn "scaledN64Natural") #-}

scaledN64NaturalTB :: Signal XilinxSystem Bool
scaledN64NaturalTB = fftTB scaledN64Natural $(memBlobTH (Just 0) (samples scaledN64Natural' (stimuli @'Scaled @6 @16 3 30 215)))
{-# ANN scaledN64NaturalTB (TestBench 'scaledN64Natural) #-}

scaledN256 :: Dut 'Scaled 8 12
scaledN256 = scaledN256'
{-# OPAQUE scaledN256 #-}
{-# ANN scaledN256 (topAnn "scaledN256") #-}

scaledN256TB :: Signal XilinxSystem Bool
scaledN256TB = fftTB scaledN256 $(memBlobTH (Just 0) (samples scaledN256' (stimuli @'Scaled @8 @12 4 12 367)))
{-# ANN scaledN256TB (TestBench 'scaledN256) #-}

scaledN16Natural :: Dut 'Scaled 4 8
scaledN16Natural = scaledN16Natural'
{-# OPAQUE scaledN16Natural #-}
{-# ANN scaledN16Natural (topAnn "scaledN16Natural") #-}

scaledN16NaturalTB :: Signal XilinxSystem Bool
scaledN16NaturalTB = fftTB scaledN16Natural $(memBlobTH (Just 0) (samples scaledN16Natural' (stimuli @'Scaled @4 @8 5 100 86)))
{-# ANN scaledN16NaturalTB (TestBench 'scaledN16Natural) #-}

scaledN16Dsps :: Dut 'Scaled 4 16
scaledN16Dsps = scaledN16Dsps'
{-# OPAQUE scaledN16Dsps #-}
{-# ANN scaledN16Dsps (topAnn "scaledN16Dsps") #-}

scaledN16DspsTB :: Signal XilinxSystem Bool
scaledN16DspsTB = fftTB scaledN16Dsps $(memBlobTH (Just 0) (samples scaledN16Dsps' (stimuli @'Scaled @4 @16 6 100 65)))
{-# ANN scaledN16DspsTB (TestBench 'scaledN16Dsps) #-}

unscaledN128 :: Dut 'Unscaled 7 16
unscaledN128 = unscaledN128'
{-# OPAQUE unscaledN128 #-}
{-# ANN unscaledN128 (topAnn "unscaledN128") #-}

unscaledN128TB :: Signal XilinxSystem Bool
unscaledN128TB = fftTB unscaledN128 $(memBlobTH (Just 0) (samples unscaledN128' (stimuli @'Unscaled @7 @16 7 20 217)))
{-# ANN unscaledN128TB (TestBench 'unscaledN128) #-}

unscaledN512 :: Dut 'Unscaled 9 24
unscaledN512 = unscaledN512'
{-# OPAQUE unscaledN512 #-}
{-# ANN unscaledN512 (topAnn "unscaledN512") #-}

unscaledN512TB :: Signal XilinxSystem Bool
unscaledN512TB = fftTB unscaledN512 $(memBlobTH (Just 0) (samples unscaledN512' (stimuli @'Unscaled @9 @24 8 6 625)))
{-# ANN unscaledN512TB (TestBench 'unscaledN512) #-}

bfpN64 :: Dut 'BlockFloatingPoint 6 16
bfpN64 = bfpN64'
{-# OPAQUE bfpN64 #-}
{-# ANN bfpN64 (topAnn "bfpN64") #-}

bfpN64TB :: Signal XilinxSystem Bool
bfpN64TB = fftTB bfpN64 $(memBlobTH (Just 0) (samples bfpN64' (stimuli @'BlockFloatingPoint @6 @16 9 30 214)))
{-# ANN bfpN64TB (TestBench 'bfpN64) #-}

bfpN32ConvNatural :: Dut 'BlockFloatingPoint 5 16
bfpN32ConvNatural = bfpN32ConvNatural'
{-# OPAQUE bfpN32ConvNatural #-}
{-# ANN bfpN32ConvNatural (topAnn "bfpN32ConvNatural") #-}

bfpN32ConvNaturalTB :: Signal XilinxSystem Bool
bfpN32ConvNaturalTB = fftTB bfpN32ConvNatural $(memBlobTH (Just 0) (samples bfpN32ConvNatural' (stimuli @'BlockFloatingPoint @5 @16 10 50 152)))
{-# ANN bfpN32ConvNaturalTB (TestBench 'bfpN32ConvNatural) #-}
