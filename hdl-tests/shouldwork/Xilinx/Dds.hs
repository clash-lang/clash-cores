{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tests of "Clash.Cores.Xilinx.Dds": the test benches instantiate the Xilinx DDS
Compiler IP and compare its outputs, cycle by cycle, with the outputs of the
simulation model (computed in "Dds.TH"). Each test uses a different
configuration of the IP; most of them also stall the IP with its clock enable.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module Dds where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Clash.Explicit.Testbench

import Clash.Cores.Xilinx.Dds (Phase, Sample)

import Dds.TH

-- | Play the samples in a ROM, one per cycle. The first output is 'True' once
-- all samples have been played.
playSampleRom ::
  forall n a dom.
  (KnownDomain dom, KnownNat n, BitPack a, 1 <= n) =>
  Clock dom ->
  Reset dom ->
  MemBlob n (BitSize a) ->
  (Signal dom Bool, Signal dom a)
playSampleRom clk rst content = (done, out)
 where
  out = unpack . asyncRomBlob content <$> cnt
  done = CEP.register clk rst enableGen False $ (== maxBound) <$> cnt
  cnt :: Signal dom (Index n)
  cnt = CEP.register clk rst enableGen 0 $ satSucc SatBound <$> cnt

-- | Test bench playing enables, CONFIG channel and input PHASE channel inputs
-- from a ROM to a design, and comparing its outputs with the expected outputs
-- in the ROM. Outputs that the simulation model leaves undefined are not
-- checked.
romTB ::
  forall n i o.
  ( KnownNat n
  , BitPack i
  , BitPack o
  , Eq o
  , ShowX o
  , 1 <= n
  ) =>
  (Clock XilinxSystem -> Enable XilinxSystem -> Signal XilinxSystem i -> Signal XilinxSystem o) ->
  MemBlob n (BitSize ((Bool, i), (Bool, o))) ->
  Signal XilinxSystem Bool
romTB design blob = done
 where
  (done0, samples) = playSampleRom clk rst blob
  (inputs, expected) = unbundle samples
  (en, x) = unbundle inputs
  (valid, expectedOut) = unbundle expected
  out = mux valid (design clk (toEnable en) x) (pure (unpack 0))
  done = mux done0 done0 $ assert clk rst "romTB" out expectedOut done0
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# INLINE romTB #-}

------------------------------------------------------------------------------
-- DDS
------------------------------------------------------------------------------

ddsFixed :: Dut XilinxSystem NoChannels NoChannels (Sample 11, Sample 11)
ddsFixed = ddsFixedDut
{-# OPAQUE ddsFixed #-}
{-# ANN ddsFixed (dutAnn "ddsFixed") #-}

ddsFixedTB :: Signal XilinxSystem Bool
ddsFixedTB = romTB ddsFixed $(memBlobTH Nothing ddsFixedSamples)
{-# ANN ddsFixedTB (TestBench 'ddsFixed) #-}

ddsStreaming :: Dut XilinxSystem NoChannels ((Phase 12, Bool), Phase 12) (Sample 9, Sample 9)
ddsStreaming = ddsStreamingDut
{-# OPAQUE ddsStreaming #-}
{-# ANN ddsStreaming (dutAnn "ddsStreaming") #-}

ddsStreamingTB :: Signal XilinxSystem Bool
ddsStreamingTB = romTB ddsStreaming $(memBlobTH Nothing ddsStreamingSamples)
{-# ANN ddsStreamingTB (TestBench 'ddsStreaming) #-}

ddsProgrammable :: Dut XilinxSystem (Phase 20, Phase 20) NoChannels (Sample 13)
ddsProgrammable = ddsProgrammableDut
{-# OPAQUE ddsProgrammable #-}
{-# ANN ddsProgrammable (dutAnn "ddsProgrammable") #-}

ddsProgrammableTB :: Signal XilinxSystem Bool
ddsProgrammableTB = romTB ddsProgrammable $(memBlobTH Nothing ddsProgrammableSamples)
{-# ANN ddsProgrammableTB (TestBench 'ddsProgrammable) #-}

ddsMixed :: Dut XilinxSystem (Phase 32, ()) ((), Phase 32) (Sample 15)
ddsMixed = ddsMixedDut
{-# OPAQUE ddsMixed #-}
{-# ANN ddsMixed (dutAnn "ddsMixed") #-}

ddsMixedTB :: Signal XilinxSystem Bool
ddsMixedTB = romTB ddsMixed $(memBlobTH Nothing ddsMixedSamples)
{-# ANN ddsMixedTB (TestBench 'ddsMixed) #-}

ddsWide :: Dut XilinxSystem NoChannels NoChannels (Sample 13, Sample 13)
ddsWide = ddsWideDut
{-# OPAQUE ddsWide #-}
{-# ANN ddsWide (dutAnn "ddsWide") #-}

ddsWideTB :: Signal XilinxSystem Bool
ddsWideTB = romTB ddsWide $(memBlobTH Nothing ddsWideSamples)
{-# ANN ddsWideTB (TestBench 'ddsWide) #-}

------------------------------------------------------------------------------
-- Phase generator
------------------------------------------------------------------------------

phaseGen :: Dut XilinxSystem ((), Phase 48) (Phase 48, ()) (Phase 48)
phaseGen = phaseGenDut
{-# OPAQUE phaseGen #-}
{-# ANN phaseGen (dutAnn "phaseGen") #-}

phaseGenTB :: Signal XilinxSystem Bool
phaseGenTB = romTB phaseGen $(memBlobTH Nothing phaseGenSamples)
{-# ANN phaseGenTB (TestBench 'phaseGen) #-}

phaseGenFixed :: Dut XilinxSystem NoChannels NoChannels (Phase 24)
phaseGenFixed = phaseGenFixedDut
{-# OPAQUE phaseGenFixed #-}
{-# ANN phaseGenFixed (dutAnn "phaseGenFixed") #-}

phaseGenFixedTB :: Signal XilinxSystem Bool
phaseGenFixedTB = romTB phaseGenFixed $(memBlobTH Nothing phaseGenFixedSamples)
{-# ANN phaseGenFixedTB (TestBench 'phaseGenFixed) #-}

------------------------------------------------------------------------------
-- SIN/COS lookup table
------------------------------------------------------------------------------

lutWide :: Dut XilinxSystem NoChannels (Phase 16) (Sample 25, Sample 25)
lutWide = lutWideDut
{-# OPAQUE lutWide #-}
{-# ANN lutWide (dutAnn "lutWide") #-}

lutWideTB :: Signal XilinxSystem Bool
lutWideTB = romTB lutWide $(memBlobTH Nothing lutWideSamples)
{-# ANN lutWideTB (TestBench 'lutWide) #-}

lutSmall :: Dut XilinxSystem NoChannels (Phase 8) (Sample 7, Sample 7)
lutSmall = lutSmallDut
{-# OPAQUE lutSmall #-}
{-# ANN lutSmall (dutAnn "lutSmall") #-}

lutSmallTB :: Signal XilinxSystem Bool
lutSmallTB = romTB lutSmall $(memBlobTH Nothing lutSmallSamples)
{-# ANN lutSmallTB (TestBench 'lutSmall) #-}
