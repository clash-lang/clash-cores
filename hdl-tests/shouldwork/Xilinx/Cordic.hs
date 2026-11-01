{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tests of "Clash.Cores.Xilinx.Cordic": the test benches instantiate the Xilinx
CORDIC IP and compare its outputs, cycle by cycle, with the outputs of the
simulation model (computed in "Cordic.TH"). Each test uses a different
function and configuration of the IP.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module Cordic where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Clash.Explicit.Testbench

import qualified Clash.Cores.Xilinx.Cordic as C

import Cordic.TH

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

-- | Test bench feeding inputs from a ROM to a design and comparing its outputs
-- with the expected outputs in the ROM. The first @d@ outputs are not
-- checked.
romTB ::
  forall d n a b.
  ( KnownNat d
  , KnownNat n
  , BitPack a
  , BitPack b
  , Eq b
  , ShowX b
  , NFDataX b
  , 1 <= n
  ) =>
  (Clock XilinxSystem -> DSignal XilinxSystem 0 a -> DSignal XilinxSystem d b) ->
  MemBlob n (BitSize (a, b)) ->
  Signal XilinxSystem Bool
romTB comp blob = done
 where
  (done0, samples) = playSampleRom clk rst blob
  (input, expected) = unbundle samples
  -- Only assert while not finished
  done = mux done0 done0 $ assert clk rst "romTB" out expected done0
  out =
      ignoreFor clk rst enableGen (SNat @d) (unpack 0)
    . toSignal $ comp clk (fromSignal input)
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# INLINE romTB #-}

------------------------------------------------------------------------------
-- Sin and cos, default configuration, 16 bits
------------------------------------------------------------------------------

sinCosBasic ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 (SFixed 3 13) ->
  DSignal XilinxSystem (C.SinCosDefDelay 14) (SFixed 2 14, SFixed 2 14)
sinCosBasic clk x = withClock clk $ withEnable enableGen $ C.sinCos x
{-# OPAQUE sinCosBasic #-}
{-# ANN sinCosBasic (unaryTopAnn "sinCosBasic") #-}

sinCosBasicTB :: Signal XilinxSystem Bool
sinCosBasicTB = romTB sinCosBasic $(memBlobTH Nothing sinCosSamples)
{-# ANN sinCosBasicTB (TestBench 'sinCosBasic) #-}

------------------------------------------------------------------------------
-- Sin and cos with a stalling pipeline
------------------------------------------------------------------------------

sinCosEnable ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (SFixed 3 13) ->
  DSignal XilinxSystem (C.SinCosDefDelay 14) (SFixed 2 14, SFixed 2 14)
sinCosEnable clk en x = withClock clk $ withEnable en $ C.sinCos x
{-# OPAQUE sinCosEnable #-}
{-# ANN sinCosEnable (Synthesize
  { t_name = "sinCosEnable"
  , t_inputs = [PortName "clk", PortName "en", PortName "x"]
  , t_output = PortName "result"
  }) #-}

sinCosEnableTB :: Signal XilinxSystem Bool
sinCosEnableTB = done
 where
  (done0, samples) = playSampleRom clk rst $(memBlobTH Nothing sinCosEnableSamples)
  (inputs, expected) = unbundle samples
  (en, x) = unbundle inputs
  (valid, expectedOut) = unbundle expected
  -- Outputs are undefined until the pipeline has filled
  out = mux valid (toSignal (sinCosEnable clk (toEnable en) (fromSignal x))) (pure (0, 0))
  done = mux done0 done0 $ assert clk rst "sinCosEnableTB" out expectedOut done0
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# ANN sinCosEnableTB (TestBench 'sinCosEnable) #-}

------------------------------------------------------------------------------
-- Rotate, default configuration, 16 bits
------------------------------------------------------------------------------

rotateBasic ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 ((SFixed 2 14, SFixed 2 14), SFixed 3 13) ->
  DSignal XilinxSystem (C.RotateDefDelay 13) (SFixed 2 14, SFixed 2 14)
rotateBasic clk inp =
  withClock clk $ withEnable enableGen $ C.rotate (fst <$> inp) (snd <$> inp)
{-# OPAQUE rotateBasic #-}
{-# ANN rotateBasic (unaryTopAnn "rotateBasic") #-}

rotateBasicTB :: Signal XilinxSystem Bool
rotateBasicTB = romTB rotateBasic $(memBlobTH Nothing rotateBasicSamples)
{-# ANN rotateBasicTB (TestBench 'rotateBasic) #-}

------------------------------------------------------------------------------
-- Rotate with compensation scaling, 16 bit inputs, 12 bit outputs
------------------------------------------------------------------------------

rotateScaled ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 ((SFixed 2 14, SFixed 2 14), SFixed 3 13) ->
  DSignal XilinxSystem 19 (SFixed 2 10, SFixed 2 10)
rotateScaled clk inp =
  withClock clk $ withEnable enableGen $ C.rotateWith rotateConfig (fst <$> inp) (snd <$> inp)
{-# OPAQUE rotateScaled #-}
{-# ANN rotateScaled (unaryTopAnn "rotateScaled") #-}

rotateScaledTB :: Signal XilinxSystem Bool
rotateScaledTB = romTB rotateScaled $(memBlobTH Nothing rotateSamples)
{-# ANN rotateScaledTB (TestBench 'rotateScaled) #-}

------------------------------------------------------------------------------
-- Translate in scaled radians, 12 bit inputs, 20 bit outputs
------------------------------------------------------------------------------

translateScaled ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 (SFixed 2 10, SFixed 2 10) ->
  DSignal XilinxSystem 29 (SFixed 2 18, SFixed 3 17)
translateScaled clk x =
  withClock clk $ withEnable enableGen $ C.translateWith translateConfig x
{-# OPAQUE translateScaled #-}
{-# ANN translateScaled (unaryTopAnn "translateScaled") #-}

translateScaledTB :: Signal XilinxSystem Bool
translateScaledTB = romTB translateScaled $(memBlobTH Nothing translateSamples)
{-# ANN translateScaledTB (TestBench 'translateScaled) #-}

------------------------------------------------------------------------------
-- Arctan without coarse rotation, 20 bit inputs, 16 bit outputs
------------------------------------------------------------------------------

arcTanFirstQuadrant ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 (SFixed 2 18, SFixed 2 18) ->
  DSignal XilinxSystem 18 (SFixed 3 13)
arcTanFirstQuadrant clk x =
  withClock clk $ withEnable enableGen $ C.arcTanWith arcTanConfig x
{-# OPAQUE arcTanFirstQuadrant #-}
{-# ANN arcTanFirstQuadrant (unaryTopAnn "arcTanFirstQuadrant") #-}

arcTanFirstQuadrantTB :: Signal XilinxSystem Bool
arcTanFirstQuadrantTB = romTB arcTanFirstQuadrant $(memBlobTH Nothing arcTanSamples)
{-# ANN arcTanFirstQuadrantTB (TestBench 'arcTanFirstQuadrant) #-}

------------------------------------------------------------------------------
-- Sinh and cosh, default configuration, 18 bit inputs, 24 bit outputs
------------------------------------------------------------------------------

sinhCoshBasic ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 (SFixed 3 15) ->
  DSignal XilinxSystem (C.SinhCoshDefDelay 22) (SFixed 2 22, SFixed 2 22)
sinhCoshBasic clk x = withClock clk $ withEnable enableGen $ C.sinhCosh x
{-# OPAQUE sinhCoshBasic #-}
{-# ANN sinhCoshBasic (unaryTopAnn "sinhCoshBasic") #-}

sinhCoshBasicTB :: Signal XilinxSystem Bool
sinhCoshBasicTB = romTB sinhCoshBasic $(memBlobTH Nothing sinhCoshSamples)
{-# ANN sinhCoshBasicTB (TestBench 'sinhCoshBasic) #-}

------------------------------------------------------------------------------
-- Arctanh with explicit iterations and precision, 16 bits
------------------------------------------------------------------------------

arcTanhExplicit ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 (SFixed 2 14, SFixed 2 14) ->
  DSignal XilinxSystem 22 (SFixed 3 13)
arcTanhExplicit clk x =
  withClock clk $ withEnable enableGen $ C.arcTanhWith arcTanhConfig x
{-# OPAQUE arcTanhExplicit #-}
{-# ANN arcTanhExplicit (unaryTopAnn "arcTanhExplicit") #-}

arcTanhExplicitTB :: Signal XilinxSystem Bool
arcTanhExplicitTB = romTB arcTanhExplicit $(memBlobTH Nothing arcTanhSamples)
{-# ANN arcTanhExplicitTB (TestBench 'arcTanhExplicit) #-}

------------------------------------------------------------------------------
-- Square roots
------------------------------------------------------------------------------

sqrtNearestEven ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 (UFixed 1 15) ->
  DSignal XilinxSystem 16 (UFixed 1 13)
sqrtNearestEven clk x = withClock clk $ withEnable enableGen $ C.sqrtWith sqrtConfig x
{-# OPAQUE sqrtNearestEven #-}
{-# ANN sqrtNearestEven (unaryTopAnn "sqrtNearestEven") #-}

sqrtNearestEvenTB :: Signal XilinxSystem Bool
sqrtNearestEvenTB = romTB sqrtNearestEven $(memBlobTH Nothing sqrtSamples)
{-# ANN sqrtNearestEvenTB (TestBench 'sqrtNearestEven) #-}

sqrtUnsignedBasic ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 (Unsigned 17) ->
  DSignal XilinxSystem (C.SqrtUnsignedDefDelay 17) (Unsigned 9)
sqrtUnsignedBasic clk x = withClock clk $ withEnable enableGen $ C.sqrtUnsigned x
{-# OPAQUE sqrtUnsignedBasic #-}
{-# ANN sqrtUnsignedBasic (unaryTopAnn "sqrtUnsignedBasic") #-}

sqrtUnsignedBasicTB :: Signal XilinxSystem Bool
sqrtUnsignedBasicTB = romTB sqrtUnsignedBasic $(memBlobTH Nothing sqrtUnsignedSamples)
{-# ANN sqrtUnsignedBasicTB (TestBench 'sqrtUnsignedBasic) #-}
