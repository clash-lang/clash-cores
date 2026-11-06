{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Configurations and test samples for the HDL tests of "Clash.Cores.Xilinx.Dds".
The expected outputs are computed by simulating the functions of
"Clash.Cores.Xilinx.Dds.Explicit", which use the simulation model.
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Dds.TH where

import Prelude

import Data.Bits (shiftL, shiftR, xor)
import Data.List (unfoldr)
import Data.Word (Word64)

import Clash.Explicit.Prelude
  ( BitPack, Clock, Enable, KnownDomain, KnownNat, NFDataX, Signal, System, Unsigned
  , bitCoerce, clockGen, fromList, hasUndefined, sampleN, toEnable, unpack )
import qualified Clash.Explicit.Prelude as C
import qualified Clash.Signal.Delayed as D

import Clash.Annotations.TopEntity (PortName (..), TopEntity (..))
import Clash.Cores.Xilinx.Dds.Explicit

-- | Annotation for a design under test
dutAnn :: String -> TopEntity
dutAnn name =
  Synthesize
    { t_name = name
    , t_inputs = [PortName "clk", PortName "en", PortName "x"]
    , t_output = PortName "result"
    }

------------------------------------------------------------------------------
-- Configurations under test
------------------------------------------------------------------------------

-- | Negated sine, block RAM
negSineConfig :: Config
negSineConfig = defConfig{negativeSine = True, memoryType = BlockRom}

-- | Unit circle amplitude, distributed memory, negated cosine
unitCircleConfig :: Config
unitCircleConfig =
  defConfig{amplitudeMode = UnitCircle, memoryType = DistributedRom, negativeCosine = True}

-- | First order Taylor series correction, negated sine
taylor1Config :: Config
taylor1Config = defConfig{noiseShaping = TaylorSeriesCorrected, negativeSine = True}

-- | Second order Taylor series correction, negated cosine, unit circle
taylor2Config :: Config
taylor2Config =
  defConfig{noiseShaping = TaylorSeriesCorrected, negativeCosine = True, amplitudeMode = UnitCircle}

-- | Taylor series correction in distributed memory
taylorDistConfig :: Config
taylorDistConfig = defConfig{noiseShaping = TaylorSeriesCorrected, memoryType = DistributedRom}

------------------------------------------------------------------------------
-- Inputs
------------------------------------------------------------------------------

-- | Pseudo random numbers (xorshift64)
randoms :: Word64 -> [Integer]
randoms = unfoldr (\s -> let s' = step s in Just (toInteger s', s'))
 where
  step x0 =
    let x1 = x0 `xor` (x0 `shiftL` 13)
        x2 = x1 `xor` (x1 `shiftR` 7)
    in x2 `xor` (x2 `shiftL` 17)

-- | Pseudo random integers in @[lo, hi]@
randomRange :: Word64 -> Integer -> Integer -> [Integer]
randomRange seed lo hi = [ lo + r `mod` (hi - lo + 1) | r <- randoms seed ]

-- | Random phases
randomPhases :: forall pw. KnownNat pw => Word64 -> [Phase pw]
randomPhases seed = map toPhase (randomRange seed 0 ((1 `shiftL` C.natToNum @pw) - 1))

-- | A phase from its raw bits
toPhase :: forall pw. KnownNat pw => Integer -> Phase pw
toPhase = bitCoerce . (fromInteger :: Integer -> Unsigned pw)

-- | Random booleans, true with probability @1 / k@
randomFlags :: Word64 -> Integer -> [Bool]
randomFlags seed k = map (== 0) (randomRange seed 0 (k - 1))

-- | Enable pattern: disabled for the first cycle (the test bench plays its
-- first sample twice), then mostly enabled with some stretches of disabled
-- cycles.
enables :: Word64 -> Int -> [Bool]
enables seed n = False : take (n - 1) (concat (zipWith run (randomRange seed 1 30) (cycle [True, False])))
 where
  run len b = replicate (fromInteger (if b then len else len `div` 6 + 1)) b

-- | Enable that is always on, except for the first cycle (see 'enables')
alwaysEnabled :: Int -> [Bool]
alwaysEnabled n = False : replicate (n - 1) True

-- | Number of samples per test
nSamples :: Int
nSamples = 600

------------------------------------------------------------------------------
-- Simulation
------------------------------------------------------------------------------

-- | Simulate a design on a list of enables and inputs, returning the test
-- samples: the inputs, and the expected outputs with a flag that says whether
-- they are defined.
simulate ::
  forall i o.
  (NFDataX i, NFDataX o, BitPack o) =>
  (Clock System -> Enable System -> Signal System i -> Signal System o) ->
  [(Bool, i)] ->
  [((Bool, i), (Bool, o))]
simulate dut ins = zip ins (map check outs)
 where
  n = length ins
  (ens, xs) = unbundleList ins
  outs =
    sampleN n $
      dut clockGen (toEnable (fromList (ens <> repeat True))) (fromList (xs <> repeat (last xs)))
  check o
    | hasUndefined o = (False, unpack 0)
    | otherwise = (True, o)
  unbundleList l = (map fst l, map snd l)

-- | Fill in the CONFIG channel and input PHASE channel
withChannels :: [Bool] -> [c] -> [p] -> [(Bool, (c, p))]
withChannels ens cs ps = zip ens (zip cs ps)

------------------------------------------------------------------------------
-- Designs under test
------------------------------------------------------------------------------

-- | A design under test: CONFIG channel and input PHASE channel in, outputs
-- out. The CONFIG channel is a valid flag and data, as the test bench's ROM
-- cannot hold the undefined bits of a 'Nothing'.
type Dut dom c p o = Clock dom -> Enable dom -> Signal dom ((Bool, c), p) -> Signal dom o

-- | Adapt a function from "Clash.Cores.Xilinx.Dds.Explicit"
dut ::
  (Clock dom -> Enable dom -> Signal dom (Maybe c) -> D.DSignal dom 0 p -> D.DSignal dom d o) ->
  Dut dom c p o
dut f clk en inp = D.toSignal (f clk en (toMaybe . fst <$> inp) (D.fromSignal (snd <$> inp)))
 where
  toMaybe (v, c) = if v then Just c else Nothing

-- | No CONFIG channel and no input PHASE channel
type NoChannels = ((), ())

-- | Fixed increment and offset, 16 bit phase, 12 bit outputs
ddsFixedDut :: KnownDomain dom => Dut dom NoChannels NoChannels (Sample 11, Sample 11)
ddsFixedDut =
  dut $ ddsWith @7 defConfig (FixedIncrement (0.0123 :: Phase 16)) (FixedOffset 0.3) SineCosineOutput

-- | Streaming increment with resynchronization, streaming offset, 12 bit
-- phase, 10 bit outputs
ddsStreamingDut :: KnownDomain dom => Dut dom NoChannels ((Phase 12, Bool), Phase 12) (Sample 9, Sample 9)
ddsStreamingDut = dut $ ddsWith @5 defConfig StreamingIncrementResync StreamingOffset SineCosineOutput

-- | Programmable increment and offset, 20 bit phase, 14 bit negated sine in
-- block RAM
ddsProgrammableDut :: KnownDomain dom => Dut dom (Phase 20, Phase 20) NoChannels (Sample 13)
ddsProgrammableDut =
  dut $ ddsWith @4 negSineConfig (ProgrammableIncrement 0.001) (ProgrammableOffset (-0.25)) SineOutput

-- | Programmable increment and streaming offset, 32 bit phase, 16 bit negated
-- cosine on the unit circle in distributed memory, at the minimum latency
ddsMixedDut :: KnownDomain dom => Dut dom (Phase 32, ()) ((), Phase 32) (Sample 15)
ddsMixedDut =
  dut $ ddsWith @1 unitCircleConfig (ProgrammableIncrement 0.1234567) StreamingOffset CosineOutput

-- | Output wider than the phase (no half step offset), default configuration
ddsWideDut :: KnownDomain dom => Dut dom NoChannels NoChannels (Sample 13, Sample 13)
ddsWideDut = dut $ \clk en _ _ -> dds @3 clk en (-0.37 :: Phase 10)

-- | Phase generator with a streaming increment and programmable offset, 48 bit
-- phase
phaseGenDut :: KnownDomain dom => Dut dom ((), Phase 48) (Phase 48, ()) (Phase 48)
phaseGenDut = dut $ phaseGeneratorWith @3 StreamingIncrement (ProgrammableOffset 0.5)

-- | Taylor series correction, first order: streaming increment, fixed offset,
-- 32 bit phase, 18 bit outputs
ddsTaylor1Dut :: KnownDomain dom => Dut dom NoChannels (Phase 32, ()) (Sample 17, Sample 17)
ddsTaylor1Dut =
  dut $ ddsWith @8 taylor1Config StreamingIncrement (FixedOffset 0.123) SineCosineOutput

-- | Taylor series correction, second order: programmable increment, 40 bit
-- phase, 26 bit outputs, at the minimum latency
ddsTaylor2Dut :: KnownDomain dom => Dut dom (Phase 40, ()) NoChannels (Sample 25, Sample 25)
ddsTaylor2Dut =
  dut $ ddsWith @3 taylor2Config (ProgrammableIncrement 0.0101) NoOffset SineCosineOutput

-- | Phase generator with a fixed increment, 24 bit phase, at the minimum
-- latency
phaseGenFixedDut :: KnownDomain dom => Dut dom NoChannels NoChannels (Phase 24)
phaseGenFixedDut = dut $ \clk en _ _ -> phaseGenerator @1 clk en (0.1234567 :: Phase 24)

-- | SIN/COS lookup table, 16 bit phase, 26 bit outputs
lutWideDut :: KnownDomain dom => Dut dom NoChannels (Phase 16) (Sample 25, Sample 25)
lutWideDut = dut $ \clk en _ -> sinCos @6 clk en

-- | SIN/COS lookup table, 8 bit phase, 8 bit outputs on the unit circle, at
-- the minimum latency
lutSmallDut :: KnownDomain dom => Dut dom NoChannels (Phase 8) (Sample 7, Sample 7)
lutSmallDut = dut $ \clk en _ -> sinCosLutWith @1 unitCircleConfig SineCosineOutput clk en

-- | SIN/COS lookup table with first order Taylor series correction, 12 bit
-- phase, 12 bit outputs (narrower than the multiplier operands)
lutTaylor1Dut :: KnownDomain dom => Dut dom NoChannels (Phase 12) (Sample 11, Sample 11)
lutTaylor1Dut =
  dut $ \clk en _ -> sinCosLutWith @7 defConfig{noiseShaping = TaylorSeriesCorrected} SineCosineOutput clk en

-- | SIN/COS lookup table with second order Taylor series correction in
-- distributed memory, 16 bit phase, 22 bit outputs, at the minimum latency
lutTaylor2Dut :: KnownDomain dom => Dut dom NoChannels (Phase 16) (Sample 21, Sample 21)
lutTaylor2Dut = dut $ \clk en _ -> sinCosLutWith @1 taylorDistConfig SineCosineOutput clk en

------------------------------------------------------------------------------
-- Samples
------------------------------------------------------------------------------

-- | Test samples of a design under test
type Samples c p o = [((Bool, ((Bool, c), p)), (Bool, o))]

noConfig :: [(Bool, NoChannels)]
noConfig = repeat (False, ((), ()))

noPhase :: [NoChannels]
noPhase = repeat ((), ())

-- | Occasional random CONFIG channel transfers
configs :: Word64 -> [a] -> [(Bool, a)]
configs seed = zip (randomFlags seed 12)

ddsFixedSamples :: Samples NoChannels NoChannels (Sample 11, Sample 11)
ddsFixedSamples = simulate ddsFixedDut (withChannels (enables 1 nSamples) noConfig noPhase)

ddsStreamingSamples :: Samples NoChannels ((Phase 12, Bool), Phase 12) (Sample 9, Sample 9)
ddsStreamingSamples =
  simulate ddsStreamingDut $
    withChannels (enables 2 nSamples) noConfig
      (zip (zip (randomPhases 3) (randomFlags 4 10)) (randomPhases 5))

ddsProgrammableSamples :: Samples (Phase 20, Phase 20) NoChannels (Sample 13)
ddsProgrammableSamples =
  simulate ddsProgrammableDut $
    withChannels (enables 6 nSamples) (configs 7 (zip (randomPhases 8) (randomPhases 9))) noPhase

ddsMixedSamples :: Samples (Phase 32, ()) ((), Phase 32) (Sample 15)
ddsMixedSamples =
  simulate ddsMixedDut $
    withChannels (enables 10 nSamples) (configs 11 (zip (randomPhases 12) (repeat ())))
      (zip (repeat ()) (randomPhases 13))

ddsWideSamples :: Samples NoChannels NoChannels (Sample 13, Sample 13)
ddsWideSamples = simulate ddsWideDut (withChannels (alwaysEnabled nSamples) noConfig noPhase)

phaseGenSamples :: Samples ((), Phase 48) (Phase 48, ()) (Phase 48)
phaseGenSamples =
  simulate phaseGenDut $
    withChannels (enables 14 nSamples) (configs 15 (zip (repeat ()) (randomPhases 16)))
      (zip (randomPhases 17) (repeat ()))

phaseGenFixedSamples :: Samples NoChannels NoChannels (Phase 24)
phaseGenFixedSamples = simulate phaseGenFixedDut (withChannels (enables 21 nSamples) noConfig noPhase)

-- | Includes the phases of the table entry that rounds differently from the
-- formula, see 'Clash.Cores.Xilinx.Dds.Internal.tableQuirk'
lutWideSamples :: Samples NoChannels (Phase 16) (Sample 25, Sample 25)
lutWideSamples =
  simulate lutWideDut $
    withChannels (alwaysEnabled nSamples) noConfig
      (map toPhase ([0 .. 16] <> [ q * 16384 + s * 2607 | q <- [0 .. 3], s <- [1, -1] ] <> [65535])
        <> randomPhases 18)

lutSmallSamples :: Samples NoChannels (Phase 8) (Sample 7, Sample 7)
lutSmallSamples =
  simulate lutSmallDut $
    withChannels (enables 19 nSamples) noConfig (map toPhase [0 .. 255] <> randomPhases 20)

ddsTaylor1Samples :: Samples NoChannels (Phase 32, ()) (Sample 17, Sample 17)
ddsTaylor1Samples =
  simulate ddsTaylor1Dut $
    withChannels (enables 22 nSamples) noConfig (zip (randomPhases 23) (repeat ()))

ddsTaylor2Samples :: Samples (Phase 40, ()) NoChannels (Sample 25, Sample 25)
ddsTaylor2Samples =
  simulate ddsTaylor2Dut $
    withChannels (enables 24 nSamples) (configs 25 (zip (randomPhases 26) (repeat ()))) noPhase

lutTaylor1Samples :: Samples NoChannels (Phase 12) (Sample 11, Sample 11)
lutTaylor1Samples =
  simulate lutTaylor1Dut $
    withChannels (alwaysEnabled nSamples) noConfig (map toPhase [0 .. 63] <> randomPhases 27)

lutTaylor2Samples :: Samples NoChannels (Phase 16) (Sample 21, Sample 21)
lutTaylor2Samples =
  simulate lutTaylor2Dut $
    withChannels (enables 28 nSamples) noConfig (map toPhase [16300 .. 16447] <> randomPhases 29)
