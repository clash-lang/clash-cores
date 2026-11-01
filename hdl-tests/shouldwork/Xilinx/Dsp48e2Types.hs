{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-- | Configurations, stimuli and test bench for "Dsp48e2". The stimuli are
-- random, generated from a fixed seed per test bench. The expected outputs are
-- computed with the Haskell simulation model; the test bench compares them to
-- the outputs of the UNISIM model.
module Dsp48e2Types where

import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam.Blob (MemBlob, memBlobTH)
import Clash.Prelude.ROM.Blob (asyncRomBlob)
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (BitVector(BV))

import Language.Haskell.TH (ExpQ, tupE)
import System.Random (StdGen, mkStdGen, randomR)

import qualified Clash.Signal.Delayed as D
import qualified Prelude as P

import Clash.Cores.Xilinx.Unisim.Dsp48e2
import Clash.Cores.Xilinx.Unisim.Dsp48e2.Internal
  (Inputs(..), Outputs, bundleOutputs, unbundleInputs)

-- | Number of cycles in each test bench
type Cycles = 1000

-- | During the first cycles, all resets are asserted, and the outputs are not
-- checked. This covers the global set/reset (GSR) pulse of the Verilog UNISIM
-- models (100 ns), which the VHDL models don't have.
warmup :: Int
warmup = 16

-- * Random generation

-- | Minimal state monad threading a random generator
newtype G a = G { runG :: StdGen -> (a, StdGen) }

instance Functor G where
  fmap f (G g) = G (\s -> let (a, s1) = g s in (f a, s1))

instance Applicative G where
  pure a = G (\s -> (a, s))
  G f <*> G a = G (\s -> let (f1, s1) = f s; (a1, s2) = a s1 in (f1 a1, s2))

instance Monad G where
  G a >>= f = G (\s -> let (a1, s1) = a s in runG (f a1) s1)

evalG :: G a -> Int -> a
evalG g seed = fst (runG g (mkStdGen seed))

uniformInteger :: Integer -> Integer -> G Integer
uniformInteger lo hi = G (randomR (lo, hi))

genBV :: forall n. KnownNat n => G (BitVector n)
genBV = fromInteger <$> uniformInteger 0 (2 P.^ natToNum @n @Integer - 1)

genBit :: G Bit
genBit = unpack <$> genBV

-- | 'True' with the given probability (in percent)
chance :: Integer -> G Bool
chance p = (< p) <$> uniformInteger 0 99

elements :: [a] -> G a
elements xs = (xs P.!!) . fromInteger <$> uniformInteger 0 (toInteger (P.length xs) - 1)

-- | How to generate the C input. The pattern detector rarely matches random
-- data, so the pattern detector tests use C values that make matches likely.
data CGen
  = CRandom
  -- ^ Uniformly random
  | CSmall
  -- ^ Mostly values below 8. When used as pattern with a rounding mask, only a
  -- few bits are compared.
  | CMostlyOnes
  -- ^ Mostly values with all but the three least significant bits set. When
  -- used as mask, only a few bits are compared.

genC :: CGen -> G (BitVector 48)
genC CRandom = genBV
genC CSmall = do
  small <- chance 80
  if small then zeroExtend <$> genBV @3 else genBV
genC CMostlyOnes = complement <$> genC CSmall

-- | Options for the generation of stimuli
data GenOpts = GenOpts
  { cGen :: CGen
  , maccExtend :: Bool
    -- ^ Use @MULTSIGNIN@, and often select the \"MACC extend\" mode of UG579:
    -- @OPMODE = 00_100_10_00@ and @CARRYINSEL = 010@. Otherwise, @OPMODE@ never
    -- selects @MULTSIGNIN@ (@OPMODE[6:4] = 100@ and @OPMODE[1:0] = 00@).
    --
    -- The VHDL UNISIM model adds @MULTSIGNIN@ to bit 0 of the X multiplexer,
    -- whereas the Verilog model (and the Haskell model) adds it to bit 1. So
    -- test benches using this option only work in Verilog.
  }

defGenOpts :: GenOpts
defGenOpts = GenOpts{cGen = CRandom, maccExtend = False}

-- | Generate OPMODE and CARRYINSEL (before inversion). Avoids the modes in
-- which the outputs of the model are undefined.
genCtrl :: Dsp48e2Config -> GenOpts -> G (BitVector 9, BitVector 3)
genCtrl cfg opts = do
  macc <- if maccExtend opts && pFdbk then chance 40 else pure False
  if macc then pure (0b00_100_10_00, 0b010) else do
    useM <- if multOk then chance 50 else pure False
    (x, y) <-
      if useM
      then pure (0b01, 0b01)
      else (,) <$> elements ([0b00] <> [0b10 | pFdbk] <> [0b11 | abOk]) <*> elements [0b00, 0b10, 0b11]
    z0 <- elements zs
    w <- elements ws
    let
      z | not (maccExtend opts) && x == 0b00 && z0 == 0b100 = 0b000
        | otherwise = z0
      op = w ++# z ++# (y :: BitVector 2) ++# (x :: BitVector 2)
    cis <- elements ciss
    let
      cisOk =
           cis /= 0b010
        || not multOk
        || (opModeReg cfg == carryInSelReg cfg && slice d3 d0 op /= 0b0101)
    pure (op, if cisOk then cis else 0)
 where
  multOk = useMult cfg /= MultNone && useSimd cfg == One48
  -- P feedback requires the P register
  pFdbk = pReg cfg == 1
  zs, ciss :: [BitVector 3]
  zs = [0b000, 0b001, 0b011, 0b101] <> (if pFdbk then [0b010, 0b100, 0b110, 0b111] else [])
  ws :: [BitVector 2]
  ws = [0b00, 0b10, 0b11] <> [0b01 | pFdbk]
  ciss = [0b000, 0b001, 0b010, 0b011, 0b110] <> (if pFdbk then [0b100, 0b101, 0b111] else [])
  abOk = not
    (  useMult cfg == MultMultiply
    && (  (aReg cfg == 0 && bReg cfg == 0 && mReg cfg == 0)
       || (aReg cfg == 0 && bReg cfg == 0 && pReg cfg == 0)
       || (mReg cfg == 0 && pReg cfg == 0) ) )

-- | Generate the values of the input pins for a single cycle
genInputs :: Dsp48e2Config -> GenOpts -> Bool -> G Inputs
genInputs cfg opts inWarmup = do
  iA <- genBV
  iAcIn <- genBV
  aluModeL <- genBV
  iB <- genBV
  iBcIn <- genBV
  iC <- genC (cGen opts)
  iCarryCascIn <- genBit
  carryInL <- genBit
  (opModeL, iCarryInSel) <- genCtrl cfg opts
  iCeA1 <- ce
  iCeA2 <- ce
  iCeAd <- ce
  iCeAluMode <- ce
  iCeB1 <- ce
  iCeB2 <- ce
  iCeC <- ce
  iCeCarryIn <- ce
  iCeCtrl <- ce
  iCeD <- ce
  iCeInMode <- ce
  iCeM <- ce
  iCeP <- ce
  iD <- genBV
  inModeL <- genBV
  iMultSignIn <- genBit
  iPcIn <- genBV
  rstA <- rst
  rstAllCarryIn <- rst
  rstAluMode <- rst
  rstB <- rst
  rstC <- rst
  rstCtrl <- rst
  rstD <- rst
  rstInMode <- rst
  rstM <- rst
  rstP <- rst
  pure Inputs
    { iAluMode = aluModeL `xor` isAluModeInverted cfg
    , iCarryIn = carryInL `xor` boolToBit (isCarryInInverted cfg)
    , iInMode = inModeL `xor` isInModeInverted cfg
    , iOpMode = opModeL `xor` isOpModeInverted cfg
    , iRstA = rstA /= isRstAInverted cfg
    , iRstAllCarryIn = rstAllCarryIn /= isRstAllCarryInInverted cfg
    , iRstAluMode = rstAluMode /= isRstAluModeInverted cfg
    , iRstB = rstB /= isRstBInverted cfg
    , iRstC = rstC /= isRstCInverted cfg
    , iRstCtrl = rstCtrl /= isRstCtrlInverted cfg
    , iRstD = rstD /= isRstDInverted cfg
    , iRstInMode = rstInMode /= isRstInModeInverted cfg
    , iRstM = rstM /= isRstMInverted cfg
    , iRstP = rstP /= isRstPInverted cfg
    , ..
    }
 where
  ce = chance 85
  rst = if inWarmup then pure True else chance 3

-- | Stimuli for a single @DSP48E2@
stimuli :: Dsp48e2Config -> GenOpts -> Int -> [Inputs]
stimuli cfg opts =
  evalG (mapM (\n -> genInputs cfg opts (n < warmup)) [0 .. natToNum @Cycles - 1])

-- * Circuits under test

type Dom = XilinxSystem

single :: Dsp48e2Config -> Clock Dom -> Signal Dom Inputs -> Signal Dom Outputs
single cfg clk i = bundleOutputs (dsp48e2 cfg clk (unbundleInputs i))

-- | Two @DSP48E2@s, the second using all cascade outputs of the first
cascaded ::
  Dsp48e2Config ->
  Dsp48e2Config ->
  Clock Dom ->
  Signal Dom (Inputs, Inputs) ->
  Signal Dom (Outputs, Outputs)
cascaded cfg1 cfg2 clk i = bundle (bundleOutputs out1, bundleOutputs out2)
 where
  (i1, i2) = unbundle i
  out1 = dsp48e2 cfg1 clk (unbundleInputs i1)
  out2 = dsp48e2 cfg2 clk (cascade out1 (unbundleInputs i2))

-- | 'multiplyAdd' and 'multiplyAccumulate', sharing their inputs
wrappers ::
  Clock Dom ->
  Signal Dom (Bool, Bool, Signed 27, Signed 18, Signed 48) ->
  Signal Dom (Signed 48, Signed 48)
wrappers clk i =
  bundle
    ( D.toSignal (multiplyAdd clk ena (D.fromSignal a) (D.fromSignal b) (D.fromSignal c))
    , D.toSignal (multiplyAccumulate clk ena (D.fromSignal start) (D.fromSignal a) (D.fromSignal b))
    )
 where
  (en, start, a, b, c) = unbundle i
  ena = toEnable en

-- | Stimuli for 'wrappers'. These functions don't have resets, so the warmup
-- makes sure the state of the multiply-accumulate does not depend on the
-- global set/reset of the Verilog UNISIM model.
wrapperStimuli :: Int -> [(Bool, Bool, Signed 27, Signed 18, Signed 48)]
wrapperStimuli = evalG (mapM gen [0 .. natToNum @Cycles - 1])
 where
  gen n = do
    en <- if n < warmup then pure True else chance 85
    start <- if n < warmup then pure True else chance 10
    a <- unpack <$> genBV
    b <- unpack <$> genBV
    c <- unpack <$> genBV
    pure (en, start, a, b, c)

-- | 'blobs' for 'wrappers'
wrapperBlobs :: Int -> ExpQ
wrapperBlobs seed = blobs wrappers (wrapperStimuli seed)

-- * Expected outputs

-- | Simulate a circuit with the Haskell model, and return the expected values
-- along with a mask of the bits to check. Undefined bits, and all bits during
-- the warmup, are not checked.
expected ::
  (BitPack i, BitPack o, NFDataX i) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  [i] ->
  [(BitVector (BitSize o), BitVector (BitSize o))]
expected circuit is = P.zipWith check [0 ..] outs
 where
  outs = sampleN_lazy (P.length is) (circuit clockGen (fromList is))
  check n o
    | n < warmup = (0, 0)
    | otherwise = case isX (pack o) of
        Left _ -> (0, 0)
        Right (BV m v) ->
          let care = complement (fromIntegral m) in (fromIntegral v .&. care, care)

expectedValues ::
  (BitPack i, BitPack o, NFDataX i) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  [i] ->
  [BitVector (BitSize o)]
expectedValues circuit = P.map fst . expected circuit

expectedMasks ::
  (BitPack i, BitPack o, NFDataX i) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  [i] ->
  [BitVector (BitSize o)]
expectedMasks circuit = P.map snd . expected circuit

-- | Memory blobs with the stimuli, expected values, and masks of the bits to
-- check, for 'tb'
blobs ::
  (BitPack i, BitPack o, NFDataX i) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  [i] ->
  ExpQ
blobs circuit is =
  tupE
    [ memBlobTH Nothing is
    , memBlobTH Nothing (expectedValues circuit is)
    , memBlobTH Nothing (expectedMasks circuit is)
    ]

-- | 'blobs' for a single @DSP48E2@
singleBlobs :: Dsp48e2Config -> GenOpts -> Int -> ExpQ
singleBlobs cfg opts seed = blobs (single cfg) (stimuli cfg opts seed)

-- | 'blobs' for two cascaded @DSP48E2@s
cascadedBlobs :: Dsp48e2Config -> Dsp48e2Config -> GenOpts -> Int -> ExpQ
cascadedBlobs cfg1 cfg2 opts seed =
  blobs (cascaded cfg1 cfg2)
    (P.zip (stimuli cfg1 opts seed) (stimuli cfg2 opts (seed + 1)))

-- * Test bench

tb ::
  forall i o.
  (BitPack i, BitPack o) =>
  (Clock Dom -> Signal Dom i -> Signal Dom o) ->
  ( MemBlob Cycles (BitSize i)
  , MemBlob Cycles (BitSize o)
  , MemBlob Cycles (BitSize o) ) ->
  Signal Dom Bool
tb circuit (stimBlob, expectedBlob, maskBlob) = done
 where
  clk = tbClockGen (not <$> done)

  cnt :: Signal Dom (Index Cycles)
  cnt = register clk noReset enableGen 0 (satSucc SatBound <$> cnt)

  stim = unpack . asyncRomBlob stimBlob <$> cnt
  actual = pack <$> circuit clk stim
  expectedVal = asyncRomBlob expectedBlob <$> cnt
  mask = asyncRomBlob maskBlob <$> cnt

  done =
    assert clk noReset "DSP48E2 outputs"
      ((.&.) <$> actual <*> mask)
      ((.&.) <$> expectedVal <*> mask)
      ((== maxBound) <$> cnt)
{-# INLINE tb #-}

-- * Configurations

-- | No registers at all
combinational :: Dsp48e2Config
combinational = defDsp48e2Config
  { aReg = 0, aCascReg = 0, bReg = 0, bCascReg = 0, cReg = 0, dReg = 0
  , adReg = 0, mReg = 0, pReg = 0, inModeReg = 0, opModeReg = 0
  , aluModeReg = 0, carryInReg = 0, carryInSelReg = 0
  , useMult = MultDynamic
  }

-- | Only the output register
pRegOnly :: Dsp48e2Config
pRegOnly = combinational{pReg = 1}

-- | All registers, two A and B registers, inverted control pins, pre-adder
-- feeding the 27-bit multiplier input
pipelined :: Dsp48e2Config
pipelined = defDsp48e2Config
  { aReg = 2, aCascReg = 1, bReg = 2, bCascReg = 2
  , aMultSel = AMultAD
  , rnd = 0x0000_0000_8000
  , isAluModeInverted = 0b1010
  , isCarryInInverted = True
  , isInModeInverted = 0b10101
  , isOpModeInverted = 0b1_0010_0100
  , isRstAllCarryInInverted = True
  , isRstAluModeInverted = True
  , isRstAInverted = True
  , isRstBInverted = True
  , isRstCtrlInverted = True
  , isRstCInverted = True
  , isRstDInverted = True
  , isRstInModeInverted = True
  , isRstMInverted = True
  , isRstPInverted = True
  }

-- | Pre-adder with B, feeding the 18-bit multiplier input; A and B from the
-- (random) cascade inputs; a mix of registers
preAddB :: Dsp48e2Config
preAddB = defDsp48e2Config
  { aInput = Cascade, bInput = Cascade
  , aReg = 2, aCascReg = 2, bReg = 1, bCascReg = 1
  , bMultSel = BMultAD, preAddInSel = PreAddInB
  , dReg = 0, adReg = 1, mReg = 0, inModeReg = 0, cReg = 0
  , opModeReg = 0, carryInSelReg = 0, aluModeReg = 0
  , useMult = MultDynamic
  }

-- | Four 12-bit ALUs, 12-bit wide XOR
simdFour12 :: Dsp48e2Config
simdFour12 = defDsp48e2Config
  { useMult = MultNone, mReg = 0, useSimd = Four12
  , useWideXor = True, xorSimd = Xor12
  , cReg = 0, carryInReg = 0
  , rnd = 0x8000_0000_0001
  }

-- | Two 24-bit ALUs, 24/48/96-bit wide XOR, no output register
simdTwo24 :: Dsp48e2Config
simdTwo24 = defDsp48e2Config
  { useMult = MultNone, mReg = 0, useSimd = Two24
  , useWideXor = True, xorSimd = Xor24_48_96
  , pReg = 0, aReg = 0, aCascReg = 0, aluModeReg = 0
  }

-- | Pattern detector with attribute pattern and mask, automatic reset on
-- match (only when CEP is asserted)
patDetAttr :: Dsp48e2Config
patDetAttr = defDsp48e2Config
  { usePatternDetect = True
  , patDetPattern = 0x0000_0000_0005
  , patDetMask = 0xFFFF_FFFF_FFF0
  , autoResetPatDet = AutoResetMatch
  , autoResetPriority = AutoResetPriorityCep
  }

-- | Pattern detector with C as pattern and a rounding mask, automatic reset
-- on a match followed by a mismatch
patDetRounding :: Dsp48e2Config
patDetRounding = defDsp48e2Config
  { usePatternDetect = True
  , selPattern = PatternC
  , selMask = MaskRoundingMode1
  , autoResetPatDet = AutoResetNotMatch
  , useMult = MultDynamic
  }

-- | Pattern detector with C as mask, rounding mode 2, no output register
patDetMaskC :: Dsp48e2Config
patDetMaskC = defDsp48e2Config
  { usePatternDetect = True
  , selMask = MaskC
  , patDetPattern = 0x0000_0000_0003
  , pReg = 0
  }

patDetRounding2 :: Dsp48e2Config
patDetRounding2 = patDetRounding{selMask = MaskRoundingMode2, cReg = 0}

-- | Second @DSP48E2@ of a cascade: A and B from the first one
cascadeSecond :: Dsp48e2Config -> Dsp48e2Config
cascadeSecond cfg = cfg{aInput = Cascade, bInput = Cascade}
