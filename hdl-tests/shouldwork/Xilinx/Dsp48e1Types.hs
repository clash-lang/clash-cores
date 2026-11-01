{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- | Configurations and stimuli for "Dsp48e1". See "Dsp48TestCommon" for the
-- test bench.
--
-- Unlike the @DSP48E2@ tests, the stimuli deliberately include invalid
-- combinations of @ALUMODE@, @OPMODE@ and @CARRYINSEL@, and @CARRYINSEL = 010@
-- while multiplying, as the UNISIM model drives X in specific ways in those
-- cases. The only combinations avoided are those using the P feedback without
-- the output register, which are combinational loops in the UNISIM models.
module Dsp48e1Types where

import Clash.Explicit.Prelude

import Language.Haskell.TH (ExpQ)

import qualified Clash.Signal.Delayed as D
import qualified Prelude as P

import Clash.Cores.Xilinx.Unisim.Dsp48e1
import Clash.Cores.Xilinx.Unisim.Dsp48e1.Internal
  (Inputs(..), Outputs, arithmeticModes, bundleOutputs, logicModes, unbundleInputs)

import Dsp48TestCommon

-- | Options for the generation of stimuli
data GenOpts = GenOpts
  { cGen :: CGen
  , maccExtend :: Bool
    -- ^ Often select the \"MACC extend\" mode of UG479: @OPMODE = 100_10_00@
    -- and @CARRYINSEL = 010@
  , invalidModes :: Bool
    -- ^ Also generate combinations of ALUMODE, OPMODE and CARRYINSEL that the
    -- UNISIM model considers invalid, and other stimuli that make outputs
    -- undefined. Once the automatic reset condition is undefined, the output
    -- register is undefined until the next reset, so the test benches using
    -- the automatic reset disable this.
  }

defGenOpts :: GenOpts
defGenOpts = GenOpts{cGen = CRandom, maccExtend = False, invalidModes = True}

-- | Whether a combination of OPMODE and CARRYINSEL uses the P feedback, which
-- is a combinational loop in the UNISIM models without output register
usesPFeedback :: BitVector 7 -> BitVector 3 -> Bool
usesPFeedback op cis =
     slice d1 d0 op == 0b10
  || slice d6 d4 op `P.elem` [0b010, 0b100, 0b110, 0b111]
  || cis `P.elem` [0b100, 0b101, 0b111]

-- | Generate ALUMODE, OPMODE and CARRYINSEL (before inversion). Mostly picks
-- combinations the UNISIM model considers valid, but also random ones.
genCtrl :: Dsp48e1Config -> GenOpts -> G (BitVector 4, BitVector 7, BitVector 3)
genCtrl cfg opts = do
  macc <- if maccExtend opts && pFdbk then chance 40 else pure False
  if macc then (,,) <$> (zeroExtend <$> genBV @2) <*> pure 0b100_10_00 <*> pure 0b010 else do
    kind <- uniformInteger 0 (if invalidModes opts then 99 else 84)
    if
      | kind < 45 -> do
          -- Arithmetic
          alu <- zeroExtend <$> genBV @2
          fromTable <- if invalidModes opts then chance 70 else pure True
          (op, cis) <-
            if fromTable
            then (\k -> (slice d9 d3 k, slice d2 d0 k)) <$> elements arith
            else randomCtrl
          pure (alu, op, cis)
      | kind < 85 -> do
          -- Logic
          alu <- (++#) <$> elements [0b01, 0b11 :: BitVector 2] <*> genBV @2
          fromTable <- if invalidModes opts then chance 70 else pure True
          op <- if fromTable then elements logic else fst <$> randomCtrl
          cis <- if invalidModes opts then snd <$> randomCtrl else pure 0
          pure (alu, op, cis)
      | otherwise -> do
          -- ALUMODE[3:2] = 10, which the OPMODE check ignores
          alu <- (0b10 ++#) <$> genBV @2
          (op, cis) <- randomCtrl
          pure (alu, op, cis)
 where
  pFdbk = pReg cfg == 1
  -- Entries of the tables of valid modes. When generating only valid modes,
  -- only the ones valid for this configuration, and without CARRYINSEL = 010
  -- (which makes the multiplier output undefined).
  arith =
    [ k
    | (k, needsPReg) <- arithmeticModes
    , pFdbk || not (usesPFeedback (slice d9 d3 k) (slice d2 d0 k))
    , invalidModes opts || ((pFdbk || not needsPReg) && slice d2 d0 k /= 0b010) ]
  logic =
    [ op
    | (op, needsPReg) <- logicModes
    , pFdbk || not (usesPFeedback op 0)
    , invalidModes opts || pFdbk || not needsPReg ]

  randomCtrl = do
    op <- genBV
    cis <- genBV
    if pFdbk || not (usesPFeedback op cis) then pure (op, cis) else randomCtrl

-- | Generate the values of the input pins for a single cycle
genInputs :: Dsp48e1Config -> GenOpts -> Bool -> G Inputs
genInputs cfg opts inWarmup = do
  iA <- genBV
  iAcIn <- genBV
  iB <- genBV
  iBcIn <- genBV
  iC <- genC (cGen opts)
  iCarryCascIn <- genBit
  carryInL <- genBit
  (aluModeL, opModeL, cis) <- genCtrl cfg opts
  -- Without the output register, CARRYINSEL = 010 can still not select the
  -- P feedback, so it is safe to use. It makes the multiplier output undefined
  -- in the UNISIM model, so use it a bit more often.
  cis010 <- if invalidModes opts then chance 5 else pure False
  let iCarryInSel = if cis010 then 0b010 else cis
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
  -- With INMODE[4] set, the Verilog and VHDL UNISIM models disagree on the
  -- internal multiplier carry, which makes it undefined
  inModeL <- (if invalidModes opts then id else (.&. 0b01111)) <$> genBV
  iMultSignIn <- genBit
  iPcIn <- genBV
  iRstA <- rst
  iRstAllCarryIn <- rst
  iRstAluMode <- rst
  iRstB <- rst
  iRstC <- rst
  iRstCtrl <- rst
  iRstD <- rst
  iRstInMode <- rst
  iRstM <- rst
  iRstP <- rst
  pure Inputs
    { iAluMode = aluModeL `xor` isAluModeInverted cfg
    , iCarryIn = carryInL `xor` boolToBit (isCarryInInverted cfg)
    , iInMode = inModeL `xor` isInModeInverted cfg
    , iOpMode = opModeL `xor` isOpModeInverted cfg
    , ..
    }
 where
  ce = chance 85
  rst = if inWarmup then pure True else chance 3

-- | Stimuli for a single @DSP48E1@
stimuli :: Dsp48e1Config -> GenOpts -> Int -> [Inputs]
stimuli cfg opts =
  evalG (mapM (\n -> genInputs cfg opts (n < warmup)) [0 .. natToNum @Cycles - 1])

-- * Circuits under test

single :: Dsp48e1Config -> Clock Dom -> Signal Dom Inputs -> Signal Dom Outputs
single cfg clk i = bundleOutputs (dsp48e1 cfg clk (unbundleInputs i))

-- | Two @DSP48E1@s, the second using all cascade outputs of the first
cascaded ::
  Dsp48e1Config ->
  Dsp48e1Config ->
  Clock Dom ->
  Signal Dom (Inputs, Inputs) ->
  Signal Dom (Outputs, Outputs)
cascaded cfg1 cfg2 clk i = bundle (bundleOutputs out1, bundleOutputs out2)
 where
  (i1, i2) = unbundle i
  out1 = dsp48e1 cfg1 clk (unbundleInputs i1)
  out2 = dsp48e1 cfg2 clk (cascade out1 (unbundleInputs i2))

-- | 'multiplyAdd' and 'multiplyAccumulate', sharing their inputs
wrappers ::
  Clock Dom ->
  Signal Dom (Bool, Bool, Signed 25, Signed 18, Signed 48) ->
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
wrapperStimuli :: Int -> [(Bool, Bool, Signed 25, Signed 18, Signed 48)]
wrapperStimuli = evalG (mapM gen [0 .. natToNum @Cycles - 1])
 where
  gen n = do
    en <- if n < warmup then pure True else chance 85
    start <- if n < warmup then pure True else chance 10
    a <- unpack <$> genBV
    b <- unpack <$> genBV
    c <- unpack <$> genBV
    pure (en, start, a, b, c)

-- * Memory blobs

-- | 'blobs' for a single @DSP48E1@
singleBlobs :: Dsp48e1Config -> GenOpts -> Int -> ExpQ
singleBlobs cfg opts seed = blobs (single cfg) (stimuli cfg opts seed)

-- | 'blobs' for two cascaded @DSP48E1@s
cascadedBlobs :: Dsp48e1Config -> Dsp48e1Config -> GenOpts -> Int -> ExpQ
cascadedBlobs cfg1 cfg2 opts seed =
  blobs (cascaded cfg1 cfg2)
    (P.zip (stimuli cfg1 opts seed) (stimuli cfg2 opts (seed + 1)))

-- | 'blobs' for 'wrappers'
wrapperBlobs :: Int -> ExpQ
wrapperBlobs seed = blobs wrappers (wrapperStimuli seed)

-- * Configurations

-- | No registers at all
combinational :: Dsp48e1Config
combinational = defDsp48e1Config
  { aReg = 0, aCascReg = 0, bReg = 0, bCascReg = 0, cReg = 0, dReg = 0
  , adReg = 0, mReg = 0, pReg = 0, inModeReg = 0, opModeReg = 0
  , aluModeReg = 0, carryInReg = 0, carryInSelReg = 0
  , useMult = MultDynamic
  }

-- | Only the output register
pRegOnly :: Dsp48e1Config
pRegOnly = combinational{pReg = 1}

-- | All registers, two A and B registers, inverted control pins, pre-adder
pipelined :: Dsp48e1Config
pipelined = defDsp48e1Config
  { aReg = 2, aCascReg = 1, bReg = 2, bCascReg = 2
  , useDPort = True
  , isAluModeInverted = 0b1010
  , isCarryInInverted = True
  , isInModeInverted = 0b10101
  , isOpModeInverted = 0b1001001
  }

-- | Pre-adder, A and B from the (random) cascade inputs, a mix of registers.
-- OPMODEREG and CARRYINSELREG differ, which the UNISIM model warns about.
preAddCascade :: Dsp48e1Config
preAddCascade = defDsp48e1Config
  { aInput = Cascade, bInput = Cascade
  , aReg = 2, aCascReg = 2, bReg = 1, bCascReg = 1
  , useDPort = True
  , dReg = 0, adReg = 1, mReg = 0, inModeReg = 0, cReg = 0
  , opModeReg = 0, aluModeReg = 0
  , useMult = MultDynamic
  }

-- | Four 12-bit ALUs
simdFour12 :: Dsp48e1Config
simdFour12 = defDsp48e1Config
  { useMult = MultNone, mReg = 0, useSimd = Four12
  , cReg = 0, carryInReg = 0
  }

-- | Two 24-bit ALUs, no output register
simdTwo24 :: Dsp48e1Config
simdTwo24 = defDsp48e1Config
  { useMult = MultNone, mReg = 0, useSimd = Two24
  , pReg = 0, aReg = 0, aCascReg = 0, aluModeReg = 0
  }

-- | Pattern detector with attribute pattern and mask, automatic reset on
-- match
patDetAttr :: Dsp48e1Config
patDetAttr = defDsp48e1Config
  { usePatternDetect = True
  , patDetPattern = 0x0000_0000_0005
  , patDetMask = 0xFFFF_FFFF_FFF0
  , autoResetPatDet = AutoResetMatch
  }

-- | Pattern detector with C as pattern and a rounding mask, automatic reset
-- on a match followed by a mismatch
patDetRounding :: Dsp48e1Config
patDetRounding = defDsp48e1Config
  { usePatternDetect = True
  , selPattern = PatternC
  , selMask = MaskRoundingMode1
  , autoResetPatDet = AutoResetNotMatch
  , useMult = MultDynamic
  }

patDetRounding2 :: Dsp48e1Config
patDetRounding2 = patDetRounding{selMask = MaskRoundingMode2, cReg = 0}

-- | Pattern detector with C as mask, no output register
patDetMaskC :: Dsp48e1Config
patDetMaskC = defDsp48e1Config
  { usePatternDetect = True
  , selMask = MaskC
  , patDetPattern = 0x0000_0000_0003
  , pReg = 0
  }

-- | Second @DSP48E1@ of a cascade: A and B from the first one
cascadeSecond :: Dsp48e1Config -> Dsp48e1Config
cascadeSecond cfg = cfg{aInput = Cascade, bInput = Cascade}
