{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation model and HDL instantiation of the @DSP48E1@ primitive. See
"Clash.Cores.Xilinx.Unisim.Dsp48e1" for documentation.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Unisim.Dsp48e1.Internal where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))

import GHC.Stack (HasCallStack)

import qualified Clash.Signal.Delayed as D
import qualified Prelude as P

import Clash.Cores.Xilinx.Unisim.Dsp48.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | Static configuration of a @DSP48E1@. The field names follow the attribute
-- names of the primitive; see UG479 for their meaning. Use 'defDsp48e1Config'
-- to start from the primitive's defaults.
--
-- Pipeline register attributes are 'Index'es, so they can be written like the
-- attributes: @aReg = 2@. The combinations of 'aReg' and 'aCascReg' (and of
-- 'bReg' and 'bCascReg') must be one of (0, 0), (1, 1), (2, 1) or (2, 2), and
-- @'useMult' = 'MultNone'@ requires @'mReg' = 0@. Other combinations are
-- rejected by 'Clash.Cores.Xilinx.Unisim.Dsp48e1.dsp48e1', just like the UNISIM
-- simulation model rejects them.
--
-- @IS_CLK_INVERTED@ is not part of the configuration: it is derived from the
-- active edge of the clock domain.
data Dsp48e1Config = Dsp48e1Config
  { aInput :: InputSource
    -- ^ @A_INPUT@
  , bInput :: InputSource
    -- ^ @B_INPUT@
  , useDPort :: Bool
    -- ^ @USE_DPORT@: use the pre-adder (and the D input). When disabled, the
    -- multiplier multiplies the (INMODE-selected) A input by B.
  , useMult :: UseMult
    -- ^ @USE_MULT@
  , useSimd :: UseSimd
    -- ^ @USE_SIMD@

  , aReg :: Index 3
    -- ^ @AREG@: number of A input registers (A1 and A2)
  , aCascReg :: Index 3
    -- ^ @ACASCREG@: number of A registers in the path to @ACOUT@ ('acOut')
  , bReg :: Index 3
    -- ^ @BREG@: number of B input registers (B1 and B2)
  , bCascReg :: Index 3
    -- ^ @BCASCREG@: number of B registers in the path to @BCOUT@ ('bcOut')
  , cReg :: Index 2
    -- ^ @CREG@
  , dReg :: Index 2
    -- ^ @DREG@
  , adReg :: Index 2
    -- ^ @ADREG@: pre-adder output register
  , mReg :: Index 2
    -- ^ @MREG@: multiplier output register
  , pReg :: Index 2
    -- ^ @PREG@: output register
  , inModeReg :: Index 2
    -- ^ @INMODEREG@
  , opModeReg :: Index 2
    -- ^ @OPMODEREG@
  , aluModeReg :: Index 2
    -- ^ @ALUMODEREG@
  , carryInReg :: Index 2
    -- ^ @CARRYINREG@
  , carryInSelReg :: Index 2
    -- ^ @CARRYINSELREG@

  , usePatternDetect :: Bool
    -- ^ @USE_PATTERN_DETECT@: @PATDET@ ('True') or @NO_PATDET@ ('False')
  , patDetPattern :: BitVector 48
    -- ^ @PATTERN@
  , patDetMask :: BitVector 48
    -- ^ @MASK@. Bits set to one are ignored by the pattern detector.
  , selPattern :: SelPattern
    -- ^ @SEL_PATTERN@
  , selMask :: SelMask
    -- ^ @SEL_MASK@
  , autoResetPatDet :: AutoResetPatDet
    -- ^ @AUTORESET_PATDET@

  , isAluModeInverted :: BitVector 4
    -- ^ @IS_ALUMODE_INVERTED@
  , isCarryInInverted :: Bool
    -- ^ @IS_CARRYIN_INVERTED@
  , isInModeInverted :: BitVector 5
    -- ^ @IS_INMODE_INVERTED@
  , isOpModeInverted :: BitVector 7
    -- ^ @IS_OPMODE_INVERTED@
  }
  deriving (Show, Eq, Generic)

-- | The default attributes of the primitive, as documented in UG953: all
-- pipeline registers enabled (one A and one B register), multiplier used,
-- pre-adder unused, no SIMD, no pattern detection, no inversions.
defDsp48e1Config :: Dsp48e1Config
defDsp48e1Config = Dsp48e1Config
  { aInput = Direct
  , bInput = Direct
  , useDPort = False
  , useMult = MultMultiply
  , useSimd = One48
  , aReg = 1
  , aCascReg = 1
  , bReg = 1
  , bCascReg = 1
  , cReg = 1
  , dReg = 1
  , adReg = 1
  , mReg = 1
  , pReg = 1
  , inModeReg = 1
  , opModeReg = 1
  , aluModeReg = 1
  , carryInReg = 1
  , carryInSelReg = 1
  , usePatternDetect = False
  , patDetPattern = 0
  , patDetMask = 0x3FFFFFFFFFFF
  , selPattern = PatternAttr
  , selMask = MaskAttr
  , autoResetPatDet = NoAutoReset
  , isAluModeInverted = 0
  , isCarryInInverted = False
  , isInModeInverted = 0
  , isOpModeInverted = 0
  }

-- | Check the attribute combinations the UNISIM model rejects
configError :: Dsp48e1Config -> Maybe String
configError Dsp48e1Config{..}
  | aReg /= 2 && aCascReg /= aReg =
      Just "when aReg is 0 or 1, aCascReg must be set to the same value"
  | aReg == 2 && aCascReg == 0 =
      Just "when aReg is 2, aCascReg must be set to 1 or 2"
  | bReg /= 2 && bCascReg /= bReg =
      Just "when bReg is 0 or 1, bCascReg must be set to the same value"
  | bReg == 2 && bCascReg == 0 =
      Just "when bReg is 2, bCascReg must be set to 1 or 2"
  | useMult == MultNone && mReg /= 0 =
      Just "mReg must be set to 0 when useMult is MultNone"
  | otherwise = Nothing

-- | Input ports of a @DSP48E1@. The field names follow the port names of the
-- primitive, except for the data inputs @A@, @B@, @C@ and @D@, which are
-- called 'dataA', 'dataB', 'dataC' and 'dataD'. See UG479 for their meaning.
-- Use 'defDsp48e1Input' to tie off the ports you don't need.
--
-- The values are the values on the pins: the inversion attributes in
-- 'Dsp48e1Config' (e.g. 'isOpModeInverted') are applied by the primitive.
--
-- All resets are synchronous and active high.
data Dsp48e1Input dom = Dsp48e1Input
  { dataA :: Signal dom (BitVector 30)
    -- ^ @A@: 30-bit data input. Its 25 least significant bits are the
    -- multiplier\/pre-adder input, all 30 bits form the upper part of the
    -- A:B concatenation.
  , acIn :: Signal dom (BitVector 30)
    -- ^ @ACIN@: A cascade input, must come from 'acOut' of another @DSP48E1@
  , aluMode :: Signal dom (BitVector 4)
    -- ^ @ALUMODE@: ALU function select
  , dataB :: Signal dom (BitVector 18)
    -- ^ @B@: 18-bit data input
  , bcIn :: Signal dom (BitVector 18)
    -- ^ @BCIN@: B cascade input, must come from 'bcOut' of another @DSP48E1@
  , dataC :: Signal dom (BitVector 48)
    -- ^ @C@: 48-bit data input
  , carryCascIn :: Signal dom Bit
    -- ^ @CARRYCASCIN@: carry cascade input, must come from 'carryCascOut' of
    -- another @DSP48E1@
  , carryIn :: Signal dom Bit
    -- ^ @CARRYIN@: carry input from the fabric
  , carryInSel :: Signal dom (BitVector 3)
    -- ^ @CARRYINSEL@: selects the carry source
  , ceA1 :: Signal dom Bool
    -- ^ @CEA1@: clock enable of the first A register (A1)
  , ceA2 :: Signal dom Bool
    -- ^ @CEA2@: clock enable of the second A register (A2)
  , ceAd :: Signal dom Bool
    -- ^ @CEAD@: clock enable of the pre-adder output register
  , ceAluMode :: Signal dom Bool
    -- ^ @CEALUMODE@: clock enable of the ALUMODE register
  , ceB1 :: Signal dom Bool
    -- ^ @CEB1@: clock enable of the first B register (B1)
  , ceB2 :: Signal dom Bool
    -- ^ @CEB2@: clock enable of the second B register (B2)
  , ceC :: Signal dom Bool
    -- ^ @CEC@: clock enable of the C register
  , ceCarryIn :: Signal dom Bool
    -- ^ @CECARRYIN@: clock enable of the CARRYIN register
  , ceCtrl :: Signal dom Bool
    -- ^ @CECTRL@: clock enable of the OPMODE and CARRYINSEL registers
  , ceD :: Signal dom Bool
    -- ^ @CED@: clock enable of the D register
  , ceInMode :: Signal dom Bool
    -- ^ @CEINMODE@: clock enable of the INMODE register
  , ceM :: Signal dom Bool
    -- ^ @CEM@: clock enable of the multiplier output register
  , ceP :: Signal dom Bool
    -- ^ @CEP@: clock enable of the output registers
  , dataD :: Signal dom (BitVector 25)
    -- ^ @D@: 25-bit pre-adder input
  , inMode :: Signal dom (BitVector 5)
    -- ^ @INMODE@: selects the pre-adder and multiplier inputs
  , multSignIn :: Signal dom Bit
    -- ^ @MULTSIGNIN@: multiplier sign cascade input, must come from
    -- 'multSignOut' of another @DSP48E1@
  , opMode :: Signal dom (BitVector 7)
    -- ^ @OPMODE@: selects the X, Y and Z multiplexer inputs of the ALU
  , pcIn :: Signal dom (BitVector 48)
    -- ^ @PCIN@: P cascade input, must come from 'pcOut' of another @DSP48E1@
  , rstA :: Signal dom Bool
    -- ^ @RSTA@: reset of the A registers
  , rstAllCarryIn :: Signal dom Bool
    -- ^ @RSTALLCARRYIN@: reset of the CARRYIN register and the internal
    -- multiplier carry register
  , rstAluMode :: Signal dom Bool
    -- ^ @RSTALUMODE@: reset of the ALUMODE register
  , rstB :: Signal dom Bool
    -- ^ @RSTB@: reset of the B registers
  , rstC :: Signal dom Bool
    -- ^ @RSTC@: reset of the C register
  , rstCtrl :: Signal dom Bool
    -- ^ @RSTCTRL@: reset of the OPMODE and CARRYINSEL registers
  , rstD :: Signal dom Bool
    -- ^ @RSTD@: reset of the D and pre-adder output registers
  , rstInMode :: Signal dom Bool
    -- ^ @RSTINMODE@: reset of the INMODE register
  , rstM :: Signal dom Bool
    -- ^ @RSTM@: reset of the multiplier output register
  , rstP :: Signal dom Bool
    -- ^ @RSTP@: reset of the output registers
  }

-- | Default inputs: all data, cascade and control inputs zero, all clock
-- enables asserted, and all resets deasserted
defDsp48e1Input :: Dsp48e1Input dom
defDsp48e1Input = Dsp48e1Input
  { dataA = pure 0
  , acIn = pure 0
  , aluMode = pure 0
  , dataB = pure 0
  , bcIn = pure 0
  , dataC = pure 0
  , carryCascIn = pure 0
  , carryIn = pure 0
  , carryInSel = pure 0
  , ceA1 = pure True
  , ceA2 = pure True
  , ceAd = pure True
  , ceAluMode = pure True
  , ceB1 = pure True
  , ceB2 = pure True
  , ceC = pure True
  , ceCarryIn = pure True
  , ceCtrl = pure True
  , ceD = pure True
  , ceInMode = pure True
  , ceM = pure True
  , ceP = pure True
  , dataD = pure 0
  , inMode = pure 0
  , multSignIn = pure 0
  , opMode = pure 0
  , pcIn = pure 0
  , rstA = pure False
  , rstAllCarryIn = pure False
  , rstAluMode = pure False
  , rstB = pure False
  , rstC = pure False
  , rstCtrl = pure False
  , rstD = pure False
  , rstInMode = pure False
  , rstM = pure False
  , rstP = pure False
  }

-- | Output ports of a @DSP48E1@. The field names follow the port names of the
-- primitive, except for the data output @P@, which is called 'dataP'.
data Dsp48e1Output dom = Dsp48e1Output
  { acOut :: Signal dom (BitVector 30)
    -- ^ @ACOUT@: A cascade output
  , bcOut :: Signal dom (BitVector 18)
    -- ^ @BCOUT@: B cascade output
  , carryCascOut :: Signal dom Bit
    -- ^ @CARRYCASCOUT@: carry cascade output
  , carryOut :: Signal dom (BitVector 4)
    -- ^ @CARRYOUT@: carry outputs of the (SIMD) ALU. Bit /i/ is the carry out
    -- of bit /12(i + 1) - 1/. The bits that are not meaningful in the
    -- selected SIMD mode, and all bits when multiplying or doing a logic
    -- operation, are undefined.
  , multSignOut :: Signal dom Bit
    -- ^ @MULTSIGNOUT@: multiplier sign cascade output
  , overflow :: Signal dom Bool
    -- ^ @OVERFLOW@: overflow detected by the pattern detector. Undefined when
    -- neither 'usePatternDetect' nor 'pReg' is enabled.
  , dataP :: Signal dom (BitVector 48)
    -- ^ @P@: data output
  , patternBDetect :: Signal dom Bool
    -- ^ @PATTERNBDETECT@: the output matches the complement of the pattern
  , patternDetect :: Signal dom Bool
    -- ^ @PATTERNDETECT@: the output matches the pattern
  , pcOut :: Signal dom (BitVector 48)
    -- ^ @PCOUT@: P cascade output
  , underflow :: Signal dom Bool
    -- ^ @UNDERFLOW@: underflow detected by the pattern detector. Undefined
    -- when neither 'usePatternDetect' nor 'pReg' is enabled.
  }

-- | Connect the cascade outputs (@ACOUT@, @BCOUT@, @PCOUT@, @CARRYCASCOUT@,
-- and @MULTSIGNOUT@) of a @DSP48E1@ to the cascade inputs of the next one.
-- Note that the next @DSP48E1@ only uses @ACIN@ and @BCIN@ when configured to
-- do so with 'aInput' and 'bInput', and only uses @PCIN@, @CARRYCASCIN@ and
-- @MULTSIGNIN@ when selected by @OPMODE@ and @CARRYINSEL@.
cascade :: Dsp48e1Output dom -> Dsp48e1Input dom -> Dsp48e1Input dom
cascade prev next = next
  { acIn = acOut prev
  , bcIn = bcOut prev
  , pcIn = pcOut prev
  , carryCascIn = carryCascOut prev
  , multSignIn = multSignOut prev
  }

-- | Values on the input ports during a single cycle
data Inputs = Inputs
  { iA :: BitVector 30
  , iAcIn :: BitVector 30
  , iAluMode :: BitVector 4
  , iB :: BitVector 18
  , iBcIn :: BitVector 18
  , iC :: BitVector 48
  , iCarryCascIn :: Bit
  , iCarryIn :: Bit
  , iCarryInSel :: BitVector 3
  , iCeA1 :: Bool
  , iCeA2 :: Bool
  , iCeAd :: Bool
  , iCeAluMode :: Bool
  , iCeB1 :: Bool
  , iCeB2 :: Bool
  , iCeC :: Bool
  , iCeCarryIn :: Bool
  , iCeCtrl :: Bool
  , iCeD :: Bool
  , iCeInMode :: Bool
  , iCeM :: Bool
  , iCeP :: Bool
  , iD :: BitVector 25
  , iInMode :: BitVector 5
  , iMultSignIn :: Bit
  , iOpMode :: BitVector 7
  , iPcIn :: BitVector 48
  , iRstA :: Bool
  , iRstAllCarryIn :: Bool
  , iRstAluMode :: Bool
  , iRstB :: Bool
  , iRstC :: Bool
  , iRstCtrl :: Bool
  , iRstD :: Bool
  , iRstInMode :: Bool
  , iRstM :: Bool
  , iRstP :: Bool
  }
  deriving (Show, Generic, NFDataX, BitPack)

-- | Values on the output ports during a single cycle
data Outputs = Outputs
  { oAcOut :: BitVector 30
  , oBcOut :: BitVector 18
  , oCarryCascOut :: Bit
  , oCarryOut :: BitVector 4
  , oMultSignOut :: Bit
  , oOverflow :: Bool
  , oP :: BitVector 48
  , oPatternBDetect :: Bool
  , oPatternDetect :: Bool
  , oPcOut :: BitVector 48
  , oUnderflow :: Bool
  }
  deriving (Show, Generic, NFDataX, BitPack)

-- | Combine the input signals into a signal of 'Inputs'
bundleInputs :: Dsp48e1Input dom -> Signal dom Inputs
bundleInputs Dsp48e1Input{..} =
  Inputs
    <$> dataA <*> acIn <*> aluMode <*> dataB <*> bcIn <*> dataC
    <*> carryCascIn <*> carryIn <*> carryInSel
    <*> ceA1 <*> ceA2 <*> ceAd <*> ceAluMode <*> ceB1 <*> ceB2 <*> ceC
    <*> ceCarryIn <*> ceCtrl <*> ceD <*> ceInMode <*> ceM <*> ceP
    <*> dataD <*> inMode <*> multSignIn <*> opMode <*> pcIn
    <*> rstA <*> rstAllCarryIn <*> rstAluMode <*> rstB <*> rstC <*> rstCtrl
    <*> rstD <*> rstInMode <*> rstM <*> rstP

-- | Split a signal of 'Inputs' into the input signals
unbundleInputs :: Signal dom Inputs -> Dsp48e1Input dom
unbundleInputs i = Dsp48e1Input
  { dataA = iA <$> i
  , acIn = iAcIn <$> i
  , aluMode = iAluMode <$> i
  , dataB = iB <$> i
  , bcIn = iBcIn <$> i
  , dataC = iC <$> i
  , carryCascIn = iCarryCascIn <$> i
  , carryIn = iCarryIn <$> i
  , carryInSel = iCarryInSel <$> i
  , ceA1 = iCeA1 <$> i
  , ceA2 = iCeA2 <$> i
  , ceAd = iCeAd <$> i
  , ceAluMode = iCeAluMode <$> i
  , ceB1 = iCeB1 <$> i
  , ceB2 = iCeB2 <$> i
  , ceC = iCeC <$> i
  , ceCarryIn = iCeCarryIn <$> i
  , ceCtrl = iCeCtrl <$> i
  , ceD = iCeD <$> i
  , ceInMode = iCeInMode <$> i
  , ceM = iCeM <$> i
  , ceP = iCeP <$> i
  , dataD = iD <$> i
  , inMode = iInMode <$> i
  , multSignIn = iMultSignIn <$> i
  , opMode = iOpMode <$> i
  , pcIn = iPcIn <$> i
  , rstA = iRstA <$> i
  , rstAllCarryIn = iRstAllCarryIn <$> i
  , rstAluMode = iRstAluMode <$> i
  , rstB = iRstB <$> i
  , rstC = iRstC <$> i
  , rstCtrl = iRstCtrl <$> i
  , rstD = iRstD <$> i
  , rstInMode = iRstInMode <$> i
  , rstM = iRstM <$> i
  , rstP = iRstP <$> i
  }

-- | Combine the output signals into a signal of 'Outputs'
bundleOutputs :: Dsp48e1Output dom -> Signal dom Outputs
bundleOutputs Dsp48e1Output{..} =
  Outputs
    <$> acOut <*> bcOut <*> carryCascOut <*> carryOut <*> multSignOut
    <*> overflow <*> dataP <*> patternBDetect <*> patternDetect <*> pcOut
    <*> underflow

-- | Split a signal of 'Outputs' into the output signals
unbundleOutputs :: Signal dom Outputs -> Dsp48e1Output dom
unbundleOutputs o = Dsp48e1Output
  { acOut = oAcOut <$> o
  , bcOut = oBcOut <$> o
  , carryCascOut = oCarryCascOut <$> o
  , carryOut = oCarryOut <$> o
  , multSignOut = oMultSignOut <$> o
  , overflow = oOverflow <$> o
  , dataP = oP <$> o
  , patternBDetect = oPatternBDetect <$> o
  , patternDetect = oPatternDetect <$> o
  , pcOut = oPcOut <$> o
  , underflow = oUnderflow <$> o
  }

-- | Contents of all registers in the primitive, plus the state of the
-- simulation model's OPMODE check. The names follow the UNISIM simulation
-- model.
data State = State
  { sA1 :: BitVector 30
  , sA2 :: BitVector 30
  , sB1 :: BitVector 18
  , sB2 :: BitVector 18
  , sC :: BitVector 48
  , sD :: BitVector 25
  , sAd :: BitVector 25
  , sInMode :: BitVector 5
  , sOpMode :: BitVector 7
  , sCarryInSel :: BitVector 3
  , sAluMode :: BitVector 4
  , sCarryIn :: Bit
  , sMultCarryIn :: Bit
  , sM :: BitVector 43
  , sPdet1 :: Bool
  , sPdet2 :: Bool
  , sPdetb1 :: Bool
  , sPdetb2 :: Bool
  , sCout :: BitVector 4
  , sCarryCascOut :: Bit
  , sMultSign :: Bit
  , sP :: BitVector 48
  , sOpModeValid :: Bool
    -- ^ Result of the most recent OPMODE check (@opmode_valid_flag@)
  , sCtrl :: BitVector 14
    -- ^ ALUMODE, OPMODE and CARRYINSEL (after their registers) in the
    -- previous cycle
  }
  deriving (Generic, NFDataX)

-- | All registers are zero after configuration of the FPGA
initState :: State
initState = State
  { sA1 = 0
  , sA2 = 0
  , sB1 = 0
  , sB2 = 0
  , sC = 0
  , sD = 0
  , sAd = 0
  , sInMode = 0
  , sOpMode = 0
  , sCarryInSel = 0
  , sAluMode = 0
  , sCarryIn = 0
  , sMultCarryIn = 0
  , sM = 0
  , sPdet1 = False
  , sPdet2 = False
  , sPdetb1 = False
  , sPdetb2 = False
  , sCout = 0
  , sCarryCascOut = 0
  , sMultSign = 0
  , sP = 0
  , sOpModeValid = True
  , sCtrl = 0
  }

-- | Whether the combination of OPMODE and CARRYINSEL is valid for the
-- arithmetic ALU modes (@ALUMODE[3:2] = 00@), according to the table in the
-- UNISIM model. The second element of each entry says whether the combination
-- requires @PREG = 1@.
arithmeticModes :: [(BitVector 10, Bool)]
arithmeticModes =
  [ (0b0000000000, False)
  , (0b0000010000, True)
  , (0b0000010010, True)
  , (0b0000011000, False)
  , (0b0000011010, False)
  , (0b0000011100, True)
  , (0b0000101000, False)
  , (0b0001000000, False)
  , (0b0001010000, True)
  , (0b0001010010, True)
  , (0b0001011000, False)
  , (0b0001011010, False)
  , (0b0001011100, True)
  , (0b0001100000, False)
  , (0b0001100010, False)
  , (0b0001100100, True)
  , (0b0001110000, True)
  , (0b0001110010, True)
  , (0b0001110101, True)
  , (0b0001110111, True)
  , (0b0001111000, False)
  , (0b0001111010, False)
  , (0b0001111100, True)
  , (0b0010000000, False)
  , (0b0010010000, True)
  , (0b0010010101, True)
  , (0b0010010111, True)
  , (0b0010011000, False)
  , (0b0010011001, False)
  , (0b0010011011, False)
  , (0b0010101000, False)
  , (0b0010101001, False)
  , (0b0010101011, False)
  , (0b0010101110, False)
  , (0b0011000000, False)
  , (0b0011010000, True)
  , (0b0011010101, True)
  , (0b0011010111, True)
  , (0b0011011000, False)
  , (0b0011011001, False)
  , (0b0011011011, False)
  , (0b0011100000, False)
  , (0b0011100001, False)
  , (0b0011100011, False)
  , (0b0011100100, False)
  , (0b0011110000, True)
  , (0b0011110001, True)
  , (0b0011110011, True)
  , (0b0011110101, True)
  , (0b0011110111, True)
  , (0b0011111000, False)
  , (0b0011111001, False)
  , (0b0011111011, False)
  , (0b0100000000, True)
  , (0b0100000010, True)
  , (0b0100010000, True)
  , (0b0100010010, True)
  , (0b0100011000, True)
  , (0b0100011010, True)
  , (0b0100011101, True)
  , (0b0100011111, True)
  , (0b0100101000, True)
  , (0b0100101101, True)
  , (0b0100101111, True)
  , (0b0101000000, True)
  , (0b0101000010, True)
  , (0b0101010000, True)
  , (0b0101011000, True)
  , (0b0101011101, True)
  , (0b0101011111, True)
  , (0b0101100000, True)
  , (0b0101100010, True)
  , (0b0101100101, True)
  , (0b0101100111, True)
  , (0b0101110000, True)
  , (0b0101110101, True)
  , (0b0101110111, True)
  , (0b0101111000, True)
  , (0b0101111101, True)
  , (0b0101111111, True)
  , (0b0110000000, False)
  , (0b0110000010, False)
  , (0b0110000100, True)
  , (0b0110010000, True)
  , (0b0110010010, True)
  , (0b0110010101, True)
  , (0b0110010111, True)
  , (0b0110011000, False)
  , (0b0110011010, False)
  , (0b0110011100, True)
  , (0b0110101000, False)
  , (0b0110101110, False)
  , (0b0111000000, False)
  , (0b0111000010, False)
  , (0b0111000100, True)
  , (0b0111010000, True)
  , (0b0111010101, True)
  , (0b0111010111, True)
  , (0b0111011000, False)
  , (0b0111100000, False)
  , (0b0111100010, False)
  , (0b0111110000, True)
  , (0b0111111000, False)
  , (0b1001000010, True)
  , (0b1010000000, False)
  , (0b1010010000, True)
  , (0b1010010101, True)
  , (0b1010010111, True)
  , (0b1010011000, False)
  , (0b1010011001, False)
  , (0b1010011011, False)
  , (0b1010101000, False)
  , (0b1010101001, False)
  , (0b1010101011, False)
  , (0b1010101110, False)
  , (0b1011000000, False)
  , (0b1011010000, True)
  , (0b1011010101, True)
  , (0b1011010111, True)
  , (0b1011011000, False)
  , (0b1011011001, False)
  , (0b1011011011, False)
  , (0b1011100000, False)
  , (0b1011100001, False)
  , (0b1011100011, False)
  , (0b1011110000, True)
  , (0b1011110001, True)
  , (0b1011110011, True)
  , (0b1011110101, True)
  , (0b1011110111, True)
  , (0b1011111000, False)
  , (0b1011111001, False)
  , (0b1011111011, False)
  , (0b1100000000, True)
  , (0b1100010000, True)
  , (0b1100011000, True)
  , (0b1100011101, True)
  , (0b1100011111, True)
  , (0b1100101000, True)
  , (0b1100101101, True)
  , (0b1100101111, True)
  , (0b1101000000, True)
  , (0b1101010000, True)
  , (0b1101011000, True)
  , (0b1101011101, True)
  , (0b1101011111, True)
  , (0b1101100000, True)
  , (0b1101100101, True)
  , (0b1101100111, True)
  , (0b1101110000, True)
  , (0b1101110101, True)
  , (0b1101110111, True)
  , (0b1101111000, True)
  , (0b1101111101, True)
  , (0b1101111111, True)
  ]

-- | Whether OPMODE is valid for the logic ALU modes (@ALUMODE[3:2] = 01@ or
-- @11@), according to the table in the UNISIM model. The second element of
-- each entry says whether the OPMODE requires @PREG = 1@.
logicModes :: [(BitVector 7, Bool)]
logicModes =
  [ (0b0000000, False)
  , (0b0000010, True)
  , (0b0000011, False)
  , (0b0001000, False)
  , (0b0001010, True)
  , (0b0001011, False)
  , (0b0010000, False)
  , (0b0010010, True)
  , (0b0010011, False)
  , (0b0011000, False)
  , (0b0011010, True)
  , (0b0011011, False)
  , (0b0100000, True)
  , (0b0100010, True)
  , (0b0100011, True)
  , (0b0101000, True)
  , (0b0101010, True)
  , (0b0101011, True)
  , (0b0110000, False)
  , (0b0110010, True)
  , (0b0110011, False)
  , (0b0111000, False)
  , (0b0111010, True)
  , (0b0111011, False)
  , (0b1010000, False)
  , (0b1010010, True)
  , (0b1010011, False)
  , (0b1011000, False)
  , (0b1011010, True)
  , (0b1011011, False)
  , (0b1100000, True)
  , (0b1100010, True)
  , (0b1100011, True)
  , (0b1101000, True)
  , (0b1101010, True)
  , (0b1101011, True)
  ]

-- | The OPMODE check of the UNISIM model, for a combination of ALUMODE,
-- OPMODE and CARRYINSEL. Returns 'Nothing' for @ALUMODE[3:2] = 10@, for which
-- the UNISIM model does not update its flag.
opModeCheck :: Bool -> BitVector 4 -> BitVector 7 -> BitVector 3 -> Maybe Bool
opModeCheck pRegUsed alu op cis = case slice d3 d2 alu of
  0b00
    -- The UNISIM model checks these combinations separately
    | slice d3 d0 op ++# cis == 0b0101010 -> Just True
    | otherwise -> Just (lookupMode (op ++# cis) arithmeticModes)
  0b10 -> Nothing
  _ -> Just (lookupMode op logicModes)
 where
  lookupMode :: Eq a => a -> [(a, Bool)] -> Bool
  lookupMode k tbl = case P.lookup k tbl of
    Nothing -> False
    Just needsPReg -> pRegUsed || not needsPReg

-- | One clock cycle of the @DSP48E1@: computes the outputs during the cycle,
-- and the register contents after the next active clock edge. This is a
-- translation of the UNISIM Verilog model (@DSP48E1.v@) of Vivado 2022.1,
-- made more undefined where the VHDL model disagrees with it.
step :: Dsp48e1Config -> State -> Inputs -> (State, Outputs)
step Dsp48e1Config{..} s Inputs{..} = (sNext, o)
 where
  -- Optional input inversion
  aluModeIn = iAluMode `xor` isAluModeInverted
  carryInIn = iCarryIn `xor` boolToBit isCarryInInverted
  inModeIn = iInMode `xor` isInModeInverted
  opModeIn = iOpMode `xor` isOpModeInverted

  -- Selects a register's output if it's enabled, its input otherwise
  regOr :: Index 2 -> a -> a -> a
  regOr r q d = if r == 1 then q else d

  pRegUsed = pReg == 1
  isFour12 = useSimd == Four12
  isOne48 = useSimd == One48

  -- Control registers
  opModeMux = regOr opModeReg (sOpMode s) opModeIn
  carryInSelMux = regOr carryInSelReg (sCarryInSel s) iCarryInSel
  aluModeMux = regOr aluModeReg (sAluMode s) aluModeIn
  inModeMux = regOr inModeReg (sInMode s) inModeIn
  carryInMux0 = regOr carryInReg (sCarryIn s) carryInIn

  opModeX = slice d1 d0 opModeMux
  opModeY = slice d3 d2 opModeMux
  opModeZ = slice d6 d4 opModeMux
  opModeXY = slice d3 d0 opModeMux

  aluModeBit = testBit aluModeMux
  inModeBit = testBit inModeMux

  -- A and B input registers. Without registers (AREG = 0), the UNISIM model
  -- never clocks A1 and A2, so they stay zero.
  aDirect = case aInput of
    Direct -> iA
    Cascade -> iAcIn
  aMux = if aReg == 0 then aDirect else sA2 s
  acOutV = if aCascReg == 1 && aReg == 2 then sA1 s else aMux

  bDirect = case bInput of
    Direct -> iB
    Cascade -> iBcIn
  bMux = if bReg == 0 then bDirect else sB2 s
  bcOutV = if bCascReg == 1 && bReg == 2 then sB1 s else bMux

  -- C and D input registers
  cMux = regOr cReg (sC s) iC
  dMux = regOr dReg (sD s) iD

  -- Pre-adder
  aPreAddSub :: BitVector 25
  aPreAddSub
    | inModeBit 1 = 0
    | inModeBit 0 = truncateB (sA1 s)
    | otherwise = truncateB aMux

  bMult = if inModeBit 4 then sB1 s else bMux

  dPortion = if inModeBit 2 then dMux else 0
  adAddSub = if inModeBit 3 then dPortion - aPreAddSub else aPreAddSub + dPortion
  adMux = regOr adReg (sAd s) adAddSub
  adMult = if useDPort then adMux else aPreAddSub

  -- Multiplier. The Verilog UNISIM model drives the multiplier output with X
  -- when the CARRYINSEL /pin/ is 010, and the multiplier register output when
  -- the CARRYINSEL register output is 010.
  multO :: BitVector 43
  multO
    | useMult == MultNone || not isOne48 = 0
    | iCarryInSel == 0b010 =
        deepErrorX "dsp48e1: multiplier output undefined, as CARRYINSEL is 010"
    | otherwise = pack ((unpack adMult :: Signed 25) `mul` (unpack bMult :: Signed 18))

  multMux
    | carryInSelMux == 0b010 =
        deepErrorX "dsp48e1: multiplier output undefined, as CARRYINSEL is 010"
    | otherwise = regOr mReg (sM s) multO

  -- Carry for rounding a product towards zero. The Verilog UNISIM model uses
  -- the sign of the (INMODE-selected) B multiplier input, the VHDL model the
  -- sign of the B2 register. Undefined when they disagree.
  multCarryIn
    | msb bMult == msb bMux = complement (msb adMult `xor` msb bMult)
    | otherwise =
        errorX "dsp48e1: multiplier carry undefined (Verilog and VHDL UNISIM models disagree)"
  multCarryMux = regOr mReg (sMultCarryIn s) multCarryIn

  -- ALU input multiplexers. The P feedback always comes from the output
  -- register; the feedback is invalid without it (see 'pFeedbackInvalid').
  pFdbk = sP s

  shiftR17 :: BitVector 48 -> BitVector 48
  shiftR17 x = pack ((unpack x :: Signed 48) `shiftR` 17)

  abInvalid =
       useMult == MultMultiply
    && (  (aReg == 0 && bReg == 0 && mReg == 0)
       || (aReg == 0 && bReg == 0 && pReg == 0)
       || (mReg == 0 && pReg == 0) )

  xMux :: BitVector 48
  xMux = case opModeX of
    0 -> 0
    1 -> signExtend multMux
    2 -> pFdbk
    _ | abInvalid ->
          -- The Verilog UNISIM model keeps its previous value, the VHDL model
          -- drives X
          deepErrorX "dsp48e1: OPMODE[1:0] = 11 is invalid with USE_MULT = MULTIPLY when (A, B, and M) or (A, B, and P) or (M and P) are not registered"
      | otherwise -> aMux ++# bMux

  yMacCascd :: BitVector 48
  yMacCascd
    | opModeZ == 0b100 = pack (repeat iMultSignIn :: Vec 48 Bit)
    | otherwise = maxBound

  yMux = case opModeY of
    2 -> yMacCascd
    3 -> cMux
    _ -> 0

  zMux = case opModeZ of
    0 -> 0
    1 -> iPcIn
    2 -> pFdbk
    3 -> cMux
    4 -> pFdbk
    5 -> shiftR17 iPcIn
    _ -> shiftR17 pFdbk

  -- Carry input
  carryCascOutMux = sCarryCascOut s

  cinTmp = case carryInSelMux of
    0 -> carryInMux0
    1 -> complement (msb iPcIn)
    2 -> iCarryCascIn
    3 -> msb iPcIn
    4 -> carryCascOutMux
    5 -> complement (msb pFdbk)
    6 -> multCarryMux
    _ -> msb pFdbk

  cin = if aluModeBit 3 || aluModeBit 2 then 0 else cinTmp

  -- ALU: three input carry-save adder (X, Y, Z), followed by the (SIMD) final
  -- adder
  zOptInv = if aluModeBit 0 then complement zMux else zMux
  co = (xMux .&. yMux) .|. (zOptInv .&. yMux) .|. (xMux .&. zOptInv)
  sumXyz = zOptInv `xor` xMux `xor` yMux

  coMux = if aluModeBit 2 then 0 else co
  sMux = if aluModeBit 3 then co else sumXyz

  bitB :: KnownNat n => BitVector n -> Int -> Bit
  bitB = (!)

  s0 :: BitVector 13
  s0 = zeroExtend (slice d10 d0 coMux ++# pack cin) + zeroExtend (slice d11 d0 sMux)
  cout0 = bitB coMux 11 `xor` bitB s0 12

  c1 = if isFour12 then 0 else bitB s0 12
  co11Lsb = if isFour12 then 0 else bitB coMux 11
  s1 :: BitVector 13
  s1 =
      zeroExtend (slice d22 d12 coMux ++# pack co11Lsb)
    + zeroExtend (slice d23 d12 sMux)
    + zeroExtend (pack c1)
  cout1 = bitB coMux 23 `xor` bitB s1 12

  c2 = if not isOne48 then 0 else bitB s1 12
  co23Lsb = if not isOne48 then 0 else bitB coMux 23
  s2 :: BitVector 13
  s2 =
      zeroExtend (slice d34 d24 coMux ++# pack co23Lsb)
    + zeroExtend (slice d35 d24 sMux)
    + zeroExtend (pack c2)
  cout2 = bitB coMux 35 `xor` bitB s2 12

  c3 = if isFour12 then 0 else bitB s2 12
  co35Lsb = if isFour12 then 0 else bitB coMux 35
  s3 :: BitVector 14
  s3 =
      zeroExtend (slice d47 d36 coMux ++# pack co35Lsb)
    + zeroExtend (slice d47 d36 sMux)
    + zeroExtend (pack c3)
  cout3 = bitB s3 12

  aluMode10 = aluModeBit 0 && aluModeBit 1
  hw c = if aluMode10 then complement c else c

  aluOut0 :: BitVector 48
  aluOut0 =
    (if aluModeBit 1 then complement else id)
      (  slice d11 d0 s3 ++# slice d11 d0 s2
     ++# slice d11 d0 s1 ++# slice d11 d0 s0 )

  -- Carry outputs. The UNISIM model drives the carry outputs that aren't
  -- meaningful with X.
  multOrLogic = opModeXY == 0b0101 || slice d3 d2 aluModeMux /= 0
  simdUndefined :: BitVector 4
  simdUndefined = case useSimd of
    One48 -> 0b0111
    Two24 -> 0b0101
    Four12 -> 0b0000
  coutIn0 :: BitVector 4
  coutIn0 =
    markUndefined (if multOrLogic then 0b1111 else simdUndefined) $
      fromBits (hw cout3 :> hw cout2 :> hw cout1 :> hw cout0 :> Nil)

  -- Without the output register, the P feedback (and the carry cascade
  -- feedback) is a combinational loop in the UNISIM models. The ALU outputs
  -- are undefined in that case.
  pFeedbackInvalid =
       not pRegUsed
    && (  opModeX == 0b10
       || opModeZ `elem` [0b010, 0b100, 0b110, 0b111]
       || carryInSelMux `elem` [0b100, 0b101, 0b111] )

  undefinedIfInvalid :: NFDataX a => a -> a
  undefinedIfInvalid a
    | pFeedbackInvalid =
        deepErrorX "dsp48e1: selecting the P feedback (OPMODE or CARRYINSEL) requires PREG = 1"
    | otherwise = a

  aluOut = undefinedIfInvalid aluOut0
  coutIn = undefinedIfInvalid coutIn0
  carryCascOutIn = undefinedIfInvalid cout3

  multSignOutIn = if opModeZ == 0b100 then iMultSignIn else msb multMux

  -- OPMODE check of the UNISIM model, which makes the pattern detector
  -- outputs undefined for invalid OPMODEs. For ALUMODE[3:2] = 10, it keeps
  -- the result of the previous check, unless one of the control signals
  -- changed: the UNISIM models check intermediate combinations of the control
  -- signals within a time step, so the result is not known in that case.
  ctrl = aluModeMux ++# opModeMux ++# carryInSelMux
  opModeValid = case opModeCheck pRegUsed aluModeMux opModeMux carryInSelMux of
    Just valid -> valid
    Nothing
      | ctrl == sCtrl s -> sOpModeValid s
      | otherwise ->
          errorX "dsp48e1: OPMODE check result unknown, as ALUMODE[3:2] is 10"

  -- Pattern detector. The Verilog UNISIM model shifts in zeros for the
  -- rounding masks, the VHDL model the least significant bit of the inverted
  -- C input. The outputs are undefined when that matters.
  thePattern = case selPattern of
    PatternAttr -> patDetPattern
    PatternC -> cMux

  theMask :: Bool -> BitVector 48
  theMask vhdl
    | not usePatternDetect = maxBound
    | otherwise = case selMask of
        MaskAttr -> patDetMask
        MaskC -> cMux
        MaskRoundingMode1 -> (complement cMux `shiftL` 1) .|. fill 0b1
        MaskRoundingMode2 -> (complement cMux `shiftL` 2) .|. fill 0b11
   where
    fill bits
      | vhdl && not (testBit cMux 0) = bits
      | otherwise = 0

  -- The VHDL model also drives the outputs low when any bit of the ALU output
  -- is undefined, even if it's masked.
  matchWith :: (BitVector 48 -> BitVector 48) -> Bool
  matchWith f
    | anyUndefined aluOut =
        errorX "dsp48e1: pattern detector output undefined, as the ALU output is (partially) undefined"
    | vlog == vhdl = vlog
    | otherwise =
        errorX "dsp48e1: pattern detector output undefined (Verilog and VHDL UNISIM models disagree)"
   where
    vlog = (f (thePattern `xor` aluOut) .|. theMask False) == maxBound
    vhdl = (f (thePattern `xor` aluOut) .|. theMask True) == maxBound

  invalidOpMode :: Bool -> Bool
  invalidOpMode b
    | opModeValid = b
    | otherwise =
        errorX "dsp48e1: pattern detector output undefined, as the combination of OPMODE, ALUMODE and CARRYINSEL is invalid"

  pdet = invalidOpMode (matchWith complement)
  pdetb = invalidOpMode (matchWith id)

  pdetMux = if pRegUsed then sPdet1 s else pdet
  pdetbMux = if pRegUsed then sPdetb1 s else pdetb

  overflowO
    | usePatternDetect || pRegUsed = sPdet2 s && not (sPdet1 s) && not (sPdetb1 s)
    | otherwise = errorX "dsp48e1: overflow is undefined without pattern detector and output register"
  underflowO
    | usePatternDetect || pRegUsed = sPdetb2 s && not (sPdet1 s) && not (sPdetb1 s)
    | otherwise = errorX "dsp48e1: underflow is undefined without pattern detector and output register"

  -- Automatic reset condition. The UNISIM models treat an undefined condition
  -- as no reset, like in an HDL @if@ statement. But where the condition is
  -- undefined in this model, it might be defined in the UNISIM models, so the
  -- registers it resets become undefined.
  autoResetCond = case autoResetPatDet of
    NoAutoReset -> False
    AutoResetMatch -> sPdet1 s
    AutoResetNotMatch -> sPdet2 s && not (sPdet1 s)
  autoReset = definitelyTrue autoResetCond
  noAutoReset = definitelyFalse autoResetCond

  -- The VHDL UNISIM model drives the carry outputs low while the automatic
  -- reset condition holds, the Verilog model doesn't.
  autoResetCarry :: NFDataX a => a -> a
  autoResetCarry a
    | noAutoReset = a
    | otherwise =
        deepErrorX "dsp48e1: carry outputs undefined during automatic reset (Verilog and VHDL UNISIM models disagree)"

  o = Outputs
    { oAcOut = acOutV
    , oBcOut = bcOutV
    , oCarryCascOut =
        autoResetCarry (if pRegUsed then sCarryCascOut s else carryCascOutIn)
    , oCarryOut =
        autoResetCarry $
          markUndefined simdUndefined (if pRegUsed then sCout s else coutIn)
    , oMultSignOut = if pRegUsed then sMultSign s else multSignOutIn
    , oOverflow = overflowO
    , oP = if pRegUsed then sP s else aluOut
    , oPatternBDetect = pdetbMux
    , oPatternDetect = pdetMux
    , oPcOut = if pRegUsed then sP s else aluOut
    , oUnderflow = underflowO
    }

  -- Register with synchronous reset and clock enable
  reg :: Bool -> Bool -> a -> a -> a -> a
  reg rst ce rstVal d q
    | rst = rstVal
    | ce = d
    | otherwise = q

  -- Output registers, reset by RSTP and the automatic reset
  regP :: NFDataX a => a -> a -> a -> a
  regP rstVal d q
    | iRstP || autoReset = rstVal
    | not noAutoReset =
        deepErrorX "dsp48e1: output register undefined, as the automatic reset condition is undefined"
    | iCeP = d
    | otherwise = q

  -- The Verilog UNISIM model registers 'multSignOutIn', the VHDL model the
  -- sign of the multiplier register. Undefined when they disagree.
  multSignReg
    | multSignOutIn == msb (sM s) = multSignOutIn
    | otherwise =
        errorX "dsp48e1: MULTSIGNOUT undefined (Verilog and VHDL UNISIM models disagree)"

  sNext = State
    { sA1 = if aReg == 0 then 0 else reg iRstA iCeA1 0 aDirect (sA1 s)
    , sA2 =
        if aReg == 0
        then 0
        else reg iRstA iCeA2 0 (if aReg == 2 then sA1 s else aDirect) (sA2 s)
    , sB1 = if bReg == 0 then 0 else reg iRstB iCeB1 0 bDirect (sB1 s)
    , sB2 =
        if bReg == 0
        then 0
        else reg iRstB iCeB2 0 (if bReg == 2 then sB1 s else bDirect) (sB2 s)
    , sC = reg iRstC iCeC 0 iC (sC s)
    , sD = reg iRstD iCeD 0 iD (sD s)
    , sAd = reg iRstD iCeAd 0 adAddSub (sAd s)
    , sInMode = reg iRstInMode iCeInMode 0 inModeIn (sInMode s)
    , sOpMode = reg iRstCtrl iCeCtrl 0 opModeIn (sOpMode s)
    , sCarryInSel = reg iRstCtrl iCeCtrl 0 iCarryInSel (sCarryInSel s)
    , sAluMode = reg iRstAluMode iCeAluMode 0 aluModeIn (sAluMode s)
    , sCarryIn = reg iRstAllCarryIn iCeCarryIn 0 carryInIn (sCarryIn s)
    , sMultCarryIn = reg iRstAllCarryIn iCeM 0 multCarryIn (sMultCarryIn s)
    , sM = reg iRstM iCeM 0 multO (sM s)
    , sPdet1 = regP False pdet (sPdet1 s)
    , sPdet2 = regP False (sPdet1 s) (sPdet2 s)
    , sPdetb1 = regP False pdetb (sPdetb1 s)
    , sPdetb2 = regP False (sPdetb1 s) (sPdetb2 s)
    , sCout = regP 0 coutIn (sCout s)
    , sCarryCascOut = regP 0 carryCascOutIn (sCarryCascOut s)
    , sMultSign = regP 0 multSignReg (sMultSign s)
    , sP = regP 0 aluOut (sP s)
    , sOpModeValid = opModeValid
    , sCtrl = ctrl
    }

-- | Haskell simulation model of the @DSP48E1@
dsp48e1Sim ::
  forall dom.
  Dsp48e1Config ->
  Dsp48e1Input dom ->
  Dsp48e1Output dom
dsp48e1Sim cfg inp = unbundleOutputs (go initState (bundleInputs inp))
 where
  -- The lazy pattern makes sure outputs can be produced before the inputs of
  -- the same cycle are known, which is necessary when the inputs depend on the
  -- (registered) outputs. The next state is forced before moving on to the
  -- next cycle to prevent building up thunks.
  go :: State -> Signal dom Inputs -> Signal dom Outputs
  go st ~(i :- is) = o :- (rnfX st1 `seq` go st1 is)
   where
    (st1, o) = step cfg st i

-- | HDL instantiation of the @DSP48E1@
dsp48e1Inst ::
  forall dom.
  KnownDomain dom =>
  Dsp48e1Config ->
  Clock dom ->
  Dsp48e1Input dom ->
  Dsp48e1Output dom
dsp48e1Inst Dsp48e1Config{..} clk Dsp48e1Input{..} = Dsp48e1Output
  { acOut = unPort acOutP
  , bcOut = unPort bcOutP
  , carryCascOut = unPort carryCascOutP
  , carryOut = unPort carryOutP
  , multSignOut = unPort multSignOutP
  , overflow = bitToBool <$> unPort overflowP
  , dataP = unPort pP
  , patternBDetect = bitToBool <$> unPort patternBDetectP
  , patternDetect = bitToBool <$> unPort patternDetectP
  , pcOut = unPort pcOutP
  , underflow = bitToBool <$> unPort underflowP
  }
 where
  ( acOutP, bcOutP, carryCascOutP, carryOutP, multSignOutP, overflowP
   , pP, patternBDetectP, patternDetectP, pcOutP, underflowP ) = ports

  ports ::
    ( Port "ACOUT" dom (BitVector 30)
    , Port "BCOUT" dom (BitVector 18)
    , Port "CARRYCASCOUT" dom Bit
    , Port "CARRYOUT" dom (BitVector 4)
    , Port "MULTSIGNOUT" dom Bit
    , Port "OVERFLOW" dom Bit
    , Port "P" dom (BitVector 48)
    , Port "PATTERNBDETECT" dom Bit
    , Port "PATTERNDETECT" dom Bit
    , Port "PCOUT" dom (BitVector 48)
    , Port "UNDERFLOW" dom Bit
    )
  ports =
    inst
      (instConfig "DSP48E1")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all" }

      (Param @"ACASCREG" @Integer (toInteger aCascReg))
      (Param @"ADREG" @Integer (toInteger adReg))
      (Param @"ALUMODEREG" @Integer (toInteger aluModeReg))
      (Param @"AREG" @Integer (toInteger aReg))
      (Param @"AUTORESET_PATDET" @String (case autoResetPatDet of
        { NoAutoReset -> "NO_RESET"
        ; AutoResetMatch -> "RESET_MATCH"
        ; AutoResetNotMatch -> "RESET_NOT_MATCH" }))
      (Param @"A_INPUT" @String (inputSourceParam aInput))
      (Param @"BCASCREG" @Integer (toInteger bCascReg))
      (Param @"BREG" @Integer (toInteger bReg))
      (Param @"B_INPUT" @String (inputSourceParam bInput))
      (Param @"CARRYINREG" @Integer (toInteger carryInReg))
      (Param @"CARRYINSELREG" @Integer (toInteger carryInSelReg))
      (Param @"CREG" @Integer (toInteger cReg))
      (Param @"DREG" @Integer (toInteger dReg))
      (Param @"INMODEREG" @Integer (toInteger inModeReg))
      (Param @"IS_ALUMODE_INVERTED" @(BitVector 4) isAluModeInverted)
      (Param @"IS_CARRYIN_INVERTED" @Bit (boolToBit isCarryInInverted))
      (Param @"IS_CLK_INVERTED" @Bit isClkInverted)
      (Param @"IS_INMODE_INVERTED" @(BitVector 5) isInModeInverted)
      (Param @"IS_OPMODE_INVERTED" @(BitVector 7) isOpModeInverted)
      (BitVectorParam @"MASK" patDetMask)
      (Param @"MREG" @Integer (toInteger mReg))
      (Param @"OPMODEREG" @Integer (toInteger opModeReg))
      (BitVectorParam @"PATTERN" patDetPattern)
      (Param @"PREG" @Integer (toInteger pReg))
      (Param @"SEL_MASK" @String (case selMask of
        { MaskAttr -> "MASK"
        ; MaskC -> "C"
        ; MaskRoundingMode1 -> "ROUNDING_MODE1"
        ; MaskRoundingMode2 -> "ROUNDING_MODE2" }))
      (Param @"SEL_PATTERN" @String (case selPattern of
        { PatternAttr -> "PATTERN"
        ; PatternC -> "C" }))
      (BoolParam @"USE_DPORT" useDPort)
      (Param @"USE_MULT" @String (case useMult of
        { MultMultiply -> "MULTIPLY"
        ; MultDynamic -> "DYNAMIC"
        ; MultNone -> "NONE" }))
      (Param @"USE_PATTERN_DETECT" @String
        (if usePatternDetect then "PATDET" else "NO_PATDET"))
      (Param @"USE_SIMD" @String (case useSimd of
        { One48 -> "ONE48"
        ; Two24 -> "TWO24"
        ; Four12 -> "FOUR12" }))

      (Port @"A" dataA)
      (Port @"ACIN" acIn)
      (Port @"ALUMODE" aluMode)
      (Port @"B" dataB)
      (Port @"BCIN" bcIn)
      (Port @"C" dataC)
      (Port @"CARRYCASCIN" carryCascIn)
      (Port @"CARRYIN" carryIn)
      (Port @"CARRYINSEL" carryInSel)
      (Port @"CEA1" (boolToBit <$> ceA1))
      (Port @"CEA2" (boolToBit <$> ceA2))
      (Port @"CEAD" (boolToBit <$> ceAd))
      (Port @"CEALUMODE" (boolToBit <$> ceAluMode))
      (Port @"CEB1" (boolToBit <$> ceB1))
      (Port @"CEB2" (boolToBit <$> ceB2))
      (Port @"CEC" (boolToBit <$> ceC))
      (Port @"CECARRYIN" (boolToBit <$> ceCarryIn))
      (Port @"CECTRL" (boolToBit <$> ceCtrl))
      (Port @"CED" (boolToBit <$> ceD))
      (Port @"CEINMODE" (boolToBit <$> ceInMode))
      (Port @"CEM" (boolToBit <$> ceM))
      (Port @"CEP" (boolToBit <$> ceP))
      (ClockPort @"CLK" clk)
      (Port @"D" dataD)
      (Port @"INMODE" inMode)
      (Port @"MULTSIGNIN" multSignIn)
      (Port @"OPMODE" opMode)
      (Port @"PCIN" pcIn)
      (Port @"RSTA" (boolToBit <$> rstA))
      (Port @"RSTALLCARRYIN" (boolToBit <$> rstAllCarryIn))
      (Port @"RSTALUMODE" (boolToBit <$> rstAluMode))
      (Port @"RSTB" (boolToBit <$> rstB))
      (Port @"RSTC" (boolToBit <$> rstC))
      (Port @"RSTCTRL" (boolToBit <$> rstCtrl))
      (Port @"RSTD" (boolToBit <$> rstD))
      (Port @"RSTINMODE" (boolToBit <$> rstInMode))
      (Port @"RSTM" (boolToBit <$> rstM))
      (Port @"RSTP" (boolToBit <$> rstP))

  isClkInverted :: Bit
  isClkInverted = case activeEdge @dom of
    SRising -> 0
    SFalling -> 1

  inputSourceParam :: InputSource -> String
  inputSourceParam Direct = "DIRECT"
  inputSourceParam Cascade = "CASCADE"

-- | See "Clash.Cores.Xilinx.Unisim.Dsp48e1"
dsp48e1 ::
  forall dom.
  (KnownDomain dom, HasCallStack) =>
  Dsp48e1Config ->
  Clock dom ->
  Dsp48e1Input dom ->
  Dsp48e1Output dom
dsp48e1 cfg clk inp
  | Just err <- configError cfg = clashCompileError ("dsp48e1: " <> err)
  | clashSimulation = dsp48e1Sim cfg inp
  | otherwise = dsp48e1Inst cfg clk inp
{-# INLINE dsp48e1 #-}

-- | See "Clash.Cores.Xilinx.Unisim.Dsp48e1"
multiplyAdd ::
  forall dom n.
  (KnownDomain dom, HasCallStack) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n (Signed 25) ->
  DSignal dom n (Signed 18) ->
  DSignal dom n (Signed 48) ->
  DSignal dom (n + 2) (Signed 48)
multiplyAdd clk ena a b c =
  D.unsafeFromSignal (unpack <$> dataP (dsp48e1 cfg clk inputs))
 where
  cfg = defDsp48e1Config
    { mReg = 0
    , inModeReg = 0
    , opModeReg = 0
    , aluModeReg = 0
    , carryInReg = 0
    , carryInSelReg = 0
    }
  en = fromEnable ena
  inputs = (enableAll en defDsp48e1Input)
    { dataA = pack . resize <$> D.toSignal a
    , dataB = pack <$> D.toSignal b
    , dataC = pack <$> D.toSignal c
    , opMode = pure 0b0110101 -- P = C + M
    }
{-# INLINE multiplyAdd #-}

-- | See "Clash.Cores.Xilinx.Unisim.Dsp48e1"
multiplyAccumulate ::
  forall dom n.
  (KnownDomain dom, HasCallStack) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n Bool ->
  DSignal dom n (Signed 25) ->
  DSignal dom n (Signed 18) ->
  DSignal dom (n + 3) (Signed 48)
multiplyAccumulate clk ena start a b =
  D.unsafeFromSignal (unpack <$> dataP (dsp48e1 cfg clk inputs))
 where
  cfg = defDsp48e1Config
    { inModeReg = 0
    , aluModeReg = 0
    , carryInReg = 0
    , carryInSelReg = 0
    }
  en = fromEnable ena
  inputs = (enableAll en defDsp48e1Input)
    { dataA = pack . resize <$> D.toSignal a
    , dataB = pack <$> D.toSignal b
      -- The product arrives at the ALU two cycles after the inputs, the OPMODE
      -- register delays it by one cycle.
    , opMode =
        mux (delay clk ena False (D.toSignal start))
          (pure 0b0000101) -- P = M
          (pure 0b0100101) -- P = P + M
    }
{-# INLINE multiplyAccumulate #-}

-- | Drive all clock enables with the same signal
enableAll :: Signal dom Bool -> Dsp48e1Input dom -> Dsp48e1Input dom
enableAll en inp = inp
  { ceA1 = en, ceA2 = en, ceAd = en, ceAluMode = en, ceB1 = en, ceB2 = en
  , ceC = en, ceCarryIn = en, ceCtrl = en, ceD = en, ceInMode = en, ceM = en
  , ceP = en
  }
