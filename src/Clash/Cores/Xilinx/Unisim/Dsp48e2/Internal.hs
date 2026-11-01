{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation model and HDL instantiation of the @DSP48E2@ primitive. See
"Clash.Cores.Xilinx.Unisim.Dsp48e2" for documentation.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Unisim.Dsp48e2.Internal where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))
import Clash.Sized.Internal.BitVector (BitVector(BV))

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

import qualified Clash.Signal.Delayed as D


import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | Source of the A or B input registers (attributes @A_INPUT@ and @B_INPUT@)
data InputSource
  = Direct
  -- ^ @DIRECT@: use the @A@ ('dataA') or @B@ ('dataB') input
  | Cascade
  -- ^ @CASCADE@: use the @ACIN@ ('acIn') or @BCIN@ ('bcIn') cascade input
  deriving (Show, Eq, Generic, NFDataX)

-- | Selects the input to the 27-bit side of the multiplier (attribute @AMULTSEL@)
data AMultSel
  = AMultA
  -- ^ @A@: the A input (after the @INMODE@ A1\/A2 selection)
  | AMultAD
  -- ^ @AD@: the pre-adder output
  deriving (Show, Eq, Generic, NFDataX)

-- | Selects the input to the 18-bit side of the multiplier (attribute @BMULTSEL@)
data BMultSel
  = BMultB
  -- ^ @B@: the B input (after the @INMODE@ B1\/B2 selection)
  | BMultAD
  -- ^ @AD@: the 18 least significant bits of the pre-adder output
  deriving (Show, Eq, Generic, NFDataX)

-- | Selects the input of the pre-adder that is added to, or subtracted from, D
-- (attribute @PREADDINSEL@)
data PreAddInSel
  = PreAddInA
  -- ^ @A@: the 27 least significant bits of the A input
  | PreAddInB
  -- ^ @B@: the sign-extended B input
  deriving (Show, Eq, Generic, NFDataX)

-- | Multiplier usage (attribute @USE_MULT@)
data UseMult
  = MultMultiply
  -- ^ @MULTIPLY@: the multiplier is used
  | MultDynamic
  -- ^ @DYNAMIC@: the ALU dynamically switches between the multiplier and the
  -- A:B concatenation
  | MultNone
  -- ^ @NONE@: the multiplier is not used, which saves power. Requires
  -- @'mReg' = 0@.
  deriving (Show, Eq, Generic, NFDataX)

-- | Single instruction, multiple data mode of the ALU (attribute @USE_SIMD@)
data UseSimd
  = One48
  -- ^ @ONE48@: one 48-bit ALU
  | Two24
  -- ^ @TWO24@: two independent 24-bit ALUs
  | Four12
  -- ^ @FOUR12@: four independent 12-bit ALUs
  deriving (Show, Eq, Generic, NFDataX)

-- | Wide XOR mode (attribute @XORSIMD@). The wide XOR operates on the bitwise
-- XOR of the X, Y and Z multiplexer outputs (with Z inverted when
-- @ALUMODE[0]@ is set); see UG579 for the bit ranges.
data XorSimd
  = Xor24_48_96
  -- ^ @XOR24_48_96@: 'xorOut' bits 0, 2, 4 and 6 are XORs over 12 bits, bits 1
  -- and 5 over 24 bits, and bit 3 over all 48 bits. Bit 7 is the XOR over bits
  -- 42 to 47.
  | Xor12
  -- ^ @XOR12@: each 'xorOut' bit /i/ is the XOR over bits /6i/ to /6i + 5/
  deriving (Show, Eq, Generic, NFDataX)

-- | Selects the pattern of the pattern detector (attribute @SEL_PATTERN@)
data SelPattern
  = PatternAttr
  -- ^ @PATTERN@: use 'patDetPattern'
  | PatternC
  -- ^ @C@: use the (registered or direct) C input
  deriving (Show, Eq, Generic, NFDataX)

-- | Selects the mask of the pattern detector (attribute @SEL_MASK@)
data SelMask
  = MaskAttr
  -- ^ @MASK@: use 'patDetMask'
  | MaskC
  -- ^ @C@: use the (registered or direct) C input
  | MaskRoundingMode1
  -- ^ @ROUNDING_MODE1@: use the inverted C input, shifted left by one
  | MaskRoundingMode2
  -- ^ @ROUNDING_MODE2@: use the inverted C input, shifted left by two
  deriving (Show, Eq, Generic, NFDataX)

-- | Automatically reset the P register based on the pattern detector
-- (attribute @AUTORESET_PATDET@)
data AutoResetPatDet
  = NoAutoReset
  -- ^ @NO_RESET@
  | AutoResetMatch
  -- ^ @RESET_MATCH@: reset the P register in the cycle after a pattern match
  | AutoResetNotMatch
  -- ^ @RESET_NOT_MATCH@: reset the P register in the cycle after a pattern
  -- match was followed by a mismatch
  deriving (Show, Eq, Generic, NFDataX)

-- | Condition under which 'autoResetPatDet' resets the P register (attribute
-- @AUTORESET_PRIORITY@)
data AutoResetPriority
  = AutoResetPriorityReset
  -- ^ @RESET@: regardless of @CEP@
  | AutoResetPriorityCep
  -- ^ @CEP@: only when @CEP@ ('ceP') is asserted
  deriving (Show, Eq, Generic, NFDataX)

-- | Static configuration of a @DSP48E2@. The field names follow the attribute
-- names of the primitive; see UG579 for their meaning. Use 'defDsp48e2Config'
-- to start from the primitive's defaults.
--
-- Pipeline register attributes are 'Index'es, so they can be written like the
-- attributes: @aReg = 2@. The combinations of 'aReg' and 'aCascReg' (and of
-- 'bReg' and 'bCascReg') must be one of (0, 0), (1, 1), (2, 1) or (2, 2), and
-- @'useMult' = 'MultNone'@ requires @'mReg' = 0@. Other combinations are
-- rejected by 'Clash.Cores.Xilinx.Unisim.Dsp48e2.dsp48e2', just like the UNISIM
-- simulation model rejects them.
--
-- @IS_CLK_INVERTED@ is not part of the configuration: it is derived from the
-- active edge of the clock domain.
data Dsp48e2Config = Dsp48e2Config
  { aInput :: InputSource
    -- ^ @A_INPUT@
  , bInput :: InputSource
    -- ^ @B_INPUT@
  , aMultSel :: AMultSel
    -- ^ @AMULTSEL@
  , bMultSel :: BMultSel
    -- ^ @BMULTSEL@
  , preAddInSel :: PreAddInSel
    -- ^ @PREADDINSEL@
  , useMult :: UseMult
    -- ^ @USE_MULT@
  , useSimd :: UseSimd
    -- ^ @USE_SIMD@
  , useWideXor :: Bool
    -- ^ @USE_WIDEXOR@. When disabled, 'xorOut' is zero.
  , xorSimd :: XorSimd
    -- ^ @XORSIMD@

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
    -- ^ @DREG@. Ignored (no register) when the pre-adder is unused, i.e. when
    -- @'aMultSel' = 'AMultA'@ and @'bMultSel' = 'BMultB'@, or when
    -- @'useMult' = 'MultNone'@.
  , adReg :: Index 2
    -- ^ @ADREG@: pre-adder output register. Ignored in the same situations as
    -- 'dReg'.
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
    -- ^ @USE_PATTERN_DETECT@: @PATDET@ ('True') or @NO_PATDET@ ('False'). When
    -- disabled, 'overflow' and 'underflow' are undefined.
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
  , autoResetPriority :: AutoResetPriority
    -- ^ @AUTORESET_PRIORITY@
  , rnd :: BitVector 48
    -- ^ @RND@: rounding constant, selected by @OPMODE[8:7] = 10@

  , isAluModeInverted :: BitVector 4
    -- ^ @IS_ALUMODE_INVERTED@
  , isCarryInInverted :: Bool
    -- ^ @IS_CARRYIN_INVERTED@
  , isInModeInverted :: BitVector 5
    -- ^ @IS_INMODE_INVERTED@
  , isOpModeInverted :: BitVector 9
    -- ^ @IS_OPMODE_INVERTED@
  , isRstAllCarryInInverted :: Bool
    -- ^ @IS_RSTALLCARRYIN_INVERTED@
  , isRstAluModeInverted :: Bool
    -- ^ @IS_RSTALUMODE_INVERTED@
  , isRstAInverted :: Bool
    -- ^ @IS_RSTA_INVERTED@
  , isRstBInverted :: Bool
    -- ^ @IS_RSTB_INVERTED@
  , isRstCtrlInverted :: Bool
    -- ^ @IS_RSTCTRL_INVERTED@
  , isRstCInverted :: Bool
    -- ^ @IS_RSTC_INVERTED@
  , isRstDInverted :: Bool
    -- ^ @IS_RSTD_INVERTED@
  , isRstInModeInverted :: Bool
    -- ^ @IS_RSTINMODE_INVERTED@
  , isRstMInverted :: Bool
    -- ^ @IS_RSTM_INVERTED@
  , isRstPInverted :: Bool
    -- ^ @IS_RSTP_INVERTED@
  }
  deriving (Show, Eq, Generic)

-- | The default attributes of the primitive, as documented in UG974: all
-- pipeline registers enabled (one A and one B register), multiplier used,
-- pre-adder unused, no SIMD, no wide XOR, no pattern detection, no inversions.
defDsp48e2Config :: Dsp48e2Config
defDsp48e2Config = Dsp48e2Config
  { aInput = Direct
  , bInput = Direct
  , aMultSel = AMultA
  , bMultSel = BMultB
  , preAddInSel = PreAddInA
  , useMult = MultMultiply
  , useSimd = One48
  , useWideXor = False
  , xorSimd = Xor24_48_96
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
  , autoResetPriority = AutoResetPriorityReset
  , rnd = 0
  , isAluModeInverted = 0
  , isCarryInInverted = False
  , isInModeInverted = 0
  , isOpModeInverted = 0
  , isRstAllCarryInInverted = False
  , isRstAluModeInverted = False
  , isRstAInverted = False
  , isRstBInverted = False
  , isRstCtrlInverted = False
  , isRstCInverted = False
  , isRstDInverted = False
  , isRstInModeInverted = False
  , isRstMInverted = False
  , isRstPInverted = False
  }

-- | Check the attribute combinations the UNISIM model rejects
configError :: Dsp48e2Config -> Maybe String
configError Dsp48e2Config{..}
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

-- | Input ports of a @DSP48E2@. The field names follow the port names of the
-- primitive, except for the data inputs @A@, @B@, @C@ and @D@, which are
-- called 'dataA', 'dataB', 'dataC' and 'dataD'. See UG579 for their meaning.
-- Use 'defDsp48e2Input' to tie off the ports you don't need.
--
-- The values are the values on the pins: the inversion attributes in
-- 'Dsp48e2Config' (e.g. 'isOpModeInverted') are applied by the primitive.
--
-- All resets are synchronous and active high (unless inverted by the
-- configuration).
data Dsp48e2Input dom = Dsp48e2Input
  { dataA :: Signal dom (BitVector 30)
    -- ^ @A@: 30-bit data input. Its 27 least significant bits are the
    -- multiplier\/pre-adder input, all 30 bits form the upper part of the
    -- A:B concatenation.
  , acIn :: Signal dom (BitVector 30)
    -- ^ @ACIN@: A cascade input, must come from 'acOut' of another @DSP48E2@
  , aluMode :: Signal dom (BitVector 4)
    -- ^ @ALUMODE@: ALU function select
  , dataB :: Signal dom (BitVector 18)
    -- ^ @B@: 18-bit data input
  , bcIn :: Signal dom (BitVector 18)
    -- ^ @BCIN@: B cascade input, must come from 'bcOut' of another @DSP48E2@
  , dataC :: Signal dom (BitVector 48)
    -- ^ @C@: 48-bit data input
  , carryCascIn :: Signal dom Bit
    -- ^ @CARRYCASCIN@: carry cascade input, must come from 'carryCascOut' of
    -- another @DSP48E2@
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
  , dataD :: Signal dom (BitVector 27)
    -- ^ @D@: 27-bit pre-adder input
  , inMode :: Signal dom (BitVector 5)
    -- ^ @INMODE@: selects the pre-adder and multiplier inputs
  , multSignIn :: Signal dom Bit
    -- ^ @MULTSIGNIN@: multiplier sign cascade input, must come from
    -- 'multSignOut' of another @DSP48E2@
  , opMode :: Signal dom (BitVector 9)
    -- ^ @OPMODE@: selects the W, X, Y and Z multiplexer inputs of the ALU
  , pcIn :: Signal dom (BitVector 48)
    -- ^ @PCIN@: P cascade input, must come from 'pcOut' of another @DSP48E2@
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
-- enables asserted, and all resets deasserted. Note that the resets are
-- deasserted by driving them low, so they need to be inverted (asserted)
-- manually when a reset inversion attribute is set.
defDsp48e2Input :: Dsp48e2Input dom
defDsp48e2Input = Dsp48e2Input
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

-- | Output ports of a @DSP48E2@. The field names follow the port names of the
-- primitive, except for the data output @P@, which is called 'dataP'.
data Dsp48e2Output dom = Dsp48e2Output
  { acOut :: Signal dom (BitVector 30)
    -- ^ @ACOUT@: A cascade output
  , bcOut :: Signal dom (BitVector 18)
    -- ^ @BCOUT@: B cascade output
  , carryCascOut :: Signal dom Bit
    -- ^ @CARRYCASCOUT@: carry cascade output
  , carryOut :: Signal dom (BitVector 4)
    -- ^ @CARRYOUT@: carry outputs of the (SIMD) ALU. Bit /i/ is the carry out
    -- of bit /12(i + 1) - 1/. The bits that are not meaningful in the
    -- selected SIMD mode, and bits 0 to 2 when multiplying or doing a logic
    -- operation, are undefined.
  , multSignOut :: Signal dom Bit
    -- ^ @MULTSIGNOUT@: multiplier sign cascade output
  , overflow :: Signal dom Bool
    -- ^ @OVERFLOW@: overflow detected by the pattern detector. Undefined when
    -- 'usePatternDetect' is disabled.
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
    -- when 'usePatternDetect' is disabled.
  , xorOut :: Signal dom (BitVector 8)
    -- ^ @XOROUT@: wide XOR outputs
  }

-- | Connect the cascade outputs (@ACOUT@, @BCOUT@, @PCOUT@, @CARRYCASCOUT@,
-- and @MULTSIGNOUT@) of a @DSP48E2@ to the cascade inputs of the next one.
-- Note that the next @DSP48E2@ only uses @ACIN@ and @BCIN@ when configured to
-- do so with 'aInput' and 'bInput', and only uses @PCIN@, @CARRYCASCIN@ and
-- @MULTSIGNIN@ when selected by @OPMODE@ and @CARRYINSEL@.
cascade :: Dsp48e2Output dom -> Dsp48e2Input dom -> Dsp48e2Input dom
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
  , iD :: BitVector 27
  , iInMode :: BitVector 5
  , iMultSignIn :: Bit
  , iOpMode :: BitVector 9
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
  , oXorOut :: BitVector 8
  }
  deriving (Show, Generic, NFDataX, BitPack)

-- | Combine the input signals into a signal of 'Inputs'
bundleInputs :: Dsp48e2Input dom -> Signal dom Inputs
bundleInputs Dsp48e2Input{..} =
  Inputs
    <$> dataA <*> acIn <*> aluMode <*> dataB <*> bcIn <*> dataC
    <*> carryCascIn <*> carryIn <*> carryInSel
    <*> ceA1 <*> ceA2 <*> ceAd <*> ceAluMode <*> ceB1 <*> ceB2 <*> ceC
    <*> ceCarryIn <*> ceCtrl <*> ceD <*> ceInMode <*> ceM <*> ceP
    <*> dataD <*> inMode <*> multSignIn <*> opMode <*> pcIn
    <*> rstA <*> rstAllCarryIn <*> rstAluMode <*> rstB <*> rstC <*> rstCtrl
    <*> rstD <*> rstInMode <*> rstM <*> rstP

-- | Split a signal of 'Inputs' into the input signals
unbundleInputs :: Signal dom Inputs -> Dsp48e2Input dom
unbundleInputs i = Dsp48e2Input
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
bundleOutputs :: Dsp48e2Output dom -> Signal dom Outputs
bundleOutputs Dsp48e2Output{..} =
  Outputs
    <$> acOut <*> bcOut <*> carryCascOut <*> carryOut <*> multSignOut
    <*> overflow <*> dataP <*> patternBDetect <*> patternDetect <*> pcOut
    <*> underflow <*> xorOut

-- | Split a signal of 'Outputs' into the output signals
unbundleOutputs :: Signal dom Outputs -> Dsp48e2Output dom
unbundleOutputs o = Dsp48e2Output
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
  , xorOut = oXorOut <$> o
  }

-- | Contents of all registers in the primitive. The names follow the UNISIM
-- simulation model.
data State = State
  { sA1 :: BitVector 30
  , sA2 :: BitVector 30
  , sB1 :: BitVector 18
  , sB2 :: BitVector 18
  , sC :: BitVector 48
  , sD :: BitVector 27
  , sAd :: BitVector 27
  , sInMode :: BitVector 5
  , sOpMode :: BitVector 9
  , sCarryInSel :: BitVector 3
  , sAluMode :: BitVector 4
  , sCarryIn :: Bit
  , sMultCarryIn :: Bit
  , sU :: BitVector 45
  , sV :: BitVector 45
  , sPdet1 :: Bool
  , sPdet2 :: Bool
  , sPdetb1 :: Bool
  , sPdetb2 :: Bool
  , sCout :: BitVector 4
  , sAluMode10 :: Bool
  , sMultSign :: Bit
  , sAluOut :: BitVector 48
  , sXor :: BitVector 8
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
  , sU = 0
  , sV = 0
  , sPdet1 = False
  , sPdet2 = False
  , sPdetb1 = False
  , sPdetb2 = False
  , sCout = 0
  , sAluMode10 = False
  , sMultSign = 0
  , sAluOut = 0
  , sXor = 0
  }

-- | Build a 'BitVector' from bits that are either defined ('Just') or
-- undefined ('Nothing'). The first element is the most significant bit.
partiallyDefined :: KnownNat n => Vec n (Maybe Bit) -> BitVector n
partiallyDefined bits =
  BV (fromInteger (toInteger mask)) (fromInteger (toInteger val))
 where
  mask = pack (map (maybe high (const low)) bits)
  val = pack (map (fromMaybe low) bits)

-- | One clock cycle of the @DSP48E2@: computes the outputs during the cycle,
-- and the register contents after the next active clock edge. This is a
-- translation of the UNISIM Verilog model (@DSP48E2.v@) of Vivado 2022.1.
step :: Dsp48e2Config -> State -> Inputs -> (State, Outputs)
step Dsp48e2Config{..} s Inputs{..} = (sNext, o)
 where
  -- Optional input inversion
  aluModeIn = iAluMode `xor` isAluModeInverted
  carryInIn = iCarryIn `xor` boolToBit isCarryInInverted
  inModeIn = iInMode `xor` isInModeInverted
  opModeIn = iOpMode `xor` isOpModeInverted
  rstAllCarryInIn = iRstAllCarryIn /= isRstAllCarryInInverted
  rstAluModeIn = iRstAluMode /= isRstAluModeInverted
  rstAIn = iRstA /= isRstAInverted
  rstBIn = iRstB /= isRstBInverted
  rstCtrlIn = iRstCtrl /= isRstCtrlInverted
  rstCIn = iRstC /= isRstCInverted
  rstDIn = iRstD /= isRstDInverted
  rstInModeIn = iRstInMode /= isRstInModeInverted
  rstMIn = iRstM /= isRstMInverted
  rstPIn = iRstP /= isRstPInverted

  -- Selects a register's output if it's enabled, its input otherwise
  regOr :: Index 2 -> a -> a -> a
  regOr r q d = if r == 1 then q else d

  pRegUsed = pReg == 1
  isFour12 = useSimd == Four12
  isOne48 = useSimd == One48

  -- Control registers
  opModeMux = regOr opModeReg (sOpMode s) opModeIn
  carryInSelMux = regOr carryInSelReg (sCarryInSel s) iCarryInSel
  aluModeData = regOr aluModeReg (sAluMode s) aluModeIn
  inModeMux = regOr inModeReg (sInMode s) inModeIn
  carryInMux = regOr carryInReg (sCarryIn s) carryInIn

  opModeX = slice d1 d0 opModeMux
  opModeY = slice d3 d2 opModeMux
  opModeZ = slice d6 d4 opModeMux
  opModeW = slice d8 d7 opModeMux
  opModeXY = slice d3 d0 opModeMux

  aluModeBit = testBit aluModeData
  inModeBit = testBit inModeMux

  -- A and B input registers
  aDirect = case aInput of
    Direct -> iA
    Cascade -> iAcIn
  aAlu = if aReg /= 0 then sA2 s else aDirect
  acOutV = if aCascReg == aReg then aAlu else sA1 s

  bDirect = case bInput of
    Direct -> iB
    Cascade -> iBcIn
  bAlu = if bReg /= 0 then sB2 s else bDirect
  bcOutV = if bCascReg == bReg then bAlu else sB1 s

  -- C input register
  cData = regOr cReg (sC s) iC

  -- Pre-adder
  preAddUnused =
    (aMultSel == AMultA && bMultSel == BMultB) || useMult == MultNone
  dRegUsed = not preAddUnused && dReg == 1
  adRegUsed = not preAddUnused && adReg == 1

  dData = if dRegUsed then sD s else iD

  a2a1 :: BitVector 27
  a2a1
    | preAddInSel == PreAddInA && inModeBit 1 = 0
    | inModeBit 0 = truncateB (sA1 s)
    | otherwise = truncateB aAlu

  b2b1 :: BitVector 18
  b2b1
    | preAddInSel == PreAddInB && inModeBit 1 = 0
    | inModeBit 4 = sB1 s
    | otherwise = bAlu

  preAddAB = case preAddInSel of
    PreAddInA -> a2a1
    PreAddInB -> signExtend b2b1

  dDataMux = if inModeBit 2 then dData else 0
  adIn = if inModeBit 3 then dDataMux - preAddAB else dDataMux + preAddAB
  adData = if adRegUsed then sAd s else adIn

  -- Multiplier. The UNISIM model splits the product in two "partial
  -- products" U and V, which add up to the product.
  aMultMux = case aMultSel of
    AMultA -> a2a1
    AMultAD -> adData
  bMultMux = case bMultSel of
    BMultB -> b2b1
    BMultAD -> truncateB adData

  mult :: BitVector 45
  mult = case useMult of
    MultNone -> 0
    _ -> pack ((unpack aMultMux :: Signed 27) `mul` (unpack bMultMux :: Signed 18))

  multLow = slice d43 d0 mult
  uIn = (1 :: BitVector 1) ++# (multLow .&. 0x55555555555)
  vIn = pack (complement (msb mult)) ++# (multLow .&. 0xAAAAAAAAAAA)

  uData = regOr mReg (sU s) uIn
  vData = regOr mReg (sV s) vIn
  uDataIn = if isOne48 then uData else 0x100000000000
  vDataIn = if isOne48 then vData else 0x100000000000

  cMult = complement (msb aMultMux `xor` msb bMultMux)
  multCarryInData = regOr mReg (sMultCarryIn s) cMult

  -- ALU input multiplexers
  pFdbk = sAluOut s

  shiftR17 :: BitVector 48 -> BitVector 48
  shiftR17 x = pack ((unpack x :: Signed 48) `shiftR` 17)

  wMux = case opModeW of
    0 -> 0
    1 -> pFdbk
    2 -> rnd
    _ -> cData

  xMacCascd :: BitVector 48
  xMacCascd
    | opModeZ == 0b100 = zeroExtend (pack iMultSignIn ++# (0 :: BitVector 1))
    | otherwise = 0

  xMux = case opModeX of
    0 -> xMacCascd
    1 -> signExtend uDataIn
    2 -> pFdbk
    _ -> aAlu ++# bAlu

  yMux = case opModeY of
    0 -> 0
    1 -> zeroExtend vDataIn
    2 -> maxBound
    _ -> cData

  zMux = case opModeZ of
    0 -> 0
    1 -> iPcIn
    2 -> pFdbk
    3 -> cData
    4 -> pFdbk
    5 -> shiftR17 iPcIn
    _ -> shiftR17 pFdbk

  -- Carry input
  ccoutIn = boolToBit (sAluMode10 s) `xor` (sCout s ! (3 :: Int))

  cinB = case carryInSelMux of
    0 -> complement carryInMux
    1 -> msb iPcIn
    2 -> complement iCarryCascIn
    3 -> complement (msb iPcIn)
    4 -> complement ccoutIn
    5 -> msb pFdbk
    6 -> complement multCarryInData
    _ -> complement (msb pFdbk)

  cin
    | aluModeBit 3 || aluModeBit 2 = 0
    | otherwise = complement cinB

  -- ALU: three input carry-save adder (X, Y, Z), followed by a full adder with
  -- W, followed by the (SIMD) final adder.
  zOptInv = if aluModeBit 0 then complement zMux else zMux
  co = (xMux .&. yMux) .|. (zOptInv .&. yMux) .|. (xMux .&. zOptInv)
  sumXyz = zOptInv `xor` xMux `xor` yMux

  coMux = if aluModeBit 2 then 0 else co
  sMux = if aluModeBit 3 then co else sumXyz

  simdCarryMask :: BitVector 48
  simdCarryMask = complement $
        (if isFour12 then bit 35 else 0)
    .|. (if not isOne48 then bit 23 else 0)
    .|. (if isFour12 then bit 11 else 0)

  coMux4Simd = coMux .&. simdCarryMask
  coShifted = coMux4Simd `shiftL` 1

  sMuxW = sMux `xor` coShifted `xor` wMux
  coMuxW = (sMux .&. coShifted) .|. (wMux .&. coShifted) .|. (sMux .&. wMux)

  aluMode10 = aluModeBit 0 && aluModeBit 1

  aInt :: BitVector 49
  aInt = coMuxW ++# pack cin

  bitB :: KnownNat n => BitVector n -> Int -> Bit
  bitB = (!)

  s0 :: BitVector 13
  s0 = zeroExtend (slice d11 d0 aInt) + zeroExtend (slice d11 d0 sMuxW)
  cout0 = boolToBit aluMode10 `xor` bitB aInt 12 `xor` bitB s0 12 `xor` bitB coMux 11

  intc1 = not isFour12 && bitToBool (bitB s0 12)
  co12Lsb = not isFour12 && bitToBool (bitB aInt 12)
  s1 :: BitVector 13
  s1 =
      zeroExtend (slice d23 d13 aInt ++# pack (boolToBit co12Lsb))
    + zeroExtend (slice d23 d12 sMuxW)
    + boolToBV intc1
  cout1 = boolToBit aluMode10 `xor` bitB aInt 24 `xor` bitB s1 12 `xor` bitB coMux 23

  intc2 = isOne48 && bitToBool (bitB s1 12)
  co24Lsb = isOne48 && bitToBool (bitB aInt 24)
  s2 :: BitVector 13
  s2 =
      zeroExtend (slice d35 d25 aInt ++# pack (boolToBit co24Lsb))
    + zeroExtend (slice d35 d24 sMuxW)
    + boolToBV intc2
  cout2 = boolToBit aluMode10 `xor` bitB aInt 36 `xor` bitB s2 12 `xor` bitB coMux 35

  intc3 = not isFour12 && bitToBool (bitB s2 12)
  co36Lsb = not isFour12 && bitToBool (bitB aInt 36)
  s3 :: BitVector 14
  s3 =
      zeroExtend (slice d48 d37 aInt ++# pack (boolToBit co36Lsb))
    + zeroExtend (pack (msb coMux4Simd) ++# slice d47 d36 sMuxW)
    + boolToBV intc3
  cout3 = boolToBit aluMode10 `xor` bitB s3 12
  cout4 = bitB s3 13

  -- Wide XOR
  xor6 :: Int -> Bit
  xor6 n
    | useWideXor = reduceXor (truncateB (sumXyz `shiftR` (6 * n)) :: BitVector 6)
    | otherwise = 0

  xor12 = map xor6 (iterateI (+1) 0) :: Vec 8 Bit
  xor24 = map (\(n :: Int) -> xor12 !! (2*n) `xor` xor12 !! (2*n + 1)) (iterateI (+1) 0) :: Vec 4 Bit
  xor48 = map (\(n :: Int) -> xor24 !! (2*n) `xor` xor24 !! (2*n + 1)) (iterateI (+1) 0) :: Vec 2 Bit
  xor96 = xor48 !! (0 :: Int) `xor` xor48 !! (1 :: Int)

  xorMxIn :: BitVector 8
  xorMxIn = pack $ reverse $ case xorSimd of
    Xor12 -> xor12
    Xor24_48_96 ->
         xor24 !! (0 :: Int) :> xor48 !! (0 :: Int)
      :> xor24 !! (1 :: Int) :> xor96
      :> xor24 !! (2 :: Int) :> xor48 !! (1 :: Int)
      :> xor24 !! (3 :: Int) :> xor12 !! (7 :: Int)
      :> Nil

  -- Carry outputs. The UNISIM model drives the carry outputs that aren't
  -- meaningful with X.
  multOrLogic = opModeXY == 0b0101 || slice d3 d2 aluModeData /= 0
  coutLow c = if multOrLogic then Nothing else Just c
  coutIn0 :: BitVector 4
  coutIn0 = partiallyDefined
    (  Just cout3
    :> (if isFour12 then coutLow cout2 else Nothing)
    :> (if not isOne48 then coutLow cout1 else Nothing)
    :> (if isFour12 then coutLow cout0 else Nothing)
    :> Nil )

  aluOutIn0 :: BitVector 48
  aluOutIn0 =
    (if aluModeBit 1 then complement else id)
      (  slice d11 d0 s3 ++# slice d11 d0 s2
     ++# slice d11 d0 s1 ++# slice d11 d0 s0 )

  -- Operating modes the UNISIM model warns about ("may show deviation between
  -- simulation results and hardware behavior"), plus using only one of the
  -- two partial products of the multiplier, plus using the P feedback without
  -- the P register (where the Verilog UNISIM model uses the unused P register,
  -- and the VHDL model creates a combinational loop). In these modes, the
  -- outputs of the ALU are undefined.
  invalidMode :: Maybe String
  invalidMode
    | (opModeX == 0b01) /= (opModeY == 0b01) =
        Just "OPMODE[3:0] selects only one of the two partial products of the multiplier"
    | opModeX == 0b11
    , useMult == MultMultiply
    , (aReg == 0 && bReg == 0 && mReg == 0)
        || (aReg == 0 && bReg == 0 && pReg == 0)
        || (mReg == 0 && pReg == 0) =
        Just "OPMODE[1:0] = 11 is invalid with USE_MULT = MULTIPLY when (A, B, and M) or (A, B, and P) or (M and P) are not registered"
    | opModeXY == 0b0101
    , useMult == MultNone || not isOne48 =
        Just "OPMODE[3:0] = 0101 is invalid with USE_MULT = NONE or USE_SIMD /= ONE48"
    | carryInSelMux == 0b010
    , opModeXY == 0b0101 =
        Just "CARRYINSEL = 010 is invalid when multiplying (OPMODE[3:0] = 0101)"
    | not pRegUsed
    , opModeW == 0b01 || opModeX == 0b10 || opModeZ `elem` [0b010, 0b100, 0b110, 0b111]
        || carryInSelMux `elem` [0b100, 0b101, 0b111] =
        Just "selecting the P feedback (OPMODE or CARRYINSEL) requires PREG = 1"
    | otherwise = Nothing

  undefinedIfInvalid :: NFDataX a => a -> a
  undefinedIfInvalid a = case invalidMode of
    Nothing -> a
    Just msg -> deepErrorX ("dsp48e2: " <> msg)

  aluOutIn = undefinedIfInvalid aluOutIn0
  coutIn = undefinedIfInvalid coutIn0
  multSignAluIn = undefinedIfInvalid cout4
  xorIn = undefinedIfInvalid xorMxIn

  -- Pattern detector
  thePattern = case selPattern of
    PatternAttr -> patDetPattern
    PatternC -> cData

  theMask
    | not usePatternDetect = maxBound
    | otherwise = case selMask of
        MaskAttr -> patDetMask
        MaskC -> cData
        MaskRoundingMode1 -> complement cData `shiftL` 1
        MaskRoundingMode2 -> complement cData `shiftL` 2

  pdet = (complement (thePattern `xor` aluOutIn) .|. theMask) == maxBound
  pdetb = ((thePattern `xor` aluOutIn) .|. theMask) == maxBound

  pdetMux = if pRegUsed then sPdet1 s else pdet
  pdetbMux = if pRegUsed then sPdetb1 s else pdetb
  overflowData = if pRegUsed then sPdet2 s else pdet
  underflowData = if pRegUsed then sPdetb2 s else pdetb

  autoResetPri = autoResetPriority == AutoResetPriorityReset || iCeP
  autoReset = case autoResetPatDet of
    NoAutoReset -> False
    AutoResetMatch -> autoResetPri && pdetMux
    AutoResetNotMatch -> autoResetPri && overflowData && not pdetMux

  patDetOnly :: String -> Bool -> Bool
  patDetOnly nm b
    | usePatternDetect = b
    | otherwise = errorX ("dsp48e2: " <> nm <> " is undefined when usePatternDetect is disabled")

  o = Outputs
    { oAcOut = acOutV
    , oBcOut = bcOutV
    , oCarryCascOut =
        if pRegUsed
        then ccoutIn
        else boolToBit aluMode10 `xor` (coutIn ! (3 :: Int))
    , oCarryOut = if pRegUsed then sCout s else coutIn
    , oMultSignOut = if pRegUsed then sMultSign s else multSignAluIn
    , oOverflow =
        patDetOnly "overflow" (not pdetMux && not pdetbMux && overflowData)
    , oP = if pRegUsed then sAluOut s else aluOutIn
    , oPatternBDetect = pdetbMux
    , oPatternDetect = pdetMux
    , oPcOut = if pRegUsed then sAluOut s else aluOutIn
    , oUnderflow =
        patDetOnly "underflow" (not pdetMux && not pdetbMux && underflowData)
    , oXorOut = if pRegUsed then sXor s else xorIn
    }

  -- Register with synchronous reset and clock enable
  reg :: Bool -> Bool -> a -> a -> a -> a
  reg rst ce rstVal d q
    | rst = rstVal
    | ce = d
    | otherwise = q

  rstPAll = rstPIn || autoReset
  ceP1 = iCeP && pRegUsed

  sNext = State
    { sA1 = reg (rstAIn || aReg == 0) iCeA1 0 aDirect (sA1 s)
    , sA2 = reg (rstAIn || aReg == 0) iCeA2 0
              (if aReg == 2 then sA1 s else aDirect) (sA2 s)
    , sB1 = reg (rstBIn || bReg == 0) iCeB1 0 bDirect (sB1 s)
    , sB2 = reg rstBIn iCeB2 0 (if bReg == 2 then sB1 s else bDirect) (sB2 s)
    , sC = reg (rstCIn || cReg == 0) iCeC 0 iC (sC s)
    , sD = reg (rstDIn || not dRegUsed) iCeD 0 iD (sD s)
    , sAd = reg rstDIn iCeAd 0 adIn (sAd s)
    , sInMode = reg (rstInModeIn || inModeReg == 0) iCeInMode 0 inModeIn (sInMode s)
    , sOpMode = reg rstCtrlIn iCeCtrl 0 opModeIn (sOpMode s)
    , sCarryInSel = reg rstCtrlIn iCeCtrl 0 iCarryInSel (sCarryInSel s)
    , sAluMode = reg rstAluModeIn iCeAluMode 0 aluModeIn (sAluMode s)
    , sCarryIn = reg rstAllCarryInIn iCeCarryIn 0 carryInIn (sCarryIn s)
    , sMultCarryIn = reg rstAllCarryInIn iCeM 0 cMult (sMultCarryIn s)
    , sU = reg (rstMIn || mReg == 0) iCeM 0 uIn (sU s)
    , sV = reg (rstMIn || mReg == 0) iCeM 0 vIn (sV s)
    , sPdet1 = reg rstPAll ceP1 False pdet (sPdet1 s)
    , sPdet2 = reg rstPAll ceP1 False (sPdet1 s) (sPdet2 s)
    , sPdetb1 = reg rstPAll ceP1 False pdetb (sPdetb1 s)
    , sPdetb2 = reg rstPAll ceP1 False (sPdetb1 s) (sPdetb2 s)
    , sCout = reg rstPAll ceP1 0 coutIn (sCout s)
    , sAluMode10 = reg rstPAll ceP1 False aluMode10 (sAluMode10 s)
    , sMultSign = reg rstPAll ceP1 0 multSignAluIn (sMultSign s)
    , sAluOut = reg rstPAll ceP1 0 aluOutIn (sAluOut s)
    , sXor = reg rstPAll ceP1 0 xorIn (sXor s)
    }

-- | Haskell simulation model of the @DSP48E2@
dsp48e2Sim ::
  forall dom.
  Dsp48e2Config ->
  Dsp48e2Input dom ->
  Dsp48e2Output dom
dsp48e2Sim cfg inp = unbundleOutputs (go initState (bundleInputs inp))
 where
  -- The lazy pattern makes sure outputs can be produced before the inputs of
  -- the same cycle are known, which is necessary when the inputs depend on the
  -- (registered) outputs. The next state is forced before moving on to the
  -- next cycle to prevent building up thunks.
  go :: State -> Signal dom Inputs -> Signal dom Outputs
  go st ~(i :- is) = o :- (rnfX st1 `seq` go st1 is)
   where
    (st1, o) = step cfg st i

-- | HDL instantiation of the @DSP48E2@
dsp48e2Inst ::
  forall dom.
  KnownDomain dom =>
  Dsp48e2Config ->
  Clock dom ->
  Dsp48e2Input dom ->
  Dsp48e2Output dom
dsp48e2Inst Dsp48e2Config{..} clk Dsp48e2Input{..} = Dsp48e2Output
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
  , xorOut = unPort xorOutP
  }
 where
  ( acOutP, bcOutP, carryCascOutP, carryOutP, multSignOutP, overflowP
   , pP, patternBDetectP, patternDetectP, pcOutP, underflowP, xorOutP ) = ports

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
    , Port "XOROUT" dom (BitVector 8)
    )
  ports =
    inst
      (instConfig "DSP48E2")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all" }

      (Param @"ACASCREG" @Integer (toInteger aCascReg))
      (Param @"ADREG" @Integer (toInteger adReg))
      (Param @"ALUMODEREG" @Integer (toInteger aluModeReg))
      (Param @"AMULTSEL" @String (case aMultSel of { AMultA -> "A"; AMultAD -> "AD" }))
      (Param @"AREG" @Integer (toInteger aReg))
      (Param @"AUTORESET_PATDET" @String (case autoResetPatDet of
        { NoAutoReset -> "NO_RESET"
        ; AutoResetMatch -> "RESET_MATCH"
        ; AutoResetNotMatch -> "RESET_NOT_MATCH" }))
      (Param @"AUTORESET_PRIORITY" @String (case autoResetPriority of
        { AutoResetPriorityReset -> "RESET"
        ; AutoResetPriorityCep -> "CEP" }))
      (Param @"A_INPUT" @String (inputSourceParam aInput))
      (Param @"BCASCREG" @Integer (toInteger bCascReg))
      (Param @"BMULTSEL" @String (case bMultSel of { BMultB -> "B"; BMultAD -> "AD" }))
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
      (Param @"IS_OPMODE_INVERTED" @(BitVector 9) isOpModeInverted)
      (Param @"IS_RSTALLCARRYIN_INVERTED" @Bit (boolToBit isRstAllCarryInInverted))
      (Param @"IS_RSTALUMODE_INVERTED" @Bit (boolToBit isRstAluModeInverted))
      (Param @"IS_RSTA_INVERTED" @Bit (boolToBit isRstAInverted))
      (Param @"IS_RSTB_INVERTED" @Bit (boolToBit isRstBInverted))
      (Param @"IS_RSTCTRL_INVERTED" @Bit (boolToBit isRstCtrlInverted))
      (Param @"IS_RSTC_INVERTED" @Bit (boolToBit isRstCInverted))
      (Param @"IS_RSTD_INVERTED" @Bit (boolToBit isRstDInverted))
      (Param @"IS_RSTINMODE_INVERTED" @Bit (boolToBit isRstInModeInverted))
      (Param @"IS_RSTM_INVERTED" @Bit (boolToBit isRstMInverted))
      (Param @"IS_RSTP_INVERTED" @Bit (boolToBit isRstPInverted))
      (Param @"MASK" @(BitVector 48) patDetMask)
      (Param @"MREG" @Integer (toInteger mReg))
      (Param @"OPMODEREG" @Integer (toInteger opModeReg))
      (Param @"PATTERN" @(BitVector 48) patDetPattern)
      (Param @"PREADDINSEL" @String (case preAddInSel of { PreAddInA -> "A"; PreAddInB -> "B" }))
      (Param @"PREG" @Integer (toInteger pReg))
      (Param @"RND" @(BitVector 48) rnd)
      (Param @"SEL_MASK" @String (case selMask of
        { MaskAttr -> "MASK"
        ; MaskC -> "C"
        ; MaskRoundingMode1 -> "ROUNDING_MODE1"
        ; MaskRoundingMode2 -> "ROUNDING_MODE2" }))
      (Param @"SEL_PATTERN" @String (case selPattern of
        { PatternAttr -> "PATTERN"
        ; PatternC -> "C" }))
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
      (Param @"USE_WIDEXOR" @String (if useWideXor then "TRUE" else "FALSE"))
      (Param @"XORSIMD" @String (case xorSimd of
        { Xor24_48_96 -> "XOR24_48_96"
        ; Xor12 -> "XOR12" }))

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

-- | See "Clash.Cores.Xilinx.Unisim.Dsp48e2"
dsp48e2 ::
  forall dom.
  (KnownDomain dom, HasCallStack) =>
  Dsp48e2Config ->
  Clock dom ->
  Dsp48e2Input dom ->
  Dsp48e2Output dom
dsp48e2 cfg clk inp
  | Just err <- configError cfg = clashCompileError ("dsp48e2: " <> err)
  | clashSimulation = dsp48e2Sim cfg inp
  | otherwise = dsp48e2Inst cfg clk inp
{-# INLINE dsp48e2 #-}

-- | See "Clash.Cores.Xilinx.Unisim.Dsp48e2"
multiplyAdd ::
  forall dom n.
  (KnownDomain dom, HasCallStack) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n (Signed 27) ->
  DSignal dom n (Signed 18) ->
  DSignal dom n (Signed 48) ->
  DSignal dom (n + 2) (Signed 48)
multiplyAdd clk ena a b c =
  D.unsafeFromSignal (unpack <$> dataP (dsp48e2 cfg clk inputs))
 where
  cfg = defDsp48e2Config
    { mReg = 0
    , inModeReg = 0
    , opModeReg = 0
    , aluModeReg = 0
    , carryInReg = 0
    , carryInSelReg = 0
    }
  en = fromEnable ena
  inputs = (enableAll en defDsp48e2Input)
    { dataA = pack . resize <$> D.toSignal a
    , dataB = pack <$> D.toSignal b
    , dataC = pack <$> D.toSignal c
    , opMode = pure 0b000110101 -- P = C + M
    }
{-# INLINE multiplyAdd #-}

-- | See "Clash.Cores.Xilinx.Unisim.Dsp48e2"
multiplyAccumulate ::
  forall dom n.
  (KnownDomain dom, HasCallStack) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n Bool ->
  DSignal dom n (Signed 27) ->
  DSignal dom n (Signed 18) ->
  DSignal dom (n + 3) (Signed 48)
multiplyAccumulate clk ena start a b =
  D.unsafeFromSignal (unpack <$> dataP (dsp48e2 cfg clk inputs))
 where
  cfg = defDsp48e2Config
    { inModeReg = 0
    , aluModeReg = 0
    , carryInReg = 0
    , carryInSelReg = 0
    }
  en = fromEnable ena
  inputs = (enableAll en defDsp48e2Input)
    { dataA = pack . resize <$> D.toSignal a
    , dataB = pack <$> D.toSignal b
      -- The product arrives at the ALU two cycles after the inputs, the OPMODE
      -- register delays it by one cycle.
    , opMode =
        mux (delay clk ena False (D.toSignal start))
          (pure 0b000000101) -- P = M
          (pure 0b000100101) -- P = P + M
    }
{-# INLINE multiplyAccumulate #-}

-- | Drive all clock enables with the same signal
enableAll :: Signal dom Bool -> Dsp48e2Input dom -> Dsp48e2Input dom
enableAll en inp = inp
  { ceA1 = en, ceA2 = en, ceAd = en, ceAluMode = en, ceB1 = en, ceB2 = en
  , ceC = en, ceCarryIn = en, ceCtrl = en, ceD = en, ceInMode = en, ceM = en
  , ceP = en
  }
