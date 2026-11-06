{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The DSP block @SB_MAC16@ of the iCE40 UltraPlus (and LM) devices: a 16x16
  multiplier, made of four 8x8 multipliers, followed by two 16-bit
  adder\/subtracters with accumulators, which can be combined into a single
  32-bit one. See the /DSP Function Usage Guide for iCE40 Devices/ (TN1295,
  FPGA-TN-02007) and the
  <http://www.latticesemi.com/~/media/LatticeSemi/Documents/TechnicalBriefs/SBTICETechnologyLibrary201504.pdf iCE Technology Library>.
  Note that, unlike the DSP blocks of the ECP5 and Xilinx devices, it has no
  pre-adder.

  'sbMac16' exposes the primitive as is: the parameters are set by a
  t'Mac16Config', and every input and output port is a field of 'Mac16Input' and
  'Mac16Output'. The simulation model follows the community simulation model of
  Yosys (@share\/yosys\/ice40\/cells_sim.v@), against which it is tested with
  Yosys 0.70 (@\/nix\/store\/cfry5mn7l5qk6cy730gspaa51fahc108-yosys-0.70@). In
  particular:

  * All registers start out undefined.

  * The resets ('irstTop', 'irstBot', 'orstTop' and 'orstBot') are
    asynchronous: they clear their registers immediately.

  * In 8x8 mode ('mode8x8'), the registers that are only needed by the 16x16
    multiplier (the pipeline registers of the cross products and of the 32-bit
    product) hold their values.

  The block is split into a top half (@TOP@, producing bits 31 to 16 of the
  output) and a bottom half (@BOT@, bits 15 to 0). Each half has an
  adder\/subtracter computing @upper + lower + carry@ when adding, or
  @upper - lower - carry@ when subtracting, where the upper input is either the
  half's output register (an accumulator) or the C (top) or D (bottom) input.

  The domain's active edge determines @NEG_TRIGGER@.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.LatticeSemi.Ice40.Dsp
  ( -- * Primitive
    sbMac16
  , Mac16Input (..)
  , defMac16Input
  , Mac16Output (..)

    -- * Convenience functions
  , multiply16

    -- * Configuration
  , Mac16Config (..)
  , defMac16Config
  , OutputSelect (..)
  , UpperInput (..)
  , TopLowerInput (..)
  , BotLowerInput (..)
  , TopCarrySelect (..)
  , BotCarrySelect (..)
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.LatticeSemi.Ice40.Internal (ice40Prim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (BitVectorParam (..), ClockPort (..), Param (..), Port (..), inst)

-- | What a half of the DSP block outputs (@TOPOUTPUT_SELECT@ and
-- @BOTOUTPUT_SELECT@)
data OutputSelect
  = OutputAddSub
  -- ^ The adder\/subtracter, unregistered (0)
  | OutputAddSubRegistered
  -- ^ The adder\/subtracter's output register, i.e. the accumulator (1)
  | OutputMult8x8
  -- ^ The 8x8 multiplier of the half: @A[15:8] * B[15:8]@ for the top half,
  -- @A[7:0] * B[7:0]@ for the bottom half (2)
  | OutputMult16x16
  -- ^ The 16x16 multiplier: its upper half for the top, its lower half for the
  -- bottom (3)
  deriving (Show, Eq, Generic, NFDataX)

-- | Upper input of an adder\/subtracter (@TOPADDSUB_UPPERINPUT@ and
-- @BOTADDSUB_UPPERINPUT@)
data UpperInput
  = UpperAccumulator
  -- ^ The half's output register (0)
  | UpperDirect
  -- ^ C for the top half, D for the bottom half (1)
  deriving (Show, Eq, Generic, NFDataX)

-- | Lower input of the top adder\/subtracter (@TOPADDSUB_LOWERINPUT@)
data TopLowerInput
  = TopLowerA
  -- ^ A (0)
  | TopLowerMult8x8
  -- ^ The top 8x8 multiplier, @A[15:8] * B[15:8]@ (1)
  | TopLowerMult16x16
  -- ^ The upper half of the 16x16 multiplier (2)
  | TopLowerSignExtension
  -- ^ The sign of the lower input of the bottom adder\/subtracter, to make a
  -- 32-bit adder out of the two halves (3)
  deriving (Show, Eq, Generic, NFDataX)

-- | Lower input of the bottom adder\/subtracter (@BOTADDSUB_LOWERINPUT@)
data BotLowerInput
  = BotLowerB
  -- ^ B (0)
  | BotLowerMult8x8
  -- ^ The bottom 8x8 multiplier, @A[7:0] * B[7:0]@ (1)
  | BotLowerMult16x16
  -- ^ The lower half of the 16x16 multiplier (2)
  | BotLowerSignExtIn
  -- ^ 'signExtIn', from the DSP block below (3)
  deriving (Show, Eq, Generic, NFDataX)

-- | Carry input of the top adder\/subtracter (@TOPADDSUB_CARRYSELECT@)
data TopCarrySelect
  = TopCarryZero
  -- ^ 0 (0)
  | TopCarryOne
  -- ^ 1 (1)
  | TopCarryBot
  -- ^ The carry out of the bottom adder\/subtracter (2)
  | TopCarryBotXorAddSub
  -- ^ The carry out of the bottom adder\/subtracter, inverted when the bottom
  -- subtracts (3)
  deriving (Show, Eq, Generic, NFDataX)

-- | Carry input of the bottom adder\/subtracter (@BOTADDSUB_CARRYSELECT@)
data BotCarrySelect
  = BotCarryZero
  -- ^ 0 (0)
  | BotCarryOne
  -- ^ 1 (1)
  | BotCarryAccumCi
  -- ^ 'accumCi', from the DSP block below (2)
  | BotCarryCi
  -- ^ 'ci', from the fabric (3)
  deriving (Show, Eq, Generic, NFDataX)

-- | Parameters of 'sbMac16'. The configuration must be a constant.
data Mac16Config = Mac16Config
  { cReg :: Bool
  -- ^ @C_REG@: register C
  , aReg :: Bool
  -- ^ @A_REG@: register A
  , bReg :: Bool
  -- ^ @B_REG@: register B
  , dReg :: Bool
  -- ^ @D_REG@: register D
  , top8x8MultReg :: Bool
  -- ^ @TOP_8x8_MULT_REG@: register the top 8x8 product
  , bot8x8MultReg :: Bool
  -- ^ @BOT_8x8_MULT_REG@: register the bottom 8x8 product
  , pipeline16x16MultReg1 :: Bool
  -- ^ @PIPELINE_16x16_MULT_REG1@: register the cross products of the 16x16
  -- multiplier
  , pipeline16x16MultReg2 :: Bool
  -- ^ @PIPELINE_16x16_MULT_REG2@: register the 32-bit product
  , topOutputSelect :: OutputSelect
  -- ^ @TOPOUTPUT_SELECT@
  , topAddSubLowerInput :: TopLowerInput
  -- ^ @TOPADDSUB_LOWERINPUT@
  , topAddSubUpperInput :: UpperInput
  -- ^ @TOPADDSUB_UPPERINPUT@
  , topAddSubCarrySelect :: TopCarrySelect
  -- ^ @TOPADDSUB_CARRYSELECT@
  , botOutputSelect :: OutputSelect
  -- ^ @BOTOUTPUT_SELECT@
  , botAddSubLowerInput :: BotLowerInput
  -- ^ @BOTADDSUB_LOWERINPUT@
  , botAddSubUpperInput :: UpperInput
  -- ^ @BOTADDSUB_UPPERINPUT@
  , botAddSubCarrySelect :: BotCarrySelect
  -- ^ @BOTADDSUB_CARRYSELECT@
  , mode8x8 :: Bool
  -- ^ @MODE_8x8@: use the block as two 8x8 multipliers, which saves power
  , aSigned :: Bool
  -- ^ @A_SIGNED@: A is signed
  , bSigned :: Bool
  -- ^ @B_SIGNED@: B is signed
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | The default values of the primitive's parameters: no registers, both
-- halves output their (unregistered) adder\/subtracter, which adds the lower
-- inputs A and B to the accumulators.
defMac16Config :: Mac16Config
defMac16Config =
  Mac16Config
    { cReg = False
    , aReg = False
    , bReg = False
    , dReg = False
    , top8x8MultReg = False
    , bot8x8MultReg = False
    , pipeline16x16MultReg1 = False
    , pipeline16x16MultReg2 = False
    , topOutputSelect = OutputAddSub
    , topAddSubLowerInput = TopLowerA
    , topAddSubUpperInput = UpperAccumulator
    , topAddSubCarrySelect = TopCarryZero
    , botOutputSelect = OutputAddSub
    , botAddSubLowerInput = BotLowerB
    , botAddSubUpperInput = UpperAccumulator
    , botAddSubCarrySelect = BotCarryZero
    , mode8x8 = False
    , aSigned = False
    , bSigned = False
    }

-- | Inputs of 'sbMac16'. The clock enable (@CE@) is the 'Enable' argument of
-- 'sbMac16'.
data Mac16Input dom = Mac16Input
  { inputA :: Signal dom (BitVector 16)
  -- ^ @A@
  , inputB :: Signal dom (BitVector 16)
  -- ^ @B@
  , inputC :: Signal dom (BitVector 16)
  -- ^ @C@
  , inputD :: Signal dom (BitVector 16)
  -- ^ @D@
  , aHold :: Signal dom Bool
  -- ^ @AHOLD@: keep the value of the A register
  , bHold :: Signal dom Bool
  -- ^ @BHOLD@: keep the value of the B register
  , cHold :: Signal dom Bool
  -- ^ @CHOLD@: keep the value of the C register
  , dHold :: Signal dom Bool
  -- ^ @DHOLD@: keep the value of the D register
  , irstTop :: Signal dom Bool
  -- ^ @IRSTTOP@: asynchronously reset the A and C registers and the registers
  -- of the top 8x8 product and of the cross product @A[7:0] * B[15:8]@
  , irstBot :: Signal dom Bool
  -- ^ @IRSTBOT@: asynchronously reset the B and D registers, the registers of
  -- the bottom 8x8 product and of the cross product @A[15:8] * B[7:0]@, and
  -- the register of the 32-bit product
  , orstTop :: Signal dom Bool
  -- ^ @ORSTTOP@: asynchronously reset the top output register
  , orstBot :: Signal dom Bool
  -- ^ @ORSTBOT@: asynchronously reset the bottom output register
  , oloadTop :: Signal dom Bool
  -- ^ @OLOADTOP@: load C into the top output register, instead of the result
  -- of the adder\/subtracter
  , oloadBot :: Signal dom Bool
  -- ^ @OLOADBOT@: load D into the bottom output register
  , addSubTop :: Signal dom Bool
  -- ^ @ADDSUBTOP@: 'True' to subtract, 'False' to add
  , addSubBot :: Signal dom Bool
  -- ^ @ADDSUBBOT@: 'True' to subtract, 'False' to add
  , oholdTop :: Signal dom Bool
  -- ^ @OHOLDTOP@: keep the value of the top output register
  , oholdBot :: Signal dom Bool
  -- ^ @OHOLDBOT@: keep the value of the bottom output register
  , ci :: Signal dom Bit
  -- ^ @CI@: carry in from the fabric
  , accumCi :: Signal dom Bit
  -- ^ @ACCUMCI@: carry in from the 'accumCo' of the DSP block below
  , signExtIn :: Signal dom Bit
  -- ^ @SIGNEXTIN@: sign extension from the 'signExtOut' of the DSP block below
  }

-- | All inputs zero
defMac16Input :: Mac16Input dom
defMac16Input =
  Mac16Input
    { inputA = pure 0
    , inputB = pure 0
    , inputC = pure 0
    , inputD = pure 0
    , aHold = pure False
    , bHold = pure False
    , cHold = pure False
    , dHold = pure False
    , irstTop = pure False
    , irstBot = pure False
    , orstTop = pure False
    , orstBot = pure False
    , oloadTop = pure False
    , oloadBot = pure False
    , addSubTop = pure False
    , addSubBot = pure False
    , oholdTop = pure False
    , oholdBot = pure False
    , ci = pure 0
    , accumCi = pure 0
    , signExtIn = pure 0
    }

-- | Outputs of 'sbMac16'
data Mac16Output dom = Mac16Output
  { output :: Signal dom (BitVector 32)
  -- ^ @O@: the top half's output in bits 31 to 16, the bottom half's in bits
  -- 15 to 0
  , carryOut :: Signal dom Bit
  -- ^ @CO@: the carry out of the top adder\/subtracter, inverted when it
  -- subtracts (i.e. a borrow becomes 1)
  , accumCo :: Signal dom Bit
  -- ^ @ACCUMCO@: the carry out of the top adder\/subtracter, for the 'accumCi'
  -- of the DSP block above
  , signExtOut :: Signal dom Bit
  -- ^ @SIGNEXTOUT@: the sign of the lower input of the top adder\/subtracter,
  -- for the 'signExtIn' of the DSP block above
  }

-- | Instantiate an @SB_MAC16@, configured by a t'Mac16Config'. See the module
-- documentation for the simulation behavior.
sbMac16 ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  Mac16Config ->
  -- | @CLK@
  Clock dom ->
  -- | @CE@
  Enable dom ->
  Mac16Input dom ->
  Mac16Output dom
sbMac16 cfg clk en inp
  | clashSimulation = mac16Model cfg clk en inp
  | otherwise =
      Mac16Output
        { output = unPort' o
        , carryOut = unPort' co
        , accumCo = unPort' accumCo'
        , signExtOut = unPort' signExtOut'
        }
 where
  unPort' :: Port nm dom a -> Signal dom a
  unPort' (Port s) = s

  (o, co, accumCo', signExtOut') = go

  go ::
    ( Port "O" dom (BitVector 32)
    , Port "CO" dom Bit
    , Port "ACCUMCO" dom Bit
    , Port "SIGNEXTOUT" dom Bit )
  go =
    inst
      (ice40Prim "SB_MAC16")
      (Param @"NEG_TRIGGER" @Bit negTrigger)
      (Param @"C_REG" (boolToBit (cReg cfg)))
      (Param @"A_REG" (boolToBit (aReg cfg)))
      (Param @"B_REG" (boolToBit (bReg cfg)))
      (Param @"D_REG" (boolToBit (dReg cfg)))
      (Param @"TOP_8x8_MULT_REG" (boolToBit (top8x8MultReg cfg)))
      (Param @"BOT_8x8_MULT_REG" (boolToBit (bot8x8MultReg cfg)))
      (Param @"PIPELINE_16x16_MULT_REG1" (boolToBit (pipeline16x16MultReg1 cfg)))
      (Param @"PIPELINE_16x16_MULT_REG2" (boolToBit (pipeline16x16MultReg2 cfg)))
      (BitVectorParam @"TOPOUTPUT_SELECT" (outputSelectBits (topOutputSelect cfg)))
      (BitVectorParam @"TOPADDSUB_LOWERINPUT" (topLowerBits (topAddSubLowerInput cfg)))
      (Param @"TOPADDSUB_UPPERINPUT" (upperBit (topAddSubUpperInput cfg)))
      (BitVectorParam @"TOPADDSUB_CARRYSELECT" (topCarryBits (topAddSubCarrySelect cfg)))
      (BitVectorParam @"BOTOUTPUT_SELECT" (outputSelectBits (botOutputSelect cfg)))
      (BitVectorParam @"BOTADDSUB_LOWERINPUT" (botLowerBits (botAddSubLowerInput cfg)))
      (Param @"BOTADDSUB_UPPERINPUT" (upperBit (botAddSubUpperInput cfg)))
      (BitVectorParam @"BOTADDSUB_CARRYSELECT" (botCarryBits (botAddSubCarrySelect cfg)))
      (Param @"MODE_8x8" (boolToBit (mode8x8 cfg)))
      (Param @"A_SIGNED" (boolToBit (aSigned cfg)))
      (Param @"B_SIGNED" (boolToBit (bSigned cfg)))
      (ClockPort @"CLK" clk)
      (Port @"CE" (boolToBit <$> fromEnable en))
      (Port @"C" (inputC inp))
      (Port @"A" (inputA inp))
      (Port @"B" (inputB inp))
      (Port @"D" (inputD inp))
      (Port @"AHOLD" (boolToBit <$> aHold inp))
      (Port @"BHOLD" (boolToBit <$> bHold inp))
      (Port @"CHOLD" (boolToBit <$> cHold inp))
      (Port @"DHOLD" (boolToBit <$> dHold inp))
      (Port @"IRSTTOP" (boolToBit <$> irstTop inp))
      (Port @"IRSTBOT" (boolToBit <$> irstBot inp))
      (Port @"ORSTTOP" (boolToBit <$> orstTop inp))
      (Port @"ORSTBOT" (boolToBit <$> orstBot inp))
      (Port @"OLOADTOP" (boolToBit <$> oloadTop inp))
      (Port @"OLOADBOT" (boolToBit <$> oloadBot inp))
      (Port @"ADDSUBTOP" (boolToBit <$> addSubTop inp))
      (Port @"ADDSUBBOT" (boolToBit <$> addSubBot inp))
      (Port @"OHOLDTOP" (boolToBit <$> oholdTop inp))
      (Port @"OHOLDBOT" (boolToBit <$> oholdBot inp))
      (Port @"CI" (ci inp))
      (Port @"ACCUMCI" (accumCi inp))
      (Port @"SIGNEXTIN" (signExtIn inp))

  negTrigger :: Bit
  negTrigger = case activeEdge @dom of
    SRising -> 0
    SFalling -> 1

outputSelectBits :: OutputSelect -> BitVector 2
outputSelectBits = \case
  OutputAddSub -> 0
  OutputAddSubRegistered -> 1
  OutputMult8x8 -> 2
  OutputMult16x16 -> 3

topLowerBits :: TopLowerInput -> BitVector 2
topLowerBits = \case
  TopLowerA -> 0
  TopLowerMult8x8 -> 1
  TopLowerMult16x16 -> 2
  TopLowerSignExtension -> 3

botLowerBits :: BotLowerInput -> BitVector 2
botLowerBits = \case
  BotLowerB -> 0
  BotLowerMult8x8 -> 1
  BotLowerMult16x16 -> 2
  BotLowerSignExtIn -> 3

upperBit :: UpperInput -> Bit
upperBit = \case
  UpperAccumulator -> 0
  UpperDirect -> 1

topCarryBits :: TopCarrySelect -> BitVector 2
topCarryBits = \case
  TopCarryZero -> 0
  TopCarryOne -> 1
  TopCarryBot -> 2
  TopCarryBotXorAddSub -> 3

botCarryBits :: BotCarrySelect -> BitVector 2
botCarryBits = \case
  BotCarryZero -> 0
  BotCarryOne -> 1
  BotCarryAccumCi -> 2
  BotCarryCi -> 3

-- | Simulation model of 'sbMac16', following Yosys' model
mac16Model ::
  forall dom.
  KnownDomain dom =>
  Mac16Config ->
  Clock dom ->
  Enable dom ->
  Mac16Input dom ->
  Mac16Output dom
mac16Model Mac16Config{..} clk en Mac16Input{..} =
  Mac16Output
    { output = (++#) <$> oh <*> ol
    , carryOut = xor <$> accumCo' <*> (boolToBit <$> addSubTop)
    , accumCo = accumCo'
    , signExtOut = msb <$> iX
    }
 where
  ce = fromEnable en

  -- A register with an asynchronous, active-high reset to 0 and a hold
  -- input, enabled by CE
  reg :: KnownNat n => Signal dom Bool -> Signal dom Bool -> Signal dom (BitVector n) -> Signal dom (BitVector n)
  reg rst hold d = q
   where
    q = mux rst 0 qReg
    qReg =
      delay clk enableGen (errorX "sbMac16: register undefined") $
        next <$> rst <*> ce <*> hold <*> d <*> q
    next r e h x q0
      | r = 0
      | e && not h = x
      | otherwise = q0

  sel :: Bool -> a -> a -> a
  sel b x y = if b then x else y

  -- Input registers
  iC = sel cReg (reg irstTop cHold inputC) inputC
  iA = sel aReg (reg irstTop aHold inputA) inputA
  iB = sel bReg (reg irstBot bHold inputB) inputB
  iD = sel dReg (reg irstBot dHold inputD) inputD

  -- Multipliers
  hiPart :: Bool -> BitVector 16 -> BitVector 16
  hiPart signed x = sel signed (pack (resize (unpack (slice d15 d8 x) :: Signed 8))) (zeroExtend (slice d15 d8 x))
  loPart :: Bool -> BitVector 16 -> BitVector 16
  loPart signed x =
    sel (signed && mode8x8) (pack (resize (unpack (slice d7 d0 x) :: Signed 8))) (zeroExtend (slice d7 d0 x))

  ah = hiPart aSigned <$> iA
  al = loPart aSigned <$> iA
  bh = hiPart bSigned <$> iB
  bl = loPart bSigned <$> iB

  -- Products, truncated to 16 bits
  pAhBh = (*) <$> ah <*> bh
  pAlBh = (\a b -> zeroExtend (slice d7 d0 a) * b) <$> al <*> bh
  pAhBl = (\a b -> a * zeroExtend (slice d7 d0 b)) <$> ah <*> bl
  pAlBl = (*) <$> al <*> bl

  hold8x8 = pure mode8x8
  iF = sel top8x8MultReg (reg irstTop (pure False) pAhBh) pAhBh
  iJ = sel pipeline16x16MultReg1 (reg irstTop hold8x8 pAlBh) pAlBh
  iK = sel pipeline16x16MultReg1 (reg irstBot hold8x8 pAhBl) pAhBl
  iG = sel bot8x8MultReg (reg irstBot (pure False) pAlBl) pAlBl

  -- 32-bit product
  ext :: Bool -> BitVector 16 -> BitVector 32
  ext signed x = sel signed (pack (resize (unpack x :: Signed 16))) (zeroExtend x)
  iL =
    (\g k j f -> zeroExtend g + (ext aSigned k `shiftL` 8) + (ext bSigned j `shiftL` 8) + (zeroExtend f `shiftL` 16))
      <$> iG <*> iK <*> iJ <*> iF
  iH = sel pipeline16x16MultReg2 (reg irstBot hold8x8 iL) iL

  -- Top adder/subtracter
  iW = sel (topAddSubUpperInput == UpperDirect) iC iQ
  iX = case topAddSubLowerInput of
    TopLowerA -> iA
    TopLowerMult8x8 -> iF
    TopLowerMult16x16 -> slice d31 d16 <$> iH
    TopLowerSignExtension -> (\z -> if msb z == 1 then maxBound else 0) <$> iZ
  (accumCo', xw) = unbundle (addSub <$> addSubTop <*> iX <*> iW <*> hci)
  iP = mux oloadTop iC (xorSub <$> addSubTop <*> xw)
  iQ = reg orstTop oholdTop iP
  oh = case topOutputSelect of
    OutputAddSub -> iP
    OutputAddSubRegistered -> iQ
    OutputMult8x8 -> iF
    OutputMult16x16 -> slice d31 d16 <$> iH
  hci = case topAddSubCarrySelect of
    TopCarryZero -> pure 0
    TopCarryOne -> pure 1
    TopCarryBot -> lco
    TopCarryBotXorAddSub -> xor <$> lco <*> (boolToBit <$> addSubBot)

  -- Bottom adder/subtracter
  iY = sel (botAddSubUpperInput == UpperDirect) iD iS
  iZ = case botAddSubLowerInput of
    BotLowerB -> iB
    BotLowerMult8x8 -> iG
    BotLowerMult16x16 -> slice d15 d0 <$> iH
    BotLowerSignExtIn -> (\s -> if s == 1 then maxBound else 0) <$> signExtIn
  (lco, yz) = unbundle (addSub <$> addSubBot <*> iZ <*> iY <*> lci)
  iR = mux oloadBot iD (xorSub <$> addSubBot <*> yz)
  iS = reg orstBot oholdBot iR
  ol = case botOutputSelect of
    OutputAddSub -> iR
    OutputAddSubRegistered -> iS
    OutputMult8x8 -> iG
    OutputMult16x16 -> slice d15 d0 <$> iH
  lci = case botAddSubCarrySelect of
    BotCarryZero -> pure 0
    BotCarryOne -> pure 1
    BotCarryAccumCi -> accumCi
    BotCarryCi -> ci

  -- @{carry, sum} = lower + (upper ^ {16{sub}}) + carry@. The result is
  -- inverted again when subtracting, which gives @upper - lower - carry@.
  addSub :: Bool -> BitVector 16 -> BitVector 16 -> Bit -> (Bit, BitVector 16)
  addSub isSub lower upper carry =
    (msb s, truncateB s)
   where
    s = zeroExtend lower + zeroExtend (xorSub isSub upper) + zeroExtend (pack carry) :: BitVector 17

  xorSub :: Bool -> BitVector 16 -> BitVector 16
  xorSub isSub x = sel isSub (complement x) x


{- | Signed 16x16 multiplication on a single 'sbMac16', with a latency of two
cycles: the inputs and the 32-bit product are registered.
-}
multiply16 ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  Clock dom ->
  -- | Clock enable (@CE@)
  Enable dom ->
  -- | @a@
  Signal dom (Signed 16) ->
  -- | @b@
  Signal dom (Signed 16) ->
  Signal dom (Signed 32)
multiply16 clk en a b = unpack <$> output (sbMac16 cfg clk en inputs)
 where
  cfg =
    defMac16Config
      { aReg = True
      , bReg = True
      , pipeline16x16MultReg2 = True
      , topOutputSelect = OutputMult16x16
      , botOutputSelect = OutputMult16x16
      , aSigned = True
      , bSigned = True
      }
  inputs =
    defMac16Input
      { inputA = pack <$> a
      , inputB = pack <$> b
      }
