-- | Configurations, circuits and stimuli for "SbMac16"
module SbMac16Types where

import Clash.Explicit.Prelude

import Clash.Cores.LatticeSemi.Ice40.Dsp
import Ice40TestCommon

import Language.Haskell.TH (ExpQ)

import qualified Prelude as P

-- | Number of cycles of each test bench
type Cycles = 600

-- | Inputs of 'sbMac16', plus its clock enable
data Mac16In = Mac16In
  { ce :: Bool
  , a :: BitVector 16
  , b :: BitVector 16
  , c :: BitVector 16
  , d :: BitVector 16
  , holds :: BitVector 4
  -- ^ A, B, C, D
  , irsts :: BitVector 2
  -- ^ Top, bottom
  , orsts :: BitVector 2
  , oloads :: BitVector 2
  , addSubs :: BitVector 2
  , oholds :: BitVector 2
  , carryIns :: BitVector 3
  -- ^ CI, ACCUMCI, SIGNEXTIN
  }
  deriving (Show, Generic, NFDataX, BitPack)

-- | 'sbMac16'. Outputs @O@, @CO@, @ACCUMCO@ and @SIGNEXTOUT@.
mac16Circuit :: Mac16Config -> Clock Dom -> Signal Dom Mac16In -> Signal Dom (BitVector 32, Bit, Bit, Bit)
mac16Circuit cfg clk i =
  bundle (output out, carryOut out, accumCo out, signExtOut out)
 where
  out = sbMac16 cfg clk (toEnable (ce <$> i)) inputs
  bitAt :: KnownNat n => (Mac16In -> BitVector n) -> Int -> Signal Dom Bool
  bitAt f n = (\x -> testBit (f x) n) <$> i
  inputs =
    Mac16Input
      { inputA = a <$> i
      , inputB = b <$> i
      , inputC = c <$> i
      , inputD = d <$> i
      , aHold = bitAt holds 3
      , bHold = bitAt holds 2
      , cHold = bitAt holds 1
      , dHold = bitAt holds 0
      , irstTop = bitAt irsts 1
      , irstBot = bitAt irsts 0
      , orstTop = bitAt orsts 1
      , orstBot = bitAt orsts 0
      , oloadTop = bitAt oloads 1
      , oloadBot = bitAt oloads 0
      , addSubTop = bitAt addSubs 1
      , addSubBot = bitAt addSubs 0
      , oholdTop = bitAt oholds 1
      , oholdBot = bitAt oholds 0
      , ci = (! (2 :: Int)) . carryIns <$> i
      , accumCi = (! (1 :: Int)) . carryIns <$> i
      , signExtIn = (! (0 :: Int)) . carryIns <$> i
      }
{-# INLINE mac16Circuit #-}

-- | Random bits, each set with the given probability (in percent)
genFlags :: forall n. KnownNat n => Integer -> G (BitVector n)
genFlags p = pack <$> traverse (const (chance p)) (repeat @n ())

genMac16In :: G Mac16In
genMac16In =
  Mac16In
    <$> chance 85
    <*> genBV
    <*> genBV
    <*> genBV
    <*> genBV
    <*> genFlags 20
    <*> genFlags 4
    <*> genFlags 4
    <*> genFlags 10
    <*> genFlags 50
    <*> genFlags 15
    <*> genBV

mac16Blobs :: Mac16Config -> Int -> ExpQ
mac16Blobs cfg seed =
  blobs (mac16Circuit cfg) (evalG (sequence (P.replicate (natToNum @Cycles) genMac16In)) seed)

-- * Configurations

-- | Unsigned 16x16 multiplier without registers
mult16Unsigned :: Mac16Config
mult16Unsigned =
  defMac16Config
    { topOutputSelect = OutputMult16x16
    , botOutputSelect = OutputMult16x16
    }

-- | Signed 16x16 multiplier with all registers
mult16Signed :: Mac16Config
mult16Signed =
  mult16Unsigned
    { aReg = True
    , bReg = True
    , top8x8MultReg = True
    , bot8x8MultReg = True
    , pipeline16x16MultReg1 = True
    , pipeline16x16MultReg2 = True
    , aSigned = True
    , bSigned = True
    }

-- | Two 8x8 multipliers, signed A and unsigned B, with registered products
mult8x8 :: Mac16Config
mult8x8 =
  defMac16Config
    { aReg = True
    , top8x8MultReg = True
    , bot8x8MultReg = True
    , pipeline16x16MultReg1 = True
    , pipeline16x16MultReg2 = True
    , topOutputSelect = OutputMult8x8
    , botOutputSelect = OutputMult8x8
    , mode8x8 = True
    , aSigned = True
    }

-- | 32-bit multiply-accumulate
mac32 :: Mac16Config
mac32 =
  mult16Signed
    { pipeline16x16MultReg1 = False
    , topOutputSelect = OutputAddSubRegistered
    , topAddSubLowerInput = TopLowerMult16x16
    , topAddSubUpperInput = UpperAccumulator
    , topAddSubCarrySelect = TopCarryBot
    , botOutputSelect = OutputAddSubRegistered
    , botAddSubLowerInput = BotLowerMult16x16
    , botAddSubUpperInput = UpperAccumulator
    , botAddSubCarrySelect = BotCarryZero
    }

-- | Two 16-bit adder/subtracters: @C +- A@ and @D +- B@
addSub16 :: Mac16Config
addSub16 =
  defMac16Config
    { cReg = True
    , dReg = True
    , topOutputSelect = OutputAddSub
    , topAddSubLowerInput = TopLowerA
    , topAddSubUpperInput = UpperDirect
    , topAddSubCarrySelect = TopCarryBotXorAddSub
    , botOutputSelect = OutputAddSubRegistered
    , botAddSubLowerInput = BotLowerB
    , botAddSubUpperInput = UpperDirect
    , botAddSubCarrySelect = BotCarryCi
    }

-- | Two 16-bit accumulators of 8x8 products, cascade carry in
accum8x8 :: Mac16Config
accum8x8 =
  defMac16Config
    { bReg = True
    , topOutputSelect = OutputAddSubRegistered
    , topAddSubLowerInput = TopLowerMult8x8
    , topAddSubCarrySelect = TopCarryOne
    , botOutputSelect = OutputAddSubRegistered
    , botAddSubLowerInput = BotLowerMult8x8
    , botAddSubCarrySelect = BotCarryAccumCi
    , mode8x8 = True
    , aSigned = True
    , bSigned = True
    }

-- | Sign extensions as lower inputs
signExtension :: Mac16Config
signExtension =
  defMac16Config
    { topOutputSelect = OutputAddSubRegistered
    , topAddSubLowerInput = TopLowerSignExtension
    , topAddSubCarrySelect = TopCarryBot
    , botOutputSelect = OutputAddSub
    , botAddSubLowerInput = BotLowerSignExtIn
    , botAddSubUpperInput = UpperDirect
    , botAddSubCarrySelect = BotCarryCi
    }

-- | 16x16 multiplier with signed A and unsigned B, cross products registered
mult16Mixed :: Mac16Config
mult16Mixed =
  mult16Unsigned
    { pipeline16x16MultReg1 = True
    , aSigned = True
    }

-- | 16x16 multiplier with unsigned A and signed B
mult16MixedB :: Mac16Config
mult16MixedB = mult16Unsigned{bSigned = True}
