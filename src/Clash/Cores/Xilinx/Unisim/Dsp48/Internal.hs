{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Attribute types and helpers shared by "Clash.Cores.Xilinx.Unisim.Dsp48e1" and
"Clash.Cores.Xilinx.Unisim.Dsp48e2".
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Unisim.Dsp48.Internal where

import Clash.Explicit.Prelude
import Clash.Sized.Internal.BitVector (Bit(Bit), BitVector(BV))

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)

-- | Source of the A or B input registers (attributes @A_INPUT@ and @B_INPUT@)
data InputSource
  = Direct
  -- ^ @DIRECT@: use the @A@ or @B@ input
  | Cascade
  -- ^ @CASCADE@: use the @ACIN@ or @BCIN@ cascade input
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
  -- @MREG = 0@.
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

-- | Selects the pattern of the pattern detector (attribute @SEL_PATTERN@)
data SelPattern
  = PatternAttr
  -- ^ @PATTERN@: use the @PATTERN@ attribute
  | PatternC
  -- ^ @C@: use the (registered or direct) C input
  deriving (Show, Eq, Generic, NFDataX)

-- | Selects the mask of the pattern detector (attribute @SEL_MASK@)
data SelMask
  = MaskAttr
  -- ^ @MASK@: use the @MASK@ attribute
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

-- | Build a 'BitVector' from bits that are either defined ('Just') or
-- undefined ('Nothing'). The first element is the most significant bit.
partiallyDefined :: KnownNat n => Vec n (Maybe Bit) -> BitVector n
partiallyDefined bits =
  BV (fromInteger (toInteger mask)) (fromInteger (toInteger val))
 where
  mask = pack (map (maybe high (const low)) bits)
  val = pack (map (fromMaybe low) bits)

-- | 'True' if the argument is defined and 'True'. This is how an HDL simulator
-- treats an undefined condition in an @if@ statement.
definitelyTrue :: Bool -> Bool
definitelyTrue = fromRight False . isX

-- | 'True' if the argument is defined and 'False'
definitelyFalse :: Bool -> Bool
definitelyFalse = fromRight False . fmap not . isX

-- | Whether any bit of a 'BitVector' is undefined
anyUndefined :: BitVector n -> Bool
anyUndefined bv = case isX bv of
  Left _ -> True
  Right (BV m _) -> m /= 0

-- | Build a 'BitVector' from bits, keeping track of which bits are undefined.
-- Unlike 'pack', a bit that throws an 'XException' only makes that bit
-- undefined. The first element is the most significant bit.
fromBits :: KnownNat n => Vec n Bit -> BitVector n
fromBits bits = BV (fromIntegral (toInteger masks)) (fromIntegral (toInteger vals))
 where
  info b = case isX b of
    Left _ -> (high, low)
    Right (Bit m v) -> (if m == 0 then low else high, if v == 0 then low else high)
  masks = pack (map (fst . info) bits)
  vals = pack (map (snd . info) bits)

-- | Mark the bits that are set in the first argument as undefined
markUndefined :: KnownNat n => BitVector n -> BitVector n -> BitVector n
markUndefined undefBits bv = case isX bv of
  Left _ -> bv
  Right (BV m v) ->
    let m1 = m .|. fromIntegral (toInteger undefBits)
    in BV m1 (v `xor` (v .&. m1))
