{-|
Copyright  :  (C) 2026,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation models and IP instantiation for "Clash.Cores.Xilinx.Divider".
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Divider.Internal
  ( -- * Configuration
    Config(..)
  , Algorithm(..)
  , algorithmToTcl
  , defConfig
    -- * Operands
  , DividerOperand(..)
  , ByteWidth
  , HighRadixDefDelay
    -- * Simulation models
  , quotRemModel
  , quotFracModel
  , highRadixModel
    -- * Instantiating the IP
  , dividerInst
  , highRadixInst
  ) where

import Clash.Prelude
import qualified Prelude as P

import Data.Ratio (denominator, numerator, (%))
import Data.Type.Bool (If)

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | Customize the Divider Generator IP for 'Clash.Cores.Xilinx.Divider.quotRemWith'.
--
-- These customizations influence how the IP is synthesized. They do not
-- affect its behavior, but they do affect the latencies it supports: see
-- 'Algorithm'.
data Config = Config
  { algorithm :: !Algorithm
    -- ^ Division algorithm
  } deriving (Eq, Show)

-- | Implementation of integer division with remainder. Both compute the exact
-- quotient and remainder; they differ in resource usage and supported widths
-- and latencies. See
-- [PG151](https://docs.amd.com/v/u/en-US/pg151-div-gen), chapter 3.
data Algorithm
  = Radix2
    -- ^ Radix-2 non-restoring division in FPGA logic. Dividend and divisor
    -- widths from 2 to 64 bits. The latency ranges from 1 to the dividend
    -- width plus 2 (unsigned) or 4 (signed).
  | LutMult
    -- ^ A reciprocal lookup table followed by a multiplier, using DSP slices
    -- and block RAM. Dividend width from 2 to 17 bits, divisor width from 2
    -- to 12 bits, at most 23 bits together. The latency ranges from 1 to 8
    -- (unsigned) or 10 (signed).
  deriving (Eq, Show)

algorithmToTcl :: Algorithm -> String
algorithmToTcl = \case
  Radix2 -> "Radix2"
  LutMult -> "LutMult"

-- | Default customization: 'Radix2'
defConfig :: Config
defConfig = Config{algorithm = Radix2}

-- | Width of an operand padded to whole bytes, as on the AXI4-Stream
-- interfaces of the IP
type ByteWidth (n :: Nat) = 8 * DivRU n 8

-- | Operand types of the divider: 'Signed' and 'Unsigned'. Its methods are
-- used by the simulation models and the IP instantiation; you should not need
-- them.
class DividerOperand (a :: Nat -> Type) where
  -- | Fractional part of the quotient computed by
  -- 'Clash.Cores.Xilinx.Divider.quotFrac', with @f@ fractional bits. It holds
  -- the remainder divided by the divisor, truncated towards zero:
  --
  -- * For 'Unsigned' operands, an @'UFixed' 0 f@ in @[0, 1)@
  -- * For 'Signed' operands, an @'SFixed' 1 f@ in @(-1, 1)@, which has the
  --   sign of the quotient
  type Fraction a (f :: Nat) = (r :: Type) | r -> a f

  -- | Width of 'Fraction' in bits: the fractional width of the IP
  type FractionWidth a (f :: Nat) :: Nat

  -- | Default (and maximum) latency of 'Clash.Cores.Xilinx.Divider.quotRem'
  -- with 'Radix2', for a dividend of width @m@
  type QuotRemDefDelay a (m :: Nat) :: Nat

  -- | Default (and maximum) latency of 'Clash.Cores.Xilinx.Divider.quotFrac'
  -- for a dividend of width @m@ and a 'Fraction' with @f@ fractional bits
  type QuotFracDefDelay a (m :: Nat) (f :: Nat) :: Nat

  -- | @Signed@ or @Unsigned@, the value of the IP's @operand_sign@ parameter
  operandSign :: String

  -- | Value of an operand
  operandToInteger :: KnownNat n => a n -> Integer

  -- | Wraps around if the value does not fit
  operandFromInteger :: KnownNat n => Integer -> a n

  -- | 'pack'
  operandToBitVector :: KnownNat n => a n -> BitVector n

  -- | 'unpack'
  operandFromBitVector :: KnownNat n => BitVector n -> a n

  -- | Fraction from the raw integer value of its bits
  fractionFromInteger :: KnownNat f => Integer -> Fraction a f

  -- | 'unpack'
  fractionFromBitVector :: KnownNat f => BitVector (FractionWidth a f) -> Fraction a f

instance DividerOperand Signed where
  type Fraction Signed f = SFixed 1 f
  type FractionWidth Signed f = f + 1
  type QuotRemDefDelay Signed m = m + 4
  type QuotFracDefDelay Signed m f = m + f + 5
  operandSign = "Signed"
  operandToInteger = toInteger
  operandFromInteger = fromInteger
  operandToBitVector = pack
  operandFromBitVector = unpack
  fractionFromInteger = Fixed . fromInteger
  fractionFromBitVector = unpack

instance DividerOperand Unsigned where
  type Fraction Unsigned f = UFixed 0 f
  type FractionWidth Unsigned f = f
  type QuotRemDefDelay Unsigned m = m + 2
  type QuotFracDefDelay Unsigned m f = m + f + 2
  operandSign = "Unsigned"
  operandToInteger = toInteger
  operandFromInteger = fromInteger
  operandToBitVector = pack
  operandFromBitVector = unpack
  fractionFromInteger = Fixed . fromInteger
  fractionFromBitVector = unpack

-- | Default (and maximum) latency of
-- 'Clash.Cores.Xilinx.Divider.divHighRadix' with a divisor of width @n@: the
-- first column of table 2-3 of PG151, for a quotient of up to 12 bits. Vivado
-- gives divisors wider than 54 bits the same latency as divisors of 53 and 54
-- bits.
type HighRadixDefDelay (n :: Nat) =
  If (n <=? 8) 16
    (If (n <=? 18) 17
      (If (n <=? 32) 18
        (If (n <=? 35) 19
          (If (n <=? 48) 20
            (If (n <=? 52) 22 23)))))

-- | Model of integer division with remainder: the quotient is truncated
-- towards zero, and the remainder has the sign of the dividend (Haskell's
-- 'quotRem'). The quotient wraps around for the most negative dividend divided
-- by -1. Gives 'Nothing' when dividing by zero.
quotRemModel ::
  forall a m n.
  (DividerOperand a, KnownNat m, KnownNat n) =>
  a m ->
  a n ->
  Maybe (a m, a n)
quotRemModel (operandToInteger -> x) (operandToInteger -> y)
  | y == 0 = Nothing
  | otherwise = Just (operandFromInteger q, operandFromInteger r)
 where
  (q, r) = x `quotRem` y

-- | Model of division with fractional output: the quotient as in
-- 'quotRemModel', and the remainder divided by the divisor with @f@
-- fractional bits, truncated towards zero. Gives 'Nothing' when dividing by
-- zero.
quotFracModel ::
  forall a m n f.
  (DividerOperand a, KnownNat m, KnownNat n, KnownNat f) =>
  a m ->
  a n ->
  Maybe (a m, Fraction a f)
quotFracModel (operandToInteger -> x) (operandToInteger -> y)
  | y == 0 = Nothing
  | otherwise =
      Just
        ( operandFromInteger q
        , fractionFromInteger @a @f ((r `shiftL` natToNum @f) `quot` y) )
 where
  (q, r) = x `quotRem` y

-- | Model of the IP's /High Radix/ algorithm with a single iteration, which it
-- uses when the quotient has at most 12 bits (@m + f <= 12@). The IP rounds the
-- quotient to @f@ fractional bits, but not quite to the nearest value:
--
-- 1. The IP normalizes the divisor @y@ to @y_n = y / 2^e@, with
--    @e = floor (log2 |y|)@, and shifts the dividend @x@ along. It keeps @f + 2@
--    fractional bits of the shifted dividend, rounding down: the dividend
--    becomes @x' = floor (x * 2^(f+2) / 2^e) * 2^e / 2^(f+2)@.
--
-- 2. It computes the quotient @x' / y@ with @g = 14 - m - f@ extra fractional
--    bits: @14@ bits in total. For a positive divisor it rounds down, for a
--    negative divisor it rounds to the nearest value (half up).
--
-- 3. It rounds this extended quotient to @f@ fractional bits, to the nearest
--    value, half up.
--
-- The IP divides by multiplying with an estimate of the reciprocal of @y_n@.
-- That estimate is very slightly too small, except for @y_n = 1.5@, where it is
-- very slightly too large. This only shows when the extended quotient of step
-- 2 is exact (or exactly halfway, for negative divisors). The estimate then
-- breaks the tie: the IP's extended quotient is just closer to zero, or, for
-- @y_n = 1.5@ and a positive divisor, just further from zero.
--
-- The result is within 3/4 of its least significant bit from the exact
-- quotient. It wraps around for the most negative dividend divided by -1.
-- Division by zero gives 'Nothing'.
--
-- We found these rules with the C model of the IP and confirmed them for all
-- dividends and divisors of several configurations; see
-- @test/Test/Cores/Xilinx/Divider.hs@.
highRadixModel ::
  forall m n f.
  (KnownNat m, KnownNat n, KnownNat f) =>
  Signed m ->
  Signed n ->
  Maybe (SFixed m f)
highRadixModel (toInteger -> x) (toInteger -> y)
  | y == 0 = Nothing
  | otherwise = Just (Fixed (fromInteger (roundHalfUp g q)))
 where
  fracWidth = natToNum @f
  g = 14 - natToNum @m - fracWidth
  -- e = floor (log2 |y|)
  e = P.length (P.takeWhile (> 1) (P.iterate (`shiftR` 1) (abs y)))
  -- Step 1: the dividend, rounded down to f + 2 fractional bits after
  -- shifting it right by e, in units of 2^(e - f - 2)
  x' = (x `shiftL` (fracWidth + 2)) `shiftR` e
  -- Step 2: the extended quotient x' / y in units of 2^-(f + g), and the
  -- direction of the error of the IP's reciprocal estimate
  exact = (x' `shiftL` (g - 2 + e)) % y
  reciprocalTooLarge = y > 0 && 2 * y == 3 `shiftL` e
  q
    | y > 0 = floorNudged (if reciprocalTooLarge then exact else negate exact) exact
    | otherwise = floorNudged (negate exact) (exact + 1 % 2)

-- | @floorNudged dir v@ rounds @v@ down, after nudging it by an infinitesimal
-- amount in the direction of @dir@
floorNudged :: Rational -> Rational -> Integer
floorNudged dir v
  | denominator v == 1 && dir < 0 = numerator v - 1
  | otherwise = floor v

-- | Round an integer @q@ to a multiple of @2^g@ (the nearest, half up), and
-- divide by @2^g@
roundHalfUp :: Int -> Integer -> Integer
roundHalfUp g q = (q + bit (g - 1)) `shiftR` g

-- | Instantiate the Divider Generator. It is always configured as:
--
-- * Non-blocking AXI4-Stream interfaces, no @TREADY@ on the output, always
--   valid inputs
-- * A clock enable (@aclken@), no reset
-- * Division by zero detection, on @TUSER@ of the output
-- * Manual latency, set to @d@
-- * 1 clock per division
--
-- Gives the output's @TUSER@ (division by zero) and @TDATA@.
dividerInst ::
  forall d dvdW dvsW outW nOpts dom.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat dvdW
  , KnownNat dvsW
  , KnownNat outW
  , KnownNat nOpts
  , HasCallStack
  ) =>
  -- | Instantiation name hint
  String ->
  -- | Algorithm, operand sign, remainder type and fractional width options
  Vec nOpts (OptionName, XilinxWizardOption) ->
  Clock dom ->
  Enable dom ->
  -- | Dividend, padded to whole bytes
  Signal dom (BitVector dvdW) ->
  -- | Divisor, padded to whole bytes
  Signal dom (BitVector dvsW) ->
  (Signal dom Bit, Signal dom (BitVector outW))
dividerInst nm opts clk en dividend divisor =
  (unpack <$> unPort tuser, unPort tdata)
 where
  (_tvalid, tuser, tdata) = go

  go ::
    ( Port "m_axis_dout_tvalid" dom Bit
    , Port "m_axis_dout_tuser" dom (BitVector 1)
    , Port "m_axis_dout_tdata" dom (BitVector outW)
    )
  go =
    instWithXilinxWizard
      (instConfig nm)
      (dividerWizard @d opts)
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_dividend_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_dividend_tdata" dividend)
      (Port @"s_axis_divisor_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_divisor_tdata" divisor)
{-# INLINE dividerInst #-}

-- | Like 'dividerInst', for the IP's /High Radix/ algorithm. The IP then has
-- @TREADY@ outputs on its input channels. They are always asserted for the
-- configurations we support (see 'highRadixModel'), so we ignore them.
highRadixInst ::
  forall d dvdW dvsW outW nOpts dom.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat dvdW
  , KnownNat dvsW
  , KnownNat outW
  , KnownNat nOpts
  , HasCallStack
  ) =>
  -- | Instantiation name hint
  String ->
  -- | Algorithm, operand sign, remainder type and fractional width options
  Vec nOpts (OptionName, XilinxWizardOption) ->
  Clock dom ->
  Enable dom ->
  -- | Dividend, padded to whole bytes
  Signal dom (BitVector dvdW) ->
  -- | Divisor, padded to whole bytes
  Signal dom (BitVector dvsW) ->
  (Signal dom Bit, Signal dom (BitVector outW))
highRadixInst nm opts clk en dividend divisor =
  (unpack <$> unPort tuser, unPort tdata)
 where
  (_dividendReady, _divisorReady, _tvalid, tuser, tdata) = go

  go ::
    ( Port "s_axis_dividend_tready" dom Bit
    , Port "s_axis_divisor_tready" dom Bit
    , Port "m_axis_dout_tvalid" dom Bit
    , Port "m_axis_dout_tuser" dom (BitVector 1)
    , Port "m_axis_dout_tdata" dom (BitVector outW)
    )
  go =
    instWithXilinxWizard
      (instConfig nm)
      (dividerWizard @d opts)
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_dividend_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_dividend_tdata" dividend)
      (Port @"s_axis_divisor_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_divisor_tdata" divisor)
{-# INLINE highRadixInst #-}

-- | Options of the Divider Generator, see 'dividerInst'
dividerWizard ::
  forall d nOpts.
  KnownNat d =>
  Vec nOpts (OptionName, XilinxWizardOption) ->
  XilinxWizard (8 + nOpts)
dividerWizard opts =
  XilinxWizard
    { wiz_name = "div_gen"
    , wiz_vendor = "xilinx.com"
    , wiz_library = "ip"
    , wiz_version = "5.1"
    , wiz_options =
           ("CONFIG.FlowControl",           StrOpt "NonBlocking")
        :> ("CONFIG.OutTready",             BoolOpt False)
        :> ("CONFIG.ACLKEN",                BoolOpt True)
        :> ("CONFIG.ARESETN",               BoolOpt False)
        :> ("CONFIG.divide_by_zero_detect", BoolOpt True)
        :> ("CONFIG.clocks_per_division",   IntegerOpt 1)
        :> ("CONFIG.latency_configuration", StrOpt "Manual")
        :> ("CONFIG.latency",               IntegerOpt (natToNum @d))
        :> opts
    }
{-# INLINE dividerWizard #-}
