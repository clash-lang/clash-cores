{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation model, configuration and IP instantiation for
"Clash.Cores.Xilinx.ComplexMultiplier".
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.ComplexMultiplier.Internal
  ( -- * Configuration
    Config(..)
  , MultiplierConstruction(..)
  , OptimizeGoal(..)
  , defConfig
  , configOpts
    -- * Widths and latencies
  , NaturalWidth
  , ByteWidth
  , ComplexMultiplyDefDelay
  , DspTiles
    -- * Simulation models
  , fullProduct
  , truncateModel
  , roundModel
    -- * Instantiating the IP
  , packComplex
  , unpackComplex
  , cmpyInst
  , cmpyRoundInst
  ) where

import Clash.Prelude

import Data.Complex (Complex(..))
import Data.Type.Bool (If)

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | How the IP builds its real multipliers, see /Multiplier Construction
-- Options/ in [PG104](https://docs.amd.com/v/u/en-US/pg104-cmpy). This does
-- not affect the results, only the resources the IP uses and its fully
-- pipelined latency.
data MultiplierConstruction
  = UseDsps
    -- ^ DSP slices (the IP's @Use_Mults@)
  | UseLuts
    -- ^ FPGA logic only (@Use_LUTs@). The IP then always uses the three real
    -- multiplier structure.
  deriving (Eq, Show)

-- | Optimization goal of the IP, see /Optimization Goal/ in
-- [PG104](https://docs.amd.com/v/u/en-US/pg104-cmpy). This does not affect
-- the results, only the resources the IP uses and its fully pipelined latency.
data OptimizeGoal
  = Resources
    -- ^ For 'UseDsps': the three real multiplier structure, which uses the
    -- pre-adders of the DSP slices, unless the four real multiplier structure
    -- needs fewer DSP slices. The IP's default.
  | Performance
    -- ^ For 'UseDsps': always the four real multiplier structure, which
    -- reaches a higher clock frequency
  deriving (Eq, Show)

-- | Customization of the Complex Multiplier IP. These options do not affect
-- the results: the IP always computes the exact product before truncating or
-- rounding it. They do affect the latency at which the IP is fully pipelined,
-- which Vivado shows in the IP's customization dialog when its latency is set
-- to /Automatic/. For operands of 8 to 63 bits, on 7 series, UltraScale and
-- UltraScale+ devices, that latency (measured with Vivado 2022.1) is:
--
-- * 'UseDsps' and 'Performance': 4 to 34 cycles, see 'ComplexMultiplyDefDelay'
-- * 'UseDsps' and 'Resources': 4 to 37 cycles. Vivado picks the three or four
--   real multiplier structure per operand width, so this does not follow a
--   simple formula.
-- * 'UseLuts': 4 to 7 cycles
--
-- Any latency from 1 to 55 works for every configuration: the IP pipelines
-- less, or adds delay registers.
data Config = Config
  { multiplierConstruction :: !MultiplierConstruction
    -- ^ DSP slices or FPGA logic
  , optimizeGoal :: !OptimizeGoal
    -- ^ Resources or performance
  } deriving (Eq, Show)

-- | Default customization: 'UseDsps' and 'Performance'. The default latency,
-- 'ComplexMultiplyDefDelay', fully pipelines the IP in this configuration.
defConfig :: Config
defConfig = Config{multiplierConstruction = UseDsps, optimizeGoal = Performance}

-- | IP options for a configuration
configOpts :: Config -> Vec 2 (OptionName, XilinxWizardOption)
configOpts Config{multiplierConstruction, optimizeGoal} =
     ("CONFIG.MultType", StrOpt multType)
  :> ("CONFIG.OptimizeGoal", StrOpt goal)
  :> Nil
 where
  multType = case multiplierConstruction of
    UseDsps -> "Use_Mults"
    UseLuts -> "Use_LUTs"
  goal = case optimizeGoal of
    Resources -> "Resources"
    Performance -> "Performance"

-- | Width of the exact product of a complex number with @a@-bit components and
-- one with @b@-bit components: the /natural width/ of PG104.
type NaturalWidth (a :: Nat) (b :: Nat) = a + b + 1

-- | Width of a field padded to whole bytes, as on the AXI4-Stream interfaces
-- of the IP
type ByteWidth (n :: Nat) = 8 * DivRU n 8

-- | Number of slices of an operand of @n@ bits, for a DSP slice port of
-- @first@ bits: the least significant slices are unsigned and take 17 bits
-- each, the most significant slice is signed and takes up to @first@ bits.
type DspTiles (first :: Nat) (n :: Nat) =
  If (n <=? first) 1 (1 + DivRU (n - first) 17)

-- | Default latency of 'Clash.Cores.Xilinx.ComplexMultiplier.complexMultiply'
-- for operands with @a@-bit and @b@-bit components. It is at least the latency
-- with which the IP is fully pipelined in the default configuration
-- ('defConfig'), on 7 series, UltraScale and UltraScale+ devices.
--
-- With 'Performance', every real multiplier is a grid of DSP slices, one
-- operand split over the 25-bit ports (27 bits on UltraScale and later) and
-- the other over the 18-bit ports. Vivado's fully pipelined latency is
-- @2 + 2 * t@ for @t@ DSP slices per real multiplier. This type family
-- computes @t@ for the 25 by 18-bit DSP slices of 7 series devices, in the
-- best orientation. That is exact for 7 series devices for most operand widths
-- (it is up to 4 cycles more for some operands of 36 bits or more, where
-- Vivado needs fewer slices). On UltraScale(+) devices, whose DSP slices are
-- larger, it is up to 12 cycles more than needed for operands wider than 25
-- bits.
--
-- We measured the fully pipelined latencies with Vivado 2022.1 for all
-- operand widths from 8 to 63 bits. They do not depend on the output width or
-- rounding.
type ComplexMultiplyDefDelay (a :: Nat) (b :: Nat) =
  2 + 2 * Min (DspTiles 25 a * DspTiles 18 b) (DspTiles 18 a * DspTiles 25 b)

-- | The exact product of two complex numbers
fullProduct ::
  (KnownNat a, KnownNat b) =>
  Complex (Signed a) ->
  Complex (Signed b) ->
  Complex Integer
fullProduct (ar :+ ai) (br :+ bi) =
  (xr * yr - xi * yi) :+ (xr * yi + xi * yr)
 where
  (xr, xi, yr, yi) = (toInteger ar, toInteger ai, toInteger br, toInteger bi)

-- | Model of the IP with truncation: the @o@ most significant bits of the
-- exact product, which has 'NaturalWidth' bits. That is, the product divided
-- by @2^(a + b + 1 - o)@, rounded towards negative infinity.
truncateModel ::
  forall o a b.
  (KnownNat a, KnownNat b, KnownNat o) =>
  Complex (Signed a) ->
  Complex (Signed b) ->
  Complex (Signed o)
truncateModel x y = fromInteger . (`shiftR` k) <$> fullProduct x y
 where
  k = discardedBits @a @b @o

-- | Model of the IP with random rounding: the product divided by @2^k@, with
-- @k = a + b + 1 - o@, rounded to the nearest integer. The /round carry/
-- decides ties: they are rounded down if it is 0 and up if it is 1. The IP
-- implements this by adding @2^(k-1) - 1@ plus the round carry to the exact
-- product, and truncating the sum.
roundModel ::
  forall o a b.
  (KnownNat a, KnownNat b, KnownNat o) =>
  -- | Round carry (@ROUND_CY@)
  Bit ->
  Complex (Signed a) ->
  Complex (Signed b) ->
  Complex (Signed o)
roundModel cy x y = fromInteger . rnd <$> fullProduct x y
 where
  k = discardedBits @a @b @o
  rnd p = (p + bit (k - 1) - 1 + toInteger cy) `shiftR` k

-- | Number of least significant bits of the exact product that the IP drops
discardedBits :: forall a b o. (KnownNat a, KnownNat b, KnownNat o) => Int
discardedBits = natToNum @(NaturalWidth a b) - natToNum @o

-- | Pack a complex number into @TDATA@: the real part in the least significant
-- bits, the imaginary part from the next byte boundary. Both are sign-extended
-- to whole bytes; the IP ignores the padding.
packComplex ::
  forall n.
  KnownNat n =>
  Complex (Signed n) ->
  BitVector (2 * ByteWidth n)
packComplex (re :+ im) = pack (ext im) ++# pack (ext re)
 where
  ext :: Signed n -> Signed (ByteWidth n)
  ext = resize

-- | Inverse of 'packComplex'
unpackComplex ::
  forall n.
  KnownNat n =>
  BitVector (2 * ByteWidth n) ->
  Complex (Signed n)
unpackComplex dout = field re :+ field im
 where
  (im, re) = split @_ @(ByteWidth n) @(ByteWidth n) dout
  -- Truncating the padding of a 'BitVector'; the IP sign-extends its outputs
  field = unpack . resize

-- | Options of the Complex Multiplier, see 'cmpyInst'
cmpyWizard ::
  forall d a b o.
  (KnownNat d, KnownNat a, KnownNat b, KnownNat o) =>
  Config ->
  -- | Value of @CONFIG.RoundMode@
  String ->
  XilinxWizard 11
cmpyWizard cfg roundMode =
  XilinxWizard
    { wiz_name = "cmpy"
    , wiz_vendor = "xilinx.com"
    , wiz_library = "ip"
    , wiz_version = "6.0"
    , wiz_options =
           ("CONFIG.APortWidth",     IntegerOpt (natToNum @a))
        :> ("CONFIG.BPortWidth",     IntegerOpt (natToNum @b))
        :> ("CONFIG.OutputWidth",    IntegerOpt (natToNum @o))
        :> ("CONFIG.RoundMode",      StrOpt roundMode)
        :> ("CONFIG.FlowControl",    StrOpt "NonBlocking")
        :> ("CONFIG.ACLKEN",         BoolOpt True)
        :> ("CONFIG.ARESETN",        BoolOpt False)
        :> ("CONFIG.LatencyConfig",  StrOpt "Manual")
        :> ("CONFIG.MinimumLatency", IntegerOpt (natToNum @d))
        :> configOpts cfg
    }
{-# INLINE cmpyWizard #-}

-- | Instantiate the Complex Multiplier with truncation. It is always
-- configured as:
--
-- * Non-blocking AXI4-Stream interfaces, always valid inputs
-- * A clock enable (@aclken@), no reset
-- * Manual latency, set to @d@
-- * No @TLAST@ or @TUSER@
--
-- Gives the output's @TDATA@.
cmpyInst ::
  forall d a b o dom.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat a
  , KnownNat b
  , KnownNat o
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Operand A, packed with 'packComplex'
  Signal dom (BitVector (2 * ByteWidth a)) ->
  -- | Operand B, packed with 'packComplex'
  Signal dom (BitVector (2 * ByteWidth b)) ->
  Signal dom (BitVector (2 * ByteWidth o))
cmpyInst cfg clk en aData bData = unPort tdata
 where
  (_tvalid, tdata) = go

  go ::
    ( Port "m_axis_dout_tvalid" dom Bit
    , Port "m_axis_dout_tdata" dom (BitVector (2 * ByteWidth o))
    )
  go =
    instWithXilinxWizard
      (instConfig "complexMultiply")
      (cmpyWizard @d @a @b @o cfg "Truncate")
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_a_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_a_tdata" aData)
      (Port @"s_axis_b_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_b_tdata" bData)
{-# INLINE cmpyInst #-}

-- | Like 'cmpyInst', with random rounding. The round carry goes to bit 0 of
-- the @TDATA@ of the IP's CTRL channel.
cmpyRoundInst ::
  forall d a b o dom.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat a
  , KnownNat b
  , KnownNat o
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Round carry
  Signal dom Bit ->
  -- | Operand A, packed with 'packComplex'
  Signal dom (BitVector (2 * ByteWidth a)) ->
  -- | Operand B, packed with 'packComplex'
  Signal dom (BitVector (2 * ByteWidth b)) ->
  Signal dom (BitVector (2 * ByteWidth o))
cmpyRoundInst cfg clk en cy aData bData = unPort tdata
 where
  (_tvalid, tdata) = go

  go ::
    ( Port "m_axis_dout_tvalid" dom Bit
    , Port "m_axis_dout_tdata" dom (BitVector (2 * ByteWidth o))
    )
  go =
    instWithXilinxWizard
      (instConfig "complexMultiplyRound")
      (cmpyWizard @d @a @b @o cfg "Random_Rounding")
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_a_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_a_tdata" aData)
      (Port @"s_axis_b_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_b_tdata" bData)
      (Port @"s_axis_ctrl_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_ctrl_tdata" (zeroExtend . pack <$> cy :: Signal dom (BitVector 8)))
{-# INLINE cmpyRoundInst #-}
