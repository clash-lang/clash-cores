{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx CORDIC LogiCORE IP v6.0](https://docs.amd.com/v/u/en-US/pg105-cordic),
with explicit clock and enable arguments. See "Clash.Cores.Xilinx.Cordic" for
the documentation.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Cordic.Explicit
  ( -- * Instantiating IP
    rotateWith
  , rotate
  , RotateDefDelay
  , translateWith
  , translate
  , TranslateDefDelay
  , sinCosWith
  , sinCos
  , SinCosDefDelay
  , arcTanWith
  , arcTan
  , ArcTanDefDelay
    -- * Customizing IP
  , Config(..)
  , defConfig
  , RoundMode(..)
  , PhaseFormat(..)
  , CompensationScaling(..)
    -- * Latency
  , latencyRotate
  , latencyTranslate
  , latencySinCos
  , latencyArcTan
  ) where

import Clash.Explicit.Prelude hiding (rotate)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Xilinx.Cordic.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import qualified Clash.Signal.Delayed as D

-- | Width of a field in TDATA: rounded up to a multiple of 8
type Pad8 w = 8 * DivRU w 8

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

toIntegerSF :: forall int frac. (KnownNat int, KnownNat frac) => SFixed int frac -> Integer
toIntegerSF = toInteger . unSF

fromIntegerSF :: forall int frac. (KnownNat int, KnownNat frac) => Integer -> SFixed int frac
fromIntegerSF = sf (SNat @frac) . fromInteger

-- | Pack a field into TDATA
padField :: (BitPack a, KnownNat (BitSize a), KnownNat m) => a -> BitVector m
padField = resize . pack

-- | Unpack a field from TDATA
unpadField :: (BitPack a, KnownNat (BitSize a), KnownNat m) => BitVector m -> a
unpadField = unpack . resize

-- | Pack an X and a Y field into TDATA: X in the least significant bits
packXY ::
  forall w a.
  (BitPack a, KnownNat (BitSize a), KnownNat w) =>
  (a, a) ->
  BitVector (Pad8 w + Pad8 w)
packXY (x, y) = padField @_ @(Pad8 w) y ++# padField @_ @(Pad8 w) x

-- | Unpack two fields from TDATA: the first in the least significant bits
unpackPair ::
  forall w a b.
  ( BitPack a, KnownNat (BitSize a), BitPack b, KnownNat (BitSize b)
  , KnownNat w ) =>
  BitVector (Pad8 w + Pad8 w) ->
  (a, b)
unpackPair bv = (unpadField lo, unpadField hi)
 where
  (hi, lo) = split @_ @(Pad8 w) @(Pad8 w) bv

-- | Check that the delay in the type matches the latency of the IP
checkedLatency :: forall d. (KnownNat d, HasCallStack) => Params -> Int
checkedLatency params
  | natToNum @d == lat = lat
  | otherwise = withFrozenCallStack $ error $
         "Clash.Cores.Xilinx.Cordic: the delay of the result (" <> show (natToNum @d :: Int)
      <> ") does not match the latency of the Xilinx CORDIC IP (" <> show lat
      <> ") for configuration " <> show params
 where
  lat = latency params

-- | Options of the Xilinx wizard common to all functions. Note that Clash
-- evaluates these when generating HDL, so they must be simple: no recursion.
commonOptions ::
  CordicFunction ->
  Config ->
  -- | Input width
  Int ->
  -- | Output width
  Int ->
  Vec 11 (OptionName, XilinxWizardOption)
commonOptions function cfg wIn wOut =
     ("CONFIG.Functional_Selection",        StrOpt (functionToTcl function))
  :> ("CONFIG.Architectural_Configuration", StrOpt "Parallel")
  :> ("CONFIG.Pipelining_Mode",             StrOpt "Maximum")
  :> ("CONFIG.Input_Width",                 IntegerOpt (toInteger wIn))
  :> ("CONFIG.Output_Width",                IntegerOpt (toInteger wOut))
  :> ("CONFIG.Round_Mode",                  StrOpt (roundModeToTcl (roundMode cfg)))
     -- Zero lets the IP pick the number of iterations and the precision, as
     -- the simulation model does.
  :> ("CONFIG.Iterations",                  IntegerOpt (explicitly (iterations cfg)))
  :> ("CONFIG.Precision",                   IntegerOpt (explicitly (precision cfg)))
  :> ("CONFIG.ACLKEN",                      BoolOpt True)
  :> ("CONFIG.flow_control",                StrOpt "NonBlocking")
  :> ("CONFIG.out_tready",                  BoolOpt False)
  :> Nil
 where
  explicitly = maybe 0 toInteger

-- | Options for functions with a circular phase input or output
circularOptions :: Config -> Vec 2 (OptionName, XilinxWizardOption)
circularOptions cfg =
     ("CONFIG.Phase_Format",    StrOpt (phaseFormatToTcl (phaseFormat cfg)))
  :> ("CONFIG.Coarse_Rotation", BoolOpt (coarseRotation cfg))
  :> Nil

wizard :: Vec n (OptionName, XilinxWizardOption) -> XilinxWizard n
wizard opts = XilinxWizard
  { wiz_name = "cordic"
  , wiz_vendor = "xilinx.com"
  , wiz_library = "ip"
  , wiz_version = "6.0"
  , wiz_options = opts
  }

-- | Instantiate the IP with a phase input
phaseInst ::
  forall wIn wOutT dom n a.
  (KnownDomain dom, KnownNat wIn, KnownNat wOutT, KnownNat n) =>
  String ->
  Vec n (OptionName, XilinxWizardOption) ->
  Clock dom ->
  Enable dom ->
  Signal dom a ->
  (a -> BitVector (Pad8 wIn)) ->
  Signal dom (BitVector wOutT)
phaseInst nm opts clk en inp packIn = unPort (snd go)
 where
  go ::
    ( Port "m_axis_dout_tvalid" dom Bit
    , Port "m_axis_dout_tdata" dom (BitVector wOutT)
    )
  go =
    instWithXilinxWizard
      (instConfig nm)
      (wizard opts)
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_phase_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_phase_tdata" (packIn <$> inp))
{-# INLINE phaseInst #-}

-- | Instantiate the IP with a Cartesian input
cartesianInst ::
  forall wInT wOutT dom n a.
  (KnownDomain dom, KnownNat wInT, KnownNat wOutT, KnownNat n) =>
  String ->
  Vec n (OptionName, XilinxWizardOption) ->
  Clock dom ->
  Enable dom ->
  Signal dom a ->
  (a -> BitVector wInT) ->
  Signal dom (BitVector wOutT)
cartesianInst nm opts clk en inp packIn = unPort (snd go)
 where
  go ::
    ( Port "m_axis_dout_tvalid" dom Bit
    , Port "m_axis_dout_tdata" dom (BitVector wOutT)
    )
  go =
    instWithXilinxWizard
      (instConfig nm)
      (wizard opts)
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_cartesian_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_cartesian_tdata" (packIn <$> inp))
{-# INLINE cartesianInst #-}

------------------------------------------------------------------------------
-- Rotate
------------------------------------------------------------------------------

-- | Latency of 'rotateWith' for a configuration, input width and output width
latencyRotate :: Config -> Int -> Int -> Int
latencyRotate cfg wIn wOut = latency (mkParams Rotate cfg wIn wOut)

-- | Customizable vector rotation: rotate the vector @(x, y)@ by a phase. The
-- input width of the IP is @fIn + 3@, the output width @fOut + 3@: X and Y
-- have one more fractional bit than the phase.
rotateWith ::
  forall d dom n fIn fOut.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Vector @(x, y)@
  DSignal dom n (SFixed 2 (fIn + 1), SFixed 2 (fIn + 1)) ->
  -- | Phase
  DSignal dom n (SFixed 3 fIn) ->
  -- | Rotated vector
  DSignal dom (n + d) (SFixed 2 (fOut + 1), SFixed 2 (fOut + 1))
rotateWith cfg clk en xy phase
  | clashSimulation = sim
  | otherwise = D.unsafeFromSignal synth
 where
  params = mkParams Rotate cfg (natToNum @(fIn + 3)) (natToNum @(fOut + 3))
  model = modelRotate params
  f (x, y) ph =
    let (x', y') = model (toIntegerSF x, toIntegerSF y) (toIntegerSF ph)
    in (fromIntegerSF x', fromIntegerSF y')
  sim = checkedLatency @d params `seq` delayI und en clk (f <$> xy <*> phase)
  und = withFrozenCallStack $ deepErrorX "Initial values of rotate undefined"
  opts =
       commonOptions Rotate cfg (natToNum @(fIn + 3)) (natToNum @(fOut + 3))
    ++ circularOptions cfg
    ++ (("CONFIG.Compensation_Scaling", StrOpt (compensationScalingToTcl (compensationScaling cfg))) :> Nil)
  synth = unpackPair @(fOut + 3) <$> unPort (snd go)
  go ::
    ( Port "m_axis_dout_tvalid" dom Bit
    , Port "m_axis_dout_tdata" dom (BitVector (Pad8 (fOut + 3) + Pad8 (fOut + 3)))
    )
  go =
    instWithXilinxWizard
      (instConfig "rotate")
      (wizard opts)
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_phase_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_phase_tdata" (padField @_ @(Pad8 (fIn + 3)) <$> D.toSignal phase))
      (Port @"s_axis_cartesian_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_cartesian_tdata" (packXY @(fIn + 3) <$> D.toSignal xy))
{-# INLINE rotateWith #-}

-- | Vector rotation with default settings
rotate ::
  forall dom n fIn fOut.
  ( KnownDomain dom
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n (SFixed 2 (fIn + 1), SFixed 2 (fIn + 1)) ->
  DSignal dom n (SFixed 3 fIn) ->
  DSignal dom (n + RotateDefDelay fOut) (SFixed 2 (fOut + 1), SFixed 2 (fOut + 1))
rotate = withFrozenCallStack $ rotateWith defConfig
{-# INLINE rotate #-}

-- | Latency of 'rotate' for an output of @fOut + 3@ bits: the number of
-- iterations (two less than the output width) plus six.
type RotateDefDelay fOut = fOut + 3 + 4

------------------------------------------------------------------------------
-- Translate
------------------------------------------------------------------------------

-- | Latency of 'translateWith' for a configuration, input width and output
-- width
latencyTranslate :: Config -> Int -> Int -> Int
latencyTranslate cfg wIn wOut = latency (mkParams Translate cfg wIn wOut)

-- | Customizable vector translation: compute the magnitude and phase of the
-- vector @(x, y)@. The input width of the IP is @fIn + 2@, the output width
-- @fOut + 3@: the magnitude has one more fractional bit than the phase.
translateWith ::
  forall d dom n fIn fOut.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Vector @(x, y)@
  DSignal dom n (SFixed 2 fIn, SFixed 2 fIn) ->
  -- | Magnitude and phase
  DSignal dom (n + d) (SFixed 2 (fOut + 1), SFixed 3 fOut)
translateWith cfg clk en xy
  | clashSimulation = sim
  | otherwise = D.unsafeFromSignal synth
 where
  params = mkParams Translate cfg (natToNum @(fIn + 2)) (natToNum @(fOut + 3))
  model = modelTranslate params
  f (x, y) =
    let (m, ph) = model (toIntegerSF x, toIntegerSF y)
    in (fromIntegerSF m, fromIntegerSF ph)
  sim = checkedLatency @d params `seq` delayI und en clk (f <$> xy)
  und = withFrozenCallStack $ deepErrorX "Initial values of translate undefined"
  opts =
       commonOptions Translate cfg (natToNum @(fIn + 2)) (natToNum @(fOut + 3))
    ++ circularOptions cfg
    ++ (("CONFIG.Compensation_Scaling", StrOpt (compensationScalingToTcl (compensationScaling cfg))) :> Nil)
  synth =
    unpackPair @(fOut + 3) <$>
      cartesianInst @_ @(Pad8 (fOut + 3) + Pad8 (fOut + 3))
        "translate" opts clk en (D.toSignal xy) (packXY @(fIn + 2))
{-# INLINE translateWith #-}

-- | Vector translation with default settings
translate ::
  forall dom n fIn fOut.
  ( KnownDomain dom
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n (SFixed 2 fIn, SFixed 2 fIn) ->
  DSignal dom (n + TranslateDefDelay fOut) (SFixed 2 (fOut + 1), SFixed 3 fOut)
translate = withFrozenCallStack $ translateWith defConfig
{-# INLINE translate #-}

-- | Latency of 'translate' for an output of @fOut + 3@ bits
type TranslateDefDelay fOut = fOut + 3 + 4

------------------------------------------------------------------------------
-- Sin and Cos
------------------------------------------------------------------------------

-- | Latency of 'sinCosWith' for a configuration, input width and output width
latencySinCos :: Config -> Int -> Int -> Int
latencySinCos cfg wIn wOut = latency (mkParams SinCos cfg wIn wOut)

-- | Customizable sine and cosine. Returns @(cos phase, sin phase)@. The input
-- width of the IP is @fIn + 3@, the output width @fOut + 2@.
sinCosWith ::
  forall d dom n fIn fOut.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Phase
  DSignal dom n (SFixed 3 fIn) ->
  -- | Cosine and sine
  DSignal dom (n + d) (SFixed 2 fOut, SFixed 2 fOut)
sinCosWith cfg clk en phase
  | clashSimulation = sim
  | otherwise = D.unsafeFromSignal synth
 where
  params = mkParams SinCos cfg (natToNum @(fIn + 3)) (natToNum @(fOut + 2))
  model = modelSinCos params
  f ph =
    let (c, s) = model (toIntegerSF ph)
    in (fromIntegerSF c, fromIntegerSF s)
  sim = checkedLatency @d params `seq` delayI und en clk (f <$> phase)
  und = withFrozenCallStack $ deepErrorX "Initial values of sinCos undefined"
  opts = commonOptions SinCos cfg (natToNum @(fIn + 3)) (natToNum @(fOut + 2)) ++ circularOptions cfg
  synth =
    unpackPair @(fOut + 2) <$>
      phaseInst @(fIn + 3) @(Pad8 (fOut + 2) + Pad8 (fOut + 2))
        "sinCos" opts clk en (D.toSignal phase) padField
{-# INLINE sinCosWith #-}

-- | Sine and cosine with default settings. Returns @(cos phase, sin phase)@.
sinCos ::
  forall dom n fIn fOut.
  ( KnownDomain dom
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n (SFixed 3 fIn) ->
  DSignal dom (n + SinCosDefDelay fOut) (SFixed 2 fOut, SFixed 2 fOut)
sinCos = withFrozenCallStack $ sinCosWith defConfig
{-# INLINE sinCos #-}

-- | Latency of 'sinCos' for an output of @fOut + 2@ bits
type SinCosDefDelay fOut = fOut + 2 + 4

------------------------------------------------------------------------------
-- Arc tan
------------------------------------------------------------------------------

-- | Latency of 'arcTanWith' for a configuration, input width and output width
latencyArcTan :: Config -> Int -> Int -> Int
latencyArcTan cfg wIn wOut = latency (mkParams ArcTan cfg wIn wOut)

-- | Customizable arctangent: the angle of the vector @(x, y)@, @atan2(y, x)@.
-- The input width of the IP is @fIn + 2@, the output width @fOut + 3@.
arcTanWith ::
  forall d dom n fIn fOut.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  Config ->
  Clock dom ->
  Enable dom ->
  -- | Vector @(x, y)@
  DSignal dom n (SFixed 2 fIn, SFixed 2 fIn) ->
  -- | Phase
  DSignal dom (n + d) (SFixed 3 fOut)
arcTanWith cfg clk en xy
  | clashSimulation = sim
  | otherwise = D.unsafeFromSignal synth
 where
  params = mkParams ArcTan cfg (natToNum @(fIn + 2)) (natToNum @(fOut + 3))
  model = modelArcTan params
  f (x, y) = fromIntegerSF (model (toIntegerSF x, toIntegerSF y))
  sim = checkedLatency @d params `seq` delayI und en clk (f <$> xy)
  und = withFrozenCallStack $ deepErrorX "Initial values of arcTan undefined"
  opts = commonOptions ArcTan cfg (natToNum @(fIn + 2)) (natToNum @(fOut + 3)) ++ circularOptions cfg
  synth =
    unpadField <$>
      cartesianInst @_ @(Pad8 (fOut + 3))
        "arcTan" opts clk en (D.toSignal xy) (packXY @(fIn + 2))
{-# INLINE arcTanWith #-}

-- | Arctangent with default settings
arcTan ::
  forall dom n fIn fOut.
  ( KnownDomain dom
  , KnownNat fIn
  , KnownNat fOut
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n (SFixed 2 fIn, SFixed 2 fIn) ->
  DSignal dom (n + ArcTanDefDelay fOut) (SFixed 3 fOut)
arcTan = withFrozenCallStack $ arcTanWith defConfig
{-# INLINE arcTan #-}

-- | Latency of 'arcTan' for an output of @fOut + 3@ bits
type ArcTanDefDelay fOut = fOut + 3 + 4
