{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx DDS Compiler LogiCORE IP v6.0](https://docs.amd.com/v/u/en-US/pg141-dds-compiler),
with explicit clock and enable arguments. See "Clash.Cores.Xilinx.Dds" for
the documentation.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Dds.Explicit
  ( -- * Instantiating IP
    ddsWith
  , dds
  , phaseGeneratorWith
  , phaseGenerator
  , sinCosLutWith
  , sinCos
    -- * Phases and samples
  , Phase
  , Sample
    -- * Phase increment and offset
  , Increment(..)
  , Offset(..)
  , ConfigUpdate
  , PhaseInput
    -- * Outputs
  , Output(..)
    -- * Customizing IP
  , Config(..)
  , defConfig
  , NoiseShaping(..)
  , AmplitudeMode(..)
  , MemoryType(..)
    -- * Field packing
  , Field(..)
  , IncrementField(..)
  , DataOutput(..)
  , PhaseChannel
  , PhaseInputWidth
  , Pad8
  ) where

import Clash.Explicit.Prelude

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Xilinx.Dds.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import qualified Clash.Signal.Delayed as D

-- | A phase with @pw@ bits, as a fraction of a full cycle: @0.25@ is a quarter
-- of a cycle (90 degrees, or pi\/2 radians). Phases wrap around, so the range
-- @[-0.5, 0.5)@ of 'SFixed' @0@ @pw@ covers the full circle. This is the
-- type of the phase increment (in cycles per sample: the output frequency
-- divided by the clock frequency), the phase offset, and the phase output of
-- the phase generator.
type Phase pw = SFixed 0 pw

-- | A sine or cosine sample with @f@ fractional bits: an output of @f + 1@
-- bits, in @[-1, 1)@.
type Sample f = SFixed 1 f

-- | Width of a field in TDATA: rounded up to a multiple of 8
type Pad8 w = 8 * DivRU w 8

------------------------------------------------------------------------------
-- Phase increment, phase offset and outputs
------------------------------------------------------------------------------

-- | How the phase generator gets its phase increment (PINC), see /Phase
-- Increment Programmability/ in PG141. The increment is the frequency of the
-- output, in cycles per sample.
--
-- The type parameters are the type of the increment in the input PHASE channel
-- ('PhaseInput') and in the CONFIG channel ('ConfigUpdate'). They are @()@ for
-- options that do not use those channels.
data Increment pw stream cfg where
  -- | A fixed increment
  FixedIncrement :: Phase pw -> Increment pw () ()
  -- | An increment that can be changed through the CONFIG channel, with its
  -- initial value
  ProgrammableIncrement :: Phase pw -> Increment pw () (Phase pw)
  -- | A new increment for every sample, from the input PHASE channel
  StreamingIncrement :: Increment pw (Phase pw) ()
  -- | A new increment for every sample, from the input PHASE channel, and a
  -- resynchronization flag. If the flag is set, the accumulated phase is
  -- reset: the accumulated phase of that sample is its increment.
  StreamingIncrementResync :: Increment pw (Phase pw, Bool) ()

-- | How the phase generator gets its phase offset (POFF), see /Phase Offset
-- Programmability/ in PG141. The offset is added to the accumulated phase.
--
-- The type parameters are the type of the offset in the input PHASE channel
-- ('PhaseInput') and in the CONFIG channel ('ConfigUpdate'). They are @()@ for
-- options that do not use those channels.
data Offset pw stream cfg where
  -- | No phase offset
  NoOffset :: Offset pw () ()
  -- | A fixed offset
  FixedOffset :: Phase pw -> Offset pw () ()
  -- | An offset that can be changed through the CONFIG channel, with its
  -- initial value
  ProgrammableOffset :: Phase pw -> Offset pw () (Phase pw)
  -- | A new offset for every sample, from the input PHASE channel
  StreamingOffset :: Offset pw (Phase pw) ()

-- | An update of the programmable phase increment and offset: @(increment,
-- offset)@. The types are those of the second type parameter of 'Increment'
-- and 'Offset', so @()@ for one that is not programmable.
type ConfigUpdate incCfg offCfg = (incCfg, offCfg)

-- | Input of the phase generator for one sample: @(increment, offset)@. The
-- types are those of the first type parameter of 'Increment' and 'Offset',
-- so @()@ for one that is not streaming.
type PhaseInput incStream offStream = (incStream, offStream)

-- | Which of the sine and cosine the IP outputs, see /Output Selection/ in
-- PG141. The type parameter is the type of the output.
data Output f o where
  -- | Only the sine
  SineOutput :: Output f (Sample f)
  -- | Only the cosine
  CosineOutput :: Output f (Sample f)
  -- | Both, as @(cosine, sine)@: the real and imaginary part of
  -- @e^(i phase)@
  SineCosineOutput :: Output f (Sample f, Sample f)

------------------------------------------------------------------------------
-- TDATA fields
------------------------------------------------------------------------------

-- | A field of the input PHASE channel or the CONFIG channel. Fields are
-- padded to a multiple of 8 bits; @()@ is a field that does not exist.
class KnownNat (FieldWidth a) => Field a where
  -- | Width of the field in TDATA
  type FieldWidth a :: Nat
  -- | Raw value of the field
  packField :: a -> BitVector (FieldWidth a)

instance Field () where
  type FieldWidth () = 0
  packField _ = 0

instance KnownNat pw => Field (SFixed 0 pw) where
  type FieldWidth (SFixed 0 pw) = Pad8 pw
  packField = resize . pack

instance Field Bool where
  type FieldWidth Bool = 8
  packField = resize . pack

-- | The streaming phase increment ('Increment'): a phase increment field and
-- a resynchronization field.
class (Field (IncrementPart a), Field (ResyncPart a)) => IncrementField a where
  -- | Type of the phase increment field
  type IncrementPart a
  -- | Type of the resynchronization field
  type ResyncPart a
  -- | Split into the phase increment and resynchronization fields
  splitIncrement :: a -> (IncrementPart a, ResyncPart a)

instance IncrementField () where
  type IncrementPart () = ()
  type ResyncPart () = ()
  splitIncrement _ = ((), ())

instance KnownNat pw => IncrementField (SFixed 0 pw) where
  type IncrementPart (SFixed 0 pw) = SFixed 0 pw
  type ResyncPart (SFixed 0 pw) = ()
  splitIncrement x = (x, ())

instance KnownNat pw => IncrementField (SFixed 0 pw, Bool) where
  type IncrementPart (SFixed 0 pw, Bool) = SFixed 0 pw
  type ResyncPart (SFixed 0 pw, Bool) = Bool
  splitIncrement = id

-- | Width of TDATA of the input PHASE channel
type PhaseInputWidth incStream offStream =
  FieldWidth (ResyncPart incStream) + FieldWidth offStream + FieldWidth (IncrementPart incStream)

-- | Constraints on the types of the input PHASE channel and the CONFIG
-- channel
type PhaseChannel incStream offStream incCfg offCfg =
  ( IncrementField incStream
  , Field offStream
  , Field incCfg
  , Field offCfg
  , KnownNat (PhaseInputWidth incStream offStream)
  , KnownNat (FieldWidth offCfg + FieldWidth incCfg)
  )

-- | TDATA of the input PHASE channel: from least to most significant, the
-- increment, the offset and the resynchronization flag, each padded to a
-- multiple of 8 bits.
packPhaseInput ::
  (IncrementField incStream, Field offStream) =>
  PhaseInput incStream offStream ->
  BitVector (PhaseInputWidth incStream offStream)
packPhaseInput (i, o) = packField r ++# packField o ++# packField inc
 where
  (inc, r) = splitIncrement i

-- | TDATA of the CONFIG channel: the increment in the least significant bits,
-- then the offset.
packConfig ::
  (Field incCfg, Field offCfg) =>
  ConfigUpdate incCfg offCfg ->
  BitVector (FieldWidth offCfg + FieldWidth incCfg)
packConfig (i, o) = packField o ++# packField i

-- | The output DATA channel
class (KnownNat (DataWidth o), NFDataX o) => DataOutput o where
  -- | Width of TDATA of the output DATA channel
  type DataWidth o :: Nat
  -- | Unpack TDATA of the output DATA channel
  unpackData :: BitVector (DataWidth o) -> o

instance KnownNat f => DataOutput (SFixed 1 f) where
  type DataWidth (SFixed 1 f) = Pad8 (f + 1)
  unpackData = unpack . resize

instance KnownNat f => DataOutput (SFixed 1 f, SFixed 1 f) where
  type DataWidth (SFixed 1 f, SFixed 1 f) = Pad8 (f + 1) + Pad8 (f + 1)
  unpackData bv = (unpack (resize lo), unpack (resize hi))
   where
    (hi, lo) = split @_ @(Pad8 (f + 1)) @(Pad8 (f + 1)) bv

------------------------------------------------------------------------------
-- Model helpers
------------------------------------------------------------------------------

incrementSourceOf :: Increment pw s c -> PhaseSource
incrementSourceOf = \case
  FixedIncrement _ -> FixedSource
  ProgrammableIncrement _ -> ProgrammableSource
  StreamingIncrement -> StreamingSource
  StreamingIncrementResync -> StreamingSource

offsetSourceOf :: Offset pw s c -> PhaseSource
offsetSourceOf = \case
  NoOffset -> NoSource
  FixedOffset _ -> FixedSource
  ProgrammableOffset _ -> ProgrammableSource
  StreamingOffset -> StreamingSource

-- | Value of the increment for the wizard: the fixed or initial value
incrementValue :: Increment pw s c -> Maybe (Phase pw)
incrementValue = \case
  FixedIncrement v -> Just v
  ProgrammableIncrement v -> Just v
  _ -> Nothing

offsetValue :: Offset pw s c -> Maybe (Phase pw)
offsetValue = \case
  FixedOffset v -> Just v
  ProgrammableOffset v -> Just v
  _ -> Nothing

-- | The increment and resynchronization flag of a sample, given the
-- programmed increment
incrementOf :: KnownNat pw => Increment pw s c -> Phase pw -> s -> (Phase pw, Bool)
incrementOf inc programmed s = case inc of
  FixedIncrement v -> (v, False)
  ProgrammableIncrement _ -> (programmed, False)
  StreamingIncrement -> (s, False)
  StreamingIncrementResync -> s

offsetOf :: KnownNat pw => Offset pw s c -> Phase pw -> s -> Phase pw
offsetOf off programmed s = case off of
  NoOffset -> 0
  FixedOffset v -> v
  ProgrammableOffset _ -> programmed
  StreamingOffset -> s

-- | Update the programmed increment from the CONFIG channel
programIncrement :: Increment pw s c -> Phase pw -> c -> Phase pw
programIncrement inc old c = case inc of
  ProgrammableIncrement _ -> c
  _ -> old

programOffset :: Offset pw s c -> Phase pw -> c -> Phase pw
programOffset off old c = case off of
  ProgrammableOffset _ -> c
  _ -> old

toRaw :: forall pw. KnownNat pw => Phase pw -> Integer
toRaw = toInteger . (bitCoerce :: Phase pw -> Unsigned pw)

fromRaw :: forall pw. KnownNat pw => Integer -> Phase pw
fromRaw = (bitCoerce :: Unsigned pw -> Phase pw) . fromInteger

toSample :: forall f. KnownNat f => Integer -> Sample f
toSample = sf (SNat @f) . fromInteger

selectOutput :: Output f o -> (Sample f, Sample f) -> o
selectOutput out (c, s) = case out of
  SineOutput -> s
  CosineOutput -> c
  SineCosineOutput -> (c, s)

-- | Simulation model of the phase generator: the output phase of each sample
phaseGeneratorModel ::
  forall dom pw is ic os oc.
  (KnownDomain dom, KnownNat pw) =>
  Increment pw is ic ->
  Offset pw os oc ->
  Clock dom ->
  Enable dom ->
  Signal dom (Maybe (ConfigUpdate ic oc)) ->
  Signal dom (PhaseInput is os) ->
  Signal dom (Phase pw)
phaseGeneratorModel inc off clk en cfgIn input = phase
 where
  pw = natToNum @pw
  initial = (fromMaybe 0 (incrementValue inc), fromMaybe 0 (offsetValue off))
  -- Programmed increment and offset: updated by the CONFIG channel, used from
  -- the next cycle on
  programmed = delay clk en initial (update <$> programmed <*> cfgIn)
  update p Nothing = p
  update (pi0, po0) (Just (ci, co)) = (programIncrement inc pi0 ci, programOffset off po0 co)
  acc = delay clk en (0 :: Integer) (fst <$> step)
  phase = fromRaw . snd <$> step
  step = go <$> acc <*> programmed <*> input
  go a (pinc0, poff0) (is, os) =
    let (pinc, resync) = incrementOf inc pinc0 is
        poff = offsetOf off poff0 os
    in accumulate pw a (toRaw pinc) (toRaw poff) resync

-- | Check that the IP supports a configuration, and that the delay in the
-- type is a latency the IP supports for it
checkedParams :: forall d. (KnownNat d, HasCallStack) => Params -> ()
checkedParams params
  | Just msg <- checkWidths params = withFrozenCallStack $ err msg
  | lat < lo || lat > hi = withFrozenCallStack $ err $
         "the delay of the result (" <> show lat <> ") must be between " <> show lo
      <> " and " <> show hi <> ", the latencies the IP supports"
  | otherwise = ()
 where
  err msg = error $
    "Clash.Cores.Xilinx.Dds: unsupported configuration of the Xilinx DDS Compiler IP: "
      <> msg <> ". Configuration: " <> show params
  lat = natToNum @d :: Int
  (lo, hi) = latencyRange params

-- | Range of latencies the IP supports for a configuration
latencyRange :: Params -> (Int, Int)
latencyRange params = (minLatency params, maxLatency)

------------------------------------------------------------------------------
-- Instantiation
------------------------------------------------------------------------------

wizard :: Vec n (OptionName, XilinxWizardOption) -> XilinxWizard n
wizard opts = XilinxWizard
  { wiz_name = "dds_compiler"
  , wiz_vendor = "xilinx.com"
  , wiz_library = "ip"
  , wiz_version = "6.0"
  , wiz_options = opts
  }

-- | Options common to all configurations. Note that Clash evaluates these
-- when generating HDL.
commonOptions ::
  forall d.
  KnownNat d =>
  PartsPresent ->
  -- | Phase width
  Int ->
  -- | Output width
  Int ->
  Vec 9 (OptionName, XilinxWizardOption)
commonOptions parts_ pw ow =
     ("CONFIG.PartsPresent",          StrOpt (partsPresentToTcl parts_))
  :> ("CONFIG.Parameter_Entry",       StrOpt "Hardware_Parameters")
  :> ("CONFIG.Phase_Width",           IntegerOpt (toInteger pw))
  :> ("CONFIG.Output_Width",          IntegerOpt (toInteger ow))
  :> ("CONFIG.Latency_Configuration", StrOpt "Configurable")
  :> ("CONFIG.Latency",               IntegerOpt (natToNum @d))
  :> ("CONFIG.Has_ACLKEN",            BoolOpt True)
  :> ("CONFIG.Has_TREADY",            BoolOpt False)
  :> ("CONFIG.Has_ARESETn",           BoolOpt False)
  :> Nil

-- | Options for the SIN\/COS lookup table
lutOptions :: Config -> Output f o -> Vec 6 (OptionName, XilinxWizardOption)
lutOptions cfg out =
     ("CONFIG.Noise_Shaping",    StrOpt (noiseShapingToTcl (noiseShaping cfg)))
  :> ("CONFIG.Output_Selection", StrOpt outSel)
  :> ("CONFIG.Negative_Sine",    BoolOpt (negativeSine cfg))
  :> ("CONFIG.Negative_Cosine",  BoolOpt (negativeCosine cfg))
  :> ("CONFIG.Amplitude_Mode",   StrOpt (amplitudeModeToTcl (amplitudeMode cfg)))
  :> ("CONFIG.Memory_Type",      StrOpt (memoryTypeToTcl (memoryType cfg)))
  :> Nil
 where
  outSel = case out of
    SineOutput -> "Sine"
    CosineOutput -> "Cosine"
    SineCosineOutput -> "Sine_and_Cosine"

-- | Options for the phase generator
phaseGenOptions ::
  forall pw is ic os oc.
  KnownNat pw =>
  Increment pw is ic ->
  Offset pw os oc ->
  Vec 6 (OptionName, XilinxWizardOption)
phaseGenOptions inc off =
     ("CONFIG.Phase_Increment", StrOpt (phaseSourceToTcl (incrementSourceOf inc)))
  :> ("CONFIG.Resync",          BoolOpt resync)
  :> ("CONFIG.Phase_offset",    StrOpt (phaseSourceToTcl (offsetSourceOf off)))
     -- The IP checks that these fit the phase width, also if they are unused
  :> ("CONFIG.PINC1",           BinaryOpt pw (maybe 0 toRaw (incrementValue inc)))
  :> ("CONFIG.POFF1",           BinaryOpt pw (maybe 0 toRaw (offsetValue off)))
  :> ("CONFIG.Channels",        IntegerOpt 1)
  :> Nil
 where
  pw = natToNum @pw :: Integer
  resync = case inc of
    StreamingIncrementResync -> True
    _ -> False

-- | Whether a configuration has an input PHASE channel
hasPhaseInput :: Increment pw is ic -> Offset pw os oc -> Bool
hasPhaseInput inc off = incrementSourceOf inc == StreamingSource || offsetSourceOf off == StreamingSource

-- | Whether a configuration has a CONFIG channel
hasConfigInput :: Increment pw is ic -> Offset pw os oc -> Bool
hasConfigInput inc off = incrementSourceOf inc == ProgrammableSource || offsetSourceOf off == ProgrammableSource

-- | Instantiate the IP with a phase generator, with the given output channel
phaseGenInst ::
  forall outValid outData wOut wPhase wCfg dom n.
  ( KnownDomain dom
  , KnownNat wOut
  , KnownNat wPhase
  , KnownNat wCfg
  , KnownNat n
  , KnownSymbol outValid
  , KnownSymbol outData
  ) =>
  String ->
  Vec n (OptionName, XilinxWizardOption) ->
  -- | Has an input PHASE channel
  Bool ->
  -- | Has a CONFIG channel
  Bool ->
  Clock dom ->
  Enable dom ->
  Signal dom (Bool, BitVector wCfg) ->
  Signal dom (BitVector wPhase) ->
  Signal dom (BitVector wOut)
phaseGenInst nm opts hasPhase hasCfg clk en cfgIn phaseIn
  | hasPhase && hasCfg = unPort (snd goBoth)
  | hasPhase = unPort (snd goPhase)
  | hasCfg = unPort (snd goCfg)
  | otherwise = unPort (snd goNone)
 where
  aclken = Port @"aclken" (boolToBit <$> fromEnable en)
  phaseValid = Port @"s_axis_phase_tvalid" (pure 1 :: Signal dom Bit)
  phaseData = Port @"s_axis_phase_tdata" phaseIn
  cfgValid = Port @"s_axis_config_tvalid" (boolToBit . fst <$> cfgIn)
  cfgData = Port @"s_axis_config_tdata" (snd <$> cfgIn)

  goBoth, goPhase, goCfg, goNone :: (Port outValid dom Bit, Port outData dom (BitVector wOut))
  goBoth =
    instWithXilinxWizard (instConfig nm) (wizard opts)
      (ClockPort @"aclk" clk) aclken cfgValid cfgData phaseValid phaseData
  goPhase =
    instWithXilinxWizard (instConfig nm) (wizard opts)
      (ClockPort @"aclk" clk) aclken phaseValid phaseData
  goCfg =
    instWithXilinxWizard (instConfig nm) (wizard opts)
      (ClockPort @"aclk" clk) aclken cfgValid cfgData
  goNone =
    instWithXilinxWizard (instConfig nm) (wizard opts)
      (ClockPort @"aclk" clk) aclken
{-# INLINE phaseGenInst #-}

------------------------------------------------------------------------------
-- DDS
------------------------------------------------------------------------------

-- | Customizable DDS: a phase generator followed by a SIN\/COS lookup table.
--
-- The phase generator accumulates the phase increment, adds the phase offset,
-- and looks up the sine and cosine of the result. There is one output sample
-- for each sample of the input PHASE channel, which is a sample per clock
-- cycle: if neither the increment nor the offset is streaming, its type is
-- @((), ())@ and it serves as a clock for the output.
--
-- The CONFIG channel changes programmable increments and offsets: a 'Just'
-- in a clock cycle sets the increment and offset of the samples of the
-- following cycles. Its type is @Maybe ((), ())@ if neither is programmable;
-- give it @pure Nothing@.
--
-- The output comes @d@ cycles after the input; see "Clash.Cores.Xilinx.Dds"
-- for the latencies the IP supports.
ddsWith ::
  forall d dom n pw f is ic os oc o.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat pw
  , KnownNat f
  , PhaseChannel is os ic oc
  , DataOutput o
  , HasCallStack
  ) =>
  Config ->
  Increment pw is ic ->
  Offset pw os oc ->
  Output f o ->
  Clock dom ->
  Enable dom ->
  -- | CONFIG channel
  Signal dom (Maybe (ConfigUpdate ic oc)) ->
  -- | Input PHASE channel
  DSignal dom n (PhaseInput is os) ->
  DSignal dom (n + d) o
ddsWith cfg inc off out clk en cfgIn input
  | clashSimulation = sim
  | otherwise = D.unsafeFromSignal synth
 where
  params =
    mkParams PhaseGeneratorAndSinCosLut (incrementSourceOf inc) (offsetSourceOf off) cfg
      (natToNum @pw) (natToNum @(f + 1))
  phases = phaseGeneratorModel inc off clk en cfgIn (D.toSignal input)
  model ph =
    let (c, s) = modelSinCos params (toRaw ph)
    in selectOutput out (toSample c, toSample s)
  sim = checkedParams @d params `seq` delayI @d @n und en clk (D.unsafeFromSignal (model <$> phases))
  und = withFrozenCallStack $ deepErrorX "Initial values of dds undefined"
  opts =
       commonOptions @d PhaseGeneratorAndSinCosLut (natToNum @pw) (natToNum @(f + 1))
    ++ phaseGenOptions inc off
    ++ lutOptions cfg out
    ++ (("CONFIG.Has_Phase_Out", BoolOpt False) :> Nil)
  synth =
    unpackData <$>
      phaseGenInst @"m_axis_data_tvalid" @"m_axis_data_tdata"
        "dds" opts (hasPhaseInput inc off) (hasConfigInput inc off) clk en
        (cfgChannel <$> cfgIn) (packPhaseInput <$> D.toSignal input)
  cfgChannel = \case
    Nothing -> (False, 0)
    Just c -> (True, packConfig c)
{-# INLINE ddsWith #-}

-- | A DDS with a fixed phase increment, no phase offset, and the default
-- configuration: a quadrature oscillator. Returns @(cosine, sine)@. The
-- first @d@ samples are undefined.
dds ::
  forall d dom pw f.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat pw
  , KnownNat f
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  -- | Phase increment: the output frequency divided by the clock frequency
  Phase pw ->
  DSignal dom d (Sample f, Sample f)
dds clk en pinc =
  withFrozenCallStack $
    ddsWith @d @dom @0 defConfig (FixedIncrement pinc) NoOffset SineCosineOutput clk en
      (pure Nothing) (pure ((), ()))
{-# INLINE dds #-}

------------------------------------------------------------------------------
-- Phase generator
------------------------------------------------------------------------------

-- | Customizable phase generator: accumulates the phase increment and adds
-- the phase offset. See 'ddsWith' for the inputs. Returns the phase of each
-- sample.
phaseGeneratorWith ::
  forall d dom n pw is ic os oc.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat pw
  , PhaseChannel is os ic oc
  , KnownNat (Pad8 pw)
  , HasCallStack
  ) =>
  Increment pw is ic ->
  Offset pw os oc ->
  Clock dom ->
  Enable dom ->
  -- | CONFIG channel
  Signal dom (Maybe (ConfigUpdate ic oc)) ->
  -- | Input PHASE channel
  DSignal dom n (PhaseInput is os) ->
  DSignal dom (n + d) (Phase pw)
phaseGeneratorWith inc off clk en cfgIn input
  | clashSimulation = sim
  | otherwise = D.unsafeFromSignal synth
 where
  params =
    mkParams PhaseGeneratorOnly (incrementSourceOf inc) (offsetSourceOf off) defConfig
      (natToNum @pw) 0
  phases = phaseGeneratorModel inc off clk en cfgIn (D.toSignal input)
  sim = checkedParams @d params `seq` delayI @d @n und en clk (D.unsafeFromSignal phases)
  und = withFrozenCallStack $ deepErrorX "Initial values of phaseGenerator undefined"
  opts =
       commonOptions @d PhaseGeneratorOnly (natToNum @pw) 3
    ++ phaseGenOptions inc off
    ++ (("CONFIG.Has_Phase_Out", BoolOpt True) :> Nil)
  synth =
    unpack . resize <$>
      phaseGenInst @"m_axis_phase_tvalid" @"m_axis_phase_tdata" @(Pad8 pw)
        "phaseGenerator" opts (hasPhaseInput inc off) (hasConfigInput inc off) clk en
        (cfgChannel <$> cfgIn) (packPhaseInput <$> D.toSignal input)
  cfgChannel = \case
    Nothing -> (False, 0)
    Just c -> (True, packConfig c)
{-# INLINE phaseGeneratorWith #-}

-- | A phase generator with a fixed phase increment and no phase offset. The
-- first @d@ phases are undefined.
phaseGenerator ::
  forall d dom pw.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat pw
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  -- | Phase increment
  Phase pw ->
  DSignal dom d (Phase pw)
phaseGenerator clk en pinc =
  withFrozenCallStack $
    phaseGeneratorWith @d @dom @0 (FixedIncrement pinc) NoOffset clk en (pure Nothing) (pure ((), ()))
{-# INLINE phaseGenerator #-}

------------------------------------------------------------------------------
-- SIN/COS lookup table
------------------------------------------------------------------------------

-- | Customizable SIN\/COS lookup table: the sine and\/or cosine of a phase.
-- The phase width @pw@ must be between 3 and 16 bits.
sinCosLutWith ::
  forall d dom n pw f o.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat pw
  , KnownNat f
  , DataOutput o
  , HasCallStack
  ) =>
  Config ->
  Output f o ->
  Clock dom ->
  Enable dom ->
  DSignal dom n (Phase pw) ->
  DSignal dom (n + d) o
sinCosLutWith cfg out clk en phase
  | clashSimulation = sim
  | otherwise = D.unsafeFromSignal synth
 where
  params = mkParams SinCosLutOnly FixedSource NoSource cfg (natToNum @pw) (natToNum @(f + 1))
  model ph =
    let (c, s) = modelSinCos params (toRaw ph)
    in selectOutput out (toSample c, toSample s)
  sim = checkedParams @d params `seq` delayI und en clk (model <$> phase)
  und = withFrozenCallStack $ deepErrorX "Initial values of sinCosLut undefined"
  opts =
       commonOptions @d SinCosLutOnly (natToNum @pw) (natToNum @(f + 1))
    ++ lutOptions cfg out
  synth = unpackData <$> unPort (snd go)
  go ::
    ( Port "m_axis_data_tvalid" dom Bit
    , Port "m_axis_data_tdata" dom (BitVector (DataWidth o))
    )
  go =
    instWithXilinxWizard
      (instConfig "sinCosLut")
      (wizard opts)
      (ClockPort @"aclk" clk)
      (Port @"aclken" (boolToBit <$> fromEnable en))
      (Port @"s_axis_phase_tvalid" (pure 1 :: Signal dom Bit))
      (Port @"s_axis_phase_tdata" (packField <$> D.toSignal phase))
{-# INLINE sinCosLutWith #-}

-- | Sine and cosine of a phase with the default configuration. Returns
-- @(cosine, sine)@.
sinCos ::
  forall d dom n pw f.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat pw
  , KnownNat f
  , HasCallStack
  ) =>
  Clock dom ->
  Enable dom ->
  DSignal dom n (Phase pw) ->
  DSignal dom (n + d) (Sample f, Sample f)
sinCos = withFrozenCallStack $ sinCosLutWith @d defConfig SineCosineOutput
{-# INLINE sinCos #-}
