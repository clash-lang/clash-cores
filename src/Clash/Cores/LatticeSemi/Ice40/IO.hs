{-|
  Copyright   :  (C) 2019, Foamspace corp
                     2022-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  LATTICE ICE IO primitives. Implementations are documented in the
  <http://www.latticesemi.com/~/media/LatticeSemi/Documents/TechnicalBriefs/SBTICETechnologyLibrary201504.pdf LATTICE ICE Technology Library>,
  referred to as LITL.

  The simulation models follow the community simulation models of Yosys
  (@share\/yosys\/ice40\/cells_sim.v@), against which they are tested with
  Yosys 0.70 (@\/nix\/store\/cfry5mn7l5qk6cy730gspaa51fahc108-yosys-0.70@).
  There are no Lattice simulation libraries available to test against.

  == Simulation of double data rate signals

  'sbio' lives in a single domain: its pad is sampled once per clock cycle. A
  sample of the pad is its value in the second half of the clock cycle, i.e.
  the value a register sampling the pad at the next active edge sees. That is
  the full story for all pin types, except for those that use both edges of the
  clock: the @D_IN_1@ output (sampled at the inactive edge) is undefined, and in
  the @PIN_OUTPUT_DDR*@ modes the pad shows @D_OUT_1@ only. Use 'sbioDdr' for
  double data rate signals: its pad lives in a domain with half the period of
  the clock, so that it has two samples per clock cycle.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.LatticeSemi.Ice40.IO
  ( -- * SB_IO
    sbio
  , sbioDdr
  , spiConfig
  , PinOutputConfig(..)
  , PinInputConfig(..)
    -- * Global buffers
  , sbGb
  , sbGbSignal
  , sbGbIo
  , sbGbIoClock
  ) where

import           GHC.Stack                    (HasCallStack())

import           Clash.Annotations.BitRepresentation
  (ConstrRepr(..), DataReprAnn(..), liftQ)
import           Clash.Annotations.BitRepresentation.Deriving (deriveBitPack)
import           Clash.Explicit.Prelude
import           Clash.Signal.Internal (Signal((:-)))
import qualified Clash.Signal as Hidden

import           Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( BitBiSignalInPort(..), BiSignalOutPort(..), BitVectorParam(..)
  , ClockPort(..), Inst, Param(..), Port(..), inst, unPort )
import           Clash.Cores.LatticeSemi.Ice40.Internal (ice40Prim)

-- | Input pinType  mnemonics as documented in the first table of LITL p88. Note
-- that @PIN_INPUT_DDR@ is missing. Use 'PIN_INPUT_REGISTERED' instead.
data PinInputConfig
  = PIN_INPUT_REGISTERED
  -- ^ Input data is registered in input cell. Same as @PIN_INPUT_DDR@.
  | PIN_INPUT
  -- ^ Simple input pin (D_IN_0)
  | PIN_INPUT_REGISTERED_LATCH
  -- ^ Like 'PIN_INPUT_REGISTERED', but @LATCH_INPUT_VALUE@ freezes @D_IN_0@.
  -- Simulation: @D_IN_0@ is undefined while @LATCH_INPUT_VALUE@ is asserted,
  -- as Clash does not support clockless latches.
  | PIN_INPUT_LATCH
  -- ^ Like 'PIN_INPUT', but @LATCH_INPUT_VALUE@ freezes @D_IN_0@. Simulation:
  -- @D_IN_0@ is undefined while @LATCH_INPUT_VALUE@ is asserted, as Clash
  -- does not support clockless latches.
  deriving (Show, Generic, BitPack)

-- | Output pinType  mnemonics as documented in the second table of LITL p88.
-- The constructors are named after Lattice's mnemonics. Their behavior is
-- given by their encoding, see the description of each constructor.
data PinOutputConfig
  = PIN_NO_OUTPUT
  -- ^ Disables the output function
  | PIN_OUTPUT
  -- ^ Simple output pin, (no enable)
  | PIN_OUTPUT_TRISTATE
  -- ^ The output pin may be tristated using the enable
  | PIN_OUTPUT_ENABLE_REGISTERED
  -- ^ The output pin may be tristated using a registered enable signal
  | PIN_OUTPUT_REGISTERED
  -- ^ Output registered, (no enable)
  | PIN_OUTPUT_REGISTERED_ENABLE
  -- ^ Output registered with enable (enable is not registered)
  | PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED
  -- ^ Output registered and enable registered
  | PIN_OUTPUT_DDR
  -- ^ Output DDR data is clocked out on rising and falling clock edges. See
  -- 'sbioDdr'.
  | PIN_OUTPUT_DDR_ENABLE
  -- ^ Output data is clocked out on rising and falling clock edges. See
  -- 'sbioDdr'.
  | PIN_OUTPUT_DDR_ENABLE_REGISTERED
  -- ^ Output DDR data with registered enable signal. See 'sbioDdr'.
  | PIN_OUTPUT_REGISTERED_INVERTED
  -- ^ Output registered signal is inverted
  | PIN_OUTPUT_REGISTERED_ENABLE_INVERTED
  -- ^ Output signal is registered and inverted. Note that, despite LITL's
  -- description (\"no enable function\"), its encoding enables the output with
  -- (an unregistered) @OUTPUT_ENABLE@.
  | PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED
  -- ^ Output signal is registered and inverted, the enable/tristate control
  -- is registered.
  deriving (Show)

{-# ANN module (DataReprAnn
                  $(liftQ [t|PinOutputConfig|])
                  4
                  [ ConstrRepr 'PIN_NO_OUTPUT 0b1111 0b0000 []
                  , ConstrRepr 'PIN_OUTPUT 0b1111 0b0110 []
                  , ConstrRepr 'PIN_OUTPUT_TRISTATE 0b1111 0b1010 []
                  , ConstrRepr 'PIN_OUTPUT_ENABLE_REGISTERED 0b1111 0b1110 []
                  , ConstrRepr 'PIN_OUTPUT_REGISTERED 0b1111 0b0101 []
                  , ConstrRepr 'PIN_OUTPUT_REGISTERED_ENABLE 0b1111 0b1001 []
                  , ConstrRepr 'PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED 0b1111 0b1101 []

                  , ConstrRepr 'PIN_OUTPUT_DDR 0b1111 0b0100 []
                  , ConstrRepr 'PIN_OUTPUT_DDR_ENABLE 0b1111 0b1000 []
                  , ConstrRepr 'PIN_OUTPUT_DDR_ENABLE_REGISTERED 0b1111 0b1100 []

                  , ConstrRepr 'PIN_OUTPUT_REGISTERED_INVERTED 0b1111 0b0111 []
                  , ConstrRepr 'PIN_OUTPUT_REGISTERED_ENABLE_INVERTED 0b1111 0b1011 []
                  , ConstrRepr 'PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED 0b1111 0b1111 []
                  ]) #-}

deriveBitPack [t|PinOutputConfig|]

-- | Create configuration bitvector based on pin function mnemonics. See
-- documentation on "PinInputConfig" and "PinOutputConfig" for more information.
spiConfig
  :: PinInputConfig
  -> PinOutputConfig
  -> BitVector 6
spiConfig pic poc = pack poc ++# pack pic

{- | SB_IO primitive as described in LITL p87. The registers of the I/O cell
are clocked by the hidden clock (@INPUT_CLK@ and @OUTPUT_CLK@) and enabled by
the hidden enable (@CLOCK_ENABLE@).

The pin configuration must be a constant. To force Clash to calculate it at
compile time, use Template Haskell:

@
let pinType = $(lift (spiConfig PIN_INPUT PIN_OUTPUT_TRISTATE)) in
sbio pinType pkgPin latchInput ..
@

Simulation: every pin type is supported, see the module documentation for
what that means for the double data rate features. The output registers start
out undefined, like those of the Yosys simulation model. Use 'sbioDdr' to
simulate double data rate signals.
-}
sbio
  :: forall dom
   . ( HasCallStack
     , Hidden.HiddenClock dom  -- INPUT_CLK
     , Hidden.HiddenEnable dom -- CLOCK_ENABLE
     )
  => BitVector 6
  -- ^ Config, see 'spiConfig'
  -> BiSignalIn 'Floating dom 1
  -- ^ PACKAGE_PIN
  -> Signal dom Bit
  -- ^ LATCH_INPUT_VALUE
  -> Signal dom Bit
  -- ^ D_OUT_0
  -> Signal dom Bit
  -- ^ D_OUT_1. Only used by the @PIN_OUTPUT_DDR*@ modes.
  -> Signal dom Bool
  -- ^ OUTPUT_ENABLE
  -> ( BiSignalOut 'Floating dom 1  -- PACKAGE_PIN
     , Signal dom Bit               -- D_IN_0
     , Signal dom Bit               -- D_IN_1
     )
sbio pinConf pkgPin latchInput dOut0 dOut1 outputEnable
  | clashSimulation = (writeToBiSignal pkgPin driveSecond, dIn0, dIn1)
  | otherwise =
      sbioInst
        clk en pinConf (BitBiSignalInPort pkgPin) latchInput dOut0 dOut1
        outputEnable
 where
  clk = Hidden.hasClock
  en = Hidden.hasEnable
  (_driveFirst, driveSecond, dIn0, dIn1) =
    sbioModel clk en pinConf padFirst (readFromBiSignal pkgPin) latchInput
      dOut0 dOut1 outputEnable
  padFirst =
    pure (errorX "sbio: the pad is only sampled once per clock cycle. Use sbioDdr.")

{- | SB_IO primitive as described in LITL p87, for double data rate signals.
Like 'sbio', but with an explicit clock and enable, and the pad in a domain
@fast@ with half the period of the clock's domain @slow@. For every cycle of
@slow@ the pad has two samples: one in the first half of the clock cycle
(after the active edge) and one in the second half (after the inactive edge).

Sample /k/ of @slow@ spans samples /2k-1/ and /2k/ of @fast@: sample 0 of a
signal is its value before the first active edge of its clock, so sample /k/
of @slow@ is the clock cycle after active edge /k-1/. With @D_OUT_0(k)@ the
value of @D_OUT_0@ in cycle /k/ of @slow@, @pad(2k-1)@ and @pad(2k)@ the samples
of the pad in the first and second half of that cycle, and the clock enabled,
the cell behaves as follows:

* @PIN_OUTPUT_DDR*@: @pad(2k-1) = D_OUT_0(k-1)@ and @pad(2k) = D_OUT_1(k)@.
  @D_OUT_0@ is registered at the active edge, @D_OUT_1@ at the inactive edge.
* Registered input (@PIN_INPUT_REGISTERED@, also known as @PIN_INPUT_DDR@):
  @D_IN_0(k) = pad(2k-2)@, the value captured at the active edge, and
  @D_IN_1(k) = pad(2k-1)@, the value captured at the inactive edge. Note that
  @D_IN_1@ changes halfway through the clock cycle.
* @D_IN_1@ is captured at the inactive edge for all input pin types.

The enable gates the registers at the active edge directly. The registers at
the inactive edge are gated by the enable registered at the preceding active
edge, so they follow the enable of the previous clock cycle, like in the Yosys
simulation model.

Note that 'sbio' and 'sbioDdr' instantiate the same primitive in HDL: the
difference is in the Clash simulation only.
-}
sbioDdr
  :: forall slow fast
   . ( HasCallStack
     , KnownDomain slow
     , KnownDomain fast
     , DomainPeriod slow ~ (2 * DomainPeriod fast)
     )
  => Clock slow
  -- ^ INPUT_CLK and OUTPUT_CLK
  -> Enable slow
  -- ^ CLOCK_ENABLE
  -> BitVector 6
  -- ^ Config, see 'spiConfig'
  -> BiSignalIn 'Floating fast 1
  -- ^ PACKAGE_PIN
  -> Signal slow Bit
  -- ^ LATCH_INPUT_VALUE
  -> Signal slow Bit
  -- ^ D_OUT_0
  -> Signal slow Bit
  -- ^ D_OUT_1
  -> Signal slow Bool
  -- ^ OUTPUT_ENABLE
  -> ( BiSignalOut 'Floating fast 1  -- PACKAGE_PIN
     , Signal slow Bit               -- D_IN_0
     , Signal slow Bit               -- D_IN_1
     )
sbioDdr clk en pinConf pkgPin latchInput dOut0 dOut1 outputEnable
  | clashSimulation =
      (writeToBiSignal pkgPin (interleaveSignals driveFirst driveSecond), dIn0, dIn1)
  | otherwise =
      sbioInst
        clk en pinConf (BitBiSignalInPort pkgPin) latchInput dOut0 dOut1
        outputEnable
 where
  (padFirst, padSecond) = deinterleaveSignals (readFromBiSignal pkgPin)
  (driveFirst, driveSecond, dIn0, dIn1) =
    sbioModel clk en pinConf padFirst padSecond latchInput dOut0 dOut1
      outputEnable

{- Note [Samples of the pad of sbioDdr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sample 0 of any signal is its value before the first active edge of its clock,
and sample /i/ its value after active edge /i-1/. As the active edges of @slow@
coincide with every other active edge of @fast@, sample /k/ of @slow@ (for
/k > 0/) spans samples /2k-1/ (the first half of the clock cycle) and /2k/ (the
second half) of @fast@. Sample 0 of @slow@ corresponds to sample 0 of @fast@.
-}

-- | Combine the first and second half of each cycle of a signal into a signal
-- with half the period. See Note [Samples of the pad of sbioDdr].
interleaveSignals :: Signal slow a -> Signal slow a -> Signal fast a
interleaveSignals ~(_ :- firsts) ~(second0 :- seconds) = second0 :- go firsts seconds
 where
  go ~(a :- as) ~(b :- bs) = a :- b :- go as bs

-- | Split a signal into the first and the second half of each cycle of a
-- signal with twice its period. Inverse of 'interleaveSignals'.
deinterleaveSignals :: Signal fast a -> (Signal slow a, Signal slow a)
deinterleaveSignals ~(x0 :- xs) = (x0 :- firsts, x0 :- seconds)
 where
  (firsts, seconds) = go xs
  go ~(a :- ~(b :- rest)) = (a :- as, b :- bs)
   where
    (as, bs) = go rest

{- | Simulation model of SB_IO, following the Yosys simulation model. As the
pad can change at both edges of the clock, it works on two samples of the pad
per clock cycle: one for the first half of the cycle (after the active edge),
and one for the second half.
-}
sbioModel ::
  forall dom.
  KnownDomain dom =>
  Clock dom ->
  Enable dom ->
  -- | PIN_TYPE
  BitVector 6 ->
  -- | Pad, first half of the cycle
  Signal dom Bit ->
  -- | Pad, second half of the cycle
  Signal dom Bit ->
  -- | LATCH_INPUT_VALUE
  Signal dom Bit ->
  -- | D_OUT_0
  Signal dom Bit ->
  -- | D_OUT_1
  Signal dom Bit ->
  -- | OUTPUT_ENABLE
  Signal dom Bool ->
  -- | (Drive pad in first half, drive pad in second half, D_IN_0, D_IN_1)
  ( Signal dom (Maybe Bit)
  , Signal dom (Maybe Bit)
  , Signal dom Bit
  , Signal dom Bit )
sbioModel clk en pinType padFirst padSecond latchInput dOut0 dOut1 outputEnable =
  (drive <$> oe <*> doutFirst, drive <$> oe <*> doutSecond, dIn0, dIn1)
 where
  reg :: NFDataX a => String -> Signal dom a -> Signal dom a
  reg nm = delay clk en (deepErrorX ("sbio: " <> nm <> " undefined"))

  -- The registers at the inactive edge are enabled by the enable registered at
  -- the preceding active edge.
  enRegistered =
    delay clk enableGen (errorX "sbio: clock enable undefined") (fromEnable en)
  regInactive nm d = q
   where
    q = mux enRegistered d (delay clk enableGen (errorX ("sbio: " <> nm <> " undefined")) q)

  -- Input
  dInQ0 = reg "D_IN_0" padSecond
  dIn0Unlatched = if testBit pinType 0 then padSecond else dInQ0
  dIn0
    | testBit pinType 1 = mux (bitToBool <$> latchInput) latched dIn0Unlatched
    | otherwise = dIn0Unlatched
  latched = pure (errorX "sbio: LATCH_INPUT_VALUE asserted, latches are not supported")
  dIn1 = regInactive "D_IN_1" padFirst

  -- Output
  dOutQ0 = reg "D_OUT_0" dOut0
  dOutQ1 = regInactive "D_OUT_1" dOut1
  (doutFirst, doutSecond) =
    case slice d3 d2 pinType of
      0b00 -> (dOutQ0, dOutQ1)
      0b01 -> (dOutQ0, dOutQ0)
      0b10 -> (dOut0, dOut0)
      _ -> (complement <$> dOutQ0, complement <$> dOutQ0)

  oe =
    case slice d5 d4 pinType of
      0b00 -> pure False
      0b01 -> pure True
      0b10 -> outputEnable
      _ -> reg "OUTPUT_ENABLE" outputEnable

  drive enable d = if enable then Just d else Nothing

-- | Instantiate SB_IO
sbioInst ::
  forall dom domPin.
  (KnownDomain dom, KnownDomain domPin) =>
  Clock dom ->
  Enable dom ->
  BitVector 6 ->
  BitBiSignalInPort "PACKAGE_PIN" 'Floating domPin ->
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bool ->
  (BiSignalOut 'Floating domPin 1, Signal dom Bit, Signal dom Bit)
sbioInst clk en pinConf pkgPin latchInput dOut0 dOut1 outputEnable =
  (pkgPinOut, unPort dIn0, unPort dIn1)
 where
  (BiSignalOutPort pkgPinOut, dIn0, dIn1) = go

  go ::
    ( BiSignalOutPort "PACKAGE_PIN" 'Floating domPin 1
    , Port "D_IN_0" dom Bit
    , Port "D_IN_1" dom Bit )
  go = sbioInstWith "SB_IO" clk en pinConf pkgPin latchInput dOut0 dOut1 outputEnable

{- | Instantiate SB_IO or SB_GB_IO, with the given output ports. Sets
@NEG_TRIGGER@ if the active edge of the clock is the falling edge. Only sets
@PIN_TYPE@ and @NEG_TRIGGER@: @PULLUP@ (pull-up resistor disabled) and
@IO_STANDARD@ (@SB_LVCMOS@) keep their defaults.
-}
sbioInstWith ::
  forall dom domPin r.
  (KnownDomain dom, Inst r) =>
  -- | Component name
  String ->
  Clock dom ->
  Enable dom ->
  BitVector 6 ->
  BitBiSignalInPort "PACKAGE_PIN" 'Floating domPin ->
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bit ->
  Signal dom Bool ->
  r
sbioInstWith compNm clk en pinConf pkgPin latchInput dOut0 dOut1 outputEnable =
  case activeEdge @dom of
    SRising -> ports (inst config pinTypeParam)
    -- The registers of the cell trigger on the falling edge of the clock
    SFalling -> ports (inst config pinTypeParam (Param @"NEG_TRIGGER" @Bit 1))
 where
  config = ice40Prim compNm
  pinTypeParam = BitVectorParam @"PIN_TYPE" pinConf

  ports ::
    ( BitBiSignalInPort "PACKAGE_PIN" 'Floating domPin ->
      Port "LATCH_INPUT_VALUE" dom Bit ->
      Port "CLOCK_ENABLE" dom Bit ->
      ClockPort "INPUT_CLK" dom ->
      ClockPort "OUTPUT_CLK" dom ->
      Port "OUTPUT_ENABLE" dom Bit ->
      Port "D_OUT_0" dom Bit ->
      Port "D_OUT_1" dom Bit ->
      r ) ->
    r
  ports f =
    f
      pkgPin
      (Port @"LATCH_INPUT_VALUE" latchInput)
      (Port @"CLOCK_ENABLE" (boolToBit <$> fromEnable en))
      (ClockPort @"INPUT_CLK" clk)
      (ClockPort @"OUTPUT_CLK" clk)
      (Port @"OUTPUT_ENABLE" (boolToBit <$> outputEnable))
      (Port @"D_OUT_0" dOut0)
      (Port @"D_OUT_1" dOut1)

-- * Global buffers

{- | Global buffer @SB_GB@ (see LITL), driving a clock from the fabric onto one
of the eight global networks, e.g. the output of a PLL or a divided clock.

Simulation: the identity function.
-}
sbGb :: forall dom. KnownDomain dom => Clock dom -> Clock dom
sbGb clk
  | clashSimulation = clk
  | otherwise = unPort go
 where
  go :: ClockPort "GLOBAL_BUFFER_OUTPUT" dom
  go =
    inst
      (ice40Prim "SB_GB")
      (ClockPort @"USER_SIGNAL_TO_GLOBAL_BUFFER" clk)

{- | Global buffer @SB_GB@ (see LITL), driving a signal from the fabric onto
one of the eight global networks. The global networks are meant for clocks and
for signals with a high fanout, such as resets and clock enables.

Simulation: the identity function.
-}
sbGbSignal :: forall dom. KnownDomain dom => Signal dom Bit -> Signal dom Bit
sbGbSignal s
  | clashSimulation = s
  | otherwise = unPort go
 where
  go :: Port "GLOBAL_BUFFER_OUTPUT" dom Bit
  go =
    inst
      (ice40Prim "SB_GB")
      (Port @"USER_SIGNAL_TO_GLOBAL_BUFFER" s)

{- | I/O cell with a global buffer, @SB_GB_IO@ (see LITL): an 'sbio' whose pad
also drives one of the eight global networks, @GLOBAL_BUFFER_OUTPUT@. Only the
pins named @GBIN@ in the data sheet have one. Use 'sbGbIoClock' if all you want
is a clock input.

Simulation: the 'sbio' part is simulated like 'sbio'. The global buffer output
is an ideal clock of domain @domGb@, regardless of the level of the pad: Clash
can't turn a signal into a clock.
-}
sbGbIo
  :: forall domGb dom
   . ( HasCallStack
     , KnownDomain domGb
     , Hidden.HiddenClock dom  -- INPUT_CLK
     , Hidden.HiddenEnable dom -- CLOCK_ENABLE
     )
  => BitVector 6
  -- ^ Config, see 'spiConfig'
  -> BiSignalIn 'Floating dom 1
  -- ^ PACKAGE_PIN
  -> Signal dom Bit
  -- ^ LATCH_INPUT_VALUE
  -> Signal dom Bit
  -- ^ D_OUT_0
  -> Signal dom Bit
  -- ^ D_OUT_1. Only used by the @PIN_OUTPUT_DDR*@ modes.
  -> Signal dom Bool
  -- ^ OUTPUT_ENABLE
  -> ( BiSignalOut 'Floating dom 1  -- PACKAGE_PIN
     , Signal dom Bit               -- D_IN_0
     , Signal dom Bit               -- D_IN_1
     , Clock domGb                  -- GLOBAL_BUFFER_OUTPUT
     )
sbGbIo pinConf pkgPin latchInput dOut0 dOut1 outputEnable
  | clashSimulation = (pkgPinSim, dIn0Sim, dIn1Sim, clockGen)
  | otherwise = (pkgPinOut, unPort dIn0, unPort dIn1, unPort gbOut)
 where
  (pkgPinSim, dIn0Sim, dIn1Sim) = sbio pinConf pkgPin latchInput dOut0 dOut1 outputEnable

  (BiSignalOutPort pkgPinOut, gbOut, dIn0, dIn1) = go

  go ::
    ( BiSignalOutPort "PACKAGE_PIN" 'Floating dom 1
    , ClockPort "GLOBAL_BUFFER_OUTPUT" domGb
    , Port "D_IN_0" dom Bit
    , Port "D_IN_1" dom Bit )
  go =
    sbioInstWith
      "SB_GB_IO" Hidden.hasClock Hidden.hasEnable pinConf
      (BitBiSignalInPort pkgPin) latchInput dOut0 dOut1 outputEnable

{- | A clock input pin driving one of the eight global networks: @SB_GB_IO@
(see LITL) configured as a simple input (@PIN_INPUT@, @PIN_NO_OUTPUT@). Only the
pins named @GBIN@ in the data sheet have a global buffer.

The domain of the pad, @domPin@, is irrelevant: the pad is never read or
written by the fabric.

Simulation: an ideal clock of domain @dom@, regardless of the level of the
pad. Clash can't turn a signal into a clock.
-}
sbGbIoClock ::
  forall dom domPin.
  (KnownDomain dom, KnownDomain domPin) =>
  -- | PACKAGE_PIN
  BiSignalIn 'Floating domPin 1 ->
  -- | GLOBAL_BUFFER_OUTPUT
  Clock dom
sbGbIoClock pkgPin
  | clashSimulation = clockGen
  | otherwise = unPort gbOut
 where
  (_ :: BiSignalOutPort "PACKAGE_PIN" 'Floating domPin 1, gbOut) = go

  go ::
    ( BiSignalOutPort "PACKAGE_PIN" 'Floating domPin 1
    , ClockPort "GLOBAL_BUFFER_OUTPUT" dom )
  go =
    inst
      (ice40Prim "SB_GB_IO")
      (BitVectorParam @"PIN_TYPE" (spiConfig PIN_INPUT PIN_NO_OUTPUT))
      (BitBiSignalInPort @"PACKAGE_PIN" pkgPin)
      (Port @"LATCH_INPUT_VALUE" zero)
      (Port @"CLOCK_ENABLE" zero)
      (Port @"INPUT_CLK" zero)
      (Port @"OUTPUT_CLK" zero)
      (Port @"OUTPUT_ENABLE" zero)
      (Port @"D_OUT_0" zero)
      (Port @"D_OUT_1" zero)

  zero :: Signal domPin Bit
  zero = pure 0
