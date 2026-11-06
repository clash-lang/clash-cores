{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Configuration primitives of 7 series devices (Artix-7, Kintex-7, Virtex-7,
Zynq-7000): the interface to the configuration logic (@STARTUPE2@), the
internal configuration access port (@ICAPE2@) and the device DNA (@DNA_PORT@).
For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/Other-Components)
and the /7 Series FPGAs Configuration/ user guide (UG470).

The @USR_ACCESS@ register of 7 series devices is read with
'Clash.Cores.Xilinx.Unisim.UsrAccessE2.usrAccessE2', which supports 7 series
and UltraScale devices alike.
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Unisim.Series7.Config
  ( -- * STARTUPE2
    startupE2
  , StartupE2Input (..)
  , startupE2DefaultInput
  , StartupE2Output (..)
    -- * ICAPE2
  , icapE2
    -- * DNA_PORT
  , dnaPort
  , readDnaPort
  , simDna
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Internal (unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (BitVectorParam (..), ClockPort (..), Port (..), inst, unPort)

-- | Inputs of 'startupE2'. The 3-state controls are active-high: 'True'
-- releases the pin, 'False' drives it.
data StartupE2Input dom = StartupE2Input
  { startupUsrCclkO :: Signal dom Bit
  -- ^ Value for the configuration clock pin @CCLK@ (@USRCCLKO@), for example
  -- to access the configuration flash after configuration. After
  -- configuration, the first three clock cycles on this input are used to
  -- switch the pin over to the fabric, see UG470.
  , startupUsrCclkTs :: Signal dom Bool
  -- ^ 3-state control of @CCLK@ (@USRCCLKTS@)
  , startupUsrDoneO :: Signal dom Bit
  -- ^ Value for the @DONE@ pin (@USRDONEO@)
  , startupUsrDoneTs :: Signal dom Bool
  -- ^ 3-state control of @DONE@ (@USRDONETS@)
  , startupKeyClearB :: Signal dom Bool
  -- ^ Clear the AES key in battery-backed RAM while 'False' (@KEYCLEARB@)
  , startupPack :: Signal dom Bool
  -- ^ Acknowledge a request to reconfigure the device (@PACK@)
  }

-- | Inputs that leave all configuration pins to the configuration logic,
-- don't clear the AES key, and don't acknowledge reconfiguration requests
startupE2DefaultInput :: StartupE2Input dom
startupE2DefaultInput =
  StartupE2Input
    { startupUsrCclkO = pure 0
    , startupUsrCclkTs = pure True
    , startupUsrDoneO = pure 1
    , startupUsrDoneTs = pure True
    , startupKeyClearB = pure True
    , startupPack = pure False
    }

-- | Outputs of 'startupE2'. Except for the clock, they are asynchronous to
-- every fabric clock and must be synchronized before use.
data StartupE2Output domCfgm dom = StartupE2Output
  { startupCfgMClk :: Clock domCfgm
  -- ^ Internal configuration oscillator, running at about 65 MHz (@CFGMCLK@).
  -- See the data sheet of your device for its tolerance.
  , startupEos :: Signal dom Bool
  -- ^ End of startup: configuration has finished (@EOS@)
  , startupPreq :: Signal dom Bool
  -- ^ Request to reconfigure the device, from the @PROGRAM_B@ pin (@PREQ@).
  -- Only asserted when the primitive's @PROG_USR@ attribute is set, which
  -- this function doesn't do.
  }

{- | Interface between the fabric and the configuration logic (@STARTUPE2@):
access to the @CCLK@ and @DONE@ configuration pins, the end-of-startup signal
and the internal configuration oscillator. For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/STARTUPE2).

The primitive's @GSR@ and @GTS@ inputs, which assert the global set/reset and
3-state signals, are tied to 0. Its @CLK@ input, a user clock for the startup
sequence (only used if the bitstream is generated with
@BITSTREAM.STARTUP.STARTUPCLK@ set to @UserClk@), is tied to 0 too. Its
@CFGCLK@ output is not returned. Its attributes are left at their defaults
(@PROG_USR = \"FALSE\"@, @SIM_CCLK_FREQ = 0.0@).

Simulation: 'startupEos' is asserted and 'startupPreq' is deasserted.
'startupCfgMClk' is a clock with the period of @domCfgm@, which should be
15.385 ns to match the nominal frequency (the UNISIM simulation model uses a
period of 15.384 ns). The UNISIM simulation model asserts @EOS@ when the
simulated global set/reset ends (after 100 ns).

Only one @STARTUPE2@ can be instantiated in a design.
-}
startupE2 ::
  forall domCfgm dom.
  (KnownDomain domCfgm, KnownDomain dom) =>
  StartupE2Input dom ->
  StartupE2Output domCfgm dom
startupE2 StartupE2Input{..}
  | clashSimulation =
      StartupE2Output
        { startupCfgMClk = clockGen
        , startupEos = pure True
        , startupPreq = pure False
        }
  | otherwise =
      StartupE2Output
        { startupCfgMClk = unPort cfgMClk
        , startupEos = bitToBool <$> unPort eos
        , startupPreq = bitToBool <$> unPort preq
        }
 where
  (_cfgClk, cfgMClk, eos, preq) = go
  go ::
    ( Port "CFGCLK" dom Bit
    , ClockPort "CFGMCLK" domCfgm
    , Port "EOS" dom Bit
    , Port "PREQ" dom Bit
    )
  go =
    inst
      (unisim "STARTUPE2")
      (Port @"CLK" (pure 0 :: Signal dom Bit))
      (Port @"GSR" (pure 0 :: Signal dom Bit))
      (Port @"GTS" (pure 0 :: Signal dom Bit))
      (Port @"KEYCLEARB" (boolToBit <$> startupKeyClearB))
      (Port @"PACK" (boolToBit <$> startupPack))
      (Port @"USRCCLKO" startupUsrCclkO)
      (Port @"USRCCLKTS" (boolToBit <$> startupUsrCclkTs))
      (Port @"USRDONEO" startupUsrDoneO)
      (Port @"USRDONETS" (boolToBit <$> startupUsrDoneTs))

{- | Internal configuration access port (@ICAPE2@): read and write the
configuration registers from the fabric, for example to read the device's
IDCODE or to trigger a reconfiguration from another bitstream (@IPROG@). Data
is transferred at the rising edges of the clock while the port is selected.
Like on the SelectMAP interface, the bits of every byte of the data are in
reversed order: see UG470 for the bit ordering and for the command sequences.
For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/ICAPE2).

The clock may run at up to 100 MHz (see the data sheet of your device). The
primitive's attributes are left at their defaults: the port is 32 bits wide
(@ICAP_WIDTH = \"X32\"@), and the UNISIM simulation model uses its default
@DEVICE_ID@ and no @SIM_CFG_FILE_NAME@.

Simulation: no configuration logic is simulated, reads return 0. The UNISIM
simulation model does simulate the configuration logic, but only after it has
finished initializing, after about 1.2 ms of simulated time; before that, it
outputs 0 as well. It can be used to test a design that uses this function in
HDL simulation.
-}
icapE2 ::
  forall dom.
  (KnownDomain dom) =>
  -- | Clock (@CLK@)
  Clock dom ->
  -- | Select the port (inverse of @CSIB@)
  Signal dom Bool ->
  -- | Read ('True') or write ('False') (@RDWRB@)
  Signal dom Bool ->
  -- | Data to write (@I@)
  Signal dom (BitVector 32) ->
  -- | Data read from the configuration logic (@O@)
  Signal dom (BitVector 32)
icapE2 clk sel rdwr dat
  | clashSimulation = pure 0
  | otherwise = unPort go
 where
  go :: Port "O" dom (BitVector 32)
  go =
    inst
      (unisim "ICAPE2")
      (ClockPort @"CLK" clk)
      (Port @"CSIB" (boolToBit . not <$> sel))
      (Port @"I" dat)
      (Port @"RDWRB" (boolToBit <$> rdwr))

-- | A DNA value for simulation
simDna :: BitVector 57
simDna = 0x1DEADBEEFC0FFEE

{- | Access to the 57-bit device DNA (@DNA_PORT@): a factory-programmed,
read-only identifier of the device. The primitive contains a 57-bit shift
register whose most significant bit is output on @DOUT@:

  * At a rising edge of the clock at which @READ@ is asserted, the shift
    register is loaded with the DNA.

  * At a rising edge at which @READ@ is deasserted and @SHIFT@ is asserted, the
    shift register shifts towards its most significant bit, and @DIN@ is
    shifted in.

So, after a read, the DNA is output most significant bit first, one bit per
shift. Connect @DOUT@ to @DIN@ to rotate the DNA instead of shifting zeroes in.
See 'readDnaPort' for a function that reads the DNA. The clock may run at up
to 100 MHz (see the data sheet of your device). For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries/DNA_PORT)
and UG470.

Simulation: the shift register is loaded with the given simulation-only DNA
value (the @SIM_DNA_VALUE@ attribute) instead. Before the first read, the
simulation model outputs 0. In hardware, and in the UNISIM simulation models,
the output is undefined until then. The VHDL UNISIM model ignores the clock
while the simulated global set/reset is asserted, which is the first 100 ns of
an HDL simulation.
-}
dnaPort ::
  forall dom.
  (KnownDomain dom) =>
  -- | Simulation only: the DNA value (@SIM_DNA_VALUE@)
  BitVector 57 ->
  -- | Clock (@CLK@)
  Clock dom ->
  -- | Bit to shift in (@DIN@)
  Signal dom Bit ->
  -- | Load the DNA into the shift register (@READ@)
  Signal dom Bool ->
  -- | Shift the shift register (@SHIFT@)
  Signal dom Bool ->
  -- | Most significant bit of the shift register (@DOUT@)
  Signal dom Bit
dnaPort simValue clk din doRead doShift
  | clashSimulation = msb <$> reg
  | otherwise = unPort go
 where
  reg =
    delay clk enableGen 0 $
      mux doRead (pure simValue) (mux doShift ((.<<+) <$> reg <*> din) reg)

  go :: Port "DOUT" dom Bit
  go =
    inst
      (unisim "DNA_PORT")
      (BitVectorParam @"SIM_DNA_VALUE" simValue)
      (ClockPort @"CLK" clk)
      (Port @"DIN" din)
      (Port @"READ" (boolToBit <$> doRead))
      (Port @"SHIFT" (boolToBit <$> doShift))

-- | State of 'readDnaPort'
data ReadDnaState
  = -- | Load the DNA into the shift register of the primitive
    DnaLoad
  | -- | Shift in bit /n/ (counting from the most significant one) of the DNA
    DnaShift (Index 57) (BitVector 57)
  | -- | The DNA has been read
    DnaDone (BitVector 57)
  deriving (Generic, NFDataX)

{- | Read the device DNA using 'dnaPort'. Returns 'Nothing' until the DNA has
been read, which takes 58 clock cycles after the reset is deasserted, and the
DNA from then on. The reset restarts the read; the enable pauses it.

The DNA is only available once configuration has finished. In an HDL
simulation that uses the VHDL UNISIM models, keep the reset asserted until the
simulated global set/reset has ended, see 'dnaPort'.
-}
readDnaPort ::
  forall dom.
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Simulation only: the DNA value (@SIM_DNA_VALUE@), for example 'simDna'
  BitVector 57 ->
  Signal dom (Maybe (BitVector 57))
readDnaPort clk rst ena simValue = dna
 where
  dout = dnaPort simValue clk 0 (doRead .&&. en) (doShift .&&. en)
  en = fromEnable ena
  (doRead, doShift, dna) = mooreB clk rst ena goState goOut DnaLoad dout

  goOut :: ReadDnaState -> (Bool, Bool, Maybe (BitVector 57))
  goOut DnaLoad = (True, False, Nothing)
  goOut DnaShift{} = (False, True, Nothing)
  goOut (DnaDone d) = (False, False, Just d)

  goState :: ReadDnaState -> Bit -> ReadDnaState
  goState DnaLoad _ = DnaShift 0 0
  goState (DnaShift n acc) b
    | n == maxBound = DnaDone acc1
    | otherwise = DnaShift (n + 1) acc1
   where
    acc1 = acc .<<+ b
  goState s@DnaDone{} _ = s
