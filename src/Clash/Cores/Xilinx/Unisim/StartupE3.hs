{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Interface between the fabric and the configuration logic of UltraScale and
UltraScale+ devices (@STARTUPE3@): access to the configuration pins (for
example to read and write the configuration flash after configuration), the
end-of-startup signal and the internal configuration oscillator. For more
information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/STARTUPE3)
and the /UltraScale Architecture Configuration/ user guide (UG570).
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Unisim.StartupE3
  ( startupE3
  , StartupE3Input (..)
  , startupE3DefaultInput
  , StartupE3Output (..)
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Internal (unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (ClockPort (..), Port (..), inst, unPort)

-- | Inputs of 'startupE3'. The 3-state controls are active-high: 'True'
-- releases the pin, 'False' drives it.
data StartupE3Input dom = StartupE3Input
  { startupUsrCclkO :: Signal dom Bit
  -- ^ Value for the configuration clock pin @CCLK@ (@USRCCLKO@). After
  -- configuration, the first three clock cycles on this input are used to
  -- switch the pin over to the fabric, see UG570.
  , startupUsrCclkTs :: Signal dom Bool
  -- ^ 3-state control of @CCLK@ (@USRCCLKTS@)
  , startupDo :: Signal dom (BitVector 4)
  -- ^ Values for the configuration data pins @D00@ to @D03@ (@DO@)
  , startupDts :: Signal dom (BitVector 4)
  -- ^ 3-state controls of @D00@ to @D03@, 1 releases a pin (@DTS@)
  , startupFcsbO :: Signal dom Bit
  -- ^ Value for the flash chip select pin @FCS_B@ (@FCSBO@)
  , startupFcsbTs :: Signal dom Bool
  -- ^ 3-state control of @FCS_B@ (@FCSBTS@)
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
startupE3DefaultInput :: StartupE3Input dom
startupE3DefaultInput =
  StartupE3Input
    { startupUsrCclkO = pure 0
    , startupUsrCclkTs = pure True
    , startupDo = pure 0
    , startupDts = pure maxBound
    , startupFcsbO = pure 1
    , startupFcsbTs = pure True
    , startupUsrDoneO = pure 1
    , startupUsrDoneTs = pure True
    , startupKeyClearB = pure True
    , startupPack = pure False
    }

-- | Outputs of 'startupE3'. Except for the clock, they are asynchronous to
-- every fabric clock and must be synchronized before use.
data StartupE3Output domCfgm dom = StartupE3Output
  { startupCfgMClk :: Clock domCfgm
  -- ^ Internal configuration oscillator, running at about 50 MHz (@CFGMCLK@).
  -- See the data sheet of your device for its tolerance.
  , startupDi :: Signal dom (BitVector 4)
  -- ^ Values on the configuration data pins @D00@ to @D03@ (@DI@)
  , startupEos :: Signal dom Bool
  -- ^ End of startup: configuration has finished (@EOS@)
  , startupPreq :: Signal dom Bool
  -- ^ Request to reconfigure the device, from the @PROGRAM_B@ pin (@PREQ@).
  -- Only asserted when the primitive's @PROG_USR@ attribute is set, which
  -- these functions don't do.
  }

{- | Interface between the fabric and the configuration logic (@STARTUPE3@).
The primitive's @GSR@ and @GTS@ inputs, which assert the global set/reset and
3-state signals, are tied to 0. Its @CFGCLK@ output is not returned.

Simulation: 'startupEos' is asserted, 'startupPreq' is deasserted, and
'startupDi' is the given simulation value. 'startupCfgMClk' is a clock with
the period of @domCfgm@, which should be 20 ns to match the nominal frequency.
The UNISIM simulation model asserts @EOS@ when the simulated global set/reset
ends (after 100 ns), and drives @DI@ from its simulated configuration pins.

Only one @STARTUPE3@ can be instantiated in a design.
-}
startupE3 ::
  forall domCfgm dom.
  (KnownDomain domCfgm, KnownDomain dom) =>
  -- | Simulation only: the values on the configuration data pins (@DI@)
  Signal dom (BitVector 4) ->
  StartupE3Input dom ->
  StartupE3Output domCfgm dom
startupE3 simDi StartupE3Input{..}
  | clashSimulation =
      StartupE3Output
        { startupCfgMClk = clockGen
        , startupDi = simDi
        , startupEos = pure True
        , startupPreq = pure False
        }
  | otherwise =
      StartupE3Output
        { startupCfgMClk = unPort cfgMClk
        , startupDi = unPort di
        , startupEos = bitToBool <$> unPort eos
        , startupPreq = bitToBool <$> unPort preq
        }
 where
  (_cfgClk, cfgMClk, di, eos, preq) = go
  go ::
    ( Port "CFGCLK" dom Bit
    , ClockPort "CFGMCLK" domCfgm
    , Port "DI" dom (BitVector 4)
    , Port "EOS" dom Bit
    , Port "PREQ" dom Bit
    )
  go =
    inst
      (unisim "STARTUPE3")
      (Port @"DO" startupDo)
      (Port @"DTS" startupDts)
      (Port @"FCSBO" startupFcsbO)
      (Port @"FCSBTS" (boolToBit <$> startupFcsbTs))
      (Port @"GSR" (pure 0 :: Signal dom Bit))
      (Port @"GTS" (pure 0 :: Signal dom Bit))
      (Port @"KEYCLEARB" (boolToBit <$> startupKeyClearB))
      (Port @"PACK" (boolToBit <$> startupPack))
      (Port @"USRCCLKO" startupUsrCclkO)
      (Port @"USRCCLKTS" (boolToBit <$> startupUsrCclkTs))
      (Port @"USRDONEO" startupUsrDoneO)
      (Port @"USRDONETS" (boolToBit <$> startupUsrDoneTs))
