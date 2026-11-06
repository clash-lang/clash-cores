{- | Compares the Haskell simulation model of 'sbioOd' to Yosys' simulation
model of @SB_IO_OD@, for a number of pin types, each with random stimuli. The
pad has a pull-up. See "SbIoOdTypes" and "Ice40TestCommon".
-}
module SbIoOd where

import Clash.Explicit.Prelude

import Clash.Cores.LatticeSemi.Ice40.IO
import Ice40TestCommon
import SbIoOdTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbInputRegistered :: Signal Dom Bool
tbInputRegistered = tb (sbioOdCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_NO_OUTPUT)))
  $(sbioOdBlobs (spiConfig PIN_INPUT_REGISTERED PIN_NO_OUTPUT) 1)
{-# ANN tbInputRegistered (TestBench 'topEntity) #-}

tbOutput :: Signal Dom Bool
tbOutput = tb (sbioOdCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT)))
  $(sbioOdBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT) 2)
{-# ANN tbOutput (TestBench 'topEntity) #-}

tbOutputTristate :: Signal Dom Bool
tbOutputTristate = tb (sbioOdCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT_TRISTATE)))
  $(sbioOdBlobs (spiConfig PIN_INPUT PIN_OUTPUT_TRISTATE) 3)
{-# ANN tbOutputTristate (TestBench 'topEntity) #-}

tbOutputRegisteredEnableRegisteredInverted :: Signal Dom Bool
tbOutputRegisteredEnableRegisteredInverted = tb (sbioOdCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED)))
  $(sbioOdBlobs (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED) 4)
{-# ANN tbOutputRegisteredEnableRegisteredInverted (TestBench 'topEntity) #-}

-- | A DDR output, sampled once per clock cycle: the pad shows @DOUT1@
tbOutputDdr :: Signal Dom Bool
tbOutputDdr = tb (sbioOdCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_DDR)))
  $(sbioOdBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_DDR) 5)
{-# ANN tbOutputDdr (TestBench 'topEntity) #-}
