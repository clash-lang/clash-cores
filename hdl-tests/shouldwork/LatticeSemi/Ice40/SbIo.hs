{- | Compares the Haskell simulation models of 'sbio' and 'sbioDdr' to Yosys'
simulation model of @SB_IO@, for a number of pin types, each with random
stimuli. See "SbIoTypes" and "Ice40TestCommon".
-}
module SbIo where

import Clash.Explicit.Prelude

import Clash.Cores.LatticeSemi.Ice40.IO
import Ice40TestCommon
import SbIoTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- * Single data rate: 'sbio'

tbInput :: Signal Dom Bool
tbInput = tb (sbioCircuit $(lift (spiConfig PIN_INPUT PIN_NO_OUTPUT)))
  $(sbioBlobs (spiConfig PIN_INPUT PIN_NO_OUTPUT) 1)
{-# ANN tbInput (TestBench 'topEntity) #-}

tbInputRegistered :: Signal Dom Bool
tbInputRegistered = tb (sbioCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_NO_OUTPUT)))
  $(sbioBlobs (spiConfig PIN_INPUT_REGISTERED PIN_NO_OUTPUT) 2)
{-# ANN tbInputRegistered (TestBench 'topEntity) #-}

tbInputLatch :: Signal Dom Bool
tbInputLatch = tb (sbioCircuit $(lift (spiConfig PIN_INPUT_LATCH PIN_NO_OUTPUT)))
  $(sbioBlobs (spiConfig PIN_INPUT_LATCH PIN_NO_OUTPUT) 3)
{-# ANN tbInputLatch (TestBench 'topEntity) #-}

tbInputRegisteredLatch :: Signal Dom Bool
tbInputRegisteredLatch = tb (sbioCircuit $(lift (spiConfig PIN_INPUT_REGISTERED_LATCH PIN_NO_OUTPUT)))
  $(sbioBlobs (spiConfig PIN_INPUT_REGISTERED_LATCH PIN_NO_OUTPUT) 4)
{-# ANN tbInputRegisteredLatch (TestBench 'topEntity) #-}

tbOutput :: Signal Dom Bool
tbOutput = tb (sbioCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT)))
  $(sbioBlobs (spiConfig PIN_INPUT PIN_OUTPUT) 5)
{-# ANN tbOutput (TestBench 'topEntity) #-}

tbOutputTristate :: Signal Dom Bool
tbOutputTristate = tb (sbioCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT_TRISTATE)))
  $(sbioBlobs (spiConfig PIN_INPUT PIN_OUTPUT_TRISTATE) 6)
{-# ANN tbOutputTristate (TestBench 'topEntity) #-}

tbOutputEnableRegistered :: Signal Dom Bool
tbOutputEnableRegistered = tb (sbioCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_ENABLE_REGISTERED)))
  $(sbioBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_ENABLE_REGISTERED) 7)
{-# ANN tbOutputEnableRegistered (TestBench 'topEntity) #-}

tbOutputRegistered :: Signal Dom Bool
tbOutputRegistered = tb (sbioCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_REGISTERED)))
  $(sbioBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_REGISTERED) 8)
{-# ANN tbOutputRegistered (TestBench 'topEntity) #-}

tbOutputRegisteredEnable :: Signal Dom Bool
tbOutputRegisteredEnable = tb (sbioCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_ENABLE)))
  $(sbioBlobs (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_ENABLE) 9)
{-# ANN tbOutputRegisteredEnable (TestBench 'topEntity) #-}

tbOutputRegisteredEnableRegistered :: Signal Dom Bool
tbOutputRegisteredEnableRegistered = tb (sbioCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED)))
  $(sbioBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED) 10)
{-# ANN tbOutputRegisteredEnableRegistered (TestBench 'topEntity) #-}

tbOutputRegisteredInverted :: Signal Dom Bool
tbOutputRegisteredInverted = tb (sbioCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_INVERTED)))
  $(sbioBlobs (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_INVERTED) 11)
{-# ANN tbOutputRegisteredInverted (TestBench 'topEntity) #-}

tbOutputRegisteredEnableInverted :: Signal Dom Bool
tbOutputRegisteredEnableInverted = tb (sbioCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_REGISTERED_ENABLE_INVERTED)))
  $(sbioBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_REGISTERED_ENABLE_INVERTED) 12)
{-# ANN tbOutputRegisteredEnableInverted (TestBench 'topEntity) #-}

tbOutputRegisteredEnableRegisteredInverted :: Signal Dom Bool
tbOutputRegisteredEnableRegisteredInverted = tb (sbioCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED)))
  $(sbioBlobs (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED) 13)
{-# ANN tbOutputRegisteredEnableRegisteredInverted (TestBench 'topEntity) #-}

-- | A DDR output, sampled once per clock cycle: the pad shows @D_OUT_1@
tbOutputDdr :: Signal Dom Bool
tbOutputDdr = tb (sbioCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_DDR)))
  $(sbioBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_DDR) 14)
{-# ANN tbOutputDdr (TestBench 'topEntity) #-}

-- * Double data rate: 'sbioDdr'

tbDdrInput :: Signal Dom Bool
tbDdrInput = tbDdr (sbioDdrCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_NO_OUTPUT)))
  $(sbioDdrBlobs (spiConfig PIN_INPUT_REGISTERED PIN_NO_OUTPUT) 21)
{-# ANN tbDdrInput (TestBench 'topEntity) #-}

tbDdrInputUnregistered :: Signal Dom Bool
tbDdrInputUnregistered = tbDdr (sbioDdrCircuit $(lift (spiConfig PIN_INPUT PIN_NO_OUTPUT)))
  $(sbioDdrBlobs (spiConfig PIN_INPUT PIN_NO_OUTPUT) 22)
{-# ANN tbDdrInputUnregistered (TestBench 'topEntity) #-}

tbDdrOutput :: Signal Dom Bool
tbDdrOutput = tbDdr (sbioDdrCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_DDR)))
  $(sbioDdrBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_DDR) 23)
{-# ANN tbDdrOutput (TestBench 'topEntity) #-}

tbDdrOutputEnable :: Signal Dom Bool
tbDdrOutputEnable = tbDdr (sbioDdrCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_DDR_ENABLE)))
  $(sbioDdrBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_DDR_ENABLE) 24)
{-# ANN tbDdrOutputEnable (TestBench 'topEntity) #-}

tbDdrOutputEnableRegistered :: Signal Dom Bool
tbDdrOutputEnableRegistered = tbDdr (sbioDdrCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT_DDR_ENABLE_REGISTERED)))
  $(sbioDdrBlobs (spiConfig PIN_INPUT PIN_OUTPUT_DDR_ENABLE_REGISTERED) 25)
{-# ANN tbDdrOutputEnableRegistered (TestBench 'topEntity) #-}

tbDdrOutputRegistered :: Signal Dom Bool
tbDdrOutputRegistered = tbDdr (sbioDdrCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_REGISTERED)))
  $(sbioDdrBlobs (spiConfig PIN_INPUT_REGISTERED PIN_OUTPUT_REGISTERED) 26)
{-# ANN tbDdrOutputRegistered (TestBench 'topEntity) #-}
