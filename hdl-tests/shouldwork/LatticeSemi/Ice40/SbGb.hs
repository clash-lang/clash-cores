{- | Compares the Haskell simulation models of 'sbGb', 'sbGbSignal', 'sbGbIo'
and 'sbGbIoClock' to Yosys' simulation models of @SB_GB@ and @SB_GB_IO@. See
"SbGbTypes" and "Ice40TestCommon".
-}
module SbGb where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench (tbClockGen)

import Clash.Cores.LatticeSemi.Ice40.IO
import Ice40TestCommon
import SbGbTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- | The test bench runs on a clock through 'sbGb'
tbSbGb :: Signal Dom Bool
tbSbGb =
  tbWithClock (\done -> sbGb (tbClockGen (not <$> done))) registerCircuit $(registerBlobs 1)
{-# ANN tbSbGb (TestBench 'topEntity) #-}

-- | The test bench runs on a clock from a pad, through 'sbGbIoClock'
tbSbGbIoClock :: Signal Dom Bool
tbSbGbIoClock = tbWithClock padClock registerCircuit $(registerBlobs 2)
{-# ANN tbSbGbIoClock (TestBench 'topEntity) #-}

-- | The 'sbio' part of 'sbGbIo'
tbSbGbIoInput :: Signal Dom Bool
tbSbGbIoInput = tb (sbGbIoCircuit $(lift (spiConfig PIN_INPUT_REGISTERED PIN_NO_OUTPUT)))
  $(sbGbIoBlobs (spiConfig PIN_INPUT_REGISTERED PIN_NO_OUTPUT) 3)
{-# ANN tbSbGbIoInput (TestBench 'topEntity) #-}

-- | The 'sbio' part of 'sbGbIo'
tbSbGbIoOutput :: Signal Dom Bool
tbSbGbIoOutput = tb (sbGbIoCircuit $(lift (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED)))
  $(sbGbIoBlobs (spiConfig PIN_INPUT PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED) 4)
{-# ANN tbSbGbIoOutput (TestBench 'topEntity) #-}
