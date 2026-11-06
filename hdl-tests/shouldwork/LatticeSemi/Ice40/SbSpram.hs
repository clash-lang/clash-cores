{- | Compares the Haskell simulation model of 'sbSpram256ka' to Yosys'
simulation model of @SB_SPRAM256KA@, with random stimuli. See "SbSpramTypes"
and "Ice40TestCommon".
-}
module SbSpram where

import Clash.Explicit.Prelude

import Ice40TestCommon
import SbSpramTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- | Reads and writes only
tbReadWrite :: Signal Dom Bool
tbReadWrite = tb spramCircuit $(spramBlobs 0 0 0 1)
{-# ANN tbReadWrite (TestBench 'topEntity) #-}

-- | With the low-power modes
tbLowPower :: Signal Dom Bool
tbLowPower = tb spramCircuit $(spramBlobs 5 5 1 2)
{-# ANN tbLowPower (TestBench 'topEntity) #-}
