{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell models of 'Clash.Cores.Intel.Cdc.stdSynchronizer' and
'Clash.Cores.Intel.Cdc.stdSynchronizerBundle' against Intel's simulation
models (@altera_mf@), for several depths.
-}
module Synchronizer where

import Clash.Explicit.Prelude

import IntelTestCommon
import SynchronizerTypes

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbSingle2 :: Signal System Bool
tbSingle2 = tb (dutSingle d2) $(blobs (dutSingle d2) (genStimuli 1))
{-# ANN tbSingle2 (TestBench 'topEntity) #-}

tbSingle3 :: Signal System Bool
tbSingle3 = tb (dutSingle d3) $(blobs (dutSingle d3) (genStimuli 2))
{-# ANN tbSingle3 (TestBench 'topEntity) #-}

tbBundle3 :: Signal System Bool
tbBundle3 = tb (dutBundle d3) $(blobs (dutBundle d3) (genStimuli 3))
{-# ANN tbBundle3 (TestBench 'topEntity) #-}

tbBundle5 :: Signal System Bool
tbBundle5 = tb (dutBundle d5) $(blobs (dutBundle d5) (genStimuli 4))
{-# ANN tbBundle5 (TestBench 'topEntity) #-}
