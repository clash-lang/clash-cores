module DcFifo0 where

import Clash.Cores.Xilinx.DcFifo
import Clash.Explicit.Prelude

import DcFifo.Abstract

topEntity :: ConfiguredFifo (BitVector 16) Dom17 Dom2
topEntity = dcFifo defConfig
{-# NOINLINE topEntity #-}

testBench :: Signal Dom17 Bool
testBench = mkTestBench topEntity
{-# NOINLINE testBench #-}
