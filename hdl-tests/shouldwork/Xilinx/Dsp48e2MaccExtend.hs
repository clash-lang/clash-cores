-- | Like "Dsp48e2", but for the \"MACC extend\" mode, which uses
-- @MULTSIGNIN@. Only works in Verilog: the VHDL UNISIM model adds @MULTSIGNIN@
-- to bit 0 of the X multiplexer, whereas the Verilog model (and the Haskell
-- model) adds it to bit 1.
module Dsp48e2MaccExtend where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Dsp48e2
import Dsp48e2Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbMaccExtend :: Signal Dom Bool
tbMaccExtend =
  tb (cascaded defDsp48e2Config (cascadeSecond defDsp48e2Config))
    $(cascadedBlobs defDsp48e2Config (cascadeSecond defDsp48e2Config) defGenOpts{maccExtend = True} 15)
{-# ANN tbMaccExtend (TestBench 'topEntity) #-}

-- | Only the output registers. (\"MACC extend\" uses the P feedback, which
-- requires the output register.)
tbMaccExtendPRegOnly :: Signal Dom Bool
tbMaccExtendPRegOnly =
  tb (cascaded pRegOnly (cascadeSecond pRegOnly))
    $(cascadedBlobs pRegOnly (cascadeSecond pRegOnly) defGenOpts{maccExtend = True} 17)
{-# ANN tbMaccExtendPRegOnly (TestBench 'topEntity) #-}
