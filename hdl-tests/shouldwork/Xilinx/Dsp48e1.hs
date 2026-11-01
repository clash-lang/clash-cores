-- | Compares the Haskell simulation model of the @DSP48E1@ to the UNISIM model,
-- for a number of configurations, each with random stimuli. See
-- "Dsp48e1Types".
module Dsp48e1 where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Dsp48e1
import Dsp48e1Types
import Dsp48TestCommon

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- | Default configuration: all registers, multiplier
tbDefault :: Signal Dom Bool
tbDefault = tb (single defDsp48e1Config) $(singleBlobs defDsp48e1Config defGenOpts 1)
{-# ANN tbDefault (TestBench 'topEntity) #-}

-- | No registers at all, dynamic multiplier usage
tbCombinational :: Signal Dom Bool
tbCombinational = tb (single combinational) $(singleBlobs combinational defGenOpts 2)
{-# ANN tbCombinational (TestBench 'topEntity) #-}

-- | All registers, two A and B registers, inversions, pre-adder
tbPipelined :: Signal Dom Bool
tbPipelined = tb (single pipelined) $(singleBlobs pipelined defGenOpts 3)
{-# ANN tbPipelined (TestBench 'topEntity) #-}

-- | Pre-adder, A and B from the cascade inputs, mix of registers
tbPreAddCascade :: Signal Dom Bool
tbPreAddCascade = tb (single preAddCascade) $(singleBlobs preAddCascade defGenOpts 4)
{-# ANN tbPreAddCascade (TestBench 'topEntity) #-}

-- | SIMD with four 12-bit ALUs
tbSimdFour12 :: Signal Dom Bool
tbSimdFour12 = tb (single simdFour12) $(singleBlobs simdFour12 defGenOpts 5)
{-# ANN tbSimdFour12 (TestBench 'topEntity) #-}

-- | SIMD with two 24-bit ALUs
tbSimdTwo24 :: Signal Dom Bool
tbSimdTwo24 = tb (single simdTwo24) $(singleBlobs simdTwo24 defGenOpts 6)
{-# ANN tbSimdTwo24 (TestBench 'topEntity) #-}

-- | Pattern detector: attribute pattern and mask, reset on match
tbPatDetAttr :: Signal Dom Bool
tbPatDetAttr = tb (single patDetAttr) $(singleBlobs patDetAttr defGenOpts{invalidModes = False} 7)
{-# ANN tbPatDetAttr (TestBench 'topEntity) #-}

-- | Pattern detector: C as pattern, rounding mode 1, reset on mismatch
tbPatDetRounding :: Signal Dom Bool
tbPatDetRounding =
  tb (single patDetRounding) $(singleBlobs patDetRounding defGenOpts{cGen = CSmallOdd, invalidModes = False} 8)
{-# ANN tbPatDetRounding (TestBench 'topEntity) #-}

-- | Pattern detector: C as pattern, rounding mode 2
tbPatDetRounding2 :: Signal Dom Bool
tbPatDetRounding2 =
  tb (single patDetRounding2) $(singleBlobs patDetRounding2 defGenOpts{cGen = CSmallOdd, invalidModes = False} 9)
{-# ANN tbPatDetRounding2 (TestBench 'topEntity) #-}

-- | Pattern detector: C as mask, no output register
tbPatDetMaskC :: Signal Dom Bool
tbPatDetMaskC =
  tb (single patDetMaskC) $(singleBlobs patDetMaskC defGenOpts{cGen = CMostlyOnes} 10)
{-# ANN tbPatDetMaskC (TestBench 'topEntity) #-}

-- | Two cascaded DSPs with all registers
tbCascade :: Signal Dom Bool
tbCascade =
  tb (cascaded defDsp48e1Config (cascadeSecond defDsp48e1Config))
    $(cascadedBlobs defDsp48e1Config (cascadeSecond defDsp48e1Config) defGenOpts 11)
{-# ANN tbCascade (TestBench 'topEntity) #-}

-- | Two cascaded DSPs without registers
tbCascadeCombinational :: Signal Dom Bool
tbCascadeCombinational =
  tb (cascaded combinational (cascadeSecond combinational))
    $(cascadedBlobs combinational (cascadeSecond combinational) defGenOpts 13)
{-# ANN tbCascadeCombinational (TestBench 'topEntity) #-}

-- | Two cascaded DSPs in the \"MACC extend\" mode
tbMaccExtend :: Signal Dom Bool
tbMaccExtend =
  tb (cascaded defDsp48e1Config (cascadeSecond defDsp48e1Config))
    $(cascadedBlobs defDsp48e1Config (cascadeSecond defDsp48e1Config) defGenOpts{maccExtend = True} 15)
{-# ANN tbMaccExtend (TestBench 'topEntity) #-}

-- | Two cascaded DSPs in the \"MACC extend\" mode, only output registers
tbMaccExtendPRegOnly :: Signal Dom Bool
tbMaccExtendPRegOnly =
  tb (cascaded pRegOnly (cascadeSecond pRegOnly))
    $(cascadedBlobs pRegOnly (cascadeSecond pRegOnly) defGenOpts{maccExtend = True} 17)
{-# ANN tbMaccExtendPRegOnly (TestBench 'topEntity) #-}

-- | 'multiplyAdd' and 'multiplyAccumulate'
tbWrappers :: Signal Dom Bool
tbWrappers = tb wrappers $(wrapperBlobs 19)
{-# ANN tbWrappers (TestBench 'topEntity) #-}
