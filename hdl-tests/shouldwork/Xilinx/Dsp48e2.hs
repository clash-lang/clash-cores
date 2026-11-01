-- | Compares the Haskell simulation model of the @DSP48E2@ to the UNISIM model,
-- for a number of configurations, each with random stimuli. See
-- "Dsp48e2Types".
module Dsp48e2 where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Dsp48e2
import Dsp48e2Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

-- | Default configuration: all registers, multiplier
tbDefault :: Signal Dom Bool
tbDefault = tb (single defDsp48e2Config) $(singleBlobs defDsp48e2Config defGenOpts 1)
{-# ANN tbDefault (TestBench 'topEntity) #-}

-- | No registers at all, dynamic multiplier usage
tbCombinational :: Signal Dom Bool
tbCombinational = tb (single combinational) $(singleBlobs combinational defGenOpts 2)
{-# ANN tbCombinational (TestBench 'topEntity) #-}

-- | All registers, two A and B registers, inversions, pre-adder with A
tbPipelined :: Signal Dom Bool
tbPipelined = tb (single pipelined) $(singleBlobs pipelined defGenOpts 3)
{-# ANN tbPipelined (TestBench 'topEntity) #-}

-- | Pre-adder with B, A and B from the cascade inputs
tbPreAddB :: Signal Dom Bool
tbPreAddB = tb (single preAddB) $(singleBlobs preAddB defGenOpts 4)
{-# ANN tbPreAddB (TestBench 'topEntity) #-}

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
tbPatDetAttr = tb (single patDetAttr) $(singleBlobs patDetAttr defGenOpts 7)
{-# ANN tbPatDetAttr (TestBench 'topEntity) #-}

-- | Pattern detector: C as pattern, rounding mode 1, reset on mismatch
tbPatDetRounding :: Signal Dom Bool
tbPatDetRounding =
  tb (single patDetRounding) $(singleBlobs patDetRounding defGenOpts{cGen = CSmall} 8)
{-# ANN tbPatDetRounding (TestBench 'topEntity) #-}

-- | Pattern detector: C as pattern, rounding mode 2
tbPatDetRounding2 :: Signal Dom Bool
tbPatDetRounding2 =
  tb (single patDetRounding2) $(singleBlobs patDetRounding2 defGenOpts{cGen = CSmall} 9)
{-# ANN tbPatDetRounding2 (TestBench 'topEntity) #-}

-- | Pattern detector: C as mask, no output register
tbPatDetMaskC :: Signal Dom Bool
tbPatDetMaskC =
  tb (single patDetMaskC) $(singleBlobs patDetMaskC defGenOpts{cGen = CMostlyOnes} 10)
{-# ANN tbPatDetMaskC (TestBench 'topEntity) #-}

-- | Two cascaded DSPs with all registers
tbCascade :: Signal Dom Bool
tbCascade =
  tb (cascaded defDsp48e2Config (cascadeSecond defDsp48e2Config))
    $(cascadedBlobs defDsp48e2Config (cascadeSecond defDsp48e2Config) defGenOpts 11)
{-# ANN tbCascade (TestBench 'topEntity) #-}

-- | Two cascaded DSPs without registers
tbCascadeCombinational :: Signal Dom Bool
tbCascadeCombinational =
  tb (cascaded combinational (cascadeSecond combinational))
    $(cascadedBlobs combinational (cascadeSecond combinational) defGenOpts 13)
{-# ANN tbCascadeCombinational (TestBench 'topEntity) #-}

-- | 'multiplyAdd' and 'multiplyAccumulate'
tbWrappers :: Signal Dom Bool
tbWrappers = tb wrappers $(wrapperBlobs 19)
{-# ANN tbWrappers (TestBench 'topEntity) #-}
