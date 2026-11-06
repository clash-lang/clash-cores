{- | Compares the Haskell simulation model of 'sbMac16' to Yosys' simulation
model of @SB_MAC16@, for a number of configurations, each with random stimuli.
See "SbMac16Types" and "Ice40TestCommon".
-}
module SbMac16 where

import Clash.Explicit.Prelude

import Ice40TestCommon
import SbMac16Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbMult16Unsigned :: Signal Dom Bool
tbMult16Unsigned = tb (mac16Circuit mult16Unsigned) $(mac16Blobs mult16Unsigned 1)
{-# ANN tbMult16Unsigned (TestBench 'topEntity) #-}

tbMult16Signed :: Signal Dom Bool
tbMult16Signed = tb (mac16Circuit mult16Signed) $(mac16Blobs mult16Signed 2)
{-# ANN tbMult16Signed (TestBench 'topEntity) #-}

tbMult8x8 :: Signal Dom Bool
tbMult8x8 = tb (mac16Circuit mult8x8) $(mac16Blobs mult8x8 3)
{-# ANN tbMult8x8 (TestBench 'topEntity) #-}

tbMac32 :: Signal Dom Bool
tbMac32 = tb (mac16Circuit mac32) $(mac16Blobs mac32 4)
{-# ANN tbMac32 (TestBench 'topEntity) #-}

tbAddSub16 :: Signal Dom Bool
tbAddSub16 = tb (mac16Circuit addSub16) $(mac16Blobs addSub16 5)
{-# ANN tbAddSub16 (TestBench 'topEntity) #-}

tbAccum8x8 :: Signal Dom Bool
tbAccum8x8 = tb (mac16Circuit accum8x8) $(mac16Blobs accum8x8 6)
{-# ANN tbAccum8x8 (TestBench 'topEntity) #-}

tbSignExtension :: Signal Dom Bool
tbSignExtension = tb (mac16Circuit signExtension) $(mac16Blobs signExtension 7)
{-# ANN tbSignExtension (TestBench 'topEntity) #-}

tbMult16Mixed :: Signal Dom Bool
tbMult16Mixed = tb (mac16Circuit mult16Mixed) $(mac16Blobs mult16Mixed 8)
{-# ANN tbMult16Mixed (TestBench 'topEntity) #-}

tbMult16MixedB :: Signal Dom Bool
tbMult16MixedB = tb (mac16Circuit mult16MixedB) $(mac16Blobs mult16MixedB 9)
{-# ANN tbMult16MixedB (TestBench 'topEntity) #-}
