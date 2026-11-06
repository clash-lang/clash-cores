{- | Compares the Haskell simulation model of 'sbRam40_4k' to Yosys'
simulation models of @SB_RAM40_4K@ and @SB_RAM40_4KNRNW@, for a number of
read and write modes, each with random stimuli and initial contents. See
"SbRam40Types" and "Ice40TestCommon".
-}
module SbRam40 where

import Clash.Explicit.Prelude

import Clash.Cores.LatticeSemi.Ice40.Memory
import Ice40TestCommon
import SbRam40Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tb256x16 :: Signal Dom Bool
tb256x16 =
  tb (ramCircuit RamMode256x16 RamMode256x16 $(lift (ramInitWords (randomInit 1))))
    $(ramBlobs @Dom RamMode256x16 RamMode256x16 (ramInitWords (randomInit 1)) 1)
{-# ANN tb256x16 (TestBench 'topEntity) #-}

tb512x8 :: Signal Dom Bool
tb512x8 =
  tb (ramCircuit RamMode512x8 RamMode512x8 $(lift (ramInitWords (randomInit 2))))
    $(ramBlobs @Dom RamMode512x8 RamMode512x8 (ramInitWords (randomInit 2)) 2)
{-# ANN tb512x8 (TestBench 'topEntity) #-}

tb1024x4 :: Signal Dom Bool
tb1024x4 =
  tb (ramCircuit RamMode1024x4 RamMode1024x4 $(lift (ramInitWords (randomInit 3))))
    $(ramBlobs @Dom RamMode1024x4 RamMode1024x4 (ramInitWords (randomInit 3)) 3)
{-# ANN tb1024x4 (TestBench 'topEntity) #-}

tb2048x2 :: Signal Dom Bool
tb2048x2 =
  tb (ramCircuit RamMode2048x2 RamMode2048x2 $(lift init2048x2))
    $(ramBlobs @Dom RamMode2048x2 RamMode2048x2 init2048x2 4)
{-# ANN tb2048x2 (TestBench 'topEntity) #-}

-- | Write 16 bits, read 2 bits
tbWide2Narrow :: Signal Dom Bool
tbWide2Narrow =
  tb (ramCircuit RamMode256x16 RamMode2048x2 $(lift (ramInitWords (randomInit 5))))
    $(ramBlobs @Dom RamMode256x16 RamMode2048x2 (ramInitWords (randomInit 5)) 5)
{-# ANN tbWide2Narrow (TestBench 'topEntity) #-}

-- | Write 4 bits, read 8 bits
tbNarrow2Wide :: Signal Dom Bool
tbNarrow2Wide =
  tb (ramCircuit RamMode1024x4 RamMode512x8 $(lift (ramInitWords (randomInit 6))))
    $(ramBlobs @Dom RamMode1024x4 RamMode512x8 (ramInitWords (randomInit 6)) 6)
{-# ANN tbNarrow2Wide (TestBench 'topEntity) #-}

-- | Both clocks inverted: @SB_RAM40_4KNRNW@
tbFalling :: Signal DomFalling Bool
tbFalling =
  tb (ramCircuit RamMode512x8 RamMode256x16 $(lift (ramInitWords (randomInit 7))))
    $(ramBlobs @DomFalling RamMode512x8 RamMode256x16 (ramInitWords (randomInit 7)) 7)
{-# ANN tbFalling (TestBench 'topEntity) #-}

-- | Write clock in 'Dom', read clock in 'Fast'
tbDualClock :: Signal Dom Bool
tbDualClock = tbDdr ramCircuitDual $(ramDualBlobs 8)
{-# ANN tbDualClock (TestBench 'topEntity) #-}
