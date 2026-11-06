{- | Instantiates components with a differential clock output, see
https://github.com/clash-lang/clash-cores/issues/33.

To be able to observe the differential clocks, we feed data through the
differential buffers instead of a clock.
-}
module DiffClockOutput where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

type Dom = XilinxSystem

-- | Differential output buffer @OBUFDS@: drives @O@ with @I@ and @OB@ with its
-- complement.
obufds :: Signal Dom Bit -> DiffClock Dom
obufds i = unPort go
 where
  go :: NamedDiffClockPort "O" "OB" Dom
  go =
    inst
      (instConfig "OBUFDS")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }
      (Port @"I" i)

-- | Differential input buffer @IBUFDS_DIFF_OUT@: drives @O@ with @I@ and @OB@
-- with its complement, if @I@ and @IB@ are complementary. Its outputs are
-- interpreted as a differential clock, accompanied by a zero-width port.
ibufdsDiffOutDiffClock :: DiffClock Dom -> DiffClock Dom
ibufdsDiffOutDiffClock diffClk = unPort o
 where
  (_, o) = go

  go :: (Port "zero_width" Dom (), NamedDiffClockPort "O" "OB" Dom)
  go =
    inst
      (instConfig "IBUFDS_DIFF_OUT")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }
      (NamedDiffClockPort @"I" @"IB" diffClk)

-- | Differential input buffer @IBUFDS_DIFF_OUT@, with its outputs interpreted
-- as data.
ibufdsDiffOut :: DiffClock Dom -> (Signal Dom Bit, Signal Dom Bit)
ibufdsDiffOut diffClk = (unPort o, unPort ob)
 where
  (o, ob) = go

  go :: (Port "O" Dom Bit, Port "OB" Dom Bit)
  go =
    inst
      (instConfig "IBUFDS_DIFF_OUT")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }
      (NamedDiffClockPort @"I" @"IB" diffClk)

-- | Feeds the input through 'obufds' and 'ibufdsDiffOut', and through 'obufds',
-- 'ibufdsDiffOutDiffClock' and 'ibufdsDiffOut'. Both should yield the input and
-- its complement. Note that we check the result of the shorter chain too, as
-- swapping the positive and negative phase twice would go unnoticed.
topEntity :: Signal Dom Bit -> Signal Dom (BitVector 4)
topEntity i = pack <$> bundle (o1, ob1, o2, ob2)
 where
  diffClk = obufds i
  (o1, ob1) = ibufdsDiffOut diffClk
  (o2, ob2) = ibufdsDiffOut (ibufdsDiffOutDiffClock diffClk)
{-# OPAQUE topEntity #-}

testBench :: Signal Dom Bool
testBench = done
 where
  input = stimuliGenerator clk rst (0 :> 1 :> 1 :> 0 :> 1 :> 0 :> 0 :> Nil)
  expected = 0b0101 :> 0b1010 :> 0b1010 :> 0b0101 :> 0b1010 :> 0b0101 :> 0b0101 :> Nil
  done = outputVerifier' clk rst expected (topEntity input)
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# OPAQUE testBench #-}
