{- | Instantiates components of which some, but not all, ports have a
zero-width type. These should be left out of the port map, while the other
output ports should still be matched to the right tuple elements. In the past
'inst' would error out, see https://github.com/clash-lang/clash-cores/issues/35.
-}
module ZeroWidthPorts where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

type Dom = XilinxSystem

-- | Instantiates @IBUFDS_DIFF_OUT@, a differential input buffer with
-- complementary outputs @O@ and @OB@. Zero-width ports are interspersed with
-- the existing ports. They don't exist on the primitive, so this would fail to
-- elaborate if they were rendered.
ibufdsDiffOut :: Signal Dom Bit -> Signal Dom Bit -> (Signal Dom Bit, Signal Dom Bit)
ibufdsDiffOut i ib = (unPort o, unPort ob)
 where
  (_, o, _, ob) = go

  go ::
    ( Port "zero_width_0" Dom ()
    , Port "O" Dom Bit
    , Port "zero_width_1" Dom (BitVector 0)
    , Port "OB" Dom Bit
    )
  go =
    inst
      (instConfig "IBUFDS_DIFF_OUT")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }
      (Port @"zero_width_in_0" (pure () :: Signal Dom ()))
      (Port @"I" i)
      (Port @"zero_width_in_1" (pure 0 :: Signal Dom (BitVector 0)))
      (Port @"IB" ib)

-- | Instantiates @VCC@, with a zero-width port next to its only output port,
-- @P@. Clash represents a tuple with a single non-zero-width element as that
-- element.
vcc :: Signal Dom Bit
vcc = unPort p
 where
  (p, _) = go

  go :: (Port "P" Dom Bit, Port "zero_width" Dom (Vec 0 Bit))
  go =
    inst
      (instConfig "VCC")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }

topEntity :: Signal Dom Bit -> Signal Dom (BitVector 3)
topEntity i = bundle3 <$> o <*> ob <*> vcc
 where
  (o, ob) = ibufdsDiffOut i (complement <$> i)
  bundle3 a b c = pack (a, b, c)
{-# OPAQUE topEntity #-}

testBench :: Signal Dom Bool
testBench = done
 where
  input = stimuliGenerator clk rst (0 :> 1 :> 1 :> 0 :> 1 :> 0 :> 0 :> Nil)
  -- (O, OB, VCC)
  expected = 0b011 :> 0b101 :> 0b101 :> 0b011 :> 0b101 :> 0b011 :> 0b011 :> Nil
  done = outputVerifier' clk rst expected (topEntity input)
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# OPAQUE testBench #-}
