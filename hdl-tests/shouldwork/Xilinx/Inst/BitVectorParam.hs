{- | Instantiates UNISIM's @LUT6@, whose @INIT@ generic is a @bit_vector@ in
VHDL. A 'Param' carrying a 'BitVector' renders as a @std_logic_vector@ in VHDL,
which doesn't type check against it, see
https://github.com/clash-lang/clash-cores/issues/81.
-}
module BitVectorParam where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

type Dom = XilinxSystem

-- | @INIT@ of a @LUT6@ calculating the parity of its inputs
parityInit :: BitVector 64
parityInit = 0x6996966996696996

-- | @INIT@ of a @LUT6@ calculating the parity of its inputs, inverted if input
-- @I5@ is set. Clash has to calculate this constant at compile time.
parityXorI5Init :: BitVector 64
parityXorI5Init = parityInit `xor` (maxBound `shiftL` 32)

-- | Instantiates UNISIM's @LUT6@ with the given @INIT@
lut6 ::
  forall dom .
  KnownDomain dom =>
  BitVector 64 ->
  Signal dom (BitVector 6) ->
  Signal dom Bit
lut6 initVal i = unPort go
 where
  go :: Port "O" dom Bit
  go =
    inst
      (instConfig "LUT6")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }
      (BitVectorParam @"INIT" initVal)
      (Port @"I0" ((! (0 :: Int)) <$> i))
      (Port @"I1" ((! (1 :: Int)) <$> i))
      (Port @"I2" ((! (2 :: Int)) <$> i))
      (Port @"I3" ((! (3 :: Int)) <$> i))
      (Port @"I4" ((! (4 :: Int)) <$> i))
      (Port @"I5" ((! (5 :: Int)) <$> i))
{-# INLINE lut6 #-}

topEntity :: Signal Dom (BitVector 6) -> Signal Dom (BitVector 2)
topEntity i = pack <$> bundle (lut6 parityInit i, lut6 parityXorI5Init i)
{-# OPAQUE topEntity #-}

testBench :: Signal Dom Bool
testBench = done
 where
  inputs = iterateI (+1) 0 :: Vec 64 (BitVector 6)
  expected = map (\i -> pack (parityInit ! i, parityXorI5Init ! i)) inputs
  done = outputVerifier' clk rst expected (topEntity (stimuliGenerator clk rst inputs))
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# OPAQUE testBench #-}
