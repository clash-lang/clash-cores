{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Configurations for the @XPM_MEMORY_TDPRAM@ tests, see "XpmMemoryCommon"
module XpmMemoryTdpramTypes where

import Clash.Cores.Xilinx.Xpm.Memory
import Clash.Cores.Xilinx.Xpm.Memory.Internal (isCommonClock)
import Clash.Explicit.Prelude

import XpmMemoryCommon

-- | A true dual port RAM driven by 'stimuli'
dut ::
  forall depth nBytes w latA latB domA domB.
  ( KnownNat depth, 2 <= depth
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat w, 1 <= w, w <= 64
  , KnownDomain domA
  , KnownDomain domB ) =>
  XpmMemoryConfig ->
  XpmMemoryPortConfig latA (BitVector w) ->
  XpmMemoryPortConfig latB (BitVector w) ->
  Clock domA ->
  Clock domB ->
  (Signal domA (BitVector w), Signal domB (BitVector w))
dut cfg portA portB clkA clkB =
  xpmMemoryTdpramWith @latA @latB @depth @nBytes cfg portA portB
    clkA (psReset a) (psEnable a) (psRegce a) (psAddress a) (psWriteEnable a) (psData a)
    clkB (psReset b) (psEnable b) (psRegce b) (psAddress b) (psWriteEnable b) (psData b)
 where
  (parityA, parityB)
    | isCommonClock @domA @domB = (Nothing, Nothing)
    | otherwise = independentWrites
  a = stimuli 40 parityA clkA 0x0123456789ABCDEF
  b = stimuli 40 parityB clkB 0xFEDCBA9876543210

-- | Common clock, word writes, write first, read latency 1
dut0 :: Clock XilinxSystem -> Clock XilinxSystem -> (Signal XilinxSystem (BitVector 16), Signal XilinxSystem (BitVector 16))
dut0 = dut @16 @1
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d1 WriteFirst 0x1234)
  (XpmMemoryPortConfig d1 WriteFirst 0xBEEF)

-- | Common clock, byte writes, read first (no read/write collisions), read
-- latencies 2 and 3
dut1 :: Clock XilinxSystem -> Clock XilinxSystem -> (Signal XilinxSystem (BitVector 16), Signal XilinxSystem (BitVector 16))
dut1 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d2 ReadFirst 0x00FF)
  (XpmMemoryPortConfig d3 ReadFirst 0)

-- | Common clock, 9 bit bytes, no change and write first, initialized
dut2 :: Clock XilinxSystem -> Clock XilinxSystem -> (Signal XilinxSystem (BitVector 18), Signal XilinxSystem (BitVector 18))
dut2 = dut @16 @2
  defaultXpmMemoryConfig{memoryInit = InitFile $(memFilePath "init_16x18.mem")}
  (XpmMemoryPortConfig d1 NoChange 0x3FFFF)
  (XpmMemoryPortConfig d2 WriteFirst 0x15555)

-- | Independent clocks, asynchronous reset on port B
dut3 :: Clock D100 -> Clock D70Async -> (Signal D100 (BitVector 16), Signal D70Async (BitVector 16))
dut3 = dut @16 @1
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d2 NoChange 5)
  (XpmMemoryPortConfig d1 ReadFirst 0)

-- | Independent clocks, byte writes, read latency 3, initialized
dut4 :: Clock D70 -> Clock D100 -> (Signal D70 (BitVector 32), Signal D100 (BitVector 32))
dut4 = dut @32 @4
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam, memoryInit = InitFile $(memFilePath "init_32x32.mem")}
  (XpmMemoryPortConfig d3 WriteFirst 0xDEADBEEF)
  (XpmMemoryPortConfig d3 WriteFirst 0xCAFE)

-- | UltraRAM: port A's operations happen before port B's
dut5 :: Clock XilinxSystem -> Clock XilinxSystem -> (Signal XilinxSystem (BitVector 16), Signal XilinxSystem (BitVector 16))
dut5 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = UltraRam}
  (XpmMemoryPortConfig d1 NoChange 0)
  (XpmMemoryPortConfig d3 NoChange 0)

-- | Independent fast clocks, which use a smaller collision window
dut6 :: Clock D400 -> Clock D500 -> (Signal D400 (BitVector 8), Signal D500 (BitVector 8))
dut6 = dut @8 @1
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d1 ReadFirst 0x11)
  (XpmMemoryPortConfig d2 WriteFirst 0x22)

-- | Independent clocks with coinciding edges
dut7 :: Clock D100 -> Clock D100b -> (Signal D100 (BitVector 12), Signal D100b (BitVector 12))
dut7 = dut @8 @1
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d1 WriteFirst 0x123)
  (XpmMemoryPortConfig d1 NoChange 0x456)

-- | Common clock with an asynchronous reset
dut8 :: Clock D100Async -> Clock D100Async -> (Signal D100Async (BitVector 16), Signal D100Async (BitVector 16))
dut8 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d1 NoChange 0)
  (XpmMemoryPortConfig d2 ReadFirst 0)
