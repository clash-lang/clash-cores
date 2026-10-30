{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Configurations for the @XPM_MEMORY_SDPRAM@ tests, see "XpmMemoryCommon"
module XpmMemorySdpramTypes where

import Clash.Cores.Xilinx.Xpm.Memory
import Clash.Explicit.Prelude

import XpmMemoryCommon

-- | A simple dual port RAM driven by 'stimuli'. Port A does not have an output,
-- so it outputs zero.
dut ::
  forall depth nBytes w latB domA domB.
  ( KnownNat depth, 2 <= depth
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat w, 1 <= w, w <= 64
  , KnownDomain domA
  , KnownDomain domB ) =>
  XpmMemoryConfig ->
  XpmMemoryPortConfig latB (BitVector w) ->
  Clock domA ->
  Clock domB ->
  (Signal domA (BitVector w), Signal domB (BitVector w))
dut cfg portB clkA clkB =
  ( pure 0
  , xpmMemorySdpramWith @latB @depth @nBytes cfg portB
      clkA (psEnable a) (psAddress a) (psWriteEnable a) (psData a)
      clkB (psReset b) (psEnable b) (psRegce b) (psAddress b)
  )
 where
  -- Only port A writes, so there are no write/write collisions
  a = stimuli 50 Nothing clkA 0x0123456789ABCDEF
  b = stimuli 0 Nothing clkB 0xFEDCBA9876543210 :: PortStimuli domB depth 1 w

type DutS w = Clock XilinxSystem -> Clock XilinxSystem -> (Signal XilinxSystem (BitVector w), Signal XilinxSystem (BitVector w))

-- | Read first: reading an address that is written returns the old contents
dut0 :: DutS 16
dut0 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d1 ReadFirst 0x1234)

-- | No change: reading an address that is written is a collision
dut1 :: DutS 16
dut1 = dut @16 @1
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d2 NoChange 0x4321)

-- | Distributed RAM, asynchronous read
dut2 :: DutS 12
dut2 = dut @16 @1
  defaultXpmMemoryConfig{memoryPrimitive = DistributedRam}
  (XpmMemoryPortConfig d0 ReadFirst 0)

-- | UltraRAM, write first: reading an address that is written returns the new
-- contents
dut3 :: DutS 16
dut3 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = UltraRam}
  (XpmMemoryPortConfig d3 WriteFirst 0)

-- | UltraRAM, read first
dut4 :: DutS 16
dut4 = dut @16 @1
  defaultXpmMemoryConfig{memoryPrimitive = UltraRam}
  (XpmMemoryPortConfig d1 ReadFirst 0)

-- | Independent clocks, initialized
dut5 :: Clock D100 -> Clock D70 -> (Signal D100 (BitVector 16), Signal D70 (BitVector 16))
dut5 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam, memoryInit = InitFile $(memFilePath "init_16x16.mem")}
  (XpmMemoryPortConfig d2 NoChange 0xFFFF)

-- | Independent clocks, asynchronous reset
dut6 :: Clock D70 -> Clock D100Async -> (Signal D70 (BitVector 18), Signal D100Async (BitVector 18))
dut6 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d3 ReadFirst 0)

-- | Independent clocks, distributed RAM
dut7 :: Clock D100 -> Clock D70 -> (Signal D100 (BitVector 8), Signal D70 (BitVector 8))
dut7 = dut @64 @1
  defaultXpmMemoryConfig{memoryPrimitive = DistributedRam, memoryInit = InitFile $(memFilePath "init_64x8.mem")}
  (XpmMemoryPortConfig d1 ReadFirst 0x5A)
