{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Configurations for the @XPM_MEMORY_DPDISTRAM@ tests, see "XpmMemoryCommon"
module XpmMemoryDpdistramTypes where

import Clash.Cores.Xilinx.Xpm.Memory
import Clash.Explicit.Prelude

import XpmMemoryCommon

-- | A dual port distributed RAM driven by 'stimuli'
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
  xpmMemoryDpdistramWith @latA @latB @depth @nBytes cfg portA portB
    clkA (psReset a) (psEnable a) (psRegce a) (psAddress a) (psWriteEnable a) (psData a)
    clkB (psReset b) (psEnable b) (psRegce b) (psAddress b)
 where
  -- Only port A writes, so there are no write/write collisions
  a = stimuli 50 Nothing clkA 0x0123456789ABCDEF
  b = stimuli 0 Nothing clkB 0xFEDCBA9876543210 :: PortStimuli domB depth 1 w

type DutS w = Clock XilinxSystem -> Clock XilinxSystem -> (Signal XilinxSystem (BitVector w), Signal XilinxSystem (BitVector w))

-- | Common clock, asynchronous reads
dut0 :: DutS 16
dut0 = dut @16 @1
  defaultXpmMemoryConfig
  (XpmMemoryPortConfig d0 ReadFirst 0)
  (XpmMemoryPortConfig d0 ReadFirst 0)

-- | Common clock, byte writes, initialized
dut1 :: DutS 16
dut1 = dut @16 @2
  defaultXpmMemoryConfig{memoryInit = InitFile $(memFilePath "init_16x16.mem")}
  (XpmMemoryPortConfig d1 ReadFirst 0xAAAA)
  (XpmMemoryPortConfig d2 ReadFirst 0x5555)

-- | Independent clocks
dut2 :: Clock D100 -> Clock D70 -> (Signal D100 (BitVector 8), Signal D70 (BitVector 8))
dut2 = dut @64 @1
  defaultXpmMemoryConfig
  (XpmMemoryPortConfig d1 ReadFirst 0x12)
  (XpmMemoryPortConfig d0 ReadFirst 0)

-- | Independent clocks, longer latencies
dut3 :: Clock D70 -> Clock D100 -> (Signal D70 (BitVector 18), Signal D100 (BitVector 18))
dut3 = dut @16 @2
  defaultXpmMemoryConfig{memoryInit = InitFile $(memFilePath "init_16x18.mem")}
  (XpmMemoryPortConfig d2 ReadFirst 0x3FFFF)
  (XpmMemoryPortConfig d3 ReadFirst 0x1)
