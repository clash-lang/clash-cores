{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Configurations for the @XPM_MEMORY_DPROM@ tests, see "XpmMemoryCommon"
module XpmMemoryDpromTypes where

import Clash.Cores.Xilinx.Xpm.Memory
import Clash.Explicit.Prelude

import XpmMemoryCommon

-- | A dual port ROM driven by 'stimuli'
dut ::
  forall depth w latA latB domA domB.
  ( KnownNat depth, 2 <= depth
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
  xpmMemoryDpromWith @latA @latB @depth cfg portA portB
    clkA (psReset a) (psEnable a) (psRegce a) (psAddress a)
    clkB (psReset b) (psEnable b) (psRegce b) (psAddress b)
 where
  a = stimuli 0 Nothing clkA 0x0123456789ABCDEF :: PortStimuli domA depth 1 w
  b = stimuli 0 Nothing clkB 0xFEDCBA9876543210 :: PortStimuli domB depth 1 w

-- | Common clock, block RAM
dut0 :: Clock XilinxSystem -> Clock XilinxSystem -> (Signal XilinxSystem (BitVector 16), Signal XilinxSystem (BitVector 16))
dut0 = dut @16
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam, memoryInit = InitFile $(memFilePath "init_16x16.mem")}
  (XpmMemoryPortConfig d1 ReadFirst 0x1234)
  (XpmMemoryPortConfig d2 ReadFirst 0x5678)

-- | Independent clocks, distributed RAM
dut1 :: Clock D100 -> Clock D70 -> (Signal D100 (BitVector 8), Signal D70 (BitVector 8))
dut1 = dut @64
  defaultXpmMemoryConfig{memoryPrimitive = DistributedRam, memoryInit = InitFile $(memFilePath "init_64x8.mem")}
  (XpmMemoryPortConfig d0 ReadFirst 0)
  (XpmMemoryPortConfig d1 ReadFirst 0x11)

-- | Independent clocks, asynchronous reset
dut2 :: Clock D70 -> Clock D100Async -> (Signal D70 (BitVector 32), Signal D100Async (BitVector 32))
dut2 = dut @32
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam, memoryInit = InitFile $(memFilePath "init_32x32.mem")}
  (XpmMemoryPortConfig d3 ReadFirst 0xABCDEF)
  (XpmMemoryPortConfig d1 ReadFirst 0)
