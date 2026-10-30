{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Configurations for the @XPM_MEMORY_SPRAM@ tests, see "XpmMemoryCommon"
module XpmMemorySpramTypes where

import Clash.Cores.Xilinx.Xpm.Memory
import Clash.Explicit.Prelude

import XpmMemoryCommon

-- | A single port RAM driven by 'stimuli'. The test bench needs two ports, the
-- second one outputs zero.
dut ::
  forall depth nBytes w lat dom.
  ( KnownNat depth, 2 <= depth
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat w, 1 <= w, w <= 64
  , KnownDomain dom ) =>
  XpmMemoryConfig ->
  XpmMemoryPortConfig lat (BitVector w) ->
  Clock dom ->
  Clock dom ->
  (Signal dom (BitVector w), Signal dom (BitVector w))
dut cfg port clk _ =
  ( xpmMemorySpramWith @lat @depth @nBytes cfg port
      clk (psReset a) (psEnable a) (psRegce a) (psAddress a) (psWriteEnable a) (psData a)
  , pure 0
  )
 where
  a = stimuli 40 Nothing clk 0x0123456789ABCDEF

type Dut dom w = Clock dom -> Clock dom -> (Signal dom (BitVector w), Signal dom (BitVector w))

-- | Write first, read latency 1
dut0 :: Dut XilinxSystem 16
dut0 = dut @16 @1
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d1 WriteFirst 0x1234)

-- | Read first, byte writes, read latency 2
dut1 :: Dut XilinxSystem 16
dut1 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d2 ReadFirst 0x8001)

-- | No change, read latency 3, initialized
dut2 :: Dut XilinxSystem 18
dut2 = dut @16 @2
  defaultXpmMemoryConfig{memoryInit = InitFile $(memFilePath "init_16x18.mem")}
  (XpmMemoryPortConfig d3 NoChange 0x2AAAA)

-- | Distributed RAM, asynchronous read
dut3 :: Dut XilinxSystem 8
dut3 = dut @64 @1
  defaultXpmMemoryConfig{memoryPrimitive = DistributedRam, memoryInit = InitFile $(memFilePath "init_64x8.mem")}
  (XpmMemoryPortConfig d0 ReadFirst 0)

-- | UltraRAM, write first
dut4 :: Dut XilinxSystem 16
dut4 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = UltraRam}
  (XpmMemoryPortConfig d2 WriteFirst 0)

-- | Asynchronous reset
dut5 :: Dut D100Async 16
dut5 = dut @16 @1
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam}
  (XpmMemoryPortConfig d1 NoChange 0)

-- | Distributed RAM, 9 bit bytes, read latency 1
dut6 :: Dut XilinxSystem 18
dut6 = dut @16 @2
  defaultXpmMemoryConfig{memoryPrimitive = DistributedRam}
  (XpmMemoryPortConfig d1 ReadFirst 0x12345)
