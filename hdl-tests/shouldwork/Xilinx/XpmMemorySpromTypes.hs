{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Configurations for the @XPM_MEMORY_SPROM@ tests, see "XpmMemoryCommon"
module XpmMemorySpromTypes where

import Clash.Cores.Xilinx.Xpm.Memory
import Clash.Explicit.Prelude

import XpmMemoryCommon

-- | A single port ROM driven by 'stimuli'. The test bench needs two ports, the
-- second one outputs zero.
dut ::
  forall depth w lat dom.
  ( KnownNat depth, 2 <= depth
  , KnownNat w, 1 <= w, w <= 64
  , KnownDomain dom ) =>
  XpmMemoryConfig ->
  XpmMemoryPortConfig lat (BitVector w) ->
  Clock dom ->
  Clock dom ->
  (Signal dom (BitVector w), Signal dom (BitVector w))
dut cfg port clk _ =
  ( xpmMemorySpromWith @lat @depth cfg port
      clk (psReset a) (psEnable a) (psRegce a) (psAddress a)
  , pure 0
  )
 where
  a = stimuli 0 Nothing clk 0x0123456789ABCDEF :: PortStimuli dom depth 1 w

type Dut dom w = Clock dom -> Clock dom -> (Signal dom (BitVector w), Signal dom (BitVector w))

-- | Block RAM, read latency 1
dut0 :: Dut XilinxSystem 16
dut0 = dut @16
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam, memoryInit = InitFile $(memFilePath "init_16x16.mem")}
  (XpmMemoryPortConfig d1 ReadFirst 0x1234)

-- | Read latency 3
dut1 :: Dut XilinxSystem 32
dut1 = dut @32
  defaultXpmMemoryConfig{memoryInit = InitFile $(memFilePath "init_32x32.mem")}
  (XpmMemoryPortConfig d3 ReadFirst 0xFFFF0000)

-- | Distributed RAM, asynchronous read
dut2 :: Dut XilinxSystem 8
dut2 = dut @64
  defaultXpmMemoryConfig{memoryPrimitive = DistributedRam, memoryInit = InitFile $(memFilePath "init_64x8.mem")}
  (XpmMemoryPortConfig d0 ReadFirst 0)

-- | Asynchronous reset, read latency 2
dut3 :: Dut D100Async 18
dut3 = dut @16
  defaultXpmMemoryConfig{memoryPrimitive = BlockRam, memoryInit = InitFile $(memFilePath "init_16x18.mem")}
  (XpmMemoryPortConfig d2 ReadFirst 0)
