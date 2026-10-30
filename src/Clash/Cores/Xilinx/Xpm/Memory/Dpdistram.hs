{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}


module Clash.Cores.Xilinx.Xpm.Memory.Dpdistram
  ( xpmMemoryDpdistram
  , xpmMemoryDpdistramWith
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Memory.Internal

-- | Dual port distributed RAM: port A reads and writes, port B only reads.
-- Instantiates Xilinx's @XPM_MEMORY_DPDISTRAM@, see
-- [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_DPDISTRAM).
--
-- Uses 'defaultXpmMemoryPortConfig' for both ports: a read latency of /2/ and a
-- reset value of zero. The output registers are always enabled (@regcea@ and
-- @regceb@ are tied high). Use 'xpmMemoryDpdistramWith' to change these
-- settings.
--
-- See "Clash.Cores.Xilinx.Xpm.Memory" for the behavior of the ports.
xpmMemoryDpdistram ::
  forall depth nBytes a domA domB.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
  , KnownNat nBytes, 1 <= nBytes
  , KnownDomain domA
  , KnownDomain domB
  , BitPack a, 1 <= BitSize a
  , NFDataX a
  ) =>
  Clock domA ->
  -- | Synchronously resets port A's output register to its reset value
  Reset domA ->
  -- | Port A enable
  Enable domA ->
  "addra" ::: Signal domA (Index depth) ->
  -- | Port A write enable, one bit per byte. Use @BitVector 1@ to write whole
  -- words, or a write enable of @n@ bits to write @BitSize a / n@ bit wide
  -- bytes, which must be /8/ or /9/ bits wide. The first bit enables the least
  -- significant byte.
  "wea" ::: Signal domA (BitVector nBytes) ->
  "dina" ::: Signal domA a ->
  Clock domB ->
  Reset domB ->
  Enable domB ->
  "addrb" ::: Signal domB (Index depth) ->
  ( "douta" ::: Signal domA a
  , "doutb" ::: Signal domB a
  )
xpmMemoryDpdistram clkA rstA enA addrA weA dinA clkB rstB enB addrB =
  xpmMemoryDpdistramWith
    defaultXpmMemoryConfig
    defaultXpmMemoryPortConfig
    defaultXpmMemoryPortConfig
    clkA rstA enA (pure True) addrA weA dinA
    clkB rstB enB (pure True) addrB
{-# INLINE xpmMemoryDpdistram #-}

-- | Like 'xpmMemoryDpdistram', but with a configurable memory and ports, and
-- explicit output register enables. The memory primitive of the memory
-- configuration and the write modes of the port configurations are ignored:
-- this macro always uses distributed RAM in 'ReadFirst' mode. Distributed RAM
-- supports a read latency of /0/, i.e., asynchronous reads. Domains with
-- asynchronous resets are not supported.
xpmMemoryDpdistramWith ::
  forall latA latB depth nBytes a domA domB.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
  , KnownNat nBytes, 1 <= nBytes
  , KnownDomain domA
  , KnownDomain domB
  , BitPack a, 1 <= BitSize a
  , NFDataX a
  ) =>
  XpmMemoryConfig ->
  XpmMemoryPortConfig latA a ->
  XpmMemoryPortConfig latB a ->
  Clock domA ->
  -- | Synchronously resets port A's output register to its reset value
  Reset domA ->
  -- | Port A enable
  Enable domA ->
  -- | Enable of port A's last output register. Ignored for read latencies
  -- smaller than /2/.
  "regcea" ::: Signal domA Bool ->
  "addra" ::: Signal domA (Index depth) ->
  -- | Port A write enable, one bit per byte. Use @BitVector 1@ to write whole
  -- words, or a write enable of @n@ bits to write @BitSize a / n@ bit wide
  -- bytes, which must be /8/ or /9/ bits wide. The first bit enables the least
  -- significant byte.
  "wea" ::: Signal domA (BitVector nBytes) ->
  "dina" ::: Signal domA a ->
  Clock domB ->
  Reset domB ->
  Enable domB ->
  "regceb" ::: Signal domB Bool ->
  "addrb" ::: Signal domB (Index depth) ->
  ( "douta" ::: Signal domA a
  , "doutb" ::: Signal domB a
  )
xpmMemoryDpdistramWith XpmMemoryConfig{memoryInit = memInit} portA0 portB0
  clkA rstA enA regceA addrA weA dinA
  clkB rstB enB regceB addrB
  | Just msg <- err = clashCompileError msg
  | clashSimulation = sim
  | otherwise = synth
 where
  portA = portA0{writeMode = ReadFirst}
  portB = portB0{writeMode = ReadFirst}

  err =
    configError
      "xpmMemoryDpdistramWith" DualPortDistributedRam DistributedRam
      (isCommonClock @domA @domB) (natToNum @depth) (natToNum @(BitSize a))
      (portCheck @domA "A" True True (natToNum @nBytes) portA)
      (portCheck @domB "B" True False 1 portB)

  sim = (unpack <$> outA, unpack <$> outB)
   where
    (outA, outB) =
      xpmMemoryModel
        ModelConfig
          { mcMemoryType = DualPortDistributedRam
          , mcPrimitive = DistributedRam
          , mcInit = memInit
          , mcDepth = natToNum @depth
          , mcPortA = portModelConfig @domA portA
          , mcPortB = portModelConfig @domB portB
          }
        clkA (portInputs rstA enA regceA addrA weA dinA)
        clkB (portInputs rstB enB regceB addrB (pure (0 :: BitVector 1)) (pure (unpack 0 :: a)))

  synth = (unpack <$> unPort douta, unpack <$> unPort doutb)
   where
    (douta, doutb) = go
    go ::
      ( Port "douta" domA (BitVector (BitSize a))
      , Port "doutb" domB (BitVector (BitSize a))
      )
    go =
      inst
        (instConfig "xpm_memory_dpdistram")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"ADDR_WIDTH_A"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"ADDR_WIDTH_B"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"BYTE_WRITE_WIDTH_A"  @Integer byteWidth)
        (Param @"CLOCKING_MODE"       @String  (clockingModeParam @domA @domB))
        (Param @"MEMORY_INIT_FILE"    @String  (initFileParam memInit))
        (Param @"MEMORY_SIZE"         @Integer (natToNum @depth * width))
        (Param @"READ_DATA_WIDTH_A"   @Integer width)
        (Param @"READ_DATA_WIDTH_B"   @Integer width)
        (Param @"READ_LATENCY_A"      @Integer (snatToInteger (readLatency portA)))
        (Param @"READ_LATENCY_B"      @Integer (snatToInteger (readLatency portB)))
        (HexStringParam @"READ_RESET_VALUE_A" (pack (readResetValue portA)))
        (HexStringParam @"READ_RESET_VALUE_B" (pack (readResetValue portB)))
        (Param @"RST_MODE_A"          @String  (rstModeParam @domA))
        (Param @"RST_MODE_B"          @String  (rstModeParam @domB))
        (Param @"WRITE_DATA_WIDTH_A"  @Integer width)

        (ClockPort @"clka"            clkA)
        (ResetPort @"rsta" @ActiveHigh rstA)
        (Port      @"ena"             (boolToBit <$> fromEnable enA))
        (Port      @"regcea"          (boolToBit <$> regceA))
        (Port      @"wea"             weA)
        (Port      @"addra"           (pack <$> addrA))
        (Port      @"dina"            (pack <$> dinA))

        (ClockPort @"clkb"            clkB)
        (ResetPort @"rstb" @ActiveHigh rstB)
        (Port      @"enb"             (boolToBit <$> fromEnable enB))
        (Port      @"regceb"          (boolToBit <$> regceB))
        (Port      @"addrb"           (pack <$> addrB))

  width = natToNum @(BitSize a) :: Integer
  byteWidth = width `div` natToNum @nBytes
{-# INLINE xpmMemoryDpdistramWith #-}
