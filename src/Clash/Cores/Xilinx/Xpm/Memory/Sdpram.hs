{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Memory.Sdpram
  ( xpmMemorySdpram
  , xpmMemorySdpramWith
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Memory.Internal

-- | Simple dual port RAM: port A writes, port B reads. Instantiates Xilinx's
-- @XPM_MEMORY_SDPRAM@, see
-- [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_SDPRAM).
--
-- Uses 'defaultXpmMemoryConfig' and 'defaultXpmMemoryPortConfig' for port B:
-- block RAM or distributed RAM (chosen by Vivado), no initialization, a read
-- latency of /2/, 'NoChange' (which makes reading an address that is being
-- written a collision) and a reset value of zero. The output register is always
-- enabled (@regceb@ is tied high). Use 'xpmMemorySdpramWith' to change these
-- settings.
--
-- See "Clash.Cores.Xilinx.Xpm.Memory" for the behavior of the ports, including
-- collisions.
xpmMemorySdpram ::
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
  -- | Synchronously or asynchronously (depending on the domain) resets port B's
  -- output register to its reset value
  Reset domB ->
  -- | Port B enable
  Enable domB ->
  "addrb" ::: Signal domB (Index depth) ->

  "doutb" ::: Signal domB a
xpmMemorySdpram clkA enA addrA weA dinA clkB rstB enB addrB =
  xpmMemorySdpramWith
    defaultXpmMemoryConfig
    defaultXpmMemoryPortConfig
    clkA enA addrA weA dinA
    clkB rstB enB (pure True) addrB
{-# INLINE xpmMemorySdpram #-}

-- | Like 'xpmMemorySdpram', but with a configurable memory and read port, and
-- an explicit output register enable.
--
-- The write mode of the port configuration determines what happens when port B
-- reads an address port A writes at the same time: with a common clock and
-- 'ReadFirst', port B reads the old contents. Otherwise, this is a collision
-- and port B reads an undefined value. Block RAM does not support 'WriteFirst'.
-- UltraRAM does not support 'NoChange', and in 'WriteFirst' mode port B reads
-- the new contents.
xpmMemorySdpramWith ::
  forall latB depth nBytes a domA domB.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
  , KnownNat nBytes, 1 <= nBytes
  , KnownDomain domA
  , KnownDomain domB
  , BitPack a, 1 <= BitSize a
  , NFDataX a
  ) =>
  XpmMemoryConfig ->
  -- | Configuration of read port B
  XpmMemoryPortConfig latB a ->

  Clock domA ->
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
  -- | Synchronously or asynchronously (depending on the domain) resets port B's
  -- output register to its reset value
  Reset domB ->
  -- | Port B enable
  Enable domB ->
  -- | Enable of port B's last output register. Ignored for read latencies
  -- smaller than /2/.
  "regceb" ::: Signal domB Bool ->
  "addrb" ::: Signal domB (Index depth) ->

  "doutb" ::: Signal domB a
xpmMemorySdpramWith XpmMemoryConfig{..} portB
  clkA enA addrA weA dinA
  clkB rstB enB regceB addrB
  | Just msg <- err = clashCompileError msg
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Port A uses the same write mode as port B
  portA :: XpmMemoryPortConfig 1 a
  portA = XpmMemoryPortConfig d1 (writeMode portB) (unpack 0)

  err =
    configError
      "xpmMemorySdpramWith" SimpleDualPortRam memoryPrimitive
      (isCommonClock @domA @domB) (natToNum @depth) (natToNum @(BitSize a))
      (portCheck @domA "A" False True (natToNum @nBytes) portA)
      (portCheck @domB "B" True False 1 portB)

  sim = unpack <$> outB
   where
    (_, outB) =
      xpmMemoryModel
        ModelConfig
          { mcMemoryType = SimpleDualPortRam
          , mcPrimitive = memoryPrimitive
          , mcInit = memoryInit
          , mcDepth = natToNum @depth
          , mcPortA = (portModelConfig @domA portA){pmAsyncReset = False}
          , mcPortB = portModelConfig @domB portB
          }
        clkA (portInputs noReset enA (pure False) addrA weA dinA)
        clkB (portInputs rstB enB regceB addrB (pure (0 :: BitVector 1)) (pure (unpack 0 :: a)))

  synth = unpack <$> unPort doutb
   where
    doutb :: Port "doutb" domB (BitVector (BitSize a))
    doutb =
      inst
        (instConfig "xpm_memory_sdpram")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"ADDR_WIDTH_A"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"ADDR_WIDTH_B"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"BYTE_WRITE_WIDTH_A"  @Integer byteWidth)
        (Param @"CLOCKING_MODE"       @String  (clockingModeParam @domA @domB))
        (Param @"ECC_MODE"            @String  "no_ecc")
        (Param @"MEMORY_INIT_FILE"    @String  (initFileParam memoryInit))
        (Param @"MEMORY_PRIMITIVE"    @String  (primitiveParam memoryPrimitive))
        (Param @"MEMORY_SIZE"         @Integer (natToNum @depth * width))
        (Param @"READ_DATA_WIDTH_B"   @Integer width)
        (Param @"READ_LATENCY_B"      @Integer (snatToInteger (readLatency portB)))
        (HexStringParam @"READ_RESET_VALUE_B" (pack (readResetValue portB)))
        (Param @"RST_MODE_A"          @String  "SYNC")
        (Param @"RST_MODE_B"          @String  (rstModeParam @domB))
        (Param @"WAKEUP_TIME"         @String  "disable_sleep")
        (Param @"WRITE_DATA_WIDTH_A"  @Integer width)
        (Param @"WRITE_MODE_B"        @String  (writeModeParam (writeMode portB)))

        (Port      @"sleep"           (pure 0 :: Signal domA Bit))

        (ClockPort @"clka"            clkA)
        (Port      @"ena"             (boolToBit <$> fromEnable enA))
        (Port      @"wea"             weA)
        (Port      @"addra"           (pack <$> addrA))
        (Port      @"dina"            (pack <$> dinA))
        (Port      @"injectsbiterra"  (pure 0 :: Signal domA Bit))
        (Port      @"injectdbiterra"  (pure 0 :: Signal domA Bit))

        (ClockPort @"clkb"            clkB)
        (ResetPort @"rstb" @ActiveHigh rstB)
        (Port      @"enb"             (boolToBit <$> fromEnable enB))
        (Port      @"regceb"          (boolToBit <$> regceB))
        (Port      @"addrb"           (pack <$> addrB))

  width = natToNum @(BitSize a) :: Integer
  byteWidth = width `div` natToNum @nBytes
{-# INLINE xpmMemorySdpramWith #-}
