{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Memory.Tdpram
  ( xpmMemoryTdpram
  , xpmMemoryTdpramWith
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Memory.Internal

-- | True dual port RAM: two ports that can both read and write. Instantiates
-- Xilinx's @XPM_MEMORY_TDPRAM@, see
-- [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_TDPRAM).
--
-- Uses 'defaultXpmMemoryConfig' and 'defaultXpmMemoryPortConfig' for both ports:
-- block RAM or distributed RAM (chosen by Vivado), no initialization, a read
-- latency of /2/, 'NoChange' and a reset value of zero. The output registers
-- are always enabled (@regce@ is tied high). Use 'xpmMemoryTdpramWith' to
-- change these settings.
--
-- See "Clash.Cores.Xilinx.Xpm.Memory" for the behavior of the ports, including
-- collisions.
xpmMemoryTdpram ::
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
  -- | Synchronously or asynchronously (depending on the domain) resets port A's
  -- output register to its reset value
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
  "web" ::: Signal domB (BitVector nBytes) ->
  "dinb" ::: Signal domB a ->

  ( "douta" ::: Signal domA a
  , "doutb" ::: Signal domB a
  )
xpmMemoryTdpram clkA rstA enA addrA weA dinA clkB rstB enB addrB weB dinB =
  xpmMemoryTdpramWith
    defaultXpmMemoryConfig
    defaultXpmMemoryPortConfig
    defaultXpmMemoryPortConfig
    clkA rstA enA (pure True) addrA weA dinA
    clkB rstB enB (pure True) addrB weB dinB
{-# INLINE xpmMemoryTdpram #-}

-- | Like 'xpmMemoryTdpram', but with a configurable memory and ports, and
-- explicit output register enables.
xpmMemoryTdpramWith ::
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
  -- | Synchronously or asynchronously (depending on the domain) resets port A's
  -- output register to its reset value
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
  "web" ::: Signal domB (BitVector nBytes) ->
  "dinb" ::: Signal domB a ->

  ( "douta" ::: Signal domA a
  , "doutb" ::: Signal domB a
  )
xpmMemoryTdpramWith XpmMemoryConfig{..} portA portB
  clkA rstA enA regceA addrA weA dinA
  clkB rstB enB regceB addrB weB dinB
  | Just msg <- err = clashCompileError msg
  | clashSimulation = sim
  | otherwise = synth
 where
  err =
    configError
      "xpmMemoryTdpramWith" TrueDualPortRam memoryPrimitive
      (isCommonClock @domA @domB) (natToNum @depth) (natToNum @(BitSize a))
      (portCheck @domA "A" True True (natToNum @nBytes) portA)
      (portCheck @domB "B" True True (natToNum @nBytes) portB)

  sim = (unpack <$> outA, unpack <$> outB)
   where
    (outA, outB) =
      xpmMemoryModel
        ModelConfig
          { mcMemoryType = TrueDualPortRam
          , mcPrimitive = memoryPrimitive
          , mcInit = memoryInit
          , mcDepth = natToNum @depth
          , mcPortA = portModelConfig @domA portA
          , mcPortB = portModelConfig @domB portB
          }
        clkA (portInputs rstA enA regceA addrA weA dinA)
        clkB (portInputs rstB enB regceB addrB weB dinB)

  synth = (unpack <$> unPort douta, unpack <$> unPort doutb)
   where
    (douta, doutb) = go
    go ::
      ( Port "douta" domA (BitVector (BitSize a))
      , Port "doutb" domB (BitVector (BitSize a))
      )
    go =
      inst
        (instConfig "xpm_memory_tdpram")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"ADDR_WIDTH_A"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"ADDR_WIDTH_B"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"BYTE_WRITE_WIDTH_A"  @Integer byteWidth)
        (Param @"BYTE_WRITE_WIDTH_B"  @Integer byteWidth)
        (Param @"CLOCKING_MODE"       @String  (clockingModeParam @domA @domB))
        (Param @"ECC_MODE"            @String  "no_ecc")
        (Param @"MEMORY_INIT_FILE"    @String  (initFileParam memoryInit))
        (Param @"MEMORY_PRIMITIVE"    @String  (primitiveParam memoryPrimitive))
        (Param @"MEMORY_SIZE"         @Integer (natToNum @depth * width))
        (Param @"READ_DATA_WIDTH_A"   @Integer width)
        (Param @"READ_DATA_WIDTH_B"   @Integer width)
        (Param @"READ_LATENCY_A"      @Integer (snatToInteger (readLatency portA)))
        (Param @"READ_LATENCY_B"      @Integer (snatToInteger (readLatency portB)))
        (HexStringParam @"READ_RESET_VALUE_A" (pack (readResetValue portA)))
        (HexStringParam @"READ_RESET_VALUE_B" (pack (readResetValue portB)))
        (Param @"RST_MODE_A"          @String  (rstModeParam @domA))
        (Param @"RST_MODE_B"          @String  (rstModeParam @domB))
        (Param @"WAKEUP_TIME"         @String  "disable_sleep")
        (Param @"WRITE_DATA_WIDTH_A"  @Integer width)
        (Param @"WRITE_DATA_WIDTH_B"  @Integer width)
        (Param @"WRITE_MODE_A"        @String  (writeModeParam (writeMode portA)))
        (Param @"WRITE_MODE_B"        @String  (writeModeParam (writeMode portB)))

        (Port      @"sleep"           (pure 0 :: Signal domA Bit))

        (ClockPort @"clka"            clkA)
        (ResetPort @"rsta" @ActiveHigh rstA)
        (Port      @"ena"             (boolToBit <$> fromEnable enA))
        (Port      @"regcea"          (boolToBit <$> regceA))
        (Port      @"wea"             weA)
        (Port      @"addra"           (pack <$> addrA))
        (Port      @"dina"            (pack <$> dinA))
        (Port      @"injectsbiterra"  (pure 0 :: Signal domA Bit))
        (Port      @"injectdbiterra"  (pure 0 :: Signal domA Bit))

        (ClockPort @"clkb"            clkB)
        (ResetPort @"rstb" @ActiveHigh rstB)
        (Port      @"enb"             (boolToBit <$> fromEnable enB))
        (Port      @"regceb"          (boolToBit <$> regceB))
        (Port      @"web"             weB)
        (Port      @"addrb"           (pack <$> addrB))
        (Port      @"dinb"            (pack <$> dinB))
        (Port      @"injectsbiterrb"  (pure 0 :: Signal domB Bit))
        (Port      @"injectdbiterrb"  (pure 0 :: Signal domB Bit))

  width = natToNum @(BitSize a) :: Integer
  byteWidth = width `div` natToNum @nBytes
{-# INLINE xpmMemoryTdpramWith #-}
