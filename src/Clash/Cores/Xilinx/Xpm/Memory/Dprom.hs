{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Memory.Dprom
  ( xpmMemoryDprom
  , xpmMemoryDpromWith
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Memory.Internal

-- | Dual port ROM. Instantiates Xilinx's @XPM_MEMORY_DPROM@, see
-- [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_DPROM).
--
-- Uses 'defaultXpmMemoryPortConfig' for both ports: a read latency of /2/ and a
-- reset value of zero. Vivado chooses between block RAM and distributed RAM.
-- The output registers are always enabled (@regcea@ and @regceb@ are tied
-- high). Use 'xpmMemoryDpromWith' to change these settings.
--
-- See "Clash.Cores.Xilinx.Xpm.Memory" for the behavior of the ports.
xpmMemoryDprom ::
  forall depth a domA domB.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
  , KnownDomain domA
  , KnownDomain domB
  , BitPack a, 1 <= BitSize a
  , NFDataX a
  ) =>
  -- | Contents of the ROM
  MemoryInit ->
  Clock domA ->
  -- | Synchronously or asynchronously (depending on the domain) resets port A's
  -- output register to its reset value
  Reset domA ->
  -- | Port A enable
  Enable domA ->
  "addra" ::: Signal domA (Index depth) ->
  Clock domB ->
  Reset domB ->
  Enable domB ->
  "addrb" ::: Signal domB (Index depth) ->
  ( "douta" ::: Signal domA a
  , "doutb" ::: Signal domB a
  )
xpmMemoryDprom contents clkA rstA enA addrA clkB rstB enB addrB =
  xpmMemoryDpromWith
    defaultXpmMemoryConfig{memoryInit = contents}
    defaultXpmMemoryPortConfig
    defaultXpmMemoryPortConfig
    clkA rstA enA (pure True) addrA
    clkB rstB enB (pure True) addrB
{-# INLINE xpmMemoryDprom #-}

-- | Like 'xpmMemoryDprom', but with a configurable memory and ports, and
-- explicit output register enables. The write modes of the port configurations
-- are ignored.
xpmMemoryDpromWith ::
  forall latA latB depth a domA domB.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
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
  Clock domB ->
  Reset domB ->
  Enable domB ->
  "regceb" ::: Signal domB Bool ->
  "addrb" ::: Signal domB (Index depth) ->
  ( "douta" ::: Signal domA a
  , "doutb" ::: Signal domB a
  )
xpmMemoryDpromWith XpmMemoryConfig{..} portA0 portB0
  clkA rstA enA regceA addrA
  clkB rstB enB regceB addrB
  | Just msg <- err = clashCompileError msg
  | clashSimulation = sim
  | otherwise = synth
 where
  portA = portA0{writeMode = ReadFirst}
  portB = portB0{writeMode = ReadFirst}

  err =
    configError
      "xpmMemoryDpromWith" DualPortRom memoryPrimitive
      (isCommonClock @domA @domB) (natToNum @depth) (natToNum @(BitSize a))
      (portCheck @domA "A" True False 1 portA)
      (portCheck @domB "B" True False 1 portB)

  sim = (unpack <$> outA, unpack <$> outB)
   where
    (outA, outB) =
      xpmMemoryModel
        ModelConfig
          { mcMemoryType = DualPortRom
          , mcPrimitive = memoryPrimitive
          , mcInit = memoryInit
          , mcDepth = natToNum @depth
          , mcPortA = portModelConfig @domA portA
          , mcPortB = portModelConfig @domB portB
          }
        clkA (romInputs rstA enA regceA addrA)
        clkB (romInputs rstB enB regceB addrB)

  romInputs ::
    forall d.
    KnownDomain d =>
    Reset d -> Enable d -> Signal d Bool -> Signal d (Index depth) -> Signal d (PortIn (BitSize a))
  romInputs r e c ad = portInputs r e c ad (pure (0 :: BitVector 1)) (pure (unpack 0 :: a))

  synth = (unpack <$> unPort douta, unpack <$> unPort doutb)
   where
    (douta, doutb) = go
    go ::
      ( Port "douta" domA (BitVector (BitSize a))
      , Port "doutb" domB (BitVector (BitSize a))
      )
    go =
      inst
        (instConfig "xpm_memory_dprom")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"ADDR_WIDTH_A"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"ADDR_WIDTH_B"        @Integer (natToNum @(CLog 2 depth)))
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

        (Port      @"sleep"           (pure 0 :: Signal domA Bit))

        (ClockPort @"clka"            clkA)
        (ResetPort @"rsta" @ActiveHigh rstA)
        (Port      @"ena"             (boolToBit <$> fromEnable enA))
        (Port      @"regcea"          (boolToBit <$> regceA))
        (Port      @"addra"           (pack <$> addrA))
        (Port      @"injectsbiterra"  (pure 0 :: Signal domA Bit))
        (Port      @"injectdbiterra"  (pure 0 :: Signal domA Bit))

        (ClockPort @"clkb"            clkB)
        (ResetPort @"rstb" @ActiveHigh rstB)
        (Port      @"enb"             (boolToBit <$> fromEnable enB))
        (Port      @"regceb"          (boolToBit <$> regceB))
        (Port      @"addrb"           (pack <$> addrB))
        (Port      @"injectsbiterrb"  (pure 0 :: Signal domB Bit))
        (Port      @"injectdbiterrb"  (pure 0 :: Signal domB Bit))

  width = natToNum @(BitSize a) :: Integer
{-# INLINE xpmMemoryDpromWith #-}
