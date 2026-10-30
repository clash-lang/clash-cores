{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Memory.Sprom
  ( xpmMemorySprom
  , xpmMemorySpromWith
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Memory.Internal

-- | Single port ROM. Instantiates Xilinx's @XPM_MEMORY_SPROM@, see
-- [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_SPROM).
--
-- Uses 'defaultXpmMemoryPortConfig': a read latency of /2/ and a reset value of
-- zero. Vivado chooses between block RAM and distributed RAM. The output
-- register is always enabled (@regcea@ is tied high). Use 'xpmMemorySpromWith'
-- to change these settings.
--
-- See "Clash.Cores.Xilinx.Xpm.Memory" for the behavior of the port.
xpmMemorySprom ::
  forall depth a dom.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
  , KnownDomain dom
  , BitPack a, 1 <= BitSize a
  , NFDataX a
  ) =>
  -- | Contents of the ROM
  MemoryInit ->
  Clock dom ->
  -- | Synchronously or asynchronously (depending on the domain) resets the
  -- output register to its reset value
  Reset dom ->
  Enable dom ->
  "addra" ::: Signal dom (Index depth) ->
  "douta" ::: Signal dom a
xpmMemorySprom contents clk rst en addr =
  xpmMemorySpromWith
    defaultXpmMemoryConfig{memoryInit = contents}
    defaultXpmMemoryPortConfig
    clk rst en (pure True) addr
{-# INLINE xpmMemorySprom #-}

-- | Like 'xpmMemorySprom', but with a configurable memory and port, and an
-- explicit output register enable. The write mode of the port configuration
-- is ignored.
xpmMemorySpromWith ::
  forall lat depth a dom.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
  , KnownDomain dom
  , BitPack a, 1 <= BitSize a
  , NFDataX a
  ) =>
  XpmMemoryConfig ->
  XpmMemoryPortConfig lat a ->
  Clock dom ->
  -- | Synchronously or asynchronously (depending on the domain) resets the
  -- output register to its reset value
  Reset dom ->
  Enable dom ->
  -- | Enable of the last output register. Ignored for read latencies smaller
  -- than /2/.
  "regcea" ::: Signal dom Bool ->
  "addra" ::: Signal dom (Index depth) ->
  "douta" ::: Signal dom a
xpmMemorySpromWith XpmMemoryConfig{..} port0 clk rst en regce addr
  | Just msg <- err = clashCompileError msg
  | clashSimulation = sim
  | otherwise = synth
 where
  port = port0{writeMode = ReadFirst}

  err =
    configError
      "xpmMemorySpromWith" SinglePortRom memoryPrimitive
      True (natToNum @depth) (natToNum @(BitSize a))
      (portCheck @dom "A" True False 1 port)
      (portCheck @dom "B" False False 1 port)

  sim = unpack <$> outA
   where
    (outA, _) =
      xpmMemoryModel
        ModelConfig
          { mcMemoryType = SinglePortRom
          , mcPrimitive = memoryPrimitive
          , mcInit = memoryInit
          , mcDepth = natToNum @depth
          , mcPortA = portModelConfig @dom port
          , mcPortB = portModelConfig @dom port
          }
        clk (romInputs rst en regce addr)
        clk (romInputs @dom @depth noReset (toEnable (pure False)) (pure False) (pure 0))

  romInputs ::
    forall d n.
    (KnownDomain d, KnownNat n) =>
    Reset d -> Enable d -> Signal d Bool -> Signal d (Index n) -> Signal d (PortIn (BitSize a))
  romInputs r e c ad = portInputs r e c ad (pure (0 :: BitVector 1)) (pure (unpack 0 :: a))

  synth = unpack <$> unPort douta
   where
    douta :: Port "douta" dom (BitVector (BitSize a))
    douta =
      inst
        (instConfig "xpm_memory_sprom")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"ADDR_WIDTH_A"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"ECC_MODE"            @String  "no_ecc")
        (Param @"MEMORY_INIT_FILE"    @String  (initFileParam memoryInit))
        (Param @"MEMORY_PRIMITIVE"    @String  (primitiveParam memoryPrimitive))
        (Param @"MEMORY_SIZE"         @Integer (natToNum @depth * width))
        (Param @"READ_DATA_WIDTH_A"   @Integer width)
        (Param @"READ_LATENCY_A"      @Integer (snatToInteger (readLatency port)))
        (HexStringParam @"READ_RESET_VALUE_A" (pack (readResetValue port)))
        (Param @"RST_MODE_A"          @String  (rstModeParam @dom))
        (Param @"WAKEUP_TIME"         @String  "disable_sleep")

        (Port      @"sleep"           (pure 0 :: Signal dom Bit))
        (ClockPort @"clka"            clk)
        (ResetPort @"rsta" @ActiveHigh rst)
        (Port      @"ena"             (boolToBit <$> fromEnable en))
        (Port      @"regcea"          (boolToBit <$> regce))
        (Port      @"addra"           (pack <$> addr))
        (Port      @"injectsbiterra"  (pure 0 :: Signal dom Bit))
        (Port      @"injectdbiterra"  (pure 0 :: Signal dom Bit))

  width = natToNum @(BitSize a) :: Integer
{-# INLINE xpmMemorySpromWith #-}
