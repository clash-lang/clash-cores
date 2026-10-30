{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Memory.Spram
  ( xpmMemorySpram
  , xpmMemorySpramWith
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Memory.Internal

-- | Single port RAM. Instantiates Xilinx's @XPM_MEMORY_SPRAM@, see
-- [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_SPRAM).
--
-- Uses 'defaultXpmMemoryConfig' and 'defaultXpmMemoryPortConfig': block RAM or
-- distributed RAM (chosen by Vivado), no initialization, a read latency of /2/,
-- 'NoChange' and a reset value of zero. The output register is always enabled
-- (@regcea@ is tied high). Use 'xpmMemorySpramWith' to change these settings.
--
-- See "Clash.Cores.Xilinx.Xpm.Memory" for the behavior of the port.
xpmMemorySpram ::
  forall depth nBytes a dom.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
  , KnownNat nBytes, 1 <= nBytes
  , KnownDomain dom
  , BitPack a, 1 <= BitSize a
  , NFDataX a
  ) =>
  Clock dom ->
  -- | Synchronously or asynchronously (depending on the domain) resets the
  -- output register to its reset value
  Reset dom ->
  Enable dom ->
  "addra" ::: Signal dom (Index depth) ->
  -- | Write enable, one bit per byte. Use @BitVector 1@ to write whole words,
  -- or a write enable of @n@ bits to write @BitSize a / n@ bit wide bytes,
  -- which must be /8/ or /9/ bits wide. The first bit enables the least
  -- significant byte.
  "wea" ::: Signal dom (BitVector nBytes) ->
  "dina" ::: Signal dom a ->
  "douta" ::: Signal dom a
xpmMemorySpram clk rst en addr we din =
  xpmMemorySpramWith
    defaultXpmMemoryConfig
    defaultXpmMemoryPortConfig
    clk rst en (pure True) addr we din
{-# INLINE xpmMemorySpram #-}

-- | Like 'xpmMemorySpram', but with a configurable memory and port, and an
-- explicit output register enable.
xpmMemorySpramWith ::
  forall lat depth nBytes a dom.
  ( HasCallStack
  , KnownNat depth, 2 <= depth
  , KnownNat nBytes, 1 <= nBytes
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
  -- | Write enable, one bit per byte. Use @BitVector 1@ to write whole words,
  -- or a write enable of @n@ bits to write @BitSize a / n@ bit wide bytes,
  -- which must be /8/ or /9/ bits wide. The first bit enables the least
  -- significant byte.
  "wea" ::: Signal dom (BitVector nBytes) ->
  "dina" ::: Signal dom a ->
  "douta" ::: Signal dom a
xpmMemorySpramWith XpmMemoryConfig{..} port clk rst en regce addr we din
  | Just msg <- err = clashCompileError msg
  | clashSimulation = sim
  | otherwise = synth
 where
  err =
    configError
      "xpmMemorySpramWith" SinglePortRam memoryPrimitive
      True (natToNum @depth) (natToNum @(BitSize a))
      (portCheck @dom "A" True True (natToNum @nBytes) port)
      (portCheck @dom "B" False False 1 port)

  sim = unpack <$> outA
   where
    (outA, _) =
      xpmMemoryModel
        ModelConfig
          { mcMemoryType = SinglePortRam
          , mcPrimitive = memoryPrimitive
          , mcInit = memoryInit
          , mcDepth = natToNum @depth
          , mcPortA = portModelConfig @dom port
          , mcPortB = portModelConfig @dom port
          }
        clk (portInputs rst en regce addr we din)
        clk (portInputs @dom @depth noReset disabled (pure False) (pure 0) (pure (0 :: BitVector 1)) (pure (unpack 0 :: a)))

  disabled = toEnable (pure False)

  synth = unpack <$> unPort douta
   where
    douta :: Port "douta" dom (BitVector (BitSize a))
    douta =
      inst
        (instConfig "xpm_memory_spram")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"ADDR_WIDTH_A"        @Integer (natToNum @(CLog 2 depth)))
        (Param @"BYTE_WRITE_WIDTH_A"  @Integer byteWidth)
        (Param @"ECC_MODE"            @String  "no_ecc")
        (Param @"MEMORY_INIT_FILE"    @String  (initFileParam memoryInit))
        (Param @"MEMORY_PRIMITIVE"    @String  (primitiveParam memoryPrimitive))
        (Param @"MEMORY_SIZE"         @Integer (natToNum @depth * width))
        (Param @"READ_DATA_WIDTH_A"   @Integer width)
        (Param @"READ_LATENCY_A"      @Integer (snatToInteger (readLatency port)))
        (HexStringParam @"READ_RESET_VALUE_A" (pack (readResetValue port)))
        (Param @"RST_MODE_A"          @String  (rstModeParam @dom))
        (Param @"WAKEUP_TIME"         @String  "disable_sleep")
        (Param @"WRITE_DATA_WIDTH_A"  @Integer width)
        (Param @"WRITE_MODE_A"        @String  (writeModeParam (writeMode port)))

        (Port      @"sleep"           (pure 0 :: Signal dom Bit))
        (ClockPort @"clka"            clk)
        (ResetPort @"rsta" @ActiveHigh rst)
        (Port      @"ena"             (boolToBit <$> fromEnable en))
        (Port      @"regcea"          (boolToBit <$> regce))
        (Port      @"wea"             we)
        (Port      @"addra"           (pack <$> addr))
        (Port      @"dina"            (pack <$> din))
        (Port      @"injectsbiterra"  (pure 0 :: Signal dom Bit))
        (Port      @"injectdbiterra"  (pure 0 :: Signal dom Bit))

  width = natToNum @(BitSize a) :: Integer
  byteWidth = width `div` natToNum @nBytes
{-# INLINE xpmMemorySpramWith #-}
