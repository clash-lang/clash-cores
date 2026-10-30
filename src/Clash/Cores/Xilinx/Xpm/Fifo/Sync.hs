{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Wrapper for Xilinx's @xpm_fifo_sync@ macro, a FIFO with a single (common)
  clock. See [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_FIFO_SYNC).
-}

{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Sync
  ( xpmFifoSync
  , xpmFifoSyncWith
  , XpmFifoConfig(..)
  , defXpmFifoConfig
  , XpmFifoOut(..)
  ) where

import Clash.Explicit.Prelude

import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Fifo.Internal
import Clash.Cores.Xilinx.Xpm.Fifo.Internal.Base

-- | A synchronous FIFO, holding @2^depth@ words of type @a@, using the
-- defaults of the XPM (see 'defXpmFifoConfig'). Use 'xpmFifoSyncWith' to
-- change them.
--
-- __Writing__: a write request (@'Just' a@) is accepted when the FIFO is not
-- 'isFull' and not 'writeResetBusy'. Requests that are not accepted are
-- ignored, which 'isOverflow' reports in the next cycle.
--
-- __Reading__: a read request ('True') is accepted when the FIFO is not
-- 'isEmpty'. In standard read mode, the data appears on 'fifoData' the read
-- latency after an accepted read. In first word fall through mode, 'fifoData'
-- holds the word at the head of the FIFO while the FIFO is not 'isEmpty', and
-- a read removes it.
--
-- __Capacity__: in standard read mode, 'isFull' asserts when the FIFO holds
-- @2^depth@ words. In FWFT mode, two more words fit in its output stages.
--
-- __Reset__: the reset is synchronous to the clock. Apart from the reset
-- input, the FIFO resets itself after configuration (i.e., at the start of a
-- simulation). 'writeResetBusy' and 'readResetBusy' (which are equal for this
-- FIFO) assert in the cycle after the reset asserts, and deassert four cycles
-- after it deasserts. Writes are ignored while 'writeResetBusy' is asserted.
--
-- __Optional outputs__: outputs that need a feature in 'advFeatures' are
-- 'XException' in simulation if it is not enabled. Note that unlike
-- @xpm_fifo_async@, Vivado removes unused optional outputs of
-- @xpm_fifo_sync@ during synthesis, so enabling them all ('allAdvFeatures')
-- costs no resources if they are not used.
--
-- The Haskell simulation model is cycle accurate with respect to the XPM.
xpmFifoSync ::
  forall depth dom a .
  ( KnownNat depth, 4 <= depth, depth <= 22
  , KnownDomain dom
  , BitPack a, NFDataX a
  , HasCallStack
  ) =>
  -- | @wr_clk@
  Clock dom ->
  -- | @rst@, synchronous to the clock
  Reset dom ->
  -- | Write request: @wr_en@ and @din@
  Signal dom (Maybe a) ->
  -- | Read request: @rd_en@
  Signal dom Bool ->
  XpmFifoOut dom dom depth a
xpmFifoSync = xpmFifoSyncWith defXpmFifoConfig
{-# INLINE xpmFifoSync #-}

-- | Like 'xpmFifoSync', but with a configurable 'XpmFifoConfig'. The settings
-- 'cdcSyncStages' and 'relatedClocks' are ignored.
--
-- Not supported: ECC (@ECC_MODE@), sleep (@WAKEUP_TIME@, @sleep@ is tied
-- low), different read and write widths, read latency 0 and data count outputs
-- narrower than the full count. @DOUT_RESET_VALUE@ is fixed to all zeroes,
-- @CASCADE_HEIGHT@ to 0 (automatic) and @SIM_ASSERT_CHK@ to 0.
xpmFifoSyncWith ::
  forall depth dom a .
  ( KnownNat depth, 4 <= depth, depth <= 22
  , KnownDomain dom
  , BitPack a, NFDataX a
  , HasCallStack
  ) =>
  XpmFifoConfig depth ->
  -- | @wr_clk@
  Clock dom ->
  -- | @rst@, synchronous to the clock
  Reset dom ->
  -- | Write request: @wr_en@ and @din@
  Signal dom (Maybe a) ->
  -- | Read request: @rd_en@
  Signal dom Bool ->
  XpmFifoOut dom dom depth a
xpmFifoSyncWith cfg@XpmFifoConfig{..} clk rst wrData rdEn
  | clashSimulation = sim
  | otherwise = synth
 where
  sim =
    assertXpmFifoConfig "xpmFifoSync" False (natToNum @(BitSize a)) cfg $
      maskDisabled advFeatures $
        xpmFifoSyncModel (toModelConfig cfg) (unsafeToActiveHigh rst) wrData rdEn

  synth = XpmFifoOut
    { isFull = bitToBool <$> unPort full
    , isAlmostFull = bitToBool <$> unPort almostFull
    , isProgFull = bitToBool <$> unPort progFull
    , writeCount = unpack <$> unPort wrDataCount
    , isOverflow = bitToBool <$> unPort overflow
    , writeAck = bitToBool <$> unPort wrAck
    , writeResetBusy = bitToBool <$> unPort wrRstBusy
    , fifoData = unpack <$> unPort dout
    , isEmpty = bitToBool <$> unPort empty_
    , isAlmostEmpty = bitToBool <$> unPort almostEmpty
    , isProgEmpty = bitToBool <$> unPort progEmpty
    , readCount = unpack <$> unPort rdDataCount
    , isUnderflow = bitToBool <$> unPort underflow
    , isDataValid = bitToBool <$> unPort dataValid
    , readResetBusy = bitToBool <$> unPort rdRstBusy
    }

  ( full, progFull, wrDataCount, overflow, wrRstBusy, almostFull, wrAck
   , dout, empty_, progEmpty, rdDataCount, underflow, rdRstBusy, almostEmpty
   , dataValid ) = go

  go ::
    ( Port "full" dom Bit
    , Port "prog_full" dom Bit
    , Port "wr_data_count" dom (BitVector (depth + 1))
    , Port "overflow" dom Bit
    , Port "wr_rst_busy" dom Bit
    , Port "almost_full" dom Bit
    , Port "wr_ack" dom Bit
    , Port "dout" dom (BitVector (BitSize a))
    , Port "empty" dom Bit
    , Port "prog_empty" dom Bit
    , Port "rd_data_count" dom (BitVector (depth + 1))
    , Port "underflow" dom Bit
    , Port "rd_rst_busy" dom Bit
    , Port "almost_empty" dom Bit
    , Port "data_valid" dom Bit
    )
  go =
    inst
      (instConfig "xpm_fifo_sync")
        { library = Just "xpm"
        , libraryImport = Just "xpm.vcomponents.all" }

      (Param @"CASCADE_HEIGHT"      @Integer 0)
      (Param @"DOUT_RESET_VALUE"    @String  "0")
      (Param @"ECC_MODE"            @String  "no_ecc")
      (Param @"FIFO_MEMORY_TYPE"    @String  (fifoMemoryTypeParam fifoMemoryType))
      (Param @"FIFO_READ_LATENCY"   @Integer (readLatencyParam readMode))
      (Param @"FIFO_WRITE_DEPTH"    @Integer (natToNum @(2 ^ depth)))
      (Param @"FULL_RESET_VALUE"    @Integer (boolParam fullResetValue))
      (Param @"PROG_EMPTY_THRESH"   @Integer (toInteger progEmptyThresh))
      (Param @"PROG_FULL_THRESH"    @Integer (toInteger progFullThresh))
      (Param @"RD_DATA_COUNT_WIDTH" @Integer (natToNum @(depth + 1)))
      (Param @"READ_DATA_WIDTH"     @Integer (natToNum @(BitSize a)))
      (Param @"READ_MODE"           @String  (readModeParam readMode))
      (Param @"SIM_ASSERT_CHK"      @Integer 0)
      (Param @"USE_ADV_FEATURES"    @String  (advFeaturesParam advFeatures))
      (Param @"WAKEUP_TIME"         @Integer 0)
      (Param @"WRITE_DATA_WIDTH"    @Integer (natToNum @(BitSize a)))
      (Param @"WR_DATA_COUNT_WIDTH" @Integer (natToNum @(depth + 1)))

      (Port      @"sleep"         (pure (0 :: Bit) :: Signal dom Bit))
      (ResetPort @"rst" @'ActiveHigh rst)
      (ClockPort @"wr_clk"        clk)
      (Port      @"wr_en"         (boolToBit . isJust <$> wrData))
      (Port      @"din"           (pack . fromJustX <$> wrData))
      (Port      @"rd_en"         (boolToBit <$> rdEn))
      (Port      @"injectsbiterr" (pure (0 :: Bit) :: Signal dom Bit))
      (Port      @"injectdbiterr" (pure (0 :: Bit) :: Signal dom Bit))
{-# INLINE xpmFifoSyncWith #-}
