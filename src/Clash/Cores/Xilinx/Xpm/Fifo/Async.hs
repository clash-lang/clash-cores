{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Wrapper for Xilinx's @xpm_fifo_async@ macro, a FIFO with independent write
  and read clocks. See [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_FIFO_ASYNC).
-}

{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Async
  ( xpmFifoAsync
  , xpmFifoAsyncWith
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

-- | An asynchronous FIFO, holding @2^depth@ words of type @a@, using the
-- defaults of the XPM (see 'defXpmFifoConfig'). Use 'xpmFifoAsyncWith' to
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
-- @2^depth - 1@ words. In FWFT mode, two more words fit in its output stages.
--
-- __Latency__: the write and read pointers cross clock domains through
-- Gray-coded synchronizers with 'cdcSyncStages' stages (or a single register
-- if 'relatedClocks' is set). It therefore takes a number of read clock cycles
-- before a written word makes the FIFO non-empty, and a number of write clock
-- cycles before a read makes the FIFO non-full. The flags are conservative:
-- the FIFO never reports more words available for reading, or more space
-- available for writing, than there are.
--
-- __Reset__: the reset is synchronous to the write clock. Apart from the reset
-- input, the FIFO resets itself after configuration (i.e., at the start of a
-- simulation). The reset sequence crosses to the read domain and back, during
-- which 'writeResetBusy' and 'readResetBusy' are asserted. Do not write while
-- 'writeResetBusy' is asserted, and do not read while 'readResetBusy' is
-- asserted. Do not assert the reset again while 'writeResetBusy' is asserted.
--
-- __Optional outputs__: outputs that need a feature in 'advFeatures' are
-- 'XException' in simulation if it is not enabled. Vivado keeps the logic of
-- enabled features in @xpm_fifo_async@ even if their outputs are not used, so
-- only enable the features you need.
--
-- The Haskell simulation model is cycle accurate with respect to the XPM, in
-- the sense that it models every register in the XPM, including the clock
-- domain crossings. This assumes that a value crossing clock domains is
-- sampled correctly by the first synchronizer register, i.e., it does not model
-- metastability.
xpmFifoAsync ::
  forall depth write read a .
  ( KnownNat depth, 4 <= depth, depth <= 22
  , KnownDomain write
  , KnownDomain read
  , BitPack a, NFDataX a
  , HasCallStack
  ) =>
  -- | @wr_clk@
  Clock write ->
  -- | @rst@, synchronous to the write clock
  Reset write ->
  -- | @rd_clk@
  Clock read ->
  -- | Write request: @wr_en@ and @din@
  Signal write (Maybe a) ->
  -- | Read request: @rd_en@
  Signal read Bool ->
  XpmFifoOut write read depth a
xpmFifoAsync = xpmFifoAsyncWith defXpmFifoConfig
{-# INLINE xpmFifoAsync #-}

-- | Like 'xpmFifoAsync', but with a configurable 'XpmFifoConfig'.
--
-- Not supported: ECC (@ECC_MODE@), sleep (@WAKEUP_TIME@, @sleep@ is tied
-- low), different read and write widths, read latency 0 and data count outputs
-- narrower than the full count. @DOUT_RESET_VALUE@ is fixed to all zeroes,
-- @CASCADE_HEIGHT@ to 0 (automatic) and @SIM_ASSERT_CHK@ to 0.
xpmFifoAsyncWith ::
  forall depth write read a .
  ( KnownNat depth, 4 <= depth, depth <= 22
  , KnownDomain write
  , KnownDomain read
  , BitPack a, NFDataX a
  , HasCallStack
  ) =>
  XpmFifoConfig depth ->
  -- | @wr_clk@
  Clock write ->
  -- | @rst@, synchronous to the write clock
  Reset write ->
  -- | @rd_clk@
  Clock read ->
  -- | Write request: @wr_en@ and @din@
  Signal write (Maybe a) ->
  -- | Read request: @rd_en@
  Signal read Bool ->
  XpmFifoOut write read depth a
xpmFifoAsyncWith cfg@XpmFifoConfig{..} clkWr rst clkRd wrData rdEn
  | clashSimulation = sim
  | otherwise = synth
 where
  sim =
    assertXpmFifoConfig "xpmFifoAsync" True (natToNum @(BitSize a)) cfg $
      maskDisabled advFeatures $
        xpmFifoAsyncModel (toModelConfig cfg) clkWr clkRd
          (unsafeToActiveHigh rst) wrData rdEn

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
    ( Port "full" write Bit
    , Port "prog_full" write Bit
    , Port "wr_data_count" write (BitVector (depth + 1))
    , Port "overflow" write Bit
    , Port "wr_rst_busy" write Bit
    , Port "almost_full" write Bit
    , Port "wr_ack" write Bit
    , Port "dout" read (BitVector (BitSize a))
    , Port "empty" read Bit
    , Port "prog_empty" read Bit
    , Port "rd_data_count" read (BitVector (depth + 1))
    , Port "underflow" read Bit
    , Port "rd_rst_busy" read Bit
    , Port "almost_empty" read Bit
    , Port "data_valid" read Bit
    )
  go =
    inst
      (instConfig "xpm_fifo_async")
        { library = Just "xpm"
        , libraryImport = Just "xpm.vcomponents.all" }

      (Param @"CASCADE_HEIGHT"      @Integer 0)
      (Param @"CDC_SYNC_STAGES"     @Integer (toInteger cdcSyncStages))
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
      (Param @"RELATED_CLOCKS"      @Integer (boolParam relatedClocks))
      (Param @"SIM_ASSERT_CHK"      @Integer 0)
      (Param @"USE_ADV_FEATURES"    @String  (advFeaturesParam advFeatures))
      (Param @"WAKEUP_TIME"         @Integer 0)
      (Param @"WRITE_DATA_WIDTH"    @Integer (natToNum @(BitSize a)))
      (Param @"WR_DATA_COUNT_WIDTH" @Integer (natToNum @(depth + 1)))

      (Port      @"sleep"         (pure (0 :: Bit) :: Signal write Bit))
      (ResetPort @"rst" @'ActiveHigh rst)
      (ClockPort @"wr_clk"        clkWr)
      (Port      @"wr_en"         (boolToBit . isJust <$> wrData))
      (Port      @"din"           (pack . fromJustX <$> wrData))
      (ClockPort @"rd_clk"        clkRd)
      (Port      @"rd_en"         (boolToBit <$> rdEn))
      (Port      @"injectsbiterr" (pure (0 :: Bit) :: Signal write Bit))
      (Port      @"injectdbiterr" (pure (0 :: Bit) :: Signal write Bit))
{-# INLINE xpmFifoAsyncWith #-}
