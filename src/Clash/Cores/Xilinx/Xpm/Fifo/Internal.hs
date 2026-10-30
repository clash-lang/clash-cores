{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Parameter types shared by the XPM FIFO wrappers (@xpm_fifo_sync@,
  @xpm_fifo_async@, @xpm_fifo_axis@, @xpm_fifo_axif@ and @xpm_fifo_axil@). Each
  type documents the XPM parameter it maps to. For more information see
  [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_FIFO_SYNC).
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Internal
  ( -- * Memory type
    FifoMemoryType(..)
  , fifoMemoryTypeParam

    -- * ECC
  , EccMode(..)
  , eccModeParam

    -- * Read mode
  , ReadMode(..)
  , readModeParam
  , readLatencyParam
  , isFwft

    -- * Advanced features
  , AdvFeatures(..)
  , defAdvFeatures
  , noAdvFeatures
  , allAdvFeatures
  , advFeaturesParam

    -- * Programmable full\/empty thresholds
  , progFullThreshRange
  , progEmptyThreshRange

    -- * Clock domain crossing
  , cdcSyncStagesRange

    -- * Helpers
  , boolParam
  ) where

import Clash.Prelude

import qualified Prelude as P

import Clash.Cores.Xilinx.Xpm.Fifo.Internal.TH (hexStringTree)

-- | Memory primitive the FIFO is mapped to, parameter @FIFO_MEMORY_TYPE@.
data FifoMemoryType
  = -- | Let Vivado choose, @\"auto\"@
    Auto
  | -- | Block RAM, @\"block\"@
    Block
  | -- | Distributed (LUT) RAM, @\"distributed\"@
    Distributed
  | -- | UltraRAM, @\"ultra\"@. Only supported by synchronous (common clock)
    -- FIFOs.
    Ultra
  deriving (Show, Eq, Generic)

-- | Value for the XPM parameter @FIFO_MEMORY_TYPE@
fifoMemoryTypeParam :: FifoMemoryType -> String
fifoMemoryTypeParam = \case
  Auto -> "auto"
  Block -> "block"
  Distributed -> "distributed"
  Ultra -> "ultra"

-- | Error correction, parameter @ECC_MODE@.
data EccMode
  = -- | No error correction, @\"no_ecc\"@
    NoEcc
  | -- | Hamming code based single bit error correction, double bit error
    -- detection, @\"en_ecc\"@
    EnEcc
  deriving (Show, Eq, Generic)

-- | Value for the XPM parameter @ECC_MODE@
eccModeParam :: EccMode -> String
eccModeParam = \case
  NoEcc -> "no_ecc"
  EnEcc -> "en_ecc"

-- | Read mode, parameters @READ_MODE@ and @FIFO_READ_LATENCY@.
data ReadMode
  = -- | Standard read mode (@READ_MODE = \"std\"@). Data appears on the output
    -- the given number of read clock cycles after the read request
    -- (@FIFO_READ_LATENCY@). The latency must be at least /1/.
    Std Natural
  | -- | First word fall through (@READ_MODE = \"fwft\"@). The first word
    -- written to the FIFO automatically appears on the output, without
    -- requesting a read. A read request acknowledges the current word and
    -- makes the next one appear.
    Fwft
  deriving (Show, Eq, Generic)

-- | Value for the XPM parameter @READ_MODE@
readModeParam :: ReadMode -> String
readModeParam = \case
  Std _ -> "std"
  Fwft -> "fwft"

-- | Value for the XPM parameter @FIFO_READ_LATENCY@. UG974 documents /0/ as
-- the only applicable value in FWFT mode.
readLatencyParam :: ReadMode -> Integer
readLatencyParam = \case
  Std n -> toInteger n
  Fwft -> 0

-- | Whether a 'ReadMode' is 'Fwft'
isFwft :: ReadMode -> Bool
isFwft = \case
  Std _ -> False
  Fwft -> True

-- | Optional outputs, parameter @USE_ADV_FEATURES@. Each field enables one
-- output port of the XPM. Disabled outputs are tied to a constant by the XPM.
data AdvFeatures = AdvFeatures
  { enOverflow :: Bool
  -- ^ Enable @overflow@, bit 0
  , enProgFull :: Bool
  -- ^ Enable @prog_full@, bit 1
  , enWrDataCount :: Bool
  -- ^ Enable @wr_data_count@, bit 2
  , enAlmostFull :: Bool
  -- ^ Enable @almost_full@, bit 3
  , enWrAck :: Bool
  -- ^ Enable @wr_ack@, bit 4
  , enUnderflow :: Bool
  -- ^ Enable @underflow@, bit 8
  , enProgEmpty :: Bool
  -- ^ Enable @prog_empty@, bit 9
  , enRdDataCount :: Bool
  -- ^ Enable @rd_data_count@, bit 10
  , enAlmostEmpty :: Bool
  -- ^ Enable @almost_empty@, bit 11
  , enDataValid :: Bool
  -- ^ Enable @data_valid@, bit 12
  }
  deriving (Show, Eq, Generic)

-- | The XPM's default value of @USE_ADV_FEATURES@ for @xpm_fifo_sync@ and
-- @xpm_fifo_async@, @\"0707\"@: overflow, underflow, programmable full\/empty
-- flags and data counts.
defAdvFeatures :: AdvFeatures
defAdvFeatures = AdvFeatures
  { enOverflow = True
  , enProgFull = True
  , enWrDataCount = True
  , enAlmostFull = False
  , enWrAck = False
  , enUnderflow = True
  , enProgEmpty = True
  , enRdDataCount = True
  , enAlmostEmpty = False
  , enDataValid = False
  }

-- | All optional outputs disabled, @\"0000\"@
noAdvFeatures :: AdvFeatures
noAdvFeatures = AdvFeatures
  { enOverflow = False
  , enProgFull = False
  , enWrDataCount = False
  , enAlmostFull = False
  , enWrAck = False
  , enUnderflow = False
  , enProgEmpty = False
  , enRdDataCount = False
  , enAlmostEmpty = False
  , enDataValid = False
  }

-- | All optional outputs enabled, @\"1F1F\"@
allAdvFeatures :: AdvFeatures
allAdvFeatures = AdvFeatures
  { enOverflow = True
  , enProgFull = True
  , enWrDataCount = True
  , enAlmostFull = True
  , enWrAck = True
  , enUnderflow = True
  , enProgEmpty = True
  , enRdDataCount = True
  , enAlmostEmpty = True
  , enDataValid = True
  }

-- | Value for the XPM parameter @USE_ADV_FEATURES@: four hexadecimal digits.
--
-- >>> advFeaturesParam defAdvFeatures
-- "0707"
-- >>> advFeaturesParam allAdvFeatures
-- "1f1f"
advFeaturesParam :: AdvFeatures -> String
advFeaturesParam AdvFeatures{..} =
  -- Clash can only render string literals as parameters, so instead of computing
  -- the string we select it from a tree of literals. See 'hexStringTree'.
  $(hexStringTree 4 [0, 1, 2, 3, 4, 8, 9, 10, 11, 12])
    enOverflow enProgFull enWrDataCount enAlmostFull enWrAck
    enUnderflow enProgEmpty enRdDataCount enAlmostEmpty enDataValid

-- | Legal range of @PROG_FULL_THRESH@, as checked by the XPM, for a FIFO with
-- equal read and write widths.
progFullThreshRange ::
  -- | @FIFO_WRITE_DEPTH@
  Integer ->
  -- | Whether the FIFO uses first word fall through
  Bool ->
  -- | 'Nothing' for common clock FIFOs, the number of CDC synchronization
  -- stages for independent clock FIFOs.
  Maybe Integer ->
  -- | Inclusive (minimum, maximum)
  (Integer, Integer)
progFullThreshRange depth fwft cdcStages =
  ( 3 + fwftWords + P.maybe 0 id cdcStages
  , depth - 3 - fwftWords )
 where
  fwftWords = if fwft then 2 else 0

-- | Legal range of @PROG_EMPTY_THRESH@, as checked by the XPM, for a FIFO with
-- equal read and write widths.
progEmptyThreshRange ::
  -- | @FIFO_WRITE_DEPTH@
  Integer ->
  -- | Whether the FIFO uses first word fall through
  Bool ->
  -- | Inclusive (minimum, maximum)
  (Integer, Integer)
progEmptyThreshRange depth fwft = (3 + fwftWords, depth - 3 - fwftWords)
 where
  fwftWords = if fwft then 2 else 0

-- | Legal range of @CDC_SYNC_STAGES@ for a given @FIFO_WRITE_DEPTH@ and
-- @RELATED_CLOCKS@ setting.
cdcSyncStagesRange ::
  -- | @FIFO_WRITE_DEPTH@
  Integer ->
  -- | @RELATED_CLOCKS@
  Bool ->
  -- | Inclusive (minimum, maximum)
  (Integer, Integer)
cdcSyncStagesRange depth relatedClocks
  | relatedClocks = (2, 2)
  | depth == 16 = (2, 4)
  | otherwise = (2, 8)

-- | Render a 'Bool' as an integer XPM parameter
boolParam :: Bool -> Integer
boolParam b = if b then 1 else 0
