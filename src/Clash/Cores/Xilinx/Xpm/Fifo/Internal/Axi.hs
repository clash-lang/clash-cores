{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Types and simulation helpers shared by the wrappers of Xilinx's AXI FIFO
macros (@xpm_fifo_axis@, @xpm_fifo_axil@, and @xpm_fifo_axif@). These macros
are built from @xpm_fifo_base@, always in first-word-fall-through mode
(@READ_MODE = 1@), with equal read and write widths, @FULL_RESET_VALUE = 1@,
and without ECC or sleep. 'fifoBase' models it using the register-level
models of "Clash.Cores.Xilinx.Xpm.Fifo.Internal.Base"; the rest of the AXI
macros (reset synchronizers, packet mode logic, channel handshakes) is
modelled here and in the wrapper modules, register by register, after
@xpm_fifo.sv@ of Vivado 2022.1.

The models are only meant for simulation: the wrappers use them when
'clashSimulation' is 'True'.

What is not modelled:

  * The global set/reset (@glbl.GSR@) pulse with which Vivado's simulator starts
    every simulation. During the first 100 ns, it forces the registers of the
    synchronizers in @xpm_cdc_*@ to their initial values.
  * Metastability: a synchronizer passes on whatever value its input had at the
    sampling clock edge.
-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Internal.Axi where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))

import Data.Maybe (fromMaybe, isJust)

import qualified Prelude as P

import Clash.Cores.Xilinx.Xpm.Fifo.Internal
import Clash.Cores.Xilinx.Xpm.Fifo.Internal.TH (hexStringTree)
import qualified Clash.Cores.Xilinx.Xpm.Fifo.Internal.Base as Base

import Data.Proxy (Proxy(..))
import qualified GHC.TypeNats as TN

-- | Optional status outputs of a FIFO. See 'XpmFifoWriteStatus' and
-- 'XpmFifoReadStatus' for the outputs.
data XpmFifoStatusConfig = XpmFifoStatusConfig
  { statusProgFullThresh :: Maybe Natural
  -- ^ Enable the programmable full flag ('progFull', @USE_ADV_FEATURES[1]@),
  -- asserted when the FIFO holds at least this many words
  -- (@PROG_FULL_THRESH@). See 'progFullThreshRange' for the legal values; the
  -- AXI FIFOs use first word fall through.
  , statusProgEmptyThresh :: Maybe Natural
  -- ^ Enable the programmable empty flag ('progEmpty', @USE_ADV_FEATURES[9]@),
  -- asserted when the FIFO holds at most this many words
  -- (@PROG_EMPTY_THRESH@). See 'progEmptyThreshRange' for the legal values;
  -- the AXI FIFOs use first word fall through.
  , statusWriteDataCount :: Bool
  -- ^ Enable 'writeDataCount' (@USE_ADV_FEATURES[2]@)
  , statusReadDataCount :: Bool
  -- ^ Enable 'readDataCount' (@USE_ADV_FEATURES[10]@)
  }
  deriving (Show, Eq, Generic)

-- | All optional status outputs disabled
noStatus :: XpmFifoStatusConfig
noStatus = XpmFifoStatusConfig
  { statusProgFullThresh = Nothing
  , statusProgEmptyThresh = Nothing
  , statusWriteDataCount = False
  , statusReadDataCount = False
  }

-- | Status outputs of a FIFO in its write domain
data XpmFifoWriteStatus (depth :: Nat) = XpmFifoWriteStatus
  { progFull :: Bool
  -- ^ Programmable full flag (@prog_full@). Always 'False' if not enabled.
  , almostFull :: Bool
  -- ^ Asserted when only one more word can be written (@almost_full@).
  -- Always 'False' if not enabled. Only available for @XPM_FIFO_AXIS@.
  , writeDataCount :: Unsigned (CLog 2 depth + 1)
  -- ^ Number of words in the FIFO as seen from the write domain
  -- (@wr_data_count@). Always /0/ if not enabled.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Status outputs of a FIFO in its read domain
data XpmFifoReadStatus (depth :: Nat) = XpmFifoReadStatus
  { progEmpty :: Bool
  -- ^ Programmable empty flag (@prog_empty@). Always 'False' if not enabled.
  , almostEmpty :: Bool
  -- ^ Asserted when only one more word can be read (@almost_empty@). Always
  -- 'False' if not enabled. Only available for @XPM_FIFO_AXIS@.
  , readDataCount :: Unsigned (CLog 2 depth + 1)
  -- ^ Number of words in the FIFO as seen from the read domain
  -- (@rd_data_count@). Always /0/ if not enabled.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | AXI response (@BRESP@, @RRESP@)
data AxiResp
  = Okay
  -- ^ @OKAY@, normal access success
  | ExOkay
  -- ^ @EXOKAY@, exclusive access success
  | SlvErr
  -- ^ @SLVERR@, subordinate error
  | DecErr
  -- ^ @DECERR@, decode error
  deriving (Show, Eq, Generic, NFDataX, BitPack, ShowX)

-- | Value of @USE_ADV_FEATURES@ given a status configuration and whether to
-- enable the almost full (bit 3) and almost empty (bit 11) flags and @USE_ADV_FEATURES[13]@.
-- Bit 12 (data valid) is always set, like the AXI FIFOs do themselves.
statusAdvFeatures :: XpmFifoStatusConfig -> Bool -> Bool -> Bool -> String
statusAdvFeatures XpmFifoStatusConfig{..} af ae bit13 =
  $(hexStringTree 4 [1, 2, 3, 9, 10, 11, 12, 13])
    (isJust statusProgFullThresh)
    statusWriteDataCount
    af
    (isJust statusProgEmptyThresh)
    statusReadDataCount
    ae
    True
    bit13

-- | Check a status configuration. Returns an error message for illegal
-- configurations.
statusConfigError ::
  String ->
  -- | Depth
  Integer ->
  -- | 'Nothing' for common clock FIFOs, CDC stages otherwise
  Maybe Natural ->
  XpmFifoStatusConfig ->
  Maybe String
statusConfigError nm depth cdc XpmFifoStatusConfig{..}
  | Just t <- statusProgFullThresh, outside pf (toInteger t) =
      Just (nm <> ": statusProgFullThresh must be between " <> range pf)
  | Just t <- statusProgEmptyThresh, outside pe (toInteger t) =
      Just (nm <> ": statusProgEmptyThresh must be between " <> range pe)
  | otherwise = Nothing
 where
  pf = progFullThreshRange depth True (toInteger <$> cdc)
  pe = progEmptyThreshRange depth True

-- | Is a number outside of an (inclusive) range?
outside :: Ord a => (a, a) -> a -> Bool
outside (lo, hi) x = x < lo || x > hi

-- | Render a range for an error message
range :: Show a => (a, a) -> String
range (lo, hi) = show lo <> " and " <> show hi

-- | Check depth and CDC stages of a FIFO. Returns an error message for illegal
-- configurations.
depthCdcConfigError ::
  String ->
  -- | Depth
  Integer ->
  -- | 'Nothing' for common clock FIFOs, CDC stages otherwise
  Maybe Natural ->
  Maybe String
depthCdcConfigError nm depth cdc
  | depth < 16 || depth > 4194304 || depth .&. (depth - 1) /= 0 =
      Just (nm <> ": depth must be a power of two between 16 and 4194304, not " <> show depth)
  | Just n <- cdc, outside cdcRange (toInteger n) =
      Just (nm <> ": cdcSyncStages must be between " <> range cdcRange)
  | otherwise = Nothing
 where
  cdcRange = cdcSyncStagesRange depth False

-- | Value of @FIFO_MEMORY_TYPE_*@ for @XPM_FIFO_AXIL@ and @XPM_FIFO_AXIF@.
-- These macros recognize other strings than @XPM_FIFO_AXIS@ (and treat
-- unrecognized strings as @"auto"@). For distributed RAM, we use @"lutram"@:
-- only for that exact string, the macros keep a channel's ready signal low
-- while its FIFO is resetting. See 'axiChannel'.
axiMmMemoryTypeParam :: FifoMemoryType -> String
axiMmMemoryTypeParam t = case t of
  Auto -> "auto"
  Block -> "bram"
  Distributed -> "lutram"
  Ultra -> "uram"

-- | Convert outputs of the base model to 'XpmFifoWriteStatus'
toWriteStatus :: (KnownNat depth, 1 <= depth) => BaseWrOut -> XpmFifoWriteStatus depth
toWriteStatus w = XpmFifoWriteStatus
  { progFull = bwProgFull w
  , almostFull = bwAlmostFull w
  , writeDataCount = fromIntegral (bwWrDataCount w)
  }

-- | Convert outputs of the base model to 'XpmFifoReadStatus'
toReadStatus :: (KnownNat depth, 1 <= depth) => BaseRdOut a -> XpmFifoReadStatus depth
toReadStatus r = XpmFifoReadStatus
  { progEmpty = brProgEmpty r
  , almostEmpty = brAlmostEmpty r
  , readDataCount = fromIntegral (brRdDataCount r)
  }

-- | Base model configuration for a channel of @XPM_FIFO_AXIL@ or
-- @XPM_FIFO_AXIF@ with the given depth and status configuration
channelConfig :: Bool -> Natural -> Natural -> XpmFifoStatusConfig -> BaseConfig
channelConfig commonClock cdc depth XpmFifoStatusConfig{..} = BaseConfig
  { bcCommonClock = commonClock
  , bcDepth = fromIntegral depth
  , bcCdcStages = fromIntegral cdc
  , bcProgFullThresh = fromIntegral <$> statusProgFullThresh
  , bcProgEmptyThresh = fromIntegral <$> statusProgEmptyThresh
  , bcAlmostFull = False
  , bcAlmostEmpty = False
  , bcWrDataCount = statusWriteDataCount
  , bcRdDataCount = statusReadDataCount
  }

-- | Model of a channel of @XPM_FIFO_AXIL@ or @XPM_FIFO_AXIF@: an instance of
-- @xpm_fifo_base@ whose write side accepts data when it is not full, and whose
-- read side offers data when it is not empty.
--
-- For memory type @"lutram"@ ('Distributed'), the write side is also not
-- ready while the FIFO is resetting (@wr_rst_busy@). For other memory types,
-- the write side signals ready for a cycle during the power-on reset of a FIFO
-- with independent clocks (@full@ is deasserted for a cycle), and drops the
-- data written in that cycle.
axiChannel ::
  ( KnownDomain wr
  , KnownDomain rd
  , NFDataX a
  , BitPack a
  ) =>
  FifoMemoryType ->
  BaseConfig ->
  Clock wr ->
  Clock rd ->
  -- | Reset (active high)
  Signal wr Bool ->
  -- | Input (valid and payload)
  Signal wr (Maybe a) ->
  -- | Ready of the receiving side
  Signal rd Bool ->
  ( Signal wr Bool
  -- ^ Ready (not full)
  , Signal rd (Maybe a)
  -- ^ Output (valid and payload)
  , Signal wr BaseWrOut
  , Signal rd (BaseRdOut a)
  )
axiChannel memType cfg clkWr clkRd rst inp rdy = (ready, out, wrOut, rdOut)
 where
  (wrOut, rdOut) =
    fifoBase cfg clkWr clkRd rst
      (isJust <$> inp)
      (fromMaybe (deepErrorX "axiChannel: no data") <$> inp)
      rdEn
  ready
    | memType == Distributed = (\w -> bwFullN w && not (bwWrRstBusy w)) <$> wrOut
    | otherwise = bwFullN <$> wrOut
  valid = not . brEmpty <$> rdOut
  rdEn = (&&) <$> valid <*> rdy
  out = (\v d -> if v then Just d else Nothing) <$> valid <*> (brDout <$> rdOut)

-- | Configuration of the @xpm_fifo_base@ model. Other @xpm_fifo_base@
-- parameters are fixed: @READ_MODE = 1@ (first word fall through),
-- @FIFO_READ_LATENCY = 0@, @READ_DATA_WIDTH = WRITE_DATA_WIDTH@,
-- @FULL_RESET_VALUE = 1@, @ECC_MODE = 0@, @RELATED_CLOCKS = 0@, and
-- @REMOVE_WR_RD_PROT_LOGIC = 0@.
data BaseConfig = BaseConfig
  { bcCommonClock :: Bool
  -- ^ @COMMON_CLOCK@
  , bcDepth :: Int
  -- ^ @FIFO_WRITE_DEPTH@, a power of two of at least 16
  , bcCdcStages :: Int
  -- ^ @CDC_DEST_SYNC_FF@, only used for independent clocks
  , bcProgFullThresh :: Maybe Int
  -- ^ @PROG_FULL_THRESH@ if the programmable full flag is enabled
  -- (@USE_ADV_FEATURES[1]@)
  , bcProgEmptyThresh :: Maybe Int
  -- ^ @PROG_EMPTY_THRESH@ if the programmable empty flag is enabled
  -- (@USE_ADV_FEATURES[9]@)
  , bcAlmostFull :: Bool
  -- ^ @USE_ADV_FEATURES[3]@
  , bcAlmostEmpty :: Bool
  -- ^ @USE_ADV_FEATURES[11]@
  , bcWrDataCount :: Bool
  -- ^ @USE_ADV_FEATURES[2]@. The model's write data count is always
  -- @log2(FIFO_WRITE_DEPTH) + 1@ bits wide.
  , bcRdDataCount :: Bool
  -- ^ @USE_ADV_FEATURES[10]@. The model's read data count is always
  -- @log2(FIFO_WRITE_DEPTH) + 1@ bits wide.
  }
  deriving (Show)

-- | Outputs of @xpm_fifo_base@ in the write domain
data BaseWrOut = BaseWrOut
  { bwFull :: Bool
  , bwFullN :: Bool
  , bwProgFull :: Bool
  , bwAlmostFull :: Bool
  , bwWrRstBusy :: Bool
  , bwWrDataCount :: Int
  }

-- | Outputs of @xpm_fifo_base@ in the read domain
data BaseRdOut a = BaseRdOut
  { brDout :: a
  , brEmpty :: Bool
  , brDataValid :: Bool
  , brAlmostEmpty :: Bool
  , brProgEmpty :: Bool
  , brRdRstBusy :: Bool
  , brRdDataCount :: Int
  }

-- | Register without enable or reset, with an explicit initial value
reg :: a -> Signal dom a -> Signal dom a
reg = (:-)
{-# INLINE reg #-}

-- | Register with a synchronous reset. The reset value is also the initial
-- value, as is the case for all registers in @xpm_fifo_base@ that have both.
regRst :: a -> Signal dom Bool -> Signal dom a -> Signal dom a
regRst i rst d = o
 where
  o = i :- (mux rst (pure i) d)
{-# INLINE regRst #-}

-- | Register with a synchronous reset and an initial value that is undefined
-- (Verilog @reg@ without initializer).
regRstNoInit :: NFDataX a => String -> a -> Signal dom Bool -> Signal dom a -> Signal dom a
regRstNoInit nm r rst d = deepErrorX (nm <> ": undefined before first reset") :- mux rst (pure r) d
{-# INLINE regRstNoInit #-}

-- | Synchronizer: @n@ registers in the destination domain, each initialized
-- to @i@. Models the synchronization stages of @xpm_cdc_single@,
-- @xpm_cdc_gray@, and @xpm_cdc_sync_rst@ (with @INIT_SYNC_FF = 1@).
syncStages ::
  (KnownDomain src, KnownDomain dst) =>
  Int -> a -> Clock src -> Clock dst -> Signal src a -> Signal dst a
syncStages n i clkSrc clkDst = go n . unsafeSynchronizer clkSrc clkDst
 where
  go 0 s = s
  go k s = i :- go (k - 1) s

-- | @xpm_cdc_gray@ with @INIT_SYNC_FF = 1@. The binary to Gray conversion and
-- back is lossless for a synchronous model, so this is a register in the
-- source domain followed by synchronization stages (and an optional output
-- register) in the destination domain.
cdcGray ::
  (KnownDomain src, KnownDomain dst) =>
  -- | @DEST_SYNC_FF@
  Int ->
  -- | @REG_OUTPUT@
  Bool ->
  Clock src -> Clock dst -> Signal src Int -> Signal dst Int
cdcGray n regOutput clkSrc clkDst srcIn
  | regOutput = 0 :- synced
  | otherwise = synced
 where
  synced = syncStages n 0 clkSrc clkDst (0 :- srcIn)

-- | Ceiling of log2
clog2 :: Int -> Int
clog2 n = P.length (P.takeWhile (< n) (P.iterate (*2) 1))


-- | Model of @xpm_fifo_base@ as instantiated by the AXI FIFO macros, using the
-- models of "Clash.Cores.Xilinx.Xpm.Fifo.Internal.Base". In common clock mode,
-- @wr@ and @rd@ must be the same domain and the same clock must be passed
-- twice. Disabled outputs are 'False' or /0/, like the macro's.
fifoBase ::
  forall wr rd a .
  ( KnownDomain wr
  , KnownDomain rd
  , NFDataX a
  , BitPack a
  ) =>
  BaseConfig ->
  Clock wr ->
  Clock rd ->
  -- | @rst@ (active high, write domain)
  Signal wr Bool ->
  -- | @wr_en@
  Signal wr Bool ->
  -- | @din@
  Signal wr a ->
  -- | @rd_en@
  Signal rd Bool ->
  (Signal wr BaseWrOut, Signal rd (BaseRdOut a))
fifoBase BaseConfig{..} clkWr clkRd rst wrEn din rdEn =
  case TN.someNatVal (fromIntegral (clog2 bcDepth)) of
    TN.SomeNat (Proxy :: Proxy n) -> go (SNat @n)
 where
  mcfg = Base.ModelConfig
    { Base.mReadMode = Fwft
    , Base.mFullResetValue = True
    , Base.mProgFullThresh = maybe 0 toInteger bcProgFullThresh
    , Base.mProgEmptyThresh = maybe 0 toInteger bcProgEmptyThresh
    , Base.mCdcSyncStages = bcCdcStages
    , Base.mRelatedClocks = False
    }
  wrData = (\e d -> if e then Just d else Nothing) <$> wrEn <*> din

  go :: forall n . SNat n -> (Signal wr BaseWrOut, Signal rd (BaseRdOut a))
  go SNat
    | bcCommonClock =
        let o = Base.xpmFifoSyncModel @n mcfg rst wrData (unsafeSynchronizer clkRd clkWr rdEn)
        in (wrSide o, unsafeSynchronizer clkWr clkRd (rdSide o))
    | otherwise =
        let o = Base.xpmFifoAsyncModel @n mcfg clkWr clkRd rst wrData rdEn
        in (wrSide o, rdSide o)
   where
    wrSide :: forall r . Base.XpmFifoOut wr r n a -> Signal wr BaseWrOut
    wrSide o = BaseWrOut
      <$> Base.isFull o
      <*> (not <$> Base.isFull o)
      <*> (if isJust bcProgFullThresh then Base.isProgFull o else pure False)
      <*> (if bcAlmostFull then Base.isAlmostFull o else pure False)
      <*> Base.writeResetBusy o
      <*> (if bcWrDataCount then fromIntegral <$> Base.writeCount o else pure 0)

    rdSide :: forall w r . Base.XpmFifoOut w r n a -> Signal r (BaseRdOut a)
    rdSide o = BaseRdOut
      <$> Base.fifoData o
      <*> Base.isEmpty o
      <*> Base.isDataValid o
      <*> (if bcAlmostEmpty then Base.isAlmostEmpty o else pure False)
      <*> (if isJust bcProgEmptyThresh then Base.isProgEmpty o else pure False)
      <*> Base.readResetBusy o
      <*> (if bcRdDataCount then fromIntegral <$> Base.readCount o else pure 0)
