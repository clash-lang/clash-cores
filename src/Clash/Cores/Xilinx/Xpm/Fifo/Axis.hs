{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Wrappers for Xilinx's @XPM_FIFO_AXIS@ macro, an AXI4-Stream FIFO. See
[UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_FIFO_AXIS)
for the macro's documentation.

The FIFO accepts data on its subordinate (@s_axis@) side and offers it on its
manager (@m_axis@) side. Both sides use the AXI4-Stream types of
"Protocols.Experimental.Axi4.Stream". The FIFO can use a common clock
('xpmFifoAxis', 'xpmFifoAxisC') or independent clocks ('xpmFifoAxisAsync',
'xpmFifoAxisAsyncC') for its two sides.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Axis
  ( -- * Circuits
    xpmFifoAxisC
  , xpmFifoAxisAsyncC

    -- * Signal level
  , xpmFifoAxis
  , xpmFifoAxisAsync

    -- * Configuration
  , XpmFifoAxisConfig(..)
  , xpmFifoAxisConfig
  , FifoMemoryType(..)

    -- * Status outputs
  , XpmFifoStatusConfig(..)
  , noStatus
  , XpmFifoWriteStatus(..)
  , XpmFifoReadStatus(..)
  ) where

import Clash.Explicit.Prelude

import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack)
import Protocols (Circuit, fromSignals)
import Protocols.Experimental.Axi4.Stream

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Fifo.Internal
import Clash.Cores.Xilinx.Xpm.Fifo.Internal.Axi

-- | Configuration for the @XPM_FIFO_AXIS@ wrappers. Use 'xpmFifoAxisConfig' to
-- get a configuration with defaults.
--
-- The macro's other parameters are fixed:
--
-- +-------------------------+------------------+---------------------------------------------------+
-- | Parameter               | Value            | Reason                                            |
-- +=========================+==================+===================================================+
-- | @ECC_MODE@              | @"no_ecc"@       | Not supported by this wrapper                     |
-- +-------------------------+------------------+---------------------------------------------------+
-- | @RELATED_CLOCKS@        | @0@              | Not supported by this wrapper                     |
-- +-------------------------+------------------+---------------------------------------------------+
-- | @CASCADE_HEIGHT@        | @0@              | Let Vivado choose                                 |
-- +-------------------------+------------------+---------------------------------------------------+
-- | @SIM_ASSERT_CHK@        | @0@              |                                                   |
-- +-------------------------+------------------+---------------------------------------------------+
-- | @WR_DATA_COUNT_WIDTH@,  | @log2(depth)+1@  | Counts are always full width                      |
-- | @RD_DATA_COUNT_WIDTH@   |                  |                                                   |
-- +-------------------------+------------------+---------------------------------------------------+
-- | @USE_ADV_FEATURES[15]@  | @0@              | Setting it skips the reset synchronizer, but then |
-- |                         |                  | the macro uses @s_aresetn@ as an active high      |
-- |                         |                  | reset, which is likely a bug in the macro.        |
-- +-------------------------+------------------+---------------------------------------------------+
data XpmFifoAxisConfig (depth :: Nat) = XpmFifoAxisConfig
  { axisDepth :: SNat depth
  -- ^ Number of words the FIFO can store (@FIFO_DEPTH@). Must be a power of
  -- two between 16 and 4194304.
  , axisMemoryType :: FifoMemoryType
  -- ^ Memory primitive to use (@FIFO_MEMORY_TYPE@)
  , axisPacketFifo :: Bool
  -- ^ Packet mode (@PACKET_FIFO@). In packet mode, the manager side only
  -- offers data once a complete packet (a transfer with @_tlast@ set) has been
  -- written, or once the FIFO is almost full. Packet mode needs at least one
  -- reset before use: the macro's packet counter is undefined until reset.
  -- Packet mode also enables the almost full and almost empty flags.
  , axisCdcSyncStages :: Natural
  -- ^ Number of synchronization stages used to cross between clock domains
  -- (@CDC_SYNC_STAGES@). Must be between 2 and 8 (at most 4 for a depth of 16).
  -- Only used by the FIFOs with independent clocks.
  , axisPacketSizeLessThan8 :: Bool
  -- ^ Only for packet mode with independent clocks (@USE_ADV_FEATURES[13]@).
  -- Adds three extra registers to the crossing of the packet count, which the
  -- macro needs if packets can be smaller than 8 words.
  , axisStatus :: XpmFifoStatusConfig
  -- ^ Optional status outputs
  , axisAlmostFull :: Bool
  -- ^ Enable 'almostFull' (@USE_ADV_FEATURES[3]@)
  , axisAlmostEmpty :: Bool
  -- ^ Enable 'almostEmpty' (@USE_ADV_FEATURES[11]@)
  }
  deriving (Show)

-- | Default configuration: no packet mode, automatic memory type, two CDC
-- stages, and all optional status outputs disabled.
xpmFifoAxisConfig :: SNat depth -> XpmFifoAxisConfig depth
xpmFifoAxisConfig depth = XpmFifoAxisConfig
  { axisDepth = depth
  , axisMemoryType = Auto
  , axisPacketFifo = False
  , axisCdcSyncStages = 2
  , axisPacketSizeLessThan8 = False
  , axisStatus = noStatus
  , axisAlmostFull = False
  , axisAlmostEmpty = False
  }

-- | Constraints shared by all wrappers
type AxisConstraints depth conf user =
  ( KnownNat depth
  , 16 <= depth
  , depth <= 4194304
  , FLog 2 depth ~ CLog 2 depth
  , KnownAxi4StreamConfig conf
  , 1 <= DataWidth conf
  , DataWidth conf <= 256
  , IdWidth conf <= 32
  , DestWidth conf <= 32
  , BitPack user
  , NFDataX user
  )

-- | AXI4-Stream FIFO with a common clock for both sides, as a 'Circuit'. See
-- 'xpmFifoAxis' for details and a version with status outputs.
xpmFifoAxisC ::
  forall depth conf user dom .
  ( AxisConstraints depth conf user
  , KnownDomain dom
  , HasCallStack
  ) =>
  XpmFifoAxisConfig depth ->
  Clock dom ->
  Reset dom ->
  Circuit (Axi4Stream dom conf user) (Axi4Stream dom conf user)
xpmFifoAxisC cfg clk rst = fromSignals go
 where
  go (sIn, mBwd) = (sBwd, mOut)
   where
    (sBwd, mOut, _, _) = xpmFifoAxis cfg clk rst sIn mBwd
{-# INLINE xpmFifoAxisC #-}

-- | AXI4-Stream FIFO with independent clocks for both sides, as a 'Circuit'.
-- See 'xpmFifoAxisAsync' for details and a version with status outputs.
xpmFifoAxisAsyncC ::
  forall depth conf user s m .
  ( AxisConstraints depth conf user
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  XpmFifoAxisConfig depth ->
  Clock s ->
  Clock m ->
  Reset s ->
  Circuit (Axi4Stream s conf user) (Axi4Stream m conf user)
xpmFifoAxisAsyncC cfg clkS clkM rst = fromSignals go
 where
  go (sIn, mBwd) = (sBwd, mOut)
   where
    (sBwd, mOut, _, _) = xpmFifoAxisAsync cfg clkS clkM rst sIn mBwd
{-# INLINE xpmFifoAxisAsyncC #-}

-- | AXI4-Stream FIFO with a common clock (@CLOCKING_MODE = "common_clock"@).
--
-- The reset is synchronized to the clock by the macro (four stages), after
-- which the FIFO goes through its own reset sequence. The FIFO does not accept
-- data while resetting. The FIFO also resets itself after power-on, so
-- asserting the reset is only needed in packet mode.
--
-- Fields of the AXI4-Stream payload that are zero bits wide (e.g. the ID if
-- the @conf@ has an ID width of 0) are stored as a single bit, because the
-- macro does not support zero-width fields.
xpmFifoAxis ::
  forall depth conf user dom .
  ( AxisConstraints depth conf user
  , KnownDomain dom
  , HasCallStack
  ) =>
  XpmFifoAxisConfig depth ->
  Clock dom ->
  -- | Reset. Asserting it empties the FIFO.
  Reset dom ->
  -- | Subordinate side input (@s_axis_*@)
  Signal dom (Maybe (Axi4StreamM2S conf user)) ->
  -- | Manager side input (@m_axis_tready@)
  Signal dom Axi4StreamS2M ->
  ( Signal dom Axi4StreamS2M
  -- ^ Subordinate side output (@s_axis_tready@)
  , Signal dom (Maybe (Axi4StreamM2S conf user))
  -- ^ Manager side output (@m_axis_*@)
  , Signal dom (XpmFifoWriteStatus depth)
  -- ^ Status outputs in the write domain
  , Signal dom (XpmFifoReadStatus depth)
  -- ^ Status outputs in the read domain
  )
xpmFifoAxis cfg clk = xpmFifoAxisImpl True cfg clk clk
{-# INLINE xpmFifoAxis #-}

-- | AXI4-Stream FIFO with independent clocks
-- (@CLOCKING_MODE = "independent_clock"@).
--
-- The reset, part of the subordinate side's domain, is synchronized by the
-- macro ('axisCdcSyncStages' stages), after which the FIFO goes through its own
-- reset sequence, which involves both clock domains. The FIFO does not accept
-- data while resetting. The FIFO also resets itself after power-on, so
-- asserting the reset is only needed in packet mode.
--
-- __N.B.__: Right after power-on, the macro asserts @s_axis_tready@ for one
-- clock cycle while it is still resetting, and drops the data written in that
-- cycle. Do not offer data in the first few cycles after power-on.
--
-- Fields of the AXI4-Stream payload that are zero bits wide (e.g. the ID if
-- the @conf@ has an ID width of 0) are stored as a single bit, because the
-- macro does not support zero-width fields.
xpmFifoAxisAsync ::
  forall depth conf user s m .
  ( AxisConstraints depth conf user
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  XpmFifoAxisConfig depth ->
  -- | Clock of the subordinate (write) side (@s_aclk@)
  Clock s ->
  -- | Clock of the manager (read) side (@m_aclk@)
  Clock m ->
  -- | Reset. Asserting it empties the FIFO.
  Reset s ->
  -- | Subordinate side input (@s_axis_*@)
  Signal s (Maybe (Axi4StreamM2S conf user)) ->
  -- | Manager side input (@m_axis_tready@)
  Signal m Axi4StreamS2M ->
  ( Signal s Axi4StreamS2M
  -- ^ Subordinate side output (@s_axis_tready@)
  , Signal m (Maybe (Axi4StreamM2S conf user))
  -- ^ Manager side output (@m_axis_*@)
  , Signal s (XpmFifoWriteStatus depth)
  -- ^ Status outputs in the write domain
  , Signal m (XpmFifoReadStatus depth)
  -- ^ Status outputs in the read domain
  )
xpmFifoAxisAsync = xpmFifoAxisImpl False
{-# INLINE xpmFifoAxisAsync #-}

xpmFifoAxisImpl ::
  forall depth conf user s m .
  ( AxisConstraints depth conf user
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  -- | Common clock?
  Bool ->
  XpmFifoAxisConfig depth ->
  Clock s ->
  Clock m ->
  Reset s ->
  Signal s (Maybe (Axi4StreamM2S conf user)) ->
  Signal m Axi4StreamS2M ->
  ( Signal s Axi4StreamS2M
  , Signal m (Maybe (Axi4StreamM2S conf user))
  , Signal s (XpmFifoWriteStatus depth)
  , Signal m (XpmFifoReadStatus depth)
  )
xpmFifoAxisImpl commonClock XpmFifoAxisConfig{..} clkS clkM rst sIn mBwd
  | Just err <- configError = clashCompileError err
  | clashSimulation = sim
  | otherwise = synth
 where
  depthI = snatToNum axisDepth :: Int
  depthInteger = snatToNum axisDepth :: Integer
  cdc = if commonClock then Nothing else Just axisCdcSyncStages

  configError
    | Just err <- depthCdcConfigError "xpmFifoAxis" depthInteger cdc = Just err
    | Just err <- statusConfigError "xpmFifoAxis" depthInteger cdc axisStatus = Just err
    | commonClock && axisPacketSizeLessThan8 =
        Just "xpmFifoAxis: axisPacketSizeLessThan8 is only supported for independent clocks"
    | not commonClock && axisMemoryType == Ultra =
        Just "xpmFifoAxis: UltraRAM is only supported for a common clock"
    | otherwise = Nothing

  advFeatures =
    statusAdvFeatures axisStatus axisAlmostFull axisAlmostEmpty
      (axisPacketFifo && not commonClock && axisPacketSizeLessThan8)

  --------------------------------------------------------------------------
  -- HDL
  --------------------------------------------------------------------------
  synth = (sBwdS, mOutS, wrStatusS, rdStatusS)
   where
    sBwdS = Axi4StreamS2M . bitToBool <$> unPort sReadyP
    mOutS = toM2S <$> unPort mValidP <*> unPort mDataP <*> unPort mStrbP <*> unPort mKeepP
               <*> unPort mLastP <*> unPort mIdP <*> unPort mDestP <*> unPort mUserP
     where
      toM2S valid dat strb keep lst tid tdest tuser
        | bitToBool valid = Just Axi4StreamM2S
            { _tdata = reverse (unpack dat)
            , _tstrb = reverse (unpack strb)
            , _tkeep = reverse (unpack keep)
            , _tlast = bitToBool lst
            , _tid = unpack (resize tid)
            , _tdest = unpack (resize tdest)
            , _tuser = unpack (resize tuser)
            }
        | otherwise = Nothing
    wrStatusS = XpmFifoWriteStatus
      <$> (bitToBool <$> unPort progFullP)
      <*> (bitToBool <$> unPort almostFullP)
      <*> (unpack <$> unPort wrCountP)
    rdStatusS = XpmFifoReadStatus
      <$> (bitToBool <$> unPort progEmptyP)
      <*> (bitToBool <$> unPort almostEmptyP)
      <*> (unpack <$> unPort rdCountP)

    sValid = boolToBit . isJust <$> sIn
    sPayload = fromMaybe (deepErrorX "xpmFifoAxis: no data") <$> sIn

    ( sReadyP, mValidP, mDataP, mStrbP, mKeepP, mLastP, mIdP, mDestP, mUserP
      , progFullP, wrCountP, almostFullP, progEmptyP, rdCountP, almostEmptyP ) = go

    go ::
      ( Port "s_axis_tready" s Bit
      , Port "m_axis_tvalid" m Bit
      , Port "m_axis_tdata" m (BitVector (8 * DataWidth conf))
      , Port "m_axis_tstrb" m (BitVector (DataWidth conf))
      , Port "m_axis_tkeep" m (BitVector (DataWidth conf))
      , Port "m_axis_tlast" m Bit
      , Port "m_axis_tid" m (BitVector (Max 1 (IdWidth conf)))
      , Port "m_axis_tdest" m (BitVector (Max 1 (DestWidth conf)))
      , Port "m_axis_tuser" m (BitVector (Max 1 (BitSize user)))
      , Port "prog_full_axis" s Bit
      , Port "wr_data_count_axis" s (BitVector (CLog 2 depth + 1))
      , Port "almost_full_axis" s Bit
      , Port "prog_empty_axis" m Bit
      , Port "rd_data_count_axis" m (BitVector (CLog 2 depth + 1))
      , Port "almost_empty_axis" m Bit
      )
    go =
      inst
        (instConfig "xpm_fifo_axis")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"CASCADE_HEIGHT"      @Integer 0)
        (Param @"CDC_SYNC_STAGES"     @Integer (toInteger axisCdcSyncStages))
        (Param @"CLOCKING_MODE"       @String (if commonClock then "common_clock" else "independent_clock"))
        (Param @"ECC_MODE"            @String "no_ecc")
        (Param @"FIFO_DEPTH"          @Integer (natToNum @depth))
        (Param @"FIFO_MEMORY_TYPE"    @String (fifoMemoryTypeParam axisMemoryType))
        (Param @"PACKET_FIFO"         @String (if axisPacketFifo then "true" else "false"))
        (Param @"PROG_EMPTY_THRESH"   @Integer (toInteger (fromMaybe 10 (statusProgEmptyThresh axisStatus))))
        (Param @"PROG_FULL_THRESH"    @Integer (toInteger (fromMaybe 10 (statusProgFullThresh axisStatus))))
        (Param @"RD_DATA_COUNT_WIDTH" @Integer (natToNum @(CLog 2 depth + 1)))
        (Param @"RELATED_CLOCKS"      @Integer 0)
        (Param @"SIM_ASSERT_CHK"      @Integer 0)
        (Param @"TDATA_WIDTH"         @Integer (natToNum @(8 * DataWidth conf)))
        (Param @"TDEST_WIDTH"         @Integer (natToNum @(Max 1 (DestWidth conf))))
        (Param @"TID_WIDTH"           @Integer (natToNum @(Max 1 (IdWidth conf))))
        (Param @"TUSER_WIDTH"         @Integer (natToNum @(Max 1 (BitSize user))))
        (Param @"USE_ADV_FEATURES"    @String advFeatures)
        (Param @"WR_DATA_COUNT_WIDTH" @Integer (natToNum @(CLog 2 depth + 1)))

        (ResetPort @"s_aresetn" @'ActiveLow rst)
        (ClockPort @"s_aclk" clkS)
        (ClockPort @"m_aclk" clkM)

        (Port @"s_axis_tvalid" sValid)
        (Port @"s_axis_tdata" (pack . reverse . _tdata <$> sPayload))
        (Port @"s_axis_tstrb" (pack . reverse . _tstrb <$> sPayload))
        (Port @"s_axis_tkeep" (pack . reverse . _tkeep <$> sPayload))
        (Port @"s_axis_tlast" (boolToBit . _tlast <$> sPayload))
        (Port @"s_axis_tid" (resize . pack . _tid <$> sPayload :: Signal s (BitVector (Max 1 (IdWidth conf)))))
        (Port @"s_axis_tdest" (resize . pack . _tdest <$> sPayload :: Signal s (BitVector (Max 1 (DestWidth conf)))))
        (Port @"s_axis_tuser" (resize . pack . _tuser <$> sPayload :: Signal s (BitVector (Max 1 (BitSize user)))))

        (Port @"m_axis_tready" (boolToBit . _tready <$> mBwd))

        (Port @"injectsbiterr_axis" (pure low :: Signal s Bit))
        (Port @"injectdbiterr_axis" (pure low :: Signal s Bit))

  --------------------------------------------------------------------------
  -- Simulation
  --------------------------------------------------------------------------
  sim = (sBwdM, mOutM, wrStatusM, rdStatusM)
   where
    s2m :: Signal s x -> Signal m x
    s2m = unsafeSynchronizer clkS clkM

    cdcI = fromIntegral axisCdcSyncStages
    baseCfg = BaseConfig
      { bcCommonClock = commonClock
      , bcDepth = depthI
      , bcCdcStages = cdcI
      , bcProgFullThresh = fromIntegral <$> statusProgFullThresh axisStatus
      , bcProgEmptyThresh = fromIntegral <$> statusProgEmptyThresh axisStatus
      , bcAlmostFull = axisAlmostFull || axisPacketFifo
      , bcAlmostEmpty = axisAlmostEmpty || axisPacketFifo
      , bcWrDataCount = statusWriteDataCount axisStatus
      , bcRdDataCount = statusReadDataCount axisStatus
      }

    -- xpm_cdc_sync_rst with INIT = 0. Source and destination are both s_aclk.
    rstAxis = syncStages (if commonClock then 4 else cdcI) False clkS clkS (unsafeToActiveHigh rst)

    sValid = isJust <$> sIn
    sPayload = fromMaybe (deepErrorX "xpmFifoAxis: no data") <$> sIn
    -- The FIFO model needs a BitPack instance, which Axi4StreamM2S lacks
    toTuple Axi4StreamM2S{..} = (_tdata, _tkeep, _tstrb, _tlast, _tid, _tdest, _tuser)
    fromTuple (_tdata, _tkeep, _tstrb, _tlast, _tid, _tdest, _tuser) = Axi4StreamM2S{..}
    (wrOut, rdOutT) = fifoBase baseCfg clkS clkM rstAxis sValid (toTuple <$> sPayload) rdEn
    rdOut = (\r -> r{brDout = fromTuple (brDout r)}) <$> rdOutT

    sReady = bwFullN <$> wrOut
    sBwdM = Axi4StreamS2M <$> sReady

    dataValid = brDataValid <$> rdOut
    mValid = (&&) <$> dataValid <*> pktRead
    mReady = _tready <$> mBwd
    rdEn = (&&) <$> mValid <*> mReady
    mOutM = (\v d -> if v then Just d else Nothing) <$> mValid <*> (brDout <$> rdOut)

    wrStatusM = toWriteStatus <$> wrOut
    rdStatusM = toReadStatus <$> rdOut

    -- Packet mode
    wrEop = (\v r d -> v && r && _tlast d) <$> sValid <*> sReady <*> sPayload
    rdEop = (\v r d p -> v && r && _tlast d && p) <$> mValid <*> mReady <*> (brDout <$> rdOut) <*> pktRead

    pktRead :: Signal m Bool
    pktRead
      | not axisPacketFifo = pure True
      | commonClock = pktReadCc
      | otherwise = pktReadIc

    -- Common clock. Even though s ~ m in this mode, the types don't know.
    -- Signals are moved between them with 's2m', which is the identity
    -- function if 'clkS' and 'clkM' are the same clock.
    wrEopD1 = regRst False rstAxis wrEop
    pktCnt :: Signal s Int
    pktCnt = regRstNoInit "xpmFifoAxis: packet count" 0 rstAxis
      (goCnt <$> wrEopD1 <*> unsafeSynchronizer clkM clkS rdEop <*> pktCnt)
     where
      goCnt wd re c
        | wd && not re = c + 1
        | re && not wd = c - 1
        | otherwise = c
    pktReadCc = pr
     where
      pr = reg False (goPr <$> s2m rstAxis <*> rdEop <*> s2m pktCnt <*> s2m wrEopD1
                       <*> s2m (bwAlmostFull <$> wrOut) <*> dataValid <*> pr)
      goPr r re cnt wd af dv x
        | r = False
        | re && cnt == 1 && not wd = False
        | cnt > 0 || (af && dv) = True
        | otherwise = x

    -- Independent clocks
    rdRstBusy = brRdRstBusy <$> rdOut
    wpktCnt = regRst 0 rstAxis ((\e c -> if e then (c + 1) `mod` depthI else c) <$> wrEop <*> wpktCnt)
    wpktCntRd0 = cdcGray cdcI True clkS clkM wpktCnt
    wpktCntRd
      | axisPacketSizeLessThan8 = r (r (r wpktCntRd0))
      | otherwise = wpktCntRd0
     where
      r = regRst 0 rdRstBusy
    afRd = syncStages cdcI False clkS clkM (bwAlmostFull <$> wrOut)
    rpktCnt = regRst 0 rdRstBusy ((\e c -> if e then (c + 1) `mod` depthI else c) <$> rdEop <*> rpktCnt)
    diffPktCnt = regRst 0 rdRstBusy
      ((\wc rc e -> (wc - rc - (if e then 1 else 0)) `mod` depthI) <$> wpktCntRd <*> rpktCnt <*> rdEop)
    pktReadIc = pr
     where
      pr = reg False (goPr <$> rdRstBusy <*> rdEop <*> diffPktCnt <*> afRd <*> dataValid <*> pr)
      goPr r re d af dv x
        | r = False
        | re && d == 1 = False
        | d > 0 || (af && dv) = True
        | otherwise = x
{-# INLINE xpmFifoAxisImpl #-}
