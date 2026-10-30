{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Wrappers for Xilinx's @XPM_FIFO_AXIL@ macro, which puts a FIFO in each of the
five channels of an AXI4-Lite interface. See
[UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_FIFO_AXIL)
for the macro's documentation.

The FIFO has a subordinate side (@s_axi@), to be connected to an AXI4-Lite
manager, and a manager side (@m_axi@), to be connected to an AXI4-Lite
subordinate. The write address, write data, and read address channels flow from
the subordinate side to the manager side; the write response and read data
channels flow back. The two sides can use a common clock ('xpmFifoAxil') or
independent clocks ('xpmFifoAxilAsync').

The signals of the interface are bundled in 'Axi4LiteM2S' (manager to
subordinate) and 'Axi4LiteS2M' (subordinate to manager). These types mirror the
macro's ports one to one. (Version 0.1.1 of @clash-protocols@, which this
package depends on, does not define AXI4-Lite types.) 'Axi4Lite' makes them a
'Protocol', so the FIFOs are also available as 'Circuit's: 'xpmFifoAxilC' and
'xpmFifoAxilAsyncC'.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Axil
  ( -- * Circuits
    xpmFifoAxilC
  , xpmFifoAxilAsyncC

    -- * Signal level
  , xpmFifoAxil
  , xpmFifoAxilAsync

    -- * AXI4-Lite types
  , Axi4Lite
  , Axi4LiteM2S(..)
  , Axi4LiteS2M(..)
  , Axi4LiteAddress(..)
  , Axi4LiteWriteData(..)
  , Axi4LiteReadData(..)
  , AxiResp(..)
  , axi4LiteM2SIdle
  , axi4LiteS2MIdle

    -- * Configuration
  , XpmFifoAxilConfig(..)
  , xpmFifoAxilConfig
  , FifoMemoryType(..)

    -- * Status outputs
  , XpmFifoAxilStatus(..)
  , XpmFifoStatusConfig(..)
  , noStatus
  , XpmFifoWriteStatus(..)
  , XpmFifoReadStatus(..)
  ) where

import Clash.Explicit.Prelude

import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack)
import Protocols (Circuit, Protocol(..), fromSignals)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Fifo.Internal
import Clash.Cores.Xilinx.Xpm.Fifo.Internal.Axi

-- | Address of a read or write transaction (@AxADDR@ and @AxPROT@)
data Axi4LiteAddress (addrWidth :: Nat) = Axi4LiteAddress
  { axilAddr :: BitVector addrWidth
  -- ^ @AWADDR@ / @ARADDR@
  , axilProt :: BitVector 3
  -- ^ @AWPROT@ / @ARPROT@
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, ShowX)

-- | Write data (@WDATA@ and @WSTRB@). Byte /i/ of the data is bits
-- @[8*i+7:8*i]@, and is written if bit /i/ of the strobe is set.
data Axi4LiteWriteData (dataBytes :: Nat) = Axi4LiteWriteData
  { axilWData :: BitVector (dataBytes * 8)
  -- ^ @WDATA@
  , axilWStrb :: BitVector dataBytes
  -- ^ @WSTRB@
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, ShowX)

-- | Read data (@RDATA@ and @RRESP@)
data Axi4LiteReadData (dataBytes :: Nat) = Axi4LiteReadData
  { axilRData :: BitVector (dataBytes * 8)
  -- ^ @RDATA@
  , axilRResp :: AxiResp
  -- ^ @RRESP@
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, ShowX)

-- | Signals from an AXI4-Lite manager to a subordinate. A channel's @VALID@
-- signal is represented by its payload being 'Just'.
data Axi4LiteM2S (addrWidth :: Nat) (dataBytes :: Nat) = Axi4LiteM2S
  { axilAw :: Maybe (Axi4LiteAddress addrWidth)
  -- ^ Write address channel: @AWVALID@, @AWADDR@, @AWPROT@
  , axilW :: Maybe (Axi4LiteWriteData dataBytes)
  -- ^ Write data channel: @WVALID@, @WDATA@, @WSTRB@
  , axilBReady :: Bool
  -- ^ Write response channel: @BREADY@
  , axilAr :: Maybe (Axi4LiteAddress addrWidth)
  -- ^ Read address channel: @ARVALID@, @ARADDR@, @ARPROT@
  , axilRReady :: Bool
  -- ^ Read data channel: @RREADY@
  }
  deriving (Show, Eq, Generic, NFDataX, ShowX)

-- | Signals from an AXI4-Lite subordinate to a manager. A channel's @VALID@
-- signal is represented by its payload being 'Just'.
data Axi4LiteS2M (dataBytes :: Nat) = Axi4LiteS2M
  { axilAwReady :: Bool
  -- ^ Write address channel: @AWREADY@
  , axilWReady :: Bool
  -- ^ Write data channel: @WREADY@
  , axilB :: Maybe AxiResp
  -- ^ Write response channel: @BVALID@, @BRESP@
  , axilArReady :: Bool
  -- ^ Read address channel: @ARREADY@
  , axilR :: Maybe (Axi4LiteReadData dataBytes)
  -- ^ Read data channel: @RVALID@, @RDATA@, @RRESP@
  }
  deriving (Show, Eq, Generic, NFDataX, ShowX)

-- | Manager that does not do anything
axi4LiteM2SIdle :: Axi4LiteM2S addrWidth dataBytes
axi4LiteM2SIdle = Axi4LiteM2S Nothing Nothing False Nothing False

-- | Subordinate that does not do anything
axi4LiteS2MIdle :: Axi4LiteS2M dataBytes
axi4LiteS2MIdle = Axi4LiteS2M False False Nothing False Nothing

-- | AXI4-Lite interface as a 'Protocol'
data Axi4Lite (dom :: Domain) (addrWidth :: Nat) (dataBytes :: Nat)

instance Protocol (Axi4Lite dom addrWidth dataBytes) where
  type Fwd (Axi4Lite dom addrWidth dataBytes) = Signal dom (Axi4LiteM2S addrWidth dataBytes)
  type Bwd (Axi4Lite dom addrWidth dataBytes) = Signal dom (Axi4LiteS2M dataBytes)

-- | Configuration for the @XPM_FIFO_AXIL@ wrappers. Use 'xpmFifoAxilConfig' to
-- get a configuration with defaults. The depth of each channel's FIFO must be
-- a power of two between 16 and 4194304. The depths of the write data and read
-- data channels are type level numbers, because they determine the size of
-- the data counts in 'XpmFifoAxilStatus'.
--
-- The macro's other parameters are fixed:
--
-- +-----------------------------+-----------------+--------------------------------+
-- | Parameter                   | Value           | Reason                         |
-- +=============================+=================+================================+
-- | @ECC_MODE_WDCH@,            | @"no_ecc"@      | Not supported by this wrapper  |
-- | @ECC_MODE_RDCH@             |                 |                                |
-- +-----------------------------+-----------------+--------------------------------+
-- | @CASCADE_HEIGHT@            | @0@             | Let Vivado choose              |
-- +-----------------------------+-----------------+--------------------------------+
-- | @SIM_ASSERT_CHK@            | @0@             |                                |
-- +-----------------------------+-----------------+--------------------------------+
-- | @WR_DATA_COUNT_WIDTH_*@,    | @log2(depth)+1@ | Counts are always full width   |
-- | @RD_DATA_COUNT_WIDTH_*@     |                 |                                |
-- +-----------------------------+-----------------+--------------------------------+
data XpmFifoAxilConfig (wdchDepth :: Nat) (rdchDepth :: Nat) = XpmFifoAxilConfig
  { axilWriteAddressDepth :: Natural
  -- ^ @FIFO_DEPTH_WACH@
  , axilWriteAddressMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_WACH@
  , axilWriteDataDepth :: SNat wdchDepth
  -- ^ @FIFO_DEPTH_WDCH@
  , axilWriteDataMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_WDCH@
  , axilWriteDataStatus :: XpmFifoStatusConfig
  -- ^ Optional status outputs of the write data channel (@USE_ADV_FEATURES_WDCH@,
  -- @PROG_FULL_THRESH_WDCH@, @PROG_EMPTY_THRESH_WDCH@)
  , axilWriteResponseDepth :: Natural
  -- ^ @FIFO_DEPTH_WRCH@
  , axilWriteResponseMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_WRCH@
  , axilReadAddressDepth :: Natural
  -- ^ @FIFO_DEPTH_RACH@
  , axilReadAddressMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_RACH@
  , axilReadDataDepth :: SNat rdchDepth
  -- ^ @FIFO_DEPTH_RDCH@
  , axilReadDataMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_RDCH@
  , axilReadDataStatus :: XpmFifoStatusConfig
  -- ^ Optional status outputs of the read data channel (@USE_ADV_FEATURES_RDCH@,
  -- @PROG_FULL_THRESH_RDCH@, @PROG_EMPTY_THRESH_RDCH@)
  , axilCdcSyncStages :: Natural
  -- ^ Number of synchronization stages used to cross between clock domains
  -- (@CDC_SYNC_STAGES@). Must be between 2 and 8 (at most 4 if a channel has a
  -- depth of 16). Only used by 'xpmFifoAxilAsync'.
  , axilResetSynchronizer :: Bool
  -- ^ Synchronize the reset to both clocks before using it
  -- (@EN_RESET_SYNCHRONIZER@). Without it, the macro uses the reset directly
  -- in both clock domains. This setting must be 'True' for 'xpmFifoAxilAsync',
  -- as the reset is part of the subordinate side's domain.
  }
  deriving (Show)

-- | Default configuration, using the macro's default depths and memory types
-- where possible: distributed RAM for the address channels (16 words) and the
-- write response channel (2048 words), and block RAM of the given depths for
-- the write data and read data channels. No status outputs, two CDC stages,
-- and a reset synchronizer.
xpmFifoAxilConfig :: SNat wdchDepth -> SNat rdchDepth -> XpmFifoAxilConfig wdchDepth rdchDepth
xpmFifoAxilConfig wdch rdch = XpmFifoAxilConfig
  { axilWriteAddressDepth = 16
  , axilWriteAddressMemoryType = Distributed
  , axilWriteDataDepth = wdch
  , axilWriteDataMemoryType = Block
  , axilWriteDataStatus = noStatus
  , axilWriteResponseDepth = 2048
  , axilWriteResponseMemoryType = Distributed
  , axilReadAddressDepth = 16
  , axilReadAddressMemoryType = Distributed
  , axilReadDataDepth = rdch
  , axilReadDataMemoryType = Block
  , axilReadDataStatus = noStatus
  , axilCdcSyncStages = 2
  , axilResetSynchronizer = True
  }

-- | Status outputs of 'xpmFifoAxil' and 'xpmFifoAxilAsync'. The write data
-- channel flows from the subordinate side (@s@) to the manager side (@m@), the
-- read data channel the other way around.
data XpmFifoAxilStatus s m wdchDepth rdchDepth = XpmFifoAxilStatus
  { axilWriteDataWriteStatus :: Signal s (XpmFifoWriteStatus wdchDepth)
  -- ^ @prog_full_wdch@, @wr_data_count_wdch@
  , axilWriteDataReadStatus :: Signal m (XpmFifoReadStatus wdchDepth)
  -- ^ @prog_empty_wdch@, @rd_data_count_wdch@
  , axilReadDataWriteStatus :: Signal m (XpmFifoWriteStatus rdchDepth)
  -- ^ @prog_full_rdch@, @wr_data_count_rdch@
  , axilReadDataReadStatus :: Signal s (XpmFifoReadStatus rdchDepth)
  -- ^ @prog_empty_rdch@, @rd_data_count_rdch@
  }

-- | Constraints shared by all wrappers
type AxilConstraints addrWidth dataBytes wdchDepth rdchDepth =
  ( KnownNat addrWidth
  , 1 <= addrWidth
  , KnownNat dataBytes
  , 1 <= dataBytes
  , KnownNat wdchDepth
  , 1 <= wdchDepth
  , KnownNat rdchDepth
  , 1 <= rdchDepth
  )

-- | AXI4-Lite FIFO with a common clock, as a 'Circuit'. See 'xpmFifoAxil' for
-- details and a version with status outputs.
xpmFifoAxilC ::
  forall addrWidth dataBytes wdchDepth rdchDepth dom .
  ( AxilConstraints addrWidth dataBytes wdchDepth rdchDepth
  , KnownDomain dom
  , HasCallStack
  ) =>
  XpmFifoAxilConfig wdchDepth rdchDepth ->
  Clock dom ->
  Reset dom ->
  Circuit (Axi4Lite dom addrWidth dataBytes) (Axi4Lite dom addrWidth dataBytes)
xpmFifoAxilC cfg clk rst = fromSignals go
 where
  go (sM2S, mS2M) = (sS2M, mM2S)
   where
    (sS2M, mM2S, _) = xpmFifoAxil cfg clk rst sM2S mS2M
{-# INLINE xpmFifoAxilC #-}

-- | AXI4-Lite FIFO with independent clocks, as a 'Circuit'. See
-- 'xpmFifoAxilAsync' for details and a version with status outputs.
xpmFifoAxilAsyncC ::
  forall addrWidth dataBytes wdchDepth rdchDepth s m .
  ( AxilConstraints addrWidth dataBytes wdchDepth rdchDepth
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  XpmFifoAxilConfig wdchDepth rdchDepth ->
  Clock s ->
  Clock m ->
  Reset s ->
  Circuit (Axi4Lite s addrWidth dataBytes) (Axi4Lite m addrWidth dataBytes)
xpmFifoAxilAsyncC cfg clkS clkM rst = fromSignals go
 where
  go (sM2S, mS2M) = (sS2M, mM2S)
   where
    (sS2M, mM2S, _) = xpmFifoAxilAsync cfg clkS clkM rst sM2S mS2M
{-# INLINE xpmFifoAxilAsyncC #-}

-- | AXI4-Lite FIFO with a common clock (@CLOCKING_MODE = "common_clock"@).
--
-- Asserting the reset empties all FIFOs. Each FIFO goes through its own reset
-- sequence, during which it does not accept data. The FIFOs also reset
-- themselves after power-on.
xpmFifoAxil ::
  forall addrWidth dataBytes wdchDepth rdchDepth dom .
  ( AxilConstraints addrWidth dataBytes wdchDepth rdchDepth
  , KnownDomain dom
  , HasCallStack
  ) =>
  XpmFifoAxilConfig wdchDepth rdchDepth ->
  Clock dom ->
  -- | Reset (@s_aresetn@)
  Reset dom ->
  -- | Subordinate side input (@s_axi_*@), driven by an AXI4-Lite manager
  Signal dom (Axi4LiteM2S addrWidth dataBytes) ->
  -- | Manager side input (@m_axi_*@), driven by an AXI4-Lite subordinate
  Signal dom (Axi4LiteS2M dataBytes) ->
  ( Signal dom (Axi4LiteS2M dataBytes)
  -- ^ Subordinate side output (@s_axi_*@)
  , Signal dom (Axi4LiteM2S addrWidth dataBytes)
  -- ^ Manager side output (@m_axi_*@)
  , XpmFifoAxilStatus dom dom wdchDepth rdchDepth
  )
xpmFifoAxil cfg clk = xpmFifoAxilImpl True cfg clk clk
{-# INLINE xpmFifoAxil #-}

-- | AXI4-Lite FIFO with independent clocks
-- (@CLOCKING_MODE = "independent_clock"@).
--
-- Asserting the reset empties all FIFOs. The reset is synchronized to both
-- clocks first ('axilResetSynchronizer' must be set). Each FIFO goes through
-- its own reset sequence, during which it does not accept data. The FIFOs also
-- reset themselves after power-on.
--
-- __N.B.__: Right after power-on, the FIFOs that do not use distributed RAM
-- assert their ready signal for one clock cycle while they are still
-- resetting, and drop the data written in that cycle. Do not offer data in the
-- first few cycles after power-on. (The macro masks ready during reset only
-- for distributed RAM.)
xpmFifoAxilAsync ::
  forall addrWidth dataBytes wdchDepth rdchDepth s m .
  ( AxilConstraints addrWidth dataBytes wdchDepth rdchDepth
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  XpmFifoAxilConfig wdchDepth rdchDepth ->
  -- | Clock of the subordinate side (@s_aclk@)
  Clock s ->
  -- | Clock of the manager side (@m_aclk@)
  Clock m ->
  -- | Reset (@s_aresetn@)
  Reset s ->
  -- | Subordinate side input (@s_axi_*@), driven by an AXI4-Lite manager
  Signal s (Axi4LiteM2S addrWidth dataBytes) ->
  -- | Manager side input (@m_axi_*@), driven by an AXI4-Lite subordinate
  Signal m (Axi4LiteS2M dataBytes) ->
  ( Signal s (Axi4LiteS2M dataBytes)
  -- ^ Subordinate side output (@s_axi_*@)
  , Signal m (Axi4LiteM2S addrWidth dataBytes)
  -- ^ Manager side output (@m_axi_*@)
  , XpmFifoAxilStatus s m wdchDepth rdchDepth
  )
xpmFifoAxilAsync = xpmFifoAxilImpl False
{-# INLINE xpmFifoAxilAsync #-}

xpmFifoAxilImpl ::
  forall addrWidth dataBytes wdchDepth rdchDepth s m .
  ( AxilConstraints addrWidth dataBytes wdchDepth rdchDepth
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  -- | Common clock?
  Bool ->
  XpmFifoAxilConfig wdchDepth rdchDepth ->
  Clock s ->
  Clock m ->
  Reset s ->
  Signal s (Axi4LiteM2S addrWidth dataBytes) ->
  Signal m (Axi4LiteS2M dataBytes) ->
  ( Signal s (Axi4LiteS2M dataBytes)
  , Signal m (Axi4LiteM2S addrWidth dataBytes)
  , XpmFifoAxilStatus s m wdchDepth rdchDepth
  )
xpmFifoAxilImpl commonClock XpmFifoAxilConfig{..} clkS clkM rst sM2S mS2M
  | Just err <- configError = clashCompileError err
  | clashSimulation = sim
  | otherwise = synth
 where
  nm = "xpmFifoAxil"
  wdchDepthI = snatToNum axilWriteDataDepth :: Integer
  rdchDepthI = snatToNum axilReadDataDepth :: Integer
  cdc = if commonClock then Nothing else Just axilCdcSyncStages
  depths =
    [ toInteger axilWriteAddressDepth, wdchDepthI, toInteger axilWriteResponseDepth
    , toInteger axilReadAddressDepth, rdchDepthI ]

  configError
    | (err:_) <- [e | Just e <- fmap (\d -> depthCdcConfigError nm d cdc) depths] = Just err
    | Just err <- statusConfigError nm wdchDepthI cdc axilWriteDataStatus = Just err
    | Just err <- statusConfigError nm rdchDepthI cdc axilReadDataStatus = Just err
    | not commonClock && not axilResetSynchronizer =
        Just (nm <> ": independent clocks need axilResetSynchronizer")
    | not commonClock && Ultra `elem` memoryTypes =
        Just (nm <> ": UltraRAM is only supported for a common clock")
    | otherwise = Nothing

  memoryTypes =
    [ axilWriteAddressMemoryType, axilWriteDataMemoryType, axilWriteResponseMemoryType
    , axilReadAddressMemoryType, axilReadDataMemoryType ]

  --------------------------------------------------------------------------
  -- HDL
  --------------------------------------------------------------------------
  synth = (sS2MS, mM2SS, statusS)
   where
    sS2MS = Axi4LiteS2M
      <$> (bitToBool <$> unPort sAwReady)
      <*> (bitToBool <$> unPort sWReady)
      <*> (mkMaybe <$> unPort sBValid <*> (unpack <$> unPort sBResp))
      <*> (bitToBool <$> unPort sArReady)
      <*> (mkMaybe <$> unPort sRValid <*> (Axi4LiteReadData <$> unPort sRData <*> (unpack <$> unPort sRResp)))

    mM2SS = Axi4LiteM2S
      <$> (mkMaybe <$> unPort mAwValid <*> (Axi4LiteAddress <$> unPort mAwAddr <*> unPort mAwProt))
      <*> (mkMaybe <$> unPort mWValid <*> (Axi4LiteWriteData <$> unPort mWData <*> unPort mWStrb))
      <*> (bitToBool <$> unPort mBReady)
      <*> (mkMaybe <$> unPort mArValid <*> (Axi4LiteAddress <$> unPort mArAddr <*> unPort mArProt))
      <*> (bitToBool <$> unPort mRReady)

    statusS = XpmFifoAxilStatus
      { axilWriteDataWriteStatus =
          XpmFifoWriteStatus <$> (bitToBool <$> unPort progFullWdch) <*> pure False <*> (unpack <$> unPort wrCountWdch)
      , axilWriteDataReadStatus =
          XpmFifoReadStatus <$> (bitToBool <$> unPort progEmptyWdch) <*> pure False <*> (unpack <$> unPort rdCountWdch)
      , axilReadDataWriteStatus =
          XpmFifoWriteStatus <$> (bitToBool <$> unPort progFullRdch) <*> pure False <*> (unpack <$> unPort wrCountRdch)
      , axilReadDataReadStatus =
          XpmFifoReadStatus <$> (bitToBool <$> unPort progEmptyRdch) <*> pure False <*> (unpack <$> unPort rdCountRdch)
      }

    mkMaybe :: Bit -> a -> Maybe a
    mkMaybe v a = if bitToBool v then Just a else Nothing

    sAw = axilAw <$> sM2S
    sW = axilW <$> sM2S
    sAr = axilAr <$> sM2S
    mB = axilB <$> mS2M
    mR = axilR <$> mS2M

    valid :: Signal dom (Maybe a) -> Signal dom Bit
    valid = fmap (boolToBit . isJust)

    payload :: NFDataX a => Signal dom (Maybe a) -> Signal dom a
    payload = fmap (fromMaybe (deepErrorX "xpmFifoAxil: no data"))

    ( sAwReady, sWReady, sBResp, sBValid, sArReady, sRData, sRResp, sRValid
      , mAwAddr, mAwProt, mAwValid, mWData, mWStrb, mWValid, mBReady
      , mArAddr, mArProt, mArValid, mRReady
      , progFullWdch, progEmptyWdch, wrCountWdch, rdCountWdch
      , progFullRdch, progEmptyRdch, wrCountRdch, rdCountRdch ) = go

    go ::
      ( Port "s_axi_awready" s Bit
      , Port "s_axi_wready" s Bit
      , Port "s_axi_bresp" s (BitVector 2)
      , Port "s_axi_bvalid" s Bit
      , Port "s_axi_arready" s Bit
      , Port "s_axi_rdata" s (BitVector (dataBytes * 8))
      , Port "s_axi_rresp" s (BitVector 2)
      , Port "s_axi_rvalid" s Bit
      , Port "m_axi_awaddr" m (BitVector addrWidth)
      , Port "m_axi_awprot" m (BitVector 3)
      , Port "m_axi_awvalid" m Bit
      , Port "m_axi_wdata" m (BitVector (dataBytes * 8))
      , Port "m_axi_wstrb" m (BitVector dataBytes)
      , Port "m_axi_wvalid" m Bit
      , Port "m_axi_bready" m Bit
      , Port "m_axi_araddr" m (BitVector addrWidth)
      , Port "m_axi_arprot" m (BitVector 3)
      , Port "m_axi_arvalid" m Bit
      , Port "m_axi_rready" m Bit
      , Port "prog_full_wdch" s Bit
      , Port "prog_empty_wdch" m Bit
      , Port "wr_data_count_wdch" s (BitVector (CLog 2 wdchDepth + 1))
      , Port "rd_data_count_wdch" m (BitVector (CLog 2 wdchDepth + 1))
      , Port "prog_full_rdch" m Bit
      , Port "prog_empty_rdch" s Bit
      , Port "wr_data_count_rdch" m (BitVector (CLog 2 rdchDepth + 1))
      , Port "rd_data_count_rdch" s (BitVector (CLog 2 rdchDepth + 1))
      )
    go =
      inst
        (instConfig "xpm_fifo_axil")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"AXI_ADDR_WIDTH"           @Integer (natToNum @addrWidth))
        (Param @"AXI_DATA_WIDTH"           @Integer (natToNum @(dataBytes * 8)))
        (Param @"CASCADE_HEIGHT"           @Integer 0)
        (Param @"CDC_SYNC_STAGES"          @Integer (toInteger axilCdcSyncStages))
        (Param @"CLOCKING_MODE"            @String (if commonClock then "common_clock" else "independent_clock"))
        (Param @"ECC_MODE_RDCH"            @String "no_ecc")
        (Param @"ECC_MODE_WDCH"            @String "no_ecc")
        (Param @"EN_RESET_SYNCHRONIZER"    @Integer (boolParam axilResetSynchronizer))
        (Param @"FIFO_DEPTH_RACH"          @Integer (toInteger axilReadAddressDepth))
        (Param @"FIFO_DEPTH_RDCH"          @Integer rdchDepthI)
        (Param @"FIFO_DEPTH_WACH"          @Integer (toInteger axilWriteAddressDepth))
        (Param @"FIFO_DEPTH_WDCH"          @Integer wdchDepthI)
        (Param @"FIFO_DEPTH_WRCH"          @Integer (toInteger axilWriteResponseDepth))
        (Param @"FIFO_MEMORY_TYPE_RACH"    @String (axiMmMemoryTypeParam axilReadAddressMemoryType))
        (Param @"FIFO_MEMORY_TYPE_RDCH"    @String (axiMmMemoryTypeParam axilReadDataMemoryType))
        (Param @"FIFO_MEMORY_TYPE_WACH"    @String (axiMmMemoryTypeParam axilWriteAddressMemoryType))
        (Param @"FIFO_MEMORY_TYPE_WDCH"    @String (axiMmMemoryTypeParam axilWriteDataMemoryType))
        (Param @"FIFO_MEMORY_TYPE_WRCH"    @String (axiMmMemoryTypeParam axilWriteResponseMemoryType))
        (Param @"PROG_EMPTY_THRESH_RDCH"   @Integer (toInteger (fromMaybe 10 (statusProgEmptyThresh axilReadDataStatus))))
        (Param @"PROG_EMPTY_THRESH_WDCH"   @Integer (toInteger (fromMaybe 10 (statusProgEmptyThresh axilWriteDataStatus))))
        (Param @"PROG_FULL_THRESH_RDCH"    @Integer (toInteger (fromMaybe 10 (statusProgFullThresh axilReadDataStatus))))
        (Param @"PROG_FULL_THRESH_WDCH"    @Integer (toInteger (fromMaybe 10 (statusProgFullThresh axilWriteDataStatus))))
        (Param @"RD_DATA_COUNT_WIDTH_RDCH" @Integer (natToNum @(CLog 2 rdchDepth + 1)))
        (Param @"RD_DATA_COUNT_WIDTH_WDCH" @Integer (natToNum @(CLog 2 wdchDepth + 1)))
        (Param @"SIM_ASSERT_CHK"           @Integer 0)
        (Param @"USE_ADV_FEATURES_RDCH"    @String (statusAdvFeatures axilReadDataStatus False False False))
        (Param @"USE_ADV_FEATURES_WDCH"    @String (statusAdvFeatures axilWriteDataStatus False False False))
        (Param @"WR_DATA_COUNT_WIDTH_RDCH" @Integer (natToNum @(CLog 2 rdchDepth + 1)))
        (Param @"WR_DATA_COUNT_WIDTH_WDCH" @Integer (natToNum @(CLog 2 wdchDepth + 1)))

        (ClockPort @"m_aclk" clkM)
        (ClockPort @"s_aclk" clkS)
        (ResetPort @"s_aresetn" @'ActiveLow rst)

        (Port @"s_axi_awaddr" (axilAddr <$> payload sAw))
        (Port @"s_axi_awprot" (axilProt <$> payload sAw))
        (Port @"s_axi_awvalid" (valid sAw))
        (Port @"s_axi_wdata" (axilWData <$> payload sW))
        (Port @"s_axi_wstrb" (axilWStrb <$> payload sW))
        (Port @"s_axi_wvalid" (valid sW))
        (Port @"s_axi_bready" (boolToBit . axilBReady <$> sM2S))
        (Port @"s_axi_araddr" (axilAddr <$> payload sAr))
        (Port @"s_axi_arprot" (axilProt <$> payload sAr))
        (Port @"s_axi_arvalid" (valid sAr))
        (Port @"s_axi_rready" (boolToBit . axilRReady <$> sM2S))

        (Port @"m_axi_awready" (boolToBit . axilAwReady <$> mS2M))
        (Port @"m_axi_wready" (boolToBit . axilWReady <$> mS2M))
        (Port @"m_axi_bresp" (pack <$> payload mB))
        (Port @"m_axi_bvalid" (valid mB))
        (Port @"m_axi_arready" (boolToBit . axilArReady <$> mS2M))
        (Port @"m_axi_rdata" (axilRData <$> payload mR))
        (Port @"m_axi_rresp" (pack . axilRResp <$> payload mR))
        (Port @"m_axi_rvalid" (valid mR))

        (Port @"injectsbiterr_wdch" (pure low :: Signal s Bit))
        (Port @"injectdbiterr_wdch" (pure low :: Signal s Bit))
        (Port @"injectsbiterr_rdch" (pure low :: Signal m Bit))
        (Port @"injectdbiterr_rdch" (pure low :: Signal m Bit))

  --------------------------------------------------------------------------
  -- Simulation
  --------------------------------------------------------------------------
  sim = (sS2MM, mM2SM, statusM)
   where
    cdcI = fromIntegral axilCdcSyncStages :: Int
    cdcN = axilCdcSyncStages

    -- Resets in both clock domains: either synchronized (xpm_cdc_sync_rst with
    -- INIT = 0) or used directly.
    rstHigh = unsafeToActiveHigh rst
    (rstS, rstM)
      | axilResetSynchronizer =
          ( syncStages syncStagesN False clkS clkS rstHigh
          , syncStages syncStagesN False clkS clkM rstHigh )
      | otherwise = (rstHigh, unsafeSynchronizer clkS clkM rstHigh)
     where
      syncStagesN = if commonClock then 4 else cdcI

    cfg depth = channelConfig commonClock cdcN depth noStatus

    (awReady, mAw, _, _) =
      axiChannel axilWriteAddressMemoryType (cfg axilWriteAddressDepth) clkS clkM rstS (axilAw <$> sM2S) (axilAwReady <$> mS2M)
    (wReady, mW, wdchWr, wdchRd) =
      axiChannel axilWriteDataMemoryType (channelConfig commonClock cdcN (snatToNum axilWriteDataDepth) axilWriteDataStatus)
        clkS clkM rstS (axilW <$> sM2S) (axilWReady <$> mS2M)
    (bReady, sB, _, _) =
      axiChannel axilWriteResponseMemoryType (cfg axilWriteResponseDepth) clkM clkS rstM (axilB <$> mS2M) (axilBReady <$> sM2S)
    (arReady, mAr, _, _) =
      axiChannel axilReadAddressMemoryType (cfg axilReadAddressDepth) clkS clkM rstS (axilAr <$> sM2S) (axilArReady <$> mS2M)
    (rReady, sR, rdchWr, rdchRd) =
      axiChannel axilReadDataMemoryType (channelConfig commonClock cdcN (snatToNum axilReadDataDepth) axilReadDataStatus)
        clkM clkS rstM (axilR <$> mS2M) (axilRReady <$> sM2S)

    sS2MM = Axi4LiteS2M <$> awReady <*> wReady <*> sB <*> arReady <*> sR
    mM2SM = Axi4LiteM2S <$> mAw <*> mW <*> bReady <*> mAr <*> rReady

    statusM = XpmFifoAxilStatus
      { axilWriteDataWriteStatus = toWriteStatus <$> wdchWr
      , axilWriteDataReadStatus = toReadStatus <$> wdchRd
      , axilReadDataWriteStatus = toWriteStatus <$> rdchWr
      , axilReadDataReadStatus = toReadStatus <$> rdchRd
      }
{-# INLINE xpmFifoAxilImpl #-}
