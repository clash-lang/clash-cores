{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Wrappers for Xilinx's @XPM_FIFO_AXIF@ macro, which puts a FIFO in each of the
five channels of a (full) AXI4 memory mapped interface. See
[UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_FIFO_AXIF)
for the macro's documentation.

The FIFO has a subordinate side (@s_axi@), to be connected to an AXI4 manager,
and a manager side (@m_axi@), to be connected to an AXI4 subordinate. The
write address, write data, and read address channels flow from the subordinate
side to the manager side; the write response and read data channels flow back.
The two sides can use a common clock ('xpmFifoAxif') or independent clocks
('xpmFifoAxifAsync').

The signals of the interface are bundled in 'Axi4M2S' (manager to subordinate)
and 'Axi4S2M' (subordinate to manager). These types mirror the macro's ports
one to one, with the widths of the configurable signals given by an
'Axi4Config'. The AXI4 types of @clash-protocols@ 0.1.1 cannot represent all
values of all signals (e.g. a QoS value of 15), so they are not used here.
'Axi4' makes the bundles a 'Protocol', so the FIFOs are also available as
'Circuit's: 'xpmFifoAxifC' and 'xpmFifoAxifAsyncC'.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Axif
  ( -- * Circuits
    xpmFifoAxifC
  , xpmFifoAxifAsyncC

    -- * Signal level
  , xpmFifoAxif
  , xpmFifoAxifAsync

    -- * AXI4 types
  , Axi4
  , Axi4Config(..)
  , AxiIdWidth
  , AxiAddrWidth
  , AxiDataBytes
  , AxiAwUserWidth
  , AxiWUserWidth
  , AxiBUserWidth
  , AxiArUserWidth
  , AxiRUserWidth
  , KnownAxi4Config
  , Axi4M2S(..)
  , Axi4S2M(..)
  , Axi4Address(..)
  , Axi4WriteData(..)
  , Axi4WriteResponse(..)
  , Axi4ReadData(..)
  , AxiResp(..)
  , axi4M2SIdle
  , axi4S2MIdle

    -- * Configuration
  , XpmFifoAxifConfig(..)
  , xpmFifoAxifConfig
  , FifoMemoryType(..)

    -- * Status outputs
  , XpmFifoAxifStatus(..)
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

-- | Widths of the configurable signals of an AXI4 interface. Used at the type
-- level only.
--
-- > 'Axi4Config idWidth addrWidth dataBytes awUserWidth wUserWidth bUserWidth arUserWidth rUserWidth
--
-- with the width of @AxID@ / @BID@ / @RID@, @AxADDR@, @WDATA@ / @RDATA@ (in
-- bytes), @AWUSER@, @WUSER@, @BUSER@, @ARUSER@, and @RUSER@.
data Axi4Config = Axi4Config Nat Nat Nat Nat Nat Nat Nat Nat

-- | Width of the ID signals
type family AxiIdWidth (c :: Axi4Config) :: Nat where
  AxiIdWidth ('Axi4Config a _ _ _ _ _ _ _) = a
-- | Width of the address signals
type family AxiAddrWidth (c :: Axi4Config) :: Nat where
  AxiAddrWidth ('Axi4Config _ a _ _ _ _ _ _) = a
-- | Width of the data signals, in bytes
type family AxiDataBytes (c :: Axi4Config) :: Nat where
  AxiDataBytes ('Axi4Config _ _ a _ _ _ _ _) = a
-- | Width of @AWUSER@
type family AxiAwUserWidth (c :: Axi4Config) :: Nat where
  AxiAwUserWidth ('Axi4Config _ _ _ a _ _ _ _) = a
-- | Width of @WUSER@
type family AxiWUserWidth (c :: Axi4Config) :: Nat where
  AxiWUserWidth ('Axi4Config _ _ _ _ a _ _ _) = a
-- | Width of @BUSER@
type family AxiBUserWidth (c :: Axi4Config) :: Nat where
  AxiBUserWidth ('Axi4Config _ _ _ _ _ a _ _) = a
-- | Width of @ARUSER@
type family AxiArUserWidth (c :: Axi4Config) :: Nat where
  AxiArUserWidth ('Axi4Config _ _ _ _ _ _ a _) = a
-- | Width of @RUSER@
type family AxiRUserWidth (c :: Axi4Config) :: Nat where
  AxiRUserWidth ('Axi4Config _ _ _ _ _ _ _ a) = a

-- | All widths of an 'Axi4Config' are known
type KnownAxi4Config c =
  ( KnownNat (AxiIdWidth c)
  , KnownNat (AxiAddrWidth c)
  , KnownNat (AxiDataBytes c)
  , KnownNat (AxiAwUserWidth c)
  , KnownNat (AxiWUserWidth c)
  , KnownNat (AxiBUserWidth c)
  , KnownNat (AxiArUserWidth c)
  , KnownNat (AxiRUserWidth c)
  )

-- | Address of a read or write transaction (@AW*@ or @AR*@)
data Axi4Address (idWidth :: Nat) (addrWidth :: Nat) (userWidth :: Nat) = Axi4Address
  { axiId :: BitVector idWidth
  -- ^ @AxID@
  , axiAddr :: BitVector addrWidth
  -- ^ @AxADDR@
  , axiLen :: BitVector 8
  -- ^ @AxLEN@: number of transfers in the burst, minus one
  , axiSize :: BitVector 3
  -- ^ @AxSIZE@
  , axiBurst :: BitVector 2
  -- ^ @AxBURST@
  , axiLock :: Bit
  -- ^ @AxLOCK@
  , axiCache :: BitVector 4
  -- ^ @AxCACHE@
  , axiProt :: BitVector 3
  -- ^ @AxPROT@
  , axiQos :: BitVector 4
  -- ^ @AxQOS@
  , axiRegion :: BitVector 4
  -- ^ @AxREGION@
  , axiUser :: BitVector userWidth
  -- ^ @AxUSER@
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, ShowX)

-- | Write data (@W*@). Byte /i/ of the data is bits @[8*i+7:8*i]@, and is
-- written if bit /i/ of the strobe is set.
data Axi4WriteData (dataBytes :: Nat) (userWidth :: Nat) = Axi4WriteData
  { axiWData :: BitVector (dataBytes * 8)
  -- ^ @WDATA@
  , axiWStrb :: BitVector dataBytes
  -- ^ @WSTRB@
  , axiWLast :: Bool
  -- ^ @WLAST@
  , axiWUser :: BitVector userWidth
  -- ^ @WUSER@
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, ShowX)

-- | Write response (@B*@)
data Axi4WriteResponse (idWidth :: Nat) (userWidth :: Nat) = Axi4WriteResponse
  { axiBId :: BitVector idWidth
  -- ^ @BID@
  , axiBResp :: AxiResp
  -- ^ @BRESP@
  , axiBUser :: BitVector userWidth
  -- ^ @BUSER@
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, ShowX)

-- | Read data (@R*@)
data Axi4ReadData (idWidth :: Nat) (dataBytes :: Nat) (userWidth :: Nat) = Axi4ReadData
  { axiRId :: BitVector idWidth
  -- ^ @RID@
  , axiRData :: BitVector (dataBytes * 8)
  -- ^ @RDATA@
  , axiRResp :: AxiResp
  -- ^ @RRESP@
  , axiRLast :: Bool
  -- ^ @RLAST@
  , axiRUser :: BitVector userWidth
  -- ^ @RUSER@
  }
  deriving (Show, Eq, Generic, NFDataX, BitPack, ShowX)

-- | Signals from an AXI4 manager to a subordinate. A channel's @VALID@ signal
-- is represented by its payload being 'Just'.
data Axi4M2S (c :: Axi4Config) = Axi4M2S
  { axiAw :: Maybe (Axi4Address (AxiIdWidth c) (AxiAddrWidth c) (AxiAwUserWidth c))
  -- ^ Write address channel
  , axiW :: Maybe (Axi4WriteData (AxiDataBytes c) (AxiWUserWidth c))
  -- ^ Write data channel
  , axiBReady :: Bool
  -- ^ Write response channel: @BREADY@
  , axiAr :: Maybe (Axi4Address (AxiIdWidth c) (AxiAddrWidth c) (AxiArUserWidth c))
  -- ^ Read address channel
  , axiRReady :: Bool
  -- ^ Read data channel: @RREADY@
  }
  deriving (Generic)

deriving instance KnownAxi4Config c => Show (Axi4M2S c)
deriving instance KnownAxi4Config c => ShowX (Axi4M2S c)
deriving instance KnownAxi4Config c => Eq (Axi4M2S c)
deriving instance KnownAxi4Config c => NFDataX (Axi4M2S c)

-- | Signals from an AXI4 subordinate to a manager. A channel's @VALID@ signal
-- is represented by its payload being 'Just'.
data Axi4S2M (c :: Axi4Config) = Axi4S2M
  { axiAwReady :: Bool
  -- ^ Write address channel: @AWREADY@
  , axiWReady :: Bool
  -- ^ Write data channel: @WREADY@
  , axiB :: Maybe (Axi4WriteResponse (AxiIdWidth c) (AxiBUserWidth c))
  -- ^ Write response channel
  , axiArReady :: Bool
  -- ^ Read address channel: @ARREADY@
  , axiR :: Maybe (Axi4ReadData (AxiIdWidth c) (AxiDataBytes c) (AxiRUserWidth c))
  -- ^ Read data channel
  }
  deriving (Generic)

deriving instance KnownAxi4Config c => Show (Axi4S2M c)
deriving instance KnownAxi4Config c => ShowX (Axi4S2M c)
deriving instance KnownAxi4Config c => Eq (Axi4S2M c)
deriving instance KnownAxi4Config c => NFDataX (Axi4S2M c)

-- | Manager that does not do anything
axi4M2SIdle :: Axi4M2S c
axi4M2SIdle = Axi4M2S Nothing Nothing False Nothing False

-- | Subordinate that does not do anything
axi4S2MIdle :: Axi4S2M c
axi4S2MIdle = Axi4S2M False False Nothing False Nothing

-- | AXI4 interface as a 'Protocol'
data Axi4 (dom :: Domain) (c :: Axi4Config)

instance Protocol (Axi4 dom c) where
  type Fwd (Axi4 dom c) = Signal dom (Axi4M2S c)
  type Bwd (Axi4 dom c) = Signal dom (Axi4S2M c)

-- | Configuration for the @XPM_FIFO_AXIF@ wrappers. Use 'xpmFifoAxifConfig' to
-- get a configuration with defaults. The depth of each channel's FIFO must be
-- a power of two between 16 and 4194304. The depths of the write data and read
-- data channels are type level numbers, because they determine the size of
-- the data counts in 'XpmFifoAxifStatus'.
--
-- The macro's other parameters are fixed:
--
-- +-----------------------------+-----------------+--------------------------------+
-- | Parameter                   | Value           | Reason                         |
-- +=============================+=================+================================+
-- | @AXI_LEN_WIDTH@             | @8@             | AXI4 burst length              |
-- +-----------------------------+-----------------+--------------------------------+
-- | @PACKET_FIFO@               | @"false"@       | Not supported by this wrapper  |
-- +-----------------------------+-----------------+--------------------------------+
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
--
-- The macro's lock signals are two bits wide (as in AXI3); the upper bit is
-- driven with /0/.
data XpmFifoAxifConfig (wdchDepth :: Nat) (rdchDepth :: Nat) = XpmFifoAxifConfig
  { axifWriteAddressDepth :: Natural
  -- ^ @FIFO_DEPTH_WACH@
  , axifWriteAddressMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_WACH@
  , axifWriteDataDepth :: SNat wdchDepth
  -- ^ @FIFO_DEPTH_WDCH@
  , axifWriteDataMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_WDCH@
  , axifWriteDataStatus :: XpmFifoStatusConfig
  -- ^ Optional status outputs of the write data channel (@USE_ADV_FEATURES_WDCH@,
  -- @PROG_FULL_THRESH_WDCH@, @PROG_EMPTY_THRESH_WDCH@)
  , axifWriteResponseDepth :: Natural
  -- ^ @FIFO_DEPTH_WRCH@
  , axifWriteResponseMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_WRCH@
  , axifReadAddressDepth :: Natural
  -- ^ @FIFO_DEPTH_RACH@
  , axifReadAddressMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_RACH@
  , axifReadDataDepth :: SNat rdchDepth
  -- ^ @FIFO_DEPTH_RDCH@
  , axifReadDataMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE_RDCH@
  , axifReadDataStatus :: XpmFifoStatusConfig
  -- ^ Optional status outputs of the read data channel (@USE_ADV_FEATURES_RDCH@,
  -- @PROG_FULL_THRESH_RDCH@, @PROG_EMPTY_THRESH_RDCH@)
  , axifCdcSyncStages :: Natural
  -- ^ Number of synchronization stages used to cross between clock domains
  -- (@CDC_SYNC_STAGES@). Must be between 2 and 8 (at most 4 if a channel has a
  -- depth of 16). Only used by 'xpmFifoAxifAsync'.
  , axifResetSynchronizer :: Bool
  -- ^ Synchronize the reset to both clocks before using it
  -- (@EN_RESET_SYNCHRONIZER@). Without it, the macro uses the reset directly
  -- in both clock domains. This setting must be 'True' for 'xpmFifoAxifAsync',
  -- as the reset is part of the subordinate side's domain.
  }
  deriving (Show)

-- | Default configuration, using the macro's default depths and memory types
-- where possible: distributed RAM for the address channels (16 words) and the
-- write response channel (2048 words), and block RAM of the given depths for
-- the write data and read data channels. No status outputs, two CDC stages,
-- and a reset synchronizer.
xpmFifoAxifConfig :: SNat wdchDepth -> SNat rdchDepth -> XpmFifoAxifConfig wdchDepth rdchDepth
xpmFifoAxifConfig wdch rdch = XpmFifoAxifConfig
  { axifWriteAddressDepth = 16
  , axifWriteAddressMemoryType = Distributed
  , axifWriteDataDepth = wdch
  , axifWriteDataMemoryType = Block
  , axifWriteDataStatus = noStatus
  , axifWriteResponseDepth = 2048
  , axifWriteResponseMemoryType = Distributed
  , axifReadAddressDepth = 16
  , axifReadAddressMemoryType = Distributed
  , axifReadDataDepth = rdch
  , axifReadDataMemoryType = Block
  , axifReadDataStatus = noStatus
  , axifCdcSyncStages = 2
  , axifResetSynchronizer = True
  }

-- | Status outputs of 'xpmFifoAxif' and 'xpmFifoAxifAsync'. The write data
-- channel flows from the subordinate side (@s@) to the manager side (@m@), the
-- read data channel the other way around.
data XpmFifoAxifStatus s m wdchDepth rdchDepth = XpmFifoAxifStatus
  { axifWriteDataWriteStatus :: Signal s (XpmFifoWriteStatus wdchDepth)
  -- ^ @prog_full_wdch@, @wr_data_count_wdch@
  , axifWriteDataReadStatus :: Signal m (XpmFifoReadStatus wdchDepth)
  -- ^ @prog_empty_wdch@, @rd_data_count_wdch@
  , axifReadDataWriteStatus :: Signal m (XpmFifoWriteStatus rdchDepth)
  -- ^ @prog_full_rdch@, @wr_data_count_rdch@
  , axifReadDataReadStatus :: Signal s (XpmFifoReadStatus rdchDepth)
  -- ^ @prog_empty_rdch@, @rd_data_count_rdch@
  }

-- | Constraints shared by all wrappers
type AxifConstraints c wdchDepth rdchDepth =
  ( KnownAxi4Config c
  , 1 <= AxiAddrWidth c
  , 1 <= AxiDataBytes c
  , KnownNat wdchDepth
  , 1 <= wdchDepth
  , KnownNat rdchDepth
  , 1 <= rdchDepth
  )

-- | AXI4 FIFO with a common clock, as a 'Circuit'. See 'xpmFifoAxif' for
-- details and a version with status outputs.
xpmFifoAxifC ::
  forall c wdchDepth rdchDepth dom .
  ( AxifConstraints c wdchDepth rdchDepth
  , KnownDomain dom
  , HasCallStack
  ) =>
  XpmFifoAxifConfig wdchDepth rdchDepth ->
  Clock dom ->
  Reset dom ->
  Circuit (Axi4 dom c) (Axi4 dom c)
xpmFifoAxifC cfg clk rst = fromSignals go
 where
  go (sM2S, mS2M) = (sS2M, mM2S)
   where
    (sS2M, mM2S, _) = xpmFifoAxif cfg clk rst sM2S mS2M
{-# INLINE xpmFifoAxifC #-}

-- | AXI4 FIFO with independent clocks, as a 'Circuit'. See
-- 'xpmFifoAxifAsync' for details and a version with status outputs.
xpmFifoAxifAsyncC ::
  forall c wdchDepth rdchDepth s m .
  ( AxifConstraints c wdchDepth rdchDepth
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  XpmFifoAxifConfig wdchDepth rdchDepth ->
  Clock s ->
  Clock m ->
  Reset s ->
  Circuit (Axi4 s c) (Axi4 m c)
xpmFifoAxifAsyncC cfg clkS clkM rst = fromSignals go
 where
  go (sM2S, mS2M) = (sS2M, mM2S)
   where
    (sS2M, mM2S, _) = xpmFifoAxifAsync cfg clkS clkM rst sM2S mS2M
{-# INLINE xpmFifoAxifAsyncC #-}

-- | AXI4 FIFO with a common clock (@CLOCKING_MODE = "common_clock"@).
--
-- Asserting the reset empties all FIFOs. Each FIFO goes through its own reset
-- sequence, during which it does not accept data. The FIFOs also reset
-- themselves after power-on.
--
-- Signals that are zero bits wide (IDs and user signals) are passed to the
-- macro as a single bit, because it does not support zero-width signals.
xpmFifoAxif ::
  forall c wdchDepth rdchDepth dom .
  ( AxifConstraints c wdchDepth rdchDepth
  , KnownDomain dom
  , HasCallStack
  ) =>
  XpmFifoAxifConfig wdchDepth rdchDepth ->
  Clock dom ->
  -- | Reset (@s_aresetn@)
  Reset dom ->
  -- | Subordinate side input (@s_axi_*@), driven by an AXI4 manager
  Signal dom (Axi4M2S c) ->
  -- | Manager side input (@m_axi_*@), driven by an AXI4 subordinate
  Signal dom (Axi4S2M c) ->
  ( Signal dom (Axi4S2M c)
  -- ^ Subordinate side output (@s_axi_*@)
  , Signal dom (Axi4M2S c)
  -- ^ Manager side output (@m_axi_*@)
  , XpmFifoAxifStatus dom dom wdchDepth rdchDepth
  )
xpmFifoAxif cfg clk = xpmFifoAxifImpl True cfg clk clk
{-# INLINE xpmFifoAxif #-}

-- | AXI4 FIFO with independent clocks
-- (@CLOCKING_MODE = "independent_clock"@).
--
-- Asserting the reset empties all FIFOs. The reset is synchronized to both
-- clocks first ('axifResetSynchronizer' must be set). Each FIFO goes through
-- its own reset sequence, during which it does not accept data. The FIFOs also
-- reset themselves after power-on.
--
-- __N.B.__: Right after power-on, the FIFOs that do not use distributed RAM
-- assert their ready signal for one clock cycle while they are still
-- resetting, and drop the data written in that cycle. Do not offer data in the
-- first few cycles after power-on. (The macro masks ready during reset only
-- for distributed RAM.)
--
-- Signals that are zero bits wide (IDs and user signals) are passed to the
-- macro as a single bit, because it does not support zero-width signals.
xpmFifoAxifAsync ::
  forall c wdchDepth rdchDepth s m .
  ( AxifConstraints c wdchDepth rdchDepth
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  XpmFifoAxifConfig wdchDepth rdchDepth ->
  -- | Clock of the subordinate side (@s_aclk@)
  Clock s ->
  -- | Clock of the manager side (@m_aclk@)
  Clock m ->
  -- | Reset (@s_aresetn@)
  Reset s ->
  -- | Subordinate side input (@s_axi_*@), driven by an AXI4 manager
  Signal s (Axi4M2S c) ->
  -- | Manager side input (@m_axi_*@), driven by an AXI4 subordinate
  Signal m (Axi4S2M c) ->
  ( Signal s (Axi4S2M c)
  -- ^ Subordinate side output (@s_axi_*@)
  , Signal m (Axi4M2S c)
  -- ^ Manager side output (@m_axi_*@)
  , XpmFifoAxifStatus s m wdchDepth rdchDepth
  )
xpmFifoAxifAsync = xpmFifoAxifImpl False
{-# INLINE xpmFifoAxifAsync #-}

-- | Pad a signal to at least one bit
pad1 :: forall n . KnownNat n => BitVector n -> BitVector (Max 1 n)
pad1 = resize

-- | Undo 'pad1'
unpad1 :: forall n . KnownNat n => BitVector (Max 1 n) -> BitVector n
unpad1 = resize

xpmFifoAxifImpl ::
  forall c wdchDepth rdchDepth s m .
  ( AxifConstraints c wdchDepth rdchDepth
  , KnownDomain s
  , KnownDomain m
  , HasCallStack
  ) =>
  -- | Common clock?
  Bool ->
  XpmFifoAxifConfig wdchDepth rdchDepth ->
  Clock s ->
  Clock m ->
  Reset s ->
  Signal s (Axi4M2S c) ->
  Signal m (Axi4S2M c) ->
  ( Signal s (Axi4S2M c)
  , Signal m (Axi4M2S c)
  , XpmFifoAxifStatus s m wdchDepth rdchDepth
  )
xpmFifoAxifImpl commonClock XpmFifoAxifConfig{..} clkS clkM rst sM2S mS2M
  | Just err <- configError = clashCompileError err
  | clashSimulation = sim
  | otherwise = synth
 where
  nm = "xpmFifoAxif"
  wdchDepthI = snatToNum axifWriteDataDepth :: Integer
  rdchDepthI = snatToNum axifReadDataDepth :: Integer
  cdc = if commonClock then Nothing else Just axifCdcSyncStages
  depths =
    [ toInteger axifWriteAddressDepth, wdchDepthI, toInteger axifWriteResponseDepth
    , toInteger axifReadAddressDepth, rdchDepthI ]

  configError
    | (err:_) <- [e | Just e <- fmap (\d -> depthCdcConfigError nm d cdc) depths] = Just err
    | Just err <- statusConfigError nm wdchDepthI cdc axifWriteDataStatus = Just err
    | Just err <- statusConfigError nm rdchDepthI cdc axifReadDataStatus = Just err
    | not commonClock && not axifResetSynchronizer =
        Just (nm <> ": independent clocks need axifResetSynchronizer")
    | not commonClock && Ultra `elem` memoryTypes =
        Just (nm <> ": UltraRAM is only supported for a common clock")
    | otherwise = Nothing

  memoryTypes =
    [ axifWriteAddressMemoryType, axifWriteDataMemoryType, axifWriteResponseMemoryType
    , axifReadAddressMemoryType, axifReadDataMemoryType ]

  --------------------------------------------------------------------------
  -- HDL
  --------------------------------------------------------------------------
  synth = (sS2MS, mM2SS, statusS)
   where
    sS2MS = Axi4S2M
      <$> (bitToBool <$> unPort sAwReady)
      <*> (bitToBool <$> unPort sWReady)
      <*> (mkMaybe <$> unPort sBValid <*>
            (Axi4WriteResponse
              <$> (unpad1 <$> unPort sBId)
              <*> (unpack <$> unPort sBResp)
              <*> (unpad1 <$> unPort sBUser)))
      <*> (bitToBool <$> unPort sArReady)
      <*> (mkMaybe <$> unPort sRValid <*>
            (Axi4ReadData
              <$> (unpad1 <$> unPort sRId)
              <*> unPort sRData
              <*> (unpack <$> unPort sRResp)
              <*> (bitToBool <$> unPort sRLast)
              <*> (unpad1 <$> unPort sRUser)))

    mM2SS = Axi4M2S
      <$> (mkMaybe <$> unPort mAwValid <*>
            (Axi4Address
              <$> (unpad1 <$> unPort mAwId) <*> unPort mAwAddr <*> unPort mAwLen
              <*> unPort mAwSize <*> unPort mAwBurst <*> (lsb <$> unPort mAwLock)
              <*> unPort mAwCache <*> unPort mAwProt <*> unPort mAwQos
              <*> unPort mAwRegion <*> (unpad1 <$> unPort mAwUser)))
      <*> (mkMaybe <$> unPort mWValid <*>
            (Axi4WriteData
              <$> unPort mWData <*> unPort mWStrb <*> (bitToBool <$> unPort mWLast)
              <*> (unpad1 <$> unPort mWUser)))
      <*> (bitToBool <$> unPort mBReady)
      <*> (mkMaybe <$> unPort mArValid <*>
            (Axi4Address
              <$> (unpad1 <$> unPort mArId) <*> unPort mArAddr <*> unPort mArLen
              <*> unPort mArSize <*> unPort mArBurst <*> (lsb <$> unPort mArLock)
              <*> unPort mArCache <*> unPort mArProt <*> unPort mArQos
              <*> unPort mArRegion <*> (unpad1 <$> unPort mArUser)))
      <*> (bitToBool <$> unPort mRReady)

    statusS = XpmFifoAxifStatus
      { axifWriteDataWriteStatus =
          XpmFifoWriteStatus <$> (bitToBool <$> unPort progFullWdch) <*> pure False <*> (unpack <$> unPort wrCountWdch)
      , axifWriteDataReadStatus =
          XpmFifoReadStatus <$> (bitToBool <$> unPort progEmptyWdch) <*> pure False <*> (unpack <$> unPort rdCountWdch)
      , axifReadDataWriteStatus =
          XpmFifoWriteStatus <$> (bitToBool <$> unPort progFullRdch) <*> pure False <*> (unpack <$> unPort wrCountRdch)
      , axifReadDataReadStatus =
          XpmFifoReadStatus <$> (bitToBool <$> unPort progEmptyRdch) <*> pure False <*> (unpack <$> unPort rdCountRdch)
      }

    mkMaybe :: Bit -> a -> Maybe a
    mkMaybe v a = if bitToBool v then Just a else Nothing

    sAw = payload (axiAw <$> sM2S)
    sW = payload (axiW <$> sM2S)
    sAr = payload (axiAr <$> sM2S)
    mB = payload (axiB <$> mS2M)
    mR = payload (axiR <$> mS2M)

    valid :: Signal dom (Maybe a) -> Signal dom Bit
    valid = fmap (boolToBit . isJust)

    payload :: NFDataX a => Signal dom (Maybe a) -> Signal dom a
    payload = fmap (fromMaybe (deepErrorX "xpmFifoAxif: no data"))

    lock :: Bit -> BitVector 2
    lock = zeroExtend . pack

    ( sAwReady, sWReady, sBId, sBResp, sBUser, sBValid, sArReady
      , sRId, sRData, sRResp, sRLast, sRUser, sRValid
      , mAwId, mAwAddr, mAwLen, mAwSize, mAwBurst, mAwLock, mAwCache, mAwProt
      , mAwQos, mAwRegion, mAwUser, mAwValid
      , mWData, mWStrb, mWLast, mWUser, mWValid, mBReady
      , mArId, mArAddr, mArLen, mArSize, mArBurst, mArLock, mArCache, mArProt
      , mArQos, mArRegion, mArUser, mArValid, mRReady
      , progFullWdch, progEmptyWdch, wrCountWdch, rdCountWdch
      , progFullRdch, progEmptyRdch, wrCountRdch, rdCountRdch ) = go

    go ::
      ( Port "s_axi_awready" s Bit
      , Port "s_axi_wready" s Bit
      , Port "s_axi_bid" s (BitVector (Max 1 (AxiIdWidth c)))
      , Port "s_axi_bresp" s (BitVector 2)
      , Port "s_axi_buser" s (BitVector (Max 1 (AxiBUserWidth c)))
      , Port "s_axi_bvalid" s Bit
      , Port "s_axi_arready" s Bit
      , Port "s_axi_rid" s (BitVector (Max 1 (AxiIdWidth c)))
      , Port "s_axi_rdata" s (BitVector (AxiDataBytes c * 8))
      , Port "s_axi_rresp" s (BitVector 2)
      , Port "s_axi_rlast" s Bit
      , Port "s_axi_ruser" s (BitVector (Max 1 (AxiRUserWidth c)))
      , Port "s_axi_rvalid" s Bit
      , Port "m_axi_awid" m (BitVector (Max 1 (AxiIdWidth c)))
      , Port "m_axi_awaddr" m (BitVector (AxiAddrWidth c))
      , Port "m_axi_awlen" m (BitVector 8)
      , Port "m_axi_awsize" m (BitVector 3)
      , Port "m_axi_awburst" m (BitVector 2)
      , Port "m_axi_awlock" m (BitVector 2)
      , Port "m_axi_awcache" m (BitVector 4)
      , Port "m_axi_awprot" m (BitVector 3)
      , Port "m_axi_awqos" m (BitVector 4)
      , Port "m_axi_awregion" m (BitVector 4)
      , Port "m_axi_awuser" m (BitVector (Max 1 (AxiAwUserWidth c)))
      , Port "m_axi_awvalid" m Bit
      , Port "m_axi_wdata" m (BitVector (AxiDataBytes c * 8))
      , Port "m_axi_wstrb" m (BitVector (AxiDataBytes c))
      , Port "m_axi_wlast" m Bit
      , Port "m_axi_wuser" m (BitVector (Max 1 (AxiWUserWidth c)))
      , Port "m_axi_wvalid" m Bit
      , Port "m_axi_bready" m Bit
      , Port "m_axi_arid" m (BitVector (Max 1 (AxiIdWidth c)))
      , Port "m_axi_araddr" m (BitVector (AxiAddrWidth c))
      , Port "m_axi_arlen" m (BitVector 8)
      , Port "m_axi_arsize" m (BitVector 3)
      , Port "m_axi_arburst" m (BitVector 2)
      , Port "m_axi_arlock" m (BitVector 2)
      , Port "m_axi_arcache" m (BitVector 4)
      , Port "m_axi_arprot" m (BitVector 3)
      , Port "m_axi_arqos" m (BitVector 4)
      , Port "m_axi_arregion" m (BitVector 4)
      , Port "m_axi_aruser" m (BitVector (Max 1 (AxiArUserWidth c)))
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
        (instConfig "xpm_fifo_axif")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"AXI_ADDR_WIDTH"           @Integer (natToNum @(AxiAddrWidth c)))
        (Param @"AXI_ARUSER_WIDTH"         @Integer (natToNum @(Max 1 (AxiArUserWidth c))))
        (Param @"AXI_AWUSER_WIDTH"         @Integer (natToNum @(Max 1 (AxiAwUserWidth c))))
        (Param @"AXI_BUSER_WIDTH"          @Integer (natToNum @(Max 1 (AxiBUserWidth c))))
        (Param @"AXI_DATA_WIDTH"           @Integer (natToNum @(AxiDataBytes c * 8)))
        (Param @"AXI_ID_WIDTH"             @Integer (natToNum @(Max 1 (AxiIdWidth c))))
        (Param @"AXI_LEN_WIDTH"            @Integer 8)
        (Param @"AXI_RUSER_WIDTH"          @Integer (natToNum @(Max 1 (AxiRUserWidth c))))
        (Param @"AXI_WUSER_WIDTH"          @Integer (natToNum @(Max 1 (AxiWUserWidth c))))
        (Param @"CASCADE_HEIGHT"           @Integer 0)
        (Param @"CDC_SYNC_STAGES"          @Integer (toInteger axifCdcSyncStages))
        (Param @"CLOCKING_MODE"            @String (if commonClock then "common_clock" else "independent_clock"))
        (Param @"ECC_MODE_RDCH"            @String "no_ecc")
        (Param @"ECC_MODE_WDCH"            @String "no_ecc")
        (Param @"EN_RESET_SYNCHRONIZER"    @Integer (boolParam axifResetSynchronizer))
        (Param @"FIFO_DEPTH_RACH"          @Integer (toInteger axifReadAddressDepth))
        (Param @"FIFO_DEPTH_RDCH"          @Integer rdchDepthI)
        (Param @"FIFO_DEPTH_WACH"          @Integer (toInteger axifWriteAddressDepth))
        (Param @"FIFO_DEPTH_WDCH"          @Integer wdchDepthI)
        (Param @"FIFO_DEPTH_WRCH"          @Integer (toInteger axifWriteResponseDepth))
        (Param @"FIFO_MEMORY_TYPE_RACH"    @String (axiMmMemoryTypeParam axifReadAddressMemoryType))
        (Param @"FIFO_MEMORY_TYPE_RDCH"    @String (axiMmMemoryTypeParam axifReadDataMemoryType))
        (Param @"FIFO_MEMORY_TYPE_WACH"    @String (axiMmMemoryTypeParam axifWriteAddressMemoryType))
        (Param @"FIFO_MEMORY_TYPE_WDCH"    @String (axiMmMemoryTypeParam axifWriteDataMemoryType))
        (Param @"FIFO_MEMORY_TYPE_WRCH"    @String (axiMmMemoryTypeParam axifWriteResponseMemoryType))
        (Param @"PACKET_FIFO"              @String "false")
        (Param @"PROG_EMPTY_THRESH_RDCH"   @Integer (toInteger (fromMaybe 10 (statusProgEmptyThresh axifReadDataStatus))))
        (Param @"PROG_EMPTY_THRESH_WDCH"   @Integer (toInteger (fromMaybe 10 (statusProgEmptyThresh axifWriteDataStatus))))
        (Param @"PROG_FULL_THRESH_RDCH"    @Integer (toInteger (fromMaybe 10 (statusProgFullThresh axifReadDataStatus))))
        (Param @"PROG_FULL_THRESH_WDCH"    @Integer (toInteger (fromMaybe 10 (statusProgFullThresh axifWriteDataStatus))))
        (Param @"RD_DATA_COUNT_WIDTH_RDCH" @Integer (natToNum @(CLog 2 rdchDepth + 1)))
        (Param @"RD_DATA_COUNT_WIDTH_WDCH" @Integer (natToNum @(CLog 2 wdchDepth + 1)))
        (Param @"SIM_ASSERT_CHK"           @Integer 0)
        (Param @"USE_ADV_FEATURES_RDCH"    @String (statusAdvFeatures axifReadDataStatus False False False))
        (Param @"USE_ADV_FEATURES_WDCH"    @String (statusAdvFeatures axifWriteDataStatus False False False))
        (Param @"WR_DATA_COUNT_WIDTH_RDCH" @Integer (natToNum @(CLog 2 rdchDepth + 1)))
        (Param @"WR_DATA_COUNT_WIDTH_WDCH" @Integer (natToNum @(CLog 2 wdchDepth + 1)))

        (ClockPort @"m_aclk" clkM)
        (ClockPort @"s_aclk" clkS)
        (ResetPort @"s_aresetn" @'ActiveLow rst)

        (Port @"s_axi_awid" (pad1 . axiId <$> sAw))
        (Port @"s_axi_awaddr" (axiAddr <$> sAw))
        (Port @"s_axi_awlen" (axiLen <$> sAw))
        (Port @"s_axi_awsize" (axiSize <$> sAw))
        (Port @"s_axi_awburst" (axiBurst <$> sAw))
        (Port @"s_axi_awlock" (lock . axiLock <$> sAw))
        (Port @"s_axi_awcache" (axiCache <$> sAw))
        (Port @"s_axi_awprot" (axiProt <$> sAw))
        (Port @"s_axi_awqos" (axiQos <$> sAw))
        (Port @"s_axi_awregion" (axiRegion <$> sAw))
        (Port @"s_axi_awuser" (pad1 . axiUser <$> sAw))
        (Port @"s_axi_awvalid" (valid (axiAw <$> sM2S)))
        (Port @"s_axi_wdata" (axiWData <$> sW))
        (Port @"s_axi_wstrb" (axiWStrb <$> sW))
        (Port @"s_axi_wlast" (boolToBit . axiWLast <$> sW))
        (Port @"s_axi_wuser" (pad1 . axiWUser <$> sW))
        (Port @"s_axi_wvalid" (valid (axiW <$> sM2S)))
        (Port @"s_axi_bready" (boolToBit . axiBReady <$> sM2S))
        (Port @"s_axi_arid" (pad1 . axiId <$> sAr))
        (Port @"s_axi_araddr" (axiAddr <$> sAr))
        (Port @"s_axi_arlen" (axiLen <$> sAr))
        (Port @"s_axi_arsize" (axiSize <$> sAr))
        (Port @"s_axi_arburst" (axiBurst <$> sAr))
        (Port @"s_axi_arlock" (lock . axiLock <$> sAr))
        (Port @"s_axi_arcache" (axiCache <$> sAr))
        (Port @"s_axi_arprot" (axiProt <$> sAr))
        (Port @"s_axi_arqos" (axiQos <$> sAr))
        (Port @"s_axi_arregion" (axiRegion <$> sAr))
        (Port @"s_axi_aruser" (pad1 . axiUser <$> sAr))
        (Port @"s_axi_arvalid" (valid (axiAr <$> sM2S)))
        (Port @"s_axi_rready" (boolToBit . axiRReady <$> sM2S))

        (Port @"m_axi_awready" (boolToBit . axiAwReady <$> mS2M))
        (Port @"m_axi_wready" (boolToBit . axiWReady <$> mS2M))
        (Port @"m_axi_bid" (pad1 . axiBId <$> mB))
        (Port @"m_axi_bresp" (pack . axiBResp <$> mB))
        (Port @"m_axi_buser" (pad1 . axiBUser <$> mB))
        (Port @"m_axi_bvalid" (valid (axiB <$> mS2M)))
        (Port @"m_axi_arready" (boolToBit . axiArReady <$> mS2M))
        (Port @"m_axi_rid" (pad1 . axiRId <$> mR))
        (Port @"m_axi_rdata" (axiRData <$> mR))
        (Port @"m_axi_rresp" (pack . axiRResp <$> mR))
        (Port @"m_axi_rlast" (boolToBit . axiRLast <$> mR))
        (Port @"m_axi_ruser" (pad1 . axiRUser <$> mR))
        (Port @"m_axi_rvalid" (valid (axiR <$> mS2M)))

        (Port @"injectsbiterr_wdch" (pure low :: Signal s Bit))
        (Port @"injectdbiterr_wdch" (pure low :: Signal s Bit))
        (Port @"injectsbiterr_rdch" (pure low :: Signal m Bit))
        (Port @"injectdbiterr_rdch" (pure low :: Signal m Bit))

  --------------------------------------------------------------------------
  -- Simulation
  --------------------------------------------------------------------------
  sim = (sS2MM, mM2SM, statusM)
   where
    cdcI = fromIntegral axifCdcSyncStages :: Int
    cdcN = axifCdcSyncStages

    -- Resets in both clock domains: either synchronized (xpm_cdc_sync_rst with
    -- INIT = 0) or used directly.
    rstHigh = unsafeToActiveHigh rst
    (rstS, rstM)
      | axifResetSynchronizer =
          ( syncStages syncStagesN False clkS clkS rstHigh
          , syncStages syncStagesN False clkS clkM rstHigh )
      | otherwise = (rstHigh, unsafeSynchronizer clkS clkM rstHigh)
     where
      syncStagesN = if commonClock then 4 else cdcI

    cfg depth = channelConfig commonClock cdcN depth noStatus

    (awReady, mAw, _, _) =
      axiChannel axifWriteAddressMemoryType (cfg axifWriteAddressDepth) clkS clkM rstS (axiAw <$> sM2S) (axiAwReady <$> mS2M)
    (wReady, mW, wdchWr, wdchRd) =
      axiChannel axifWriteDataMemoryType (channelConfig commonClock cdcN (snatToNum axifWriteDataDepth) axifWriteDataStatus)
        clkS clkM rstS (axiW <$> sM2S) (axiWReady <$> mS2M)
    (bReady, sB, _, _) =
      axiChannel axifWriteResponseMemoryType (cfg axifWriteResponseDepth) clkM clkS rstM (axiB <$> mS2M) (axiBReady <$> sM2S)
    (arReady, mAr, _, _) =
      axiChannel axifReadAddressMemoryType (cfg axifReadAddressDepth) clkS clkM rstS (axiAr <$> sM2S) (axiArReady <$> mS2M)
    (rReady, sR, rdchWr, rdchRd) =
      axiChannel axifReadDataMemoryType (channelConfig commonClock cdcN (snatToNum axifReadDataDepth) axifReadDataStatus)
        clkM clkS rstM (axiR <$> mS2M) (axiRReady <$> sM2S)

    sS2MM = Axi4S2M <$> awReady <*> wReady <*> sB <*> arReady <*> sR
    mM2SM = Axi4M2S <$> mAw <*> mW <*> bReady <*> mAr <*> rReady

    statusM = XpmFifoAxifStatus
      { axifWriteDataWriteStatus = toWriteStatus <$> wdchWr
      , axifWriteDataReadStatus = toReadStatus <$> wdchRd
      , axifReadDataWriteStatus = toWriteStatus <$> rdchWr
      , axifReadDataReadStatus = toReadStatus <$> rdchRd
      }
{-# INLINE xpmFifoAxifImpl #-}
