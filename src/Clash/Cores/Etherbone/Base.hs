{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Clash.Cores.Etherbone.Base where

import Clash.Prelude
import Protocols
import Protocols.PacketStream
import Debug.Trace
import qualified Protocols.Df as Df
import Control.DeepSeq (NFData)

etherboneVersion :: Natural
etherboneVersion = 1

etherboneMagic :: Unsigned 16
etherboneMagic = 0x4e6f

data EBHeader = EBHeader
  { _magic    :: Unsigned 16
  , _version  :: Unsigned 4
  , _res      :: Bit
  , _nr       :: Bool
  , _pr       :: Bool
  , _pf       :: Bool
  , _addrSize :: BitVector 4
  , _portSize :: BitVector 4
  } deriving (Generic, BitPack, NFDataX, Show, ShowX)

data RecordHeader = RecordHeader
  { _bca    :: Bool
  , _rca    :: Bool
  , _rff    :: Bool
  , _res0   :: Bit
  , _cyc    :: Bool
  , _wca    :: Bool
  , _wff    :: Bool
  , _res1   :: Bit
  , _byteEn :: BitVector 8
  , _wCount :: Unsigned 8
  , _rCount :: Unsigned 8
  } deriving (Generic, BitPack, NFDataX, NFData, Show, ShowX, Eq)

data AddressSpace = WishboneAddressSpace | ConfigAddressSpace
  deriving (Generic, Show, ShowX, NFDataX, NFData, Eq)

-- | Input data for @wishboneMasterC@ and @configMasterC@
data WishboneOperation addrWidth selWidth dat
  = WishboneOperation
  { _opAddr   :: BitVector addrWidth
  -- | Input data. This determines whether the operation is a read or a write.
  -- Read for @Nothing@, write for @Just@.
  , _opDat    :: Maybe dat
  , _opSel    :: BitVector selWidth
  -- | Indicates whether the operation is the last of this record.
  -- Since there is no support for multi-record EB packets this also indicates
  -- the end of the packet (for now).
  , _opLast   :: Bool
  -- | This is set if the @_abort@ bit is set in the incoming @PacketStream@.
  -- The state machine should stop handling data and be kept in the initial
  -- state while this is set.
  -- It is assumed that this bit is set until the @_last@ of the packet was set.
  -- This is the default behaviour of @depacketizerC@.
  , _opAbort  :: Bool
  -- | Indicates whether the @busCycle@ line should be dropped after the
  -- operation.
  , _dropCyc  :: Bool
  -- | The address space this operation should act on. 
  , _addrSpace :: AddressSpace
  } deriving (Generic, Show, ShowX, NFDataX, NFData, Eq)

-- | Output data from @wishboneMasterC@ and @configMasterC@
data WishboneResult dat
  = WishboneResult
  -- | @Nothing@ for a write and @Just dat@ for a read.
  -- This can be used to wait for all operations to be finished, including
  -- writes, before sending the response. The default behaviour for now is to
  -- not wait for all writes to finish.
  { _resDat  :: Maybe dat
  -- | Forwarded @_last@ signal. Indicates that this is the last (finished)
  -- operation from this packet.
  , _resLast :: Bool
  } deriving (Generic, Show)

-- | Extract @EBHeader@ data from a @PacketStream@ into the metadata.
etherboneDepacketizerC :: forall dom dataWidth .
 ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
 => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EBHeader)
etherboneDepacketizerC = depacketizerC @_ metaMap
  where
    metaMap :: (EBHeader, Vec (dataWidth - 4) (BitVector 8)) -> () -> EBHeader
    metaMap hdr _ = fst hdr

-- | Concat @EBHeader@ metadata back into the data stream. This gives
-- backpressure for one cycle.
etherbonePacketizerC :: forall dom dataWidth .
  ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth ())
etherbonePacketizerC = packetizerC (const ()) metaMap
  where
    metaMap :: EBHeader -> (EBHeader, Vec (dataWidth - 4) (BitVector 8))
    metaMap hdr = (hdr, repeat 0)

-- | Extract @RecordHeader@ data from a @PacketStream@ into the metadata.
-- @EBHeader@ can be dropped. All the info in it is static and known.
recordDepacketizerC :: forall dom dataWidth . 
  ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth RecordHeader)
recordDepacketizerC = depacketizerC metaMap
  where
    metaMap :: (RecordHeader, Vec (dataWidth - 4) (BitVector 8)) -> EBHeader -> RecordHeader
    metaMap hdr _ = fst hdr

-- | Helper function to convert a dataWidth in bytes to the correct format for
-- the @_addrSize@ and @_portSize@ fields in the @EBHeader@.
sizeMask :: Integer -> BitVector 4
sizeMask n
  | n <= 2    = fromInteger n
  | n <= 4    = 0b0100
  | n <= 8    = 0b1000
  | otherwise = error "Etherbone v1 only supports up to 64-bit busses."

-- | A dispatcher that validates incoming packets and dispatches them to the
-- appropriate path. Either to the @ProbeHandler@ or to the @RecordHandler@.
receiverC :: forall dom dataWidth addrWidth . 
  ( HiddenClockResetEnable dom, KnownNat dataWidth, KnownNat addrWidth )
  => SNat addrWidth
  -> Circuit (PacketStream dom dataWidth EBHeader)
             (Vec 2 (PacketStream dom dataWidth EBHeader))
receiverC SNat = packetDispatcherC (probePred :> recordPred :> Nil)
  where
    isValid = and . sequenceA [validMagic, validVersion, validAddr, validPort]

    validMagic    = (== etherboneMagic) . _magic
    validVersion  = (== fromIntegral etherboneVersion) . _version
    validAddr     = (/= 0) . (.&. addrSizeMask) . _addrSize
    validPort     = (/= 0) . (.&. portSizeMask) . _portSize

    isProbe       = _pf

    probePred m = isValid m && isProbe m
    recordPred = isValid

    portSizeMask = sizeMask $ natToInteger @dataWidth
    addrSizeMask = sizeMask $ natToInteger @(Div addrWidth 8)

-- Probe is packet _always_ padded to max alignment. Only header and no records
-- | The handler for Probe packets.
-- Sets the appropriate header flags for a probe response, the EB device's
-- bus sizes and forwards the probe-id.
probeHandlerC :: forall dom dataWidth addrWidth .
  ( KnownNat dataWidth, KnownNat addrWidth )
  => SNat addrWidth
  -> Circuit
  (PacketStream dom dataWidth EBHeader)
  (PacketStream dom dataWidth EBHeader)
probeHandlerC SNat = mapMeta metaMap
  where
    metaMap m = m { _pf = False, _pr = True, _nr = True
                  , _addrSize = _addrSize m .&. addrSizeMask
                  , _portSize = _portSize m .&. portSizeMask
                  }

    portSizeMask = sizeMask $ natToInteger @dataWidth
    addrSizeMask = sizeMask $ natToInteger @(Div addrWidth 8)

-- arbiterC :: (HiddenClockResetEnable dom, KnownNat n, 1 <= n) =>
--   Circuit (Vec n (PacketStream dom dataWidth meta)) (PacketStream dom dataWidth meta)
-- arbiterC = packetArbiterC RoundRobin

-- TODO: Remove this from the library
traceC :: forall dom dataWidth meta .
  (HiddenClockResetEnable dom, Show meta)
  => String
  -> Circuit (PacketStream dom dataWidth meta)
             (PacketStream dom dataWidth meta)
traceC name = Circuit go
  where
    go (fwd, bwd) = (printf "<<" <$> bwd <*> counter, printf ">>" <$> fwd <*> counter)

    counter = register @dom (0 :: Integer) (counter + 1)

    printf :: (Show a, Show b) => String -> a -> b -> a
    printf dir f c = trace (dir <> " " <> show c <> " " <> name <> " " <> ": " <> show f) f
    -- printf _ f _ = f
