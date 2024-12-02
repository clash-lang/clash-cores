{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Clash.Cores.Etherbone.Base where

import Clash.Prelude
import Control.DeepSeq (NFData)
import Data.Maybe
import Protocols
import Protocols.PacketStream

etherboneVersion :: Natural
etherboneVersion = 1

etherboneMagic :: BitVector 16
etherboneMagic = 0x4e6f

-- | Etherbone packet header
-- For more information about the individual fields see the Etherbone spec.
data EBHeader = EBHeader
  { _magic     :: BitVector 16
  , _version   :: BitVector 4
  , _res       :: Bit
  -- | No-Reads flag. Indicates that there are no reads in this etherbone
  -- packet.
  , _noReads   :: Bool
  -- | Probe-Response flag. Indicates that this packet is a probe response.
  , _probeResp :: Bool
  -- | Probe-Flag. Indicates that this packet is a etherbone probe.
  , _probeFlag :: Bool
  -- | Width of address fields
  -- This and the @_portSize@ are bitflag fields formatted as {8, 16, 32, 64}
  -- wide corresponding to {1, 2, 4, 8} in the vector. See 'sizeMask'.
  , _addrSize  :: BitVector 4
  -- | Width of data bus
  , _portSize  :: BitVector 4
  } deriving (Generic, BitPack, NFDataX, NFData, Show, ShowX, Eq)

-- | Header for each Record in an Etherbone packet.
-- For more information see the Etherbone spec.
data RecordHeader = RecordHeader
  -- | Whether the @BaseRetAddr@ for the read values is in config space of the
  -- originating device.
  { _baseConfigAddr  :: Bool
  -- | The read operations are meant for the config space.
  , _readConfigAddr  :: Bool
  -- | Read results should be written to a FIFO at BaseRetAddr at the remote.
  , _readFifo        :: Bool
  , _res0            :: Bit
  -- | Whether the @CYC@ line should be dropped after this record.
  , _cyc             :: Bool
  -- | The write operations are meant for the config space.
  , _writeConfigAddr :: Bool
  -- | Write to a local FIFO
  , _writeFifo       :: Bool
  , _res1            :: Bit
  -- | Wishbone byte enable field
  , _byteEn          :: BitVector 8
  -- | Number of write entries in this record
  , _wCount          :: Unsigned 8
  -- | Number of read entries in this record
  , _rCount          :: Unsigned 8
  } deriving (Generic, BitPack, NFDataX, NFData, Show, ShowX, Eq)

-- | The address space a WishboneOperation is meant for.
data AddressSpace = WishboneAddressSpace | ConfigAddressSpace
  deriving (Generic, Show, ShowX, NFDataX, NFData, Eq)

-- | Input data for 'wishboneMasterC' and 'configMasterC'
data WishboneOperation addrWidth selWidth dat
  = WishboneOperation
  { _opAddr      :: BitVector addrWidth
  -- | Input data. This determines whether the operation is a read or a write.
  -- Read for @Nothing@, write for @Just@.
  , _opDat       :: Maybe dat
  -- | Wishbone @sel@ field
  , _opSel       :: BitVector selWidth
  -- | Indicates whether the @busCycle@ line should be dropped after the
  -- operation.
  , _opDropCyc   :: Bool
  -- | The address space this operation should act on.
  , _opAddrSpace :: AddressSpace
  -- | End-of-record. Indicates whether the operation is the last of this record.
  , _opEOR       :: Bool
  -- | End-of-packet. Indicates whether the operation is the last of the etherbone packet.
  , _opEOP       :: Bool
  -- | This is set if the @_abort@ bit is set in the incoming @PacketStream@.
  -- The state machine should stop handling data and be kept in the initial
  -- state while this is set.
  -- It is assumed that this bit is set until the @_last@ of the packet was set.
  -- This is the default behavior of 'depacketizerC'.
  -- The master circuits should return __no Data__ while this is set.
  , _opAbort     :: Bool
  } deriving (Generic, Show, ShowX, NFDataX, NFData, Eq)

-- | Output data from 'wishboneMasterC' and 'configMasterC'
data WishboneResult dat
  = WishboneResult
  -- | @Nothing@ for a write and @Just dat@ for a read.
  -- This can be used to wait for all operations to be finished, including
  -- writes, before sending the response. The default behaviour for now is to
  -- not wait for all writes to finish.
  { _resDat  :: Maybe dat
  -- | Forwarded End-of-record flag
  , _resEOR  :: Bool
  -- | Forwarded End-of-packet flag
  , _resEOP  :: Bool
  } deriving (Generic, Show, NFDataX, ShowX, NFData, Eq)

-- | The data sent over the bypass line.
-- It is not assured that the data on the bypass line is in sync with the data
-- returned from the WishboneMaster. This means that the RecordBuilder should
-- save the header and base address in its state.
--
-- By using a bypass line the state machine of the WishboneMaster can be made
-- simpler and the RecordBuilder can know about the number of reads and writes
-- before an operation is finished.
data Bypass addrWidth = Bypass
  -- | The 'RecordHeader' from the @_meta@ field.
  { _bpHeader :: RecordHeader
  -- | The @BaseRetAddr@ that needs to be returned if a read operation has is
  -- sent.
  , _bpBase   :: Maybe (BitVector addrWidth)
  -- | The @_abort@ signal from the PacketStream. If this is @True@, no new data
  -- will come from 'wishboneMasterC' and 'recordBuilderC' should be set back
  -- into its initial state.
  , _bpAbort  :: Bool
  } deriving (Generic, NFDataX, NFData, Show, ShowX, Eq)

type ByteSize dat = DivRU (BitSize dat) 8

-- | Extract 'EBHeader' data from a @PacketStream@ into the metadata.
-- With a 64-bit bus this shifts the data-stream by 4 bytes.
etherboneDepacketizerC :: forall dom dataWidth .
 ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
 => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EBHeader)
etherboneDepacketizerC = depacketizerC const

-- | Concat 'EBHeader' metadata back into the data stream. This gives
-- backpressure for one cycle.
etherbonePacketizerC :: forall dom dataWidth .
  ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth ())
etherbonePacketizerC = packetizerC (const ()) id

-- | Strips 4 bytes from the start of a packet in the case of a 64-bit wishbone
-- bus and passes the input for a 32-bit bus.
-- This is needed to get rid of the 'potential padding' in the case of a 64-bit
-- bus.
etherbonePaddingStripperC :: forall dom dataWidth .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 4 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth EBHeader)
etherbonePaddingStripperC = case compareSNat (SNat @dataWidth) d4 of
  SNatLE -> idC
  SNatGT -> depacketizerC metaMap
  where
    metaMap :: Vec (dataWidth-4) (BitVector 8) -> EBHeader -> EBHeader
    metaMap _ hdr = hdr

-- | Adds 4 bytes of padding to the start of a packet in the case of a 64-bit
-- wishbone bus. It passes the input for a 32-bit bus.
-- Needed to apply the 'potential padding' in the case of a 64-bit wishbone bus.
etherbonePaddingAdderC :: forall dom dataWidth .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , 4 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth EBHeader)
etherbonePaddingAdderC = case compareSNat (SNat @dataWidth) d4 of
  SNatLE -> idC
  SNatGT -> packetizerC id (const $ repeat @(dataWidth-4) (0 :: BitVector 8))

-- | Extract 'RecordHeader' data from a @PacketStream@ into the metadata. Strips
-- potential padding from the header. Splits an Etherbone packet @PacketStream@
-- into separate Record @PacketStream@s. The added bool in the metadata
-- indicates end-of-packet.
--
-- 'EBHeader' can be dropped. All the info in it is static and known.
recordDepacketizerC :: forall dom dataWidth .
  ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth (Bool, RecordHeader))
recordDepacketizerC = recordSplitterC |> depacketizerC metaMap
  where
    metaMap
      :: (RecordHeader, Vec (dataWidth - 4) (BitVector 8))
      -> Bool
      -> (Bool, RecordHeader)
    metaMap hdr b = (b, fst hdr)

    -- This circuit splits a Etherbone @PacketStream@ into separate Record
    -- streams. The @_last@ field indicates a end-of-record.
    -- The metadata is replaced by a Bool indicating end-of-packet.
    recordSplitterC = Circuit $ mealyB recordSplitterT 0
    recordSplitterT
      -- | Counter of the number of words left in a record. A record can hold at
      -- most 256 write and 256 read operations. So, with the base addresses, at
      -- most 514 words. Rounded up, this requires 10 bits.
      :: Unsigned 10
      -> (Maybe (PacketStreamM2S dataWidth meta), PacketStreamS2M)
      -> (Unsigned 10, (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth Bool)))
    recordSplitterT 0 (Nothing, _) = (0, (PacketStreamS2M True, Nothing))
    recordSplitterT 0 (Just ps, oBwd) = (st', (oBwd, Just ps {_meta = False}))
      where
        wCount = bitCoerce $ resize $ _data ps !! (2 :: Integer)
        rCount = bitCoerce $ resize $ _data ps !! (3 :: Integer)
        wCount'
          | wCount > 0 = wCount + 1
          | otherwise  = 0
        rCount'
          | rCount > 0 = rCount + 1
          | otherwise  = 0
        st'
          | _ready oBwd = wCount' + rCount'
          | otherwise   = 0
    recordSplitterT left (Nothing, _) = (left, (PacketStreamS2M True, Nothing))
    recordSplitterT left (Just ps, oBwd) = (st', (oBwd, oFwd))
      where
        left'
          | isJust $ _last ps = 0
          | otherwise         = left - 1

        st'
          | _ready oBwd = left'
          | otherwise   = left

        oFwd = Just $ ps { _last = lst, _meta = isJust $ _last ps }
        lst = if left' == 0 then Just maxBound else Nothing

-- | Helper function to convert a dataWidth in bytes to the correct format for
-- the @_addrSize@ and @_portSize@ fields in the 'EBHeader'.
sizeMask :: Integer -> BitVector 4
sizeMask n
  | n == 1    = 0b0001
  | n == 2    = 0b0010
  | n == 4    = 0b0100
  | n == 8    = 0b1000
  | n > 8     = errorX "Etherbone v1 only supports up to 64-bit busses."
  | otherwise = errorX "Only byte multiples of 2 are supported."

-- | A dispatcher that validates incoming packets and dispatches them to the
-- appropriate path. Either to the @ProbeHandler@ or to the @RecordHandler@.
receiverC :: forall dom dataWidth addrWidth .
  ( HiddenClockResetEnable dom, KnownNat dataWidth, KnownNat addrWidth )
  => SNat addrWidth
  -> Circuit (PacketStream dom dataWidth EBHeader)
             (Vec 2 (PacketStream dom dataWidth EBHeader))
receiverC SNat = packetDispatcherC (probePredicate :> recordPredicate :> Nil)
  where
    isValid = and . sequenceA [validMagic, validVersion, validAddr, validPort]

    validMagic    = (== etherboneMagic) . _magic
    validVersion  = (== fromIntegral etherboneVersion) . _version
    validAddr     = (/= 0) . (.&. addrSizeMask) . _addrSize
    validPort     = (/= 0) . (.&. portSizeMask) . _portSize

    probePredicate m = isValid m && _probeFlag m
    recordPredicate = isValid

    portSizeMask = sizeMask $ natToInteger @dataWidth
    addrSizeMask = sizeMask $ natToInteger @(Div addrWidth 8)

-- | The handler for Probe packets.
-- Sets the appropriate header flags for a probe response, the Etherbone
-- device's bus sizes and forwards the probe-id.
--
-- On a 32-bit bus the probe-id fits in a whole word. With a 64-bit bus it is
-- half a word. So this circuit returns a @_last = Just 4@ for both bus sizes.
-- Later components need to be able to handle this.
probeHandlerC :: forall dom dataWidth addrWidth .
  ( KnownNat dataWidth, KnownNat addrWidth )
  -- | Address width
  => SNat addrWidth
  -> Circuit
  (PacketStream dom dataWidth EBHeader)
  (PacketStream dom dataWidth EBHeader)
probeHandlerC SNat = mapMeta metaMap
  where
    metaMap m = m { _probeFlag = False, _probeResp = True, _noReads = True
                  , _addrSize = _addrSize m .&. addrSizeMask
                  , _portSize = _portSize m .&. portSizeMask
                  }

    portSizeMask = sizeMask $ natToInteger @dataWidth
    addrSizeMask = sizeMask $ natToInteger @(Div addrWidth 8)
{-# OPAQUE probeHandlerC #-}
