{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.RecordProcessor where

import Clash.Prelude

import Clash.Cores.Etherbone.Base

import Protocols
import Protocols.PacketStream
import qualified Protocols.Df as Df

import qualified Data.Bifunctor as B
import Data.Maybe
import Control.DeepSeq (NFData)


-- | This is the data sent over the bypass line.
-- It is not assured that the data on the bypass line is in sync with the data
-- returned from the WishboneMaster. This means that the RecordBuilder should
-- save the header and base address in its state.
--
-- By using a bypass line the state machine of the WishboneMaster can be made
-- simpler and the RecordBuilder can know about the number of reads and writes
-- before an operation is finished.
data Bypass addrWidth = Bypass
  -- | The @RecordHeader@ from the @_meta@ field.
  { _bpHeader :: RecordHeader
  -- | The @BaseRetAddr@ that needs to be returned if a read operation has is
  -- sent.
  , _bpBase   :: Maybe (BitVector addrWidth)
  -- | The @_abort@ signal from the PacketStream. If this is @True@, no new data
  -- will come from the @WishboneMaster@ and the @RecordBuilder@ should be set
  -- back into its initial state.
  , _bpAbort  :: Bool
  } deriving (Generic, NFDataX, NFData, Show, ShowX, Eq)

data RecordProcessorState addrWidth
  -- | Initial state of the @RecordProcessor@.
  -- In this state either a @BaseWriteAddr@ or a @BaseRetAddr@ is received. The
  -- @_wCount@ and @_rCount@ fields determine whether the next state is @Write@
  -- or @Read@.
  = WriteOrReadAddr
  -- | State to handle Write operations.
  -- When @_writesLeft == 1@ the state progresses to @ReadAddr@ or back to
  -- @WriteOrReadAddr@.
  | Write { _writesLeft :: Unsigned 8
          -- ^ Number of write operations left to fullfill.
          , _addr :: BitVector addrWidth
          -- ^ The (incrementing) address to write to.
          }
  -- | Handle the @BaseRetAddr@. This is sent over the Bypass line.
  | ReadAddr
  -- | Handle Read operations.
  -- When @_readsLeft == 1@ the state is set back to @WriteOrReadAddr@.
  | Read  { _readsLeft :: Unsigned 8 }
  | WaitForLast
  deriving (Show, Generic, ShowX, NFDataX)

recordProcessorT :: forall dataWidth addrWidth dat selWidth .
  ( KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  , Show dat
  , selWidth ~ dataWidth)
  => RecordProcessorState addrWidth
  -> ( Maybe (PacketStreamM2S dataWidth RecordHeader)
     , ((), Ack)
     )
  -> ( RecordProcessorState addrWidth
     , ( PacketStreamS2M
       , ( Maybe (Bypass addrWidth)
         , Df.Data (WishboneOperation addrWidth selWidth dat)
         )
       )
     )
-- No data in -> no data out
recordProcessorT state (Nothing, _)
  = (state, (PacketStreamS2M True, (Nothing, Df.NoData)))
-- If in the initial state and abort is asserted, stay in this state.
recordProcessorT WriteOrReadAddr (Just PacketStreamM2S{_abort=True}, _)
  = (WriteOrReadAddr, (PacketStreamS2M True, (Nothing, Df.NoData)))
recordProcessorT state (Just psFwd, ((), Ack wbAck))
  = (nextState, (PacketStreamS2M psBwd, (bpOut, wbOut)))
  where
    nextState
      | ack       = state'
      | otherwise = state
    state' = fsm state psFwd

    psWord = pack $ _data psFwd
    hdr = _meta psFwd

    -- Cannot read the Ack channels if the forward channels are NoData. 
    ack = case state of
      WriteOrReadAddr -> True
      ReadAddr        -> True
      _               -> wbAck
    psBwd = ack

    -- Only write wishbone operations in the @Write@ or @Read@ state
    wbOut = case state of
      Write i a -> Df.Data (WishboneOperation a (Just dat) sel isLast abort (dropCyc $ i + _rCount hdr) wca)
      Read i    -> Df.Data (WishboneOperation addr Nothing sel isLast abort (dropCyc i) rca)
      _         -> Df.NoData
      where
        dat = bitCoerce $ _data psFwd
        isLast = isJust $ _last psFwd
        sel = resize $ _byteEn hdr
        abort = _abort psFwd

        -- Drop @CYC@ if this was requested. Also drop if this is the last
        -- fragment of the packet.
        -- NOTE: With no support for multiple records yet, this means that @CYC@
        -- is set low at the end of each Etherbone packet.
        dropCyc left = isJust (_last psFwd) || (left == 1 && _cyc hdr)

        addr = resize psWord

        wca = if _wca hdr then ConfigAddressSpace else WishboneAddressSpace
        rca = if _rca hdr then ConfigAddressSpace else WishboneAddressSpace

    -- The bypass line always receives data if data is streaming in.
    bpOut = Just $ Bypass hdr base (_abort psFwd)
      where
        base = case (state, state') of
          (WriteOrReadAddr, Read _) -> Just $ resize psWord
          (ReadAddr, _)             -> Just $ resize psWord
          _ -> Nothing

    fsm
      :: RecordProcessorState addrWidth
      -> PacketStreamM2S dataWidth RecordHeader
      -> RecordProcessorState addrWidth
    -- If this is the last fragment of the packet, jump back to the initial
    -- state.
    fsm _ PacketStreamM2S{_last}
      | _last == Just maxBound = WriteOrReadAddr
    fsm st@WriteOrReadAddr PacketStreamM2S{..} = st'
      where
        wCount = _wCount _meta
        rCount = _rCount _meta

        st'
          | wCount > 0 = Write wCount $ resize psWord
          | rCount > 0 = Read rCount
          | otherwise  = st
    fsm Write{..} PacketStreamM2S{..} = st'
      where
        wCount = _writesLeft
        wCount' = wCount - 1
        rCount = _rCount _meta

        addr'
          -- If @wff@ is set, the write address is a FIFO and should not
          -- increment.
          | _wff _meta = _addr
          | otherwise  = _addr + (natToNum @dataWidth)
        
        st'
          | wCount' == 0 =
            if rCount > 0
              then ReadAddr
              else WriteOrReadAddr
          | otherwise = Write wCount' addr'
    fsm ReadAddr PacketStreamM2S{..} = Read rCount
      where
        rCount = _rCount _meta
    fsm Read{..} PacketStreamM2S{} = st'
      where
        rCount = _readsLeft
        rCount' = rCount - 1

        st'
          | rCount' == 0 = WaitForLast
          | otherwise    = Read rCount'
    -- If there is more data than indicated in the header, the data is dropped.
    -- When @_last@ is set, the state is set to @WriteOrReadAddr@
    fsm WaitForLast _ = WaitForLast


-- | Converts a @PacketStream@ of a Record into separate operations for the
-- @WishboneMaster@.
-- It also bypasses the @WishboneMaster@ with the @RecordHeader@ so that the
-- @RecordBuilder@ does not have to wait for the wishbone bus to start
-- constructing the response. The bypass line also holds the @BaseRetAddr@
-- required in the response in the case of reads, and an abort signal.
--
-- This circuit makes use of the fact that the depacketizers set @_abort@ for
-- the rest of a packet if it was toggled. This means that there is no need to
-- have an additional state to handle aborts.
--
-- This circuit assumes that @_last@ is set to @maxBound@ with the final
-- fragment of a packet.
recordProcessorC :: forall dom dataWidth addrWidth dat selWidth .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , Show dat
  , BitSize dat ~ dataWidth * 8
  , selWidth ~ dataWidth
  )
  => Circuit (PacketStream dom dataWidth RecordHeader)
             (CSignal dom (Maybe (Bypass addrWidth)), Df.Df dom (WishboneOperation addrWidth selWidth dat))
recordProcessorC = forceResetSanity |> Circuit (B.second unbundle . fsm . B.second bundle)
  where
    fsm = mealyB recordProcessorT WriteOrReadAddr
