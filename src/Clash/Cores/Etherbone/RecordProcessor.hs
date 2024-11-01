{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.RecordProcessor where

import Clash.Cores.Etherbone.Base
import Clash.Prelude
import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream

import qualified Data.Bifunctor as B
import Data.Maybe

data RecordProcessorState addrWidth
  -- | Initial state of the @RecordProcessor@.
  -- In this state either a @BaseWriteAddr@ or a @BaseRetAddr@ is received. The
  -- @_wCount@ and @_rCount@ fields determine whether the next state is @Write@
  -- or @Read@.
  = WriteOrReadAddr
  -- | State to handle Write operations.
  -- When @_writesLeft == 1@ the state progresses to @ReadAddr@ or to
  -- @WaitForLast@.
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
  -- Wait until @_last@ was set.
  | WaitForLast
  deriving (Show, Generic, ShowX, NFDataX)

recordProcessorT :: forall dataWidth addrWidth dat .
  ( KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  , Show dat
  )
  => RecordProcessorState addrWidth
  -> (Maybe (PacketStreamM2S dataWidth (Bool, RecordHeader))
     , ((), Ack)
     )
  -> ( RecordProcessorState addrWidth
     , ( PacketStreamS2M
       , ( Maybe (Bypass addrWidth)
         , Df.Data (WishboneOperation addrWidth dataWidth dat)
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
    meta = _meta psFwd
    eop = fst meta
    hdr = snd meta

    -- Cannot read the Ack channels if the forward channels are NoData.
    ack = case state of
      WriteOrReadAddr -> True
      ReadAddr        -> True
      _               -> wbAck
    psBwd = ack

    -- Only write wishbone operations in the @Write@ or @Read@ state
    wbOut = case state of
      Write i a -> Df.Data
        $ WishboneOperation
          { _opAddr = a
          , _opDat = Just dat
          , _opSel = sel
          , _opDropCyc = dropCyc $ i + _rCount hdr
          , _opAddrSpace = if _writeIsConfig hdr then ConfigAddressSpace else WishboneAddressSpace
          , _opEOR = isLast
          , _opEOP = eop
          , _opAbort = abort
          }
      Read i -> Df.Data
        $ WishboneOperation
          { _opAddr = resize psWord
          , _opDat = Nothing
          , _opSel = sel
          , _opDropCyc = dropCyc i
          , _opAddrSpace = if _readIsConfig hdr then ConfigAddressSpace else WishboneAddressSpace
          , _opEOR = isLast
          , _opEOP = eop
          , _opAbort = abort
          }
      _ -> Df.NoData
      where
        dat = bitCoerce $ _data psFwd
        isLast = isJust $ _last psFwd
        sel = resize $ _byteEn hdr
        abort = _abort psFwd

        -- Drop @CYC@ if this was requested. Also drop if this is the last
        -- fragment of the packet.
        dropCyc left = eop || (left == 1 && _cyc hdr)

    -- The bypass line always receives data if data is streaming in.
    bpOut = Just $ Bypass hdr base (_abort psFwd)
      where
        base = case (state, state') of
          (WriteOrReadAddr, Read _) -> Just $ resize psWord
          (ReadAddr, _)             -> Just $ resize psWord
          _ -> Nothing

    fsm
      :: RecordProcessorState addrWidth
      -> PacketStreamM2S dataWidth (Bool, RecordHeader)
      -> RecordProcessorState addrWidth
    -- If this is the last fragment of the packet, jump back to the initial
    -- state. This can happen from any of the states.
    fsm _ PacketStreamM2S{_last}
      | isJust _last = WriteOrReadAddr
    fsm st@WriteOrReadAddr PacketStreamM2S{..} = st'
      where
        meta' = snd _meta
        wCount = _wCount meta'
        rCount = _rCount meta'

        st'
          | wCount > 0 = Write wCount $ resize psWord
          | rCount > 0 = Read rCount
          | otherwise  = st
    fsm Write{..} PacketStreamM2S{..} = st'
      where
        meta' = snd _meta
        wCount = _writesLeft
        wCount' = wCount - 1
        rCount = _rCount meta'

        addr'
          -- If @writeFifo@ is set, the write address is a FIFO and should not
          -- increment.
          | _writeFifo meta' = _addr
          | otherwise        = _addr + (natToNum @dataWidth)

        st'
          | wCount' == 0 =
            if rCount > 0
              then ReadAddr
              else WaitForLast
          | otherwise = Write wCount' addr'
    fsm ReadAddr PacketStreamM2S{..} = (Read . _rCount . snd) _meta
    fsm Read{..} _ = st'
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
-- This circuit assumes that @_last@ is set to @maxBound@ for the final fragment
-- of a Record packet, so that no additional edge-cases need to be handled here.
-- The @Bool@ in the @_meta@ field indicates the end of a whole Etherbone
-- packet, and is forwarded.
recordProcessorC :: forall dom dataWidth addrWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , Show dat
  , BitSize dat ~ dataWidth * 8
  )
  => Circuit (PacketStream dom dataWidth (Bool, RecordHeader))
             ( CSignal dom (Maybe (Bypass addrWidth))
             , Df.Df dom (WishboneOperation addrWidth dataWidth dat)
             )
recordProcessorC = forceResetSanity |> Circuit (B.second unbundle . fsm . B.second bundle)
  where
    fsm = mealyB recordProcessorT WriteOrReadAddr
{-# OPAQUE recordProcessorC #-}
