{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.RecordBuilder where

import Clash.Cores.Etherbone.Base
import Clash.Prelude
import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream

import Control.DeepSeq
import Data.Maybe

-- Convert a RecordHeader from incoming to outgoing
hdrRx2Tx :: RecordHeader -> RecordHeader
hdrRx2Tx hdr
  = hdr { _baseIsConfig  = False
        , _readIsConfig  = False
        , _readFifo      = False
        , _writeIsConfig = _baseIsConfig hdr
        , _writeFifo     = _readFifo hdr
        , _rCount        = 0
        , _wCount        = _rCount hdr
        }

-- EBHeader properly formatted for an outgoing packet
ebTxMeta :: forall dataWidth addrWidth. SNat dataWidth -> SNat addrWidth -> EBHeader
ebTxMeta SNat SNat = EBHeader
  { _magic      = etherboneMagic
  , _version    = fromIntegral etherboneVersion
  , _res        = 0
  , _noReads    = True
  , _probeResp  = False
  , _probeFlag  = False
  , _addrSize   = addrSizeMask
  , _portSize   = portSizeMask
  }
  where
    portSizeMask = sizeMask $ natToInteger @dataWidth
    addrSizeMask = sizeMask $ natToInteger @(Div addrWidth 8)

data RecordBuilderState
  -- | Wait for a new packet.
  -- For a write, write zeros and jump to @BaseWriteAddr@.
  -- For a read, write the response header and jump to @BaseRetAddr@.
  -- This state always gives backpressure, to make up for the lost cycle form
  -- the record header depacketizer.
  = Init
  -- | Write zeros in the spot of the @baseWriteAddr@ in the reponse.
  | BaseWriteAddr
  -- | Write zeros for each finished write.
  -- If second-to-last write, jump to @Header@.
  | PadWrites {_writesLeft :: Unsigned 8}
  -- | Write the response record header
  | Header
  -- | Write the @BaseRetAddr@. By now this is known and sent over the bypass
  -- line.
  | BaseRetAddr
  -- | Write the returned values from the @WishboneMaster@.
  | ReadValues
  -- | Send an abort @PacketStream@ signal. This state is used to decouple the
  -- @Bypass@ input from the @PacketStream@ output.
  | Aborted
  deriving (Generic, NFDataX, NFData, Show, ShowX, Eq)

-- In the case of an abort, the state machine will be put into the @Aborted@
-- state for one cycle. It then sends an 'end-of-packet' (by setting @_last@)
-- with no data and @_abort@ asserted as well.
-- The ConfigMaster and WishboneMaster __may not__ send anything after an abort
-- was asserted. So there is no need to manage the incoming Df data lines.
recordBuilderT :: forall addrWidth dataWidth .
  ( KnownNat addrWidth
  , KnownNat dataWidth
  , 4 <= dataWidth
  )
  => RecordBuilderState
  -> ( Maybe (Bypass addrWidth)
     , Maybe (WishboneResult dataWidth)  -- Config space
     , Maybe (WishboneResult dataWidth)  -- Wishbone space
     , PacketStreamS2M
     )
  -> ( RecordBuilderState
     , (Ack, Maybe (PacketStreamM2S dataWidth EBHeader))
     )
recordBuilderT Init (Nothing, _, _, _) = (Init, (Ack True, Nothing))
recordBuilderT Init (Just Bypass{_bpAbort=True}, _, _, _) = (Init, (Ack True, Nothing))
recordBuilderT st@Init (Just Bypass{..}, _, _, PacketStreamS2M psBwd)
  = (nextState, (Ack False, psFwd))
  -- TODO: Get rid of the RecordWildcard uses here.
  where
    hdr = _bpHeader
    st'
      | _wCount hdr > 0 = BaseWriteAddr
      | _rCount hdr > 0 = BaseRetAddr
      | otherwise       = Init
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    datOut = case st' of
      BaseWriteAddr -> Just $ repeat 0
      BaseRetAddr   -> Just $ bitCoerce (pack $ hdrRx2Tx hdr) ++ repeat 0
      Init          -> Nothing
      _             -> error "Invalid next state? This is impossible."

    psFwd = (\x -> PacketStreamM2S x Nothing meta False) <$> datOut
    meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@BaseWriteAddr (Just Bypass{..}, _, _, PacketStreamS2M psBwd)
  = (nextState, (Ack False, psFwd))
  where
    hdr = _bpHeader
    st'
      | _bpAbort         = Aborted
      | _wCount hdr == 1 = Header
      | otherwise        = PadWrites (_wCount hdr - 1)
    nextState
      | psBwd     = st'
      | otherwise = st

    psFwd = Just $ PacketStreamM2S (repeat 0) Nothing meta False
    meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@PadWrites{..} (Just Bypass{..}, cfg, wbm, PacketStreamS2M psBwd)
  = (nextState, (Ack psBwd, psFwd))
  where
    hdr = _bpHeader
    wCount' = _writesLeft - 1

    st'
      | _bpAbort    = Aborted
      | wCount' > 0 = PadWrites wCount'
      | otherwise   = Header
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    port
      | _writeIsConfig hdr = cfg
      | otherwise          = wbm

    psFwd = case port of
      Just _ -> Just $ PacketStreamM2S (repeat 0) Nothing meta False
      Nothing -> Nothing
      where
        meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@Header (Just Bypass{..}, cfg, wbm, PacketStreamS2M psBwd)
  = (nextState, (Ack psBwd, psFwd))
  where
    hdr = _bpHeader

    st'
      | _bpAbort        = Aborted
      | _rCount hdr > 0 = BaseRetAddr
      | otherwise       = Init
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    port
      | _writeIsConfig hdr = cfg
      | otherwise          = wbm

    psFwd = case port of
      Just r -> Just $ PacketStreamM2S headerDat (lst r) meta False
      Nothing -> Nothing
      where
        headerDat = bitCoerce (pack (hdrRx2Tx hdr)) ++ repeat @(dataWidth - 4) 0
        lst r = if _resEOP r then Just maxBound else Nothing
        meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@BaseRetAddr (Just Bypass{..}, _, _, PacketStreamS2M psBwd)
  = (nextState, (Ack False, psFwd))
  where
    st'
      | _bpAbort          = Aborted
      | isNothing _bpBase = BaseRetAddr
      | otherwise         = ReadValues
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    psFwd = case _bpBase of
      Just base -> Just $ PacketStreamM2S (bitCoerce $ resize base) Nothing meta False
      Nothing   -> Nothing
    meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@ReadValues (Just Bypass{..}, cfg, wbm, PacketStreamS2M psBwd)
  = (nextState, (Ack psBwd, psFwd))
  where
    hdr = _bpHeader

    st'
      | _bpAbort  = Aborted
      | otherwise = case port of
        Just p -> if _resEOR p then Init else ReadValues
        Nothing -> ReadValues
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    port
      | _readIsConfig hdr = cfg
      | otherwise         = wbm

    psFwd = case port of
      Just WishboneResult{_resDat=Just d, _resEOP}
        -> Just $ PacketStreamM2S (bitCoerce d) (lst _resEOP) meta False
      Just WishboneResult{_resDat=Nothing}
        -> deepErrorX "Each result should hold valid data."
      Nothing -> Nothing
      where
        lst r = if r then Just maxBound else Nothing
        meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT Aborted (_, _, _, PacketStreamS2M psBwd)
  = (nextState, (Ack False, psFwd))
  where
    nextState
      | psBwd     = Init
      | otherwise = Aborted

    psFwd = Just $ PacketStreamM2S (repeat 0) (Just 0) meta True
    meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT _ (Nothing, _, _, _)
  = error "This should not be possible. Only the 'Init' and 'Aborted' states can receive an empty Bypass signal."

bypassLatchT ::
  ( KnownNat addrWidth )
  => Maybe (Bypass addrWidth)
  -> (Maybe (Bypass addrWidth), Bool, Bool)
  -> (Maybe (Bypass addrWidth), Maybe (Bypass addrWidth))
bypassLatchT Nothing (Nothing, True, _) = deepErrorX "Cannot get a last signal with no bypass data and nothing latched."
bypassLatchT Nothing (Nothing, False, _) = (Nothing, Nothing)
bypassLatchT Nothing (Just bp, lst, bwd) = (st', Just bp)
  where
    st'
      | _bpAbort bp = Nothing
      | lst && bwd  = Nothing
      | otherwise   = Just bp
bypassLatchT (Just bs) (Just bp, lst, bwd) = (nextState, Just res)
  where
    res = Bypass { _bpHeader = _bpHeader bs
                 , _bpBase   = _bpBase bs <|> _bpBase bp
                 , _bpAbort  = _bpAbort bp
                 }
    st'
      | _bpAbort bp = Nothing
      | lst         = Nothing
      | otherwise   = Just res
    nextState
      | bwd       = st'
      | otherwise = Just res
bypassLatchT (Just bs) (Nothing, lst, bwd) = (nextState, Just bs)
  where
    st'
      | _bpAbort bs = Nothing
      | lst         = Nothing
      | otherwise   = Just bs
    nextState
      | bwd       = st'
      | otherwise = Just bs

-- | This combines @WishboneResult@s from the @WishboneMaster@ and
-- @ConfigMaster@ into a response packet. The header and @retBaseAddr@ comes
-- through the bypass line.
--
-- The bypass line is used so that the builder does not have to wait on the
-- wishbone bus to start constructing a packet. The data on the bypass line is
-- latched in @bypassLatchT@ for as long as a packet takes to be completed.
--
-- This implementation waits not only for reads, but also for writes when
-- constructing a response.
recordBuilderC :: forall dom addrWidth dataWidth .
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat dataWidth
  , 4 <= dataWidth
  )
  => Circuit ( CSignal dom (Maybe (Bypass addrWidth))
             , Df.Df dom (WishboneResult dataWidth)  -- Config space
             , Df.Df dom (WishboneResult dataWidth)  -- Wishbone space
             )
             (PacketStream dom dataWidth EBHeader)
recordBuilderC = Circuit go
  where
    go ((bypass, cfg, wbm), psBwd) = (((), wbBwd, wbBwd), psFwd)
      where
        (wbBwd, psFwd) = mealyB recordBuilderT Init (bypassLatch, cfg, wbm, psBwd)

        -- Check for an end-of-record from any of the masters. After each record
        -- the @bypassLatch@ should be reset.
        -- A master __may not__ send anything when it had not received an
        -- operation specifically meant for it.
        wbPorts = (<|>) <$> wbm <*> cfg
        isLast = maybe False _resEOR <$> wbPorts

        -- Backward signal to masters, used by @bypassLatchT@ to check if a
        -- fragment was handled fully.
        -- Also check @Fwd@. If this is @Nothing@, @wbBwd@ cannot be read.
        bypassBwd = (isJust <$> wbPorts) .&&. (bwd <$> wbBwd)
          where bwd (Ack a) = a

        -- The bypass signals are latched for the duration of a packet.
        -- The Bypass record returned has a bias for earlier received fields for
        -- @_bpHeader@ and @_bpBase@. For @_bpAbort@ the newest signal is always
        -- returned.
        bypassLatch :: Signal dom (Maybe (Bypass addrWidth))
        bypassLatch = mealyB bypassLatchT Nothing (bypass, isLast, bypassBwd)
        -- bypassLatch = mealyB bypassLatchT Waiting (bypass, isLast)
{-# OPAQUE recordBuilderC #-}
