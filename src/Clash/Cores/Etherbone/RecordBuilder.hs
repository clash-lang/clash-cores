{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.RecordBuilder where

import Clash.Prelude
import Clash.Cores.Etherbone.Base
import Clash.Cores.Etherbone.RecordProcessor (Bypass (..))
import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream

import Control.DeepSeq
import Data.Maybe

-- Convert a RecordHeader from incoming to outgoing
hdrRx2Tx :: RecordHeader -> RecordHeader
hdrRx2Tx hdr
  = hdr { _bca = False
        , _rca = False
        , _rff = False
        , _wca = _bca hdr
        , _wff = _rff hdr
        , _rCount = 0
        , _wCount = _rCount hdr
        }

-- EBHeader properly formatted for an outgoing packet.
ebTxMeta :: forall dataWidth addrWidth. SNat dataWidth -> SNat addrWidth -> EBHeader
ebTxMeta SNat SNat = EBHeader
  { _magic    = etherboneMagic
  , _version  = fromIntegral etherboneVersion
  , _res      = 0
  , _nr       = True
  , _pr       = False
  , _pf       = False
  , _addrSize = addrSizeMask
  , _portSize = portSizeMask
  }
  where
    portSizeMask = sizeMask $ natToInteger @dataWidth
    addrSizeMask = sizeMask $ natToInteger @(Div addrWidth 8)

data RecordBuilderState
  -- | Wait for a new packet.
  -- For a write, write zeros and jump to @PadWrites@
  -- For a read, write the response header and jump to @BaseRetAddr@
  -- This state always gives backpressure, to make up for the lost cycle form
  -- the record header depacketizer.
  = Init
  -- | Write zeros for each queued (or finished) write.
  -- If second-to-last write, jump to @Header@.
  | BaseWriteAddr
  | PadWrites {_writesLeft :: Unsigned 8}
  -- | Write the response record header
  | Header
  -- | Write the @BaseRetAddr@. This is know either through the stored
  -- @_baseAddr@ value, or through the current data on the bypass line.
  | BaseRetAddr
  -- | Write the returned values from the @WishboneMaster@.
  | ReadValues
  deriving (Generic, NFDataX, NFData, Show, ShowX, Eq)

recordBuilderT :: forall addrWidth dataWidth dat .
  ( KnownNat addrWidth
  , KnownNat dataWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  )
  => RecordBuilderState
  -> ( Maybe (Bypass addrWidth)
     , Df.Data (WishboneResult dat)  -- Config space
     , Df.Data (WishboneResult dat)  -- Wishbone space
     , PacketStreamS2M
     )
  -> ( RecordBuilderState
     , (Ack, Maybe (PacketStreamM2S dataWidth EBHeader))
     )
recordBuilderT Init (Nothing, _, _, _) = (Init, (Ack True, Nothing))
recordBuilderT Init (Just Bypass{_bpAbort=True}, _, _, _) = (Init, (Ack True, Nothing))
recordBuilderT st@Init (Just Bypass{..}, _, _, PacketStreamS2M psBwd)
  = (nextState, (Ack False, psFwd))
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
      BaseWriteAddr -> Just 0
      BaseRetAddr   -> Just $ pack $ hdrRx2Tx hdr
      Init          -> Nothing
      _             -> error "Invalid next state? This should be impossible."

    psFwd = (\x -> PacketStreamM2S x Nothing meta False) . bitCoerce . resize <$> datOut
    meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@BaseWriteAddr (Just Bypass{..}, _, _, PacketStreamS2M psBwd)
  = (nextState, (Ack False, psFwd))
  where
    hdr = _bpHeader
    st'
      | _bpAbort         = Init
      | _wCount hdr == 1 = Header
      | otherwise        = PadWrites (_wCount hdr - 1)
    nextState
      | psBwd     = st'
      | otherwise = st

    psFwd = Just $ PacketStreamM2S (repeat 0) lst meta _bpAbort
    lst = if _bpAbort then Just maxBound else Nothing
    meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@PadWrites{..} (Just Bypass{..}, cfg, wbm, PacketStreamS2M psBwd)
  = (nextState, (Ack psBwd, psFwd))
  where
    hdr = _bpHeader
    wCount' = _writesLeft - 1

    st'
      | _bpAbort    = Init
      | wCount' > 0 = PadWrites wCount'
      | otherwise   = Header
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    port
      | _wca hdr  = cfg
      | otherwise = wbm

    psFwd = case port of
      Df.Data _ -> Just $ PacketStreamM2S (repeat 0) lst meta _bpAbort
      Df.NoData -> Nothing
      where
        lst = if _bpAbort then Just maxBound else Nothing
        meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@Header (Just Bypass{..}, cfg, wbm, PacketStreamS2M psBwd)
  = (nextState, (Ack psBwd, psFwd))
  where
    hdr = _bpHeader

    st'
      | _bpAbort        = Init
      | _rCount hdr > 0 = BaseRetAddr
      | otherwise       = Init
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    port
      | _wca hdr  = cfg
      | otherwise = wbm

    psFwd = case port of
      Df.Data r -> Just $ PacketStreamM2S (prepDat hdr) (lst r) meta _bpAbort
      Df.NoData -> Nothing
      where
        prepDat = bitCoerce . resize . pack . hdrRx2Tx
        lst r = if _bpAbort || _resLast r then Just maxBound else Nothing
        meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@BaseRetAddr (Just Bypass{..}, _, _, PacketStreamS2M psBwd)
  = (nextState, (Ack False, psFwd))
  where
    st'
      | _bpAbort          = Init
      | isNothing _bpBase = BaseRetAddr
      | otherwise         = ReadValues
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    psFwd = case _bpBase of
      Just base -> Just $ PacketStreamM2S (bitCoerce $ resize base) lst meta _bpAbort
      Nothing   -> Nothing
    lst = if _bpAbort then Just maxBound else Nothing
    meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT st@ReadValues (Just Bypass{..}, cfg, wbm, PacketStreamS2M psBwd)
  = (nextState, (Ack psBwd, psFwd))
  where
    hdr = _bpHeader

    st'
      | _bpAbort = Init
      | otherwise = case port of
        Df.Data p -> if _resLast p then Init else ReadValues
        Df.NoData -> ReadValues
    nextState
      | isJust psFwd && psBwd = st'
      | otherwise             = st

    port
      | _rca hdr  = cfg
      | otherwise = wbm

    psFwd = case port of
      Df.Data WishboneResult{_resDat=Just d, _resLast}
        -> Just $ PacketStreamM2S (bitCoerce d) (lst _resLast) meta _bpAbort
      Df.Data WishboneResult{_resDat=Nothing}
        -> Nothing
      Df.NoData -> Nothing
      where
        lst r = if _bpAbort || r then Just maxBound else Nothing
        meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT _ (Nothing, _, _, _) = error "This should not be possible. Only the 'Init' state can receive an empty Bypass signal."

data BypassLatchState addrWidth
  = Waiting
  | Latched { _bp :: Bypass addrWidth }
  | Last    { _bp :: Bypass addrWidth }
  deriving (Generic, NFDataX, Show)

-- Abort is handled by recordBuilderT. If an abort is received, @recordBuilderT@
-- constructs a response fragment with @_abort@ and @_last@ set. This resets
-- this state machine.
bypassLatchT ::
  ( KnownNat addrWidth )
  => BypassLatchState addrWidth
  -> (Maybe (Bypass addrWidth), Maybe Bool )
  -> (BypassLatchState addrWidth, Maybe (Bypass addrWidth))
bypassLatchT Waiting (Nothing, Just True) = error "Cannot get a last signal with no bypass data and nothing latched."
bypassLatchT Waiting (Nothing, _) = (Waiting, Nothing)
bypassLatchT Waiting (Just b, Just True) = (Last b, Just b)
bypassLatchT Waiting (Just b, _) = (Latched b, Just b)
bypassLatchT Latched{..} (Just b, lst) = (st', Just res)
  where
    res = Bypass { _bpHeader = _bpHeader _bp
                 , _bpBase   = _bpBase _bp <|> _bpBase b
                 , _bpAbort  = _bpAbort b
                 }
    st' = case lst of
      Just True -> Last res
      _         -> Latched res
bypassLatchT Latched{..} (Nothing, lst) = (st', Just _bp)
  where
    st' = case lst of
      Just True -> Last _bp
      _         -> Latched _bp
bypassLatchT Last{..} (_, lst) = case lst of
  Just True -> (Last _bp, Just _bp)
  _         -> (Waiting, Nothing)

-- | This combines @WishboneResult@s from the @WishboneMaster@ and
-- @ConfigMaster@ into a response packet. The header and @retBaseAddr@ comes
-- through the bypass line.
--
-- The bypass line is used so that the builder does not have to wait on the
-- wishbone bus to start constructing a packet. The data on the bypass line is
-- latched in @bypassLatchT@ for as long as a packet takes to be completed. 
recordBuilderC :: forall dom addrWidth dataWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat dataWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  , 4 <= dataWidth
  )
  => Circuit ( CSignal dom (Maybe (Bypass addrWidth))
             , Df.Df dom (WishboneResult dat)  -- Config space
             , Df.Df dom (WishboneResult dat)  -- Wishbone space
             )
             (PacketStream dom dataWidth EBHeader)
recordBuilderC = Circuit go
  where
    go ((bypass, cfg, wbm), psBwd) = ((pure (), wbBwd, wbBwd), psFwd)
      where
        (wbBwd, psFwd) = mealyB recordBuilderT Init (bypassLatch, cfg, wbm, psBwd)

        isLast = fmap (isJust . _last) <$> psFwd

        -- The bypass signals are latched for the duration of a packet.
        -- The Bypass record returned has a bias for earlier received fields for
        -- @_bpHeader@ and @_bpBase@. For @_bpAbort@ the newest signal is always
        -- returned.
        bypassLatch ::
          Signal dom (Maybe (Bypass addrWidth))
        bypassLatch = mealyB bypassLatchT Waiting (bypass, isLast)
{-# OPAQUE recordBuilderC #-}
