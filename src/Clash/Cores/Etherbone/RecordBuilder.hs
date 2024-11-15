{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.RecordBuilder where

import Clash.Prelude

import Clash.Cores.Etherbone.Base

import Protocols.PacketStream
import Protocols

import Data.Maybe
import qualified Data.Bifunctor as B
import Debug.Trace
import Clash.Cores.Etherbone.RecordProcessor (Bypass (..))
import qualified Protocols.Df as Df
import Control.DeepSeq
import Protocols.Avalon.MemMap (AvalonMmSharedConfig(addrWidth))


recordTxMetaMap :: RecordHeader -> RecordHeader
recordTxMetaMap hdr
  = hdr { _bca = False
        , _rca = False
        , _rff = False
        , _wca = _bca hdr
        , _wff = _rff hdr
        , _rCount = 0
        , _wCount = _rCount hdr
        }

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


data RecordBuilderState addrWidth
  -- | Wait for a new packet.
  -- For a write, write zeros and jump to @PadWrites@
  -- For a read, write the response header and jump to @BaseRetAddr@
  -- This state always gives backpressure, to make up for the lost cycle form
  -- the record header depacketizer.
  = Init
  -- | Write zeros for each queued (or finished) write.
  -- If second-to-last write, jump to @Header@.
  | BaseWriteAddr {_header :: RecordHeader}
  | PadWrites
    { _header     :: RecordHeader
    , _maybeBase  :: Maybe (BitVector addrWidth)
    , _writesLeft :: Unsigned 8
    }
  -- | Write the response record header
  | Header
    { _header    :: RecordHeader
    , _maybeBase :: Maybe (BitVector addrWidth)
    }
  -- | Write the @BaseRetAddr@. This is know either through the stored
  -- @_baseAddr@ value, or through the current data on the bypass line.
  | BaseRetAddr
    { _header    :: RecordHeader
    , _maybeBase :: Maybe (BitVector addrWidth)
    }
  -- | Write the returned values from the @WishboneMaster@.
  | ReadValues  {_header    :: RecordHeader}
  | WaitForLast
  deriving (Generic, NFDataX, NFData, Show, ShowX, Eq)

-- TODO: Check how difficult it would be to make it configurable whether we wait
-- for writes to finish or directly reply.


recordBuilderT :: forall addrWidth dataWidth dat .
  ( KnownNat addrWidth
  , KnownNat dataWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  )
  => RecordBuilderState addrWidth
  -> ( ( Df.Data (Bypass addrWidth)
       , Df.Data (WishboneResult dat)  -- Config space
       , Df.Data (WishboneResult dat)  -- Wishbone space
       )
     , PacketStreamS2M
     )
  -> ( RecordBuilderState addrWidth
     , ( (Ack, Ack, Ack)
       , Maybe (PacketStreamM2S dataWidth EBHeader)
       )
     )
-- TODO: Fix this impl for now. If I have time I want to rewrite this with
-- pattern matching...
recordBuilderT state ((bypass, cfgDat, wbmDat), PacketStreamS2M psBwd)
  = (trace ("BNS " <> show nextState) nextState, ((Ack bpAck, Ack cfgAck, Ack wbmAck), psFwd))
  where
    nextState
      | psAck     = state'
      | otherwise = state
    state' = fsm (trace ("BSt " <> show state) state) mBypass
    
    mBypass = Df.dataToMaybe $ trace ("BuildBP: " <> show bypass) bypass
    --psAck = isJust psFwd && psBwd
    -- Default to no backpressure if no output
    psAck = isNothing psFwd || psBwd

    wbAck = case state of
      Init            -> False
      BaseWriteAddr{} -> False
      BaseRetAddr{}   -> isNothing dataIn  -- This is a hack
      WaitForLast     -> True
      _               -> psAck
    cfgAck = wbAck
    wbmAck = wbAck

    bpAck = case state of
      Init -> False
      _    -> psAck

    -- TODO: Issue: If psFwd is Nothing, psBwd cannot be read

    psFwd = case state of
      Init -> case mBypass of
        Just bp -> let h = _bpHeader bp in
          case _bpBase bp of
            Just _  -> pkt (resize $ pack $ recordTxMetaMap h) False mBypass
            Nothing -> pkt 0 False mBypass
        Nothing -> Nothing
      BaseWriteAddr{} -> pkt 0 False mBypass
      PadWrites{}     -> pkt 0 False mBypass
      Header h _      -> pkt (resize $ pack $ recordTxMetaMap h) (onlyWrites h) mBypass
      BaseRetAddr _ b    -> basePkt b mBypass
      ReadValues h -> case Df.dataToMaybe (resPort h) >>= _resDat of
        Just dat      -> pkt (pack dat) (lastPort h) mBypass
        Nothing       -> Nothing
      WaitForLast     -> Nothing

      where
        pkt dat' isLst bp = Just $ PacketStreamM2S dat lst meta (abort bp)
          where
            dat = bitCoerce dat'
            lst
              | isLst     = Just maxBound
              | abort bp  = Just maxBound
              | otherwise = Nothing
            meta = ebTxMeta (SNat @dataWidth) (SNat @addrWidth)
        abort = maybe False _bpAbort

        basePkt Nothing Nothing = Nothing
        basePkt (Just base) bp = pkt (resize $ pack base) False bp
        basePkt Nothing bp@(Just x) = case _bpBase x of
          Just base -> pkt (resize $ pack base) False bp
          _         -> Nothing

        resPort hdr
          | _rca hdr  = cfgDat
          | otherwise = wbmDat
        lastPort hdr
          | _rca hdr  = cfgLast
          | otherwise = wbmLast

        onlyWrites hdr = _rCount hdr == 0


    cfgLast = maybe False _resLast $ Df.dataToMaybe cfgDat
    wbmLast = maybe False _resLast $ Df.dataToMaybe wbmDat

    dataIn
      =   (Df.dataToMaybe cfgDat >>= _resDat)
      <|> (Df.dataToMaybe wbmDat >>= _resDat)

    fsm
      :: RecordBuilderState addrWidth
      -> Maybe (Bypass addrWidth)
      -> RecordBuilderState addrWidth
    fsm Init Nothing = Init
    fsm Init (Just Bypass{..})
      | _bpAbort   = Init
      | wCount > 0 = BaseWriteAddr _bpHeader
      | rCount > 0 = BaseRetAddr _bpHeader _bpBase
      | otherwise  = Init
      where
        wCount = _wCount _bpHeader
        rCount = _rCount _bpHeader
    fsm BaseWriteAddr{..} bp
      | _wCount _header == 1 = Header _header base
      | otherwise            = PadWrites _header base (_wCount _header - 1) 
      where
        base = bp >>= _bpBase
    fsm PadWrites{..} bp
      | wCount' > 0 = PadWrites _header base wCount'
      | otherwise   = Header _header base
      where
        wCount = _writesLeft
        wCount' = wCount - 1
        base = _maybeBase <|> (bp >>= _bpBase)
    fsm Header{..} bp
      | rCount > 0         = BaseRetAddr _header base
      | cfgLast || wbmLast = Init
      | otherwise          = WaitForLast
      where
        base = _maybeBase <|> (bp >>= _bpBase)
        rCount = _rCount _header
    fsm BaseRetAddr{..} bp
      | isNothing base = BaseRetAddr _header Nothing
      | otherwise      = ReadValues _header
      where
        base = _maybeBase <|> (bp >>= _bpBase)
    fsm ReadValues{..} _
      -- Assumption: Never more data after 'last' signal. This is enforeced by
      -- the RecordProcessor
      | rca     && cfgLast = Init
      | not rca && wbmLast = Init
      | otherwise          = ReadValues _header
      where
        rca = _rca _header
    fsm WaitForLast _
      | cfgLast || wbmLast = Init
      | otherwise          = WaitForLast

-- TODO: Idea. Split the bypass path management (caching of incoming meta info)
-- from the work FSM. So two FSMs.
recordBuilderC :: forall dom addrWidth dataWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat dataWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  , 4 <= dataWidth
  )
  => Circuit ( Df.Df dom (Bypass addrWidth)
             , Df.Df dom (WishboneResult dat)  -- Config space
             , Df.Df dom (WishboneResult dat)  -- Wishbone space
             )
             (PacketStream dom dataWidth EBHeader)
recordBuilderC = Circuit (B.first unbundle . go . B.first bundle)
  where
    go = mealyB recordBuilderT Init

    -- Extract 'last' signal from cfg and wbm
    -- mealy with bypass and last in, bypass out.
