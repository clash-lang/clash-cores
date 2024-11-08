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


recordRxToTx :: RecordHeader -> RecordHeader
recordRxToTx hdr = hdr { _bca = False
                       , _rca = False
                       , _rff = False
                       , _wca = _bca hdr
                       , _wff = _rff hdr
                       , _rCount = 0
                       , _wCount = _rCount hdr
                       }

ebTx :: forall dataWidth addrWidth. SNat dataWidth -> SNat addrWidth -> EBHeader
ebTx SNat SNat = EBHeader { _magic    = 0x4e6f
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
  -- If wCount>0: Write zeros, jump to Pad
  --              or backpressure and then pad. Then it matches the rest
  -- If rCount>0: *Write header* and jump to Passthrough
  --              or give a cycle backpressure and jump to Header 
  -- No read or write: write header, jump to passthrough
  --              or Write Nothing, jump to Header. 
  = BuilderInit
  -- ^ Sends zeros or a header, depending on the wCount and rCount fields.
  | BuilderPad          { _header :: RecordHeader }
  -- ^ Sends zero padding. Required for wCount > 0 case
  | BuilderHeader       { _header :: RecordHeader }
  -- ^ Sends the header. Required for wCount > 0 case
  | BuilderPassthrough
  | BuilderWaitForLast
  deriving (Generic, NFDataX, Show, ShowX)

recordBuilderT :: forall dataWidth addrWidth .
  ( KnownNat dataWidth
  , KnownNat addrWidth
  , 4 <= dataWidth
  )
  => SNat addrWidth
  -> RecordBuilderState 
  -> ( ( Maybe (PacketStreamM2S dataWidth RecordHeader) -- Main
       , Maybe (PacketStreamM2S dataWidth RecordHeader) -- Bypass
       )
     , PacketStreamS2M
     )
  -> ( RecordBuilderState
     , ( (PacketStreamS2M, PacketStreamS2M)
       , Maybe (PacketStreamM2S dataWidth EBHeader) )
     )
recordBuilderT SNat _ ((_, Just PacketStreamM2S{ _abort = True, _last }), PacketStreamS2M{_ready})
  = (BuilderInit, ( (PacketStreamS2M _ready, PacketStreamS2M _ready), Just out ))
  where
    -- TODO: Check if it is possible for an abort to _not_ be the last fragment
    out = PacketStreamM2S (repeat 0) _last ebTx' True
    ebTx' = ebTx (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT SNat BuilderInit ((_, Nothing), _)
  = (BuilderInit, ( (PacketStreamS2M True, PacketStreamS2M True), Nothing ))
recordBuilderT SNat BuilderInit ((_, Just bypass), PacketStreamS2M{_ready})
  = (trace (show nextState) nextState, ( (PacketStreamS2M False, PacketStreamS2M _ready), Just out ))
  where
    (nextState', out)
      | _wCount hdr > 0 = (BuilderPad hdr,     outZeros)
      | otherwise       = (BuilderPassthrough, outHeader)

    nextState
      | not _ready     = BuilderInit
      | isJust outLast = BuilderInit
      | otherwise      = nextState'

    hdr = _meta bypass

    outLast = _last bypass
    outZeros = PacketStreamM2S (repeat 0) outLast ebTx' False
    outHeader = PacketStreamM2S (dat ++ repeat @(dataWidth-4) 0) outLast ebTx' False
    ebTx' = ebTx (SNat @dataWidth) (SNat @addrWidth)
    dat = bitCoerce (recordRxToTx hdr)
recordBuilderT SNat st@BuilderPad{_header} ((_, _), PacketStreamS2M{_ready})
  = (trace (show nextState) nextState, ( (PacketStreamS2M _ready, PacketStreamS2M True), Just outZeros ))
  where
    nextState
      | not _ready   = st
      | wCount' == 0 = BuilderHeader header'
      | otherwise    = BuilderPad header'

    wCount  = _wCount _header
    wCount' = wCount - 1
    header' = _header { _wCount = wCount' }

    outZeros = PacketStreamM2S (repeat 0) Nothing ebTx' False
    ebTx' = ebTx (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT SNat st@BuilderHeader{_header} ((_, maybeBypass), PacketStreamS2M{_ready})
  = (trace (show nextState) nextState, ( (PacketStreamS2M _ready, PacketStreamS2M True), Just outHeader ))
  where
  -- Problem here. it can happen that the header is written while there is still
  -- a bypass signal coming in. If we then jump to Init the FSM will run again.
  -- If we only have writes and
  --  - bypass is still Just: wait until the bypass signals a _last
  --  - bypass is Nothing: then we can safely jump to Init
  --
  -- Actually, since we do not do pipelined (yet) the bypass and the being
  -- processed signal should be the same. So looking at the bypass _should_ be
  -- sufficient?
  -- Either way we need a new state where it just waits until _last bypass is
  -- set
  --
  -- Fixed but very hacky
    nextState
      | not _ready            = st
      | rCount > 0            = BuilderPassthrough
      | otherwise = case maybeBypass >>= _last of
        Nothing -> BuilderWaitForLast
        Just _  -> BuilderInit

    rCount = _rCount _header
    isLast = rCount == 0

    outHeader =
      PacketStreamM2S
        (dat ++ repeat @(dataWidth-4) 0)
        (if isLast then Just (natToNum @dataWidth) else Nothing)
        ebTx'
        False
    ebTx' = ebTx (SNat @dataWidth) (SNat @addrWidth)
    dat = bitCoerce (recordRxToTx _header)
recordBuilderT SNat st@BuilderPassthrough ((Nothing, _), PacketStreamS2M{_ready})
  = (st, ( (PacketStreamS2M _ready, PacketStreamS2M True), Nothing ))
recordBuilderT SNat st@BuilderPassthrough ((Just x, _), PacketStreamS2M{_ready})
  = (nextState, ( (PacketStreamS2M _ready, PacketStreamS2M True), Just out))
  where
    nextState
      | not _ready       = st
      | isJust (_last x) = BuilderInit
      | otherwise        = st

    out = x { _meta = ebTx' }
    ebTx' = ebTx (SNat @dataWidth) (SNat @addrWidth)
recordBuilderT SNat st@BuilderWaitForLast ((_, Nothing), PacketStreamS2M{_ready})
  = (st, ( (PacketStreamS2M _ready, PacketStreamS2M _ready), Nothing ))
recordBuilderT SNat st@BuilderWaitForLast ((_, Just b), PacketStreamS2M{_ready})
  = (nextState, ( (PacketStreamS2M _ready, PacketStreamS2M _ready), Nothing ))
  -- NOTE: Bypass is also backpressured if fwd has backpressure. This is
  -- required! Otherwise we might miss the _last signal
  where
    nextState
      | not _ready        = st
      | isJust (_last b)  = BuilderInit
      | otherwise         = st

recordBuilderC
  :: forall dom dataWidth addrWidth .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , 4 <= dataWidth
  )
  => SNat addrWidth
  -> Circuit (PacketStream dom dataWidth RecordHeader, PacketStream dom dataWidth RecordHeader)
             (PacketStream dom dataWidth EBHeader)
recordBuilderC SNat = Circuit (B.first unbundle . go . B.first bundle)
  where
    go = mealyB (recordBuilderT $ SNat @addrWidth) BuilderInit


recordBuilderC
  ::
  ()
  => Circuit ()
