{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Clash.Cores.Etherbone.WishboneMaster where

import Clash.Cores.Etherbone.Base
import Clash.Prelude
import Data.Maybe
import Protocols
import Protocols.Wishbone

import qualified Protocols.Df as Df

data WishboneMasterState dataWidth
  -- | Wait for an incoming wishbone operation. If an op is available, it is
  -- directly forwarded to the wishbone bus.
  = WaitForOp  { _wbmCyc :: Bool }
  -- | Wait for a termination signal from the wishbone bus.
  | Busy
  -- | Forward result and wait for an Ack. In this state, a new operation can
  -- already be sent on the input, though only in the @WaitForOp@ state is it
  -- being handled.
  | WaitForAck { _wbmCyc :: Bool, _retDat :: Maybe (BitVector (dataWidth * 8)), _isEOR :: Bool, isEOP :: Bool}
  deriving (Generic, NFDataX, Show, Eq)

wishboneMasterT :: forall addrWidth dataWidth .
  ( KnownNat addrWidth
  , KnownNat dataWidth
  )
  => WishboneMasterState dataWidth
  -> ( Maybe (WishboneOperation addrWidth dataWidth)
     , Ack
     , WishboneS2M dataWidth
     )
  -> ( WishboneMasterState dataWidth
     , ( Ack
       , Maybe (WishboneResult dataWidth)
       , WishboneM2S addrWidth dataWidth
       , Maybe Bit
       )
     )
-- This operation is for another @AddressSpace@.
wishboneMasterT state (Just WishboneOperation{_opAddrSpace=ConfigAddressSpace}, _, _)
  = (state, (Ack True, Nothing, wbEmpty, Nothing))
    where
      wbEmpty = emptyWishboneM2S
wishboneMasterT state (iFwd, Ack oBwd, wbBwd)
  = (nextState, (Ack iBwd, oFwd, wbFwd, errBit))
  where
    nextState = fsm state iFwd oBwd

    wbErr = err wbBwd || retry wbBwd
    wbTerm = acknowledge wbBwd || wbErr

    oFwd = case state of
      WaitForAck _ dat eor eop -> Just $ WishboneResult dat eor eop
      _                        -> Nothing

    iBwd = case state of
      WaitForOp _  -> False
      Busy         -> False
      WaitForAck{} -> oBwd

    errBit = case (state, nextState) of
      (Busy, WaitForAck{}) -> Just $ boolToBit wbErr
      _                    -> Nothing

    -- The wishbone bus receives the incoming operation in the transition from
    -- @WaitForOp@ to @Busy@.
    wbFwd = case (state, iFwd) of
      (WaitForOp c, Nothing) -> wbEmpty   { strobe=False, busCycle=c }
      (WaitForOp _, Just i) -> (wbPkt i) { strobe=True,  busCycle=True }
      (Busy, Nothing)        -> errorX "No input data in Busy state, this should be impossible!"
      (Busy, Just i)        -> (wbPkt i) { strobe=True,  busCycle=True }
      (WaitForAck c _ _ _, _)  -> wbEmpty   { strobe=False, busCycle=c }
    wbPkt WishboneOperation{..} = WishboneM2S
      { addr                = _opAddr
      , writeData           = fromJustX _opDat
      , busSelect           = _opSel
      , lock                = False
      , busCycle            = deepErrorX "busCycle was not set"
      , strobe              = deepErrorX "strobe was not set"
      , writeEnable         = isJust _opDat
      , cycleTypeIdentifier = Classic
      , burstTypeExtension  = LinearBurst
      }
    wbEmpty = emptyWishboneM2S

    fsm
      :: WishboneMasterState dataWidth  -- state
      -> Maybe (WishboneOperation addrWidth dataWidth)  -- iFwd
      -> Bool                     -- oBwd
      -> WishboneMasterState dataWidth  -- nextState
    fsm st@WaitForOp{} Nothing _ = st
    fsm WaitForOp{} (Just WishboneOperation{..}) _
      | _opAbort  = WaitForOp False
      | otherwise = Busy
    fsm Busy{} Nothing _ = error "Sender did not keep Df channel constant!"
    fsm Busy{} (Just x) _
      | wbTerm    = WaitForAck (not $ _opDropCyc x) (dat $ _opDat x) (_opEOR x) (_opEOP x)
      | otherwise = Busy
      where
        dat Nothing  = Just $ readData wbBwd
        dat (Just _) = Nothing
    fsm st@WaitForAck{} _ False = st
    fsm WaitForAck{..} _ True = WaitForOp _wbmCyc

-- | Transact operations on the wishbone bus.
--
-- This is a Wishbone Classic implementation, meaning that the circuit will give
-- backpressure while the wishbone bus is busy. Once the wishbone bus has set a
-- termination signal, the circuit can receive a new operation.
-- Only once the resulting data has been acknowledged by the @RecordBuillder@
-- will this circuit start a new operation.
--
-- The @busCycle@ is controlled by the @_dropCyc@ bool in the incoming
-- operation. If this is set, the @CYC@ line is dropped in the @WaitForAck@
-- state and kept low until a new operation arives.
wishboneMasterC
  :: forall dom addrWidth dataWidth .
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat dataWidth
  )
  => Circuit (Df.Df dom (WishboneOperation addrWidth dataWidth))
             ( Df.Df dom (WishboneResult dataWidth)
             , Wishbone dom Standard addrWidth dataWidth
             , CSignal dom (Maybe Bit)
             )
wishboneMasterC = Circuit go
  where
    go (op, (ackIn, s2m, _)) = (ackOut, (resOut, m2s, err))
      where
        (ackOut, resOut, m2s, err) = mealyB wishboneMasterT (WaitForOp False) (op, ackIn, s2m)
{-# OPAQUE wishboneMasterC #-}
