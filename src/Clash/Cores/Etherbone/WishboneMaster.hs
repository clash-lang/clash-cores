{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.WishboneMaster where

import Clash.Prelude
import Protocols
import Protocols.Wishbone
import qualified Data.Bifunctor as B
import qualified Protocols.Df as Df
import Data.Maybe
import Clash.Cores.Etherbone.Base
import Clash.Debug

data WishboneMasterState dat
  -- | Wait for an incoming wishbone operation. If an op is available, it is
  -- direclty forwarded to the wishbone bus.
  = WaitForOp  { _wbmCyc :: Bool }
  -- | Wait for a termination signal from the wishbone bus.
  | Busy
  -- | Forward result and wait for an Ack. In this state, a new operation can
  -- already be sent on the input, though only in the @WaitForOp@ state is it
  -- being handled.
  | WaitForAck { _wbmCyc :: Bool, _retDat :: Maybe dat, _isLast :: Bool}
  deriving (Generic, NFDataX, Show, Eq)

wishboneMasterT :: forall addrWidth dat selWidth .
  ( KnownNat addrWidth
  , KnownNat selWidth
  , BitPack dat
  , NFDataX dat
  , selWidth ~ ByteSize dat
  , Show dat
  )
  => WishboneMasterState dat
  -> ( Bool
     , ( Df.Data (WishboneOperation addrWidth selWidth dat)
      , (Ack, WishboneS2M dat, ())
      )
     )
  -> ( WishboneMasterState dat
     , ( Ack
       , ( Df.Data (WishboneResult dat)
         , WishboneM2S addrWidth selWidth dat
         , Maybe Bit)
       )
     )
-- This operation is for another @AddressSpace@.
wishboneMasterT state (rst, (Df.Data WishboneOperation{_addrSpace=ConfigAddressSpace}, _))
  = (state, (Ack $ not rst, (Df.NoData, wbEmpty, Just 0)))
    where
      wbEmpty = emptyWishboneM2S
wishboneMasterT state (rst, (iFwd, (Ack oBwd, wbBwd, _)))
  = (trace ("WMNS: " <> show nextState) nextState, (Ack $ trace ("WMAck" <> show iBwd) iBwd, (oFwd, wbFwd, errBit)))
  where
    nextState = fsm (trace ("WMS " <> show state) state)
      (trace ("WMI: " <> show iFwd) iFwd) oBwd

    wbErr = err wbBwd || retry wbBwd
    wbTerm = acknowledge wbBwd || wbErr

    oFwd = case state of
      WaitForAck _ dat lst -> Df.Data $ WishboneResult dat lst
      _                    -> Df.NoData

    iBwd = not rst && case state of
      WaitForOp _  -> False
      Busy         -> False
      WaitForAck{} -> oBwd

    errBit = case (state, nextState) of
      (Busy, WaitForAck{}) -> Just $ boolToBit wbErr
      _                    -> Nothing

    -- The wishbone bus receives the incoming operation in the transition from
    -- @WaitForOp@ to @Busy@.
    wbFwd = case (state, iFwd) of
      (WaitForOp c, Df.NoData) -> wbEmpty   { strobe=False, busCycle=c }
      (WaitForOp _, Df.Data i) -> (wbPkt i) { strobe=True,  busCycle=True }
      (Busy, Df.NoData)        -> error "No input data in Busy state, this should be impossible!"
      (Busy, Df.Data i)        -> (wbPkt i) { strobe=True,  busCycle=True }
      (WaitForAck c _ _, _)    -> wbEmpty   { strobe=False, busCycle=c }
    wbPkt WishboneOperation{..} = WishboneM2S
      { addr                = _opAddr
      , writeData           = fromMaybe undefined _opDat
      , busSelect           = _opSel
      , lock                = False
      , busCycle            = undefined
      , strobe              = undefined
      , writeEnable         = isJust _opDat
      , cycleTypeIdentifier = Classic
      , burstTypeExtension  = LinearBurst
      }
    wbEmpty = emptyWishboneM2S

    fsm
      :: WishboneMasterState dat  -- state
      -> Df.Data (WishboneOperation addrWidth selWidth dat)  -- iFwd
      -> Bool                     -- oBwd
      -> WishboneMasterState dat  -- nextState
    fsm st@WaitForOp{} Df.NoData _ = st
    fsm WaitForOp{} (Df.Data WishboneOperation{..}) _
      | _opAbort  = WaitForOp False
      | otherwise = Busy
    fsm Busy{} Df.NoData _ = error "Sender did not keep Df channel constant!" 
    fsm Busy{} (Df.Data x) _
      | wbTerm    = WaitForAck (not $ _dropCyc x) (dat $ _opDat x) (_opLast x)
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
  :: forall dom addrWidth dat selWidth .
  ( HiddenClock dom
  , KnownNat addrWidth
  , BitPack dat
  , selWidth ~ ByteSize dat
  , NFDataX dat
  , Show dat
  )
  => Reset dom
  -> Circuit (Df.Df dom (WishboneOperation addrWidth selWidth dat))
             ( Df.Df dom (WishboneResult dat)
             , Wishbone dom Standard addrWidth dat
             , CSignal dom (Maybe Bit)
             )
wishboneMasterC rst = Circuit $ B.second unbundle . fsm . B.second bundle
  where
    fsm inp = withEnable enableGen $ withReset rst mealyB wishboneMasterT (WaitForOp False) (rst', bundle inp)
    rst' = unsafeToActiveHigh rst

    -- cnt = withReset rst $ withEnable enableGen register (0 :: Unsigned 16) (cnt+1)
{-# OPAQUE wishboneMasterC #-}
