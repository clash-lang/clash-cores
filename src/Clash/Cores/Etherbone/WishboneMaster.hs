{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.WishboneMaster where

import Clash.Prelude
import Protocols
import Protocols.Wishbone
import qualified Data.Bifunctor as B
import qualified Protocols.Df as Df
import Data.Maybe
import qualified Prelude as P
-- import Debug.Trace

type ByteSize dat = BitSize dat `DivRU` 8

data WishboneMasterInput addrWidth selWidth dat
  = WishboneMasterInput
  { _addr   :: BitVector addrWidth
  , _dat    :: Maybe dat
  , _sel    :: BitVector selWidth
  , _last   :: Bool
  , _abort  :: Bool
  } deriving (Generic, NFDataX, Show)

data WishboneMasterOutput dat
  = WishboneMasterOutput
  { _wbDatOut  :: Maybe dat
  , _wbLastOut :: Bool
  }

data WishboneMasterState addrWidth selWidth dat
  = WaitForOp  { _cyc :: Bool }
  | Busy       { _input :: WishboneMasterInput addrWidth selWidth dat }
  -- Wait till Df was acked. If new data is already availible jump back to Busy.
  -- Otherwise jump to WaitForOp
  | WaitForAck { _cyc :: Bool, _retDat :: dat }
  deriving (Generic, NFDataX, Show)


{-
-- TODO: Property test this transfer function
--
-- Backpressure is generated and handled through the Df line. If the Processor
-- is receives backpressure, it does not sent an Ack to the WBM.
--
-- This state machine should be abortable. We can use the Maybe Input line. As
-- long as this is Just, do the Wishbone transaction. If it becomes Nothing
-- while still busy, abort the transaction (cyc and stb low).
-- If waiting for ack, this can also be aborted with Nothing on Fwd.
-- If waiting for op this does not work, as it can wait in
--
-- Option 2, add a second signal on the fwd line, telling WBM to abort.
wishboneMasterT ::
  ( KnownNat addrWidth
  , BitPack dat
  , NFDataX dat
  , Show dat
  , ShowX dat
  )
  => WishboneMasterState addrWidth dat
  -> ( Maybe (WishboneMasterInput addrWidth dat)
     , (Ack, WishboneS2M dat, ()), Unsigned 8
     )
  -> ( WishboneMasterState addrWidth dat
     , ((), ( Df.Data dat
            , WishboneM2S addrWidth (ByteSize dat) dat
            , Maybe Bit
            ))
     )
wishboneMasterT state (input, (ack, wbBwd, ()), count)
  = (newState, ((), (retData, wbFwd, retErr)))
  where
    newState = fsm state input wbBwd (trace ("Ack " <> show count <> ": " <> show ack) ack)

    wbErr = err wbBwd || retry wbBwd
    wbAck = acknowledge wbBwd || wbErr

    -- retData = trace ("retData: " <> show retData') retData'
    -- retData = case (state, newState) of
    --   (Busy _, WaitForAck _ dat) -> Df.Data dat
    --   (WaitForAck _ dat, _)      -> Df.Data dat
    --   _                          -> Df.NoData
    retData = case state of
      WaitForAck _ dat -> Df.Data dat
      _                -> Df.NoData

    retErr = case (state, newState) of
      (Busy _, WaitForAck _ _) -> Just $ boolToBit wbErr
      _                        -> Nothing

    -- Set CYC low IF dropCyc is set. Otherwise it should be kept high, meaning
    -- that cyc needs to be a state variable.

    -- WaitForOp:  Directly forward incoming request to the WB bus
    -- Busy:       Always one cycle wait time for WB ack (WB classic)
    -- WaitForAck: Always (at least) one cycle STB low (WB classic)
    wbFwd = trace ("state " <> show count <> ": " <> show state <> " -- wbFwd: " <> show wbFwd') wbFwd'
    wbFwd' = case state of
      --WaitForOp c -> case newState of
      WaitForOp c -> case newState of
        WaitForOp _     -> wbEmpty   { strobe=False, busCycle=c }
        Busy i          -> (wbPkt i) { strobe=True,  busCycle=True }
        WaitForAck _ _  -> error "WBM: Cannot go from WaitForOp to WaitForAck."
      Busy i            -> (wbPkt i) { strobe=True,  busCycle=True }
      WaitForAck c _    -> wbEmpty   { strobe=False, busCycle=c }

    wbPkt WishboneMasterInput{..} = WishboneM2S
      { addr                = _addr
      , writeData           = fromMaybe undefined _dat
      , busSelect           = _sel
      , lock                = False
      , busCycle            = undefined
      , strobe              = undefined
      , writeEnable         = isJust _dat
      , cycleTypeIdentifier = Classic
      , burstTypeExtension  = LinearBurst
      }
    wbEmpty = emptyWishboneM2S

    fsm st@WaitForOp{} Nothing _ _ = st
    fsm WaitForOp{} (Just x) _ _ = Busy x
    fsm Busy{..} _ WishboneS2M{..} _
      | wbAck     = WaitForAck (not $ _dropCyc _input) readData
      | otherwise = Busy _input
    fsm st@WaitForAck{} _ _ (Ack False) = st
    fsm WaitForAck{..} _ _ (Ack True) = WaitForOp _cyc
    -- fsm WaitForAck{} (Just x) _ (Ack True) = Busy x
-}

{-
wishboneMasterC
  :: forall dom addrWidth dat .
  ( HiddenClockResetEnable dom, KnownNat addrWidth, BitPack dat, NFDataX dat, Show dat, ShowX dat )
  => Circuit (CSignal dom (Maybe (WishboneMasterInput addrWidth (ByteSize dat) dat)))
             (Df dom dat, Wishbone dom Standard addrWidth dat, CSignal dom (Maybe Bit))
wishboneMasterC = Circuit go
  where
    go (fwd, bwd) = (B.second unbundle . fsm . B.second bundle) $ seq tr (fwd, bwd)
      where
        -- tr = traceSignal1 "foo" fwd
        tr = id
    fsm (fwd, bwd) = mealyB wishboneMasterT (WaitForOp False) (fwd, bwd, count)

    count = register 0 (count + 1)
-}

{-
wishboneMasterC ::
  Circuit (Df dom (WishboneMasterInput addrWidth dat))
          (Df dom (WishboneMasterOutput dat), CSignal dom (Maybe Bit), Wishbone dom Standard addrWidth dat)
wishboneMasterC = Circuit (B.second unbundle . go . B.second bundle)
  where
    go = mealyB wishboneMasterT _
-}


type WBData = BitVector 32

wbmInput :: [( Maybe (WishboneMasterInput 32 (ByteSize WBData) WBData), (Ack, WishboneS2M WBData, ()) )]
wbmInput = [ pkt Nothing False emptyWishboneS2M
           , pkt Nothing False emptyWishboneS2M
           -- (WaitForOp) New op
           , pkt (Just (0xaaaaaaaa, Nothing, 0xf, False))
                         False emptyWishboneS2M
           -- (Busy) WB data returned and acked
           , pkt (Just (0xaaaaaaaa, Nothing, 0xf, False))
                         False (emptyWishboneS2M @WBData) { readData=0xdeadbeef, err=True}
           -- (WaitForAck) Read data sent over Df, not acked yet
           , pkt (Just (0xaaaaaaaa, Nothing, 0xf, False))
                         False emptyWishboneS2M
           -- (WaitForAck) Ack received with NO new data
           , pkt Nothing True emptyWishboneS2M
           -- (WaitForOp) New op
           , pkt (Just (0xbbbbbbbb, Just 0xcafecafe, 0xf, True))
                         False emptyWishboneS2M
           -- (Busy) WB ack
           , pkt Nothing False (emptyWishboneS2M @WBData) { readData=0xdeadbeef, acknowledge=True}
           -- (WaitForAck) Data sent, and directly acked
           , pkt Nothing True emptyWishboneS2M

           -- Here we keep data on the wbinput bus
           -- (WaitForOp) New op
           , pkt (Just (0xbbbbbbbb, Just 0xcafecafe, 0xf, True))
                         False emptyWishboneS2M
           -- (Busy) WB ack
           , pkt (Just (0xbbbbbbbb, Just 0xcafecafe, 0xf, True))
                         False (emptyWishboneS2M @WBData) { readData=0xff00ff00, acknowledge=True }
           -- (WaitForAck) Data sent, and directly acked
           , pkt (Just (0xbbbbbbbb, Just 0xcafecafe, 0xf, True))
                         True emptyWishboneS2M
           , pkt Nothing False emptyWishboneS2M
           ]
  where
    pkt
      :: Maybe (BitVector 32, Maybe WBData, BitVector (ByteSize WBData), Bool)
      -> Bool -> WishboneS2M WBData
      -> ( Maybe (WishboneMasterInput 32 (ByteSize WBData) WBData), (Ack, WishboneS2M WBData, ()) )
    pkt Nothing ack wb = ( Nothing, (Ack ack, wb, ()) )
    pkt (Just (_addr, _dat, _sel, _dropCyc)) ack wb = ( Just (WishboneMasterInput{..}), (Ack ack, wb, ()) )

type WbmInput addrWidth dat  = ( Maybe (WishboneMasterInput addrWidth (ByteSize dat) dat)
                               , (Ack, WishboneS2M dat, ()) )

type WbmOutput addrWidth dat = ( (), ( Df.Data dat
                                     , WishboneM2S addrWidth (ByteSize dat) dat
                                     , Maybe Bit) )

-- wbmSim :: forall dat addrWidth .
--   ( KnownNat addrWidth
--   , BitPack dat
--   , NFDataX dat
--   , Show dat
--   , ShowX dat
--   )
--   => [WbmInput addrWidth dat]
--   -> ([WishboneMasterState addrWidth (ByteSize dat) dat], [WbmOutput addrWidth dat])
-- wbmSim inputs = P.foldl fn ([WaitForOp False], []) inputs
--   where
--     fn :: ( [WishboneMasterState addrWidth (ByteSize dat) dat]
--           , [WbmOutput addrWidth dat])
--        -> WbmInput addrWidth dat
--        -> ( [WishboneMasterState addrWidth (ByteSize dat) dat]
--           , [WbmOutput addrWidth dat])
--     fn (ctxs, results) input = (ctxs P.++ [newCtx], results P.++ [result])
--       where
--         (newCtx, result) = wishboneMasterT (P.last ctxs) input


-- Run with
-- mapM_ putStrLn [show x P.++ "\n\t" P.++ show y | (x, y) <- uncurry P.zip (wbmSim wbmInput)]
