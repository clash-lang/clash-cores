{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Clash.Cores.Etherbone (
  etherboneC
) where

import Protocols
import Protocols.PacketStream
import Protocols.Wishbone
import Clash.Prelude
import Clash.Cores.Etherbone.Base
import Clash.Cores.Etherbone.RecordProcessor (recordProcessorC)
import Clash.Cores.Etherbone.WishboneMaster (wishboneMasterC, ByteSize)
import Clash.Cores.Etherbone.RecordBuilder (recordBuilderC)
import qualified Protocols.Df as Df
import Protocols.Idle


recordHandlerC :: forall dom dataWidth addrWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , NFDataX dat
  , Show dat
  , ShowX dat
  , addrWidth <= dataWidth * 8
  , BitSize dat ~ dataWidth * 8
  , dataWidth ~ ByteSize dat
  , 4 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth EBHeader)
             ( PacketStream dom dataWidth EBHeader
             , Wishbone dom Standard addrWidth dat)
recordHandlerC = circuit $ \psIn -> do
  dpkt <- recordDepacketizerC -< psIn

  (bypass, wbOp) <- recordProcessorC <| traceC "RecordIn" -< dpkt
  --[wbmIn, cfgIn] <- Df.fanout -< wbOp

  (wbmRes, wbBus, wbmErr) <- hideReset wishboneMasterC -< wbOp

  -- TODO: Possibly idleSink keeps Ack False. Then the fanout always gives
  -- backpressure
  --idleSink -< cfgIn
  cfgRes <- idleSource

  psOut <- recordBuilderC -< (bypass, cfgRes, wbmRes)
  signalSink -< wbmErr

  -- bypass' <- traceC "bypass" -< bypass
  -- procOut' <- traceC "ProcOut/BuilderIn" -< procOut
  -- psOut <- recordBuilderC (SNat @addrWidth) -< (procOut', bypass')
  -- signalSink -< wbmErr

  idC -< (psOut, wbBus)

  where
    signalSink :: Circuit (CSignal dom a) ()
    signalSink = Circuit $ const (pure (), ())


etherboneC :: forall dom dataWidth addrWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , NFDataX dat
  , Show dat
  , ShowX dat
  , BitSize dat ~ dataWidth * 8
  , addrWidth <= dataWidth * 8
  , dataWidth ~ ByteSize dat -- This needs to go...
  , 4 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth ())
             ( PacketStream dom dataWidth ()
             , Wishbone dom Standard addrWidth dat)
etherboneC = circuit $ \psIn -> do
  [probe, record] <- receiverC (SNat @addrWidth) <| etherboneDepacketizerC <| traceC "PSIn" -< psIn

  probeOut <- probeHandlerC (SNat @addrWidth) -< probe
  (recordOut, wbmBus) <- recordHandlerC -< record

  recordOut' <- traceC "BuilderOut/ArbIn" -< recordOut

  pktOut <- packetArbiterC RoundRobin -< [recordOut', probeOut]
  udpTx <- etherbonePacketizerC <| traceC "EBPktIn" -< pktOut

  idC -< (udpTx, wbmBus)
