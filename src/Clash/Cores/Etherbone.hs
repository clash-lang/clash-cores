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
import Clash.Cores.Etherbone.WishboneMaster (wishboneMasterC)
import Clash.Cores.Etherbone.RecordBuilder (recordBuilderC)
import qualified Protocols.Df as Df
import Protocols.Idle
import Clash.Cores.Etherbone.ConfigMaster (configMasterC)


recordHandlerC :: forall dom dataWidth addrWidth dat configRegs .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , KnownNat configRegs
  , BitPack dat
  , NFDataX dat
  , Show dat
  , ShowX dat
  , addrWidth <= dataWidth * 8
  , BitSize dat ~ dataWidth * 8
  , dataWidth ~ ByteSize dat
  , 4 <= dataWidth
  , Div 64 (BitSize dat) * BitSize dat ~ 64
  )
  => BitVector addrWidth
  -> Signal dom (Vec configRegs (BitVector 64))
  -> Circuit (PacketStream dom dataWidth EBHeader)
             ( PacketStream dom dataWidth EBHeader
             , Wishbone dom Standard addrWidth dat)
recordHandlerC sdbAddr userConfigRegs = circuit $ \psIn -> do
  dpkt <- recordDepacketizerC -< psIn

  (bypass, wbOp) <- recordProcessorC <| traceC "RecordIn" -< dpkt
  [wbmIn, cfgIn] <- Df.fanout -< wbOp

  (wbmRes, wbBus, wbmErr) <- hideReset wishboneMasterC -< wbmIn
  cfgRes <- configMasterC @_ @_ @dat sdbAddr userConfigRegs -< (cfgIn, wbmErr)

  psOut <- recordBuilderC -< (bypass, cfgRes, wbmRes)
  idC -< (psOut, wbBus)
{-# OPAQUE recordHandlerC #-}


etherboneC :: forall dom dataWidth addrWidth dat configRegs .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , KnownNat configRegs
  , BitPack dat
  , NFDataX dat
  , Show dat
  , ShowX dat
  , BitSize dat ~ dataWidth * 8
  , addrWidth <= dataWidth * 8
  , dataWidth ~ ByteSize dat -- This needs to go...
  , 4 <= dataWidth
  , Div 64 (BitSize dat) * BitSize dat ~ 64
  )
  => BitVector addrWidth
  -> Signal dom (Vec configRegs (BitVector 64))
  -> Circuit (PacketStream dom dataWidth ())
             ( PacketStream dom dataWidth ()
             , Wishbone dom Standard addrWidth dat)
etherboneC sdbAddr userConfigRegs = circuit $ \psIn -> do
  [probe, record] <- receiverC (SNat @addrWidth) <| etherboneDepacketizerC <| traceC "PSIn" -< psIn

  probeOut <- probeHandlerC (SNat @addrWidth) -< probe
  (recordOut, wbmBus) <- recordHandlerC sdbAddr userConfigRegs -< record

  recordOut' <- traceC "BuilderOut/ArbIn" -< recordOut

  pktOut <- packetArbiterC RoundRobin -< [recordOut', probeOut]
  udpTx <- etherbonePacketizerC <| traceC "EBPktIn" -< pktOut

  idC -< (udpTx, wbmBus)
{-# OPAQUE etherboneC #-}
