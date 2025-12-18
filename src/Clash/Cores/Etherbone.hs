{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Clash.Cores.Etherbone (
  etherboneC
) where

import Clash.Cores.Etherbone.Base
import Clash.Cores.Etherbone.ConfigMaster (configMasterC, ConfigReg)
import Clash.Cores.Etherbone.RecordBuilder (recordBuilderC)
import Clash.Cores.Etherbone.RecordProcessor (recordProcessorC)
import Clash.Cores.Etherbone.WishboneMaster (wishboneMasterC)
import Clash.Prelude
import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream
import Protocols.Wishbone


recordHandlerC :: forall dom dataWidth addrWidth configRegs .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , KnownNat configRegs
  , 1 <= dataWidth * 8
  , addrWidth <= dataWidth * 8
  , 4 <= dataWidth
  , DivRU 8 dataWidth * (dataWidth * 8) ~ 64
  )
  -- | Self-describing bus base address
  => BitVector addrWidth
  -- | Optional user-defined config registers
  -> Signal dom (Vec configRegs ConfigReg)
  -> Circuit (PacketStream dom dataWidth EBHeader)
             ( PacketStream dom dataWidth EBHeader
             , Wishbone dom Standard addrWidth dataWidth)
recordHandlerC sdbAddr userConfigRegs = circuit $ \psIn -> do
  dpkt <- recordDepacketizerC -< psIn

  (bypass, wbOp) <- recordProcessorC -< dpkt
  [wbmIn, cfgIn] <- Df.fanout -< wbOp

  (wbmRes, wbBus, wbmErr) <- wishboneMasterC -< wbmIn
  cfgRes <- configMasterC sdbAddr userConfigRegs -< (cfgIn, wbmErr)

  psOut <- recordBuilderC -< (bypass, cfgRes, wbmRes)
  idC -< (psOut, wbBus)
{-# OPAQUE recordHandlerC #-}

-- | The top Etherbone circuit.
--
-- @sdbAddr@ is the [Self-describing bus](https://ohwr.org/project/fpga-config-space)
-- data base address. Each Etherbone node is supposed to have a SDB structure
-- connected to the Wishbone bus according to the spec. It does function without
-- but the default tooling and APIs from CERN make use of this.
--
-- @userConfigRegs@ are additional 64-bit registers that are placed in the
-- config-space. The current implementation of the config-space is read-only.
--
-- The Wishbone bus attatched to this circuit determines the bus and address
-- widths.
etherboneC :: forall dom dataWidth addrWidth configRegs .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , KnownNat configRegs
  , 1 <= dataWidth * 8
  , addrWidth <= dataWidth * 8
  , 4 <= dataWidth
  , DivRU 8 dataWidth * (dataWidth * 8) ~ 64
  )
  -- | Self-describing bus base address
  => BitVector addrWidth
  -- | Optional user-defined config registers
  -> Signal dom (Vec configRegs ConfigReg)
  -> Circuit (PacketStream dom dataWidth ())
             ( PacketStream dom dataWidth ()
             , Wishbone dom Standard addrWidth dataWidth)
etherboneC sdbAddr userConfigRegs = circuit $ \psIn -> do
  [probe, record] <- receiverC (SNat @addrWidth) <| etherboneDepacketizerC -< psIn

  probeOut <- probeHandlerC (SNat @addrWidth) -< probe

  record' <- etherbonePaddingStripperC -< record
  (recordOut, wbmBus) <- recordHandlerC sdbAddr userConfigRegs -< record'
  recordOut' <- etherbonePaddingAdderC -< recordOut

  pktOut <- packetArbiterC Df.Parallel -< [recordOut', probeOut]
  udpTx <- etherbonePacketizerC -< pktOut

  idC -< (udpTx, wbmBus)
{-# OPAQUE etherboneC #-}
