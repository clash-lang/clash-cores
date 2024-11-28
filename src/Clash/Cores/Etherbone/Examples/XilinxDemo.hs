{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}


-- | This file implements a top entity for a Xilinx device (xcku040) with SGMII.
-- It uses the Ethernet and Etherbone cores.
--
-- It can be compiled with
-- @cabal run clash -- Clash.Cores.Etherbone.Examples.XilinxDemo --verilog@

module Clash.Cores.Etherbone.Examples.XilinxDemo where

import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog
import Clash.Cores.Etherbone.Examples.FullEthernetCircuit (fullCircuitStripped)
import Clash.Cores.Ethernet.Examples.FullUdpStack
import Clash.Cores.Ethernet.IPv4
import Clash.Cores.Ethernet.Mac
import Clash.Cores.Xilinx.Ethernet.Gmii
import Clash.Explicit.Prelude (delay, unsafeSynchronizer)
import Clash.Prelude hiding (delay)
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Protocols
import Protocols.PacketStream

import Data.Maybe


myMacAddress :: MacAddress
myMacAddress = MacAddress (0xDE :> 0xAD :> 0xBE :> 0xEF :> 0x01 :> 0x02 :> Nil)

myIPAddress :: IPv4Address
myIPAddress = IPv4Address $ 0xa :> 0x0 :> 0x0 :> 0x2 :> Nil

createDomain vXilinxSystem{vName="Ext125", vPeriod= hzToPeriod 125e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="Basic125A", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic125B", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic625", vPeriod=hzToPeriod 625e6, vResetKind=Asynchronous}

$(deriveHardwareCrc Crc32_ethernet d8 d1)
$(deriveHardwareCrc Crc32_ethernet d8 d4)

unsafeGmiiRxC :: forall domRx.
  ( HiddenClockResetEnable domRx )
  => Circuit (CSignal domRx Gmii) (PacketStream domRx 1 ())
unsafeGmiiRxC = Circuit ckt
  where
    ckt (gmiiIn, _) = (pure (), psOut <$> prevInput <*> gmiiIn)
      where
        prevInput = register @domRx gmiiDefault gmiiIn
          where gmiiDefault = Gmii 0 0 0

        psOut Gmii{gmiiValid = 0} Gmii{gmiiValid = 0} = Nothing
        psOut Gmii{gmiiValid = 1, gmiiData, gmiiError} Gmii{gmiiValid = 0}
          = Just $ PacketStreamM2S (singleton gmiiData) (Just maxBound) () (bitToBool gmiiError)
        psOut Gmii{gmiiData, gmiiError} _
          = Just $ PacketStreamM2S (singleton gmiiData) Nothing () (bitToBool gmiiError)
{-# OPAQUE unsafeGmiiRxC #-}

gmiiTxC
  :: ( HiddenClockResetEnable domTx )
  => Signal domTx Bool  -- TX ready
  -> Circuit (PacketStream domTx 1 ()) (CSignal domTx Gmii)
gmiiTxC rdy = Circuit ckt
  where
    -- Generates back-pressure while tx is not ready.
    ckt (psIn, _) = (PacketStreamS2M <$> rdy, gmiiOut <$> psIn)
      where
        gmiiOut i = Gmii (dat i) (valid i) (abort i)
        dat = maybe 0 (head . _data)
        valid = boolToBit . isJust
        abort = maybe 0 (boolToBit . _abort)
{-# OPAQUE gmiiTxC #-}

glue
  :: forall domSys domRx domTx . (KnownDomain domSys, KnownDomain domRx, KnownDomain domTx)
  => Clock domSys
  -> Reset domSys
  -> Clock domRx
  -> Reset domRx
  -> Clock domTx
  -> Reset domTx
  -> Signal domRx Gmii  -- Gmii
  -> Signal domTx Bool -- txRdy
  -> Signal domSys IPv4Address
  -> Signal domTx Gmii
glue clk rst rxClk rxRst txClk txRst rxGmii txRdy ipAddr = txGmii
  where
    mac = pure myMacAddress
    ipSubnet = let subnet = IPv4SubnetMask (0xff :> 0xff :> 0xff :> 0x00 :> Nil)
      in bundle (ipAddr, pure subnet)

    txGmii = out
      where
        out = snd $ toSignals ckt (rxGmii, pure ())

    ethStack = (exposeClockResetEnable fullStackC clk rst enableGen) rxClk rxRst enableGen txClk txRst enableGen mac ipSubnet
    ckt = circuit $ \gmiiIn -> do
      ethPsIn <- withClockResetEnable rxClk rxRst enableGen unsafeGmiiRxC -< gmiiIn
      (udpIn, ethPsOut) <- ethStack -< (udpOut, ethPsIn)
      udpOut <- withClockResetEnable clk rst enableGen fullCircuitStripped -< udpIn
      gmiiOut <- withClockResetEnable txClk txRst enableGen gmiiTxC txRdy -< ethPsOut
      idC -< gmiiOut
{-# OPAQUE glue #-}

glueEthernet
  :: Clock Basic125B
  -> Reset Basic125B
  -> DiffClock Basic625
  -> Signal Basic625 Lvds
  -> Signal Basic625 Lvds
glueEthernet sysClk sysRst sgmiiPhyClk sgmiiIn = bridgeLvdsOut
 where
  BridgeOutput{..} = bridge sgmiiIn gmiiOut
  signalDetect = pure True
  anRestart = pure False
  conf = pure def{cAutoNegEnable = True}
  anConf =
    pure
      def
        { cAcknowledge = True
        , cDuplexMode = FullDuplex
        , cLinkSpeed = Speed1000
        , cPhyLinkStatus = True
        }
  bridge = gmiiSgmiiBridge sgmiiPhyClk bridgeRst signalDetect conf anConf anRestart
  rxClk = bridgeClk125 :: Clock Basic125A
  rxRst = bridgeRst125
  bridgeRst = unsafeResetDesynchronizer sysClk sysRst
  myIp = pure myIPAddress
  gmiiOut = glue sysClk sysRst rxClk rxRst rxClk rxRst bridgeGmiiRx (pure True) myIp

glueTop
  :: DiffClock Ext125
  -> Reset Ext125
  -> DiffClock Basic625
  -> Signal Basic625 Lvds
  -> Signal Basic625 Lvds
glueTop diffClk cpuReset sgmiiClk inp = outp
 where
  (sysClk, sysRst) = clockWizardDifferential diffClk cpuReset
  outp = glueEthernet sysClk sysRst sgmiiClk inp
{-# OPAQUE glueTop #-}
{-# ANN
  glueTop
  ( Synthesize
      { t_name = "glueTop"
      , t_inputs =
          [ PortProduct
              "CLK_125MHZ"
              [PortName "p", PortName "n"]
          , PortName "CPU_RESET"
          , PortProduct
              "SGMIICLK"
              [PortName "p", PortName "n"]
          , PortProduct
              "SGMII"
              [PortName "RX_p", PortName "RX_n"]
          ]
      , t_output =
          PortProduct
            "SGMII"
            [PortName "TX_p", PortName "TX_n"]
      }
  )
  #-}


-- Taken from bittide-hardware
-- https://github.com/bittide/bittide-hardware/blob/8f6a4ddcb0477ec7f5f33b636b4c96d82bf64af1/bittide-instances/src/Bittide/Instances/Pnr/Ethernet.hs
unsafeResetDesynchronizer ::
  forall domA domS.
  (KnownDomain domA, KnownDomain domS, HasSynchronousReset domS, HasAsynchronousReset domA) =>
  -- | Clock in the source domain
  Clock domS ->
  -- | Synchronous reset in the source domain
  Reset domS ->
  -- | Asynchronous reset in the "target" domain
  Reset domA
unsafeResetDesynchronizer clkIn =
  unsafeFromActiveHigh
    . unsafeSynchronizer clkIn clockGen
    . unsafeToActiveHigh
    . delayReset Asserted clkIn

data Asserted = Asserted | Deasserted

-- Taken from bittide-hardware
-- https://github.com/bittide/bittide-hardware/blob/8f6a4ddcb0477ec7f5f33b636b4c96d82bf64af1/bittide-instances/src/Bittide/Instances/Pnr/Ethernet.hs
delayReset ::
  (HasSynchronousReset dom) =>
  -- | Initial and reset value of register
  Asserted ->
  Clock dom ->
  Reset dom ->
  Reset dom
delayReset asserted clk =
  unsafeFromActiveHigh
    . delay clk enableGen assertedBool
    . unsafeToActiveHigh
 where
  assertedBool =
    case asserted of
      Asserted -> True
      Deasserted -> False
