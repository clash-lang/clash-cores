{-# LANGUAGE NamedFieldPuns #-}

{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

KCU105 test design: the Clash SGMII PCS connected to the on-board PHY through
the LVDS SERDES, echoing received frames back. Controls and observations go
through a VIO, waveforms through an ILA, and a few LEDs show the link state.
-}
module Kcu105.Sgmii.Top where

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Sgmii (sgmii)
import Clash.Cores.Sgmii.Common (Rudi (..), SgmiiStatus (..), Status (..), Xmit (..))
import Clash.Cores.Xilinx.Ibufds (ibufdsClock)
import Clash.Cores.Xilinx.Ila (Depth (..), IlaConfig (..), ila, ilaConfig)
import Clash.Cores.Xilinx.Vio (vioProbe)
import Clash.Explicit.Prelude
import Clash.Signal.Internal (DiffClock (..))
import Clash.Xilinx.ClockGen (clockWizardDifferential)
import Kcu105.Sgmii.Domains
import Kcu105.Sgmii.Gearbox (FifoErrors)
import Kcu105.Sgmii.Serdes

-- | Controls, driven by the VIO in hardware
data Control = Control
  { ctrlTap :: Unsigned 9
  -- ^ Input delay tap of the receive line
  , ctrlRxReverse :: Bool
  -- ^ Reverse the bit order of the deserializer output
  , ctrlTxReverse :: Bool
  -- ^ Reverse the bit order of the serializer input
  , ctrlPcsReset :: Bool
  -- ^ Hold the PCS in reset
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Default controls
defaultControl :: Control
defaultControl = Control 0 False False False

-- | Observations, shown by the VIO in hardware
data Observe = Observe
  { obsLocked :: Bool
  , obsBsOk :: Bool
  , obsSyncOk :: Bool
  , obsLinkSpeed :: BitVector 2
  , obsXmit :: BitVector 2
  , obsRudi :: BitVector 2
  , obsTap :: Unsigned 9
  , obsRxFifo :: FifoErrors
  , obsTxFifo :: FifoErrors
  , obsFrames :: Unsigned 32
  , obsRxErrors :: Unsigned 32
  , obsBytes :: Unsigned 32
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Everything the design produces, for the top entity and for simulation
data Demo = Demo
  { demoTxP :: Signal Line1250 Bit
  , demoTxN :: Signal Line1250 Bit
  , demoClkPcs :: Clock Pcs125
  , demoObserve :: Signal Pcs125 Observe
  , demoStatus :: Signal Pcs125 SgmiiStatus
  , demoRxCg :: Signal Pcs125 (BitVector 10)
  , demoRxDw :: Signal Pcs125 (BitVector 8)
  , demoRxDv :: Signal Pcs125 Bool
  , demoRxEr :: Signal Pcs125 Bool
  , demoTxCg :: Signal Pcs125 (BitVector 10)
  , demoBsCg :: Signal Pcs125 (BitVector 10)
  }

-- | The design without the top-level annotation, so that it can be simulated
sgmiiDemo ::
  -- | Free-running board clock
  Clock Ext125 ->
  -- | Board reset, synchronised
  Reset Ext125 ->
  -- | 625 MHz clock from the PHY
  DiffClock Phy625 ->
  -- | Receive line, P channel
  Signal Line1250 Bit ->
  -- | Receive line, N channel
  Signal Line1250 Bit ->
  -- | Controls
  Signal Pcs125 Control ->
  Demo
sgmiiDemo clk125 rst125 diffClk625@(DiffClock clk625 _) rxP rxN control =
  Demo
    { demoTxP = txP
    , demoTxN = txN
    , demoClkPcs = clkPcs
    , demoObserve = observe
    , demoStatus = status
    , demoRxCg = rxCg
    , demoRxDw = rxDw
    , demoRxDv = rxDv
    , demoRxEr = rxEr
    , demoTxCg = txCg
    , demoBsCg = bsCg
    }
 where
  -- The MMCM is reset by the board reset. The synchroniser is a wire in
  -- hardware; the MMCM treats its reset asynchronously.
  rstPhy :: Reset Phy625
  rstPhy = unsafeFromActiveHigh (unsafeSynchronizer clk125 clk625 (unsafeToActiveHigh rst125))

  (clkSer, _rstSer, clkDiv, rstDiv, clkPcs, rstMmcm) =
    clockWizardDifferential
      @( Clock Serdes625
       , Reset Serdes625
       , Clock Serdes312
       , Reset Serdes312
       , Clock Pcs125
       , Reset Pcs125
       )
      diffClk625
      rstPhy

  rstPcs = orReset rstMmcm (unsafeFromActiveHigh (ctrlPcsReset <$> control))
  reg :: (NFDataX a) => a -> Signal Pcs125 a -> Signal Pcs125 a
  reg = register clkPcs rstPcs enableGen

  (rxCg, tapOut, rxFifo) =
    rxPath clkSer clkDiv rstDiv clkPcs rstPcs (ctrlTap <$> control) (ctrlRxReverse <$> control) rxP rxN

  -- Receive and transmit run on the same clock, so the clock domain crossing
  -- inside the SGMII core is the identity
  (status, rxDv, rxEr, rxDw, bsCg, txCg) =
    sgmii (\_ _ a b c -> (a, b, c)) clkPcs clkPcs rstPcs rstPcs txEn txEr txDw rxCg

  -- Echo received frames
  txEn = reg False rxDv
  txEr = reg False rxEr
  txDw = reg 0 rxDw

  (txP, txN, txFifo) =
    txPath clkSer clkDiv rstDiv clkPcs rstPcs (ctrlTxReverse <$> control) txCg

  locked = not <$> unsafeToActiveHigh rstMmcm
  frames = counter (rxDv .&&. (not <$> reg False rxDv))
  rxErrors = counter rxEr
  bytes = counter rxDv
  counter :: Signal Pcs125 Bool -> Signal Pcs125 (Unsigned 32)
  counter en = c where c = reg 0 (mux en (c + 1) c)

  observe =
    Observe
      <$> locked
      <*> (isOk . _cBsStatus <$> status)
      <*> (isOk . _cSyncStatus <$> status)
      <*> (pack . _cLinkSpeed <$> status)
      <*> (pack . _cXmit <$> status)
      <*> (rudiBits . _cRudi <$> status)
      <*> tapOut
      <*> rxFifo
      <*> txFifo
      <*> frames
      <*> rxErrors
      <*> bytes

  isOk Ok = True
  isOk Fail = False

  rudiBits (RudiC _) = 1
  rudiBits RudiI = 2
  rudiBits RudiInvalid = 3

-- | Top entity for the KCU105
topEntity ::
  "CLK_125MHZ" ::: DiffClock Ext125 ->
  "CPU_RESET" ::: Reset Ext125 ->
  "SGMIICLK" ::: DiffClock Phy625 ->
  "SGMII_RX_p" ::: Signal Line1250 Bit ->
  "SGMII_RX_n" ::: Signal Line1250 Bit ->
  ( "SGMII_TX_p" ::: Signal Line1250 Bit
  , "SGMII_TX_n" ::: Signal Line1250 Bit
  , "GPIO_LED" ::: Signal Ext125 (BitVector 8)
  )
topEntity diffClk125 cpuReset diffClk625 rxP rxN = hwSeqX ilaSig (demoTxP, demoTxN, leds)
 where
  clk125 = ibufdsClock diffClk125
  rst125 = resetSynchronizer clk125 cpuReset

  Demo{demoTxP, demoTxN, demoClkPcs = clkPcs, demoObserve, demoStatus, demoRxCg, demoRxDw, demoRxDv, demoRxEr} =
    sgmiiDemo clk125 rst125 diffClk625 rxP rxN control

  control :: Signal Pcs125 Control
  control =
    setName @"vioSgmii"
      $ vioProbe
        ( "vio_locked"
            :> "vio_bs_ok"
            :> "vio_sync_ok"
            :> "vio_link_speed"
            :> "vio_xmit"
            :> "vio_rudi"
            :> "vio_tap"
            :> "vio_rx_fifo_errors"
            :> "vio_tx_fifo_errors"
            :> "vio_frames"
            :> "vio_rx_errors"
            :> "vio_bytes"
            :> Nil
        )
        ("vio_ctrl_tap" :> "vio_ctrl_rx_reverse" :> "vio_ctrl_tx_reverse" :> "vio_ctrl_pcs_reset" :> Nil)
        defaultControl
        clkPcs
        demoObserve

  syncOk = obsSyncOk <$> demoObserve
  bsOk = obsBsOk <$> demoObserve

  ilaSig :: Signal Pcs125 ()
  ilaSig =
    setName @"ilaSgmii"
      $ ila
        ( ( ilaConfig
              ( "ila_trigger"
                  :> "ila_capture"
                  :> "ila_rx_cg"
                  :> "ila_rx_dw"
                  :> "ila_rx_dv"
                  :> "ila_rx_er"
                  :> "ila_sync_ok"
                  :> "ila_bs_ok"
                  :> "ila_xmit"
                  :> Nil
              )
          )
            { depth = D8192
            }
        )
        clkPcs
        demoRxDv
        (pure True :: Signal Pcs125 Bool)
        demoRxCg
        demoRxDw
        demoRxDv
        demoRxEr
        syncOk
        bsOk
        (pack . _cXmit <$> demoStatus)

  -- LEDs: locked, comma alignment, sync, link up, frame activity, receive
  -- FIFO error, transmit FIFO error, heartbeat of the board clock
  heartbeat = register clk125 rst125 enableGen (0 :: Unsigned 27) (heartbeat + 1)
  ledBits =
    ( \Observe{obsLocked, obsBsOk, obsSyncOk, obsXmit, obsFrames, obsRxFifo, obsTxFifo} ->
        pack
          ( obsLocked
          , obsBsOk
          , obsSyncOk
          , obsXmit == pack Data
          , testBit obsFrames 4
          , pack obsRxFifo /= 0
          , pack obsTxFifo /= 0
          )
    )
      <$> demoObserve
  leds =
    register clk125 rst125 enableGen 0
      $ (\l h -> pack (msb h) ++# l)
      <$> unsafeSynchronizer clkPcs clk125 ledBits
      <*> heartbeat

makeTopEntity 'topEntity
