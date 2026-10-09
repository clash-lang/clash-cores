{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Clash.Cores.Ethernet.Rgmii
Description : Functions and types to connect an RGMII PHY to a packet stream interface.

To keep this module generic, users will have to provide their own "primitive" functions:

    1. delay functions to set to the proper amount of delay (which can be different for RX and TX);
    2. iddr function to turn a single DDR (Double Data Rate) signal into 2 non-DDR signals;
    3. oddr function to turn two non-DDR signals into a single DDR signal.

Note that Clash models a DDR signal as being twice as fast, thus both facilitating
and requiring type-level separation between the two "clock domains".
-}
module Clash.Cores.Ethernet.Rgmii (
  RgmiiChannel (..),
  rgmiiReceiver,
  rgmiiTransmitter,
  rgmiiRxC,
  rgmiiTxC,
) where

import Clash.Explicit.DDR (ddrForwardClock)
import qualified Clash.Explicit.Signal as E
import Clash.Prelude

import Protocols
import Protocols.PacketStream

import Data.Maybe (isJust, isNothing)

-- | Channel from/to the RGMII PHY
data RgmiiChannel dom domDDR = RgmiiChannel
  { rgmiiClk :: "clk" ::: Clock dom
  , rgmiiCtl :: "ctl" ::: Signal domDDR Bit
  , rgmiiData :: "data" ::: Signal domDDR (BitVector 4)
  }

instance Protocol (RgmiiChannel dom domDDR) where
  type Fwd (RgmiiChannel dom domDDR) = RgmiiChannel dom domDDR
  type Bwd (RgmiiChannel dom domDDR) = Signal dom ()

-- | RGMII receiver.
rgmiiReceiver ::
  forall dom domDDR.
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  (DomainActiveEdge dom ~ 'Rising) =>
  (KnownDomain dom) =>
  -- | RX channel from the RGMII PHY
  RgmiiChannel dom domDDR ->
  Reset dom ->
  -- | RX delay function
  (forall a. Signal domDDR a -> Signal domDDR a) ->
  -- | IDDR with 'Clash.Explicit.DDR.ddrIn' ordering:
  -- (previous falling edge, current rising edge).
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Enable dom ->
    (a, a, a) ->
    Signal domDDR a ->
    Signal dom (a, a)
  ) ->
  -- | (Error bit, Received data)
  Signal dom (Bool, Maybe (BitVector 8))
rgmiiReceiver RgmiiChannel{..} rst rxdelay iddr = bundle (ethRxErr, ethRxData)
 where
  (rxCtlFall, rxCtlRise) =
    unbundle $ iddr rgmiiClk rst enableGen (False, False, False) (rxdelay (bitToBool <$> rgmiiCtl))

  -- The RXCTL signal at the falling edge is the XOR of RXDV and RXERR
  -- meaning that RXERR is the XOR of it and RXDV.
  -- See RGMII interface documentation.
  ethRxDv, ethRxErr :: Signal dom Bool
  ethRxDv = E.register rgmiiClk rst enableGen False rxCtlRise
  ethRxErr = liftA2 xor ethRxDv rxCtlFall

  -- LSB first! See RGMII interface documentation.
  (rxDataFall, rxDataRise) =
    unbundle $ iddr rgmiiClk rst enableGen (0, 0, 0) (rxdelay rgmiiData)
  rxDataLow = E.register rgmiiClk rst enableGen 0 rxDataRise

  ethRxData :: Signal dom (Maybe (BitVector 8))
  ethRxData =
    (\(dv, dat) -> if dv then Just dat else Nothing)
      <$> bundle (ethRxDv, liftA2 (++#) rxDataFall rxDataLow)

-- | RGMII transmitter.
rgmiiTransmitter ::
  forall dom domDDR.
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  (DomainActiveEdge dom ~ 'Rising) =>
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  -- | TX delay function
  (Signal domDDR Bit -> Signal domDDR Bit) ->
  -- | ODDR with 'Clash.Explicit.DDR.ddrOut' ordering:
  -- (rising edge, following falling edge).
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Enable dom ->
    a ->
    Signal dom (a, a) ->
    Signal domDDR a
  ) ->
  -- | Maybe the byte we have to send
  Signal dom (Maybe (BitVector 8)) ->
  -- | Error signal indicating whether the current packet is corrupt. This is send to the Phy as transmit error bit.
  Signal dom Bool ->
  -- | TX channel to the RGMII PHY
  RgmiiChannel dom domDDR
rgmiiTransmitter txClk rst txdelay oddr input err = channel
 where
  txEn, txErr :: Signal dom Bit
  txEn = boolToBit . isJust <$> input
  txErr = fmap boolToBit err

  ethTxData1, ethTxData2 :: Signal dom (BitVector 4)
  (ethTxData1, ethTxData2) = unbundle $
    maybe
      ( deepErrorX "rgmiiTransmitter: undefined Ethernet TX data 1"
      , deepErrorX "rgmiiTransmitter: undefined Ethernet TX data 2"
      )
      split
        <$> input

  -- The TXCTL signal at the falling edge is the XOR of TXEN and TXERR
  -- meaning that TXERR is the XOR of it and TXEN.
  -- See RGMII interface documentation.
  txCtl :: Signal domDDR Bit
  txCtl = oddr txClk rst enableGen 0 $ bundle (txEn, liftA2 xor txEn txErr)

  -- LSB first! See RGMII interface documentation.
  txData :: Signal domDDR (BitVector 4)
  txData = oddr txClk rst enableGen 0 $ bundle (ethTxData2, ethTxData1)

  channel =
    RgmiiChannel
      { rgmiiClk =
          ddrForwardClock txClk rst enableGen Nothing Nothing
            (\clk rst0 en -> txdelay . oddr clk rst0 en 0)
      , rgmiiCtl = txCtl
      , rgmiiData = txData
      }

{- |
RGMII receiver with one transfer of buffering. Ends each packet with an empty
transfer. A stalled data transfer is held until accepted, followed by an empty
transfer with '_abort' asserted. Discards incoming data until that terminator is
accepted and a new packet starts.
-}
rgmiiRxC ::
  forall dom domDDR.
  (HiddenClockResetEnable dom) =>
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  (DomainActiveEdge dom ~ 'Rising) =>
  -- | RX delay function
  (forall a. Signal domDDR a -> Signal domDDR a) ->
  -- | IDDR as described for 'rgmiiReceiver'.
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Enable dom ->
    (a, a, a) ->
    Signal domDDR a ->
    Signal dom (a, a)
  ) ->
  Circuit (RgmiiChannel dom domDDR) (PacketStream dom 1 ())
rgmiiRxC rxDelay iddr = fromSignals ckt
 where
  ckt (fwdIn, bwdIn) = (pure (), fwdOut)
   where
    (rxErr, rxData) = unbundle (rgmiiReceiver fwdIn hasReset rxDelay iddr)
    rxValid = isJust <$> rxData
    rxStart = rxValid .&&. (not <$> register False rxValid)
    ready = _ready <$> bwdIn

    load = (not <$> valid) .||. ready
    dataPending = valid .&&. (not <$> lastByte)
    stalled = register False (not <$> load)
    finish = dataPending .&&. (stalled .||. (not <$> rxValid))

    valid = regEn False load (dataPending .||. rxStart)
    lastByte = regEn False load finish
    err = regEn False load (mux finish stalled rxErr)
    byte = regEn (deepErrorX "rgmiiRxC: no data") load (fromJustX <$> rxData)
    fwdOut = makeTransfer <$> valid <*> byte <*> lastByte <*> err

    makeTransfer v b l e =
      if v then Just (PacketStreamM2S (singleton b) (if l then Just 0 else Nothing) () e)
      else Nothing

{- |
Circuit that adapts a `PacketStream` to a TX `RgmiiChannel`.
Has one clock cycle latency and accepts one transfer per cycle outside reset.
Requires contiguous transfers within each packet and upstream interpacket gaps.
-}
rgmiiTxC ::
  forall dom domDDR.
  (HiddenClockResetEnable dom) =>
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  (DomainActiveEdge dom ~ 'Rising) =>
  -- | TX delay function
  (Signal domDDR Bit -> Signal domDDR Bit) ->
  -- | ODDR as described for 'rgmiiTransmitter'.
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Enable dom ->
    a ->
    Signal dom (a, a) ->
    Signal domDDR a
  ) ->
  Circuit (PacketStream dom 1 ()) (RgmiiChannel dom domDDR)
rgmiiTxC txDelay oddr = fromSignals ckt
 where
  ckt (fwdIn, _) = (PacketStreamS2M . not <$> unsafeToActiveHigh hasReset, fwdOut)
   where
    nonempty = (>>= keepData) <$> fwdIn
    keepData transfer
      | _last transfer == Just 0 = Nothing
      | otherwise = Just transfer
    input = register Nothing (fmap (head . _data) <$> nonempty)
    open = register False (maybe False (isNothing . _last) <$> nonempty)
    emptyAbort = maybe False (\t -> _last t == Just 0 && _abort t) <$> fwdIn
    err = register False (maybe False _abort <$> nonempty) .||. (open .&&. emptyAbort)
    fwdOut = rgmiiTransmitter hasClock hasReset txDelay oddr input err
