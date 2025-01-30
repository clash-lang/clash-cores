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
  unsafeRgmiiRxC,
  rgmiiTxC,
) where

import Clash.Explicit.DDR (ddrForwardClock)
import qualified Clash.Explicit.Signal as E
import Clash.Prelude

import Protocols
import Protocols.PacketStream

import Data.Maybe (isJust)

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
    Signal domDDR a ->
    Signal dom (a, a)
  ) ->
  -- | (Error bit, Received data)
  Signal dom (Bool, Maybe (BitVector 8))
rgmiiReceiver RgmiiChannel{..} rst rxdelay iddr = bundle (ethRxErr, ethRxData)
 where
  (rxCtlFall, rxCtlRise) =
    unbundle $ iddr rgmiiClk rst enableGen (rxdelay (bitToBool <$> rgmiiCtl))

  -- The RXCTL signal at the falling edge is the XOR of RXDV and RXERR
  -- meaning that RXERR is the XOR of it and RXDV.
  -- See RGMII interface documentation.
  ethRxDv, ethRxErr :: Signal dom Bool
  ethRxDv = E.register rgmiiClk rst enableGen False rxCtlRise
  ethRxErr = liftA2 xor ethRxDv rxCtlFall

  -- LSB first! See RGMII interface documentation.
  (rxDataFall, rxDataRise) =
    unbundle $ iddr rgmiiClk rst enableGen (rxdelay rgmiiData)
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
  (forall a. Signal domDDR a -> Signal domDDR a) ->
  -- | ODDR with 'Clash.Explicit.DDR.ddrOut' ordering:
  -- (rising edge, following falling edge).
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Enable dom ->
    Signal dom (a, a) ->
    Signal domDDR a
  ) ->
  -- | Maybe the byte we have to send
  Signal dom (Maybe (BitVector 8)) ->
  -- | Error signal indicating whether the current packet is corrupt
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
  txCtl = oddr txClk rst enableGen $ bundle (txEn, liftA2 xor txEn txErr)

  -- LSB first! See RGMII interface documentation.
  txData :: Signal domDDR (BitVector 4)
  txData = oddr txClk rst enableGen $ bundle (ethTxData2, ethTxData1)

  channel =
    RgmiiChannel
      { rgmiiClk =
          ddrForwardClock txClk rst enableGen Nothing Nothing
            (\clk rst0 en -> txdelay . oddr clk rst0 en)
      , rgmiiCtl = txCtl
      , rgmiiData = txData
      }

{- |
Circuit that adapts an RX `RgmiiChannel` to a `PacketStream`. Forwards data from
the RGMII receiver with one clock cycle latency so that we can properly mark the
last transfer of a packet: if we received valid data from the RGMII receiver in
the last clock cycle and the data in the current clock cycle is invalid, we set
`_last`. If the RGMII receiver gives an error, we set `_abort`.

__UNSAFE__: ignores backpressure, because the RGMII PHY is unable to handle that.
-}
unsafeRgmiiRxC ::
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
    Signal domDDR a ->
    Signal dom (a, a)
  ) ->
  Circuit (RgmiiChannel dom domDDR) (PacketStream dom 1 ())
unsafeRgmiiRxC rxDelay iddr = fromSignals ckt
 where
  ckt (fwdIn, _) = (pure (), fwdOut)
   where
    (rxErr, rxData) = unbundle (rgmiiReceiver fwdIn hasReset rxDelay iddr)
    lastRxErr = register False rxErr
    lastRxData = register Nothing rxData

    fwdOut = go <$> bundle (rxData, lastRxData, lastRxErr)

    go (currData, lastData, lastErr) =
      ( \byte ->
          PacketStreamM2S
            { _data = singleton byte
            , _last = case currData of
                Nothing -> Just 1
                Just _ -> Nothing
            , _meta = ()
            , _abort = lastErr
            }
      )
        <$> lastData

{- |
Circuit that adapts a `PacketStream` to a TX `RgmiiChannel`.
Has one clock cycle latency and accepts one transfer per cycle outside reset.
-}
rgmiiTxC ::
  forall dom domDDR.
  (HiddenClockResetEnable dom) =>
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  (DomainActiveEdge dom ~ 'Rising) =>
  -- | TX delay function
  (forall a. Signal domDDR a -> Signal domDDR a) ->
  -- | ODDR as described for 'rgmiiTransmitter'.
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Enable dom ->
    Signal dom (a, a) ->
    Signal domDDR a
  ) ->
  Circuit (PacketStream dom 1 ()) (RgmiiChannel dom domDDR)
rgmiiTxC txDelay oddr = stripTrailingEmptyC |> fromSignals ckt
 where
  ckt (fwdIn, _) = (pure (PacketStreamS2M True), fwdOut)
   where
    nonempty = (>>= keepData) <$> fwdIn
    keepData transfer
      | _last transfer == Just 0 = Nothing
      | otherwise = Just transfer
    input = fmap (head . _data) <$> nonempty
    err = maybe False _abort <$> nonempty
    fwdOut = rgmiiTransmitter hasClock hasReset txDelay oddr input err
