{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Clash.Cores.Rgmii
Description : Functions and types to connect an RGMII PHY to a packet stream interface.

To keep this module generic, users will have to provide their own "primitive" functions:

    1. delay functions to set to the proper amount of delay (which can be different for RX and TX);
    2. iddr function to turn a single DDR (Double Data Rate) signal into 2 non-DDR signals;
    3. oddr function to turn two non-DDR signals into a single DDR signal.

Note that Clash models a DDR signal as being twice as fast, thus both facilitating
and requiring type-level separation between the two "clock domains".
-}
module Clash.Cores.Ethernet.Rgmii (
  RgmiiRxChannel (..),
  RgmiiTxChannel (..),
  rgmiiReceiver,
  rgmiiTransmitter,
  unsafeRgmiiRxC,
  rgmiiTxC,
) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream

import Data.Maybe (isJust)

-- | RX channel from the RGMII PHY
data RgmiiRxChannel dom domDDR = RgmiiRxChannel
  { rgmiiRxClk :: "rx_clk" ::: Clock dom
  , rgmiiRxCtl :: "rx_ctl" ::: Signal domDDR Bit
  , rgmiiRxData :: "rx_data" ::: Signal domDDR (BitVector 4)
  }

instance Protocol (RgmiiRxChannel dom domDDR) where
  type Fwd (RgmiiRxChannel dom domDDR) = RgmiiRxChannel dom domDDR
  type Bwd (RgmiiRxChannel dom domDDR) = Signal dom ()

-- | TX channel to the RGMII PHY
data RgmiiTxChannel domDDR = RgmiiTxChannel
  { rgmiiTxClk :: "tx_clk" ::: Signal domDDR Bit
  , rgmiiTxCtl :: "tx_ctl" ::: Signal domDDR Bit
  , rgmiiTxData :: "tx_data" ::: Signal domDDR (BitVector 4)
  }

instance Protocol (RgmiiTxChannel domDDR) where
  type Fwd (RgmiiTxChannel domDDR) = RgmiiTxChannel domDDR
  type Bwd (RgmiiTxChannel domDDR) = Signal domDDR ()

-- | RGMII receiver.
rgmiiReceiver ::
  forall dom domDDR.
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  (KnownDomain dom) =>
  -- | RX channel from the RGMII PHY
  RgmiiRxChannel dom domDDR ->
  -- | RX delay function
  (forall a. Signal domDDR a -> Signal domDDR a) ->
  -- | iddr function
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Signal domDDR a ->
    Signal dom (a, a)
  ) ->
  -- | (Error bit, Received data)
  Signal dom (Bool, Maybe (BitVector 8))
rgmiiReceiver RgmiiRxChannel{..} rxdelay iddr = bundle (ethRxErr, ethRxData)
 where
  ethRxCtl :: Signal dom (Bool, Bool)
  ethRxCtl = iddr rgmiiRxClk resetGen (rxdelay (bitToBool <$> rgmiiRxCtl))

  -- The RXCTL signal at the falling edge is the XOR of RXDV and RXERR
  -- meaning that RXERR is the XOR of it and RXDV.
  -- See RGMII interface documentation.
  ethRxDv, ethRxErr :: Signal dom Bool
  (ethRxDv, ethRxErr) = unbundle ((\(dv, err) -> (dv, dv `xor` err)) <$> ethRxCtl)

  -- LSB first! See RGMII interface documentation.
  ethRxData1, ethRxData2 :: Signal dom (BitVector 4)
  (ethRxData2, ethRxData1) = unbundle $ iddr rgmiiRxClk resetGen (rxdelay rgmiiRxData)

  ethRxData :: Signal dom (Maybe (BitVector 8))
  ethRxData =
    (\(dv, dat) -> if dv then Just dat else Nothing)
      <$> bundle (ethRxDv, liftA2 (++#) ethRxData1 ethRxData2)

-- | RGMII transmitter. Does not consider transmission error.
rgmiiTransmitter ::
  forall dom domDDR.
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  Clock dom ->
  Reset dom ->
  -- | TX delay function
  (forall a. Signal domDDR a -> Signal domDDR a) ->
  -- | oddr function
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Signal dom a ->
    Signal dom a ->
    Signal domDDR a
  ) ->
  -- | Maybe the byte we have to send
  Signal dom (Maybe (BitVector 8)) ->
  -- | Error signal indicating whether the current packet is corrupt
  Signal dom Bool ->
  -- | TX channel to the RGMII PHY
  RgmiiTxChannel domDDR
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
  txCtl = oddr txClk rst txEn (liftA2 xor txEn txErr)

  -- LSB first! See RGMII interface documentation.
  txData :: Signal domDDR (BitVector 4)
  txData = oddr txClk rst ethTxData2 ethTxData1

  channel =
    RgmiiTxChannel
      { rgmiiTxClk = txdelay (oddr txClk rst (pure 1) (pure 0))
      , rgmiiTxCtl = txdelay txCtl
      , rgmiiTxData = txdelay txData
      }

{- |
Circuit that adapts an `RgmiiRxChannel` to a `PacketStream`. Forwards data
from the RGMII receiver with one clock cycle latency so that we can properly
mark the last transfer of a packet: if we received valid data from the RGMII
receiver in the last clock cycle and the data in the current clock cycle is
invalid, we set `_last`. If the RGMII receiver gives an error, we set `_abort`.

__UNSAFE__: ignores backpressure, because the RGMII PHY is unable to handle that.
-}
unsafeRgmiiRxC ::
  forall dom domDDR.
  (HiddenClockResetEnable dom) =>
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  -- | RX delay function
  (forall a. Signal domDDR a -> Signal domDDR a) ->
  -- | iddr function
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Signal domDDR a ->
    Signal dom (a, a)
  ) ->
  Circuit (RgmiiRxChannel dom domDDR) (PacketStream dom 1 ())
unsafeRgmiiRxC rxDelay iddr = fromSignals ckt
 where
  ckt (fwdIn, _) = (pure (), fwdOut)
   where
    (rxErr, rxData) = unbundle (rgmiiReceiver fwdIn rxDelay iddr)
    lastRxErr = register False rxErr
    lastRxData = register Nothing rxData

    fwdOut = go <$> bundle (rxData, lastRxData, lastRxErr)

    go (currData, lastData, lastErr) =
      ( \byte ->
          PacketStreamM2S
            { _data = singleton byte
            , _last = case currData of
                Nothing -> Just 0
                Just _ -> Nothing
            , _meta = ()
            , _abort = lastErr
            }
      )
        <$> lastData

{- |
Circuit that adapts a `PacketStream` to an `RgmiiTxChannel`.
Never asserts backpressure.
-}
rgmiiTxC ::
  forall dom domDDR.
  (HiddenClockResetEnable dom) =>
  (DomainPeriod dom ~ 2 * DomainPeriod domDDR) =>
  -- | TX delay function
  (forall a. Signal domDDR a -> Signal domDDR a) ->
  -- | oddr function
  ( forall a.
    (NFDataX a, BitPack a) =>
    Clock dom ->
    Reset dom ->
    Signal dom a ->
    Signal dom a ->
    Signal domDDR a
  ) ->
  Circuit (PacketStream dom 1 ()) (RgmiiTxChannel domDDR)
rgmiiTxC txDelay oddr = fromSignals ckt
 where
  ckt (fwdIn, _) = (pure (PacketStreamS2M True), fwdOut)
   where
    input = fmap (head . _data) <$> fwdIn
    err = maybe False _abort <$> fwdIn
    fwdOut = rgmiiTransmitter hasClock hasReset txDelay oddr input err
