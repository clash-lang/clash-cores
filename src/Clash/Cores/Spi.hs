{-|
  Copyright   :  (C) 2019, Foamspace corp
                     2022, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  SPI master and slave core
-}
module Clash.Cores.Spi
  ( SpiMode(..)
    -- * SPI master
  , spiMaster
    -- * SPI slave
  , SpiSlaveConfig(..)
  , spiSlave
    -- ** Vendor configured SPI slaves
  , spiSlaveLatticeSBIO
  , spiSlaveLatticeBB
  ) where

import Data.Maybe (fromMaybe, isJust)
import Test.QuickCheck as QC

import Clash.Prelude
import Clash.Sized.Internal.BitVector

import Clash.Cores.LatticeSemi.Ice40.IO
import Clash.Cores.LatticeSemi.Ecp5.IO

-- | SPI mode
--
-- * CPOL: Clock POLarity
-- * CPHA: Clock PHAse
data SpiMode
  = SpiMode0
  -- ^ CPOL = 0, CPHA = 0
  --
  -- Clock is idle low, so the leading edge is rising.
  -- Phase is low, so data is sampled on the leading edge.
  --
  -- Summary: the receiver samples data on the rising edge of SCK. The
  -- transmitter shifts data on the falling edge of SCK.
  | SpiMode1
  -- ^ CPOL = 0, CPHA = 1
  --
  -- Clock is idle low, so the leading edge is rising.
  -- Phase is high, so data is sampled on the trailing edge.
  --
  -- Summary: the receiver samples data on the falling edge of SCK. The
  -- transmitter shifts data on the rising edge of SCK.
  | SpiMode2
  -- ^ CPOL = 1, CPHA = 0
  --
  -- Clock is idle high, so the leading edge is falling.
  -- Phase is low, so data is sampled on the leading edge.
  --
  -- Summary: the receiver samples data on the falling edge of SCK. The
  -- transmitter shifts data on the rising edge of SCK.
  | SpiMode3
  -- ^ CPOL = 1, CPHA = 1
  --
  -- Clock is idle high, so the leading edge is falling.
  -- Phase is high, so data is sampled on the trailing edge.
  --
  -- Summary: the receiver samples data on the rising edge of SCK. The
  -- transmitter shifts data on the falling edge of SCK.
  deriving (Eq, Show)

instance Arbitrary SpiMode where
  arbitrary = QC.elements [SpiMode0, SpiMode1, SpiMode2, SpiMode3]

idleOnLow :: SpiMode -> Bool
idleOnLow SpiMode0 = True
idleOnLow SpiMode1 = True
idleOnLow _        = False

sampleOnRising :: SpiMode -> Bool
sampleOnRising SpiMode0 = True
sampleOnRising SpiMode3 = True
sampleOnRising _        = False

sampleOnLeading :: SpiMode -> Bool
sampleOnLeading SpiMode0 = True
sampleOnLeading SpiMode2 = True
sampleOnLeading _        = False

sampleOnTrailing :: SpiMode -> Bool
sampleOnTrailing = not . sampleOnLeading

data SpiSlaveConfig ds dom
  = SpiSlaveConfig
  { spiSlaveConfigMode :: SpiMode
  -- ^ SPI mode
  , spiSlaveConfigLatch :: Bool
  -- ^ Whether to latch the SPI pins
  --
  -- Recommended:
  --
  -- * Set to /True/ when core clock is /more/ than twice as fast as the SCK
  --   Clock: 2*SCK < Core
  --
  -- * Set to /False/ when core clock is twice as fast, or as fast, as the SCK
  , spiSlaveConfigBuffer
      :: BiSignalIn ds dom 1
      -> Signal dom Bool
      -> Signal dom Bit
      -> BiSignalOut ds dom 1
  -- ^ Tri-state buffer: first argument is the inout pin, second
  -- argument is the output enable, third argument is the value to
  -- output when the enable is high
  }

-- | SPI capture and shift logic that is shared between slave and master
spiCommon
  :: forall n dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SpiMode
  -> Signal dom Bool
  -- ^ Slave select
  -> Signal dom Bit
  -- ^ Slave: MOSI; Master: MISO
  -> Signal dom Bool
  -- ^ SCK
  -> Signal dom (BitVector n)
  -> ( Signal dom Bit   -- Slave: MISO; Master: MOSI
     , Signal dom Bool  -- Acknowledge start of transfer
     , Signal dom (Maybe (BitVector n))
     )
spiCommon mode ssI msI sckI dinI =
  mooreB go cvt ( 0 :: Index n      -- cntR
                , undefined         -- sckOldR
                , deepErrorX "no initial dataInR"
                , deepErrorX "no initial dataOutR"
                , False             -- ackR
                , False             -- doneR
                )
                (ssI,msI,sckI,dinI)
 where
  cvt (_,_,dataInQ,dataOutQ,ackQ,doneQ) =
    ( head dataOutQ
    , ackQ
    , if doneQ
         then Just (pack dataInQ)
         else Nothing
    )

  go (cntQ,sckOldQ,dataInQ,dataOutQ,_,_) (ss,ms,sck,din) =
    (cntD,sck,dataInD,dataOutD,ackD,doneD)
   where
    cntD
      | ss        = 0
      | sampleSck = if cntQ == maxBound then 0 else cntQ + 1
      | otherwise = cntQ

    dataInD
      | ss        = unpack undefined#
      | sampleSck = tail @(n-1) dataInQ :< ms
      | otherwise = dataInQ

    dataOutD
      | ss || (sampleOnTrailing mode && sampleSck && cntQ == maxBound) = unpack din
      -- Modes 0 and 2: load the next word on the shift edge after the last
      -- sample of a word. At this edge, the first bit of the next word must
      -- be on the output. This load is necessary when slave select stays
      -- asserted between words. In these modes, the first edge after slave
      -- select asserts is a sample edge. Thus, this condition is true only at
      -- a word boundary.
      | sampleOnLeading mode && shiftSck && cntQ == 0 = unpack din
      | shiftSck  = if sampleOnTrailing mode && cntQ == 0
                    then dataOutQ
                    else tail @(n-1) dataOutQ :< unpack undefined#
      | otherwise = dataOutQ

    ackD
      -- Words of 1 bit (the counter has only the value 0): the next load
      -- occurs on the next shift edge in modes 0 and 2, and on the next
      -- sample edge in modes 1 and 3. The acknowledgement must occur before
      -- that load. Thus, use the edge before the load.
      | maxBound == (0 :: Index n) =
          not ss && (if sampleOnLeading mode then sampleSck else shiftSck)
      | otherwise = not ss && shiftSck && cntQ == 1

    doneD = not ss && sampleSck && cntQ == maxBound

    (sampleSck, shiftSck)
      | sampleOnRising mode = (risingSck, fallingSck)
      | otherwise           = (fallingSck, risingSck)
     where
      risingSck  = not sckOldQ && sck
      fallingSck = sckOldQ && not sck

-- | SPI slave configurable SPI mode and tri-state buffer
spiSlave
  :: forall n ds dom
   . (HiddenClockResetEnable dom, KnownNat n, 1 <= n)
  => SpiSlaveConfig ds dom
  -- ^ Configure SPI mode and tri-state buffer
  -> Signal dom Bool
  -- ^ Serial Clock (SCLK)
  -> Signal dom Bit
  -- ^ Master Output Slave Input (MOSI)
  -> BiSignalIn ds dom 1
  -- ^ Master Input Slave Output (MISO)
  --
  -- Inout port connected to the tri-state buffer for the MISO
  -> Signal dom Bool
  -- ^ Slave select (SS)
  -> Signal dom (BitVector n)
  -- ^ Data to send from slave to master
  --
  -- The slave latches the first word when slave select goes low. When slave
  -- select stays low for more than one word, the slave latches the next word
  -- at the end of the current word. Put the next word on this input after the
  -- slave asserts the acknowledgement output. Keep the next word stable until
  -- the current word ends. For words of 1 bit in modes 0 and 2, the slave
  -- loads the next word half a period of SCK after the sample edge. Thus, put
  -- the next word on this input in less than half a period of SCK after the
  -- acknowledgement.
  -> ( BiSignalOut ds dom 1
     , Signal dom Bool
     , Signal dom (Maybe (BitVector n)))
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. Acknowledgement. This output is True for one clock cycle when the
  --    slave starts to send the current word. Then, put the next word on the
  --    data input.
  --
  -- 3. (Maybe) the word from the master
spiSlave (SpiSlaveConfig mode latch buf) sclk mosi bin ss din =
  let ssL   = if latch then delay undefined ss   else ss
      mosiL = if latch then delay undefined mosi else mosi
      sclkL = if latch then delay undefined sclk else sclk
      (miso, ack, dout) = spiCommon mode ssL mosiL sclkL din
      bout = buf bin (not <$> ssL) miso
  in  (bout, ack, dout)

-- | SPI master configurable in the SPI mode and clock divider
--
-- Adds latch to MISO line if the (half period) clock divider is
-- set to 2 or higher.
spiMaster
  :: forall n halfPeriod waitTime dom
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , 1 <= halfPeriod
     , 1 <= waitTime )
  => SpiMode
  -- ^ SPI Mode
  -> SNat halfPeriod
  -- ^ Clock divider (half period)
  --
  -- If set to two or higher, the MISO line will be latched
  -> SNat waitTime
  -- ^ (core clock) cycles between de-asserting slave-select and start of
  -- the SPI clock
  -> Signal dom (Maybe (BitVector n))
  -- ^ Data to send from master to slave, transmission starts when receiving
  -- /Just/ a value
  -> Signal dom Bit
  -- ^ Master Input Slave Output (MISO)
  -> ( Signal dom Bool -- SCK
     , Signal dom Bit  -- MOSI
     , Signal dom Bool -- SS
     , Signal dom Bool -- Busy
     , Signal dom Bool -- Acknowledge
     , Signal dom (Maybe (BitVector n)) -- Data: Slave -> Master
     )
  -- ^ Parts of the tuple:
  --
  -- 1. Serial Clock (SCLK)
  -- 2. Master Output Slave Input (MOSI)
  -- 3. Slave select (SS)
  -- 4. Busy signal indicating that a transmission is in progress, new words on
  --    the data line will be ignored when /True/
  -- 5. Acknowledgement. This output is True for one clock cycle when the
  --    master starts to send the current word. Then, put the next word on the
  --    data input.
  -- 6. (Maybe) the word from the slave
spiMaster mode fN fW din miso =
  let (mosi, ack, dout)   = spiCommon mode ssL misoL sclkL
                        (fromMaybe undefined# <$> din)
      latch = snatToInteger fN /= 1
      ssL   = if latch then delay undefined ss   else ss
      misoL = if latch then delay undefined miso else miso
      sclkL = if latch then delay undefined sclk else sclk
      (ss, sclk, busy) = spiGen mode fN fW din
  in  (sclk, mosi, ss, busy, ack, dout)

-- | Generate slave select and SCK
spiGen
  :: forall n halfPeriod waitTime dom
   . ( HiddenClockResetEnable dom
     , KnownNat n
     , 1 <= n
     , 1 <= halfPeriod
     , 1 <= waitTime )
  => SpiMode
  -> SNat halfPeriod
  -> SNat waitTime
  -> Signal dom (Maybe (BitVector n))
  -> ( Signal dom Bool
     , Signal dom Bool
     , Signal dom Bool
     )
spiGen mode SNat SNat =
  unbundle . moore go cvt (0 :: Index (2*n),False,Idle @halfPeriod @waitTime)
 where
  cvt (_, sck, st) =
    ( st == Idle
    , if idleOnLow mode then sck else not sck
    , st /= Idle
    )

  go (cntQ,sckQ,stQ) din = (cntD,sckD,stD)
   where
    stD = case stQ of
      Idle
        | isJust din -> Wait maxBound

      Wait 0 -> Transfer 0
      Wait w -> Wait (w - 1)


      Transfer n
        | n /= maxBound -> Transfer (n+1)
        | cntQ == maxBound -> Finish
        | otherwise -> Transfer 0
      Finish -> Idle
      _ -> stQ

    cntD = case stQ of
      Transfer n
        | n == maxBound -> if cntQ == maxBound then 0 else cntQ+1
        | otherwise -> cntQ
      _ -> 0

    sckD = case stQ of
      Transfer n | n == maxBound -> not sckQ
      _ -> sckQ

data SpiMasterState halfPeriod waitTime
  = Idle
  | Wait (Index waitTime)
  | Transfer (Index halfPeriod)
  | Finish
  deriving (Eq, Generic, NFDataX)

-- | SPI slave configurable SPI mode, using the SB_IO tri-state buffer
-- found on Lattice ICE40 Semiconductor FPGAs
spiSlaveLatticeSBIO
  :: forall dom n
   . (HiddenClockResetEnable dom, 1 <= n, KnownNat n)
  => SpiMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether to latch the SPI pins
  --
  -- Recommended:
  --
  -- * Set to /True/ when core clock is /more/ than twice as fast as the SCK
  --   Clock: 2*SCK < Core
  --
  -- * Set to /False/ when core clock is twice as fast, or as fast, as the SCK
  -> Signal dom Bool
  -- ^ Serial Clock (SCLK)
  -> Signal dom Bit
  -- ^ Master Output Slave Input (MOSI)
  -> BiSignalIn 'Floating dom 1
  -- ^ Master Input Slave Output (MISO)
  --
  -- Inout port connected to the tri-state buffer for the MISO
  -> Signal dom Bool
  -- ^ Slave select (SS)
  -> Signal dom (BitVector n)
  -- ^ Data to send from slave to master
  --
  -- The slave latches the first word when slave select goes low. When slave
  -- select stays low for more than one word, the slave latches the next word
  -- at the end of the current word. Put the next word on this input after the
  -- slave asserts the acknowledgement output. Keep the next word stable until
  -- the current word ends. For words of 1 bit in modes 0 and 2, the slave
  -- loads the next word half a period of SCK after the sample edge. Thus, put
  -- the next word on this input in less than half a period of SCK after the
  -- acknowledgement.
  -> ( BiSignalOut 'Floating dom 1
     , Signal dom Bool
     , Signal dom (Maybe (BitVector n)))
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. Acknowledgement. This output is True for one clock cycle when the
  --    slave starts to send the current word. Then, put the next word on the
  --    data input.
  --
  -- 3. (Maybe) the word from the master
spiSlaveLatticeSBIO mode latchSpi =
  spiSlave (SpiSlaveConfig mode latchSpi sbioX)
 where
  sbioX bin en dout = bout
   where
    (bout,_,_) = sbio 0b101001 bin (pure 0) dout (pure undefined) en


-- | SPI slave configurable SPI mode, using the BB tri-state buffer
-- found on Lattice ECP5 Semiconductor FPGAs
spiSlaveLatticeBB
  :: forall dom n
   . (HiddenClockResetEnable dom, 1 <= n, KnownNat n)
  => SpiMode
  -- ^ SPI Mode
  -> Bool
  -- ^ Whether to latch the SPI pins
  --
  -- Recommended:
  --
  -- * Set to /True/ when core clock is /more/ than twice as fast as the SCK
  --   Clock: 2*SCK < Core
  --
  -- * Set to /False/ when core clock is twice as fast, or as fast, as the SCK
  -> Signal dom Bool
  -- ^ Serial Clock (SCLK)
  -> Signal dom Bit
  -- ^ Master Output Slave Input (MOSI)
  -> BiSignalIn 'Floating dom 1
  -- ^ Master Input Slave Output (MISO)
  --
  -- Inout port connected to the tri-state buffer for the MISO
  -> Signal dom Bool
  -- ^ Slave select (SS)
  -> Signal dom (BitVector n)
  -- ^ Data to send from slave to master
  --
  -- The slave latches the first word when slave select goes low. When slave
  -- select stays low for more than one word, the slave latches the next word
  -- at the end of the current word. Put the next word on this input after the
  -- slave asserts the acknowledgement output. Keep the next word stable until
  -- the current word ends. For words of 1 bit in modes 0 and 2, the slave
  -- loads the next word half a period of SCK after the sample edge. Thus, put
  -- the next word on this input in less than half a period of SCK after the
  -- acknowledgement.
  -> ( BiSignalOut 'Floating dom 1
     , Signal dom Bool
     , Signal dom (Maybe (BitVector n)))
  -- ^ Parts of the tuple:
  --
  -- 1. The "out" part of the inout port of the MISO; used only for simulation.
  --
  -- 2. Acknowledgement. This output is True for one clock cycle when the
  --    slave starts to send the current word. Then, put the next word on the
  --    data input.
  --
  -- 3. (Maybe) the word from the master
spiSlaveLatticeBB mode latchSpi =
  spiSlave (SpiSlaveConfig mode latchSpi bbX)
 where
    bbX bin en dout = bout
      where
        (bout,_) = bidirectionalBuffer (toEnable en) bin dout
