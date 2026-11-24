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
  , spiMasterTransaction
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

sampleOnLeading :: SpiMode -> Bool
sampleOnLeading SpiMode0 = True
sampleOnLeading SpiMode2 = True
sampleOnLeading _        = False

sampleOnRising :: SpiMode -> Bool
sampleOnRising SpiMode0 = True
sampleOnRising SpiMode3 = True
sampleOnRising _        = False

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
  -> Bool
  -- ^ Load the data input at the start of each word. The slave uses this
  -- option, because its user logic can change the data input until the
  -- start of the word. The master loads its word before the transfer
  -- starts. Its data input is not valid after that.
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
spiCommon mode loadEachWord ssI msI sckI dinI =
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

    -- With 'loadEachWord', load a word on the shift edge where its first bit
    -- must be on the output:
    --
    -- * Modes 0 and 2: the shift edge after the last sample of the previous
    --   word.
    -- * Modes 1 and 3: the first shift edge of the word.
    --
    -- This is the last possible moment. Thus, the user logic has the most
    -- time to put a response to the previous word on the data input. When
    -- slave select stays asserted between words, this load gives the next
    -- word.
    --
    -- Without 'loadEachWord', keep the word on this edge. In modes 1 and 3,
    -- the first bit is already on the output. In modes 0 and 2, the word is
    -- complete.
    dataOutD
      | ss = unpack din
      | shiftSck && cntQ == 0 = if loadEachWord then unpack din else dataOutQ
      | shiftSck  = tail @(n-1) dataOutQ :< unpack undefined#
      | otherwise = dataOutQ

    ackD
      -- Words of 1 bit (the counter has only the value 0):
      --
      -- * With 'loadEachWord', the slave loads a word on each shift edge. The
      --   acknowledgement must occur before that load. Thus, use the sample
      --   edge.
      -- * Without 'loadEachWord', the master loads the next word before the
      --   next transfer starts. Thus, use the first edge of the word. This is
      --   the sample edge in modes 0 and 2, and the shift edge in modes 1
      --   and 3.
      | maxBound == (0 :: Index n) =
          not ss && (if loadEachWord || sampleOnLeading mode
                       then sampleSck
                       else shiftSck)
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
  -- select stays low for more than one word, the slave latches each next
  -- word on the shift edge where its first bit must be on MISO. Put the next
  -- word on this input after the slave asserts the acknowledgement output.
  -- Keep the next word stable until the slave latches it.
  --
  -- To send a response to a received word in the next word, put the response
  -- on this input before the slave latches it. The slave latches the next
  -- word half a period of SCK or more after the sample edge of the last bit
  -- of the received word. The received word is on the output one clock cycle
  -- after that sample edge. For words of 1 bit, the acknowledgement also
  -- occurs one clock cycle after that sample edge.
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
      (miso, ack, dout) = spiCommon mode True ssL mosiL sclkL din
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
spiMaster mode fN fW = spiMasterTransaction mode fN fW (pure False)

-- | SPI master that can keep slave select asserted for more than one word.
-- Use this master for protocols with transactions of more than one word. An
-- example is a command word, followed by address words and data words.
--
-- The master sends one word for each /Just/ value on the data input. The
-- master reads the hold input at the end of each word:
--
-- * When the hold input is True, slave select stays asserted. The master
--   waits for the next word, and SCK stays at its idle level.
-- * When the hold input is False, the master de-asserts slave select.
--
-- When the master waits with slave select asserted and the hold input
-- becomes False, the master de-asserts slave select.
--
-- Each word sends data and receives data at the same time. For a write,
-- ignore the received word. For a read, send a word that the slave ignores,
-- and use the received word.
--
-- 'spiMaster' is this function with the hold input always False.
spiMasterTransaction
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
  -- ^ (core clock) cycles between asserting slave-select and start of
  -- the SPI clock
  -> Signal dom Bool
  -- ^ Hold slave select. When this input is True at the end of a word, slave
  -- select stays asserted after that word.
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
  -- 4. Busy. This output is True while the master sends a word. When the
  --    master waits with slave select asserted, this output is False. The
  --    master ignores the data input when this output is True.
  -- 5. Acknowledgement. This output is True for one clock cycle when the
  --    master starts to send the current word. Then, put the next word on the
  --    data input.
  -- 6. (Maybe) the word from the slave
spiMasterTransaction mode fN fW hold din miso =
  let (mosi, ack, dout) = spiCommon mode False idleL misoL sclkL dinL
      latch = snatToNum @Int fN /= 1
      -- The latch delays all inputs of 'spiCommon' by the same time. Thus,
      -- 'spiCommon' loads the word that was on the data input when the
      -- master started the word.
      idleL = if latch then delay undefined idle else idle
      misoL = if latch then delay undefined miso else miso
      sclkL = if latch then delay undefined sclk else sclk
      dinL  = if latch then delay undefined din' else din'
      din'  = fromMaybe undefined# <$> din
      (ss, idle, sclk, busy) = spiGen mode fN fW hold din
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
  -> Signal dom Bool
  -> Signal dom (Maybe (BitVector n))
  -> ( Signal dom Bool
     , Signal dom Bool
     , Signal dom Bool
     , Signal dom Bool
     )
spiGen mode SNat SNat holdS dinS =
  unbundle $ moore go cvt (0 :: Index (2*n),False,Idle @halfPeriod @waitTime)
                          (bundle (holdS, dinS))
 where
  cvt (_, sck, st) =
    ( st == Idle                    -- Slave select (active low)
    , st == Idle || st == Selected  -- No word in progress
    , if idleOnLow mode then sck else not sck
    , st /= Idle && st /= Selected  -- Busy
    )

  go (cntQ,sckQ,stQ) (holdI, din) = (cntD,sckD,stD)
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
      Finish
        | holdI -> Selected
        | otherwise -> Idle

      Selected
        | isJust din -> Transfer 0
        | not holdI -> Idle

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
  | Selected
  -- ^ Slave select is asserted, and the master waits for the next word
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
  -- select stays low for more than one word, the slave latches each next
  -- word on the shift edge where its first bit must be on MISO. Put the next
  -- word on this input after the slave asserts the acknowledgement output.
  -- Keep the next word stable until the slave latches it.
  --
  -- To send a response to a received word in the next word, put the response
  -- on this input before the slave latches it. The slave latches the next
  -- word half a period of SCK or more after the sample edge of the last bit
  -- of the received word. The received word is on the output one clock cycle
  -- after that sample edge. For words of 1 bit, the acknowledgement also
  -- occurs one clock cycle after that sample edge.
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
  -- select stays low for more than one word, the slave latches each next
  -- word on the shift edge where its first bit must be on MISO. Put the next
  -- word on this input after the slave asserts the acknowledgement output.
  -- Keep the next word stable until the slave latches it.
  --
  -- To send a response to a received word in the next word, put the response
  -- on this input before the slave latches it. The slave latches the next
  -- word half a period of SCK or more after the sample edge of the last bit
  -- of the received word. The received word is on the output one clock cycle
  -- after that sample edge. For words of 1 bit, the acknowledgement also
  -- occurs one clock cycle after that sample edge.
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
