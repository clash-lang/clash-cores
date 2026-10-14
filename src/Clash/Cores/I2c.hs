{- |
  Copyright   :  (C) 2016, University of Twente
                     2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  I2C controller core, based on the
  [OpenCores I2C controller core](https://opencores.org/projects/i2c) by
  Richard Herveille. It supports multi-controller operation (clock
  synchronization and arbitration) and clock stretching by targets.

  = Host interface

  The host performs byte-level operations by asserting one of the command
  inputs, and holding it until the core asserts @hostAck@:

  * __write__: write @din@ to the bus. If __start__ is asserted as well, a
    (repeated) START condition is generated first. This is how an address byte
    is sent.
  * __read__: read a byte from the bus, and send @ackIn@ as acknowledgement
    bit. If __start__ is asserted as well, a (repeated) START condition is
    generated first.
  * __stop__: generate a STOP condition. If __read__ or __write__ is asserted as
    well, the STOP condition is generated after reading or writing the byte.

  Note that __start__ on its own does not issue a command. @hostAck@ is asserted
  for a single cycle once the command has completed. The core does not start a
  new command in that cycle, so it is safe to update the command inputs in the
  cycle after @hostAck@.

  After a write, @ackOut@ holds the acknowledgement bit sent by the target. After
  a read, @dout@ holds the byte read from the bus. Acknowledgement bits are sent
  as-is on the bus, so 'False' means ACK, and 'True' means NACK.

  If arbitration is lost, @al@ is asserted for a single cycle. The core releases
  the bus and aborts the current command without asserting @hostAck@. The host
  should then deassert its command inputs.

  = Bus timing

  The bit controller advances every @clkCnt + 1@ clock cycles, and generating a
  single SCL period takes five of these steps. After releasing SCL, the core
  waits until it observes SCL to be high through its input synchronizer and
  glitch filter. This allows targets to stretch the clock, and adds a delay of
  up to roughly @clkCnt / 2@ clock cycles to every SCL period. To get an SCL
  frequency of at most @f_scl@, set @clkCnt@ to:

  > clkCnt = f_clk / (5 * f_scl) - 1

  where @f_clk@ is the frequency of the domain the core runs in. For example,
  for a 100 kHz SCL in a 50 MHz domain, set @clkCnt@ to 99.

  @clkCnt@ must be at least 3. For smaller values, the core does not wait for
  SCL to be high, breaking clock stretching and bus timing.
-}
module Clash.Cores.I2c (
  i2c,
  I2cIn,
  I2cOut,
) where

import Clash.Prelude

import Clash.Cores.I2c.BitController
import Clash.Cores.I2c.ByteController
import Clash.Cores.I2c.Types

-- | I2C controller. See the module documentation for a description of the
-- interface.
i2c ::
  (HiddenClockResetEnable dom) =>
  -- | Clock divider (@clkCnt@), must be at least 3. See the module
  -- documentation.
  Signal dom (Unsigned 16) ->
  -- | Start
  Signal dom Bool ->
  -- | Stop
  Signal dom Bool ->
  -- | Read
  Signal dom Bool ->
  -- | Write
  Signal dom Bool ->
  -- | ACK bit to send after reading a byte (@ackIn@): 'False' for ACK, 'True'
  -- for NACK
  Signal dom Bool ->
  -- | Byte to write (@din@)
  Signal dom (BitVector 8) ->
  -- | I2C bus input
  Signal dom I2cIn ->
  -- | 1. Byte read (@dout@)
  --   2. Command acknowledge (@hostAck@)
  --   3. Bus busy: a START condition has been detected, but no STOP condition
  --      since
  --   4. Arbitration lost (@al@)
  --   5. ACK bit received after writing a byte (@ackOut@): 'False' for ACK,
  --      'True' for NACK
  --   6. I2C bus output
  ( Signal dom (BitVector 8)
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom I2cOut
  )
i2c clkCnt start stop rd wr ackIn din i2cI = (dout, hostAck, busy, al, ackOut, i2cO)
 where
  (hostAck, ackOut, dout, bitCtrl) = byteController start stop rd wr ackIn din bitResp
  (bitResp, busy, i2cO) = bitController clkCnt bitCtrl i2cI
  (_cmdAck, al, _dout) = unbundle bitResp
