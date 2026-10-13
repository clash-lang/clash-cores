{- |
  Copyright   :  (C) 2016, University of Twente
                     2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Types shared by the I2C byte and bit controllers.
-}
module Clash.Cores.I2c.Types where

import Clash.Prelude

-- | Commands the byte controller issues to the bit controller.
data I2cCommand
  = -- | Generate a (repeated) START condition
    I2cStart
  | -- | Generate a STOP condition
    I2cStop
  | -- | Write a single bit
    I2cWrite
  | -- | Read a single bit
    I2cRead
  | -- | Do nothing
    I2cNop
  deriving (Eq, Show, Generic, NFDataX)

-- | Command for the bit controller, and the bit to write for 'I2cWrite'.
type BitCtrlSig = (I2cCommand, Bit)

{- | Response of the bit controller:

  1. Command acknowledge, asserted for one cycle when a command has completed
  2. Arbitration lost
  3. Last bit sampled from SDA
-}
type BitRespSig = (Bool, Bool, Bit)

-- | Sampled I2C bus lines: @(SCL, SDA)@.
type I2cIn = (Bit, Bit)

{- | I2C bus line drivers: @(sclO, sclOen, sdaO, sdaOen)@.

I2C lines are open drain: a device either pulls a line low or releases it. The
output enables (@sclOen@ and @sdaOen@) are active low: 'True' means the line
should be released (high impedance), and 'False' means the line should be driven
with the corresponding output value. The output values (@sclO@ and @sdaO@) are
always 'low'.

On an FPGA this maps to a tri-state buffer per line, e.g. in Verilog:

> assign scl = sclOen ? 1'bz : sclO;
> assign sda = sdaOen ? 1'bz : sdaO;
-}
type I2cOut = (Bit, Bool, Bit, Bool)
