{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

UNISIM primitives of 7 series devices (Artix-7, Kintex-7, Virtex-7, Zynq-7000).
For more information see
[UG953](https://docs.xilinx.com/r/en-US/ug953-vivado-7series-libraries).

Every primitive has a simulation model for Clash simulation and instantiates the
real primitive in HDL. The simulation models are tested against Xilinx's UNISIM
simulation models; see the documentation of the individual modules for what
they do and do not model.

Some primitives of "Clash.Cores.Xilinx.Unisim" exist on 7 series devices too,
and are not repeated here:

  * 'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufg' (@BUFG@)
  * 'Clash.Cores.Xilinx.Unisim.IoBuffer.ibufds' and
    'Clash.Cores.Xilinx.Unisim.IoBuffer.obufds' (@IBUFDS@ and @OBUFDS@)
-}
module Clash.Cores.Xilinx.Unisim.Series7
  ( module Clash.Cores.Xilinx.Unisim.Series7.ClockBuffer
  -- The 7 series I/O primitives (Clash.Cores.Xilinx.Unisim.Series7.Serdes and
  -- Clash.Cores.Xilinx.Unisim.Series7.Delay) are re-exported here too, once
  -- they are merged.
  ) where

import Clash.Cores.Xilinx.Unisim.Series7.ClockBuffer
