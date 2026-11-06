{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

UNISIM primitives of UltraScale and UltraScale+ devices. For more information
see [UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries).

Every primitive has a simulation model for Clash simulation and instantiates the
real primitive in HDL. The simulation models are tested against Xilinx's UNISIM
simulation models; see the documentation of the individual modules for what
they do and do not model.

For the UNISIM primitives of 7 series devices (Artix-7, Kintex-7, Virtex-7,
Zynq-7000), see "Clash.Cores.Xilinx.Unisim.Series7". Some of the primitives
exported here exist on 7 series devices too; that module lists them.
-}
module Clash.Cores.Xilinx.Unisim
  ( module Clash.Cores.Xilinx.Unisim.ClockBuffer
  , module Clash.Cores.Xilinx.Unisim.Ddr
  , module Clash.Cores.Xilinx.Unisim.Delay
  , module Clash.Cores.Xilinx.Unisim.DnaPortE2
  , module Clash.Cores.Xilinx.Unisim.IcapE3
  , module Clash.Cores.Xilinx.Unisim.IoBuffer
  , module Clash.Cores.Xilinx.Unisim.Serdes
  , module Clash.Cores.Xilinx.Unisim.StartupE3
  , module Clash.Cores.Xilinx.Unisim.UsrAccessE2
  ) where

import Clash.Cores.Xilinx.Unisim.ClockBuffer
import Clash.Cores.Xilinx.Unisim.Ddr
import Clash.Cores.Xilinx.Unisim.Delay
import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Clash.Cores.Xilinx.Unisim.IcapE3
import Clash.Cores.Xilinx.Unisim.IoBuffer
import Clash.Cores.Xilinx.Unisim.Serdes
import Clash.Cores.Xilinx.Unisim.StartupE3
import Clash.Cores.Xilinx.Unisim.UsrAccessE2
