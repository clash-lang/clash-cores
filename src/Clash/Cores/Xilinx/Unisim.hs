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
-}
module Clash.Cores.Xilinx.Unisim
  ( module Clash.Cores.Xilinx.Unisim.ClockBuffer
  , module Clash.Cores.Xilinx.Unisim.DnaPortE2
  ) where

import Clash.Cores.Xilinx.Unisim.ClockBuffer
import Clash.Cores.Xilinx.Unisim.DnaPortE2
