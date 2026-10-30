{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  A collection of Xilinx Parameterized Macros (XPMs) supported by all modern
  Xilinx FPGA platforms, including  UltraScale and UltraScale+. More information
  is available in [Xilinx's documentation](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/Xilinx-Parameterized-Macros).
-}

module Clash.Cores.Xilinx.Xpm
  ( module Clash.Cores.Xilinx.Xpm.Cdc
  , module Clash.Cores.Xilinx.Xpm.Fifo
  , module Clash.Cores.Xilinx.Xpm.Fifo.Axif
  , module Clash.Cores.Xilinx.Xpm.Fifo.Axil
  , module Clash.Cores.Xilinx.Xpm.Fifo.Axis
  ) where

import Clash.Cores.Xilinx.Xpm.Cdc
import Clash.Cores.Xilinx.Xpm.Fifo
import Clash.Cores.Xilinx.Xpm.Fifo.Axif
import Clash.Cores.Xilinx.Xpm.Fifo.Axil
import Clash.Cores.Xilinx.Xpm.Fifo.Axis
