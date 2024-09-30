{- |
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  A collection of Clock Domain Crossing (CDC) focused Xilinx Parameterized
  Macros (XPMs). These macros are supported by all modern Xilinx FPGA platforms,
  including UltraScale and UltraScale+. Use of these macros is preferred over
  manual clock domain crossings, as they include any constraints needed to make
  them work reliably. They will also automatically suppress warnings typically
  generated by manual CDC, leading to fewer false negatives in logs.

  __N.B.__: Care should always be taken when doing CDC, even when using vendor
            vetted primitives. Always read the documentation and make sure the
            primitives' prerequisites and invariants are upheld.
-}
module Clash.Cores.Xilinx.Xpm.Cdc (
  xpmCdcArraySingle,
  xpmCdcGray,
  xpmCdcHandshake,
  xpmCdcPulse,
  xpmCdcSingle,
  xpmCdcSyncRst,
) where

import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle
import Clash.Cores.Xilinx.Xpm.Cdc.Gray
import Clash.Cores.Xilinx.Xpm.Cdc.Handshake
import Clash.Cores.Xilinx.Xpm.Cdc.Pulse
import Clash.Cores.Xilinx.Xpm.Cdc.Single
import Clash.Cores.Xilinx.Xpm.Cdc.SyncRst
