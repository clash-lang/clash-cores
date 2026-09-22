{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Clock domains of the KCU105 SGMII test design
-}
module Kcu105.Sgmii.Domains where

import Clash.Explicit.Prelude

-- | Free-running 125 MHz board clock (@CLK_125MHZ_P/N@)
createDomain
  vXilinxSystem
    { vName = "Ext125"
    , vPeriod = hzToPeriod 125e6
    , vResetKind = Asynchronous
    }

-- | 625 MHz SGMII clock from the PHY (@SGMIICLK_P/N@), the input of the MMCM
createDomain
  vXilinxSystem
    { vName = "Phy625"
    , vPeriod = 1600
    , vResetKind = Asynchronous
    }

-- | MMCM output: the SERDES bit clock, 625 MHz used at both edges for 1.25 Gb/s
createDomain vXilinxSystem{vName = "Serdes625", vPeriod = 1600}

-- | MMCM output: the SERDES parallel clock, 312.5 MHz, four bits per cycle
createDomain vXilinxSystem{vName = "Serdes312", vPeriod = 3200}

-- | MMCM output: the code group clock, 125 MHz, one 10-bit code group per cycle
createDomain vXilinxSystem{vName = "Pcs125", vPeriod = 8000}

-- | Simulation only: the serial line, one bit per cycle at 1.25 Gb/s
createDomain vXilinxSystem{vName = "Line1250", vPeriod = 800}
