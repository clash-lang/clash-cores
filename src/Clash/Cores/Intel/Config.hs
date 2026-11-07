{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Configuration and system blocks of Intel FPGAs: the unique chip ID
("Clash.Cores.Intel.Config.ChipId") and the internal oscillator
("Clash.Cores.Intel.Config.Oscillator"). These are hard blocks whose
simulation models Intel only ships encrypted: see the individual modules for
what their Clash simulation models do and don't model.
-}
module Clash.Cores.Intel.Config
  ( module Clash.Cores.Intel.Config.ChipId
  , module Clash.Cores.Intel.Config.Oscillator
  ) where

import Clash.Cores.Intel.Config.ChipId (chipId, chipIdLatency)
import Clash.Cores.Intel.Config.Oscillator (internalOscillator)
