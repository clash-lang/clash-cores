{-|
  Copyright  :  (C) 2025, Google Inc,
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Xilinx differential input buffer primitives. For more information see:

https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS
-}
module Clash.Cores.Xilinx.Ibufds where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (DiffClock (..))

import Clash.Cores.Xilinx.Xpm.Cdc.Internal


{- | A differential input buffer for DiffClock. Although the @ibufds@ primitive
can be used for any differential signal, 'ibufdsClock' is specialized for Clock.
-}
ibufdsClock :: forall dom. (KnownDomain dom) => DiffClock dom -> Clock dom
ibufdsClock diffClk
  | clashSimulation, (DiffClock clkP _clkN) <- diffClk = clkP
  | otherwise = synth
 where
  synth = unPort go
   where
    go :: ClockPort "O" dom
    go =
      inst
        (instConfig "IBUFDS")
          { library = Just "UNISIM"
          , libraryImport = Just "UNISIM.vcomponents.all"
          }
        (NamedDiffClockPort @"I" @"IB" diffClk)
