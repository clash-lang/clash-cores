{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's Internal Oscillator IP (@altera_int_osc@): a clock that
doesn't need an external oscillator. See the
[MAX 10 Clocking and PLL User Guide](https://www.intel.com/content/www/us/en/docs/programmable/683047/).
Checked with Quartus Prime Lite 21.1 for MAX 10, Cyclone V and Cyclone 10 LP
devices.

Clash generates the IP with Platform Designer, see "Clash.Cores.Intel.Internal".
-}
module Clash.Cores.Intel.Config.Oscillator
  ( internalOscillator
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (unPort)

{- | The internal oscillator. Its frequency isn't accurate: for MAX 10, the
data sheet specifies 55 MHz to 116 MHz for 10M02 to 10M25 devices, and 35 MHz
to 77 MHz for 10M40 and 10M50 devices. For Cyclone IV, Cyclone 10 LP and
Cyclone V devices, the IP Parameter Editor gives a maximum of 80 MHz,
respectively 100 MHz. Choose the period of the domain accordingly, e.g. the
maximum frequency for timing analysis. The oscillator is always enabled.

Simulation: a clock of the domain. Intel's simulation models of the
oscillator of MAX 10 and Cyclone V devices are encrypted, and can only be
simulated with Intel's Questa edition, so the generated HDL is only checked
with Quartus synthesis. (The model of Cyclone 10 LP devices is a plain 80 MHz
clock.)
-}
internalOscillator :: forall dom. KnownDomain dom => Clock dom
internalOscillator
  | clashSimulation = clockGen
  | otherwise = unPort clk
 where
  clk :: ClockPort "clkout" dom
  clk =
    instWithIntelIp
      (instConfig "internal_oscillator")
      IntelIp { ipKind = "altera_int_osc", ipParams = Nil }
      (Port @"oscena" @dom (pure high))
{-# OPAQUE internalOscillator #-}
