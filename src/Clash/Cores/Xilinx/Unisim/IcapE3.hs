{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Internal configuration access port of UltraScale and UltraScale+ devices
(@ICAPE3@): read and write the configuration registers from the fabric, for
example to read the device's IDCODE or to trigger a reconfiguration from
another bitstream (@IPROG@). For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/ICAPE3)
and the /UltraScale Architecture Configuration/ user guide (UG570).
-}
module Clash.Cores.Xilinx.Unisim.IcapE3
  ( icapE3
  , IcapE3Output (..)
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Internal (unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (ClockPort (..), Port (..), inst, unPort)

-- | Outputs of 'icapE3'
data IcapE3Output dom = IcapE3Output
  { icapAvail :: Signal dom Bool
  -- ^ The port is available for reads and writes (@AVAIL@)
  , icapData :: Signal dom (BitVector 32)
  -- ^ Data read from the configuration logic (@O@)
  , icapPrDone :: Signal dom Bool
  -- ^ Partial reconfiguration done (@PRDONE@)
  , icapPrError :: Signal dom Bool
  -- ^ Partial reconfiguration error (@PRERROR@)
  }

{- | Internal configuration access port (@ICAPE3@). Data is transferred at the
rising edges of the clock while the port is selected. Like on the SelectMAP
interface, the bits of every byte of the data are in reversed order: see UG570
for the bit ordering and for the command sequences.

The clock may run at up to 200 MHz (see the data sheet of your device). The
primitive's attributes are left at their defaults (@ICAP_AUTO_SWITCH =
\"DISABLE\"@).

Simulation: no configuration logic is simulated. The port is always available,
reads return 0, and partial reconfiguration is never done or erroneous. The
UNISIM simulation model does simulate the configuration logic, but only becomes
available after about 1.2 ms of simulated time; it can be used to test a design
that uses this function in HDL simulation.
-}
icapE3 ::
  forall dom.
  (KnownDomain dom) =>
  -- | Clock (@CLK@)
  Clock dom ->
  -- | Select the port (inverse of @CSIB@)
  Signal dom Bool ->
  -- | Read ('True') or write ('False') (@RDWRB@)
  Signal dom Bool ->
  -- | Data to write (@I@)
  Signal dom (BitVector 32) ->
  IcapE3Output dom
icapE3 clk sel rdwr dat
  | clashSimulation =
      IcapE3Output
        { icapAvail = pure True
        , icapData = pure 0
        , icapPrDone = pure False
        , icapPrError = pure False
        }
  | otherwise =
      IcapE3Output
        { icapAvail = bitToBool <$> unPort avail
        , icapData = unPort o
        , icapPrDone = bitToBool <$> unPort prDone
        , icapPrError = bitToBool <$> unPort prError
        }
 where
  (avail, o, prDone, prError) = go
  go ::
    ( Port "AVAIL" dom Bit
    , Port "O" dom (BitVector 32)
    , Port "PRDONE" dom Bit
    , Port "PRERROR" dom Bit
    )
  go =
    inst
      (unisim "ICAPE3")
      (ClockPort @"CLK" clk)
      (Port @"CSIB" (boolToBit . not <$> sel))
      (Port @"I" dat)
      (Port @"RDWRB" (boolToBit <$> rdwr))
