{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Differential I/O buffers of UltraScale and UltraScale+ devices. For more
information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/I/O-Components).

I/O standards and other placement-related attributes (@IOSTANDARD@, @DIFF_TERM@,
...) are not set by these functions; set them in your constraints file instead.
-}
module Clash.Cores.Xilinx.Unisim.IoBuffer
  ( -- * Differential input buffers
    ibufds
  , ibufdsClock
  , ibufdsGte3
  , ibufdsGte4
    -- * Differential output buffers
  , obufds
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (DiffClock (..))

import Clash.Cores.Xilinx.Ibufds (ibufdsClock)
import Clash.Cores.Xilinx.Unisim.Internal (gatedClockModel, unisim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  (ClockPort (..), NamedDiffClockPort (..), Param (..), Port (..), inst, unPort)

{- | Differential input buffer (@IBUFDS@) for a data signal. Use 'ibufdsClock'
for clocks. Simulation: the positive input.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS).
-}
ibufds ::
  forall dom.
  (KnownDomain dom) =>
  -- | Positive input (@I@)
  Signal dom Bit ->
  -- | Negative input (@IB@)
  Signal dom Bit ->
  Signal dom Bit
ibufds p n
  | clashSimulation = p
  | otherwise = unPort go
 where
  go :: Port "O" dom Bit
  go = inst (unisim "IBUFDS") (Port @"I" p) (Port @"IB" n)

{- | Differential output buffer (@OBUFDS@) for a data signal. Returns the
positive and negative output. Simulation: the input and its complement.

To forward a clock off-chip, drive the buffer with an
'Clash.Cores.Xilinx.Unisim.Ddr.oddre1'
that outputs @(1, 0)@ every cycle instead of connecting the clock to it.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/OBUFDS).
-}
obufds ::
  forall dom.
  (KnownDomain dom) =>
  Signal dom Bit ->
  -- | Positive (@O@) and negative (@OB@) output
  (Signal dom Bit, Signal dom Bit)
obufds i
  | clashSimulation = (i, complement <$> i)
  | otherwise = (unPort o, unPort ob)
 where
  (o, ob) = go
  go :: (Port "O" dom Bit, Port "OB" dom Bit)
  go = inst (unisim "OBUFDS") (Port @"I" i)

{- | Differential input buffer for the reference clock of a GTH or GTY
transceiver (@IBUFDS_GTE4@) on UltraScale+ devices. Returns:

  1. The clock for the transceivers' reference clock inputs (@O@). This output
     can only drive transceiver clock inputs, not the fabric.

  2. The clock for the fabric (@ODIV2@), divided by @div@ (1 or 2). It can only
     drive a @BUFG_GT@. When the divider is 2, the first rising edge of the
     input clock produces a rising edge of this output.

The clock enable (@CEB@) is tied to always enabled.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS_GTE4)
and the /UltraScale Architecture GTH\/GTY Transceivers/ user guides (UG576,
UG578).
-}
ibufdsGte4 ::
  forall div dom domDiv.
  ( KnownDomain dom
  , KnownDomain domDiv
  , 1 <= div
  , div <= 2
  , DomainPeriod domDiv ~ div * DomainPeriod dom
  ) =>
  -- | Divider of @ODIV2@ (@REFCLK_HROW_CK_SEL@)
  SNat div ->
  DiffClock dom ->
  -- | @O@ and @ODIV2@
  (Clock dom, Clock domDiv)
ibufdsGte4 divider@SNat clkIn
  | clashSimulation
  , DiffClock clkP _ <- clkIn =
      (clkP, gatedClockModel (snatToNum divider) clkP (pure False) (pure True))
  | otherwise = (unPort o, unPort odiv2)
 where
  (o, odiv2) = go
  go :: (ClockPort "O" dom, ClockPort "ODIV2" domDiv)
  go =
    inst
      (unisim "IBUFDS_GTE4")
      (Param @"REFCLK_EN_TX_PATH" @Bit 0)
      (Param @"REFCLK_HROW_CK_SEL" @(BitVector 2) (if snatToInteger divider == 1 then 0b00 else 0b01))
      (Param @"REFCLK_ICNTL_RX" @(BitVector 2) 0b00)
      (NamedDiffClockPort @"I" @"IB" clkIn)
      (Port @"CEB" (pure 0 :: Signal dom Bit))

{- | Differential input buffer for the reference clock of a GTH or GTY
transceiver (@IBUFDS_GTE3@) on UltraScale devices. Like 'ibufdsGte4', which is
its UltraScale+ counterpart.

For more information see
[UG974](https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS_GTE3)
and the /UltraScale Architecture GTH\/GTY Transceivers/ user guides (UG576,
UG578).
-}
ibufdsGte3 ::
  forall div dom domDiv.
  ( KnownDomain dom
  , KnownDomain domDiv
  , 1 <= div
  , div <= 2
  , DomainPeriod domDiv ~ div * DomainPeriod dom
  ) =>
  -- | Divider of @ODIV2@ (@REFCLK_HROW_CK_SEL@)
  SNat div ->
  DiffClock dom ->
  -- | @O@ and @ODIV2@
  (Clock dom, Clock domDiv)
ibufdsGte3 divider@SNat clkIn
  | clashSimulation
  , DiffClock clkP _ <- clkIn =
      (clkP, gatedClockModel (snatToNum divider) clkP (pure False) (pure True))
  | otherwise = (unPort o, unPort odiv2)
 where
  (o, odiv2) = go
  go :: (ClockPort "O" dom, ClockPort "ODIV2" domDiv)
  go =
    inst
      (unisim "IBUFDS_GTE3")
      (Param @"REFCLK_EN_TX_PATH" @Bit 0)
      (Param @"REFCLK_HROW_CK_SEL" @(BitVector 2) (if snatToInteger divider == 1 then 0b00 else 0b01))
      (Param @"REFCLK_ICNTL_RX" @(BitVector 2) 0b00)
      (NamedDiffClockPort @"I" @"IB" clkIn)
      (Port @"CEB" (pure 0 :: Signal dom Bit))
