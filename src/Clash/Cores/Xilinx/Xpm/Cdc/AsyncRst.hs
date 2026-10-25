{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.AsyncRst
  ( xpmCdcAsyncRst
  , XpmCdcAsyncRstConfig(..)
  , xpmCdcAsyncRstWith
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (ClockAB(..), Signal((:-)), clockTicks)

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | This macro synchronizes an asynchronous reset signal to the destination
-- clock domain. The generated output asserts asynchronously and deasserts
-- synchronously to the destination clock domain. After deassertion, the output
-- stays asserted for at least as many destination clock cycles as there are
-- synchronization stages. By default, it uses four synchronization stages, and
-- uses initial values for the synchronization registers if the destination
-- domain supports them. Use 'xpmCdcAsyncRstWith' to change these settings. For
-- more information see
-- [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_ASYNC_RST).
--
-- __N.B.__: The source clock is only used in Clash simulation. As the source
--           reset is asynchronous, it is not connected in HDL.
--
-- __N.B.__: The source reset must be glitch-free, i.e., driven directly by a
--           register or an input port. Any glitch asserts the destination
--           reset, which Clash simulation does not model.
xpmCdcAsyncRst ::
  forall src dst.
  ( KnownDomain src
  , KnownDomain dst
  , HasCallStack
  ) =>
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmCdcAsyncRst = xpmCdcAsyncRstWith XpmCdcAsyncRstConfig{..}
 where
  stages = d4
  initialValues =
    case initBehavior @dst of
      SDefined -> True
      SUnknown -> False
{-# INLINE xpmCdcAsyncRst #-}

-- | Configuration for 'xpmCdcAsyncRstWith'
--
-- Other attributes that are hardcoded:
--
-- +-------------------+-------+
-- | Attribute         | Value |
-- +===================+=======+
-- | @RST_ACTIVE_HIGH@ |     1 |
-- +-------------------+-------+
data XpmCdcAsyncRstConfig stages = XpmCdcAsyncRstConfig
  { -- | Number of synchronization stages, i.e., number of registers in the
    -- destination domain. This is also the minimum number of destination clock
    -- cycles the output reset is asserted for.
    stages :: SNat stages

    -- | Initialize registers used within the primitive to a deasserted reset.
    -- Note that 'xpmCdcAsyncRst' will set this to 'True' if the destination
    -- domain supports initial values, and to 'False' if it does not.
  , initialValues :: Bool
  }

-- | Like 'xpmCdcAsyncRst', but with a configurable number of stages and initial
-- values. Also see 'XpmCdcAsyncRstConfig'.
xpmCdcAsyncRstWith ::
  forall stages src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  ) =>
  XpmCdcAsyncRstConfig stages ->
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmCdcAsyncRstWith XpmCdcAsyncRstConfig{stages=stages@SNat, ..} clkSrc clkDst srcIn
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Definition used in for HDL generation
  synth = unPort go
   where
    go :: ResetPort "dest_arst" 'ActiveHigh dst
    go =
      inst
        (instConfig "xpm_cdc_async_rst")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_SYNC_FF"    @Integer (natToNum @stages))
        (Param @"INIT_SYNC_FF"    @Integer (if initialValues then 1 else 0))
        (Param @"RST_ACTIVE_HIGH" @Integer 1)

        (ClockPort @"dest_clk" clkDst)
        (ResetPort @"src_arst" @'ActiveHigh srcIn)

  -- Definition used in Clash simulation
  sim = unsafeFromActiveHigh $ go (snatToNum stages)
   where
    srcRst = unsafeToActiveHigh srcIn

    initVal
      | initialValues = False
      | otherwise = deepErrorX "xpmCdcAsyncRst: initial values undefined"

    -- Whether the source reset was asserted at the edge of the destination clock
    rstAtEdge = unsafeSynchronizer clkSrc clkDst srcRst

    -- Output of the /n/th synchronization register, as seen just before a
    -- destination clock edge. A source reset asserted at any point since the
    -- previous edge asserts all registers asynchronously.
    go :: Word -> Signal dst Bool
    go 0 = pure False
    go n = rstSinceEdge .||. (initVal :- (rstAtEdge .||. go (n - 1)))

    -- Whether the source reset was asserted at any point since the previous
    -- destination clock edge. Unlike 'unsafeSynchronizer', this does not miss
    -- source resets that are deasserted before the next destination clock edge.
    rstSinceEdge = goTicks (clockTicks clkSrc clkDst) False srcRst

    goTicks :: [ClockAB] -> Bool -> Signal src Bool -> Signal dst Bool
    goTicks [] _ _ = error "xpmCdcAsyncRst: `ticks` should have been an infinite list"
    goTicks (tick:ticks) seen rsts@(~(rst :- rsts0)) =
      case tick of
        ClockA  -> goTicks ticks (seen || rst) rsts0
        ClockB  -> (seen || rst) :- goTicks ticks False rsts
        ClockAB -> goTicks (ClockB:ClockA:ticks) seen rsts
{-# INLINE xpmCdcAsyncRstWith #-}
