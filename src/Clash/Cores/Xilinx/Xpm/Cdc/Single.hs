{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Single
  ( xpmCdcSingle
  , XpmCdcSingleConfig(..)
  , xpmCdcSingleWith
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | Synchronizes a single bit from the source clock domain to the destination.
-- By default, it registers its input, uses four synchronization stages, and
-- auto-detects whether to use initial values for the synchronization
-- registers. Use 'xpmCdcSingleWith' to change these settings. For more
-- information see [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_SINGLE).
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set it using
--           'xpmCdcSingleWith'.
xpmCdcSingle ::
  forall a src dst.
  ( KnownDomain src
  , KnownDomain dst
  , HasCallStack
  , NFDataX a
  , BitPack a
  , BitSize a ~ 1
  ) =>
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcSingle = xpmCdcSingleWith XpmCdcSingleConfig{..}
 where
  registerInput = True
  stages = d4
  initialValues =
    case (initBehavior @src, initBehavior @dst) of
      (SDefined, SDefined) -> True
      (SUnknown, SUnknown) -> False
      _ -> clashCompileError $ "xpmCdcSingle: domains need to agree on initial value "
                            <> "behavior. To set initial value usage explicitly, "
                            <> "consider using 'xpmCdcSingleWith'."
{-# INLINE xpmCdcSingle #-}

-- | Configuration for 'xpmCdcSingleWith'
data XpmCdcSingleConfig stages = XpmCdcSingleConfig
  { -- | Number of synchronization stages, i.e., number of registers in the
    -- destination domain.
    stages :: SNat stages

    -- | Initialize registers used within the primitive to /0/. Note that
    -- 'xpmCdcSingle' will set this to 'True' if both domains support initial
    -- values, to 'False' if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Bool

    -- | Register input. This makes sure the synchronization pipeline does not
    -- see glitches.
  , registerInput :: Bool
  }

-- | Like 'xpmCdcSingle', but with a configurable number of stages, initial values,
-- and registered input. Also see 'XpmCdcSingleConfig'.
xpmCdcSingleWith ::
  forall stages a src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  , NFDataX a
  , BitPack a
  , BitSize a ~ 1
  ) =>
  XpmCdcSingleConfig stages ->
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcSingleWith XpmCdcSingleConfig{stages=stages@SNat, ..} clkSrc clkDst srcIn
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Definition used in for HDL generation
  synth = bitCoerce @_ @a <$> unPort go
   where
    go :: Port "dest_out" dst Bit
    go =
      inst
        (instConfig "xpm_cdc_single")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_SYNC_FF"   @Integer (natToNum @stages))
        (Param @"INIT_SYNC_FF"   @Integer (if initialValues then 1 else 0))
        (Param @"SIM_ASSERT_CHK" @Integer 0)
        (Param @"SRC_INPUT_REG"  @Integer (if registerInput then 1 else 0))

        (ClockPort @"src_clk"  clkSrc)
        (ClockPort @"dest_clk" clkDst)
        (Port      @"src_in"   (bitCoerce @_ @Bit <$> srcIn))

  -- Definition used in Clash simulation
  sim
    | registerInput = go (snatToNum stages) (initVal :- srcIn)
    | otherwise     = go (snatToNum stages) srcIn
   where
    initVal
      | initialValues = unpack 0
      | otherwise = deepErrorX "xpmCdcSingle: initial values undefined"

    go :: Word -> Signal src a -> Signal dst a
    go 0 src = unsafeSynchronizer clkSrc clkDst src
    go n src = initVal :- go (n - 1) src
{-# INLINE xpmCdcSingleWith #-}
