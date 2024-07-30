{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Gray
  ( xpmCdcGray
  , XpmCdcGrayConfig(..)
  , xpmCdcGrayWith
  ) where

import GHC.Stack (HasCallStack)

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | Synchronizes an 'Unsigned' from the source clock domain to the destination
-- clock domain using Gray code. It instantiates Xilinx's @XPM_CDC_GRAY@, so no
-- additional constraint definitions are needed. In order to synchronize data
-- from the source domain to the destination domain, consecutive inputs need to
-- be unchanged, successors (+1), or predecessors (-1). Overflows are okay. That is,
-- 'maxBound' can be followed by zero, and vice versa. If this invariant is
-- violated, the component might not transfer data correctly until the invariant
-- is upheld again and the source clock has sampled the input once, followed by
-- /4/ samples in the destination domain.
--
-- If you need all data to be transferred from the source domain to the destination
-- domain, make sure the destination domain samples the input domain at least
-- twice. Alternatively, use a FIFO.
--
-- Read more in [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_GRAY).
--
-- __N.B.__: The simulation model does not detect invariant violations.
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set initial value
--           usage by using 'xpmCdcGrayWith'.
--
xpmCdcGray ::
  forall n src dst.
  ( 2 <= n, n <= 32
  , KnownNat n
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  ) =>
  Clock src ->
  Clock dst ->
  Signal src (Unsigned n) ->
  Signal dst (Unsigned n)
xpmCdcGray = xpmCdcGrayWith XpmCdcGrayConfig{..}
 where
  stages = d4
  initialValues =
    case (initBehavior @src, initBehavior @dst) of
      (SDefined, SDefined) -> True
      (SUnknown, SUnknown) -> False
      _ -> clashCompileError $ "xpmCdcGray: domains need to agree on initial value "
                            <> "behavior. To set initial value usage explicitly, "
                            <> "consider using 'xpmCdcGrayWith'."
{-# INLINE xpmCdcGray #-}

-- | Configuration for 'xpmCdcGrayWith'
data XpmCdcGrayConfig stages = XpmCdcGrayConfig
  { -- | Number of synchronization stages. I.e., number of registers in the
    -- destination domain. Note that there is always a register in the source
    -- domain.
    stages :: SNat stages

    -- | Initialize registers used within the primitive to /0/. Note that
    -- 'xpmCdcGray' will set this to 'True' if both domains support initial
    -- values, to 'False' if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Bool
  }

-- | Like 'xpmCdcGray', but with a configurable number of stages. Also see
-- 'XpmCdcGrayConfig'.
xpmCdcGrayWith ::
  forall stages n src dst.
  ( 2 <= n, n <= 32
  , 2 <= stages, stages <= 10
  , KnownNat n
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  ) =>
  XpmCdcGrayConfig stages ->
  Clock src ->
  Clock dst ->
  Signal src (Unsigned n) ->
  Signal dst (Unsigned n)
xpmCdcGrayWith XpmCdcGrayConfig{stages=stages@SNat, ..} clkSrc clkDst srcIn
  -- = xpmCdcGray# initialValues stages
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Definition used in for HDL generation
  synth = unpack <$> unPort go
   where
    go :: Port "dest_out_bin" dst (BitVector n)
    go =
      inst
        (instConfig "xpm_cdc_gray")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_SYNC_FF"          @Integer (natToNum @stages))
        (Param @"INIT_SYNC_FF"          @Integer (if initialValues then 1 else 0))
        (Param @"REG_OUTPUT"            @Integer 0)
        (Param @"SIM_ASSERT_CHK"        @Integer 0)
        (Param @"SIM_LOSSLESS_GRAY_CHK" @Integer 0)
        (Param @"WIDTH"                 @Integer (natToNum @n))

        (ClockPort @"src_clk"    clkSrc)
        (ClockPort @"dest_clk"   clkDst)
        (Port      @"src_in_bin" (pack <$> srcIn))
  -- Definition used in Clash simulation
  sim
    = go (snatToNum stages) (initVal :- srcIn)
   where
    initVal
      | initialValues = 0
      | otherwise = errorX "xpmCdcGray: initial values undefined"

    go :: Word -> Signal src (Unsigned n) -> Signal dst (Unsigned n)
    go 0 src = unsafeSynchronizer clkSrc clkDst src
    go n src = initVal :- go (n - 1) src
{-# INLINE xpmCdcGrayWith #-}
