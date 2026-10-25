{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.LowLatencyHandshake
  ( xpmCdcLowLatencyHandshake
  , XpmCdcLowLatencyHandshakeConfig(..)
  , xpmCdcLowLatencyHandshakeWith
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Single (XpmCdcSingleConfig(..), xpmCdcSingleWith)

-- | Synchronizes data from the source clock domain to the destination using a
-- valid/ready interface on both sides. A transfer happens on the source side
-- when both @src_valid@ and @src_ready@ are asserted, and on the destination
-- side when both @dest_valid@ and @dest_ready@ are asserted. Unlike
-- 'Clash.Cores.Xilinx.Xpm.Cdc.Handshake.xpmCdcHandshake', the handshake
-- signals do not need to be deasserted between transfers. This makes it
-- possible to transfer data at a higher rate.
--
-- By default, it uses four synchronization stages in both source and
-- destination domains, and auto-detects whether to use initial values for the
-- synchronization registers. Use 'xpmCdcLowLatencyHandshakeWith' to change
-- these settings. For more information see
-- [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_LOW_LATENCY_HANDSHAKE).
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set it using
--           'xpmCdcLowLatencyHandshakeWith'.
xpmCdcLowLatencyHandshake ::
  forall a src dst.
  ( 1 <= BitSize a, BitSize a <= 2048
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HasCallStack
  ) =>
  Clock src ->
  Clock dst ->
  -- | Word to synchronize to destination domain. This value is captured when
  -- both @src_valid@ and @src_ready@ are asserted.
  "src_in" ::: Signal src a ->

  -- | Assertion of this signal indicates that @src_in@ is valid and should be
  -- synchronized to the destination domain.
  "src_valid" ::: Signal src Bool ->

  -- | Assertion of this signal indicates that the destination logic accepts the
  -- data on @dest_out@. Pass @pure True@ to accept data as soon as it arrives,
  -- i.e., to make @dest_valid@ a single cycle pulse.
  "dest_ready" ::: Signal dst Bool ->

  -- | @dest_valid@ indicates that @dest_out@ contains valid data. It is
  -- deasserted after the destination logic accepts the data. @src_ready@
  -- indicates that the macro can accept new data on @src_in@.
  ( "dest_out"   ::: Signal dst a
  , "dest_valid" ::: Signal dst Bool
  , "src_ready"  ::: Signal src Bool
  )
xpmCdcLowLatencyHandshake = xpmCdcLowLatencyHandshakeWith XpmCdcLowLatencyHandshakeConfig{..}
 where
  srcStages = d4
  dstStages = d4
  initialValues =
    case (initBehavior @src, initBehavior @dst) of
      (SDefined, SDefined) -> True
      (SUnknown, SUnknown) -> False
      _ -> clashCompileError $ "xpmCdcLowLatencyHandshake: domains need to agree on initial value "
                            <> "behavior. To set initial value usage explicitly, "
                            <> "consider using 'xpmCdcLowLatencyHandshakeWith'."
{-# INLINE xpmCdcLowLatencyHandshake #-}

-- | Configuration for 'xpmCdcLowLatencyHandshakeWith'
--
-- Other attributes that are hardcoded:
--
-- +------------------+-------+
-- | Attribute        | Value |
-- +==================+=======+
-- | @DEST_EXT_HSK@   |     1 |
-- +------------------+-------+
-- | @SIM_ASSERT_CHK@ |     0 |
-- +------------------+-------+
data XpmCdcLowLatencyHandshakeConfig srcStages dstStages = XpmCdcLowLatencyHandshakeConfig
  { -- | Number of registers, clocked by the src clock, that are used to
    -- synchronize the acknowledgement of the destination domain.
    --
    -- This is what [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_LOW_LATENCY_HANDSHAKE)
    -- calls @SRC_SYNC_FF@.
    srcStages :: SNat srcStages

    -- | Number of registers, clocked by the dst clock, that are used to
    -- synchronize the request of the source domain.
    --
    -- This is what [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_LOW_LATENCY_HANDSHAKE)
    -- calls @DEST_SYNC_FF@.
  , dstStages :: SNat dstStages

    -- | Initialize registers used within the primitive to /0/. Note that
    -- 'xpmCdcLowLatencyHandshake' will set this to 'True' if both domains
    -- support initial values, to 'False' if neither domain does, and will
    -- otherwise emit an error.
    --
    -- This is what [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_LOW_LATENCY_HANDSHAKE)
    -- calls @INIT_SYNC_FF@.
  , initialValues :: Bool
  }

-- | Like 'xpmCdcLowLatencyHandshake', but with a configurable number of stages
-- and initial values. Also see 'XpmCdcLowLatencyHandshakeConfig'.
xpmCdcLowLatencyHandshakeWith ::
  forall srcStages dstStages a src dst.
  ( 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  , 1 <= BitSize a, BitSize a <= 2048
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HasCallStack
  ) =>
  XpmCdcLowLatencyHandshakeConfig srcStages dstStages ->
  Clock src ->
  Clock dst ->

  -- | Word to synchronize to destination domain. This value is captured when
  -- both @src_valid@ and @src_ready@ are asserted.
  "src_in" ::: Signal src a ->

  -- | Assertion of this signal indicates that @src_in@ is valid and should be
  -- synchronized to the destination domain.
  "src_valid" ::: Signal src Bool ->

  -- | Assertion of this signal indicates that the destination logic accepts the
  -- data on @dest_out@. Pass @pure True@ to accept data as soon as it arrives,
  -- i.e., to make @dest_valid@ a single cycle pulse.
  "dest_ready" ::: Signal dst Bool ->

  -- | @dest_valid@ indicates that @dest_out@ contains valid data. It is
  -- deasserted after the destination logic accepts the data. @src_ready@
  -- indicates that the macro can accept new data on @src_in@.
  ( "dest_out"   ::: Signal dst a
  , "dest_valid" ::: Signal dst Bool
  , "src_ready"  ::: Signal src Bool
  )
xpmCdcLowLatencyHandshakeWith XpmCdcLowLatencyHandshakeConfig{srcStages=srcStages@SNat, dstStages=dstStages@SNat, ..} clkSrc clkDst srcIn srcValid dstReady
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Definition used in for HDL generation
  synth = (unpack <$> unPort go0, bitCoerce <$> unPort go1,  bitCoerce <$> unPort go2)
   where
    (go0,go1,go2) = go
    go :: ( Port "dest_out"   dst (BitVector (BitSize a))
          , Port "dest_valid" dst Bit
          , Port "src_ready"  src Bit
          )

    go =
      inst
        (instConfig "xpm_cdc_low_latency_handshake")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_EXT_HSK"   @Integer 1)
        (Param @"DEST_SYNC_FF"   @Integer (natToNum @dstStages))

        (Param @"INIT_SYNC_FF"   @Integer (if initialValues then 1 else 0))
        (Param @"SIM_ASSERT_CHK" @Integer 0)
        (Param @"SRC_SYNC_FF"    @Integer (natToNum @srcStages))
        (Param @"WIDTH"          @Integer (natToNum @(BitSize a)))

        (ClockPort @"dest_clk"   clkDst)
        (Port      @"dest_ready" (bitCoerce @Bool @Bit <$> dstReady))

        (ClockPort @"src_clk"    clkSrc)
        (Port      @"src_in"     (pack <$> srcIn))
        (Port      @"src_valid"  (bitCoerce @Bool @Bit <$> srcValid))

  -- Definition used in Clash simulation. Both domains keep a one bit counter
  -- that toggles on every transfer. The source side may transfer when the
  -- counters are equal, the destination side when they differ. Unlike other
  -- registers, the macro always initializes the counters to /0/, regardless of
  -- @INIT_SYNC_FF@.
  sim = (dstOut, dstValid, srcReady)

  defOpts :: forall stages. SNat stages -> XpmCdcSingleConfig stages
  defOpts nStages = XpmCdcSingleConfig
    { stages = nStages
    , initialValues = initialValues
    , registerInput = False }

  -- Source domain
  srcTransfer = srcValid .&&. srcReady
  srcCountFf = False :- (xor <$> srcCountFf <*> srcTransfer)
  srcHsDataFf = registerEn (initVal (unpack 0)) srcTransfer srcIn
  srcCountEq = (==) <$> srcCountFf <*> dstCountSynced
  srcReady = initVal False :- (srcCountEq .&&. fmap not srcTransfer)

  -- Destination domain
  dstTransfer = dstValid .&&. dstReady
  dstCountFf = False :- (xor <$> dstCountFf <*> dstTransfer)
  dstCountEq = (==) <$> srcCountSynced <*> dstCountFf
  dstOutEna = fmap not dstCountEq .&&. fmap not dstValid
  dstOut =
    registerEn
      (initVal (unpack 0)) dstOutEna
      (unsafeSynchronizer clkSrc clkDst srcHsDataFf)
  dstValid = initVal False :- (fmap not dstCountEq .&&. fmap not dstTransfer)

  -- Crossings
  srcCountSynced = xpmCdcSingleWith (defOpts dstStages) clkSrc clkDst srcCountFf
  dstCountSynced = xpmCdcSingleWith (defOpts srcStages) clkDst clkSrc dstCountFf

  -- Register with an enable. Like the macro's registers, its initial value does
  -- not depend on the domain's initial value behavior.
  registerEn :: forall dom x . x -> Signal dom Bool -> Signal dom x -> Signal dom x
  registerEn i ena d = o where o = i :- mux ena d o

  initVal :: forall x . NFDataX x => x -> x
  initVal v
    | initialValues = v
    | otherwise = deepErrorX "xpmCdcLowLatencyHandshake: initial values undefined"

{-# INLINE xpmCdcLowLatencyHandshakeWith #-}
