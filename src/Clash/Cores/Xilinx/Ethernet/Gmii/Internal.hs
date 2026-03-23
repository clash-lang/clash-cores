{- |
  Copyright   :  (C) 2024, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

 Contains the types used in the implementation of the GMII to SGMII bridge wrapper
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Ethernet.Gmii.Internal where

import Clash.Explicit.Prelude hiding ((:<))

import Clash.Cores.Xilinx.Xpm.Cdc.Internal
import Clash.Signal.Internal

import qualified Clash.Explicit.Prelude as C

-- For backward compatibility with Clash versions prior to 1.9.0
#if !MIN_VERSION_clash_prelude(1,9,0)
type Nanoseconds (ns :: Natural) = Picoseconds (1000 * ns)
type Picoseconds (ps :: Natural) = ps
#endif

-- | A differential pair of Low Voltage Differential Signaling (LVDS) signals
data Lvds = Lvds {pChannel :: Bit, nChannel :: Bit}
  deriving (Generic, NFDataX, BitPack)

-- TODO: Move to re-usable module?

-- | A generic GMII interface
data Gmii = Gmii
  { gmiiData :: BitVector 8
  , gmiiValid :: Bit
  , gmiiError :: Bit
  }
  deriving (Generic, BitPack, NFDataX)

-- The order of the constructors of `DuplexMode`, `LinkSpeed` and `Pause` is
-- important, as their derived `BitPack` instances are used to encode them in
-- the `AutoNegConfigVector`.

data DuplexMode = HalfDuplex | FullDuplex
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Link speed in megabits per second.
data LinkSpeed = Speed10 | Speed100 | Speed1000
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Whether the link partner supports pause frames.
data Pause = NoPause | SymmetricPause | AsymmetricPause | SymmetricAsymmetricPause
  deriving (Generic, NFDataX, BitPack, Eq, Show)

newtype AutoNegConfigVector = AutoNegConfigVector (BitVector 16)
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | A subset of the an_adv_config_vector that is applicable for the GMII to SGMII bridge
data AutoNegConfig
  = AutoNegConfig
  { cAcknowledge :: Bool
  , cDuplexMode :: DuplexMode
  , cLinkSpeed :: LinkSpeed
  , cPhyLinkStatus :: Bool
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Converts the AutoNegConfig Records to the an_adv_config_vector format
toAutoNegConfigVector :: AutoNegConfig -> AutoNegConfigVector
toAutoNegConfigVector AutoNegConfig{..} = AutoNegConfigVector $ pack cfg
 where
  -- See LogiCORE IP Ethernet 1000BASE-X PCS/PMA or SGMII v11.3, page 17,
  -- table 7: Optional Auto-Negotiation Interface Signal Pinout, signal
  -- an_adv_config_vector[15:0]
  -- (SGMII operating in PHY mode)
  cfg :: Vec 16 Bit
  cfg =
    ( bitCoerce cPhyLinkStatus -- Bit 15 - PHY Link Status
        :> bitCoerce cAcknowledge -- Bit 14 - Acknowledge
        :> 0 -- Bit 13 - Reserved
        :> bitCoerce cDuplexMode -- Bit 12 - Duplex Mode
        :> Nil
    )
      ++ bitCoerce cLinkSpeed -- Bit 11:10 - Speed
      ++ ( repeat 0 -- Bit 9:1 - Reserved
           C.:< 1 -- Bit 0 - Always 1
         )

-- | A conservative default configuration for the AutoNegConfig record
instance Default AutoNegConfig where
  def = AutoNegConfig{..}
   where
    cAcknowledge = False
    cDuplexMode = HalfDuplex
    cLinkSpeed = Speed10
    cPhyLinkStatus = False

-- The order of the fields in the `Config` record is important, as their derived
-- `BitPack` instance is used as input for the `gmiiSgmiiBridgePrim` primitive.
-- | A record representation of the configuration_vector
data Config = Config
  { cAutoNegEnable :: Bool
  , cIsolateGmii :: Bool
  , cPowerDown :: Bool
  , cLoopback :: Bool
  , cUnidirectional :: Bool
  }
  deriving (Generic, NFDataX, BitPack)

instance Default Config where
  def = Config{..}
   where
    cUnidirectional = False
    cLoopback = False
    cPowerDown = False
    cIsolateGmii = False
    cAutoNegEnable = False

type StatusVector = BitVector 16

-- | A record representation of the status_vector
data Status = Status
  { sLinkStatus :: Bool
  , sLinkSynchronized :: Bool
  , sRudiC :: Bool
  , sRudiI :: Bool
  , sRudiInvalid :: Bool
  , sReceivedDisparityError :: Bool
  , sReceivedInvalidCode :: Bool
  , sPhyLinkStatus :: Bool
  , sRemoteFault :: Maybe (BitVector 2)
  , sLinkSpeed :: LinkSpeed
  , sDuplexMode :: DuplexMode
  , sPause :: Pause
  }
  deriving (Generic, NFDataX, BitPack)

-- | Transform the status_vector to a Status record
fromStatusVector :: StatusVector -> Status
fromStatusVector v = Status{..}
 where
  -- There is no BitPack instance for 13-tuples without -flarge-tuples, so we break it up.
  ( ( sLinkStatus
      , sLinkSynchronized
      , sRudiC
      , sRudiI
      , sRudiInvalid
      , sReceivedDisparityError
      )
    , sReceivedInvalidCode
    , sPhyLinkStatus
    , remoteFaultCode
    , sLinkSpeed
    , sDuplexMode
    , remoteFaultValid
    , sPause
    ) = unpack v
  sRemoteFault = if remoteFaultValid then Just remoteFaultCode else Nothing

-- | 'Clash.Cores.Xilinx.Ethernet.Gmii.gmiiSgmiiBridge' IP version  to use. Other versions have not been tested and might not work.
-- Unsafe versions should be formatted as "major.minor", e.g. "17.0" or "16.2".
data Version = V16_2 | V17_0 | UnsafeVersion String

versionToString :: Version -> String
versionToString V16_2 = "16.2"
versionToString V17_0 = "17.0"
versionToString (UnsafeVersion v) = v

{- | Primitive for the LogiCORE IP Ethernet 1000BASE-X PCS/PMA or SGMII, configured
to function as a GMII to SGMII bridge using LVDS in MAC mode.
-}
gmiiSgmiiBridgePrim ::
  forall sgmii625 gmii125.
  ( KnownDomain sgmii625
  , KnownDomain gmii125
  , DomainPeriod sgmii625 ~ Picoseconds 1600
  , DomainPeriod gmii125 ~ Nanoseconds 8
  , DomainActiveEdge sgmii625 ~ 'Rising
  , DomainActiveEdge gmii125 ~ 'Rising
  , HasAsynchronousReset sgmii625
  , HasSynchronousReset gmii125
  , DomainResetPolarity sgmii625 ~ 'ActiveHigh
  , DomainResetPolarity gmii125 ~ 'ActiveHigh
  ) =>
  -- | Revision version of the IP. Unsafe versions should be formatted as "major.minor", e.g. "17.0" or "16.2".
  Version ->
  -- | P channel of the reference clock coming from the PHY
  Clock sgmii625 ->
  -- | N channel of the reference clock coming from the PHY
  ClockN sgmii625 ->
  -- | Signal detect from the PHY. Either connect to the PHY's signal detect or use
  -- a constant @True@, otherwise the link will never come up. The IP core considers this
  -- an asynchronous signal, so synchronisation logic is not needed.
  Reset sgmii625 ->
  -- | Signal detect from the PHY. Either connect to the PHY's signal detect or use
  -- a constant @True@, otherwise the link will never come up.
  Signal sgmii625 Bool ->
  -- | Configuration for the bridge
  Signal gmii125 Config ->
  -- | Auto negotiation configuration for the bridge
  Signal gmii125 AutoNegConfigVector ->
  -- | Restart auto negotiation
  Signal gmii125 Bool ->
  -- | P channel of the LVDS input from the PHY
  Signal sgmii625 Bit ->
  -- | N channel of the LVDS input from the PHY
  Signal sgmii625 Bit ->
  -- | GMII data input from the MAC
  Signal gmii125 (BitVector 8) ->
  -- | GMII valid input from the MAC
  Signal gmii125 Bit ->
  -- | GMII error input from the MAC
  Signal gmii125 Bit ->
  -- |
  -- 1. Clock output for the 125 MHz domain
  -- 2. Active high reset output for the 125 MHz domain
  -- 3. P channel of the LVDS output to the PHY
  -- 4. N channel of the LVDS output to the PHY
  -- 5. GMII data output to the MAC
  -- 6. GMII valid output to the MAC
  -- 7. GMII error output to the MAC
  -- 8. Status vector
  ( Clock gmii125
  , Signal gmii125 Bool
  , Signal sgmii625 Bit
  , Signal sgmii625 Bit
  , Signal gmii125 (BitVector 8)
  , Signal gmii125 Bit
  , Signal gmii125 Bit
  , Signal gmii125 StatusVector
  )
gmiiSgmiiBridgePrim
  version
  clockP
  clockN
  rst
  signalDetect
  pmaConfig
  pmaAnAdvancedConfig
  restartAn
  lvdsInP
  lvdsInN
  gmiiTxD
  gmiiTxDv
  gmiiTxEr
  =
  ( unPort clk125
  , bitToBool <$> unPort rst125
  , unPort txp
  , unPort txn
  , unPort gmiiRxD
  , unPort gmiiRxDv
  , unPort gmiiRxEr
  , unPort statusVector
  )
 where
  ( clk125
    , rst125
    , txp
    , txn
    , gmiiRxD
    , gmiiRxDv
    , gmiiRxEr
    , statusVector
    ) = go

  diffClk = DiffClock clockP clockN

  go ::
    ( ClockPort "clk125_out" gmii125
    , Port "rst_125_out" gmii125 Bit
    , Port "txp" sgmii625 Bit
    , Port "txn" sgmii625 Bit
    , Port "gmii_rxd" gmii125 (BitVector 8)
    , Port "gmii_rx_dv" gmii125 Bit
    , Port "gmii_rx_er" gmii125 Bit
    , Port "status_vector" gmii125 StatusVector
    )
  go =
    instWithXilinxWizard
      (instConfig "gig_ethernet_pcs_pma")
      (XilinxWizard
        { wiz_name = "gig_ethernet_pcs_pma"
        , wiz_vendor = "xilinx.com"
        , wiz_library = "ip"
        , wiz_version = versionToString version
        , wiz_options =
             ("CONFIG.LvdsRefClk",               StrOpt "625")
          :> ("CONFIG.Standard",                 StrOpt "SGMII")
          :> ("CONFIG.Physical_Interface",       StrOpt "LVDS")
          :> ("CONFIG.Management_Interface",     BoolOpt False)
          :> ("CONFIG.Ext_Management_Interface", BoolOpt False)
          :> ("CONFIG.SGMII_PHY_Mode",           BoolOpt False)
          :> ("CONFIG.SupportLevel",             StrOpt "Include_Shared_Logic_in_Core")
          :> Nil
        })
      (NamedDiffClockPort @"refclk625_p" @"refclk625_n" diffClk)
      (ResetPort @"reset" @'ActiveHigh rst)
      (Port @"speed_is_100" (pure 0 :: Signal gmii125 Bit))
      (Port @"speed_is_10_100" (pure 0 :: Signal gmii125 Bit))
      (Port @"signal_detect" (boolToBit <$> signalDetect))
      (Port @"configuration_vector" (pack <$> pmaConfig))
      (Port @"an_adv_config_vector" (pack <$> pmaAnAdvancedConfig))
      (Port @"an_restart_config" (boolToBit <$> restartAn))
      (Port @"rxp" lvdsInP)
      (Port @"rxn" lvdsInN)
      (Port @"gmii_txd" gmiiTxD)
      (Port @"gmii_tx_en" gmiiTxDv)
      (Port @"gmii_tx_er" gmiiTxEr)
