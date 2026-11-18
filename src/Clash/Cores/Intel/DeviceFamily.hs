{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

The device families of Quartus Prime Lite. Some of Intel's IP cores compute
differently depending on the device family they are generated for, so the
functions of "Clash.Cores.Intel.Floating" and "Clash.Cores.Intel.Cordic" whose
results depend on it take a 'DeviceFamily'.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}

module Clash.Cores.Intel.DeviceFamily
  ( DeviceFamily(..)
  ) where

import Prelude

import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

import Clash.XException (NFDataX)

-- | Device family an IP core is generated for. The results of some IP cores
-- depend on it: the Floating Point Functions IP truncates some of its
-- products on families with 18x18 multipliers, and the CORDIC IP sizes its
-- tables by the number of inputs of the family's LUTs. The family has to
-- match the one of the Quartus project (and @CLASH_INTEL_SIM_FAMILY@ when
-- simulating the generated HDL), otherwise the Clash simulation and the IP
-- can differ.
--
-- Only the families supported by Quartus Prime Lite 21.1 that have embedded
-- multipliers are listed. The IP cores on other families (Arria, Stratix,
-- Agilex) have not been tested.
data DeviceFamily
  = CycloneIVE
  -- ^ Cyclone IV E (18x18 multipliers, logic elements with four-input LUTs)
  | CycloneV
  -- ^ Cyclone V (27x27 multipliers, adaptive logic modules with six-input
  -- LUTs)
  | Cyclone10LP
  -- ^ Cyclone 10 LP (18x18 multipliers, logic elements with four-input LUTs)
  | Max10
  -- ^ MAX 10 (18x18 multipliers, logic elements with four-input LUTs)
  deriving (Eq, Show, Enum, Bounded, Generic, NFDataX, Lift)
