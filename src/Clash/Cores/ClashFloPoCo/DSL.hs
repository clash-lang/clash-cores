-- | Re-export module for Clash Primitives DSL
--
-- This module re-exports types and functions from Clash.Primitives.DSL
-- for use in FloPoCo blackbox generation.
module Clash.Cores.ClashFloPoCo.DSL
  ( -- * Re-exports from Clash.Primitives.DSL
    module Clash.Primitives.DSL
    -- * Re-exports from Clash.Cores.ClashFloPoCo.InfoEn
  , InfoEntity(..)
  ) where

import Clash.Primitives.DSL
import Clash.Cores.ClashFloPoCo.InfoEn (InfoEntity(..))
