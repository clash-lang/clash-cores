{-# LANGUAGE TemplateHaskell #-}

{- | Makes Intel's simulation models of the MAX 10 atoms (library
@fiftyfivenm@) available to test benches, see "IntelSimLib".
-}
module FiftyfivenmSim where

import Clash.Explicit.Prelude

import IntelSimLib (intelSimLibrary)
import IntelTestCommon (withIntelSim)

$(intelSimLibrary "fiftyfivenmLib" "fiftyfivenm" "fiftyfivenm_atoms.v"
    ["fiftyfivenm_atoms.vhd", "fiftyfivenm_components.vhd"])

-- | Like 'withIntelSim', but also makes the MAX 10 atoms part of the HDL
withFiftyfivenmSim :: Signal dom Bool -> Signal dom Bool
withFiftyfivenmSim = withIntelSim . fmap (bitToBool . fiftyfivenmLib . boolToBit)
