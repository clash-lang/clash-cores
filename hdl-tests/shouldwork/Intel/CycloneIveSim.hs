{-# LANGUAGE TemplateHaskell #-}

{- | Makes Intel's simulation models of the Cyclone IV E atoms (library
@cycloneive@) available to test benches, see "IntelSimLib".
-}
module CycloneIveSim where

import Clash.Explicit.Prelude

import IntelSimLib (intelSimLibrary)
import IntelTestCommon (withIntelSim)

$(intelSimLibrary "cycloneiveLib" "cycloneive" "cycloneive_atoms.v"
    ["cycloneive_atoms.vhd", "cycloneive_components.vhd"])

-- | Like 'withIntelSim', but also makes the Cyclone IV E atoms part of the HDL
withCycloneIveSim :: Signal dom Bool -> Signal dom Bool
withCycloneIveSim = withIntelSim . fmap (bitToBool . cycloneiveLib . boolToBit)
