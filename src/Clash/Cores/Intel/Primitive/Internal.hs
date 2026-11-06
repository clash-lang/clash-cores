{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities shared by the wrappers of Intel's primitives (@altera_primitives@)
and megafunctions (@altera_mf@).
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Intel.Primitive.Internal where

import Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.Internal (InstConfig (..), instConfig)

{- | Instantiation config for the primitive with the given name, such as
@alt_iobuf@. In VHDL, Quartus declares the primitives in the package
@altera_primitives_components@ of library @altera@.
-}
alteraPrimitive :: String -> InstConfig
alteraPrimitive name =
  (instConfig name)
    { library = Just "altera"
    , libraryImport = Just "altera.altera_primitives_components.all"
    }

{- | Instantiation config for the megafunction with the given name, such as
@altsyncram@. In VHDL, Quartus declares the megafunctions in the package
@altera_mf_components@ of library @altera_mf@.
-}
alteraMf :: String -> InstConfig
alteraMf name =
  (instConfig name)
    { library = Just "altera_mf"
    , libraryImport = Just "altera_mf.altera_mf_components.all"
    }

-- | Value of a string parameter of a primitive, where @\"NONE\"@ means the
-- parameter isn't set
noneString :: Maybe String -> String
noneString = maybe "NONE" id

-- | Value of an @\"ON\"@ \/ @\"OFF\"@ parameter of a primitive, where
-- @\"NONE\"@ means the parameter isn't set
onOffString :: Maybe Bool -> String
onOffString Nothing = "NONE"
onOffString (Just True) = "ON"
onOffString (Just False) = "OFF"
