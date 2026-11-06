-- | Location of Yosys' simulation models, see "Ice40TestCommon"
module Ice40CellsSim (cellsSimPathQ) where

import Prelude

import Language.Haskell.TH (Q, runIO)
import System.Environment (lookupEnv)
import System.Process (readProcess)

{- | Absolute path to Yosys' @ice40/cells_sim.v@. Taken from the environment
variable @ICE40_CELLS_SIM@ if set, or found with @yosys-config --datdir@
otherwise.
-}
cellsSimPathQ :: Q String
cellsSimPathQ = runIO $ do
  envPath <- lookupEnv "ICE40_CELLS_SIM"
  case envPath of
    Just p -> pure p
    Nothing -> do
      datDir <- readProcess "yosys-config" ["--datdir"] ""
      pure (filter (/= '\n') datDir ++ "/ice40/cells_sim.v")
