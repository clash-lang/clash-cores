{- | Reproducer of https://github.com/clash-lang/clash-cores/issues/35: an
instance with a zero-width 'BiSignalOutPort' next to a normal output port.
-}
module BiSignalTuple where

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import Control.Monad (forM_)
import Data.Bifunctor (bimap)
import System.Environment (getArgs)
import System.FilePath ((</>))

import qualified Data.List as L

topEntity ::
  Clock System ->
  BiSignalIn  'Floating System 32 ->
  ( BiSignalOut 'Floating System 32
  , Signal System Bool
  )
topEntity clk input = bimap unPort unPort go
 where
  go ::
    ( BiSignalOutPort "dst_inout" 'Floating System 32
    , Port "foo" System Bool
    )
  go =
    inst
      (instConfig "bb"){renderVoid = True}
      (ClockPort @"clk" clk)
      (BiSignalInPort @"src_inout" input)

mainHDL :: String -> [String] -> [String] -> IO ()
mainHDL topFile expects doNotExpects = do
  [topDir] <- getArgs
  content <- readFile (topDir </> "BiSignalTuple.topEntity" </> topFile)

  forM_ expects $ \expect -> do
    if expect `L.isInfixOf` content
    then putStrLn $ "Found expected: " <> expect
    else error $ "Expected not found: " <> expect

  forM_ doNotExpects $ \doNotExpect -> do
    if doNotExpect `L.isInfixOf` content
    then error $ "Found unexpected: " <> doNotExpect
    else putStrLn $ "Did not find unexpected: " <> doNotExpect

-- The bidirectional port should be connected to the bidirectional top entity
-- argument directly, the output port @foo@ should be connected, and there
-- should be no trace of the zero-width output port @dst_inout@.
mainSystemVerilog, mainVerilog, mainVHDL :: IO ()
mainSystemVerilog =
  mainHDL "topEntity.sv"
    ["bb bb_inst", ".src_inout (r_input)", ".foo (foo)"]
    ["dst_inout"]
mainVerilog =
  mainHDL "topEntity.v"
    ["bb bb_inst", ".src_inout (r_input)", ".foo (foo)"]
    ["dst_inout"]
mainVHDL =
  mainHDL "topEntity.vhdl"
    ["bb_inst : bb", "src_inout => r_input", "foo       => foo"]
    ["dst_inout"]
