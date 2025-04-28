module BiSignal where

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import qualified Data.List as L

topEntity ::
  Clock System ->
  BiSignalIn  'Floating System 32 ->
  BiSignalOut 'Floating System 32
topEntity clk input = unPort go
 where
  go :: BiSignalOutPort "dst_inout" 'Floating System 32
  go =
    inst
      (instConfig "bb"){renderVoid = True}
      (ClockPort @"clk" clk)
      (BiSignalInPort @"src_inout" input)


mainHDL :: String -> [String] -> [String] -> IO ()
mainHDL topFile expects doNotExpects = do
  [topDir] <- getArgs
  content <- readFile (topDir </> "BiSignal.topEntity" </> topFile)

  forM_ expects $ \expect -> do
    if expect `L.isInfixOf` content
    then putStrLn $ "Found expected: " <> expect
    else error $ "Expected not found: " <> expect

  forM_ doNotExpects $ \doNotExpect -> do
    if doNotExpect `L.isInfixOf` content
    then error $ "Found unexpected: " <> doNotExpect
    else putStrLn $ "Did not find unexpected: " <> doNotExpect

mainSystemVerilog, mainVerilog, mainVHDL :: IO ()
--                                           assert in file                assert not in file
mainSystemVerilog = mainHDL "topEntity.sv"   ["bb bb_inst", "src_inout"]   ["dst_inout"]
mainVerilog       = mainHDL "topEntity.v"    ["bb bb_inst", "src_inout"]   ["dst_inout"]
mainVHDL          = mainHDL "topEntity.vhdl" ["bb_inst : bb", "src_inout"] ["dst_inout"]
