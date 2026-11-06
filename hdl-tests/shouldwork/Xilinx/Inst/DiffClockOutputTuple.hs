{- | Instantiates a component with differential clock outputs (one with the
default port name suffixes, one with custom port names) next to other output
ports, see https://github.com/clash-lang/clash-cores/issues/33.

The component doesn't exist, so we only check the generated HDL. See
@DiffClockOutput@ for a test simulating differential clock outputs.
-}
module DiffClockOutputTuple where

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.FilePath ((</>))

import qualified Data.List as L

topEntity ::
  Clock System ->
  (DiffClock System, Signal System Bool, DiffClock System)
topEntity clk = (unPort clkOut, unPort locked, unPort clk2)
 where
  (clkOut, locked, _, clk2) = go

  go ::
    ( DiffClockPort "clk_out" System
    , Port "locked" System Bool
    , Port "zero_width" System ()
    , NamedDiffClockPort "clk2_pos" "clk2_neg" System
    )
  go = inst (instConfig "clk_gen") (ClockPort @"clk_in" clk)

mainHDL :: String -> [String] -> [String] -> IO ()
mainHDL topFile expects doNotExpects = do
  [topDir] <- getArgs
  content <- normalize <$> readFile (topDir </> "DiffClockOutputTuple.topEntity" </> topFile)

  forM_ expects $ \expect -> do
    if normalize expect `L.isInfixOf` content
    then putStrLn $ "Found expected: " <> expect
    else error $ "Expected not found: " <> expect

  forM_ doNotExpects $ \doNotExpect -> do
    if normalize doNotExpect `L.isInfixOf` content
    then error $ "Found unexpected: " <> doNotExpect
    else putStrLn $ "Did not find unexpected: " <> doNotExpect
 where
  -- Remove all whitespace, so we don't depend on alignment
  normalize = L.concat . L.words

mainSystemVerilog, mainVerilog, mainVHDL :: IO ()
mainSystemVerilog = mainVerilog' "topEntity.sv"
mainVerilog = mainVerilog' "topEntity.v"
mainVHDL =
  mainHDL "topEntity.vhdl"
    [ "clk_gen_inst : clk_gen"
    , "clk_out_p => clk_out_p"
    , "clk_out_n => clk_out_n"
    , "locked => locked"
    , "clk2_pos => clk2_pos"
    , "clk2_neg => clk2_neg"
    , "diff_clock <= ( DiffClock_sel0_clk_System => clk_out_p , DiffClock_sel1_clk_n_System => clk_out_n );"
    , "diff_clock_0 <= ( DiffClock_sel0_clk_System => clk2_pos , DiffClock_sel1_clk_n_System => clk2_neg );"
    ]
    ["zero_width"]

mainVerilog' :: String -> IO ()
mainVerilog' topFile =
  mainHDL topFile
    [ "clk_gen clk_gen_inst"
    , ".clk_out_p (clk_out_p)"
    , ".clk_out_n (clk_out_n)"
    , ".locked (locked"
    , ".clk2_pos (clk2_pos)"
    , ".clk2_neg (clk2_neg)"
    , "assign diff_clock = {clk_out_p, clk_out_n};"
    , "assign diff_clock_0 = {clk2_pos, clk2_neg};"
    ]
    ["zero_width"]
