{- | Instantiates UNISIM's @IBUF@, whose @IBUF_LOW_PWR@ parameter is a
@boolean@ in VHDL and a @\"TRUE\"@/@\"FALSE\"@ string in Verilog. A 'Param'
carrying a 'Bool' renders as a bit in Verilog, and one carrying a 'String'
doesn't type check in VHDL, see
https://github.com/clash-lang/clash-cores/issues/82.

Note that the Verilog simulation model of @IBUF@ ends the simulation (with
@$finish@) on an illegal value, before the test bench gets to check anything.
The test harness doesn't notice that, so this module also checks the rendered
parameters in the generated HDL.
-}
module BoolStringParam where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.FilePath ((</>))

import qualified Data.List as L

type Dom = XilinxSystem

-- | Instantiates UNISIM's @IBUF@, with @IBUF_LOW_PWR@ set to the given value
ibuf :: forall dom . KnownDomain dom => Bool -> Signal dom Bit -> Signal dom Bit
ibuf lowPower i = unPort go
 where
  go :: Port "O" dom Bit
  go =
    inst
      (instConfig "IBUF")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }
      (BoolStringParam @"IBUF_LOW_PWR" lowPower)
      (Port @"I" i)
{-# INLINE ibuf #-}

topEntity :: Signal Dom (BitVector 2) -> Signal Dom (BitVector 2)
topEntity i =
  pack <$> bundle (ibuf True ((! (1 :: Int)) <$> i), ibuf False ((! (0 :: Int)) <$> i))
{-# OPAQUE topEntity #-}

testBench :: Signal Dom Bool
testBench = done
 where
  inputs = 0b00 :> 0b01 :> 0b10 :> 0b11 :> Nil
  done = outputVerifier' clk rst inputs (topEntity (stimuliGenerator clk rst inputs))
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# OPAQUE testBench #-}

mainHDL :: String -> [String] -> IO ()
mainHDL topFile expects = do
  [topDir] <- getArgs
  content <- normalize <$> readFile (topDir </> "BoolStringParam.topEntity" </> topFile)

  forM_ expects $ \expect -> do
    if normalize expect `L.isInfixOf` content
    then putStrLn $ "Found expected: " <> expect
    else error $ "Expected not found: " <> expect
 where
  -- Remove all whitespace, so we don't depend on alignment
  normalize = L.concat . L.words

mainSystemVerilog, mainVerilog, mainVHDL :: IO ()
mainSystemVerilog = mainHDL "topEntity.sv" [".IBUF_LOW_PWR (\"TRUE\")", ".IBUF_LOW_PWR (\"FALSE\")"]
mainVerilog = mainHDL "topEntity.v" [".IBUF_LOW_PWR (\"TRUE\")", ".IBUF_LOW_PWR (\"FALSE\")"]
mainVHDL = mainHDL "topEntity.vhdl" ["IBUF_LOW_PWR => true", "IBUF_LOW_PWR => false"]
