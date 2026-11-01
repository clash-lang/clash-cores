{- | Instantiates components without any input ports. In the past 'inst' would
render these as an undefined value instead of an instance, see
https://github.com/clash-lang/clash-cores/issues/80.
-}
module NoInput where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

type Dom = XilinxSystem

-- | Instantiates the UNISIM primitives @VCC@ and @GND@, neither of which has
-- any input ports or parameters.
topEntity :: Signal Dom (BitVector 2)
topEntity = liftA2 (++#) (pack <$> unPort vcc) (pack <$> unPort gnd)
 where
  vcc :: Port "P" Dom Bit
  vcc =
    inst
      (instConfig "VCC")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }

  gnd :: Port "G" Dom Bit
  gnd =
    inst
      (instConfig "GND")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }
{-# OPAQUE topEntity #-}

testBench :: Signal Dom Bool
testBench = done
 where
  done = outputVerifier' clk rst (replicate d4 0b10) topEntity
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# OPAQUE testBench #-}
