{- | Instantiates components with 'ResetPort' outputs, with polarities both
matching and not matching the polarity of the domain. In the past, 'inst'
connected the output port of the instance to the inverted signal on a polarity
mismatch, instead of inverting the output of the instance.
-}
module ResetOutputs where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

createDomain vXilinxSystem{vName="DomLow", vResetPolarity=ActiveLow}

-- | Use UNISIM's @VCC@ (output @P@) as a reset source
vcc :: forall polarity . ResetPort "P" polarity DomLow
vcc =
  inst
    (instConfig "VCC")
      { library = Just "UNISIM"
      , libraryImport = Just "UNISIM.vcomponents.all"
      }

-- | Use UNISIM's @GND@ (output @G@) as a reset source
gnd :: forall polarity . ResetPort "G" polarity DomLow
gnd =
  inst
    (instConfig "GND")
      { library = Just "UNISIM"
      , libraryImport = Just "UNISIM.vcomponents.all"
      }

-- | Whether each of the resets is asserted
topEntity :: Signal DomLow (BitVector 4)
topEntity =
  fmap pack $ bundle
    ( unsafeToActiveHigh (unPort (vcc @'ActiveHigh)) -- asserted
    , unsafeToActiveHigh (unPort (gnd @'ActiveLow))  -- asserted
    , unsafeToActiveHigh (unPort (gnd @'ActiveHigh)) -- deasserted
    , unsafeToActiveHigh (unPort (vcc @'ActiveLow))  -- deasserted
    )
{-# OPAQUE topEntity #-}

testBench :: Signal DomLow Bool
testBench = done
 where
  done = outputVerifier' clk rst (replicate d4 0b1100) topEntity
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# OPAQUE testBench #-}
