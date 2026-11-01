module DdrTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Ddr
import UnisimTestCommon

-- | Domain of the clock of the DDR primitives
type Slow = XilinxSystem

-- | Domain of the DDR signals
createDomain vXilinxSystem{vName="Fast", vPeriod=hzToPeriod 200e6}

-- | Random reset, asserted during the first four cycles, and after that in one
-- out of 16 cycles. Driven by a register, as the resets of the DDR primitives
-- are asynchronous.
randomReset :: Int -> Clock Slow -> Reset Slow
randomReset seed clk =
  unsafeFromActiveHigh $ delay clk enableGen True $
    (\n r -> n < (3 :: Index 8) || r == (0 :: Unsigned 4))
      <$> cycleCount clk
      <*> genTestData seed clk

-- | Random bits through an 'oddre1', sampled by a register on the DDR clock
dutOddre1 :: Oddre1Config -> Clock Slow -> Clock Fast -> Signal Fast Bit
dutOddre1 config clkSlow clkFast =
  delay clkFast enableGen 0 $
    oddre1 config clkSlow (randomReset (randomSeed + 1) clkSlow) $
      genTestData randomSeed clkSlow

-- | Random bits from a register on the DDR clock through an 'iddre1'
dutIddre1 :: IddrMode -> Clock Slow -> Clock Fast -> Signal Slow (BitVector 2)
dutIddre1 mode clkSlow clkFast =
  pack <$> iddre1 mode clkSlow (randomReset (randomSeed + 1) clkSlow) ddr
 where
  ddr = delay clkFast enableGen 0 (genTestData randomSeed clkFast)

-- | Random bits through an 'oddre1' and back through an 'iddre1'
dutLoopback :: IddrMode -> Clock Slow -> Clock Fast -> Signal Slow (BitVector 2)
dutLoopback mode clkSlow _clkFast =
  pack <$> iddre1 mode clkSlow (randomReset (randomSeed + 2) clkSlow) ddr
 where
  ddr :: Signal Fast Bit
  ddr =
    oddre1 oddre1DefaultConfig clkSlow (randomReset (randomSeed + 1) clkSlow) $
      genTestData randomSeed clkSlow
