{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's Unique Chip ID IP (@altchip_id@), which reads the 64-bit
identifier that is unique to every device, of MAX 10 and Cyclone V devices
(and others not supported by Quartus Prime Lite). It is the Intel counterpart
of Xilinx's @DNA_PORT@. See the
[Unique Chip ID Intel FPGA IP User Guide](https://www.intel.com/content/www/us/en/docs/programmable/683206/)
and, for MAX 10, the
[MAX 10 FPGA Configuration User Guide](https://www.intel.com/content/www/us/en/docs/programmable/683865/).

Clash generates the IP with Platform Designer, see "Clash.Cores.Intel.Internal".
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Config.ChipId
  ( chipId
  , chipIdLatency
  ) where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (unPort)

-- | Number of cycles after the reset after which 'chipId' has read the ID: the
-- IP shifts the 64 bits of the ID out of the hard block, with a few cycles of
-- overhead.
type ChipIdLatency = 68

-- | Number of cycles after the reset after which 'chipId' has read the ID
chipIdLatency :: Int
chipIdLatency = natToNum @ChipIdLatency

{- | The unique ID of the device ('Just'), once it has been read. The IP reads
the ID after the (asynchronous) reset, which takes 'chipIdLatency' cycles. The
clock can run at up to 100 MHz.

Simulation: the given simulation ID, after 'chipIdLatency' cycles. The
latency was checked against the plain parts of Intel's simulation model of the
IP. The hard block that holds the ID is only simulated by Intel's encrypted
simulation models (they also return the simulation ID, which Clash passes to
the IP as @ID_VALUE@), which only Intel's Questa edition can simulate. So the
HDL generated for this function is only checked with Quartus synthesis.

The ID is meant to be read once, after configuration: like the IP, the
result stays the same until the next reset.

>>> let rst = resetGenN @System d2
>>> sampleN 3 (chipId @System 0x0123456789abcdef systemClockGen rst)
[Nothing,Nothing,Nothing]
>>> import qualified Prelude as P
>>> P.drop 69 (sampleN 71 (chipId @System 0x0123456789abcdef systemClockGen rst))
[Nothing,Just 0b0000_0001_0010_0011_0100_0101_0110_0111_1000_1001_1010_1011_1100_1101_1110_1111]
-}
chipId ::
  forall dom.
  KnownDomain dom =>
  -- | ID to use in simulation
  BitVector 64 ->
  Clock dom ->
  Reset dom ->
  Signal dom (Maybe (BitVector 64))
chipId simId clk rst
  | clashSimulation = model
  | otherwise = mux (bitToBool <$> unPort valid) (Just <$> unPort chip) (pure Nothing)
 where
  model = (\c -> if c == maxBound then Just simId else Nothing) <$> cnt
  cnt :: Signal dom (Index (ChipIdLatency + 1))
  cnt = register clk rst enableGen 0 (satSucc SatBound <$> cnt)

  (chip, valid) = go
  go :: (Port "chip_id" dom (BitVector 64), Port "data_valid" dom Bit)
  go =
    instWithIntelIp
      (instConfig "chip_id")
      IntelIp
        { ipKind = "altchip_id"
        , ipParams = ("ID_VALUE", IntegerParam (toInteger simId)) :> Nil
        }
      (ClockPort @"clkin" clk)
      (ResetPort @"reset" @ActiveHigh rst)
{-# OPAQUE chipId #-}
