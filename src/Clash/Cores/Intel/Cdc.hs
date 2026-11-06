{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Clock domain crossing primitives of Intel FPGAs: the synchronizer
@altera_std_synchronizer@ and its multi-bit version
@altera_std_synchronizer_bundle@. Quartus recognizes the registers of these
synchronizers as synchronizers: it reports them in its metastability analysis,
keeps them apart from other logic, and cuts the timing paths into them.

For more information see the section /Metastability Analysis/ of the
/Quartus Prime Standard Edition User Guide: Timing Analyzer/
(<https://www.intel.com/content/www/us/en/docs/programmable/683068/current/metastability-analysis.html>).

For FIFOs that cross clock domains, see "Clash.Cores.Intel.Fifo".
-}

module Clash.Cores.Intel.Cdc
  ( stdSynchronizer
  , stdSynchronizerBundle
  ) where

import Clash.Explicit.Prelude

import GHC.Stack (HasCallStack)

import Clash.Cores.Intel.Primitive.Internal (alteraMf)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( ClockPort (..), Param (..), Port (..), ResetPort (..), inst, unPort )

{- | Synchronizer (@altera_std_synchronizer@): a chain of @depth@ registers in
the destination domain, with an asynchronous reset to 0. The first register
samples the input, which may come from any domain, so its output may go
metastable; the other registers give it time to settle. Use it to cross single
bits that change slowly compared to the destination clock, such as status
flags. A synchronizer delays its input by @depth@ cycles of the destination
clock.

Simulation: @depth@ registers. The input from the source domain is sampled at
the active edges of the destination clock, see 'unsafeSynchronizer'.

__NB__: Intel's VHDL simulation model resets the registers synchronously,
while its Verilog model and the hardware reset them asynchronously.
-}
stdSynchronizer ::
  forall depth src dst.
  ( HasCallStack
  , KnownNat depth
  , 2 <= depth
  , KnownDomain src
  , KnownDomain dst
  , HasAsynchronousReset dst
  ) =>
  -- | Number of registers (@depth@), at least 2. Intel's default is 3.
  SNat depth ->
  -- | Destination clock (@clk@)
  Clock dst ->
  -- | Asynchronous reset (@reset_n@)
  Reset dst ->
  -- | Input (@din@)
  Signal src Bit ->
  -- | Output (@dout@)
  Signal dst Bit
stdSynchronizer depth clk rst din
  | clashSimulation = model depth clk rst din
  | otherwise = unPort go
 where
  go :: Port "dout" dst Bit
  go =
    inst
      (alteraMf "altera_std_synchronizer")
      (Param @"depth" @Integer (snatToInteger depth))
      (ClockPort @"clk" clk)
      (ResetPort @"reset_n" @'ActiveLow rst)
      (Port @"din" din)

{- | Synchronizer for a bundle of bits (@altera_std_synchronizer_bundle@): a
'stdSynchronizer' for every bit of the input.

__NB__: The bits are synchronized independently: when several bits change at
once, the output can show a combination of old and new bits for a cycle. Only
use it for values of which at most one bit changes at a time, such as Gray
codes, or that are stable while the destination reads them.
-}
stdSynchronizerBundle ::
  forall depth a src dst.
  ( HasCallStack
  , KnownNat depth
  , 2 <= depth
  , KnownDomain src
  , KnownDomain dst
  , HasAsynchronousReset dst
  , BitPack a
  , NFDataX a
  ) =>
  -- | Number of registers (@depth@), at least 2. Intel's default is 3.
  SNat depth ->
  -- | Destination clock (@clk@)
  Clock dst ->
  -- | Asynchronous reset (@reset_n@). Resets the output to all zeroes.
  Reset dst ->
  -- | Input (@din@)
  Signal src a ->
  -- | Output (@dout@)
  Signal dst a
stdSynchronizerBundle depth clk rst din
  | clashSimulation = unpack <$> model depth clk rst (pack <$> din)
  | otherwise = unpack <$> unPort go
 where
  go :: Port "dout" dst (BitVector (BitSize a))
  go =
    inst
      (alteraMf "altera_std_synchronizer_bundle")
      (Param @"depth" @Integer (snatToInteger depth))
      (Param @"width" @Integer (natToNum @(BitSize a)))
      (ClockPort @"clk" clk)
      (ResetPort @"reset_n" @'ActiveLow rst)
      (Port @"din" (pack <$> din))

-- | Simulation model of the synchronizers
model ::
  forall depth a src dst.
  (KnownNat depth, NFDataX a, Num a, KnownDomain src, KnownDomain dst) =>
  SNat depth ->
  Clock dst ->
  Reset dst ->
  Signal src a ->
  Signal dst a
model depth clk rst din =
  foldr
    (\_ s -> register clk rst enableGen 0 s)
    (unsafeSynchronizer clockGen clk din)
    (replicate depth ())
