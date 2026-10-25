{-|
Copyright  :  (C) 2021-2026, QBayLogic B.V.,
                  2022     , Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Test benches for the floating point tests that play their inputs and expected
outputs from a ROM. Use these rather than a vector of expected outputs padded
with '++', as in @repeat \@d resDef ++ expected@: Vivado 2022.1's VHDL
elaborator segfaults on the VHDL that Clash generates for that.
-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Floating.RomTB where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Clash.Explicit.Testbench

-- | Play the samples in a ROM, one per clock cycle. The first output is 'True'
-- once all samples have been played.
playSampleRom
  :: forall n a dom
   . ( KnownDomain dom
     , KnownNat n
     , BitPack a
     , 1 <= n
     )
  => Clock dom
  -> Reset dom
  -> MemBlob n (BitSize a)
  -> (Signal dom Bool, Signal dom a)
playSampleRom clk rst content = (done, out)
 where
  out = unpack . asyncRomBlob content <$> cnt
  done = CEP.register clk rst enableGen False $ (== maxBound) <$> cnt
  cnt :: Signal dom (Index n)
  cnt = CEP.register clk rst enableGen 0 $ satSucc SatBound <$> cnt

-- | Test a function with two inputs, using a ROM with both inputs and the
-- expected output. The expected outputs must already be delayed by the latency
-- of the function, the first @d@ of them should be the given default.
basicRomTB
  :: forall d n x y z
   . ( KnownNat n
     , KnownNat d
     , BitPack x
     , BitPack y
     , Eq z, ShowX z, BitPack z
     , 1 <= n
     )
  => (   Clock XilinxSystem
      -> DSignal XilinxSystem 0 x
      -> DSignal XilinxSystem 0 y
      -> DSignal XilinxSystem d z
     )
  -> z
  -> MemBlob n (BitSize (x, y, z))
  -> Signal XilinxSystem Bool
basicRomTB comp resDef sampleBlob = done
 where
  (done0, samples) = playSampleRom clk rst sampleBlob
  (inputX, inputY, expectedOutput) = unbundle samples
  -- Only assert while not finished
  done = mux done0 done0
    $ assert clk rst "basicRomTB" out expectedOutput
             done0
  out =
      ignoreFor clk rst en (SNat @d) resDef
    . toSignal $ comp clk (fromSignal inputX) (fromSignal inputY)
  clk = tbClockGen (not <$> done)
  rst = resetGen
  en = enableGen
{-# INLINE basicRomTB #-}

-- | Like 'basicRomTB', for a function with one input.
unaryRomTB
  :: forall d n x z
   . ( KnownNat n
     , KnownNat d
     , BitPack x
     , Eq z, ShowX z, BitPack z
     , 1 <= n
     )
  => (   Clock XilinxSystem
      -> DSignal XilinxSystem 0 x
      -> DSignal XilinxSystem d z
     )
  -> z
  -> MemBlob n (BitSize (x, z))
  -> Signal XilinxSystem Bool
unaryRomTB comp resDef sampleBlob = done
 where
  (done0, samples) = playSampleRom clk rst sampleBlob
  (input, expectedOutput) = unbundle samples
  -- Only assert while not finished
  done = mux done0 done0
    $ assert clk rst "unaryRomTB" out expectedOutput
             done0
  out =
      ignoreFor clk rst en (SNat @d) resDef
    . toSignal $ comp clk (fromSignal input)
  clk = tbClockGen (not <$> done)
  rst = resetGen
  en = enableGen
{-# INLINE unaryRomTB #-}
