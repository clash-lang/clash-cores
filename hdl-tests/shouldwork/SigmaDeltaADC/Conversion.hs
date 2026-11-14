{- |
  Copyright   :  (C) 2026, Rowan Goemans
  License     :  BSD2 (see the file LICENSE)

  Check full-scale saturation, startup and ready timing with and without
  averaging in generated HDL.
-}
module Conversion where

import Clash.Cores.SigmaDeltaADC
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Prelude (withClockResetEnable)

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 4) ->
  ( Signal System (BitVector 4)
  , Signal System (BitVector 3)
  , Signal System Bool
  , Signal System (BitVector 3)
  , Signal System Bool
  )
topEntity clk rst en input = (feedback, averaged, ready, raw, rawReady)
 where
  (feedback, averaged, ready) =
    withClockResetEnable clk rst en $
      sigmaDeltaADC @3 @1 @2 @3 (SNat @4) (SNat @2) input
  (_, raw, rawReady) =
    withClockResetEnable clk rst en $
      sigmaDeltaADC @3 @0 @0 @3 (SNat @3) (SNat @0) input
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  clk = tbClockGen (not <$> done)
  rst = resetGen
  en = enableGen
  cycles :: Signal System (Unsigned 8)
  cycles = register clk rst en 0 (cycles + 1)
  (feedback, averaged, ready, raw, rawReady) = topEntity clk rst en (pure 15)
  expectedFeedback = mux (cycles .==. 0) 0 15
  expectedAverage = mux (cycles .>=. 17) 7 0
  expectedRaw = mux (cycles .>=. 3) 7 0
  expectedReady = (\n -> n >= 17 && (n - 1) `mod` 16 == 0) <$> cycles
  expectedRawReady = (\n -> n >= 3 && testBit n 0) <$> cycles
  done =
    assert clk rst "Feedback delay" feedback expectedFeedback $
      assert clk rst "Full-scale average" averaged expectedAverage $
        assert clk rst "Averaged result valid" ready expectedReady $
          assert clk rst "Unfiltered full scale" raw expectedRaw $
            assert clk rst "Unfiltered result valid" rawReady expectedRawReady $
              cycles .==. 80
{-# OPAQUE testBench #-}
