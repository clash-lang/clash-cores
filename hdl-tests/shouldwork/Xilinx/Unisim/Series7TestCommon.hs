{-# LANGUAGE QuasiQuotes #-}
module Series7TestCommon where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (HDL (..), Primitive (InlineYamlPrimitive))
import Clash.Signal.Internal (Clock (..))
import Data.String.Interpolate (__i)

{- | The given clock, as a clock of another domain. Use it with a domain that
has the same period but the opposite active edge, to get registers that are
clocked by the other edge of the clock. In Clash simulation, the edges of the
two clocks coincide: Clash doesn't simulate the phase difference. This is
fine as long as signals only go from the falling edge domain to the rising
edge domain: at a rising edge, a register clocked by the falling edge has the
value it got at the falling edge before it, which is the value Clash
simulation gives it at that edge.

In HDL simulation, the clock is delayed by 1 ns, so registers clocked by it
change well after the edge of the original clock, also after clocks that
follow the original clock a few delta cycles later (such as the output of a
UNISIM clock buffer model). Only for test benches.
-}
asOtherEdgeClock ::
  forall domA domB.
  (KnownDomain domA, KnownDomain domB) =>
  Clock domA ->
  Clock domB
asOtherEdgeClock (Clock _ Nothing) = Clock SSymbol Nothing
asOtherEdgeClock (Clock _ (Just _)) = error "asOtherEdgeClock: dynamic clock"
{-# OPAQUE asOtherEdgeClock #-}
{-# ANN asOtherEdgeClock (
  let
    primName = show 'asOtherEdgeClock
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: #{primName}
        kind: Declaration
        template: |-
          -- asOtherEdgeClock begin
          -- pragma translate_off
          ~RESULT <= ~ARG[2] after 1 ns;
          -- pragma translate_on
          -- asOtherEdgeClock end
    |]) #-}
{-# ANN asOtherEdgeClock (
  let
    primName = show 'asOtherEdgeClock
  in
    InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
      BlackBox:
        name: #{primName}
        kind: Declaration
        template: |-
          // asOtherEdgeClock begin
          // A copy of the clock without the falling edges a Verilog simulation
          // sees when the clock generator initializes the clock to 0: the copy
          // stays x until the first rising edge.
          // pragma translate_off
          reg ~GENSYM[clk][0];
          always @(~VAR[clkIn][2])
            if (~VAR[clkIn][2] === 1'b1) ~SYM[0] = 1'b1;
            else if (~SYM[0] === 1'b1) ~SYM[0] = 1'b0;
          // 1 = 0.1 ps
          assign \#10000 ~RESULT = ~SYM[0];
          // pragma translate_on
          // asOtherEdgeClock end
    |]) #-}
