{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
module Series7SerdesTypes where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (HDL (..), Primitive (InlineYamlPrimitive))
import Data.String.Interpolate (__i)

import Clash.Cores.Xilinx.Unisim.Series7.Serdes
import Clash.Xilinx.DDR (oddr)
import UnisimTestCommon

-- | Domain of the serial signals: 500 Mb/s
createDomain vXilinxSystem{vName="Line", vPeriod=2000}
-- | Domain of the high-speed SERDES clock for SDR, and of the clock of the
-- 'oddr' driving the serial input of the deserializer
createDomain vXilinxSystem{vName="Clk1", vPeriod=2000}
-- | Domain of the high-speed SERDES clock for DDR
createDomain vXilinxSystem{vName="Clk2", vPeriod=4000}
-- | Domains of the divided SERDES clock, for words of 2 to 8 bits
createDomain vXilinxSystem{vName="Div2", vPeriod=4000}
createDomain vXilinxSystem{vName="Div3", vPeriod=6000}
createDomain vXilinxSystem{vName="Div4", vPeriod=8000}
createDomain vXilinxSystem{vName="Div5", vPeriod=10000}
createDomain vXilinxSystem{vName="Div6", vPeriod=12000}
createDomain vXilinxSystem{vName="Div7", vPeriod=14000}
createDomain vXilinxSystem{vName="Div8", vPeriod=16000}

{- | A signal, delayed by 100 ps in HDL simulation; the identity in Clash
simulation. Only for test benches.

The UNISIM models of the SERDES primitives are Verilog models. When VHDL
simulation drives their inputs from registers, they see the registers' new
values at the clock edge at which the registers change: a delta cycle race,
which Verilog simulation doesn't have. Real registers have a clock-to-output
delay; this function gives them one.
-}
afterEdge :: forall dom a. Signal dom a -> Signal dom a
afterEdge x = x
{-# OPAQUE afterEdge #-}
{-# ANN afterEdge (
  let
    primName = show 'afterEdge
  in
    InlineYamlPrimitive [VHDL] [__i|
      BlackBox:
        name: #{primName}
        kind: Declaration
        template: |-
          -- afterEdge begin
          ~RESULT <= ~ARG[0]
          -- pragma translate_off
            after 100 ps
          -- pragma translate_on
            ;
          -- afterEdge end
    |]) #-}
{-# ANN afterEdge (
  let
    primName = show 'afterEdge
  in
    InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
      BlackBox:
        name: #{primName}
        kind: Declaration
        template: |-
          // afterEdge begin
          // 1 = 0.1 ps
          assign
          // pragma translate_off
            \#1000
          // pragma translate_on
            ~RESULT = ~ARG[0];
          // afterEdge end
    |]) #-}

-- | High-speed clock domain for a data rate
type family ClkDom (rate :: DataRate) :: Domain where
  ClkDom 'SDR = Clk1
  ClkDom 'DDR = Clk2

-- | Random reset, driven by a register (with 'afterEdge') as the resets are
-- asynchronous. It is asserted for the first 120 ns, and after that in bursts
-- of two or three cycles, as the primitives need resets of at least two
-- cycles.
randomReset :: forall dom. (KnownDomain dom) => Int -> Clock dom -> Reset dom
randomReset seed clk = unsafeFromActiveHigh (afterEdge (delay clk enableGen True ((/= 0) <$> left)))
 where
  initial = initialResetCycles @dom
  left :: Signal dom (Index 64)
  left = delay clk enableGen (fromIntegral initial) (next <$> cycleCount clk <*> left <*> genTestData seed clk)
  -- No random resets in the 8 cycles after the initial one: the deserializer
  -- outputs a word from before a reset after it, which would still be undefined
  next :: Index 256 -> Index 64 -> Unsigned 5 -> Index 64
  next cnt n r
    | n > 0 = n - 1
    | fromIntegral cnt < initial + 8 = 0
    | r == 0 = 2
    | r == 1 = 3
    | otherwise = 0

-- | Number of cycles of the initial reset of 'randomReset'
initialResetCycles :: forall dom. (KnownDomain dom) => Int
initialResetCycles = 120000 `div` snatToNum (clockPeriod @dom) + 1

-- | Zero during the initial reset of 'randomReset' and the four cycles after
-- it. The test benches' clocks only start after the global set/reset of the
-- UNISIM models, which leaves the deserializer outputs undefined until it has
-- been reset.
maskInitial :: forall dom a. (KnownDomain dom, NFDataX a) => a -> Clock dom -> Signal dom a -> Signal dom a
maskInitial zero clk = mux ((< (fromIntegral (initialResetCycles @dom + 4) :: Index 256)) <$> cycleCount clk) (pure zero)

-- | Random bitslips, in one out of four cycles but never in two consecutive
-- cycles. Driven by a register (with 'afterEdge').
randomBitslip :: (KnownDomain dom) => Int -> Clock dom -> Signal dom Bool
randomBitslip seed clk = afterEdge slip
 where
  slip = delay clk enableGen False ((\s r -> not s && r == (0 :: Unsigned 2)) <$> slip <*> genTestData seed clk)

-- | Random serial data, deserialized with random resets and bitslips
dutIserdes ::
  forall rate width div.
  ( KnownDataRate rate, KnownNat width, SerdesWidth rate width, KnownDomain div
  , KnownDomain (ClkDom rate)
  , DomainPeriod (ClkDom rate) ~ (BitsPerClock rate * DomainPeriod Line)
  , DomainPeriod div ~ (width * DomainPeriod Line) ) =>
  Iserdese2Config ->
  Clock Line -> Clock Clk2 -> Clock (ClkDom rate) -> Clock div ->
  Signal div (BitVector width)
dutIserdes config _clkLine clkOddr clk clkDiv =
  maskInitial 0 clkDiv $ pack <$>
    iserdese2 @rate @width config clk clkDiv
      (randomReset (randomSeed + 1) clkDiv)
      (randomBitslip (randomSeed + 2) clkDiv)
      line
 where
  -- The serial data comes from an 'oddr' clocked at half the bit rate. When it
  -- comes from a register on a line rate clock instead, VHDL simulation of the
  -- deserializer (a Verilog model) samples the data after the register has been
  -- updated: a delta cycle race.
  line :: Signal Line Bit
  line = oddr clkOddr noReset enableGen (genTestData randomSeed clkOddr)

-- | Random words, serialized with random resets and sampled by a register on
-- the line clock
dutOserdes ::
  forall rate width div.
  ( KnownDataRate rate, KnownNat width, SerdesWidth rate width, KnownDomain div
  , KnownDomain (ClkDom rate), width <= 64
  , DomainPeriod (ClkDom rate) ~ (BitsPerClock rate * DomainPeriod Line)
  , DomainPeriod div ~ (width * DomainPeriod Line) ) =>
  Oserdese2Config ->
  Clock Line -> Clock (ClkDom rate) -> Clock div ->
  Signal Line Bit
dutOserdes config clkLine clk clkDiv =
  maskInitial 0 clkLine $ delay clkLine enableGen 0 $
    oserdese2 @rate @width config clk clkDiv (randomReset (randomSeed + 1) clkDiv) $
      afterEdge (unpack <$> genTestData randomSeed clkDiv)

-- | Random words, serialized and deserialized again, with random resets and
-- bitslips
dutLoopback ::
  forall rate width div.
  ( KnownDataRate rate, KnownNat width, SerdesWidth rate width, KnownDomain div
  , KnownDomain (ClkDom rate), width <= 64
  , DomainPeriod (ClkDom rate) ~ (BitsPerClock rate * DomainPeriod Line)
  , DomainPeriod div ~ (width * DomainPeriod Line) ) =>
  Clock Line -> Clock (ClkDom rate) -> Clock div ->
  Signal div (BitVector width)
dutLoopback _clkLine clk clkDiv =
  maskInitial 0 clkDiv $ pack <$>
    iserdese2 @rate @width iserdese2DefaultConfig clk clkDiv
      (randomReset (randomSeed + 3) clkDiv)
      (randomBitslip (randomSeed + 4) clkDiv)
      line
 where
  line :: Signal Line Bit
  line =
    oserdese2 @rate @width oserdese2DefaultConfig clk clkDiv (randomReset (randomSeed + 1) clkDiv) $
      afterEdge (unpack <$> genTestData randomSeed clkDiv)
