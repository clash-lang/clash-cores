{-# LANGUAGE AllowAmbiguousTypes #-}
module SerdesTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.Ddr (oddre1, oddre1DefaultConfig)
import Clash.Cores.Xilinx.Unisim.Serdes
import UnisimTestCommon

-- | Domain of the serial signals: 500 Mb/s
createDomain vXilinxSystem{vName="Line", vPeriod=2000}
-- | Domain of the high-speed SERDES clock
createDomain vXilinxSystem{vName="Clk", vPeriod=4000}
-- | Domain of the divided SERDES clock, for words of 4 bits
createDomain vXilinxSystem{vName="Div4", vPeriod=8000}
-- | Domain of the divided SERDES clock, for words of 8 bits
createDomain vXilinxSystem{vName="Div8", vPeriod=16000}

-- | Random reset, asserted during the first four cycles, and after that in one
-- out of 16 cycles. Driven by a register, as the resets are asynchronous.
randomReset :: (KnownDomain dom) => Int -> Clock dom -> Reset dom
randomReset seed clk =
  unsafeFromActiveHigh $ delay clk enableGen True $
    (\n r -> n < (3 :: Index 8) || r == (0 :: Unsigned 4))
      <$> cycleCount clk
      <*> genTestData seed clk

-- | Random serial data, deserialized. The serial data comes from an 'oddre1'
-- on the high-speed clock. When it comes from a register on a line rate clock
-- instead, VHDL simulation of the deserializer (a Verilog model) samples the
-- data after the register has been updated: a delta cycle race.
dutIserdes ::
  forall width div.
  ( KnownNat width, SerdesWidth width, KnownDomain div
  , DomainPeriod div ~ (width * DomainPeriod Line) ) =>
  SimDevice ->
  Clock Line -> Clock Clk -> Clock div ->
  Signal div (BitVector width)
dutIserdes simDevice _clkLine clk clkDiv =
  pack <$> iserdese3 @width simDevice clk clkDiv (randomReset (randomSeed + 1) clkDiv) line
 where
  line :: Signal Line Bit
  line =
    oddre1 oddre1DefaultConfig clk noReset $
      delay clk enableGen (0, 0) (genTestData randomSeed clk)

-- | Random words, serialized and sampled by a register on the line clock
dutOserdes ::
  forall width div.
  ( KnownNat width, SerdesWidth width, KnownDomain div, width <= 64
  , DomainPeriod div ~ (width * DomainPeriod Line) ) =>
  Oserdese3Config ->
  Clock Line -> Clock Clk -> Clock div ->
  Signal Line Bit
dutOserdes config clkLine clk clkDiv =
  delay clkLine enableGen 0 $
    oserdese3 @width config clk clkDiv (randomReset (randomSeed + 1) clkDiv) $
      unpack <$> genTestData randomSeed clkDiv

-- | Random words, serialized and deserialized again
dutLoopback ::
  forall width div.
  ( KnownNat width, SerdesWidth width, KnownDomain div, width <= 64
  , DomainPeriod div ~ (width * DomainPeriod Line) ) =>
  Clock Line -> Clock Clk -> Clock div ->
  Signal div (BitVector width)
dutLoopback _clkLine clk clkDiv =
  pack <$> iserdese3 @width UltraScalePlus clk clkDiv (randomReset (randomSeed + 2) clkDiv) line
 where
  line :: Signal Line Bit
  line =
    oserdese3 @width oserdese3DefaultConfig clk clkDiv (randomReset (randomSeed + 1) clkDiv) $
      unpack <$> genTestData randomSeed clkDiv
