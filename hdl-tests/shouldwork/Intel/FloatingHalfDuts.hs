{- | Functions under test for "FloatingHalf". They are defined here, as the
Template Haskell in "FloatingHalf" simulates them to create reference traces.
-}
{-# LANGUAGE TemplateHaskell #-}

module FloatingHalfDuts where

import Clash.Explicit.Prelude

import qualified Clash.Cores.Intel.Floating.Half.Explicit as E

import Numeric.Half (Half(..))

import FloatingDuts (simFamily)
import IpTestCommon

-- | E.sqrt, E.sqrtWith @2
sqrtDut :: Dut Half (Vec 2 Half)
sqrtDut clk rst en i = bundle (unary (E.sqrt simFamily) clk rst en i :> unary (E.sqrtWith @2 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE sqrtDut #-}

-- | E.recip, E.recipWith @6
recipDut :: Dut Half (Vec 2 Half)
recipDut clk rst en i = bundle (unary E.recip clk rst en i :> unary (E.recipWith @6 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE recipDut #-}

-- | E.rsqrt, E.rsqrtWith @2
rsqrtDut :: Dut Half (Vec 2 Half)
rsqrtDut clk rst en i = bundle (unary (E.rsqrt simFamily) clk rst en i :> unary (E.rsqrtWith @2 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE rsqrtDut #-}

-- | E.exp, E.expWith @4
expDut :: Dut Half (Vec 2 Half)
expDut clk rst en i = bundle (unary (E.exp simFamily) clk rst en i :> unary (E.expWith @4 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE expDut #-}

-- | E.exp2, E.exp2With @3
exp2Dut :: Dut Half (Vec 2 Half)
exp2Dut clk rst en i = bundle (unary (E.exp2 simFamily) clk rst en i :> unary (E.exp2With @3 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE exp2Dut #-}

-- | E.exp10, E.exp10With @4
exp10Dut :: Dut Half (Vec 2 Half)
exp10Dut clk rst en i = bundle (unary (E.exp10 simFamily) clk rst en i :> unary (E.exp10With @4 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE exp10Dut #-}

-- | E.log, E.logWith @9
logDut :: Dut Half (Vec 2 Half)
logDut clk rst en i = bundle (unary (E.log simFamily) clk rst en i :> unary (E.logWith @9 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE logDut #-}

-- | E.log2, E.log2With @8
log2Dut :: Dut Half (Vec 2 Half)
log2Dut clk rst en i = bundle (unary (E.log2 simFamily) clk rst en i :> unary (E.log2With @8 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE log2Dut #-}

-- | E.log10, E.log10With @9
log10Dut :: Dut Half (Vec 2 Half)
log10Dut clk rst en i = bundle (unary (E.log10 simFamily) clk rst en i :> unary (E.log10With @9 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE log10Dut #-}

-- | E.log1p, E.log1pWith @10
log1pDut :: Dut Half (Vec 2 Half)
log1pDut clk rst en i = bundle (unary (E.log1p simFamily) clk rst en i :> unary (E.log1pWith @10 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE log1pDut #-}

-- | Every @k@th half precision number (by bit pattern, starting at @start@),
-- followed by all numbers with the given sign and with biased exponents in
-- @[lo, lo + n)@
halves :: Int -> Int -> Bool -> Int -> Int -> [Half]
halves k start neg lo n = everyNth k start <> binades neg lo n

-- | Every @k@th half precision number, by bit pattern, starting at @start@
everyNth :: Int -> Int -> [Half]
everyNth k start = [Half (fromIntegral i) | i <- [start, start + k .. 65535]]

-- | All numbers with the given sign and with biased exponents in
-- @[lo, lo + n)@
binades :: Bool -> Int -> Int -> [Half]
binades neg lo n = [Half (fromIntegral (s + e * 1024 + f)) | e <- [lo .. lo + n - 1], f <- [0 .. 1023]]
 where
  s = if neg then 32768 else 0 :: Int
