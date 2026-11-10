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

-- | Every @k@th half precision number (by bit pattern, starting at @start@),
-- followed by all numbers with the given sign and with biased exponents in
-- @[lo, lo + n)@
halves :: Int -> Int -> Bool -> Int -> Int -> [Half]
halves k start neg lo n =
  [Half (fromIntegral i) | i <- [start, start + k .. 65535]]
    <> [Half (fromIntegral (s + e * 1024 + f)) | e <- [lo .. lo + n - 1], f <- [0 .. 1023]]
 where
  s = if neg then 32768 else 0 :: Int
