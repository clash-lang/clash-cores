{- | Functions under test for "FloatingHalf". They are defined here, as the
Template Haskell in "FloatingHalf" simulates them to create reference traces.
-}
{-# LANGUAGE TemplateHaskell #-}

module FloatingHalfDuts where

import Clash.Explicit.Prelude

import qualified Clash.Cores.Intel.Floating.Half.Explicit as E

import Data.Word (Word64)
import Numeric.Half (Half(..))
import qualified Prelude as P

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

-- | E.cbrt, E.cbrtWith @6
cbrtDut :: Dut Half (Vec 2 Half)
cbrtDut clk rst en i = bundle (unary (E.cbrt simFamily) clk rst en i :> unary (E.cbrtWith @6 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE cbrtDut #-}

-- | E.sinPi, E.sinPiWith @5
sinPiDut :: Dut Half (Vec 2 Half)
sinPiDut clk rst en i = bundle (unary (E.sinPi simFamily) clk rst en i :> unary (E.sinPiWith @5 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE sinPiDut #-}

-- | E.cosPi, E.cosPiWith @5
cosPiDut :: Dut Half (Vec 2 Half)
cosPiDut clk rst en i = bundle (unary (E.cosPi simFamily) clk rst en i :> unary (E.cosPiWith @5 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE cosPiDut #-}

-- | E.tanPi, E.tanPiWith @11
tanPiDut :: Dut Half (Vec 2 Half)
tanPiDut clk rst en i = bundle (unary (E.tanPi simFamily) clk rst en i :> unary (E.tanPiWith @11 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE tanPiDut #-}

-- | E.hypot, E.hypotWith @6
hypotDut :: Dut (Half, Half, Half) (Vec 2 Half)
hypotDut clk rst en i = bundle (ternary (E.hypot simFamily) clk rst en i :> ternary (E.hypotWith @6 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE hypotDut #-}

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

-- | Triples of half precision numbers: all combinations of a few special
-- values, followed by @n@ random triples of either sign whose (biased)
-- exponents differ by less than 13, so that their squares overlap when
-- 'E.hypot' adds them, in a random order
halfTriples :: Int -> Word64 -> [(Half, Half, Half)]
halfTriples n seed = [(a, b, c) | a <- sp, b <- sp, c <- sp] <> P.take n (go (randoms seed))
 where
  sp = P.map Half [0, 0x8000, 0x0001, 0x0400, 0x3c00, 0xbc00, 0x7bff, 0x7c00, 0xfc00, 0x7c01]
  go (k : a : b : c : rs) =
    let e = 13 + k `P.mod` 18
        d1 = (k `shiftR` 8) `P.mod` 13
        d2 = (k `shiftR` 16) `P.mod` 13
        mk r d = if e < d + 1 then Half 0 else Half (fromIntegral ((r .&. 0x83ff) .|. ((e - d) `shiftL` 10)))
        (x, y, z) = (mk a 0, mk b d1, mk c d2)
     in (case (k `shiftR` 24) `P.mod` 3 of
           0 -> (x, y, z)
           1 -> (y, z, x)
           _ -> (z, x, y)) : go rs
  go _ = []
