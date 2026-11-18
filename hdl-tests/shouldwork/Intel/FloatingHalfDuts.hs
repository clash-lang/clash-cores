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

-- | E.asin, E.asinWith @8
asinDut :: Dut Half (Vec 2 Half)
asinDut clk rst en i = bundle (unary (E.asin simFamily) clk rst en i :> unary (E.asinWith @8 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE asinDut #-}

-- | E.asinPi, E.asinPiWith @10
asinPiDut :: Dut Half (Vec 2 Half)
asinPiDut clk rst en i = bundle (unary (E.asinPi simFamily) clk rst en i :> unary (E.asinPiWith @10 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE asinPiDut #-}

-- | E.acos, E.acosWith @11
acosDut :: Dut Half (Vec 2 Half)
acosDut clk rst en i = bundle (unary (E.acos simFamily) clk rst en i :> unary (E.acosWith @11 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE acosDut #-}

-- | E.acosPi, E.acosPiWith @13
acosPiDut :: Dut Half (Vec 2 Half)
acosPiDut clk rst en i = bundle (unary (E.acosPi simFamily) clk rst en i :> unary (E.acosPiWith @13 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE acosPiDut #-}

-- | E.atan, E.atanWith @9
atanDut :: Dut Half (Vec 2 Half)
atanDut clk rst en i = bundle (unary (E.atan simFamily) clk rst en i :> unary (E.atanWith @9 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE atanDut #-}

-- | E.atanPi, E.atanPiWith @11
atanPiDut :: Dut Half (Vec 2 Half)
atanPiDut clk rst en i = bundle (unary (E.atanPi simFamily) clk rst en i :> unary (E.atanPiWith @11 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE atanPiDut #-}

-- | E.atan2, E.atan2With @16
atan2Dut :: Dut (Half, Half) (Vec 2 Half)
atan2Dut clk rst en i = bundle (binary (E.atan2 simFamily) clk rst en i :> binary (E.atan2With @16 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE atan2Dut #-}

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

-- | Pairs of half precision numbers for 'E.atan2': all combinations of a few
-- special values, followed by @n@ random pairs of either sign whose (biased)
-- exponents differ by less than 14, so that the arctangent of their quotient
-- is neither tiny nor pi \/ 2. In every fourth pair the second number is
-- negative and about 2^11 to 2^15 times the first: for those the IP subtracts
-- a tiny angle from pi.
halfPairs :: Int -> Word64 -> [(Half, Half)]
halfPairs n seed = [(a, b) | a <- sp, b <- sp] <> P.take n (go (randoms seed))
 where
  sp = P.map Half
    [ 0, 0x8000, 0x0001, 0x8001, 0x03ff, 0x0400, 0x3c00, 0xbc00, 0x7bff, 0xfbff
    , 0x7c00, 0xfc00, 0x7c01 ]
  go (k : a : b : rs) =
    let e = 1 + k `P.mod` 30
        tiny = (k `shiftR` 24) `P.mod` 4 == 0
        d | tiny = 11 + (k `shiftR` 8) `P.mod` 5
          | otherwise = (k `shiftR` 8) `P.mod` 14
        e2 = if (k `shiftR` 16) `P.mod` 2 == 0 then e + d else e - d
        mk r ex = Half (fromIntegral ((r .&. 0x83ff) .|. (P.max 1 (P.min 30 ex) `shiftL` 10)))
        (lo, hi) = if e <= e2 then (e, e2) else (e2, e)
     in (if tiny
           then (mk a lo, Half (getHalf (mk b hi) .|. 0x8000))
           else (mk a e, mk b e2)) : go rs
  go _ = []
