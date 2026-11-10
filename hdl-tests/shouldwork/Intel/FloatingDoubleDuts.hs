{- | Functions under test for "FloatingDouble". They are defined here, as the
Template Haskell in "FloatingDouble" simulates them to create reference
traces.
-}
{-# LANGUAGE TemplateHaskell #-}

module FloatingDoubleDuts where

import Clash.Explicit.Prelude
import qualified Prelude as P

import qualified Clash.Cores.Intel.Floating.Double.Explicit as E

import Data.Word (Word64)
import GHC.Float (castWord64ToDouble)

import FloatingDuts (simFamily)
import IpTestCommon

-- | E.add, E.addWith @4
addDut :: Dut (Double, Double) (Vec 2 Double)
addDut clk rst en i = bundle (binary E.add clk rst en i :> binary (E.addWith @4 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE addDut #-}

-- | E.sub, E.subWith @4
subDut :: Dut (Double, Double) (Vec 2 Double)
subDut clk rst en i = bundle (binary E.sub clk rst en i :> binary (E.subWith @4 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE subDut #-}

-- | E.mul, E.mulWith @4
mulDut :: Dut (Double, Double) (Vec 2 Double)
mulDut clk rst en i = bundle (binary E.mul clk rst en i :> binary (E.mulWith @4 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE mulDut #-}

-- | E.div, E.divWith @16
divDut :: Dut (Double, Double) (Vec 2 Double)
divDut clk rst en i = bundle (binary E.div clk rst en i :> binary (E.divWith @16 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE divDut #-}

-- | E.sqrt, E.sqrtWith @12
sqrtDut :: Dut Double (Vec 2 Double)
sqrtDut clk rst en i = bundle (unary (E.sqrt simFamily) clk rst en i :> unary (E.sqrtWith @12 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE sqrtDut #-}

-- | E.recip, E.recipWith @15
recipDut :: Dut Double (Vec 2 Double)
recipDut clk rst en i = bundle (unary E.recip clk rst en i :> unary (E.recipWith @15 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE recipDut #-}

-- | Bit patterns of interesting doubles: zeros, subnormal numbers, the
-- smallest and largest normal numbers, infinities, NaNs, numbers around one
-- and powers of two
specialDoubleWords :: [Word64]
specialDoubleWords =
  [ 0x0000000000000000, 0x8000000000000000, 0x0000000000000001, 0x8000000000000001
  , 0x000fffffffffffff, 0x800fffffffffffff, 0x0010000000000000, 0x8010000000000000
  , 0x0010000000000001, 0x001fffffffffffff, 0x0020000000000000, 0x7fefffffffffffff
  , 0xffefffffffffffff, 0x7fe0000000000000, 0x7fdfffffffffffff, 0x7ff0000000000000
  , 0xfff0000000000000, 0x7ff8000000000000, 0xfff8000000000000, 0x7ff0000000000001
  , 0xffffffffffffffff, 0x3ff0000000000000, 0xbff0000000000000, 0x3ff0000000000001
  , 0x3fefffffffffffff, 0x3fffffffffffffff, 0x4000000000000000, 0x3fe0000000000000
  , 0x4010000000000000, 0x3fd0000000000000, 0x400921fb54442d18, 0x3fb999999999999a ]

-- | Doubles: 'specialDoubleWords', followed by @n@ random ones of several
-- kinds: all bit patterns, positive normal numbers with biased exponents in
-- @[lo, lo + m)@, numbers with few fraction bits set, and numbers at the ends
-- of the exponent range
doublesIn :: Word64 -> Word64 -> Int -> Word64 -> [Double]
doublesIn lo m n seed =
  P.map castWord64ToDouble (specialDoubleWords <> P.take n (go (randoms seed)))
 where
  withExp x e = (x .&. 0x800fffffffffffff) .|. (e `shiftL` 52)
  go (k : x : rs) =
    let w = case k `mod` 5 of
          0 -> x
          1 -> withExp x (lo + (k `shiftR` 8) `mod` m) .&. 0x7fffffffffffffff
          2 -> withExp (x .&. 0xfff00000000000ff) (lo + (k `shiftR` 8) `mod` m) .&. 0x7fffffffffffffff
          3 -> withExp x (if testBit k 9 then 1 + (k `shiftR` 10) `mod` 3 else 2046 - (k `shiftR` 10) `mod` 3)
          _ -> withExp x (0x3fe + (k `shiftR` 8) `mod` 4) .&. 0x7fffffffffffffff
     in w : go rs
  go _ = []

-- | Pairs of doubles: all pairs of the first 20 'specialDoubleWords', followed
-- by @n@ random pairs: random bit patterns, numbers close to each other (with
-- either sign) and pairs whose product or quotient is close to the smallest
-- normal number
doublePairs :: Int -> Word64 -> [(Double, Double)]
doublePairs n seed =
  [(castWord64ToDouble x, castWord64ToDouble y) | x <- sp, y <- sp]
    <> P.take n (go (randoms seed))
 where
  sp = P.take 20 specialDoubleWords
  withExp x e = (x .&. 0x800fffffffffffff) .|. (e `shiftL` 52)
  go (k : x : y : rs) =
    let a = withExp x (0x3c0 + (k `shiftR` 8) `mod` 0x80)
        ec = 0x3e0 + (k `shiftR` 8) `mod` 0x20
        c = withExp x ec
        d = (k `shiftR` 20) `mod` 3
        pair = case k `mod` 5 of
          0 -> (castWord64ToDouble x, castWord64ToDouble y)
          1 -> (castWord64ToDouble a, castWord64ToDouble ((a `xor` (y `mod` 16)) `xor` (y .&. bit 63)))
          -- a * b close to the smallest normal number
          2 -> (castWord64ToDouble c, castWord64ToDouble (withExp y (0x3ff - ec + d)))
          -- a / b close to the smallest normal number
          3 -> (castWord64ToDouble c, castWord64ToDouble (withExp y (ec + 0x3fe - d)))
          _ -> (castWord64ToDouble a, castWord64ToDouble (withExp y (0x3c0 + (k `shiftR` 16) `mod` 0x80)))
     in pair : go rs
  go _ = []
