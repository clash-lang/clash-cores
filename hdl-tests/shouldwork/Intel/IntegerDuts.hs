{- | Functions under test and stimuli for "Integer". They are defined here, as
the Template Haskell in "Integer" simulates them to create reference traces.
-}
module IntegerDuts where

import Clash.Explicit.Prelude hiding (divide)
import qualified Prelude as P

import Data.Word (Word64)

import Clash.Cores.Intel.Divide
import qualified Clash.Cores.Intel.Sqrt as S

import IpTestCommon

-- | Pairs of numerators and denominators: interesting bit patterns (zero, one,
-- minus one, the bounds), followed by @n@ random pairs with random magnitudes.
numerDenoms :: Int -> Word64 -> [(BitVector 16, BitVector 8)]
numerDenoms n seed =
  [(a, b) | a <- specialsN, b <- specialsD] <> P.take n (go (randoms seed))
 where
  specialsN = [0, 1, 2, 0x7fff, 0x8000, 0x8001, 0xffff, 0xfffe, 0x00ff, 0x0100]
  specialsD = [0, 1, 2, 3, 0x7f, 0x80, 0x81, 0xff, 0xfe, 0x10]
  go (k : x : y : rs) =
    ( fromIntegral x `shiftR` fromIntegral (k `mod` 16)
    , fromIntegral y `shiftR` fromIntegral ((k `shiftR` 8) `mod` 8) )
    : go rs
  go _ = []

-- | Radicals: interesting ones, followed by @n@ random ones with random
-- magnitudes
radicals :: Int -> Word64 -> [Unsigned 17]
radicals n seed =
  [0, 1, 2, 3, 4, 15, 16, 17, maxBound, maxBound - 1, 0x10000, 0xffff, 0x8000]
    <> P.take n (go (randoms seed))
 where
  go (k : x : rs) = (fromIntegral x `shiftR` fromIntegral (k `mod` 17)) : go rs
  go _ = []

-- | Unsigned division with latencies 0, 3 and 8
divideUnsignedDut :: Dut (BitVector 16, BitVector 8) (Vec 3 (Unsigned 16, Unsigned 8))
divideUnsignedDut clk rst en i =
  bundle
    (  binary (\c r e a b -> divide @0 RemainderPositive c r e (unpack <$> a) (unpack <$> b)) clk rst en i
    :> binary (\c r e a b -> divide @3 RemainderPositive c r e (unpack <$> a) (unpack <$> b)) clk rst en i
    :> binary (\c r e a b -> divide @8 RemainderSignOfNumerator c r e (unpack <$> a) (unpack <$> b)) clk rst en i
    :> Nil )
{-# OPAQUE divideUnsignedDut #-}

-- | Signed division with both remainder signs
divideSignedDut :: Dut (BitVector 16, BitVector 8) (Vec 3 (Signed 16, Signed 8))
divideSignedDut clk rst en i =
  bundle
    (  binary (\c r e a b -> divide @0 RemainderPositive c r e (unpack <$> a) (unpack <$> b)) clk rst en i
    :> binary (\c r e a b -> divide @4 RemainderPositive c r e (unpack <$> a) (unpack <$> b)) clk rst en i
    :> binary (\c r e a b -> divide @2 RemainderSignOfNumerator c r e (unpack <$> a) (unpack <$> b)) clk rst en i
    :> Nil )
{-# OPAQUE divideSignedDut #-}

-- | Square roots with latencies 0, 1 and 5
sqrtDut :: Dut (Unsigned 17) (Vec 3 (Unsigned 9, Unsigned 10))
sqrtDut clk rst en i =
  bundle
    (  unary (S.sqrt @0) clk rst en i
    :> unary (S.sqrt @1) clk rst en i
    :> unary (S.sqrt @5) clk rst en i
    :> Nil )
{-# OPAQUE sqrtDut #-}
