{- | Functions under test for "Cordic", and their stimuli. They are defined
here, as the Template Haskell in "Cordic" simulates them to create reference
traces.
-}
module CordicDuts where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Data.List (sortOn)
import Data.Word (Word64)

import qualified Clash.Cores.Intel.Cordic.Explicit as E
import Clash.Cores.Intel.Cordic.Internal (SinCosWidths, sinCosMargins, sinCosWidths)

import IpTestCommon

-- * Stimuli

-- | Angles between -pi and pi for 'E.sinCos' with @fOut@ output bits: the
-- bounds, zero, angles around +-pi\/2 and +-pi\/4 (where the IP subtracts
-- pi\/2 or the direction of its first rotation changes), @n@ random angles,
-- and @n@ angles that are close to changing the result when a detail of the
-- datapath changes (see 'sinCosMargins'), picked from @20 * n@ random ones.
angles :: forall fIn. KnownNat fIn => Int -> Int -> Word64 -> [SFixed 3 fIn]
angles fOut n seed =
  P.map (sf (SNat @fIn) . fromInteger) (edges <> randomAngles <> hard)
 where
  fIn = natToNum @fIn :: Int
  maxA = P.floor (pi * 2 P.^^ fIn :: Double) :: Integer
  halfPi = P.round (pi / 2 * 2 P.^^ fIn :: Double) :: Integer
  quarterPi = P.round (pi / 4 * 2 P.^^ fIn :: Double) :: Integer
  around c = [c + d | d <- [-3 .. 3], abs (c + d) <= maxA]
  edges =
    [0, 1, -1, maxA, negate maxA, maxA - 1, negate maxA + 1]
      <> P.concatMap around [halfPi, negate halfPi, quarterPi, negate quarterPi]
  randomIn = [toInteger x `mod` (2 * maxA + 1) - maxA | x <- randoms seed]
  randomAngles = P.take n randomIn
  hard = hardInputs (sinCosWidths True fIn fOut) n (P.take (20 * n) (P.drop n randomIn))

-- | Angles between 0 and pi\/2 for 'E.sinCosUnsigned', see 'angles'
anglesUnsigned :: forall fIn. KnownNat fIn => Int -> Int -> Word64 -> [UFixed 1 fIn]
anglesUnsigned fOut n seed =
  P.map (uf (SNat @fIn) . fromInteger) (edges <> randomAngles <> hard)
 where
  fIn = natToNum @fIn :: Int
  maxA = P.floor (pi / 2 * 2 P.^^ fIn :: Double) :: Integer
  quarterPi = P.round (pi / 4 * 2 P.^^ fIn :: Double) :: Integer
  edges = [0, 1, 2, maxA, maxA - 1, maxA - 2] <> [quarterPi + d | d <- [-3 .. 3]]
  randomIn = [toInteger x `mod` (maxA + 1) | x <- randoms seed]
  randomAngles = P.take n randomIn
  hard = hardInputs (sinCosWidths False fIn fOut) n (P.take (20 * n) (P.drop n randomIn))

-- | The @n@ inputs out of the given ones with the smallest margins (see
-- 'sinCosMargins'): half of them by the angle in the iterations, half of
-- them by the distance to a rounding boundary
hardInputs :: SinCosWidths -> Int -> [Integer] -> [Integer]
hardInputs widths n xs =
  P.take (n `P.div` 2) (P.map snd (sortOn fst [(fst (margins x), x) | x <- xs]))
    <> P.take (n `P.div` 2) (P.map snd (sortOn fst [(snd (margins x), x) | x <- xs]))
 where
  margins = sinCosMargins widths

-- * Functions under test

-- | E.sinCos @3, E.sinCos @0 on 10 fractional bits
sinCos10Dut :: Dut (SFixed 3 10) (Vec 2 (SFixed 2 10, SFixed 2 10))
sinCos10Dut clk rst en i = bundle (unary (E.sinCos @3) clk rst en i :> unary (E.sinCos @0) clk rst en i :> Nil)
{-# OPAQUE sinCos10Dut #-}

-- | E.sinCos @12 with 16 fractional bits in, 8 out (the IP ignores input bits)
sinCos16x8Dut :: Dut (SFixed 3 16) (Vec 1 (SFixed 2 8, SFixed 2 8))
sinCos16x8Dut clk rst en i = bundle (unary (E.sinCos @12) clk rst en i :> Nil)
{-# OPAQUE sinCos16x8Dut #-}

-- | E.sinCos @20 with 20 fractional bits in, 16 out
sinCos20x16Dut :: Dut (SFixed 3 20) (Vec 1 (SFixed 2 16, SFixed 2 16))
sinCos20x16Dut clk rst en i = bundle (unary (E.sinCos @20) clk rst en i :> Nil)
{-# OPAQUE sinCos20x16Dut #-}

-- | E.sinCos @7 with 4 fractional bits
sinCos4Dut :: Dut (SFixed 3 4) (Vec 1 (SFixed 2 4, SFixed 2 4))
sinCos4Dut clk rst en i = bundle (unary (E.sinCos @7) clk rst en i :> Nil)
{-# OPAQUE sinCos4Dut #-}

-- | E.sinCosUnsigned @5 with 10 fractional bits in, 12 out, and
-- E.sinCosUnsigned @1 with 10 fractional bits in, 6 out
sinCosUnsignedDut :: Dut (UFixed 1 10) (Vec 2 (UFixed 1 12, UFixed 1 12))
sinCosUnsignedDut clk rst en i =
  bundle
    (  unary (E.sinCosUnsigned @5) clk rst en i
    :> fmap (\(c, s) -> (resizeF c, resizeF s)) (unary (E.sinCosUnsigned @1 @_ @_ @10 @6) clk rst en i)
    :> Nil )
{-# OPAQUE sinCosUnsignedDut #-}

-- | E.sinCos @2 with 12 fractional bits
sinCos12Dut :: Dut (SFixed 3 12) (Vec 1 (SFixed 2 12, SFixed 2 12))
sinCos12Dut clk rst en i = bundle (unary (E.sinCos @2) clk rst en i :> Nil)
{-# OPAQUE sinCos12Dut #-}
