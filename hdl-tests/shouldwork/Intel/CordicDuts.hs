{-# LANGUAGE TemplateHaskell #-}

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
import Clash.Cores.Intel.Cordic.Internal
  (AtanFunction(..), DeviceFamily(..), SinCosWidths, atanMargins, atanWidthsOrError, rotateWidths, sinCosMargins,
   sinCosWidths, vecRotateMargins, vecTranslateFracR)

import IpTestCommon

import Data.Char (isSpace, toLower)
import Language.Haskell.TH.Syntax (lift, runIO)
import System.Environment (lookupEnv)

{- | The device family the IP's simulation models are generated for, from
@CLASH_INTEL_SIM_FAMILY@ (see "IntelSimLib"), read when this module is
compiled. The angle of the vector translation (and of the arctangent with LUT
size optimization) differs on Cyclone V.
-}
simFamily :: DeviceFamily
simFamily = $(do
  fam <- runIO (lookupEnv "CLASH_INTEL_SIM_FAMILY")
  let norm = P.map toLower . P.filter (P.not . isSpace)
  lift $ case norm <$> fam of
    Just "cyclonev" -> CycloneV
    Just "cycloneive" -> CycloneIVE
    Just "cyclone10lp" -> Cyclone10LP
    _ -> Max10)

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

-- | Raw inputs @(x, y)@ for the arctangent and the vector translation with
-- @w@ bit inputs and @fOut@ fractional output bits: all pairs of interesting
-- values (zero, one, the bounds), @n@ random pairs (a third of them scaled
-- down, so small vectors are covered too), and @n@ pairs that are close to
-- changing the result when a detail of the datapath changes (see
-- 'atanMargins'), picked from @20 * n@ random ones: a third each by Y in the
-- last iterations, by the angle and by the magnitude.
vectors :: AtanFunction -> Bool -> Int -> Int -> Int -> Word64 -> [(Integer, Integer)]
vectors fun signed w fOut n seed = edges <> P.take n randomIn <> hard
 where
  (lo, hi) = if signed then (negate (2 P.^ (w - 1)), 2 P.^ (w - 1) - 1) else (0, 2 P.^ w - 1)
  special = [0, 1, 2, hi, hi - 1, hi `P.div` 2] <> (if signed then [-1, lo, lo + 1, lo `P.div` 2] else [])
  edges = [(x, y) | x <- special, y <- special]
  randomIn = go (randoms seed)
  go (a : b : k : rs) =
    let v r = toInteger r `mod` (hi - lo + 1) + lo
        sh = if k `mod` 3 == 0 then fromIntegral (k `shiftR` 8) `mod` w else 0
    in (v a `P.div` 2 P.^ sh, v b `P.div` 2 P.^ sh) : go rs
  go _ = []
  widths = atanWidthsOrError simFamily fun signed w fOut
  margins = atanMargins widths (vecTranslateFracR widths)
  cands = P.take (20 * n) (P.drop n randomIn)
  pick sel = P.take (n `P.div` 3) (P.map snd (sortOn fst [(sel (margins x y), (x, y)) | (x, y) <- cands]))
  hard = pick (\(a, _, _) -> a) <> pick (\(_, b, _) -> b) <> pick (\(_, _, c) -> c)

-- | 'vectors' as signed fixed point numbers
vectorsS :: forall i f. (KnownNat i, KnownNat f) => AtanFunction -> Int -> Int -> Word64 -> [(SFixed i f, SFixed i f)]
vectorsS fun fOut n seed =
  [ (sf (SNat @f) (fromInteger x), sf (SNat @f) (fromInteger y))
  | (x, y) <- vectors fun True (natToNum @(i + f)) fOut n seed ]

-- | 'vectors' as unsigned fixed point numbers
vectorsU :: forall i f. (KnownNat i, KnownNat f) => AtanFunction -> Int -> Int -> Word64 -> [(UFixed i f, UFixed i f)]
vectorsU fun fOut n seed =
  [ (uf (SNat @f) (fromInteger x), uf (SNat @f) (fromInteger y))
  | (x, y) <- vectors fun False (natToNum @(i + f)) fOut n seed ]

-- | Inputs @(x, y, angle)@ for 'E.vectorRotate' with @f@ fractional input
-- bits and @fOut@ fractional output bits: combinations of interesting
-- coordinates (zero, one, the extremes of the input format) and angles (zero,
-- +-pi\/4, around +-pi\/2 where the IP pre-rotates the vector, +-pi, the
-- extremes), @n@ random inputs (most in the documented ranges, some anywhere),
-- and @n@ inputs that are close to changing the result when a detail of the
-- datapath changes (see 'vecRotateMargins'), picked from @20 * n@ random ones.
rotations :: forall f. KnownNat f => Int -> Int -> Word64 -> [(SFixed 2 f, SFixed 2 f, SFixed 3 f)]
rotations fOut n seed =
  [ (sf (SNat @f) (fromInteger x), sf (SNat @f) (fromInteger y), sf (SNat @f) (fromInteger a))
  | (x, y, a) <- edges <> P.take n randomIn <> hard ]
 where
  f = natToNum @f :: Int
  one = 2 P.^ f :: Integer
  maxXY = 2 * one - 1
  maxA = 4 * one - 1
  piA = P.floor (pi * 2 P.^^ f :: Double) :: Integer
  halfPi = P.round (pi / 2 * 2 P.^^ f :: Double) :: Integer
  quarterPi = P.round (pi / 4 * 2 P.^^ f :: Double) :: Integer
  coords = [0, 1, -1, one, negate one, maxXY, negate maxXY - 1]
  angs =
    [0, quarterPi, negate quarterPi, piA, negate piA, maxA, negate maxA - 1]
      <> [c + d | c <- [halfPi, negate halfPi], d <- [-2 .. 2]]
  edges = [(x, y, a) | x <- coords, y <- coords, a <- [0, halfPi, negate halfPi - 1, piA]]
    <> [(one, 0, a) | a <- angs] <> [(x, y, a) | (x, y) <- [(one, one), (negate one, 1)], a <- angs]
  randomIn = go (randoms seed)
  go (r1 : r2 : r3 : k : rs) =
    let inRange = k `mod` 8 /= 0
        c r = if inRange then toInteger r `mod` (2 * one + 1) - one else toInteger r `mod` (4 * one) - 2 * one
        an = if inRange then toInteger r3 `mod` (2 * piA + 1) - piA else toInteger r3 `mod` (8 * one) - 4 * one
    in (c r1, c r2, an) : go rs
  go _ = []
  margins = vecRotateMargins (rotateWidths f fOut)
  cands = P.take (20 * n) (P.drop n randomIn)
  pick sel = P.take (n `P.div` 2) (P.map snd (sortOn fst [(sel (margins x y a), (x, y, a)) | (x, y, a) <- cands]))
  hard = pick fst <> pick snd

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

-- | Run an arctangent, which takes @y@ first, on pairs @(x, y)@
atanOf ::
  (Clock Dom -> Reset Dom -> Enable Dom -> DSignal Dom 0 a -> DSignal Dom 0 a -> DSignal Dom d b) ->
  Clock Dom -> Reset Dom -> Enable Dom -> Signal Dom (a, a) -> Signal Dom b
atanOf f = binary (\clk rst en x y -> f clk rst en y x)

-- | E.atan2 @3 and @0 on 10 bit inputs, 8 fractional output bits
atan2W10Dut :: Dut (SFixed 2 8, SFixed 2 8) (Vec 2 (SFixed 3 8))
atan2W10Dut clk rst en i =
  bundle (atanOf (E.atan2 @3) clk rst en i :> atanOf (E.atan2 @0) clk rst en i :> Nil)
{-# OPAQUE atan2W10Dut #-}

-- | E.atan2 @6 on 8 bit inputs, 14 fractional output bits (X and Y wider
-- than usual, see the Lambda table)
atan2W8F14Dut :: Dut (SFixed 2 6, SFixed 2 6) (Vec 1 (SFixed 3 14))
atan2W8F14Dut clk rst en i = bundle (atanOf (E.atan2 @6) clk rst en i :> Nil)
{-# OPAQUE atan2W8F14Dut #-}

-- | E.atan2 @25 on 6 bit inputs, 20 fractional output bits (extra iterations;
-- the IP needs a latency of at least about 21 for this format)
atan2W6F20Dut :: Dut (SFixed 1 5, SFixed 1 5) (Vec 1 (SFixed 3 20))
atan2W6F20Dut clk rst en i = bundle (atanOf (E.atan2 @25) clk rst en i :> Nil)
{-# OPAQUE atan2W6F20Dut #-}

-- | E.atan2 @8 on 12 bit inputs, 20 fractional output bits (one extra
-- iteration)
atan2W12F20Dut :: Dut (SFixed 2 10, SFixed 2 10) (Vec 1 (SFixed 3 20))
atan2W12F20Dut clk rst en i = bundle (atanOf (E.atan2 @8) clk rst en i :> Nil)
{-# OPAQUE atan2W12F20Dut #-}

-- | E.atan2 @12 on 32 bit inputs, 28 fractional output bits
atan2W32Dut :: Dut (SFixed 2 30, SFixed 2 30) (Vec 1 (SFixed 3 28))
atan2W32Dut clk rst en i = bundle (atanOf (E.atan2 @12) clk rst en i :> Nil)
{-# OPAQUE atan2W32Dut #-}

-- | E.atan2Unsigned @4 on 10 bit inputs with 12 fractional output bits, and
-- @1 with 1 fractional output bit (where pi\/2 wraps around to 0)
atan2UnsignedDut :: Dut (UFixed 0 10, UFixed 0 10) (Vec 2 (SFixed 2 12))
atan2UnsignedDut clk rst en i =
  bundle
    (  atanOf (E.atan2Unsigned @4) clk rst en i
    :> fmap resizeF (atanOf (E.atan2Unsigned @1 @_ @_ @0 @10 @1) clk rst en i)
    :> Nil )
{-# OPAQUE atan2UnsignedDut #-}

-- | E.atan2Lut @3 and @0 on 8 bit inputs, 6 fractional output bits (tables of
-- four and of six directions give different results for this format)
atan2LutDut :: Dut (SFixed 2 6, SFixed 2 6) (Vec 2 (SFixed 3 6))
atan2LutDut clk rst en i =
  bundle
    (  atanOf (E.atan2Lut @3 simFamily) clk rst en i
    :> atanOf (E.atan2Lut @0 simFamily) clk rst en i
    :> Nil )
{-# OPAQUE atan2LutDut #-}

-- | E.atan2Lut @9 on 16 bit inputs, 14 fractional output bits
atan2LutW16Dut :: Dut (SFixed 4 12, SFixed 4 12) (Vec 1 (SFixed 3 14))
atan2LutW16Dut clk rst en i = bundle (atanOf (E.atan2Lut @9 simFamily) clk rst en i :> Nil)
{-# OPAQUE atan2LutW16Dut #-}

-- | E.atan2UnsignedLut @4 on 8 bit inputs, 16 and 15 fractional output bits
-- (formats whose results depend on the precision and the size of the tables)
atan2UnsignedLutDut :: Dut (UFixed 0 8, UFixed 0 8) (Vec 2 (SFixed 2 16))
atan2UnsignedLutDut clk rst en i =
  bundle
    (  atanOf (E.atan2UnsignedLut @4 simFamily) clk rst en i
    :> fmap resizeF (atanOf (E.atan2UnsignedLut @3 @_ @_ @0 @8 @15 simFamily) clk rst en i)
    :> Nil )
{-# OPAQUE atan2UnsignedLutDut #-}

-- | E.vectorTranslate @5 on 8 bit inputs, 12 fractional output bits
vecTranslateDut :: Dut (SFixed 2 6, SFixed 2 6) (Vec 1 (SFixed 3 12, UFixed 3 12))
vecTranslateDut clk rst en i = bundle (binary (E.vectorTranslate @5 simFamily) clk rst en i :> Nil)
{-# OPAQUE vecTranslateDut #-}

-- | E.vectorTranslateCompensated @7 on 8 bit inputs, 8 fractional output
-- bits (X and Y wider than for ATAN2 with the same format)
vecTranslateCompDut :: Dut (SFixed 2 6, SFixed 2 6) (Vec 1 (SFixed 3 8, UFixed 2 8))
vecTranslateCompDut clk rst en i = bundle (binary (E.vectorTranslateCompensated @7 simFamily) clk rst en i :> Nil)
{-# OPAQUE vecTranslateCompDut #-}

-- | E.vectorTranslateUnsignedCompensated @4 on 8 bit inputs, 10 fractional
-- output bits
vecTranslateUnsignedCompDut :: Dut (UFixed 0 8, UFixed 0 8) (Vec 1 (SFixed 2 10, UFixed 1 11))
vecTranslateUnsignedCompDut clk rst en i =
  bundle (binary (E.vectorTranslateUnsignedCompensated @4 simFamily) clk rst en i :> Nil)
{-# OPAQUE vecTranslateUnsignedCompDut #-}

-- | E.vectorRotate @2 and @0 with 4 fractional bits in and out
vecRotate4Dut :: Dut (SFixed 2 4, SFixed 2 4, SFixed 3 4) (Vec 2 (SFixed 3 4, SFixed 3 4))
vecRotate4Dut clk rst en i =
  bundle (ternary (E.vectorRotate @2) clk rst en i :> ternary (E.vectorRotate @0) clk rst en i :> Nil)
{-# OPAQUE vecRotate4Dut #-}

-- | E.vectorRotate @5 and @0 with 8 fractional bits in and out
vecRotate8Dut :: Dut (SFixed 2 8, SFixed 2 8, SFixed 3 8) (Vec 2 (SFixed 3 8, SFixed 3 8))
vecRotate8Dut clk rst en i =
  bundle (ternary (E.vectorRotate @5) clk rst en i :> ternary (E.vectorRotate @0) clk rst en i :> Nil)
{-# OPAQUE vecRotate8Dut #-}

-- | E.vectorRotate @10 with 16 fractional bits in, 20 out
vecRotate16x20Dut :: Dut (SFixed 2 16, SFixed 2 16, SFixed 3 16) (Vec 1 (SFixed 3 20, SFixed 3 20))
vecRotate16x20Dut clk rst en i = bundle (ternary (E.vectorRotate @10) clk rst en i :> Nil)
{-# OPAQUE vecRotate16x20Dut #-}
