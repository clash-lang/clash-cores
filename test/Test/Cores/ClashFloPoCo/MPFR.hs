{-|
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  MPFR integration tests - comprehensive validation of MPFR FFI bindings
  including basic operations, rounding modes, precision handling, and edge cases.
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cores.ClashFloPoCo.MPFR where

import Prelude
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Cores.ClashFloPoCo.MPFR
import Foreign.C.Types (CLong)

-- * Generators for property-based testing

-- | Generate arbitrary doubles, excluding NaN and infinities for basic tests
genFiniteDouble :: H.Gen Double
genFiniteDouble = Gen.double (Range.exponentialFloatFrom 0 (-1e100) 1e100)

-- | Generate small finite doubles (better for precision comparisons)
genSmallDouble :: H.Gen Double
genSmallDouble = Gen.double (Range.exponentialFloatFrom 0 (-1000) 1000)

-- | Generate non-zero doubles (filters out values too close to zero)
genNonZeroDouble :: H.Gen Double
genNonZeroDouble = Gen.filter (\x -> abs x >= 1e-10) genSmallDouble

-- | Generate precision values (in bits) - typical range for floating point
-- Using 24 as minimum (single precision float) to avoid extreme rounding errors
genPrecision :: H.Gen CLong
genPrecision = Gen.integral (Range.linear 24 128)

-- | Generate low precision for rounding tests
genLowPrecision :: H.Gen CLong
genLowPrecision = Gen.integral (Range.linear 24 53)

-- | Generate precision for tests requiring numerical stability
-- Minimum 53 bits (double precision) for associativity tests
genStablePrecision :: H.Gen CLong
genStablePrecision = Gen.integral (Range.linear 53 128)

-- | Generate rounding modes
genRoundingMode :: H.Gen MPFRRnd
genRoundingMode = Gen.element [rndN, rndZ, rndU, rndD, rndA]

-- | Standard precision values for common use cases
standardPrecision :: CLong
standardPrecision = 53  -- Double's mantissa size

lowPrecision :: CLong
lowPrecision = 24  -- Float's mantissa size

-- * Helper functions

-- | Check if two doubles are approximately equal within epsilon
approxEqual :: Double -> Double -> Double -> Bool
approxEqual epsilon a b
  | isNaN a && isNaN b = True
  | isInfinite a && isInfinite b = a == b
  | otherwise = abs (a - b) <= epsilon

-- | Calculate relative epsilon based on magnitude of values
relativeEpsilon :: Double -> Double -> Double
relativeEpsilon a b = 1e-10 * max (abs a) (abs b)

-- | Property: MPFR addition is commutative
prop_mpfrAdd_commutative :: H.Property
prop_mpfrAdd_commutative = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genSmallDouble
  prec <- H.forAll genPrecision

  result1 <- H.evalIO $ mpfrAdd prec a b
  result2 <- H.evalIO $ mpfrAdd prec b a

  let epsilon = max 1e-10 (relativeEpsilon result1 result2)
  H.assert $ approxEqual epsilon result1 result2

-- | Property: MPFR multiplication is commutative
prop_mpfrMul_commutative :: H.Property
prop_mpfrMul_commutative = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genSmallDouble
  prec <- H.forAll genPrecision

  result1 <- H.evalIO $ mpfrMul prec a b
  result2 <- H.evalIO $ mpfrMul prec b a

  let epsilon = max 1e-10 (relativeEpsilon result1 result2)
  H.assert $ approxEqual epsilon result1 result2

-- | Property: MPFR addition with associativity (within fp precision)
prop_mpfrAdd_associative :: H.Property
prop_mpfrAdd_associative = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genSmallDouble
  c <- H.forAll genSmallDouble
  prec <- H.forAll genStablePrecision

  -- (a + b) + c
  ab <- H.evalIO $ mpfrAdd prec a b
  abc1 <- H.evalIO $ mpfrAdd prec ab c

  -- a + (b + c)
  bc <- H.evalIO $ mpfrAdd prec b c
  abc2 <- H.evalIO $ mpfrAdd prec a bc

  -- Due to floating point rounding, associativity may not hold exactly
  -- but should be very close for reasonable precision
  let epsilon = max 1e-8 (relativeEpsilon abc1 abc2)
  H.assert $ approxEqual epsilon abc1 abc2

-- | Property: MPFR multiplication with associativity (within fp precision)
prop_mpfrMul_associative :: H.Property
prop_mpfrMul_associative = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genSmallDouble
  c <- H.forAll genSmallDouble
  prec <- H.forAll genStablePrecision

  -- (a * b) * c
  ab <- H.evalIO $ mpfrMul prec a b
  abc1 <- H.evalIO $ mpfrMul prec ab c

  -- a * (b * c)
  bc <- H.evalIO $ mpfrMul prec b c
  abc2 <- H.evalIO $ mpfrMul prec a bc

  let epsilon = max 1e-8 (relativeEpsilon abc1 abc2)
  H.assert $ approxEqual epsilon abc1 abc2

-- | Property: Adding zero returns the same value
prop_mpfrAdd_identity :: H.Property
prop_mpfrAdd_identity = H.property $ do
  a <- H.forAll genFiniteDouble
  prec <- H.forAll genPrecision

  result <- H.evalIO $ mpfrAdd prec a 0.0

  -- Epsilon depends on precision - lower precision allows more error
  let precisionEpsilon = 2.0 ** (-(fromIntegral prec :: Double))
  let epsilon = max precisionEpsilon (abs a * precisionEpsilon * 10)
  H.assert $ approxEqual epsilon result a

-- | Property: Multiplying by one returns the same value
prop_mpfrMul_identity :: H.Property
prop_mpfrMul_identity = H.property $ do
  a <- H.forAll genFiniteDouble
  prec <- H.forAll genPrecision

  result <- H.evalIO $ mpfrMul prec a 1.0

  -- Epsilon depends on precision - lower precision allows more error
  let precisionEpsilon = 2.0 ** (-(fromIntegral prec :: Double))
  let epsilon = max precisionEpsilon (abs a * precisionEpsilon * 10)
  H.assert $ approxEqual epsilon result a

-- | Property: Multiplying by zero returns zero
prop_mpfrMul_zero :: H.Property
prop_mpfrMul_zero = H.property $ do
  a <- H.forAll genFiniteDouble
  prec <- H.forAll genPrecision

  result <- H.evalIO $ mpfrMul prec a 0.0

  H.assert $ abs result < 1e-100

-- | Property: MPFR matches Haskell's Double for standard precision
prop_mpfrAdd_matches_double :: H.Property
prop_mpfrAdd_matches_double = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genSmallDouble

  mpfrResult <- H.evalIO $ mpfrAdd standardPrecision a b
  let haskellResult = a + b

  -- Allow for small differences due to different rounding modes
  let epsilon = max 1e-10 (relativeEpsilon mpfrResult haskellResult)
  H.assert $ approxEqual epsilon mpfrResult haskellResult

-- | Property: MPFR matches Haskell's Double for multiplication
prop_mpfrMul_matches_double :: H.Property
prop_mpfrMul_matches_double = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genSmallDouble

  mpfrResult <- H.evalIO $ mpfrMul standardPrecision a b
  let haskellResult = a * b

  let epsilon = max 1e-10 (relativeEpsilon mpfrResult haskellResult)
  H.assert $ approxEqual epsilon mpfrResult haskellResult

-- | Property: Higher precision should give more accurate results for addition
-- Tests that precision parameter affects result quality
prop_precision_affects_accuracy :: H.Property
prop_precision_affects_accuracy = H.property $ do
  -- Use values with many significant digits to see precision differences
  let a = 1.23456789012345
      b = 9.87654321098765e-10
  
  lowPrecResult <- H.evalIO $ mpfrAdd 24 a b
  highPrecResult <- H.evalIO $ mpfrAdd 128 a b
  
  -- With low precision, we lose information. High precision should be closer to true sum
  -- At 24 bits, the small b might get lost in rounding
  let exactSum = a + b
      lowError = abs (lowPrecResult - exactSum)
      highError = abs (highPrecResult - exactSum)
  
  -- High precision should be at least as good or better (allowing for edge cases)
  H.assert $ highError <= lowError + 1e-15

-- | Property: NaN propagates through operations
prop_mpfr_nan_propagation :: H.Property
prop_mpfr_nan_propagation = H.property $ do
  b <- H.forAll genSmallDouble
  prec <- H.forAll genPrecision
  
  let nan = 0/0
  addResult <- H.evalIO $ mpfrAdd prec nan b
  mulResult <- H.evalIO $ mpfrMul prec nan b
  
  H.assert $ isNaN addResult && isNaN mulResult

-- | Property: Infinity arithmetic follows IEEE 754 rules
prop_mpfr_infinity_arithmetic :: H.Property
prop_mpfr_infinity_arithmetic = H.property $ do
  x <- H.forAll $ Gen.filter (> 0) genSmallDouble
  prec <- H.forAll genPrecision
  
  let posInf = 1/0
      negInf = -1/0
  
  -- Inf + finite = Inf
  addPosInf <- H.evalIO $ mpfrAdd prec posInf x
  H.assert $ isInfinite addPosInf && addPosInf > 0
  
  addNegInf <- H.evalIO $ mpfrAdd prec negInf x
  H.assert $ isInfinite addNegInf && addNegInf < 0
  
  -- Inf * positive = Inf
  mulInf <- H.evalIO $ mpfrMul prec posInf x
  H.assert $ isInfinite mulInf && mulInf > 0

-- | Property: MPFR binary operations match Haskell Double at standard precision
prop_mpfr_binary_ops_match_double :: H.Property
prop_mpfr_binary_ops_match_double = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genNonZeroDouble  -- genNonZeroDouble now filters properly
  
  -- Test subtraction
  subResult <- H.evalIO $ mpfrSub standardPrecision a b
  H.assert $ approxEqual (max 1e-10 (relativeEpsilon subResult (a - b))) subResult (a - b)
  
  -- Test division
  divResult <- H.evalIO $ mpfrDiv standardPrecision a b
  H.assert $ approxEqual (max 1e-10 (relativeEpsilon divResult (a / b))) divResult (a / b)

-- | Property: MPFR unary operations match Haskell Double at standard precision  
prop_mpfr_unary_ops_match_double :: H.Property
prop_mpfr_unary_ops_match_double = H.property $ do
  a <- H.forAll genSmallDouble
  
  -- Test absolute value
  absResult <- H.evalIO $ mpfrAbs standardPrecision a
  H.assert $ approxEqual 1e-10 absResult (abs a)
  
  -- Test sine and cosine (always defined)
  sinResult <- H.evalIO $ mpfrSin standardPrecision a
  H.assert $ approxEqual 1e-10 sinResult (sin a)
  
  cosResult <- H.evalIO $ mpfrCos standardPrecision a
  H.assert $ approxEqual 1e-10 cosResult (cos a)
  
-- | Property: MPFR exponential and logarithm match Haskell Double
prop_mpfr_exp_log_match_double :: H.Property
prop_mpfr_exp_log_match_double = H.property $ do
  -- Use restricted range to avoid overflow in exp
  a <- H.forAll $ Gen.double (Range.linearFracFrom 0 (-50) 50)
  
  -- Test exponential
  expResult <- H.evalIO $ mpfrExp standardPrecision a
  H.assert $ approxEqual (max 1e-10 (relativeEpsilon expResult (exp a))) expResult (exp a)
  
  -- Test logarithm (for positive inputs only)
  b <- H.forAll $ Gen.filter (> 0) genSmallDouble
  logResult <- H.evalIO $ mpfrLog standardPrecision b
  H.assert $ approxEqual (max 1e-10 (relativeEpsilon logResult (log b))) logResult (log b)

-- | Property: MPFR comparison matches Haskell's Double comparison
prop_mpfrCmp_matches_double :: H.Property
prop_mpfrCmp_matches_double = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genSmallDouble
  mpfrResult <- H.evalIO $ mpfrCmp standardPrecision a b
  let haskellResult = compare a b
  H.assert $ mpfrResult == haskellResult

-- * Rounding mode properties

-- | Property: Different rounding modes produce different results for operations requiring rounding
prop_rounding_modes_differ :: H.Property
prop_rounding_modes_differ = H.property $ do
  -- Use 1/3 which requires rounding at any finite precision
  let a = 1.0
      b = 3.0
  prec <- H.forAll genLowPrecision

  resultRndN <- H.evalIO $ mpfrDivRnd prec rndN a b
  resultRndZ <- H.evalIO $ mpfrDivRnd prec rndZ a b
  resultRndU <- H.evalIO $ mpfrDivRnd prec rndU a b
  resultRndD <- H.evalIO $ mpfrDivRnd prec rndD a b

  -- All results valid and at low precision, at least some modes should differ
  H.assert $ not (isNaN resultRndN) && not (isInfinite resultRndN)
  H.assert $ resultRndU /= resultRndD || resultRndZ /= resultRndN

-- | Property: Rounding toward zero produces smaller absolute values
prop_rounding_toward_zero :: H.Property
prop_rounding_toward_zero = H.property $ do
  a <- H.forAll $ Gen.double (Range.linearFracFrom 0 10 100)
  b <- H.forAll $ Gen.double (Range.linearFracFrom 0 1 10)
  prec <- H.forAll genLowPrecision

  resultRndN <- H.evalIO $ mpfrDivRnd prec rndN a b
  resultRndZ <- H.evalIO $ mpfrDivRnd prec rndZ a b

  -- For finite results, RNDZ magnitude should be <= RNDN
  if not (isInfinite resultRndZ) && not (isInfinite resultRndN)
    then H.assert $ abs resultRndZ <= abs resultRndN + 1e-6
    else H.success

-- | Property: Rounding modes are consistent across different operations
prop_rounding_consistency :: H.Property
prop_rounding_consistency = H.property $ do
  a <- H.forAll genSmallDouble
  b <- H.forAll genNonZeroDouble
  prec <- H.forAll genPrecision
  mode <- H.forAll genRoundingMode

  -- Test that using the same rounding mode produces valid results
  resultAdd <- H.evalIO $ mpfrAddRnd prec mode a b
  resultMul <- H.evalIO $ mpfrMulRnd prec mode a b
  resultSub <- H.evalIO $ mpfrSubRnd prec mode a b
  resultDiv <- H.evalIO $ mpfrDivRnd prec mode a b

  -- All results should be valid (not NaN unless inputs are problematic)
  H.assert $ not (isNaN resultAdd) && not (isNaN resultMul)
  H.assert $ not (isNaN resultSub) && not (isNaN resultDiv)

-- | Property: Rounding up gives results >= rounding down
prop_rounding_up_vs_down :: H.Property
prop_rounding_up_vs_down = H.property $ do
  a <- H.forAll $ Gen.double (Range.linearFracFrom 0 1 100)
  b <- H.forAll $ Gen.double (Range.linearFracFrom 0 1 100)
  prec <- H.forAll genLowPrecision

  resultU <- H.evalIO $ mpfrDivRnd prec rndU a b
  resultD <- H.evalIO $ mpfrDivRnd prec rndD a b

  -- For positive finite operands and results, RNDU >= RNDD
  if not (isInfinite resultU) && not (isInfinite resultD)
    then H.assert $ resultU >= resultD - 1e-10
    else H.success

-- * Struct size validation tests

-- | Property: Struct size is adequate - validated via stress test
-- If struct size were too small, operations would crash or produce garbage
prop_struct_size_stress :: H.Property
prop_struct_size_stress = H.property $ do
  prec <- H.forAll genPrecision
  start <- H.forAll $ Gen.integral (Range.linear 1 100) :: H.PropertyT IO Int
  
  -- Perform several operations in sequence - tests struct reuse
  let iterations = 10 :: Int
  results <- H.evalIO $ sequence [mpfrAdd prec (fromIntegral (start + i)) 1.0 | i <- [1..iterations]]
  
  -- All results should be valid and correct
  let expected = [fromIntegral (start + i) + 1.0 | i <- [1..iterations]]
      errors = zipWith (\r e -> abs (r - e)) results expected
  
  H.assert $ all (not . isNaN) results
  H.assert $ all (< 1e-10) errors

tests :: TestTree
tests = testGroup "MPFR Integration"
  [ testGroup "Addition Properties"
      [ testProperty "Commutativity" prop_mpfrAdd_commutative
      , testProperty "Associativity" prop_mpfrAdd_associative
      , testProperty "Identity (zero)" prop_mpfrAdd_identity
      , testProperty "Matches Haskell Double" prop_mpfrAdd_matches_double
      ]
  , testGroup "Multiplication Properties"
      [ testProperty "Commutativity" prop_mpfrMul_commutative
      , testProperty "Associativity" prop_mpfrMul_associative
      , testProperty "Identity (one)" prop_mpfrMul_identity
      , testProperty "Zero property" prop_mpfrMul_zero
      , testProperty "Matches Haskell Double" prop_mpfrMul_matches_double
      ]
  , testGroup "Other Operations Match Haskell Double"
      [ testProperty "Binary operations (sub, div)" prop_mpfr_binary_ops_match_double
      , testProperty "Unary operations (abs, sin, cos)" prop_mpfr_unary_ops_match_double
      , testProperty "Exponential and logarithm" prop_mpfr_exp_log_match_double
      , testProperty "Comparison" prop_mpfrCmp_matches_double
      ]
  , testGroup "Rounding Modes"
      [ testProperty "Different modes produce different results" prop_rounding_modes_differ
      , testProperty "Rounding toward zero behavior" prop_rounding_toward_zero
      , testProperty "Rounding consistency across operations" prop_rounding_consistency
      , testProperty "Rounding up vs down ordering" prop_rounding_up_vs_down
      ]
  , testGroup "Precision and Accuracy"
      [ testProperty "Higher precision improves accuracy" prop_precision_affects_accuracy
      , testProperty "Struct size adequate (stress test)" prop_struct_size_stress
      ]
  , testGroup "Edge Cases"
      [ testProperty "NaN propagation" prop_mpfr_nan_propagation
      , testProperty "Infinity arithmetic" prop_mpfr_infinity_arithmetic
      ]
  ]
