{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Properties of the @DSP48E1@ simulation model that can be checked against simple
references, and its undefined outputs. The model itself is checked against
Xilinx's UNISIM model by the HDL tests in
@hdl-tests/shouldwork/Xilinx/Dsp48e1.hs@.
-}

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cores.Xilinx.Dsp48e1 (tests) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))
import Clash.Sized.Internal.BitVector (BitVector(BV))
import Control.Exception (SomeException, evaluate, try)
import Data.Either (isLeft, isRight)

import qualified Clash.Signal.Delayed as D
import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P

import Hedgehog (Gen, Property, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)

import Clash.Cores.Xilinx.Unisim.Dsp48e1

genSigned :: KnownNat n => Gen (Signed n)
genSigned = Gen.integral Range.linearBounded

genList :: Gen a -> Gen [a]
genList = Gen.list (Range.linear 1 100)

sameLength :: [b] -> Gen a -> Gen [a]
sameLength xs = Gen.list (Range.singleton (L.length xs))

-- | Simulate a signal for as many cycles as there are elements in the list
sim :: NFDataX a => Int -> Signal dom a -> [a]
sim = sampleN

-- | Turn a list into a signal, repeating the last element
fromList' :: NFDataX a => [a] -> Signal dom a
fromList' xs = fromList (xs <> P.repeat (P.last xs))

clk :: Clock XilinxSystem
clk = clockGen

toA :: Signed 25 -> BitVector 30
toA = pack . resize @_ @25 @30

-- | Configuration without any registers
combConfig :: Dsp48e1Config
combConfig = defDsp48e1Config
  { aReg = 0, aCascReg = 0, bReg = 0, bCascReg = 0, cReg = 0, dReg = 0
  , adReg = 0, mReg = 0, pReg = 0, inModeReg = 0, opModeReg = 0
  , aluModeReg = 0, carryInReg = 0, carryInSelReg = 0
  , useMult = MultDynamic
  }

-- | OPMODE: Z = C, Y:X = M
opModeMPlusC :: BitVector 7
opModeMPlusC = 0b011_01_01

-- | With all registers enabled, P = A * B + C, with A and B delayed by three
-- cycles, and C delayed by two cycles.
prop_mulAddRegistered :: Property
prop_mulAddRegistered = property $ do
  as <- forAll (genList genSigned)
  bs <- forAll (sameLength as genSigned)
  cs <- forAll (sameLength as genSigned)
  let
    n = L.length as
    out = dsp48e1 defDsp48e1Config clk defDsp48e1Input
      { dataA = fromList' (toA <$> as)
      , dataB = fromList' (pack <$> bs)
      , dataC = fromList' (pack <$> cs)
      , opMode = pure opModeMPlusC
      }
    actual = L.drop 3 (sim (n + 3) (unpack @(Signed 48) <$> dataP out))
    cs1 = L.drop 1 cs <> [L.last cs]
    expected = L.zipWith3 (\a b c -> resize a * resize b + c) as bs cs1
  actual === expected

-- | Without any registers, P = A * B + C combinationally
prop_mulAddCombinational :: Property
prop_mulAddCombinational = property $ do
  as <- forAll (genList genSigned)
  bs <- forAll (sameLength as genSigned)
  cs <- forAll (sameLength as genSigned)
  let
    out = dsp48e1 combConfig clk defDsp48e1Input
      { dataA = fromList' (toA <$> as)
      , dataB = fromList' (pack <$> bs)
      , dataC = fromList' (pack <$> cs)
      , opMode = pure opModeMPlusC
      }
    actual = sim (L.length as) (unpack @(Signed 48) <$> dataP out)
    expected = L.zipWith3 (\a b c -> resize a * resize b + c) as bs cs
  actual === expected

-- | The multiply-accumulate from the module documentation
mac ::
  KnownDomain dom =>
  Clock dom ->
  Signal dom Bool ->
  Signal dom (Signed 25) ->
  Signal dom (Signed 18) ->
  Signal dom (Signed 48)
mac clk0 start a b =
  unpack <$> dataP (dsp48e1 defDsp48e1Config clk0 inputs)
 where
  inputs = defDsp48e1Input
    { dataA = resize . pack <$> a
    , dataB = pack <$> b
    , opMode = mux (delay clk0 enableGen False start)
        (pure 0b000_01_01)
        (pure 0b010_01_01)
    }

prop_mac :: Property
prop_mac = property $ do
  as <- forAll (genList genSigned)
  bs <- forAll (sameLength as genSigned)
  starts0 <- forAll (sameLength as Gen.bool)
  let
    starts = True : L.drop 1 starts0
    n = L.length as
    actual =
      L.drop 3 $ sim (n + 3) $
        mac clk (fromList' starts) (fromList' as) (fromList' bs)
    products = L.zipWith (\a b -> resize a * resize b) as bs
    expected = L.drop 1 $ L.scanl
      (\acc (start, m) -> if start then m else acc + m)
      0 (L.zip starts products)
  actual === expected

-- | The pre-adder computes D + A or D - A, the multiplier multiplies that by B
prop_preAdder :: Property
prop_preAdder = property $ do
  as <- forAll (genList genSigned)
  bs <- forAll (sameLength as genSigned)
  ds <- forAll (sameLength as genSigned)
  subtract' <- forAll Gen.bool
  let
    out = dsp48e1 combConfig{useDPort = True} clk defDsp48e1Input
      { dataA = fromList' (toA <$> as)
      , dataB = fromList' (pack <$> bs)
      , dataD = fromList' (pack <$> ds)
      , inMode = pure (if subtract' then 0b01100 else 0b00100)
      , opMode = pure 0b000_01_01
      }
    actual = sim (L.length as) (unpack @(Signed 48) <$> dataP out)
    preAdd a d = if subtract' then d - a else d + a :: Signed 25
    expected =
      L.zipWith3 (\a b d -> resize (preAdd a d) * resize b) as bs ds
  actual === expected

-- | In FOUR12 mode, the ALU consists of four independent 12-bit adders, each
-- with their own carry out.
prop_simdFour12 :: Property
prop_simdFour12 = property $ do
  abs' <- forAll (genList (Gen.integral Range.linearBounded))
  cs <- forAll (sameLength abs' (Gen.integral Range.linearBounded))
  let
    cfg = combConfig{useMult = MultNone, useSimd = Four12}
    out = dsp48e1 cfg clk defDsp48e1Input
      { dataA = fromList' (slice d47 d18 <$> abs')
      , dataB = fromList' (slice d17 d0 <$> abs')
      , dataC = fromList' cs
      , opMode = pure 0b000_11_11 -- A:B + C
      }
    actual = sim (L.length abs') (bundle (dataP out, carryOut out))
    lane :: BitVector 48 -> Vec 4 (BitVector 12)
    lane = unpack
    addLane :: BitVector 12 -> BitVector 12 -> BitVector 13
    addLane x y = zeroExtend x + zeroExtend y
    expected = L.zipWith
      (\ab c ->
         let sums = zipWith addLane (lane ab) (lane c)
         in (pack (map (slice d11 d0) sums), pack (map msb sums)))
      abs' cs
  actual === expected

-- | ALUMODE 0100 computes X XOR Z
prop_logicXor :: Property
prop_logicXor = property $ do
  abs' <- forAll (genList (Gen.integral Range.linearBounded))
  cs <- forAll (sameLength abs' (Gen.integral Range.linearBounded))
  let
    out = dsp48e1 combConfig{useMult = MultNone} clk defDsp48e1Input
      { dataA = fromList' (slice d47 d18 <$> abs')
      , dataB = fromList' (slice d17 d0 <$> abs')
      , dataC = fromList' cs
      , aluMode = pure 0b0100
      , opMode = pure 0b011_00_11 -- X = A:B, Z = C
      }
    actual = sim (L.length abs') (dataP out)
  actual === L.zipWith xor abs' cs

-- | A second DSP adds its own product to the one of the first through the P
-- cascade, using A and B from the cascade.
prop_cascade :: Property
prop_cascade = property $ do
  as <- forAll (genList genSigned)
  bs <- forAll (sameLength as genSigned)
  let
    out1 = dsp48e1 combConfig clk defDsp48e1Input
      { dataA = fromList' (toA <$> as)
      , dataB = fromList' (pack <$> bs)
      , opMode = pure 0b000_01_01 -- P = M
      }
    out2 = dsp48e1 combConfig{aInput = Cascade, bInput = Cascade} clk
      (cascade out1 defDsp48e1Input{opMode = pure 0b001_01_01}) -- P = PCIN + M
    actual = sim (L.length as) (unpack @(Signed 48) <$> dataP out2)
    expected = L.zipWith (\a b -> 2 * resize a * resize b) as bs
  actual === expected

-- | 'multiplyAdd' computes A * B + C with a latency of two cycles
prop_multiplyAdd :: Property
prop_multiplyAdd = property $ do
  as <- forAll (genList genSigned)
  bs <- forAll (sameLength as genSigned)
  cs <- forAll (sameLength as genSigned)
  let
    n = L.length as
    out = multiplyAdd clk enableGen
      (D.fromSignal (fromList' as))
      (D.fromSignal (fromList' bs))
      (D.fromSignal (fromList' cs))
    actual = L.drop 2 (sim (n + 2) (D.toSignal out))
    expected = L.zipWith3 (\a b c -> resize a * resize b + c) as bs cs
  actual === expected

-- | 'multiplyAccumulate' is the example from the module documentation, with
-- an enable. As all registers are disabled when the enable is deasserted, and
-- the output is registered, the output after /k/ enabled cycles is the output
-- of the example after /k/ cycles with only the inputs of the enabled cycles.
prop_multiplyAccumulate :: Property
prop_multiplyAccumulate = property $ do
  as <- forAll (genList genSigned)
  bs <- forAll (sameLength as genSigned)
  starts <- forAll (sameLength as Gen.bool)
  ens <- forAll (sameLength as Gen.bool)
  let
    n = L.length as
    actual =
      sim n $ D.toSignal $
        multiplyAccumulate clk (toEnable (fromList' ens))
          (D.fromSignal (fromList' starts))
          (D.fromSignal (fromList' as))
          (D.fromSignal (fromList' bs))
    enabled :: [a] -> [a]
    enabled xs = [x | (x, True) <- L.zip xs ens]
    reference =
      sim (n + 1) $
        mac clk
          (fromList' (enabled starts <> [False]))
          (fromList' (enabled as <> [0]))
          (fromList' (enabled bs <> [0]))
    nEnabledBefore = L.scanl (+) 0 (P.map fromEnum ens)
    expected = P.map (reference L.!!) (L.take n nEnabledBefore)
  actual === expected

-- | The outputs can be used to compute the inputs of the same cycle, as long
-- as there is a register in between.
feedbackP :: [BitVector 48]
feedbackP = sampleN 5 (dataP out)
 where
  out = dsp48e1 defDsp48e1Config clk defDsp48e1Input
    { dataC = (+1) <$> dataP out
    , opMode = pure 0b011_00_00 -- P = C
    }

-- | Whether a bit of a 'BitVector' is undefined
isUndefinedBit :: KnownNat n => BitVector n -> Int -> Bool
isUndefinedBit (BV m _) i = testBit m i

firstSample :: Signal dom a -> a
firstSample (a :- _) = a

-- | Assert that a value is undefined
assertUndefined :: NFDataX a => String -> a -> Assertion
assertUndefined msg a = do
  r <- try @SomeException (evaluate (rnfX a `seq` hasUndefined a))
  assertBool msg (either (const True) id r)

-- | Assert that a value is defined
assertDefined :: NFDataX a => String -> a -> Assertion
assertDefined msg a = do
  r <- try @SomeException (evaluate (hasUndefined a))
  assertBool msg (either (const False) not r)

-- | Pattern detector outputs of a DSP without registers, for a constant OPMODE,
-- ALUMODE and CARRYINSEL
patternDetectFor :: BitVector 4 -> BitVector 7 -> BitVector 3 -> Bool
patternDetectFor alu op cis = firstSample $ patternDetect $
  dsp48e1 combConfig clk defDsp48e1Input
    {aluMode = pure alu, opMode = pure op, carryInSel = pure cis}

tests :: TestTree
tests = testGroup "DSP48E1"
  [ testPropertyNamed "P = A*B + C, registered" "prop_mulAddRegistered" prop_mulAddRegistered
  , testPropertyNamed "P = A*B + C, combinational" "prop_mulAddCombinational" prop_mulAddCombinational
  , testPropertyNamed "multiply-accumulate example" "prop_mac" prop_mac
  , testPropertyNamed "pre-adder" "prop_preAdder" prop_preAdder
  , testPropertyNamed "SIMD FOUR12 adder" "prop_simdFour12" prop_simdFour12
  , testPropertyNamed "logic XOR" "prop_logicXor" prop_logicXor
  , testPropertyNamed "cascade" "prop_cascade" prop_cascade
  , testPropertyNamed "multiplyAdd" "prop_multiplyAdd" prop_multiplyAdd
  , testPropertyNamed "multiplyAccumulate" "prop_multiplyAccumulate" prop_multiplyAccumulate
  , testCase "feedback through a register" $
      -- C is registered too, so P increases by one every two cycles
      feedbackP @?= [0, 0, 1, 1, 2]
  , testCase "unused carry outputs are undefined" $ do
      let
        carry cfg alu = firstSample $ carryOut $
          dsp48e1 cfg{useMult = MultNone} clk
            defDsp48e1Input{opMode = pure 0b011_00_11, aluMode = pure alu}
        undefBits cfg alu = P.map (isUndefinedBit (carry cfg alu)) [0..3]
      undefBits combConfig 0 @?= [True, True, True, False]
      undefBits combConfig{useSimd = Two24} 0 @?= [True, False, True, False]
      undefBits combConfig{useSimd = Four12} 0 @?= [False, False, False, False]
      -- Logic operation
      undefBits combConfig{useSimd = Four12} 0b0100 @?= [True, True, True, True]
  , testCase "overflow is undefined without pattern detector and P register" $ do
      assertUndefined "overflow defined" $
        firstSample (overflow (dsp48e1 combConfig clk defDsp48e1Input))
      assertDefined "overflow undefined with P register" $
        firstSample (overflow (dsp48e1 defDsp48e1Config clk defDsp48e1Input))
  , testCase "invalid ACASCREG is rejected" $ do
      r <- try @SomeException (evaluate (firstSample (dataP
             (dsp48e1 defDsp48e1Config{aCascReg = 2} clk defDsp48e1Input))))
      assertBool "invalid configuration accepted" (isLeft r)
  , testCase "P feedback without P register is undefined" $
      assertUndefined "P feedback without P register accepted" $
        firstSample $ dataP $
          dsp48e1 combConfig clk defDsp48e1Input{opMode = pure 0b010_00_00}
  , testCase "CARRYINSEL = 010 makes the product undefined" $ do
      -- The CARRYINSEL pin is 010 in the second cycle only. In the third
      -- cycle, the CARRYINSEL register is 010, making the multiplier output
      -- undefined, which shows up in P in the fourth cycle.
      let
        ps = sampleN 6 $ dataP $
          dsp48e1 defDsp48e1Config clk defDsp48e1Input
            { dataA = pure 3, dataB = pure 4
            , opMode = pure opModeMPlusC
            , carryInSel = fromList' [0, 0b010, 0]
            }
      P.map hasUndefined ps @?= [False, False, False, True, False, False]
  , testCase "OPMODE check" $ do
      -- Valid arithmetic and logic modes
      assertDefined "P = C" (patternDetectFor 0b0000 0b011_00_00 0b000)
      assertDefined "P = C XOR 0" (patternDetectFor 0b0100 0b011_00_00 0b000)
      -- Invalid: OPMODE[3:0] = 0001 (half of the multiplier)
      assertUndefined "X = M only" (patternDetectFor 0b0000 0b000_00_01 0b000)
      -- Invalid without P register: P = P
      assertUndefined "Z = P" (patternDetectFor 0b0000 0b010_00_00 0b000)
      -- Invalid for logic modes
      assertUndefined "logic with M" (patternDetectFor 0b0100 0b000_01_01 0b000)
  , testCase "OPMODE check keeps its result for ALUMODE[3:2] = 10" $ do
      let
        pdets alus ops = sampleN 4 $ patternDetect $
          dsp48e1 combConfig clk defDsp48e1Input
            {aluMode = fromList' alus, opMode = fromList' ops}
        defined xs = P.map (isRight . isX) xs
      -- Valid, then ALUMODE 10xx without changing anything else: still valid
      defined (pdets [0b0000, 0b0000] [0b011_00_00]) @?= [True, True, True, True]
      -- Invalid, then ALUMODE 10xx: undefined, as the control signals changed
      defined (pdets [0b0000, 0b1000] [0b000_00_01]) @?= [False, False, False, False]
      -- Valid, then ALUMODE 10xx with an invalid OPMODE: undefined, as the
      -- control signals changed
      defined (pdets [0b0000, 0b1000] [0b011_00_00, 0b000_00_01])
        @?= [True, False, False, False]
  ]
