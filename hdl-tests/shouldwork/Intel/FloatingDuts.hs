{- | Functions under test for "Floating". They are defined here, as the
Template Haskell in "Floating" simulates them to create reference traces.
-}
{-# LANGUAGE TemplateHaskell #-}

module FloatingDuts where

import Clash.Explicit.Prelude
import qualified Prelude as P

import qualified Clash.Cores.Intel.Floating.Explicit as E

import IpTestCommon

import Data.Char (isSpace, toLower)
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat)
import Language.Haskell.TH.Syntax (lift, runIO)
import System.Environment (lookupEnv)

{- | The device family the IP's simulation models are generated for, from
@CLASH_INTEL_SIM_FAMILY@ (see "IntelSimLib"), read when this module is
compiled. The square root and the transcendental functions compute slightly
differently on Cyclone V than on the other families.
-}
simFamily :: E.DeviceFamily
simFamily = $(do
  fam <- runIO (lookupEnv "CLASH_INTEL_SIM_FAMILY")
  let norm = P.map toLower . P.filter (P.not . isSpace)
  lift $ case norm <$> fam of
    Just "cyclonev" -> E.CycloneV
    Just "cycloneive" -> E.CycloneIVE
    Just "cyclone10lp" -> E.Cyclone10LP
    _ -> E.Max10)

-- | E.add, E.addWith @3 E.defConfig, E.addWith @0 E.defConfig
addDut :: Dut (Float, Float) (Vec 3 Float)
addDut clk rst en i = bundle (binary (E.add) clk rst en i :> binary (E.addWith @3 E.defConfig) clk rst en i :> binary (E.addWith @0 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE addDut #-}

-- | E.sub, E.subWith @2 E.defConfig
subDut :: Dut (Float, Float) (Vec 2 Float)
subDut clk rst en i = bundle (binary (E.sub) clk rst en i :> binary (E.subWith @2 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE subDut #-}

-- | E.mul, E.mulWith @2 E.defConfig
mulDut :: Dut (Float, Float) (Vec 2 Float)
mulDut clk rst en i = bundle (binary (E.mul) clk rst en i :> binary (E.mulWith @2 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE mulDut #-}

-- | E.div, E.divWith @10 E.defConfig
divDut :: Dut (Float, Float) (Vec 2 Float)
divDut clk rst en i = bundle (binary (E.div) clk rst en i :> binary (E.divWith @10 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE divDut #-}

-- | E.mulAdd, E.mulAddWith @4 E.defConfig
mulAddDut :: Dut (Float, Float, Float) (Vec 2 Float)
mulAddDut clk rst en i = bundle (ternary (E.mulAdd) clk rst en i :> ternary (E.mulAddWith @4 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE mulAddDut #-}

-- | E.compare, E.compareWith @1 E.defConfig
compareDut :: Dut (Float, Float) (Vec 2 E.Ordering)
compareDut clk rst en i = bundle (binary (E.compare) clk rst en i :> binary (E.compareWith @1 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE compareDut #-}

-- | E.predicateWith @1 E.defConfig E.Lt, E.predicateWith @1 E.defConfig E.Le, E.predicateWith @1 E.defConfig E.Eq, E.predicateWith @1 E.defConfig E.Ne, E.predicateWith @1 E.defConfig E.Gt, E.predicateWith @1 E.defConfig E.Ge
predicateDut :: Dut (Float, Float) (Vec 6 Bool)
predicateDut clk rst en i = bundle (binary (E.predicateWith @1 E.defConfig E.Lt) clk rst en i :> binary (E.predicateWith @1 E.defConfig E.Le) clk rst en i :> binary (E.predicateWith @1 E.defConfig E.Eq) clk rst en i :> binary (E.predicateWith @1 E.defConfig E.Ne) clk rst en i :> binary (E.predicateWith @1 E.defConfig E.Gt) clk rst en i :> binary (E.predicateWith @1 E.defConfig E.Ge) clk rst en i :> Nil)
{-# OPAQUE predicateDut #-}

-- | E.fromS32, E.fromS32With @1
fromS32Dut :: Dut (Signed 32) (Vec 2 Float)
fromS32Dut clk rst en i = bundle (unary (E.fromS32) clk rst en i :> unary (E.fromS32With @1 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE fromS32Dut #-}

-- | E.fromU32, E.fromU32With @2
fromU32Dut :: Dut (Unsigned 32) (Vec 2 Float)
fromU32Dut clk rst en i = bundle (unary (E.fromU32) clk rst en i :> unary (E.fromU32With @2 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE fromU32Dut #-}

-- | E.toS32, E.toS32With @0
toS32Dut :: Dut Float (Vec 2 (Signed 32))
toS32Dut clk rst en i = bundle (unary (E.toS32) clk rst en i :> unary (E.toS32With @0 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE toS32Dut #-}

-- | E.toU32, E.toU32With @1
toU32Dut :: Dut Float (Vec 2 (Unsigned 32))
toU32Dut clk rst en i = bundle (unary (E.toU32) clk rst en i :> unary (E.toU32With @1 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE toU32Dut #-}

-- | E.toDouble, E.toDoubleWith @0
toDoubleDut :: Dut Float (Vec 2 Double)
toDoubleDut clk rst en i = bundle (unary (E.toDouble) clk rst en i :> unary (E.toDoubleWith @0 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE toDoubleDut #-}

-- | E.fromDouble, E.fromDoubleWith @1
fromDoubleDut :: Dut Double (Vec 2 Float)
fromDoubleDut clk rst en i = bundle (unary (E.fromDouble) clk rst en i :> unary (E.fromDoubleWith @1 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE fromDoubleDut #-}

-- | E.sqrt, E.sqrtWith @6
sqrtDut :: Dut Float (Vec 2 Float)
sqrtDut clk rst en i = bundle (unary (E.sqrt simFamily) clk rst en i :> unary (E.sqrtWith @6 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE sqrtDut #-}

-- | E.recip, E.recipWith @8
recipDut :: Dut Float (Vec 2 Float)
recipDut clk rst en i = bundle (unary E.recip clk rst en i :> unary (E.recipWith @8 E.defConfig) clk rst en i :> Nil)
{-# OPAQUE recipDut #-}

-- | E.rsqrt, E.rsqrtWith @6
rsqrtDut :: Dut Float (Vec 2 Float)
rsqrtDut clk rst en i = bundle (unary (E.rsqrt simFamily) clk rst en i :> unary (E.rsqrtWith @6 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE rsqrtDut #-}

-- | E.exp, E.expWith @6
expDut :: Dut Float (Vec 2 Float)
expDut clk rst en i = bundle (unary (E.exp simFamily) clk rst en i :> unary (E.expWith @6 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE expDut #-}

-- | E.exp2, E.exp2With @6
exp2Dut :: Dut Float (Vec 2 Float)
exp2Dut clk rst en i = bundle (unary (E.exp2 simFamily) clk rst en i :> unary (E.exp2With @6 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE exp2Dut #-}

-- | E.exp10, E.exp10With @8
exp10Dut :: Dut Float (Vec 2 Float)
exp10Dut clk rst en i = bundle (unary (E.exp10 simFamily) clk rst en i :> unary (E.exp10With @8 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE exp10Dut #-}

-- | E.log, E.logWith @10
logDut :: Dut Float (Vec 2 Float)
logDut clk rst en i = bundle (unary (E.log simFamily) clk rst en i :> unary (E.logWith @10 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE logDut #-}

-- | E.log2, E.log2With @10
log2Dut :: Dut Float (Vec 2 Float)
log2Dut clk rst en i = bundle (unary (E.log2 simFamily) clk rst en i :> unary (E.log2With @10 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE log2Dut #-}

-- | E.log10, E.log10With @10
log10Dut :: Dut Float (Vec 2 Float)
log10Dut clk rst en i = bundle (unary (E.log10 simFamily) clk rst en i :> unary (E.log10With @10 E.defConfig simFamily) clk rst en i :> Nil)
{-# OPAQUE log10Dut #-}

-- | Positive floats with exponents (biased) in @[lo, lo + n)@ and random
-- fractions, after the 'specials'
floatsIn :: Word32 -> Word32 -> Int -> Word64 -> [Float]
floatsIn lo n cnt seed = specials <> P.map mk (words32 cnt seed)
 where
  mk w = let v = fromIntegral w :: Word32
             e = lo + (v `shiftR` 23) `P.mod` n
         in castWord32ToFloat ((v .&. 0x807fffff .&. 0x7fffffff) .|. (e `shiftL` 23))

-- | Mostly negative or positive floats in the range where 'E.exp' is finite
expInputs :: Int -> Word64 -> [Float]
expInputs cnt seed = specials <> P.map mk (words32 cnt seed)
 where
  mk w = let v = fromIntegral w :: Word32
             e = 100 + (v `shiftR` 23) `P.mod` 34
         in castWord32ToFloat ((v .&. 0x807fffff) .|. (e `shiftL` 23))

-- | Floats of either sign with exponents (biased) in @[lo, lo + n)@ and random
-- fractions, after the 'specials'
signedFloatsIn :: Word32 -> Word32 -> Int -> Word64 -> [Float]
signedFloatsIn lo n cnt seed = specials <> P.map mk (words32 cnt seed)
 where
  mk w = let v = fromIntegral w :: Word32
             e = lo + (v `shiftR` 23) `P.mod` n
         in castWord32ToFloat ((v .&. 0x807fffff) .|. (e `shiftL` 23))
