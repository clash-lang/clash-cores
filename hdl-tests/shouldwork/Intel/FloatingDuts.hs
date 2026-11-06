{- | Functions under test for "Floating". They are defined here, as the
Template Haskell in "Floating" simulates them to create reference traces.
-}
module FloatingDuts where

import Clash.Explicit.Prelude

import qualified Clash.Cores.Intel.Floating.Explicit as E

import IpTestCommon

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
