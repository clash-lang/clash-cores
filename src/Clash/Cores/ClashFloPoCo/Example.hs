{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

{- |
Copyright   : (C) 2024, QBayLogic B.V.
License     : BSD2
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Example module demonstrating Clash FloPoCo integration for floating point operations.

This module shows how to create hardware-synthesizable floating point adders
using FloPoCo IP cores. The example includes:

* Defining an InfoEntity with pipeline depth and signal names
* Using Template Haskell to generate blackbox functions
* Creating a pipelined floating point adder with proper delay matching
* Annotating primitives for HDL generation

= Usage Example

@
import Clash.Prelude
import Clash.Cores.ClashFloPoCo.Example

-- Top entity for a simple floating point adder
topEntity 
  :: Clock System
  -> Signal System Float
  -> Signal System Float
  -> Signal System Float
topEntity clk a b = 
  let aDelayed = toSignal (fromSignal a :: DSignal System 0 Float)
      bDelayed = toSignal (fromSignal b :: DSignal System 0 Float)
      result = toSignal (floPoCoAdder clk aDelayed bDelayed)
  in result
@
-}
module Clash.Cores.ClashFloPoCo.Example
  ( -- * FloPoCo Adder
    floPoCoAdder
  , floPoCoAdderInfo
    -- * Pipeline depth type
  , PipelineDepth
  ) where

import Clash.Explicit.Prelude
import Clash.Annotations.Primitive (Primitive(..), HDL(..))
import Data.String.Interpolate (__i)
import System.IO.Unsafe (unsafePerformIO)

import Clash.Cores.ClashFloPoCo.InfoEn (InfoEntity(..))
import Clash.Cores.ClashFloPoCo.GenTemDSL (getPipeDep)
import Clash.Cores.ClashFloPoCo.MPFR (mpfrAdd)

-- | Information entity for the FloPoCo floating point adder
--
-- This defines:
--
-- * Component name: "floPoCoAdder"
-- * Target frequency: 100 MHz
-- * Pipeline depth: 2 stages
-- * Input signals: clock, operand X, operand Y
-- * Output signal: result R
floPoCoAdderInfo :: InfoEntity
floPoCoAdderInfo = InfoEntity
  { name = Just "floPoCoAdder"
  , freq = Just 100
  , pipedep = Just 2
  , insig = Just ["clk", "X", "Y"]
  , outsig = Just ["R"]
  }

-- | Type-level natural representing the pipeline depth
type PipelineDepth = $(getPipeDep floPoCoAdderInfo)

-- | Pipeline depth value for delay matching
pipelineDepth :: SNat PipelineDepth
pipelineDepth = SNat

-- | Reference implementation using MPFR for simulation
--
-- This provides bit-accurate floating point addition for simulation.
-- In hardware synthesis, this will be replaced by the FloPoCo IP core.
mpfrAddFloat :: Float -> Float -> Float
mpfrAddFloat a b = realToFrac $ unsafePerformIO $ 
  mpfrAdd 24 (realToFrac a) (realToFrac b)

-- | Hardware-synthesizable floating point adder using FloPoCo
--
-- This function adds two single-precision floating point numbers using
-- a pipelined FloPoCo IP core. The result is delayed by 'PipelineDepth'
-- cycles to match the hardware pipeline.
--
-- ==== __Parameters__
--
-- [@clk@] Clock signal for the FloPoCo core
-- [@a@] First operand (delayed signal)
-- [@b@] Second operand (delayed signal)
--
-- ==== __Returns__
--
-- The sum of @a@ and @b@, delayed by @PipelineDepth@ cycles.
--
-- ==== __Example__
--
-- @
-- topEntity :: Clock System -> Signal System Float -> Signal System Float -> Signal System Float
-- topEntity clk a b = 
--   let a' = toSignal (fromSignal a :: DSignal System 0 Float)
--       b' = toSignal (fromSignal b :: DSignal System 0 Float) 
--   in toSignal (floPoCoAdder clk a' b')
-- @
floPoCoAdder
  :: forall n dom
   . KnownDomain dom
  => Clock dom
  -> DSignal dom n Float
  -> DSignal dom n Float
  -> DSignal dom (n + PipelineDepth) Float
floPoCoAdder clk a b =
  delayN pipelineDepth undefined enableGen clk (liftA2 mpfrAddFloat a b)
{-# OPAQUE floPoCoAdder #-}

-- NOTE: BlackBox generation via Template Haskell (genBlackBoxProd) is currently
-- under development. For now, this example demonstrates the simulation model.
-- For HDL generation, see FloPoCoExample.hs for the manual blackbox approach.
--
-- $(genBlackBoxProd floPoCoAdderInfo)
--
-- {-# ANN floPoCoAdder (
--   let primName = 'floPoCoAdder
--       bbfName = 'floPoCoAdderBBF
--   in InlineYamlPrimitive [minBound..] [__i|
--     BlackBoxHaskell:
--       name: #{primName}
--       templateFunction: #{bbfName}
--       workInfo: Always
--   |]) #-}
