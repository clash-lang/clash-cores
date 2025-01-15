{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{- |
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
  This module contains example functions to work use FloPoCo with Clash in 
  the Single precision floatin point. These functions only serve the illustration purpose.
  Users can look at these functions in the source file and re-name them for custom usage. 
  Note that these functions are not exported. 
  
-}
module Clash.Cores.ClashFloPoCo.FloPoCoExample  where
import Clash.Explicit.Prelude
import Data.String.Interpolate (__i)
import Clash.Annotations.Primitive (Primitive(..), HDL(..))
import Control.Monad.State (State)
import qualified Data.List as L
import Data.Text
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(..), emptyBlackBoxMeta)
import Clash.Netlist.Types
  (BlackBox (..), BlackBoxContext, EntityOrComponent(..), TemplateFunction(..), bbResults)
import qualified Clash.Netlist.Types as NT
import qualified Clash.Primitives.DSL as DSL
import Clash.Cores.ClashFloPoCo.InfoEn
import qualified Clash.Netlist.Id as Id
import Clash.Cores.ClashFloPoCo.Help
    ( infoEnPlusExample,
      infoEnFMAExample,
      infoEnExpExample,
      infoEnVGAControllerExample )
import Clash.Cores.ClashFloPoCo.GenTemDSL(getPipeDep, flopocoPrim, genBlackBoxFunction, genTemplateFunction, genBlackBoxTemplateFunction, genBlackBoxTemplateFunctionProd,genBlackBox, genBlackBoxProd)
import Clash.Primitives.Types (Primitive(BlackBoxHaskell, workInfo))
import Text.Show.Pretty(ppShow)
import qualified Data.Text as Text
import Clash.Netlist.BlackBox.Util (bbResult)



-- | The Nplus is the Nat number to use as the pipeline depth of the plusFloatExample function
type Nplus = $(getPipeDep infoEnPlusExample)
-- | The xp serves as the delay parameter to use for delayN function used internally
-- inside of the plusFloatExample function
xp :: SNat Nplus
xp = SNat::SNat Nplus
-- | Example of the hardware synthesizable function which adds 2 single precision floating point 
--
-- ==== __Example:__
--
{-|
This is the example implementation of the plusFloatExample which uses the old way by
binding it with BlackBoxFunction, BlackBoxTemplateFunction and TemplateFunction to make it
hardware synthesizable.

@
import Clash.Explicit.Prelude
import Data.String.Interpolate (__i)
import Clash.Annotations.Primitive (Primitive(..), HDL(..))
import Control.Monad.State (State)
import qualified Data.List as L
import Data.Text
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(..), emptyBlackBoxMeta)
import Clash.Netlist.Types
  (BlackBox (..), BlackBoxContext, EntityOrComponent(..), TemplateFunction(..), bbResults)
import qualified Clash.Netlist.Types as NT
import qualified Clash.Primitives.DSL as DSL
import Clash.Cores.ClashFloPoCo.InfoEn
import qualified Clash.Netlist.Id as Id
import Clash.Cores.ClashFloPoCo.Help
    ( infoEnPlusExample,
      infoEnFMAExample,
      infoEnExpExample,
      infoEnVGAControllerExample )
import Clash.Cores.ClashFloPoCo.GenTemDSL(getPipeDep, flopocoPrim, genBlackBoxFunction, genTemplateFunction, genBlackBoxTemplateFunction, genBlackBoxTemplateFunctionProd,genBlackBox, genBlackBoxProd)
import Clash.Primitives.Types (Primitive(BlackBoxHaskell, workInfo))
import Text.Show.Pretty(ppShow)
import qualified Data.Text as Text
import Clash.Netlist.BlackBox.Util (bbResult)

type Nplus = $(getPipeDep infoEnPlusExample)
xp :: SNat Nplus
xp = SNat::SNat Nplus

plusFloatExample
  :: forall n . 
  Clock XilinxSystem
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem (n + Nplus) Float
plusFloatExample clk a b =
  delayN xp undefined enableGen clk (liftA2 (+) a b)
{-# OPAQUE plusFloatExample #-}

{-# ANN plusFloatExample (
    let
      primName = show 'plusFloatExample
      tfName = show 'plusFloatBBF
    in
      InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
      |]) #-}

plusFloatBBTF ::
  forall s .
  Backend s =>
  Text ->
  BlackBoxContext ->
  State s Doc
plusFloatBBTF entityName bbCtx
  | [ clk, a, b
    ] <- L.map fst (DSL.tInputs bbCtx)
  , [result] <- DSL.tResults bbCtx
  = do
    plusFloatInstName <- Id.makeBasic "plusFloat_inst"
    let
      compInps =
        [ ("clk", N.Bit)
        , ("X", DSL.ety a)
        , ("Y", DSL.ety b) ]
      compOuts =
        [ ("R", DSL.ety result) ]
    DSL.declaration "plusFloat_inst_block" $ do
      DSL.compInBlock entityName compInps compOuts
      let
        inps =
          [ ("clk", clk )
          , ("X", a)
          , ("Y", b)
          ]

        outs =
          [ ("R", result)
          ]
      DSL.instDecl Empty (Id.unsafeMake entityName) plusFloatInstName
        [] inps outs
  | otherwise = error $ ppShow bbCtx


plusFloatTF ::
  HasCallStack =>
  Text ->
  TemplateFunction
plusFloatTF entityName =
  TemplateFunction
    [0..2]
    (const True)
    (plusFloatBBTF entityName)

plusFloatBBF :: BlackBoxFunction
plusFloatBBF _ _ _ _ = do
  pure (Right ((emptyBlackBoxMeta {bbKind = TDecl}), (BBFunction ("plusFloatTF") 0 (plusFloatTF entityName))))
@
-}
plusFloatExample
  :: forall n . 
  -- | Clock signal
  Clock XilinxSystem 
  -- | Operand input signal
  -> DSignal XilinxSystem n Float 
  -- | Operand input signal
  -> DSignal XilinxSystem n Float 
  -- | Result output signal
  -> DSignal XilinxSystem (n + Nplus) Float
plusFloatExample clk a b =
  delayN xp undefined enableGen clk (liftA2 (+) a b)
{-# OPAQUE plusFloatExample #-}
$(genBlackBox infoEnPlusExample)
{-# ANN plusFloatExample (
    let
      primName = show 'plusFloatExample
      tfName = show 'plusFloatBBF
    in
      InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
      |]) #-}
-- |The Nplus is the Nat number to use as the pipeline depth of the plusFloatExample function
type Nfma = $(getPipeDep infoEnFMAExample)
-- | The xp2 serves as the delay parameter to use for delayN function used internally
-- inside of the fmaFloatExample function
xp2 :: SNat Nfma
xp2 = SNat::SNat Nfma
-- | Example of the hardware synthesizable function which fuse multiply-adds 3 single precision floating point
--  This function computes a * b + c
--
-- ==== __Example:__
--
{-|
This is the example implementation of fmaFloatExample using buit-in template haskell 
to auto generate BlackBoxFunction, BlackBoxTemplateFunction and TemplateFunction during
the compile time. It also uses custom built-in primitive function for the pragma
annotation. 

@
import Clash.Explicit.Prelude
import Data.String.Interpolate (__i)
import Clash.Annotations.Primitive (Primitive(..), HDL(..))
import Control.Monad.State (State)
import qualified Data.List as L
import Data.Text
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(..), emptyBlackBoxMeta)
import Clash.Netlist.Types
  (BlackBox (..), BlackBoxContext, EntityOrComponent(..), TemplateFunction(..), bbResults)
import qualified Clash.Netlist.Types as NT
import qualified Clash.Primitives.DSL as DSL
import Clash.Cores.ClashFloPoCo.InfoEn
import qualified Clash.Netlist.Id as Id
import Clash.Cores.ClashFloPoCo.Help
    ( infoEnPlusExample,
      infoEnFMAExample,
      infoEnExpExample,
      infoEnVGAControllerExample )
import Clash.Cores.ClashFloPoCo.GenTemDSL(getPipeDep, flopocoPrim, genBlackBoxFunction, genTemplateFunction, genBlackBoxTemplateFunction, genBlackBoxTemplateFunctionProd,genBlackBox, genBlackBoxProd)
import Clash.Primitives.Types (Primitive(BlackBoxHaskell, workInfo))
import Text.Show.Pretty(ppShow)
import qualified Data.Text as Text
import Clash.Netlist.BlackBox.Util (bbResult)

type Nfma = $(getPipeDep infoEnFMAExample)
xp2 :: SNat Nfma
xp2 = SNat::SNat Nfma

$(genBlackBox infoEnFMAExample)
fmaFloatExample
  :: forall n .
  Clock XilinxSystem
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem n Bit
  -> DSignal XilinxSystem n Bit
  -> DSignal XilinxSystem n (BitVector 2)
  -> DSignal XilinxSystem (n + Nfma)  Float
fmaFloatExample clk a b c negab negc _ = 
  let 
    resab = mux (fmap (== 1)  negab)
             (liftA2 (*) (liftA2 (*) a b) (pure (-1)))
             (liftA2 (*) a b)
    resc = mux (fmap (== 1) negc)
             (liftA2 (*) c (pure (-1)))
              c
  in
  delayN xp2 undefined enableGen clk (liftA2 (+) resab resc)
{-# OPAQUE fmaFloatExample #-}
{-# ANN fmaFloatExample (flopocoPrim 'fmaFloatExample 'fmaFloatBBF) #-}
@
-}
fmaFloatExample
  :: forall n .
  -- | Clock signal
  Clock XilinxSystem 
  -- | Operand input signal a
  -> DSignal XilinxSystem n Float 
  -- | Operand input signal b
  -> DSignal XilinxSystem n Float 
  -- | Operand input signal c
  -> DSignal XilinxSystem n Float 
  -- | Flag to check if a or b is negative
  -> DSignal XilinxSystem n Bit  
  -- | Flag to check if c is negative
  -> DSignal XilinxSystem n Bit  
  -- | Unused flag. In FloPoCo, it's roundmode but it has no role in the computation.
  -> DSignal XilinxSystem n (BitVector 2) 
  -- | Result output signal 
  -> DSignal XilinxSystem (n + Nfma) Float 
fmaFloatExample clk a b c negab negc _ = 
  let 
    resab = mux (fmap (== 1)  negab)
             (liftA2 (*) (liftA2 (*) a b) (pure (-1)))
             (liftA2 (*) a b)
    resc = mux (fmap (== 1) negc)
             (liftA2 (*) c (pure (-1)))
              c
  in
  delayN xp2 undefined enableGen clk (liftA2 (+) resab resc)
{-# OPAQUE fmaFloatExample #-}
$(genBlackBox infoEnFMAExample)
{-# ANN fmaFloatExample (flopocoPrim 'fmaFloatExample 'fmaFloatBBF) #-}
-- |The Nexp is the Nat number to use as the pipeline depth of the plusFloatExample function
type Nexp = $(getPipeDep infoEnExpExample)
-- | The xp3 serves as the delay parameter to use for delayN function used internally
-- inside of the expFloatExample function
xp3 :: SNat Nexp
xp3 = SNat::SNat Nexp
-- | Example of the hardware synthesizable function which compute the exponent of the single precision floating point
-- 
-- ==== __Example:__
--
{-|

@
import Clash.Explicit.Prelude
import Data.String.Interpolate (__i)
import Clash.Annotations.Primitive (Primitive(..), HDL(..))
import Control.Monad.State (State)
import qualified Data.List as L
import Data.Text
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(..), emptyBlackBoxMeta)
import Clash.Netlist.Types
  (BlackBox (..), BlackBoxContext, EntityOrComponent(..), TemplateFunction(..), bbResults)
import qualified Clash.Netlist.Types as NT
import qualified Clash.Primitives.DSL as DSL
import Clash.Cores.ClashFloPoCo.InfoEn
import qualified Clash.Netlist.Id as Id
import Clash.Cores.ClashFloPoCo.Help
    ( infoEnPlusExample,
      infoEnFMAExample,
      infoEnExpExample,
      infoEnVGAControllerExample )
import Clash.Cores.ClashFloPoCo.GenTemDSL(getPipeDep, flopocoPrim, genBlackBoxFunction, genTemplateFunction, genBlackBoxTemplateFunction, genBlackBoxTemplateFunctionProd,genBlackBox, genBlackBoxProd)
import Clash.Primitives.Types (Primitive(BlackBoxHaskell, workInfo))
import Text.Show.Pretty(ppShow)
import qualified Data.Text as Text
import Clash.Netlist.BlackBox.Util (bbResult)

type Nexp = $(getPipeDep infoEnExpExample)
xp3 :: SNat Nexp
xp3 = SNat::SNat Nexp

expFloatExample
  :: forall n .
  -- | Clock signal
  Clock XilinxSystem 
  -- | Operand input signal
  -> DSignal XilinxSystem n Float 
  -- | Result output signal 
  -> DSignal XilinxSystem (n + Nexp) Float 
expFloatExample clk a = delayN xp3 undefined enableGen clk (liftA exp a)
{-# OPAQUE expFloatExample #-}
$(genBlackBox infoEnExpExample)
{-# ANN expFloatExample (flopocoPrim 'expFloatExample 'expFloatBBF) #-}

@
-}
expFloatExample
  :: forall n .
  -- | Clock signal
  Clock XilinxSystem 
  -- | Operand input signal
  -> DSignal XilinxSystem n Float 
  -- | Result output signal 
  -> DSignal XilinxSystem (n + Nexp) Float 
expFloatExample clk a = delayN xp3 undefined enableGen clk (liftA exp a)
{-# OPAQUE expFloatExample #-}
$(genBlackBox infoEnExpExample)
{-# ANN expFloatExample (flopocoPrim 'expFloatExample 'expFloatBBF) #-}

-- | This function is not the part of the FloPoCo.
-- It serves as the illustrated purpose of how to use BlackBoxFunction,TemplateFunction, and BlackBoxTemplateFunction
-- when the function has multiple output signals 
--
-- ==== __Example:__
--
{-|
@
import Clash.Explicit.Prelude
import Data.String.Interpolate (__i)
import Clash.Annotations.Primitive (Primitive(..), HDL(..))
import Control.Monad.State (State)
import qualified Data.List as L
import Data.Text
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(..), emptyBlackBoxMeta)
import Clash.Netlist.Types
  (BlackBox (..), BlackBoxContext, EntityOrComponent(..), TemplateFunction(..), bbResults)
import qualified Clash.Netlist.Types as NT
import qualified Clash.Primitives.DSL as DSL
import Clash.Cores.ClashFloPoCo.InfoEn
import qualified Clash.Netlist.Id as Id
import Clash.Cores.ClashFloPoCo.Help
    ( infoEnPlusExample,
      infoEnFMAExample,
      infoEnExpExample,
      infoEnVGAControllerExample )
import Clash.Cores.ClashFloPoCo.GenTemDSL(getPipeDep, flopocoPrim, genBlackBoxFunction, genTemplateFunction, genBlackBoxTemplateFunction, genBlackBoxTemplateFunctionProd,genBlackBox, genBlackBoxProd)
import Clash.Primitives.Types (Primitive(BlackBoxHaskell, workInfo))
import Text.Show.Pretty(ppShow)
import qualified Data.Text as Text
import Clash.Netlist.BlackBox.Util (bbResult)

vga_controller
    :: Clock XilinxSystem ->
    Reset XilinxSystem ->
    ( -- | video_on 
      Signal XilinxSystem Bit 
      -- | Horizontal Sync         
     , Signal XilinxSystem Bit  
     -- | Vertical Sync       
     , Signal XilinxSystem Bit    
     -- | p_tick    
     , Signal XilinxSystem Bit  
     -- | X Position       
     , Signal XilinxSystem (BitVector 10)  
     -- | Y Position
     , Signal XilinxSystem (BitVector 10)  
     )
vga_controller !clk !rst = deepErrorX "vga_controller: simulation output undefined"
{-# OPAQUE vga_controller #-}
$(genBlackBoxProd infoEnVGAController)
{-# ANN vga_controller (let
      primName = show 'vga_controller
      tfName = show 'vga_controllerBBF
    in
      InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
      |]) #-}
@
The part $(genBlackBoxProd infoEnVGAController) is equivalent to

@
$(genBlackBoxTemplateFunctionProd infoEnVGAController)
$(genTemplateFunction infoEnVGAController)
$(genBlackBoxFunction infoEnVGAController)
@

which is equivalent to

@
vga_controllerBBTF ::
      forall s. Backend s => Text -> BlackBoxContext -> State s Doc
vga_controllerBBTF vga_controller bbCtx
      | [clk_100MHz, reset] <- L.map fst (DSL.tInputs bbCtx),
        [result] <- DSL.tResults bbCtx,
        N.Product _ _ resTyps <- DSL.ety result
      = do vga_controllerInstName <- Id.makeBasic "vga_controller_inst"
           let compInps
                 = [("clk_100MHz", DSL.ety clk_100MHz), ("reset", DSL.ety reset)]
               compOuts
                 = L.zip ["video_on", "hsync", "vsync", "p_tick", "x", "y"] resTyps
           (DSL.declarationReturn bbCtx "vga_controller_inst_block"
              $ (do declares <- mapM
                                  (\ (name, typ) -> DSL.declare name typ)
                                  (L.zip ["video_on", "hsync", "vsync", "p_tick", "x", "y"] resTyps)
                    let [video_on, hsync, vsync, p_tick, x, y] = declares
                    let inps = [("clk_100MHz", clk_100MHz), ("reset", reset)]
                        outs
                          = [("video_on", video_on), ("hsync", hsync), ("vsync", vsync),
                             ("p_tick", p_tick), ("x", x), ("y", y)]
                    DSL.compInBlock vga_controller compInps compOuts
                    DSL.instDecl
                      Empty (Id.unsafeMake vga_controller) vga_controllerInstName [] inps
                      outs
                    pure
                      [DSL.constructProduct
                         (DSL.ety result) [video_on, hsync, vsync, p_tick, x, y]]))
      | otherwise = error (ppShow bbCtx)
vga_controllerTF :: HasCallStack => Text -> TemplateFunction
vga_controllerTF entityName
      = TemplateFunction
          [0, 1, 2] (const True) (vga_controllerBBTF entityName)
vga_controllerBBF :: BlackBoxFunction
vga_controllerBBF _ _ _ _
      = pure
          (Right
             (emptyBlackBoxMeta {bbKind = TDecl},
              BBFunction
                "vga_controllerTF" 0 (vga_controllerTF "vga_controller")))
@
-}
vga_controller
    :: Clock XilinxSystem ->
    Reset XilinxSystem ->
    ( -- | video_on 
      Signal XilinxSystem Bit 
      -- | Horizontal Sync         
     , Signal XilinxSystem Bit  
     -- | Vertical Sync       
     , Signal XilinxSystem Bit    
     -- | p_tick    
     , Signal XilinxSystem Bit  
     -- | X Position       
     , Signal XilinxSystem (BitVector 10)  
     -- | Y Position
     , Signal XilinxSystem (BitVector 10)  
     )
vga_controller !clk !rst = deepErrorX "vga_controller: simulation output undefined"
{-# OPAQUE vga_controller #-}
$(genBlackBoxProd infoEnVGAControllerExample)

{-# ANN vga_controller (let
      primName = show 'vga_controller
      tfName = show 'vga_controllerBBF
    in
      InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
      |]) #-}
