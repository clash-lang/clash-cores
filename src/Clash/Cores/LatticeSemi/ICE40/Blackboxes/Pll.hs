{-|
  Copyright   :  (C) 2026, Felix Klein
  License     :  CERN-OHL-P-2.0
  Maintainer  :  felix@qbaylogic.com

Blackbox implementation for the Lattice ICE40 PLLs.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.LatticeSemi.ICE40.Blackboxes.Pll where

import Prelude

import Clash.Backend (Backend)
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)
import Clash.Signal.Internal (ResetPolarity(..), periodToHz)

import Control.Arrow (second)
import Control.Monad.State (State)

import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Text.Show.Pretty (ppShow)

import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

import Clash.Cores.LatticeSemi.ICE40.Internal.Pll

-- | Blackbox implementation for
-- 'Clash.Cores.LatticeSemi.ICE40.Pll.ice40Corepll'.
ice40pllCoreTF :: TemplateFunction
ice40pllCoreTF = TemplateFunction [0,1,2,3] (const True) (ice40pllTF# False)

-- | Blackbox implementation for
-- 'Clash.Cores.LatticeSemi.ICE40.Pll.ice40Padpll'.
ice40pllPadTF :: TemplateFunction
ice40pllPadTF = TemplateFunction [0,1,2,3] (const True) (ice40pllTF# True)

-- | Generates HDL for the SB_PLL40_(CORE/PAD) primitives supported by
-- ICE40 FPGAs.
ice40pllTF# :: Backend b => Bool -> BlackBoxContext -> State b Doc
ice40pllTF# isPad bbCtx
  | [ (_, N.Void (Just  dIn@N.KnownDomain{}))
    , (_, N.Void (Just dOut@N.KnownDomain{}))
    , (srcClk, _)
    , (srcRst, _)
    ] <- DSL.tInputs bbCtx
  , [ results ] <- DSL.tResults bbCtx
  , N.KnownDomain _ pIn  _ _ _ rstPolarity <- dIn
  , N.KnownDomain _ pOut _ _ _ _ <- dOut
  , let componentName =
          if isPad then "SB_PLL40_PAD" :: Text else "SB_PLL40_CORE"
        iFreq, oFreq :: Rational
        iFreq = periodToHz (fromInteger pIn ) / 1e6
        oFreq = periodToHz (fromInteger pOut) / 1e6
        pllType = if isPad then "Pad" else "Core"
  = case calcPllParams iFreq oFreq of
      Left err -> error $ "Blackbox Error (ice40pll" <> pllType <> "): " <> err
      Right PllParams{..} -> do
        instanceName <- Id.make $ componentName <> "_inst"
        DSL.declaration (componentName <> "_block") $ do
          (dstClk, locked) <-
            DSL.untuple results ["pll_clk_out", "pll_lock_out"] >>= \case
              [a, b] -> pure (a, b)
              _ -> error $ ppShow bbCtx

          cLow <- DSL.assign "pll_cLow"  DSL.Low
          cReset <- case rstPolarity of
            ActiveLow  -> return srcRst
            ActiveHigh -> DSL.notExpr "activeLowRst" srcRst

          let
            generics :: [(Text, DSL.TExpr)]
            generics = second DSL.litTExpr <$>
              [ ("FEEDBACK_PATH",          "SIMPLE")
              , ("PLLOUT_SELECT",          "GENCLK")
              , ("DIVR",            DSL.I refClkDiv)
              , ("DIVF",          DSL.I feedbackDiv)
              , ("DIVQ",               DSL.I vcoDiv)
              , ("FILTER_RANGE",  DSL.I filterRange)
              ]

            inPorts :: [(Text, DSL.TExpr)]
            inPorts =
              [ ("RESETB",                                       cReset)
              , ("BYPASS",                                         cLow)
              , (if isPad then "PACKAGEPIN" else "REFERENCECLK", srcClk)
              ]

            outPorts :: [(Text, DSL.TExpr)]
            outPorts =
              [ ("PLLOUTCORE",   dstClk)
              , ("LOCK", locked)
              ]

          DSL.instDecl
            N.Empty
            (Id.unsafeMake componentName)
            instanceName
            generics
            inPorts
            outPorts

ice40pllTF# isPad bbCtx =
  error $ "Blackbox Error (ice40pll" <> pllType <> "): " <> ppShow bbCtx
 where
  pllType = if isPad then "Pad" else "Core"
