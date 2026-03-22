{-|
  Copyright   :  (C) 2026, Felix Klein
  License     :  CERN-OHL-P-2.0
  Maintainer  :  felix@qbaylogic.com

Blackbox implementation for the Lattice ECP5 PLLs.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.LatticeSemi.ECP5.Blackboxes.Pll where

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

import Clash.Cores.LatticeSemi.ECP5.Internal.Pll

-- | Blackbox implementation for 'Clash.Cores.LatticeSemi.ECP5.Pll.ecp5pll'.
ecp5pllTF :: TemplateFunction
ecp5pllTF = TemplateFunction [0,1,2,3] (const True) ecp5pllTF#

-- | Generates HDL for the EHXPLLL primitive supported by ECP5 FPGAs.
ecp5pllTF# :: Backend b => BlackBoxContext -> State b Doc
ecp5pllTF# bbCtx
  | [ (_, N.Void (Just  dIn@N.KnownDomain{}))
    , (_, N.Void (Just dOut@N.KnownDomain{}))
    , (srcClk, _)
    , (srcRst, _)
    ] <- DSL.tInputs bbCtx
  , [ results ] <- DSL.tResults bbCtx
  , N.KnownDomain _ pIn  _ _ _ rstPolarity <- dIn
  , N.KnownDomain _ pOut _ _ _ _ <- dOut
  , let componentName = "EHXPLLL" :: Text
        iFreq, oFreq :: Rational
        iFreq = periodToHz (fromInteger pIn ) / 1e6
        oFreq = periodToHz (fromInteger pOut) / 1e6
  = case calcPllParams iFreq oFreq of
      Left err -> error $ "Blackbox Error (ecp5pll): " <> err
      Right PllParams{..} -> do
        -- shift the primary by 180 degrees; Lattice also seems to do this
        let primaryCPhase = outputDiv `div` 2

        instanceName <- Id.make $ componentName <> "_inst"
        DSL.declaration (componentName <> "_block") $ do
          (dstClk, locked) <-
            DSL.untuple results ["pll_clk_out", "pll_lock_out"] >>= \case
              [a, b] -> pure (a, b)
              _ -> error $ ppShow bbCtx

          cLow  <- DSL.assign "pll_cLow"  DSL.Low
          cHigh <- DSL.assign "pll_cHigh" DSL.High
          cReset <- case rstPolarity of
            ActiveLow  -> DSL.notExpr "activeHighRst" srcRst
            ActiveHigh -> return srcRst

          let
            generics :: [(Text, DSL.TExpr)]
            generics = second DSL.litTExpr <$>
              [ ("PLLRST_ENA",               "DISABLED")
              , ("INTFB_WAKE",               "DISABLED")
              , ("STDBY_ENABLE",             "DISABLED")
              , ("DPHASE_SOURCE",            "DISABLED")
              , ("OUTDIVIDER_MUXA",              "DIVA")
              , ("OUTDIVIDER_MUXB",              "DIVB")
              , ("OUTDIVIDER_MUXC",              "DIVC")
              , ("OUTDIVIDER_MUXD",              "DIVD")
              , ("CLKI_DIV",             DSL.I inputDiv)
              , ("CLKOP_ENABLE",              "ENABLED")
              , ("CLKOP_DIV",           DSL.I outputDiv)
              , ("CLKOP_CPHASE",    DSL.I primaryCPhase)
              , ("CLKOP_FPHASE",                      0)
              , ("FEEDBK_PATH",                 "CLKOP")
              , ("CLKFB_DIV",         DSL.I feedbackDiv)
              ]

            inPorts :: [(Text, DSL.TExpr)]
            inPorts =
              [ ("CLKI",         srcClk)
              , ("CLKFB",        dstClk)
              , ("PHASESEL0",      cLow)
              , ("PHASESEL1",      cLow)
              , ("PHASEDIR",      cHigh)
              , ("PHASESTEP",     cHigh)
              , ("PHASELOADREG",  cHigh)
              , ("STDBY",          cLow)
              , ("PLLWAKESYNC",    cLow)
              , ("RST",          cReset)
              , ("ENCLKOP",        cLow)
              ]

            outPorts :: [(Text, DSL.TExpr)]
            outPorts =
              [ ("CLKOP", dstClk)
              , ("LOCK",  locked)
              ]

          DSL.instDecl
            N.Empty
            (Id.unsafeMake componentName)
            instanceName
            generics
            inPorts
            outPorts

ecp5pllTF# bbCtx = error $ "Blackbox Error (ecp5pll): " <> ppShow bbCtx
