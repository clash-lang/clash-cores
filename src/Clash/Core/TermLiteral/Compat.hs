{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | This module provides a way to work around:
-- https://github.com/clash-lang/clash-compiler/issues/2926
module Clash.Core.TermLiteral.Compat
  ( termToData
  , termToDataError
  ) where

import Prelude
import Clash.Core.Term
import Clash.Core.TermLiteral (TermLiteral)
import GHC.Stack (HasCallStack)

import qualified Clash.Core.TermLiteral as TermLiteral

termToDataError :: forall a. TermLiteral a => Term -> Either String a
termToDataError = TermLiteral.termToDataError . stripAllTicks

termToData :: forall a. HasCallStack => TermLiteral a => Term -> Either Term a
termToData = TermLiteral.termToData . stripAllTicks
