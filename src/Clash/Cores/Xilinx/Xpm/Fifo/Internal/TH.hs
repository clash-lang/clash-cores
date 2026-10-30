{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Template Haskell helpers for "Clash.Cores.Xilinx.Xpm.Fifo.Internal".
-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Internal.TH
  ( hexStringTree
  ) where

import Prelude

import Data.Bits (bit, (.|.))
import Language.Haskell.TH (ExpQ, newName, lamE, varP, condE, varE, litE, stringL)
import Numeric (showHex)

-- | Generate a function taking a number of 'Bool's and returning the
-- hexadecimal representation of the number they form, padded to the given
-- number of digits. The /n/th argument sets the bit at the /n/th position in the
-- given list. The generated function is a tree of @if@s with literal strings as
-- leaves. Unlike a function computing the string, Clash can reduce applications
-- of this function with constant arguments to a string literal, which is what it
-- needs to render a string parameter.
hexStringTree ::
  -- | Number of hexadecimal digits
  Int ->
  -- | Bit position set by each argument
  [Int] ->
  ExpQ
hexStringTree nDigits positions = do
  names <- mapM (\i -> newName ("b" <> show i)) positions
  lamE (map varP names) (go (zip names positions) (0 :: Integer))
 where
  go [] acc = litE (stringL (pad (showHex acc "")))
  go ((b, i):bs) acc = condE (varE b) (go bs (acc .|. bit i)) (go bs acc)

  pad s = replicate (nDigits - length s) '0' <> s
