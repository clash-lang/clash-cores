{- |
  Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@proton.me>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  5b/6b encoding and decoding functions
-}
module Clash.Cores.LineCoding.Lc5b6b where

import Clash.Prelude

import qualified Clash.Cores.LineCoding.Lc5b6b.Decoder as Dec
import qualified Clash.Cores.LineCoding.Lc5b6b.Encoder as Enc

decode5b6b ::
  Bool ->
  BitVector 6 ->
  Maybe (Bool, BitVector 5)
decode5b6b rd cg = case $(listToVecTH Dec.decoderLut) !! (pack rd ++# cg) of
  Just (rdChange, bv) -> Just (if rdChange then not rd else rd, bv)
  Nothing -> Nothing
{-# OPAQUE decode5b6b #-}

encode5b6b ::
  Bool ->
  BitVector 5 ->
  Maybe (Bool, BitVector 6)
encode5b6b rd bv = $(listToVecTH Enc.encoderLut) !! (pack rd ++# bv)
{-# OPAQUE encode5b6b #-}
