{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

5b/6b encoding and decoding functions
-}
module Clash.Cores.LineCoding.Lc3b4b where

import qualified Clash.Cores.LineCoding.Lc3b4b.Decoder as Dec
import qualified Clash.Cores.LineCoding.Lc3b4b.Encoder as Enc
import Clash.Prelude

decode3b4b ::
  Bool ->
  Bool ->
  BitVector 4 ->
  Maybe (Bool, BitVector 3)
decode3b4b cw rd cg =
  case $(listToVecTH Dec.decoderLut) !! (pack cw ++# pack rd ++# cg) of
    Just (rdChange, bv) -> Just (if rdChange then not rd else rd, bv)
    Nothing -> Nothing
{-# OPAQUE decode3b4b #-}

encode3b4b ::
  Bool ->
  Bool ->
  Bool ->
  BitVector 3 ->
  Maybe (Bool, BitVector 4)
encode3b4b cw alt rd bv =
  $(listToVecTH Enc.encoderLut) !! (pack cw ++# pack alt ++# pack rd ++# bv)
{-# OPAQUE encode3b4b #-}
