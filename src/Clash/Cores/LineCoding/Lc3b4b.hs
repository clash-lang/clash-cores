{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>,
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

3b/4b encoding and decoding functions: the 3-bit part of the 8b/10b line code
defined in IEEE 802.3 Clause 36. Together with "Clash.Cores.LineCoding.Lc5b6b"
it makes up the 8b/10b code of "Clash.Cores.LineCoding.Lc8b10b".

The running disparity is a 'Bool': 'False' is a negative running disparity,
'True' a positive one. Code groups are written as @fghj@ with @f@, the first bit
on the line, as the most significant bit.
-}
module Clash.Cores.LineCoding.Lc3b4b where

import qualified Clash.Cores.LineCoding.Lc3b4b.Decoder as Dec
import qualified Clash.Cores.LineCoding.Lc3b4b.Encoder as Enc
import Clash.Prelude

-- | Encode a 3-bit value into a 4-bit code group
encode3b4b ::
  -- | Whether to encode a control word (@K.x.y@) instead of a data word
  --   (@D.x.y@)
  Bool ->
  -- | Whether to use the alternate form @D.x.A7@ instead of the primary form
  --   @D.x.P7@. Only has an effect on data words with value 7; see
  --   'useAlternate7' for when it has to be set.
  Bool ->
  -- | Running disparity before the code group
  Bool ->
  -- | Value to encode (@y@ in @D.x.y@ or @K.x.y@)
  BitVector 3 ->
  -- | Running disparity after the code group and the code group
  (Bool, BitVector 4)
encode3b4b cw alt rd y =
  $(listToVecTH Enc.encoderLut) !! (pack cw ++# pack alt ++# pack rd ++# y)
{-# OPAQUE encode3b4b #-}

-- | Whether a data word with @y = 7@ has to be encoded with the alternate form
-- @D.x.A7@. This depends on the 5-bit value @x@ and the running disparity between
-- the 5b/6b and the 3b/4b code group: the alternate form is used for @x@ in
-- {17, 18, 20} when that running disparity is negative and for @x@ in
-- {11, 13, 14} when it is positive. It prevents runs of five identical bits and
-- false comma sequences (IEEE 802.3 36.2.4.6).
useAlternate7 ::
  -- | Running disparity between the 5b/6b and the 3b/4b code group
  Bool ->
  -- | The 5-bit value @x@
  BitVector 5 ->
  Bool
useAlternate7 rd x
  | rd = x == 11 || x == 13 || x == 14
  | otherwise = x == 17 || x == 18 || x == 20

-- | Decode a 4-bit code group into a 3-bit value
decode3b4b ::
  -- | Whether the code group belongs to a control word. The decoder cannot
  --   derive this from the 4-bit code group alone, as data and control code
  --   groups overlap; a caller determines it from the 5b/6b part (@K.28@) or,
  --   for @K.23.7@, @K.27.7@, @K.29.7@ and @K.30.7@, from the code group being
  --   an alternate form where a data word would have used the primary form
  --   (see 'useAlternate7').
  Bool ->
  -- | Running disparity before the code group
  Bool ->
  -- | Code group
  BitVector 4 ->
  -- | Running disparity after the code group and the decoded value, or
  --   'Nothing' if the code group is not valid for this control word flag and
  --   running disparity
  Maybe (Bool, BitVector 3)
decode3b4b cw rd cg =
  $(listToVecTH Dec.decoderLut) !! (pack cw ++# pack rd ++# cg)
{-# OPAQUE decode3b4b #-}

-- | The alternate form @D.x.A7@ of the data code group with value 7, for a
--   running disparity
alternate7 :: Bool -> BitVector 4
alternate7 rd = if rd then snd Enc.alternate7 else fst Enc.alternate7
