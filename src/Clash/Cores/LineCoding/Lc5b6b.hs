{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>,
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

5b/6b encoding and decoding functions: the 5-bit part of the 8b/10b line code
defined in IEEE 802.3 Clause 36. Together with "Clash.Cores.LineCoding.Lc3b4b"
it makes up the 8b/10b code of "Clash.Cores.LineCoding.Lc8b10b".

The running disparity is a 'Bool': 'False' is a negative running disparity,
'True' a positive one. Code groups are written as @abcdei@ with @a@, the first
bit on the line, as the most significant bit.
-}
module Clash.Cores.LineCoding.Lc5b6b where

import qualified Clash.Cores.LineCoding.Lc5b6b.Decoder as Dec
import qualified Clash.Cores.LineCoding.Lc5b6b.Encoder as Enc
import Clash.Prelude

-- | Encode a 5-bit value into a 6-bit code group
encode5b6b ::
  -- | Whether to encode a control word (@K.x@) instead of a data word (@D.x@)
  Bool ->
  -- | Running disparity before the code group
  Bool ->
  -- | Value to encode (@x@ in @D.x@ or @K.x@)
  BitVector 5 ->
  -- | Running disparity after the code group and the code group, or 'Nothing'
  --   for a control word that does not exist (only @K.23@, @K.27@, @K.28@,
  --   @K.29@ and @K.30@ are defined)
  Maybe (Bool, BitVector 6)
encode5b6b cw rd x =
  $(listToVecTH Enc.encoderLut) !! (pack cw ++# pack rd ++# x)
{-# OPAQUE encode5b6b #-}

-- | Decode a 6-bit code group into a 5-bit value
decode5b6b ::
  -- | Running disparity before the code group
  Bool ->
  -- | Code group
  BitVector 6 ->
  -- | Whether the code group is @K.28@, the running disparity after the code
  --   group and the decoded value, or 'Nothing' if the code group is not valid
  --   at this running disparity. The control code groups other than @K.28@
  --   share their 6-bit code group with a data code group and decode as data
  --   here; they are told apart by the 3b/4b part, see
  --   "Clash.Cores.LineCoding.Lc3b4b".
  Maybe (Bool, Bool, BitVector 5)
decode5b6b rd cg = $(listToVecTH Dec.decoderLut) !! (pack rd ++# cg)
{-# OPAQUE decode5b6b #-}
