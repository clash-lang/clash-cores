{- |
  Copyright   :  (C) 2024-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The former table-based 8b/10b encoder and decoder, kept as reference for
  the tests of "Clash.Cores.LineCoding.Lc8b10b". The encoder table was
  validated in hardware as part of the SGMII core. The decoder table is known
  to accept some code groups that no encoder produces.
-}
module Test.Cores.LineCoding.Lc8b10b.Reference where

import Clash.Prelude
import qualified Test.Cores.LineCoding.Lc8b10b.Reference.Decoder as Dec
import qualified Test.Cores.LineCoding.Lc8b10b.Reference.Encoder as Enc

-- | Reference encoder. Takes the control word flag, the running disparity and
--   the value, and returns whether the input is an undefined control word, the
--   new running disparity and the code group.
referenceEncode :: Bool -> Bool -> BitVector 8 -> (Bool, Bool, BitVector 10)
referenceEncode cw rd w =
  unpack
    $ asyncRomBlobPow2 $(memBlobTH Nothing Enc.encoderLut)
    $ unpack (pack cw ++# pack rd ++# w)

-- | Reference decoder. Takes the running disparity and the code group, and
--   returns whether there is a disparity error, whether there is a code error,
--   whether the code group is a control word, the new running disparity and the
--   decoded value.
referenceDecode :: Bool -> BitVector 10 -> (Bool, Bool, Bool, Bool, BitVector 8)
referenceDecode rd cg =
  unpack
    $ asyncRomBlobPow2 $(memBlobTH Nothing Dec.decoderLut)
    $ unpack (pack rd ++# cg)
