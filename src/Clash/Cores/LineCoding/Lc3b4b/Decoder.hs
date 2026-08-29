{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

3b/4b decoding look-up table
-}
module Clash.Cores.LineCoding.Lc3b4b.Decoder where

import Clash.Prelude
import qualified Clash.Sized.Internal.BitVector as BV
import qualified Prelude as P

decoderLut :: [Maybe (Bool, BitVector 3)]
decoderLut =
  P.map ((`P.lookup` decoderLutList) P.. unpack . BV.toEnum#) [0 .. 63]

decoderLutList :: [((Bool, Bool, BitVector 4), (Bool, BitVector 3))]
decoderLutList =
  [ ((False, False, 0b1011), (True, 0b000)) -- D.x.0
  , ((False, True, 0b0100), (True, 0b000)) -- D.x.0
  , ((False, False, 0b1001), (False, 0b001)) -- D.x.1
  , ((False, True, 0b1001), (False, 0b001)) -- D.x.1
  , ((False, False, 0b0101), (False, 0b010)) -- D.x.2
  , ((False, True, 0b0101), (False, 0b010)) -- D.x.2
  , ((False, False, 0b1100), (True, 0b011)) -- D.x.3
  , ((False, True, 0b0011), (True, 0b011)) -- D.x.3
  , ((False, False, 0b1101), (True, 0b100)) -- D.x.4
  , ((False, True, 0b0010), (True, 0b100)) -- D.x.4
  , ((False, False, 0b1010), (False, 0b101)) -- D.x.5
  , ((False, True, 0b1010), (False, 0b101)) -- D.x.5
  , ((False, False, 0b0110), (False, 0b110)) -- D.x.6
  , ((False, True, 0b0110), (False, 0b110)) -- D.x.6
  , ((False, False, 0b1110), (True, 0b111)) -- D.x.P7
  , ((False, True, 0b0001), (True, 0b111)) -- D.x.P7
  , ((False, False, 0b0111), (True, 0b111)) -- D.x.A7
  , ((False, True, 0b1000), (True, 0b111)) -- D.x.A7
  , ((True, False, 0b1011), (True, 0b000)) -- K.x.0
  , ((True, True, 0b0100), (True, 0b000)) -- K.x.0
  , ((True, False, 0b0110), (True, 0b001)) -- K.x.1
  , ((True, True, 0b1001), (True, 0b001)) -- K.x.1
  , ((True, False, 0b1010), (True, 0b010)) -- K.x.2
  , ((True, True, 0b0101), (True, 0b010)) -- K.x.2
  , ((True, False, 0b1100), (True, 0b011)) -- K.x.3
  , ((True, True, 0b0011), (True, 0b011)) -- K.x.3
  , ((True, False, 0b1101), (True, 0b100)) -- K.x.4
  , ((True, True, 0b0010), (True, 0b100)) -- K.x.4
  , ((True, False, 0b0101), (True, 0b101)) -- K.x.5
  , ((True, True, 0b1010), (True, 0b101)) -- K.x.5
  , ((True, False, 0b1001), (True, 0b110)) -- K.x.6
  , ((True, True, 0b0110), (True, 0b110)) -- K.x.6
  , ((True, False, 0b0111), (True, 0b111)) -- K.x.7
  , ((True, True, 0b1000), (True, 0b111)) -- K.x.7
  ]
