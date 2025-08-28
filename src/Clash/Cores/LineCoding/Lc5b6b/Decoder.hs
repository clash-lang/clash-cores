{- |
 Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@proton.me>
 License     :  BSD2 (see the file LICENSE)
 Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

 5b/6b decoding look-up table
-}
module Clash.Cores.LineCoding.Lc5b6b.Decoder where

import Clash.Prelude
import qualified Clash.Sized.Internal.BitVector as BV
import qualified Prelude as P

decoderLut :: [Maybe (Bool, BitVector 5)]
decoderLut = P.map ((`P.lookup` decoderLutList) P.. unpack . BV.toEnum#) [0 .. 127]

decoderLutList :: [((Bool, BitVector 6), (Bool, BitVector 5))]
decoderLutList =
  [ ((False, 0b100111), (True, 0b00000)) -- D.00
  , ((True, 0b011000), (True, 0b00000)) -- D.00
  , ((False, 0b011101), (True, 0b00001)) -- D.01
  , ((True, 0b100010), (True, 0b00001)) -- D.01
  , ((False, 0b101101), (True, 0b00010)) -- D.02
  , ((True, 0b010010), (True, 0b00010)) -- D.02
  , ((False, 0b110001), (False, 0b00011)) -- D.03
  , ((True, 0b110001), (False, 0b00011)) -- D.03
  , ((False, 0b110101), (True, 0b00100)) -- D.04
  , ((True, 0b001010), (True, 0b00100)) -- D.04
  , ((False, 0b101001), (False, 0b00101)) -- D.05
  , ((True, 0b101001), (False, 0b00101)) -- D.05
  , ((False, 0b011001), (False, 0b00110)) -- D.06
  , ((True, 0b011001), (False, 0b00110)) -- D.06
  , ((False, 0b111000), (True, 0b00111)) -- D.07
  , ((True, 0b000111), (True, 0b00111)) -- D.07
  , ((False, 0b111001), (True, 0b01000)) -- D.08
  , ((True, 0b000110), (True, 0b01000)) -- D.08
  , ((False, 0b100101), (False, 0b01001)) -- D.09
  , ((True, 0b100101), (False, 0b01001)) -- D.09
  , ((False, 0b010101), (False, 0b01010)) -- D.10
  , ((True, 0b010101), (False, 0b01010)) -- D.10
  , ((False, 0b110100), (False, 0b01011)) -- D.11
  , ((True, 0b110100), (False, 0b01011)) -- D.11
  , ((False, 0b001101), (False, 0b01100)) -- D.12
  , ((True, 0b001101), (False, 0b01100)) -- D.12
  , ((False, 0b101100), (False, 0b01101)) -- D.13
  , ((True, 0b101100), (False, 0b01101)) -- D.13
  , ((False, 0b011100), (False, 0b01110)) -- D.14
  , ((True, 0b011100), (False, 0b01110)) -- D.14
  , ((False, 0b010111), (True, 0b01111)) -- D.15
  , ((True, 0b101000), (True, 0b01111)) -- D.15
  , ((False, 0b011011), (True, 0b10000)) -- D.16
  , ((True, 0b100100), (True, 0b10000)) -- D.16
  , ((False, 0b100011), (False, 0b10001)) -- D.17
  , ((True, 0b100011), (False, 0b10001)) -- D.17
  , ((False, 0b010011), (False, 0b10010)) -- D.18
  , ((True, 0b010011), (False, 0b10010)) -- D.18
  , ((False, 0b110010), (False, 0b10011)) -- D.19
  , ((True, 0b110010), (False, 0b10011)) -- D.19
  , ((False, 0b001011), (False, 0b10100)) -- D.20
  , ((True, 0b001011), (False, 0b10100)) -- D.20
  , ((False, 0b101010), (False, 0b10101)) -- D.21
  , ((True, 0b101010), (False, 0b10101)) -- D.21
  , ((False, 0b011010), (False, 0b10110)) -- D.22
  , ((True, 0b011010), (False, 0b10110)) -- D.22
  , ((False, 0b111010), (True, 0b10111)) -- D.23, K.23
  , ((True, 0b000101), (True, 0b10111)) -- D.23, K.23
  , ((False, 0b110011), (True, 0b11000)) -- D.24
  , ((True, 0b001100), (True, 0b11000)) -- D.24
  , ((False, 0b100110), (False, 0b11001)) -- D.25
  , ((True, 0b100110), (False, 0b11001)) -- D.25
  , ((False, 0b010110), (False, 0b11010)) -- D.26
  , ((True, 0b010110), (False, 0b11010)) -- D.26
  , ((False, 0b110110), (True, 0b11011)) -- D.27, K.27
  , ((True, 0b001001), (True, 0b11011)) -- D.27, K.27
  , ((False, 0b001110), (False, 0b11100)) -- D.28
  , ((True, 0b001110), (False, 0b11100)) -- D.28
  , ((False, 0b101110), (True, 0b11101)) -- D.29, K.29
  , ((True, 0b010001), (True, 0b11101)) -- D.29, K.29
  , ((False, 0b011110), (True, 0b11110)) -- D.30, K.30
  , ((True, 0b100001), (True, 0b11110)) -- D.30, K.30
  , ((False, 0b101011), (True, 0b11111)) -- D.31
  , ((True, 0b010100), (True, 0b11111)) -- D.31
  , ((False, 0b001111), (True, 0b11100)) -- K.28
  , ((True, 0b110000), (True, 0b11100)) -- K.28
  ]
