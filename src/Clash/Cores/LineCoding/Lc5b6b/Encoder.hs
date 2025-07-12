{- |
  Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@proton.me>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  5b/6b encoding look-up table
-}
module Clash.Cores.LineCoding.Lc5b6b.Encoder where

import Clash.Prelude

encoderLut :: [Maybe (Bit, BitVector 6)]
encoderLut =
  [ Just (1, 0b100111) -- D.00
  , Just (1, 0b011101) -- D.01
  , Just (1, 0b101101) -- D.02
  , Just (0, 0b110001) -- D.03
  , Just (1, 0b110101) -- D.04
  , Just (0, 0b101001) -- D.05
  , Just (0, 0b011001) -- D.06
  , Just (1, 0b111000) -- D.07
  , Just (1, 0b111001) -- D.08
  , Just (0, 0b100101) -- D.09
  , Just (0, 0b010101) -- D.10
  , Just (0, 0b110100) -- D.11
  , Just (0, 0b001101) -- D.12
  , Just (0, 0b101100) -- D.13
  , Just (0, 0b011100) -- D.14
  , Just (1, 0b010111) -- D.15
  , Just (1, 0b011011) -- D.16
  , Just (0, 0b100011) -- D.17
  , Just (0, 0b010011) -- D.18
  , Just (0, 0b110010) -- D.19
  , Just (0, 0b001011) -- D.20
  , Just (0, 0b101010) -- D.21
  , Just (0, 0b011010) -- D.22
  , Just (1, 0b111010) -- D.23
  , Just (1, 0b110011) -- D.24
  , Just (0, 0b100110) -- D.25
  , Just (0, 0b010110) -- D.26
  , Just (1, 0b110110) -- D.27
  , Just (0, 0b001110) -- D.28
  , Just (1, 0b101110) -- D.29
  , Just (1, 0b011110) -- D.30
  , Just (1, 0b101011) -- D.31
  , Just (0, 0b011000) -- D.00
  , Just (0, 0b100010) -- D.01
  , Just (0, 0b010010) -- D.02
  , Just (1, 0b110001) -- D.03
  , Just (0, 0b001010) -- D.04
  , Just (1, 0b101001) -- D.05
  , Just (1, 0b011001) -- D.06
  , Just (0, 0b000111) -- D.07
  , Just (0, 0b000110) -- D.08
  , Just (1, 0b100101) -- D.09
  , Just (1, 0b010101) -- D.10
  , Just (1, 0b110100) -- D.11
  , Just (1, 0b001101) -- D.12
  , Just (1, 0b101100) -- D.13
  , Just (1, 0b011100) -- D.14
  , Just (0, 0b101000) -- D.15
  , Just (0, 0b100100) -- D.16
  , Just (1, 0b100011) -- D.17
  , Just (1, 0b010011) -- D.18
  , Just (1, 0b110010) -- D.19
  , Just (1, 0b001011) -- D.20
  , Just (1, 0b101010) -- D.21
  , Just (1, 0b011010) -- D.22
  , Just (0, 0b000101) -- D.23
  , Just (0, 0b001100) -- D.24
  , Just (1, 0b100110) -- D.25
  , Just (1, 0b010110) -- D.26
  , Just (0, 0b001001) -- D.27
  , Just (1, 0b001110) -- D.28
  , Just (0, 0b010001) -- D.29
  , Just (0, 0b100001) -- D.30
  , Just (0, 0b010100) -- D.31
  , Nothing -- K.00
  , Nothing -- K.01
  , Nothing -- K.02
  , Nothing -- K.03
  , Nothing -- K.04
  , Nothing -- K.05
  , Nothing -- K.06
  , Nothing -- K.07
  , Nothing -- K.08
  , Nothing -- K.09
  , Nothing -- K.10
  , Nothing -- K.11
  , Nothing -- K.12
  , Nothing -- K.13
  , Nothing -- K.14
  , Nothing -- K.15
  , Nothing -- K.16
  , Nothing -- K.17
  , Nothing -- K.18
  , Nothing -- K.19
  , Nothing -- K.20
  , Nothing -- K.21
  , Nothing -- K.22
  , Just (1, 0b111010) -- K.23
  , Nothing -- K.24
  , Nothing -- K.25
  , Nothing -- K.26
  , Just (1, 0b110110) -- K.27
  , Just (1, 0b001111) -- K.28
  , Just (1, 0b101110) -- K.29
  , Just (1, 0b011110) -- K.30
  , Nothing -- K.31
  , Nothing -- K.00
  , Nothing -- K.01
  , Nothing -- K.02
  , Nothing -- K.03
  , Nothing -- K.04
  , Nothing -- K.05
  , Nothing -- K.06
  , Nothing -- K.07
  , Nothing -- K.08
  , Nothing -- K.09
  , Nothing -- K.10
  , Nothing -- K.11
  , Nothing -- K.12
  , Nothing -- K.13
  , Nothing -- K.14
  , Nothing -- K.15
  , Nothing -- K.16
  , Nothing -- K.17
  , Nothing -- K.18
  , Nothing -- K.19
  , Nothing -- K.20
  , Nothing -- K.21
  , Nothing -- K.22
  , Just (0, 0b000101) -- K.23
  , Nothing -- K.24
  , Nothing -- K.25
  , Nothing -- K.26
  , Just (0, 0b001001) -- K.27
  , Just (0, 0b110000) -- K.28
  , Just (0, 0b010001) -- K.29
  , Just (0, 0b100001) -- K.30
  , Nothing -- K.31
  ]
