{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

3b/4b encoding look-up table
-}
module Clash.Cores.LineCoding.Lc3b4b.Encoder where

import Clash.Prelude

encoderLut :: [Maybe (Bool, BitVector 4)]
encoderLut =
  [ Just (True, 0b1011) -- D.x.0
  , Just (False, 0b1001) -- D.x.1
  , Just (False, 0b0101) -- D.x.2
  , Just (False, 0b1100) -- D.x.3
  , Just (True, 0b1101) -- D.x.4
  , Just (False, 0b1010) -- D.x.5
  , Just (False, 0b0110) -- D.x.6
  , Just (True, 0b1110) -- D.x.P7
  , Just (False, 0b0100) -- D.x.0
  , Just (True, 0b1001) -- D.x.1
  , Just (True, 0b0101) -- D.x.2
  , Just (True, 0b0011) -- D.x.3
  , Just (False, 0b0010) -- D.x.4
  , Just (True, 0b1010) -- D.x.5
  , Just (True, 0b0110) -- D.x.6
  , Just (False, 0b0001) -- D.x.P7
  , Nothing -- D.x.0
  , Nothing -- D.x.1
  , Nothing -- D.x.2
  , Nothing -- D.x.3
  , Nothing -- D.x.4
  , Nothing -- D.x.5
  , Nothing -- D.x.6
  , Just (True, 0b0111) -- D.x.A7
  , Nothing -- D.x.0
  , Nothing -- D.x.1
  , Nothing -- D.x.2
  , Nothing -- D.x.3
  , Nothing -- D.x.4
  , Nothing -- D.x.5
  , Nothing -- D.x.6
  , Just (False, 0b1000) -- D.x.A7
  , Just (True, 0b1011) -- K.x.0
  , Just (False, 0b0110) -- K.x.1
  , Just (False, 0b1010) -- K.x.2
  , Just (False, 0b1100) -- K.x.3
  , Just (True, 0b1101) -- K.x.4
  , Just (False, 0b0101) -- K.x.5
  , Just (False, 0b1001) -- K.x.6
  , Just (True, 0b0111) -- K.x.7
  , Just (False, 0b0100) -- K.x.0
  , Just (True, 0b1001) -- K.x.1
  , Just (True, 0b0101) -- K.x.2
  , Just (True, 0b0011) -- K.x.3
  , Just (False, 0b0010) -- K.x.4
  , Just (True, 0b1010) -- K.x.5
  , Just (True, 0b0110) -- K.x.6
  , Just (False, 0b1000) -- K.x.7
  , Nothing -- K.x.0
  , Nothing -- K.x.1
  , Nothing -- K.x.2
  , Nothing -- K.x.3
  , Nothing -- K.x.4
  , Nothing -- K.x.5
  , Nothing -- K.x.6
  , Nothing -- K.x.7
  , Nothing -- K.x.0
  , Nothing -- K.x.1
  , Nothing -- K.x.2
  , Nothing -- K.x.3
  , Nothing -- K.x.4
  , Nothing -- K.x.5
  , Nothing -- K.x.6
  , Nothing -- K.x.7
  ]
