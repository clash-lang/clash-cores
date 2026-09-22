{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>,
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

3b/4b encoding look-up tables
-}
module Clash.Cores.LineCoding.Lc3b4b.Encoder where

import Clash.Cores.LineCoding.Internal (nextDisparity)
import Clash.Prelude
import qualified Data.List as L
import Data.Maybe (fromMaybe)

-- | A row of the 3b/4b encoding table: the 3-bit input value @y@, the code
-- group to transmit when the running disparity is negative, and the code group to
-- transmit when it is positive. Code groups are written as @fghj@ with @f@, the
-- first bit on the line, as the most significant bit.
type Row = (BitVector 3, BitVector 4, BitVector 4)

-- | Data code groups @D.x.y@ (IEEE 802.3 Table 36-1b), with the primary form
-- @D.x.P7@ for @y = 7@
dataRows :: [Row]
dataRows =
  [ (0, 0b1011, 0b0100) -- D.x.0
  , (1, 0b1001, 0b1001) -- D.x.1
  , (2, 0b0101, 0b0101) -- D.x.2
  , (3, 0b1100, 0b0011) -- D.x.3
  , (4, 0b1101, 0b0010) -- D.x.4
  , (5, 0b1010, 0b1010) -- D.x.5
  , (6, 0b0110, 0b0110) -- D.x.6
  , (7, 0b1110, 0b0001) -- D.x.P7
  ]

-- | Alternate form @D.x.A7@ of the data code group with @y = 7@, for a negative
-- and for a positive running disparity. When it has to be used is decided by
-- 'Clash.Cores.LineCoding.Lc3b4b.useAlternate7'.
alternate7 :: (BitVector 4, BitVector 4)
alternate7 = (0b0111, 0b1000)

-- | Control code groups @K.x.y@ (IEEE 802.3 Table 36-2)
controlRows :: [Row]
controlRows =
  [ (0, 0b1011, 0b0100) -- K.x.0
  , (1, 0b0110, 0b1001) -- K.x.1
  , (2, 0b1010, 0b0101) -- K.x.2
  , (3, 0b1100, 0b0011) -- K.x.3
  , (4, 0b1101, 0b0010) -- K.x.4
  , (5, 0b0101, 0b1010) -- K.x.5
  , (6, 0b1001, 0b0110) -- K.x.6
  , (7, 0b0111, 0b1000) -- K.x.7
  ]

-- | Look-up table for 'Clash.Cores.LineCoding.Lc3b4b.encode3b4b', indexed by
-- the concatenation of the control word flag, the alternate form flag, the
-- running disparity and the 3-bit input value. An entry holds the running
-- disparity after the code group and the code group itself. The alternate form
-- flag only has an effect on data words with value 7.
encoderLut :: [(Bool, BitVector 4)]
encoderLut =
  [ encode cw alt rd y
  | cw <- [False, True]
  , alt <- [False, True]
  , rd <- [False, True]
  , y <- [minBound .. maxBound]
  ]
 where
  encode cw alt rd y = (nextDisparity rd code, code)
   where
    code = if rd then codeP else codeN
    (codeN, codeP)
      | not cw && alt && y == 7 = alternate7
      | otherwise = (rowN, rowP)
    (_, rowN, rowP) =
      fromMaybe (error "encoderLut: incomplete table")
        $ L.find (\(y', _, _) -> y' == y) (if cw then controlRows else dataRows)
