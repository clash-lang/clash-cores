{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>,
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

5b/6b encoding look-up tables
-}
module Clash.Cores.LineCoding.Lc5b6b.Encoder where

import Clash.Cores.LineCoding.Internal (nextDisparity)
import Clash.Prelude
import qualified Data.List as L

-- | A row of the 5b/6b encoding table: the 5-bit input value @x@, the code
-- group to transmit when the running disparity is negative, and the code group to
-- transmit when it is positive. Code groups are written as @abcdei@ with @a@, the
-- first bit on the line, as the most significant bit.
type Row = (BitVector 5, BitVector 6, BitVector 6)

-- | Data code groups @D.x@ (IEEE 802.3 Table 36-1a)
dataRows :: [Row]
dataRows =
  [ (0, 0b100111, 0b011000) -- D.00
  , (1, 0b011101, 0b100010) -- D.01
  , (2, 0b101101, 0b010010) -- D.02
  , (3, 0b110001, 0b110001) -- D.03
  , (4, 0b110101, 0b001010) -- D.04
  , (5, 0b101001, 0b101001) -- D.05
  , (6, 0b011001, 0b011001) -- D.06
  , (7, 0b111000, 0b000111) -- D.07
  , (8, 0b111001, 0b000110) -- D.08
  , (9, 0b100101, 0b100101) -- D.09
  , (10, 0b010101, 0b010101) -- D.10
  , (11, 0b110100, 0b110100) -- D.11
  , (12, 0b001101, 0b001101) -- D.12
  , (13, 0b101100, 0b101100) -- D.13
  , (14, 0b011100, 0b011100) -- D.14
  , (15, 0b010111, 0b101000) -- D.15
  , (16, 0b011011, 0b100100) -- D.16
  , (17, 0b100011, 0b100011) -- D.17
  , (18, 0b010011, 0b010011) -- D.18
  , (19, 0b110010, 0b110010) -- D.19
  , (20, 0b001011, 0b001011) -- D.20
  , (21, 0b101010, 0b101010) -- D.21
  , (22, 0b011010, 0b011010) -- D.22
  , (23, 0b111010, 0b000101) -- D.23
  , (24, 0b110011, 0b001100) -- D.24
  , (25, 0b100110, 0b100110) -- D.25
  , (26, 0b010110, 0b010110) -- D.26
  , (27, 0b110110, 0b001001) -- D.27
  , (28, 0b001110, 0b001110) -- D.28
  , (29, 0b101110, 0b010001) -- D.29
  , (30, 0b011110, 0b100001) -- D.30
  , (31, 0b101011, 0b010100) -- D.31
  ]

-- | Control code groups @K.x@ (IEEE 802.3 Table 36-2). Only these five values
-- have a control code group.
controlRows :: [Row]
controlRows =
  [ (23, 0b111010, 0b000101) -- K.23
  , (27, 0b110110, 0b001001) -- K.27
  , (28, 0b001111, 0b110000) -- K.28
  , (29, 0b101110, 0b010001) -- K.29
  , (30, 0b011110, 0b100001) -- K.30
  ]

-- | Look-up table for 'Clash.Cores.LineCoding.Lc5b6b.encode5b6b', indexed by
-- the concatenation of the control word flag, the running disparity and the 5-bit
-- input value. An entry holds the running disparity after the code group and the
-- code group itself, or is 'Nothing' for a control word that does not exist.
encoderLut :: [Maybe (Bool, BitVector 6)]
encoderLut =
  [ encode cw rd x
  | cw <- [False, True]
  , rd <- [False, True]
  , x <- [minBound .. maxBound]
  ]
 where
  encode cw rd x = do
    (_, codeN, codeP) <-
      L.find (\(x', _, _) -> x' == x) (if cw then controlRows else dataRows)
    let code = if rd then codeP else codeN
    pure (nextDisparity rd code, code)
