{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>,
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

3b/4b decoding look-up table, derived from the encoding tables
-}
module Clash.Cores.LineCoding.Lc3b4b.Decoder where

import Clash.Cores.LineCoding.Internal (nextDisparity)
import qualified Clash.Cores.LineCoding.Lc3b4b.Encoder as Enc
import Clash.Prelude
import qualified Data.List as L
import qualified Prelude as P

-- | All valid combinations of control word flag, running disparity and 4-bit
-- code group, with the 3-bit value they decode to. Both forms of @D.x.7@ decode
-- to 7.
validCodes :: [((Bool, Bool, BitVector 4), BitVector 3)]
validCodes =
  [ ((cw, rd, code), y)
  | (cw, rows) <- [(False, dataRows), (True, Enc.controlRows)]
  , (y, codeN, codeP) <- rows
  , (rd, code) <- [(False, codeN), (True, codeP)]
  ]
 where
  dataRows = Enc.dataRows P.++ [(7, fst Enc.alternate7, snd Enc.alternate7)]

-- | Look-up table for 'Clash.Cores.LineCoding.Lc3b4b.decode3b4b', indexed by
-- the concatenation of the control word flag, the running disparity and the
-- 4-bit code group. An entry holds the running disparity after the code group and
-- the decoded value, or is 'Nothing' if the code group is not valid for that
-- control word flag and running disparity.
decoderLut :: [Maybe (Bool, BitVector 3)]
decoderLut =
  [ decode cw rd code
  | cw <- [False, True]
  , rd <- [False, True]
  , code <- [minBound .. maxBound]
  ]
 where
  decode cw rd code = do
    y <- L.lookup (cw, rd, code) validCodes
    pure (nextDisparity rd code, y)
