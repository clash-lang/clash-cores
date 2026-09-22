{- |
Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@mailbox.org>,
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

5b/6b decoding look-up table, derived from the encoding tables
-}
module Clash.Cores.LineCoding.Lc5b6b.Decoder where

import Clash.Cores.LineCoding.Internal (nextDisparity)
import qualified Clash.Cores.LineCoding.Lc5b6b.Encoder as Enc
import Clash.Prelude
import qualified Data.List as L

-- | All valid combinations of running disparity and 6-bit code group, with the
-- control flag and the 5-bit value they decode to. Of the control code groups only
-- @K.28@ has a 6-bit code group of its own; @K.23@, @K.27@, @K.29@ and @K.30@
-- share theirs with the data code groups with the same value and can only be told
-- apart by the 3b/4b part, so they decode as data words here.
validCodes :: [((Bool, BitVector 6), (Bool, BitVector 5))]
validCodes =
  [ ((rd, code), (cw, x))
  | (cw, rows) <- [(False, Enc.dataRows), (True, L.filter isK28 Enc.controlRows)]
  , (x, codeN, codeP) <- rows
  , (rd, code) <- [(False, codeN), (True, codeP)]
  ]
 where
  isK28 (x, _, _) = x == 28

-- | Look-up table for 'Clash.Cores.LineCoding.Lc5b6b.decode5b6b', indexed by
-- the concatenation of the running disparity and the 6-bit code group. An entry
-- holds whether the code group is @K.28@, the running disparity after the code
-- group and the decoded value, or is 'Nothing' if the code group is not valid at
-- that running disparity.
decoderLut :: [Maybe (Bool, Bool, BitVector 5)]
decoderLut =
  [ decode rd code
  | rd <- [False, True]
  , code <- [minBound .. maxBound]
  ]
 where
  decode rd code = do
    (cw, x) <- L.lookup (rd, code) validCodes
    pure (cw, nextDisparity rd code, x)
