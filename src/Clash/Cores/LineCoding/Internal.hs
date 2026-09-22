{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Helpers shared by the line coding modules
-}
module Clash.Cores.LineCoding.Internal where

import Clash.Prelude

-- | Running disparity after transmitting a code, given the running disparity
-- before it. 'False' is a negative running disparity, 'True' a positive one. A
-- code with as many ones as zeros leaves the running disparity unchanged,
-- otherwise the majority bit value decides.
nextDisparity :: forall n. (KnownNat n) => Bool -> BitVector n -> Bool
nextDisparity rd code = case compare (2 * popCount code) (natToNum @n) of
  GT -> True
  LT -> False
  EQ -> rd
