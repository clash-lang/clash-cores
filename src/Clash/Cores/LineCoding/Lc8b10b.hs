{- |
  Copyright   :  (C) 2024-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  8b/10b encoding and decoding functions (IEEE 802.3 Clause 36), built from
  the 5b/6b and 3b/4b sub-codes in "Clash.Cores.LineCoding.Lc5b6b" and
  "Clash.Cores.LineCoding.Lc3b4b".

  A code group is a 'BitVector' of 10 bits in transmission order: bit 0 is
  @a@, the first bit on the line, and bit 9 is @j@. The running disparity is a
  'Bool': 'False' is a negative running disparity, 'True' a positive one.
-}
module Clash.Cores.LineCoding.Lc8b10b where

import Clash.Cores.LineCoding.Internal (nextDisparity)
import Clash.Cores.LineCoding.Lc3b4b
import Clash.Cores.LineCoding.Lc5b6b
import Clash.Prelude
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

-- | Data type that contains a 'BitVector' with the corresponding error
--   condition of the decode function
data Symbol8b10b
  = -- | Correct data word
    Dw (BitVector 8)
  | -- | Correct control word
    Cw (BitVector 8)
  | -- | Incorrect data word
    DwError (BitVector 8)
  | -- | Running disparity error
    RdError (BitVector 8)
  deriving (Generic, NFDataX, Eq, Show)

-- | Function to check whether a 'Symbol8b10b' results in a data word
isDw :: Symbol8b10b -> Bool
isDw (Dw _) = True
isDw _ = False

-- | Function to check whether a 'Symbol8b10b' results in a control word
isCw :: Symbol8b10b -> Bool
isCw (Cw _) = True
isCw _ = False

-- | Function to check whether a 'Symbol8b10b' is not an error value
isValidSymbol :: Symbol8b10b -> Bool
isValidSymbol sym = isDw sym || isCw sym

-- | Function to convert a 'Symbol8b10b' to a plain 'BitVector'
fromSymbol :: Symbol8b10b -> BitVector 8
fromSymbol sym = case sym of
  Dw w -> w
  Cw w -> w
  DwError w -> w
  RdError w -> w

-- | Reverse the order of the bits in a 'BitVector'
reverseBits :: (KnownNat n) => BitVector n -> BitVector n
reverseBits = v2bv . reverse . bv2v

-- | Split a code group into its 6-bit part @abcdei@ and its 4-bit part @fghj@,
--   both with the first bit on the line as most significant bit, as used by
--   the sub-codes
splitCodeGroup :: BitVector 10 -> (BitVector 6, BitVector 4)
splitCodeGroup = split . reverseBits

-- | Inverse of 'splitCodeGroup'
joinCodeGroup :: BitVector 6 -> BitVector 4 -> BitVector 10
joinCodeGroup c6 c4 = reverseBits (c6 ++# c4)

-- | Take the running disparity and the current 'Symbol8b10b', and return a
--   tuple containing the new running disparity and a 'BitVector' containing the
--   encoded value. Error symbols and undefined control words (all but @K.28.y@,
--   @K.23.7@, @K.27.7@, @K.29.7@ and @K.30.7@) leave the running disparity
--   unchanged and give the code group 0.
encode8b10b ::
  -- | Running disparity
  Bool ->
  -- | Symbol
  Symbol8b10b ->
  -- | Tuple containing the new running disparity and the code group
  (Bool, BitVector 10)
encode8b10b rd sym = fromMaybe (rd, 0) $ do
  guard (isValidSymbol sym)
  (rd1, c6) <- encode5b6b cw rd x
  guard (not cw || x == 28 || y == 7)
  let (rd2, c4) = encode3b4b cw (useAlternate7 rd1 x) rd1 y
  pure (rd2, joinCodeGroup c6 c4)
 where
  cw = isCw sym
  (y, x) = split (fromSymbol sym) :: (BitVector 3, BitVector 5)
{-# OPAQUE encode8b10b #-}

-- | Decode a code group that 'encode8b10b' can produce at the given running
--   disparity: 'Just' whether it is a control word and its value, 'Nothing'
--   for every other code group.
decodeValid :: Bool -> BitVector 10 -> Maybe (Bool, BitVector 8)
decodeValid rd cg = do
  (isK28, rd1, x) <- decode5b6b rd c6
  let isAlt = c4 == alternate7 rd1
      altRequired = useAlternate7 rd1 x
      -- K.23.7, K.27.7, K.29.7 and K.30.7 share their 6-bit part with a data
      -- word. They are recognised by an alternate form where a data word would
      -- have used the primary form.
      cw = isK28 || (isAlt && not altRequired && isControl7 x)
  (_, y) <- decode3b4b cw rd1 c4
  -- A data word with value 7 has to use the form the rule prescribes
  guard (cw || y /= 7 || isAlt == altRequired)
  pure (cw, y ++# x)
 where
  (c6, c4) = splitCodeGroup cg
  isControl7 x = x == 23 || x == 27 || x == 29 || x == 30

-- | Take the running disparity and the current code group, and return a tuple
--   containing the new running disparity and a 'Symbol8b10b' containing the
--   decoded value. A code group that is only valid at the other running
--   disparity gives 'RdError' with the value it has there; any other invalid
--   code group gives @'DwError' 0@. The new running disparity follows from the
--   disparity of the code group, also for invalid code groups.
decode8b10b ::
  -- | Running disparity
  Bool ->
  -- | Code group
  BitVector 10 ->
  -- | Tuple containing the new running disparity and the 'Symbol8b10b'
  (Bool, Symbol8b10b)
decode8b10b rd cg = (nextDisparity rd cg, sym)
 where
  sym = case decodeValid rd cg of
    Just (True, w) -> Cw w
    Just (False, w) -> Dw w
    Nothing -> case decodeValid (not rd) cg of
      Just (_, w) -> RdError w
      Nothing -> DwError 0
{-# OPAQUE decode8b10b #-}
