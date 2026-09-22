{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

A fixed Ethernet test frame with its frame check sequence, computed at compile
time (this module is only evaluated by Template Haskell).
-}
module Kcu105.Sgmii.TestFrame where

import Data.Bits (shiftR, testBit, xor, (.&.))
import Data.Word (Word32, Word8)
import Prelude

-- | IEEE 802.3 CRC-32 of a byte sequence, as transmitted (least significant
--   byte first)
fcs :: [Word8] -> [Word8]
fcs bytes = [fromIntegral (final `shiftR` (8 * i)) | i <- [0 .. 3]]
 where
  final = foldl step 0xFFFFFFFF bytes `xor` 0xFFFFFFFF
  step :: Word32 -> Word8 -> Word32
  step crc byte = iterate shift1 (crc `xor` fromIntegral byte) !! 8
  shift1 c
    | testBit c 0 = (c `shiftR` 1) `xor` 0xEDB88320
    | otherwise = c `shiftR` 1

-- | Preamble and start frame delimiter as a MAC presents them on GMII
preamble :: [Word8]
preamble = replicate 7 0x55 ++ [0xD5]

-- | A broadcast frame with a locally administered source address and the
--   experimental EtherType 0x88B5, padded to the minimum size
testFrameBody :: [Word8]
testFrameBody = header ++ payload
 where
  header = replicate 6 0xFF ++ [0x02, 0x00, 0x00, 0x00, 0x00, 0x01] ++ [0x88, 0xB5]
  payload = take 46 (map fromIntegral [0 :: Int ..] ++ repeat 0)

-- | A longer frame of the same kind with a 300-byte payload
longFrameBody :: [Word8]
longFrameBody = header ++ payload
 where
  header = replicate 6 0xFF ++ [0x02, 0x00, 0x00, 0x00, 0x00, 0x02] ++ [0x88, 0xB5]
  payload = take 300 (map fromIntegral [0 :: Int ..])

-- | The complete GMII byte stream of a frame: preamble, frame and FCS
gmiiBytes :: [Word8] -> [Integer]
gmiiBytes body = map fromIntegral (preamble ++ body ++ fcs body)

-- | The short test frame (72 bytes on GMII)
testFrameBytes :: [Integer]
testFrameBytes = gmiiBytes testFrameBody

-- | The long test frame (326 bytes on GMII)
longFrameBytes :: [Integer]
longFrameBytes = gmiiBytes longFrameBody
