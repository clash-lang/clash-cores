module BitBiSignalTypes where

import Clash.Explicit.Prelude

import qualified Data.List as L

-- | Inputs @(I_A, T_A, I_B, T_B)@ of the two buffers in "BitBiSignal", and the
-- expected outputs @(O_A, O_B)@. The first 16 cycles are not checked.
testVectors :: [((Bit, Bit, Bit, Bit), (Bit, Bit))]
testVectors =
  L.replicate 16 ((0, 1, 0, 1), (0, 0)) <>
  [ ((0, 0, 0, 1), (0, 0)) -- A drives
  , ((1, 0, 0, 1), (1, 1))
  , ((0, 1, 1, 0), (1, 1)) -- B drives
  , ((1, 1, 0, 0), (0, 0))
  , ((0, 1, 0, 0), (0, 0))
  , ((1, 1, 1, 0), (1, 1))
  , ((1, 0, 1, 1), (1, 1)) -- A drives
  , ((0, 0, 1, 1), (0, 0))
  ]
