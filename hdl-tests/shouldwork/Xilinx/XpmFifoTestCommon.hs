{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test infrastructure shared by the @XpmFifoAx*@ tests
module XpmFifoTestCommon
  ( module XpmFifoTestCommon
  , D3, D5, D10, D11
  ) where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Explicit.BlockRam.Blob (memBlobTH)
import Clash.Prelude.ROM.Blob (asyncRomBlob)
import Clash.Sized.Internal.BitVector (BitVector(BV), xToBV)


import Language.Haskell.TH.Lib (ExpQ)

import Data.Maybe (fromMaybe, isJust)

import qualified Prelude as P

import XpmTestCommon (D3, D5, D10, D11)

-- | Probability of an event, in eighths, switching between two values every
-- @2^phShift@ cycles. This makes a FIFO alternately fill up and drain.
data Phases = Phases
  { phLow :: Unsigned 4
  , phHigh :: Unsigned 4
  , phShift :: Index 16
  }

-- | Stimuli of a test
data Stimuli = Stimuli
  { stInitialReset :: Unsigned 16
  -- ^ Number of cycles the reset is asserted at the start (write domain)
  , stResetStart :: Unsigned 16
  -- ^ Cycle (write domain) at which the reset is asserted again
  , stResetLength :: Unsigned 16
  -- ^ Number of cycles the second reset is asserted
  , stWritePhases :: Phases
  -- ^ Probability of the source offering new data
  , stReadPhases :: Phases
  -- ^ Probability of the sink accepting data
  , stTestDuration :: Int
  -- ^ Test duration in nanoseconds. Only used to calculate expected values.
  }

-- | Default stimuli: a long reset at the start (well past the 100 ns global
-- set/reset of Vivado's simulator), a short one halfway, and alternating
-- fill and drain phases.
defStimuli :: Stimuli
defStimuli = Stimuli
  { stInitialReset = 20
  , stResetStart = 200
  , stResetLength = 3
  , stWritePhases = Phases{phLow = 7, phHigh = 1, phShift = 5}
  , stReadPhases = Phases{phLow = 1, phHigh = 8, phShift = 6}
  , stTestDuration = 15000
  }

inReset :: Stimuli -> Unsigned 16 -> Bool
inReset Stimuli{stInitialReset, stResetStart, stResetLength} c =
  c < stInitialReset || (c >= stResetStart && c - stResetStart < stResetLength)

phaseOf :: Phases -> Unsigned 16 -> Unsigned 4
phaseOf Phases{phLow, phHigh, phShift} c
  | testBit c (fromIntegral phShift) = phHigh
  | otherwise = phLow

-- | Decide on an event with the given probability (in eighths), given a
-- random number
chance :: Unsigned 4 -> Unsigned 16 -> Bool
chance p r = p >= 8 || (resize (r `shiftR` 3) :: Unsigned 3) < resize p

-- | Free running cycle counter
cycleCounter :: KnownDomain dom => Clock dom -> Signal dom (Unsigned 16)
cycleCounter clk = c where c = delay clk enableGen 0 (c + 1)

-- | 16 bit Galois LFSR
lfsr :: KnownDomain dom => Clock dom -> Unsigned 16 -> Signal dom (Unsigned 16)
lfsr clk seed = s
 where
  s = delay clk enableGen seed (next <$> s)
  next x
    | testBit x 0 = (x `shiftR` 1) `xor` 0xB400
    | otherwise = x `shiftR` 1

-- | Pack a field into a fixed size bit vector, keeping it in place. If the
-- first argument is 'True', undefined values become undefined bits (only for
-- expected values, never for test benches).
field :: (BitPack a, KnownNat w) => Bool -> Int -> a -> BitVector w
field x offset a = resize (if x then packX a else pack a) `shiftL` offset

-- | Like 'field', for a 'Maybe': a valid bit at the given offset, followed by
-- the payload
fieldMaybe :: (BitPack a, NFDataX a, KnownNat w) => Bool -> Int -> Maybe a -> BitVector w
fieldMaybe x offset m =
  field x offset (isJust m) .|. field x (offset + 1) (fromMaybe (deepErrorX "no data") m)

-- | Random stream of counter values, for one channel. Offers new data with a
-- probability given by the phases. Once it offers data, it keeps offering it
-- until it is accepted, as AXI requires.
stream ::
  KnownDomain dom =>
  Phases -> Unsigned 16 -> Clock dom -> Signal dom Bool -> Signal dom (Maybe (Unsigned 16))
stream phases seed clk ready =
  mealy clk noReset enableGen go (False, 0) (bundle (ready, lfsr clk seed, cycleCounter clk))
 where
  go (pend, n) (rdy, r, c) = ((pend', n'), if pend then Just n else Nothing)
   where
    (pend', n')
      | pend && not rdy = (pend, n)
      | otherwise = (chance (phaseOf phases c) r, if pend then n + 1 else n)

-- | Random ready signal for one channel
readyGen :: KnownDomain dom => Phases -> Unsigned 16 -> Clock dom -> Signal dom Bool
readyGen phases seed clk = chance <$> (phaseOf phases <$> cycleCounter clk) <*> lfsr clk seed

-- | Reset generated according to the stimuli
stimuliReset :: KnownDomain dom => Stimuli -> Clock dom -> Reset dom
stimuliReset stim clk =
  unsafeFromActiveHigh (delay clk enableGen True (inReset stim <$> cycleCounter clk))

-- | Compare with expected values, stored in a 'MemBlob' as pairs of a mask and
-- a value (see 'expectedTH'). Masked bits are not compared. Returns 'True' once
-- all expected values have been compared.
verify ::
  forall dom n w .
  (KnownDomain dom, KnownNat n, 1 <= n, KnownNat w) =>
  Clock dom -> String -> MemBlob n (w + w) -> Signal dom (BitVector w) -> Signal dom Bool
verify clk msg blob actual = mux done done checked
 where
  counter :: Signal dom (Index n)
  counter = delay clk enableGen 0 (satSucc SatBound <$> counter)
  done = delay clk enableGen False ((== maxBound) <$> counter)
  entry = asyncRomBlob blob <$> counter
  (mask, expected) = unbundle (split @_ @w @w <$> entry)
  actualMasked = (\a m -> a .&. complement m) <$> actual <*> mask
  checked = assert clk noReset msg actualMasked expected done
{-# INLINE verify #-}

-- | Sample a signal for the given duration (in ns). Samples in the first 200 ns
-- are replaced by undefined values, as the model does not model the 100 ns
-- global set/reset Vivado's simulator starts with.
samplesFor ::
  forall dom n .
  (KnownDomain dom, KnownNat n) =>
  Int -> Signal dom (BitVector n) -> [BitVector n]
samplesFor durationNs sig = P.zipWith mask [0 ..] (sampleN nSamples sig)
 where
  period = snatToNum (clockPeriod @dom) :: Int
  nSamples = durationNs * 1000 `div` period
  mask :: Int -> BitVector n -> BitVector n
  mask i bv
    | i * period < 200000 = undefined#
    | otherwise = bv
  undefined# = deepErrorX "masked"

-- | Like 'pack', but turns an undefined value into undefined bits instead of an
-- undefined bit vector. Only to be used for expected values.
packX :: BitPack a => a -> BitVector (BitSize a)
packX = xToBV . pack

-- | Turn expected values for the write and read domain into a tuple of
-- 'MemBlob's. Each entry holds a mask (undefined bits of the expected value)
-- and the value itself.
expectedTH :: forall w1 w2 . (KnownNat w1, KnownNat w2) => [BitVector w1] -> [BitVector w2] -> ExpQ
expectedTH ws rs = [| ($(memBlobTH Nothing (P.map maskValue ws)), $(memBlobTH Nothing (P.map maskValue rs))) |]
 where
  maskValue :: forall w . KnownNat w => BitVector w -> BitVector (w + w)
  maskValue (xToBV -> BV m v) = mask ++# (value .&. complement mask)
   where
    mask = fromIntegral m :: BitVector w
    value = fromIntegral v :: BitVector w
