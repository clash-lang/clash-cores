{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module BidirectionalBufferTypes where

import Clash.Cores.LatticeSemi.Ecp5.IO (bidirectionalBuffer)
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Signal.BiSignal (HasBiSignalDefault, SBiSignalDefault)
import Data.Reflection (Given)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.Word (Word32)
import Language.Haskell.TH (ExpQ)
import qualified Prelude as P

import YosysSim (yosysSimModels)

$(yosysSimModels "ecp5SimModels" ["ecp5/common_sim.vh", "ecp5/cells_io.vh"] ("ecp5u", "ecp5u_components.vhdl"))

type Dom = System

-- | (output enable, value driven by the buffer, value driven by another driver)
type Stimulus = (Bool, Bit, Maybe Bit)

{- | A pad driven by both 'bidirectionalBuffer' and another driver, e.g. a
device on the other side of the pin. Returns the value 'bidirectionalBuffer'
reads from the pad.
-}
dut ::
  forall ds dom.
  (HasBiSignalDefault ds, Given (SBiSignalDefault ds), KnownDomain dom) =>
  Signal dom Stimulus ->
  Signal dom Bit
dut stimulus = dIn
 where
  (en, dOut, other) = unbundle stimulus
  pad :: BiSignalIn ds dom 1
  pad = veryUnsafeToBiSignalIn (mergeBiSignalOuts (padOut :> otherOut :> Nil))
  otherOut = writeToBiSignal pad (fmap pack <$> other)
  (padOut, dIn) = bidirectionalBuffer (toEnable en) pad dOut

-- | Random stimuli, without contention on the pad
stimuliList :: [Stimulus]
stimuliList = P.take 256 (P.map toStimulus (P.iterate lcg 0xC1A5))
 where
  lcg :: Word32 -> Word32
  lcg x = 1664525 * x + 1013904223

  toStimulus x =
    let bit n = testBit x n
        en = bit 31
        dOut = boolToBit (bit 30)
        other
          | en = Nothing
          | bit 29 = Just (boolToBit (bit 28))
          | otherwise = Nothing
     in (en, dOut, other)

stimuli :: ExpQ
stimuli = listToVecTH stimuliList

{- | Value the Haskell model reads from the pad. 'Nothing' if nothing drives
the pad: the Haskell model then reads the value of the pull-up or pull-down
(or an undefined value), but the Yosys models don't model pull resistors at
all and read @z@.
-}
expected ::
  forall ds.
  (HasBiSignalDefault ds, Given (SBiSignalDefault ds)) =>
  Proxy ds ->
  ExpQ
expected Proxy = listToVecTH (P.zipWith mask stimuliList actual)
 where
  actual = sampleN (P.length stimuliList) (dut @ds @Dom (fromList stimuliList))
  mask (en, _, other) b
    | en || isJust other = Just b
    | otherwise = Nothing

tb ::
  forall ds n.
  (HasBiSignalDefault ds, Given (SBiSignalDefault ds), KnownNat n, 1 <= n) =>
  Proxy ds ->
  Vec n Stimulus ->
  Vec n (Maybe Bit) ->
  Signal Dom Bool
tb Proxy stimuliVec expectedVec = ecp5SimModels done
 where
  clk = tbClockGen (not <$> done)
  rst = resetGen

  input = stimuliGenerator clk rst stimuliVec
  expectedMasked = stimuliGenerator clk rst expectedVec
  actual = dut @ds @Dom input

  -- Replace the cycles we don't care about by their expected value
  actualMasked =
    mux (isJust <$> expectedMasked) actual (fromMaybe 0 <$> expectedMasked)

  done =
    outputVerifier' clk rst (map (fromMaybe 0) expectedVec) actualMasked
