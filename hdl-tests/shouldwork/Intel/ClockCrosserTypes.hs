module ClockCrosserTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Cdc

import IntelTestCommon

import qualified Prelude as P

-- | Output domains, faster and slower than 'System'
createDomain vSystem{vName = "O7", vPeriod = 7000}
createDomain vSystem{vName = "O25", vPeriod = 25000}

-- | Stimuli of the input domain: reset, and a value to send
type InStim = (Bool, Maybe (BitVector 8))

-- | Stimuli of the output domain: reset, and ready
type OutStim = (Bool, Bool)

{- | Random stimuli for a test of 8 microseconds, with an input clock of 10 ns
and an output clock of the given period (in nanoseconds). Both domains start
in reset, offer and take values in phases of different intensities, and end
with idle cycles in which the output takes everything.
-}
genStimuli :: Int -> Int -> ([InStim], [OutStim])
genStimuli outPeriod = evalG $ do
  is <- P.concat <$> listOf nPhases (do
    p <- elements [10, 50, 90, 100]
    listOf 20 ((\v d -> (False, if v then Just d else Nothing)) <$> chance p <*> genBV))
  os <- P.concat <$> listOf nPhases (do
    p <- elements [0, 10, 50, 90, 100]
    listOf (200 `div` outPeriod) ((,) False <$> chance p))
  pure
    ( P.replicate 5 (True, Nothing) <> P.take nIn is <> P.replicate 50 (False, Nothing)
    , P.replicate (50 `div` outPeriod + 2) (True, False) <> P.take nOut os
        <> P.replicate (500 `div` outPeriod) (False, True) )
 where
  duration = 8000
  nIn = duration `div` 10
  nOut = duration `div` outPeriod
  nPhases = duration `div` 200 + 1

-- | A clock crosser with the given configuration
dut ::
  (KnownDomain out, HasAsynchronousReset out) =>
  ClockCrosserConfig ->
  Circuit2 System out InStim OutStim Bool (Maybe (BitVector 8))
dut cfg inClk outClk inStim outStim =
  avalonStClockCrosser cfg
    inClk (unsafeFromActiveHigh (fst <$> inStim))
    outClk (unsafeFromActiveHigh (fst <$> outStim))
    (snd <$> inStim) (snd <$> outStim)
