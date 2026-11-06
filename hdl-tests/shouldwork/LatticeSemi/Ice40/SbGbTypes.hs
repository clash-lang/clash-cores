-- | Circuits and stimuli for "SbGb"
module SbGbTypes where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench (tbClockGen)
import Clash.Signal (withClock, withEnable)

import Clash.Cores.LatticeSemi.Ice40.IO
import Ice40TestCommon
import SbIoTypes (Cycles, SbioIn (..), genSbioIn)

import Language.Haskell.TH (ExpQ)

import qualified Prelude as P

-- | A register clocked by its clock, and the input passed through 'sbGbSignal'
registerCircuit :: Clock Dom -> Signal Dom (BitVector 8) -> Signal Dom (BitVector 8, Bit)
registerCircuit clk i = bundle (delay clk enableGen 0 i, sbGbSignal (lsb <$> i))

registerBlobs :: Int -> ExpQ
registerBlobs seed =
  blobs registerCircuit (evalG (sequence (P.replicate (natToNum @Cycles) genBV)) seed)

-- | Like 'SbIoTypes.sbioCircuit', with 'sbGbIo' instead of 'sbio'
sbGbIoCircuit :: BitVector 6 -> Clock Dom -> Signal Dom SbioIn -> Signal Dom (Bit, Bit, Bit)
sbGbIoCircuit pinType clk i = bundle (readFromBiSignal pad, dIn0, dIn1)
 where
  pad = veryUnsafeToBiSignalIn (mergeBiSignalOuts (sbioOut :> tbOut :> Nil))
  tbOut = writeToBiSignal pad (padDrive <$> i)
  (sbioOut, dIn0, dIn1, _gbClk :: Clock Dom) =
    withClock clk $
      withEnable (toEnable (clockEnable <$> i)) $
        sbGbIo
          pinType
          pad
          (latchInput <$> i)
          (dOut0 <$> i)
          (dOut1 <$> i)
          (outputEnable <$> i)
{-# INLINE sbGbIoCircuit #-}

sbGbIoBlobs :: BitVector 6 -> Int -> ExpQ
sbGbIoBlobs pinType seed =
  blobs
    (sbGbIoCircuit pinType)
    (evalG (sequence (P.replicate (natToNum @Cycles) (genSbioIn pinType))) seed)

{- | A clock for 'Dom', from a pad driven by a register in 'Fast' toggling
every cycle, through 'sbGbIoClock'. The clock stops once the given signal is
'True'.
-}
padClock :: Signal Dom Bool -> Clock Dom
padClock done = sbGbIoClock pad
 where
  clkFast = tbClockGen (not <$> unsafeSynchronizer (padClock done) clkFast done)
  toggle :: Signal Fast Bit
  toggle = register clkFast noReset enableGen 0 (complement <$> toggle)
  pad :: BiSignalIn 'Floating Fast 1
  pad = veryUnsafeToBiSignalIn (writeToBiSignal pad (Just <$> toggle))
