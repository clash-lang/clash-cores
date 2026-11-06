-- | Circuits and stimuli for "SbIoOd"
module SbIoOdTypes where

import Clash.Explicit.Prelude
import Clash.Signal (withClock, withEnable)

import Clash.Cores.LatticeSemi.Ice40.IO
import Ice40TestCommon
import SbIoTypes (SbioIn (..), sbioStimuli)

import Language.Haskell.TH (ExpQ)

-- | 'sbioOd', with its pad connected to a pull-up and a driver in the test
-- bench. Outputs the level of the pad, @DIN0@ and @DIN1@.
sbioOdCircuit :: BitVector 6 -> Clock Dom -> Signal Dom SbioIn -> Signal Dom (Bit, Bit, Bit)
sbioOdCircuit pinType clk i = bundle (readFromBiSignal pad, dIn0, dIn1)
 where
  pad = veryUnsafeToBiSignalIn (mergeBiSignalOuts (sbioOut :> tbOut :> pullUp pad :> Nil))
  tbOut = writeToBiSignal pad (padDrive <$> i)
  (sbioOut, dIn0, dIn1) =
    withClock clk $
      withEnable (toEnable (clockEnable <$> i)) $
        sbioOd
          pinType
          pad
          (latchInput <$> i)
          (dOut0 <$> i)
          (dOut1 <$> i)
          (outputEnable <$> i)
{-# INLINE sbioOdCircuit #-}

sbioOdBlobs :: BitVector 6 -> Int -> ExpQ
sbioOdBlobs pinType seed = blobs (sbioOdCircuit pinType) (sbioStimuli pinType seed)
