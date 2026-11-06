-- | Circuits and stimuli for "SbSpram"
module SbSpramTypes where

import Clash.Explicit.Prelude

import Clash.Cores.LatticeSemi.Ice40.Memory
import Ice40TestCommon

import Language.Haskell.TH (ExpQ)

import qualified Prelude as P

-- | Number of cycles of each test bench
type Cycles = 1000

data SpramIn = SpramIn
  { chipSelect :: Bool
  , address :: Index 16384
  , write :: Maybe (BitVector 4, BitVector 16)
  , standby :: Bool
  , sleep :: Bool
  , powerOn :: Bool
  }
  deriving (Show, Generic, NFDataX, BitPack)

spramCircuit :: Clock Dom -> Signal Dom SpramIn -> Signal Dom (BitVector 16)
spramCircuit clk i =
  sbSpram256ka
    clk
    (chipSelect <$> i)
    (address <$> i)
    (write <$> i)
    (standby <$> i)
    (sleep <$> i)
    (powerOn <$> i)

-- | Random stimuli. The addresses are mostly from a small set, so that reads
-- often hit written words. The low-power modes are entered with the given
-- probabilities (in percent).
genSpramIn :: Integer -> Integer -> Integer -> G SpramIn
genSpramIn pStandby pSleep pOff = do
  cs <- chance 85
  small <- chance 90
  addr <- if small then fromIntegral <$> genBV @5 else unpack <$> genBV
  w <- chance 50
  m <- genBV
  d <- genBV
  standby' <- chance pStandby
  sleep' <- chance pSleep
  off <- chance pOff
  pure
    SpramIn
      { chipSelect = cs
      , address = addr
      , write = if w then Just (m, d) else Nothing
      , standby = standby'
      , sleep = sleep'
      , powerOn = not off
      }

spramBlobs :: Integer -> Integer -> Integer -> Int -> ExpQ
spramBlobs pStandby pSleep pOff seed =
  blobs spramCircuit
    (evalG (sequence (P.replicate (natToNum @Cycles) (genSpramIn pStandby pSleep pOff))) seed)
