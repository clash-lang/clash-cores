{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}

-- | Shared definitions for the @XPM_MEMORY@ tests. Each test generates
-- pseudo-random stimuli for the ports of a memory, runs the Haskell simulation
-- model in Template Haskell to determine the expected outputs, and compares
-- these with the outputs of the real XPM in an HDL simulator. Undefined bits
-- in the expected outputs are "don't care".
module XpmMemoryCommon where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (undefined#)
import Language.Haskell.TH (ExpQ, Loc(loc_filename), location, runIO)
import Language.Haskell.TH.Syntax (lift)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, (</>))

-- | Domains with independent clocks
createDomain vXilinxSystem{vName="D100", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="D70", vPeriod=hzToPeriod 70e6}
createDomain vXilinxSystem{vName="D100Async", vPeriod=hzToPeriod 100e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="D70Async", vPeriod=hzToPeriod 70e6, vResetKind=Asynchronous}
-- | Fast clocks, with a collision window in the XPM's simulation model smaller
-- than the default 3000 ps
createDomain vXilinxSystem{vName="D400", vPeriod=2500}
createDomain vXilinxSystem{vName="D500", vPeriod=2000}
-- | Same period as 'D100', but a different domain: independent clocks whose
-- edges always coincide
createDomain vXilinxSystem{vName="D100b", vPeriod=hzToPeriod 100e6}

-- | Number of samples to check per port
type Samples = 300

-- | Inputs of a memory port
data PortStimuli dom depth nBytes w = PortStimuli
  { psReset :: Reset dom
  , psEnable :: Enable dom
  , psRegce :: Signal dom Bool
  , psAddress :: Signal dom (Index depth)
  , psWriteEnable :: Signal dom (BitVector nBytes)
  , psData :: Signal dom (BitVector w)
  }

-- | Xorshift pseudo random number generator
xorshift64 :: BitVector 64 -> BitVector 64
xorshift64 x0 = x3
 where
  x1 = x0 `xor` (x0 `shiftL` 13)
  x2 = x1 `xor` (x1 `shiftR` 7)
  x3 = x2 `xor` (x2 `shiftL` 17)

-- | Pseudo random number stream
prng :: KnownDomain dom => Clock dom -> BitVector 64 -> Signal dom (BitVector 64)
prng clk seed = r
 where
  r = register clk noReset enableGen seed (xorshift64 <$> r)

-- | Pseudo-random stimuli for a memory port. All signals come straight from
-- registers, so (asynchronous) resets do not glitch. The reset is asserted for
-- the first few cycles, and occasionally after that.
stimuli ::
  forall dom depth nBytes w.
  ( KnownDomain dom
  , KnownNat depth, 1 <= depth
  , KnownNat nBytes
  , KnownNat w, w <= 64 ) =>
  -- | Percentage of cycles the port writes, if enabled
  Unsigned 7 ->
  -- | Only write to addresses with this least significant bit. Used to prevent
  -- write/write collisions, see 'independentWrites'.
  Maybe Bit ->
  Clock dom ->
  -- | Seed
  BitVector 64 ->
  PortStimuli dom depth nBytes w
stimuli writePct writeParity clk seed = PortStimuli
  { psReset = unsafeFromActiveHigh rst
  , psEnable = toEnable en
  , psRegce = regce
  , psAddress = addr
  , psWriteEnable = we
  , psData = din
  }
 where
  ctrl = prng clk seed
  dat = prng clk (complement seed)
  cnt = register clk noReset enableGen (0 :: Index 4) (satSucc SatBound <$> cnt)
  (rst, en, regce, addr, we, din) =
    unbundle $ register clk noReset enableGen (True, False, False, 0, 0, 0)
             $ decode <$> cnt <*> ctrl <*> dat

  decode ::
    Index 4 -> BitVector 64 -> BitVector 64 ->
    (Bool, Bool, Bool, Index depth, BitVector nBytes, BitVector w)
  decode n c d = (rst1, en1, regce1, addr1, we1, truncateB' d)
   where
    rst1 = n < 3 || slice d13 d8 c == 0
    en1 = slice d1 d0 c /= 0
    regce1 = slice d4 d2 c /= 0
    doWrite = unpack (slice d20 d14 c) `mod` 100 < writePct && parityOk
    parityOk = case writeParity of
      Nothing -> True
      Just p -> lsb addr1 == p
    byteMask = resize (slice d31 d24 c) :: BitVector nBytes
    we1
      | not doWrite = 0
      | byteMask == 0 = maxBound
      | otherwise = byteMask
    addr1 = unpack (resize (slice d47 d32 c `mod` natToNum @depth))

  truncateB' :: BitVector 64 -> BitVector w
  truncateB' = resize

-- | With independent clocks, Vivado's simulation model of the XPM makes the
-- reads of /both/ ports undefined while one port is about to write an address
-- the other port wrote within the collision window, regardless of the address
-- they read. The Clash model does not model this, so tests with independent
-- clocks let port A write even and port B write odd addresses.
independentWrites :: (Maybe Bit, Maybe Bit)
independentWrites = (Just 0, Just 1)

-- | Test bench comparing the outputs of both ports of a memory with expected
-- values. Both clocks keep running until both ports are verified, so the ports
-- keep interacting like they do in the Haskell simulation.
tb ::
  forall domA domB w n.
  ( KnownDomain domA
  , KnownDomain domB
  , KnownNat w
  , KnownNat n, 1 <= n ) =>
  (Clock domA -> Clock domB -> (Signal domA (BitVector w), Signal domB (BitVector w))) ->
  (Vec n (BitVector w), Vec n (BitVector w)) ->
  Signal domA Bool
tb dut (expectedA, expectedB) = done
 where
  (outA, outB) = dut clkA clkB
  doneA = outputVerifierBitVector' clkA noReset expectedA outA
  doneB = outputVerifierBitVector' clkB noReset expectedB outB
  done = strictAnd <$> doneA <*> unsafeSynchronizer clkB clkA doneB
  strictAnd !a !b = a && b
  clkA = tbClockGen (not <$> done)
  clkB = tbClockGen (not <$> unsafeSynchronizer clkA clkB done)

-- | Expected outputs of a memory, as computed by the Haskell simulation model.
-- The last value is undefined, i.e., "don't care": 'outputVerifierBitVector''
-- keeps comparing with the last expected value after it is done, while the test
-- bench keeps running until both ports are verified.
expected ::
  forall domA domB w.
  ( KnownDomain domA
  , KnownDomain domB
  , KnownNat w ) =>
  (Clock domA -> Clock domB -> (Signal domA (BitVector w), Signal domB (BitVector w))) ->
  ExpQ
expected dut =
  [| ( $(listToVecTH (sampleN (natToNum @Samples) outA <> [undefined#]))
     , $(listToVecTH (sampleN (natToNum @Samples) outB <> [undefined#])) ) |]
 where
  (outA, outB) = dut clockGen clockGen

-- | Absolute path of a file in the directory @XpmMemory@ next to the module
-- calling this function
memFilePath :: FilePath -> ExpQ
memFilePath name = do
  loc <- location
  path <- runIO (makeAbsolute (takeDirectory (loc_filename loc) </> "XpmMemory" </> name))
  lift path
