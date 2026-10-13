{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  I2C controller tests
-}
module Test.Cores.I2c where

import qualified Data.List as L
import Prelude

import Clash.Hedgehog.Sized.BitVector (genDefinedBitVector)
import Clash.Prelude (BitVector, Index, SaturationMode (SatWrap), Unsigned, Vec)
import qualified Clash.Prelude as C

import Hedgehog (Gen, PropertyT, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import Test.Cores.I2c.Model

genClkCnt :: Gen (Unsigned 16)
genClkCnt = Gen.integral (Range.linear 3 20)

genStretch :: Gen (Unsigned 16)
genStretch = Gen.frequency [(1, pure 0), (3, Gen.integral (Range.linear 1 30))]

genIndex :: Gen (Index 16)
genIndex = Gen.enumBounded

genRegs :: Gen (Vec 16 (BitVector 8))
genRegs = sequenceA (C.repeat genDefinedBitVector)

genAddr :: Gen (BitVector 7)
genAddr = genDefinedBitVector

-- | Register pointer, and bytes to write starting at that pointer
genWrite :: Gen (Index 16, [BitVector 8])
genWrite = (,) <$> genIndex <*> Gen.list (Range.linear 1 4) genDefinedBitVector

-- | Randomly delay commands
genDelays :: [Cmd] -> Gen [Cmd]
genDelays = traverse (\cmd -> (\d -> cmd{delay = d}) <$> genDelay)
 where
  genDelay = Gen.frequency [(3, pure 0), (1, Gen.int (Range.linear 1 100))]

-- | Expected register file after writing
applyWrites :: Vec 16 (BitVector 8) -> [(Index 16, [BitVector 8])] -> Vec 16 (BitVector 8)
applyWrites = L.foldl applyWrite
 where
  applyWrite regs (ptr, bytes) =
    L.foldl (\r (i, b) -> C.replace i b r) regs (L.zip (L.iterate succWrap ptr) bytes)

succWrap :: Index 16 -> Index 16
succWrap = C.satSucc SatWrap

-- | Upper bound on the number of cycles needed to execute commands
maxCycles :: Unsigned 16 -> Unsigned 16 -> [Cmd] -> Int
maxCycles clkCnt stretch cmds =
  L.sum (L.map (.delay) cmds)
    + L.length cmds * 10 * (8 * (fromIntegral clkCnt + fromIntegral stretch + 1) + 100)

runSim ::
  (H.MonadTest m) =>
  Int ->
  TargetConfig ->
  [(Unsigned 16, [Cmd])] ->
  m ([[Observation]], Vec 16 (BitVector 8))
runSim n cfg controllers = case simulate n cfg controllers of
  Nothing -> do
    H.footnote ("Simulation did not finish within " <> show n <> " cycles")
    H.failure
  Just samples ->
    pure (L.transpose (L.map fst samples), snd (L.last samples))

-- | Check that a controller completed all its commands without losing
-- arbitration
assertCompleted :: (Monad m) => [Cmd] -> [Observation] -> PropertyT m ()
assertCompleted cmds obss = do
  L.length (results obss) === L.length cmds
  H.assert (not (any (.al) obss))
  H.assert (not (L.last obss).busy)

-- | Write random data to registers, and read back random registers
prop_writeRead :: H.Property
prop_writeRead = H.withTests 50 $ H.property $ do
  clkCnt <- H.forAll genClkCnt
  stretch <- H.forAll genStretch
  regs0 <- H.forAll genRegs
  writes <- H.forAll (Gen.list (Range.linear 1 2) genWrite)
  rdPtr <- H.forAll genIndex
  rdN <- H.forAll (Gen.int (Range.linear 1 8))
  cmds <-
    H.forAll $
      genDelays $
        L.concatMap (uncurry (writeTransaction targetAddr)) writes
          <> readTransaction targetAddr rdPtr rdN

  let cfg = TargetConfig{addr = targetAddr, stretch, regs = regs0}
      expectedRegs = applyWrites regs0 writes
  ([obss], regs) <- runSim (maxCycles clkCnt stretch cmds) cfg [(clkCnt, cmds)]

  let res = L.zip cmds (results obss)
  assertCompleted cmds obss
  H.assert (any (.busy) obss)
  -- All written bytes are acknowledged by the target
  [ack | (cmd, (_, ack)) <- res, cmd.write] === [False | cmd <- cmds, cmd.write]
  -- Reads return the register contents
  [dout | (cmd, (dout, _)) <- res, cmd.read]
    === L.take rdN [expectedRegs C.!! i | i <- L.iterate succWrap rdPtr]
  regs === expectedRegs

-- | Writes to a different address are not acknowledged
prop_nack :: H.Property
prop_nack = H.withTests 20 $ H.property $ do
  clkCnt <- H.forAll genClkCnt
  regs0 <- H.forAll genRegs
  addr <- H.forAll (Gen.filter (/= targetAddr) genAddr)
  (ptr, bytes) <- H.forAll genWrite
  let cmds = writeTransaction addr ptr bytes
      cfg = TargetConfig{addr = targetAddr, stretch = 0, regs = regs0}
  ([obss], regs) <- runSim (maxCycles clkCnt 0 cmds) cfg [(clkCnt, cmds)]

  assertCompleted cmds obss
  [ack | (cmd, (_, ack)) <- L.zip cmds (results obss), cmd.write]
    === [True | cmd <- cmds, cmd.write]
  regs === regs0

{- | Two controllers start writing at (nearly) the same time. The controller
writing the lowest address wins arbitration, and completes its transaction.
-}
prop_arbitration :: H.Property
prop_arbitration = H.withTests 50 $ H.property $ do
  clkCnt <- H.forAll genClkCnt
  regs0 <- H.forAll genRegs
  addrA <- H.forAll genAddr
  addrB <- H.forAll (Gen.filter (/= addrA) genAddr)
  addr <- H.forAll (Gen.element [addrA, addrB])
  writeA <- H.forAll genWrite
  writeB <- H.forAll genWrite
  offset <- H.forAll (Gen.int (Range.linear 0 (fromIntegral clkCnt `div` 4)))

  let cmdsA = uncurry (writeTransaction addrA) writeA
      cmdsB = case uncurry (writeTransaction addrB) writeB of
        cmd : cmds -> cmd{delay = offset} : cmds
        [] -> []
      cfg = TargetConfig{addr, stretch = 0, regs = regs0}
      n = maxCycles clkCnt 0 (cmdsA <> cmdsB)
  ([obssA, obssB], regs) <- runSim n cfg [(clkCnt, cmdsA), (clkCnt, cmdsB)]

  let (winnerAddr, winnerWrite, winnerCmds, winnerObss, loserObss)
        | addrA < addrB = (addrA, writeA, cmdsA, obssA, obssB)
        | otherwise = (addrB, writeB, cmdsB, obssB, obssA)
      addressed = winnerAddr == addr

  H.assert (any (.al) loserObss)
  assertCompleted winnerCmds winnerObss
  [ack | (cmd, (_, ack)) <- L.zip winnerCmds (results winnerObss), cmd.write]
    === [not addressed | cmd <- winnerCmds, cmd.write]
  regs === if addressed then applyWrites regs0 [winnerWrite] else regs0

{- | A controller that is not issuing any commands observes the bus being busy,
but does not lose arbitration, while another controller uses the bus.
-}
prop_bystander :: H.Property
prop_bystander = H.withTests 20 $ H.property $ do
  clkCntA <- H.forAll genClkCnt
  clkCntB <- H.forAll genClkCnt
  regs0 <- H.forAll genRegs
  write <- H.forAll genWrite
  let cmdsB = uncurry (writeTransaction targetAddr) write
      cfg = TargetConfig{addr = targetAddr, stretch = 0, regs = regs0}
      n = maxCycles (max clkCntA clkCntB) 0 cmdsB
  ([obssA, obssB], regs) <- runSim n cfg [(clkCntA, []), (clkCntB, cmdsB)]

  assertCompleted [] obssA
  H.assert (any (.busy) obssA)
  assertCompleted cmdsB obssB
  regs === applyWrites regs0 [write]

tests :: TestTree
tests = $(testGroupGenerator)
