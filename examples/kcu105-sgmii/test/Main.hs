{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation tests of the KCU105 SGMII test design: the SERDES models and
gearboxes round-trip data, and the whole design brings up a link with itself
when its transmit line is looped back to its receive line.
-}
module Main (main) where

import Clash.Cores.LineCoding.Lc8b10b (decode8b10b, isValidSymbol)
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench (clockToDiffClock)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Kcu105.Sgmii.Domains
import Kcu105.Sgmii.Gearbox
import Kcu105.Sgmii.Primitives
import Kcu105.Sgmii.Top
import Test.Tasty
import Test.Tasty.HUnit
import qualified Prelude as P

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "kcu105-sgmii"
    [ testCase "serializer and deserializer models round-trip" serdesModelRoundTrip
    , testCase "gearboxes round-trip" gearboxRoundTrip
    , testCase "clock crossings round-trip" crossingRoundTrip
    , testCase "design synchronises to its own transmit line" loopbackSync
    ]

-- | Code groups sent through the transmit crossing, the receive gearbox and
--   the receive crossing come out as a contiguous stretch of the input
crossingRoundTrip :: Assertion
crossingRoundTrip = do
  let
    clkPcs = clockGen @Pcs125
    rstPcs = resetGen @Pcs125
    clkDiv = clockGen @Serdes312
    rstDiv = resetGen @Serdes312
    inp = fromList (P.map fromIntegral [1 :: Int ..]) :: Signal Pcs125 (BitVector 10)
    (nibbles, txErrors) = txCrossing clkPcs rstPcs clkDiv rstDiv inp
    (out, rxErrors) = rxCrossing clkDiv rstDiv clkPcs rstPcs (rxGearbox clkDiv rstDiv nibbles)
    observed = P.drop 200 (sampleN 400 out)
    first = P.fromIntegral (P.sum (P.take 1 observed)) :: Int
    expected = P.map fromIntegral [first .. first + 150]
    errors = (P.last (sampleN 400 txErrors), P.last (sampleN 400 rxErrors))
  assertEqual "no FIFO errors" (FifoErrors False False, FifoErrors False False) errors
  assertBool ("not contiguous: " P.++ show (P.take 24 observed)) (isInfixOfList observed expected)

-- | Bits of a stream of nibbles, bit 0 of each nibble first
bits :: [BitVector 4] -> [Bit]
bits = P.concatMap (\n -> [n ! i | i <- [0 .. 3 :: Int]])

-- | Whether the second list occurs in the first one
isInfixOfList :: (Eq a) => [a] -> [a] -> Bool
isInfixOfList = flip L.isInfixOf

-- | The nibbles that come out of the deserializer model equal the nibbles that
--   went into the serializer model, up to a constant bit offset
serdesModelRoundTrip :: Assertion
serdesModelRoundTrip = do
  let
    clkDiv = clockGen @Serdes312
    rstDiv = resetGen @Serdes312
    inp = P.take 200 (P.cycle [0x1, 0x2, 0x4, 0x8, 0xF, 0x0, 0x9, 0x6, 0x3, 0xC, 0xA, 0x5])
    line = oserdese3 (clockGen @Serdes625) clkDiv rstDiv (fromList (inp P.++ P.repeat 0))
    out = sampleN 200 (iserdese3 (clockGen @Serdes625) clkDiv rstDiv line)
    -- Drop the start-up samples on both sides
    expected = bits (P.take 150 (P.drop 10 inp))
    observed = bits (P.drop 10 out)
  assertBool ("stream not found: " P.++ show (P.take 40 observed)) (isInfixOfList observed expected)

-- | The pairs that come out of the receive gearbox equal the pairs that went
--   into the transmit gearbox
gearboxRoundTrip :: Assertion
gearboxRoundTrip = do
  let
    clk = clockGen @Serdes312
    rst = resetGen @Serdes312
    pairs = [(fromIntegral i, fromIntegral (1000 - i)) | i <- [1 .. 60 :: Int]] :: [CodeGroupPair]
    -- A FIFO model: the output advances to the next pair the cycle after a read
    fifoData = mealy clk rst enableGen fifoStep (0 :: Int) rdEn
    fifoStep n rd = (if rd then n + 1 else n, pairs P.!! min n (P.length pairs - 1))
    (rdEn, nibbles) = txGearbox clk rst fifoData (pure True)
    out = catMaybes (sampleN 400 (rxGearbox clk rst nibbles))
    expected = P.take 40 (P.drop 5 pairs)
  assertBool ("pairs not found: " P.++ show (P.take 8 out)) (isInfixOfList out expected)

-- | With the transmit line connected to the receive line, the PCS acquires
--   comma alignment and synchronisation on its own idles and stays synchronised
loopbackSync :: Assertion
loopbackSync = do
  let
    clk125 = clockGen @Ext125
    rst125 = resetGen @Ext125
    diffClk625 = clockToDiffClock (clockGen @Phy625)
    demo = sgmiiDemo clk125 rst125 diffClk625 (demoTxP demo) (demoTxN demo) (pure defaultControl)
    observes = sampleN 4000 (demoObserve demo)
    okFrom n = P.all (\o -> obsSyncOk o && obsBsOk o) (P.drop n observes)
    observe = P.last observes
    txCgs = sampleN 4000 (demoTxCg demo)
    rxCgs = sampleN 4000 (demoRxCg demo)
    cgBits = P.concatMap (\w -> [w ! i | i <- [0 .. 9 :: Int]])
    -- the receive stream must contain a long stretch of the transmit stream
    streamOk = isInfixOfList (cgBits (P.drop 500 rxCgs)) (cgBits (P.take 1000 (P.drop 500 txCgs)))
  assertBool ("receive stream is not the transmit stream; tx: " P.++ show (P.take 12 (P.drop 600 txCgs)) P.++ " rx: " P.++ show (P.take 12 (P.drop 600 rxCgs))) streamOk
  assertBool ("sync never OK; bs OK cycles: " P.++ show (P.length (P.filter obsBsOk observes)) P.++ ", sync OK cycles: " P.++ show (P.length (P.filter obsSyncOk observes)) P.++ ", first rx: " P.++ show (P.take 12 (P.drop 300 rxCgs))) (P.any obsSyncOk observes)
  assertBool
    ("FIFO errors: " P.++ show (obsRxFifo observe, obsTxFifo observe))
    (pack (obsRxFifo observe) == 0 && pack (obsTxFifo observe) == 0)
  let
    syncs = P.map obsSyncOk observes
    transitions = [i | (i, a, b) <- P.zip3 [0 :: Int ..] syncs (P.drop 1 syncs), a /= b]
    firstLoss = case [i | (i, True, False) <- P.zip3 [0 :: Int ..] syncs (P.drop 1 syncs), i > 3000] of
      i : _ -> i
      [] -> 3000
    bsCgs = sampleN 4000 (demoBsCg demo)
    around = P.take 24 (P.drop (firstLoss - 12) bsCgs)
    -- decode the window with both running disparities as a starting point
    decoded rd0 = snd (L.mapAccumL (\rd cg -> let (rd', sym) = decode8b10b rd cg in (rd', sym)) rd0 around)
  let
    txDecoded = snd (L.mapAccumL (\rd cg -> let (rd', sym) = decode8b10b rd cg in (rd', (cg, sym))) False txCgs)
    txInvalid = [(i, cg, sym) | (i, (cg, sym)) <- P.zip [0 :: Int ..] txDecoded, not (isValidSymbol sym)]
  assertBool
    ( "not synchronised; "
        P.++ show (P.length transitions)
        P.++ " sync transitions, first at: "
        P.++ show (P.take 12 transitions)
        P.++ ", last at: "
        P.++ show (P.drop (P.length transitions - 6) transitions)
        P.++ "\ninvalid code groups in the transmit stream ("
        P.++ show (P.length txInvalid)
        P.++ "): "
        P.++ show (P.take 10 txInvalid)
        P.++ "\ntransmit stream around the first invalid one: "
        P.++ show (P.take 12 (P.drop (P.maximum (0 : [i - 6 | (i, _, _) <- P.take 1 txInvalid])) txDecoded))
        P.++ "\naligned code groups around a late loss (cycle "
        P.++ show firstLoss
        P.++ "): "
        P.++ show around
        P.++ "\nraw received code groups there: "
        P.++ show (P.take 24 (P.drop (firstLoss - 12) rxCgs))
        P.++ "\ntransmitted code groups there: "
        P.++ show (P.take 24 (P.drop (firstLoss - 40) txCgs))
        P.++ "\nobservations there (bsOk, syncOk, xmit, frames): "
        P.++ show (P.map (\o -> (obsBsOk o, obsSyncOk o, obsXmit o, obsFrames o)) (P.take 24 (P.drop (firstLoss - 12) observes)))
        P.++ "\ndecoded from RD-: "
        P.++ show (decoded False)
        P.++ "\ndecoded from RD+: "
        P.++ show (decoded True)
    )
    (okFrom 2000)
