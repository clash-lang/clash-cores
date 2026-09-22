{-# LANGUAGE NamedFieldPuns #-}

{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Gearboxes between the 4-bit SERDES interface at 312.5 MHz and 10-bit code
groups at 125 MHz. Five nibbles make two code groups, so pairs of code groups
cross the clock domains through a dual-clock FIFO: one pair per five cycles on
the SERDES side, one pair per two cycles on the code group side. Both clocks
come from the same MMCM, so the rates match exactly and the FIFO settles at a
constant fill after start-up.
-}
module Kcu105.Sgmii.Gearbox where

import Clash.Cores.Xilinx.DcFifo
import Clash.Explicit.Prelude
import Kcu105.Sgmii.Domains

-- | Two consecutive code groups, the first one first
type CodeGroupPair = (BitVector 10, BitVector 10)

-- | Sticky FIFO error flags
data FifoErrors = FifoErrors
  { fifoUnderflow :: Bool
  , fifoOverflow :: Bool
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Receive gearbox: collects nibbles (bit 0 the earliest bit) into a pair of
--   code groups every fifth cycle. Bit 0 of a code group is its earliest bit.
rxGearbox ::
  Clock Serdes312 ->
  Reset Serdes312 ->
  Signal Serdes312 (BitVector 4) ->
  Signal Serdes312 (Maybe CodeGroupPair)
rxGearbox clk rst = mealy clk rst enableGen go (0 :: Index 5, 0 :: BitVector 20)
 where
  go (n, acc) nibble = ((satSucc SatWrap n, acc'), out)
   where
    acc' = (acc `shiftR` 4) .|. (resize nibble `shiftL` 16)
    out
      | n == maxBound = Just (truncateB acc', truncateB (acc' `shiftR` 10))
      | otherwise = Nothing

-- | Transmit gearbox: emits the bits of a pair of code groups as five nibbles
-- (bit 0 first) and requests the next pair from the FIFO one cycle before it is
-- needed. When no pair can be read, the previous FIFO output is sent again.
txGearbox ::
  Clock Serdes312 ->
  Reset Serdes312 ->
  -- | FIFO output, valid the cycle after a read request
  Signal Serdes312 CodeGroupPair ->
  -- | Whether the FIFO may be read
  Signal Serdes312 Bool ->
  -- | Read request, nibble, and whether a pair was needed but none could be
  --   read (zeros are sent instead)
  (Signal Serdes312 Bool, Signal Serdes312 (BitVector 4), Signal Serdes312 Bool)
txGearbox clk rst fifoData canRead =
  mealyB clk rst enableGen go (0 :: Index 5, 0 :: BitVector 20, False) (fifoData, canRead)
 where
  -- The third state component records whether a read was issued in the
  -- previous cycle, so that the FIFO output is only used when it is valid.
  go (n, acc, pending) ((w0, w1), readable) =
    ((satSucc SatWrap n, acc', rdEn), (rdEn, truncateB acc, starved))
   where
    rdEn = n == 3 && readable
    starved = n == maxBound && not pending
    acc'
      | n == maxBound && pending = (resize w1 `shiftL` 10) .|. resize w0
      | n == maxBound = 0
      | otherwise = acc `shiftR` 4

-- | FIFO configuration: 15 pairs, with the error flags enabled
fifoConfig :: DcConfig 4
fifoConfig =
  DcConfig
    { dcDepth = d4
    , dcReadDataCount = False
    , dcWriteDataCount = False
    , dcOverflow = True
    , dcUnderflow = True
    }

-- | Accumulate sticky error flags
stickyErrors ::
  (KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom Bool ->
  Signal dom Bool ->
  Signal dom FifoErrors
stickyErrors clk rst under over = errors
 where
  errors =
    register clk rst enableGen (FifoErrors False False)
      $ ( \FifoErrors{fifoUnderflow, fifoOverflow} u o ->
            FifoErrors (fifoUnderflow || u) (fifoOverflow || o)
        )
      <$> errors
      <*> under
      <*> over

-- | Start-up delay: 'True' once the input has been 'True' for 16 cycles
startAfterFill :: (KnownDomain dom) => Clock dom -> Reset dom -> Signal dom Bool -> Signal dom Bool
startAfterFill clk rst filled = (== maxBound) <$> count
 where
  count =
    register clk rst enableGen (0 :: Index 17)
      $ (\c f -> if f then satSucc SatBound c else c)
      <$> count
      <*> filled

-- | Receive crossing: pairs of code groups from the SERDES clock to a
-- continuous stream of code groups on the code group clock. Reading starts once
-- the FIFO has been non-empty for a while, and then never stops; an empty FIFO
-- at that point is an underflow.
rxCrossing ::
  Clock Serdes312 ->
  Reset Serdes312 ->
  Clock Pcs125 ->
  Reset Pcs125 ->
  Signal Serdes312 (Maybe CodeGroupPair) ->
  (Signal Pcs125 (BitVector 10), Signal Pcs125 FifoErrors)
rxCrossing wClk wRst rClk rRst pairs = (cg, errors)
 where
  FifoOut{isOverflow, isEmpty, isUnderflow, fifoData} =
    dcFifo fifoConfig wClk wRst rClk rRst pairs rdEn
  running = startAfterFill rClk rRst (not <$> isEmpty)
  (rdEn, cg) = mealyB rClk rRst enableGen go (RxIdle, 0 :: BitVector 10) (fifoData, running)
  -- Once running, alternate between taking a pair from the FIFO (emitting its
  -- first code group and holding the second) and emitting the held code group
  -- while requesting the next pair. The FIFO output is only used in the cycle
  -- after a read request.
  go (phase, held) ((w0, w1), run) = case phase of
    RxIdle
      | run -> ((RxExpect, 0), (True, 0))
      | otherwise -> ((RxIdle, 0), (False, 0))
    RxExpect -> ((RxHold, w1), (False, w0))
    RxHold -> ((RxExpect, 0), (True, held))
  errors = stickyErrors rClk rRst isUnderflow (unsafeSynchronizer wClk rClk isOverflow)

-- | Phases of the receive crossing reader
data RxPhase = RxIdle | RxExpect | RxHold
  deriving (Generic, NFDataX, Eq, Show)

-- | Transmit crossing: a continuous stream of code groups on the code group
-- clock to nibbles on the SERDES clock
txCrossing ::
  Clock Pcs125 ->
  Reset Pcs125 ->
  Clock Serdes312 ->
  Reset Serdes312 ->
  Signal Pcs125 (BitVector 10) ->
  -- | Nibbles, sticky FIFO errors, and the number of pairs that could not be
  --   read in time (sent as zeros)
  (Signal Serdes312 (BitVector 4), Signal Pcs125 FifoErrors, Signal Pcs125 (Unsigned 16))
txCrossing wClk wRst rClk rRst cg = (nibble, errors, starvedCount)
 where
  -- Pair up consecutive code groups
  pairs = mealy wClk wRst enableGen pairUp (False, 0 :: BitVector 10) cg
  pairUp (second, held) w
    | second = ((False, 0), Just (held, w))
    | otherwise = ((True, w), Nothing)
  FifoOut{isOverflow, isEmpty, isUnderflow, fifoData} =
    dcFifo fifoConfig wClk wRst rClk rRst pairs rdEn
  running = startAfterFill rClk rRst (not <$> isEmpty)
  (rdEn, nibble, starved) = txGearbox rClk rRst fifoData (running .&&. (not <$> isEmpty))
  errors = stickyErrors wClk wRst (unsafeSynchronizer rClk wClk isUnderflow) isOverflow
  -- counted in the SERDES domain (after start-up), shown in the code group domain
  starvedR = register rClk rRst enableGen (0 :: Unsigned 16) $
    (\c s r -> if s && r then satSucc SatBound c else c) <$> starvedR <*> starved <*> running
  starvedCount = register wClk wRst enableGen 0 (unsafeSynchronizer rClk wClk starvedR)
