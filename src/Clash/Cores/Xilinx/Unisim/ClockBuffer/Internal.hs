{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Simulation model of the @BUFGCTRL@ clock multiplexer, and the types of its
configuration and control signals.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Unisim.ClockBuffer.Internal where

import Clash.Explicit.Prelude

import Clash.Signal.Internal (Clock (..), Femtoseconds (..), Signal ((:-)))
import Data.Int (Int64)

import qualified Data.List as L

import Clash.Cores.Xilinx.Unisim.Internal (clockPeriodsFs)

-- | One of the two clock inputs of 'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgctrl'
data BufgctrlInput
  = -- | Clock input @I0@
    BufgctrlI0
  | -- | Clock input @I1@
    BufgctrlI1
  deriving (Show, Eq, Generic, NFDataX)

-- | Configuration of 'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgctrl'
data BufgctrlConfig = BufgctrlConfig
  { bufgctrlPreselect :: Maybe BufgctrlInput
  -- ^ Input selected right after configuration, before the control signals
  -- have had any effect (@PRESELECT_I0@, @PRESELECT_I1@). With 'Nothing', no
  -- input is selected until the control signals select one.
  , bufgctrlInitOut :: Bit
  -- ^ Level of the output while no input is selected (@INIT_OUT@). This also
  -- determines at which edges of the inputs the selection changes: their
  -- falling edges if 0, and their rising edges if 1.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Control signals of one of the two inputs of
-- 'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgctrl'. An input is selected if
-- both its select and its clock enable are asserted, and the other input is
-- not selected.
data BufgctrlControl = BufgctrlControl
  { bufgctrlS :: Bool
  -- ^ Select (@S0@, @S1@)
  , bufgctrlCe :: Bool
  -- ^ Clock enable (@CE0@, @CE1@)
  , bufgctrlIgnore :: Bool
  -- ^ Ignore the input clock when switching (@IGNORE0@, @IGNORE1@): if
  -- asserted, the input is selected and deselected as soon as its select or
  -- enable change, instead of at the input's next falling edge (rising edge if
  -- 'bufgctrlInitOut' is 1). This makes the output glitch, but allows switching
  -- away from an input clock that stopped.
  }
  deriving (Show, Eq, Generic, NFDataX)

{- | State of the four latches of @BUFGCTRL@, along with the current levels of
its inputs. Follows the UNISIM simulation model (@BUFGCTRL.v@):

  * @qb00@: transparent while @I0@ is high (low if @INIT_OUT@ is 1), or if
    @IGNORE0@ is asserted. Its input is @S0@, gated by "@I1@ is not
    selected".

  * @qb01@: transparent while @I0@ is low (high if @INIT_OUT@ is 1), or if
    @IGNORE0@ is asserted. Its input is @qb00@, gated by @CE0@. @I0@ is
    selected if it is set.

  * @qb10@, @qb11@: the same for @I1@.
-}
data BufgctrlState = BufgctrlState
  { stI0 :: !Bool
  , stI1 :: !Bool
  , stCtrl0 :: BufgctrlControl
  , stCtrl1 :: BufgctrlControl
  , stQb00 :: !Bool
  , stQb01 :: !Bool
  , stQb10 :: !Bool
  , stQb11 :: !Bool
  }
  deriving (Show, Eq)

-- | Something that happens to the inputs of @BUFGCTRL@
data BufgctrlEvent
  = -- | New level of @I0@
    EvI0 !Bool
  | -- | New level of @I1@
    EvI1 !Bool
  | -- | New control signals of @I0@
    EvCtrl0 BufgctrlControl
  | -- | New control signals of @I1@
    EvCtrl1 BufgctrlControl

{- | An event with its time in femtoseconds and its priority. Events with the
same time are ordered by priority: clock edges (priority 0) come before changes
of control signals (priority 1). A control signal driven by a register changes
just /after/ the clock edge of that register, so when that edge coincides with
an edge of an input clock, the latches see the input clock change first.
-}
type TimedEvent = (Int64, Int, BufgctrlEvent)

{- | Edges of a clock with the given periods: a rising edge at the start of
every period, and a falling edge halfway. Clash clocks have no duty cycle, so
this assumes a duty cycle of 50%, like the clocks Clash generates in HDL test
benches.
-}
clockEvents :: (Bool -> BufgctrlEvent) -> Signal dom Int64 -> [TimedEvent]
clockEvents mk = go 0
 where
  go !t (p :- ps) = (t, 0, mk True) : (t + p `quot` 2, 0, mk False) : go (t + p) ps

{- | The initial value of a signal, and its changes. Sample /k + 1/ of a signal
is the value it takes right after edge /k/ of its clock. The samples are
matched lazily: an event's time only depends on the periods of the clock, so
the model can find out which event comes next without evaluating any sample.
-}
signalEvents ::
  (a -> BufgctrlEvent) -> Signal dom Int64 -> Signal dom a -> (a, [TimedEvent])
signalEvents mk periods ~(x0 :- xs0) = (x0, go 0 periods xs0)
 where
  go !t (p :- ps) ~(x :- xs) = (t, 1, mk x) : go (t + p) ps xs

-- | Merge two streams of events ordered by time and priority
mergeEvents :: [TimedEvent] -> [TimedEvent] -> [TimedEvent]
mergeEvents [] ys = ys
mergeEvents xs [] = xs
mergeEvents xs@(x@(tx, px, _) : xs0) ys@(y@(ty, py, _) : ys0)
  | (tx, px) <= (ty, py) = x : mergeEvents xs0 ys
  | otherwise = y : mergeEvents xs ys0

applyEvent :: BufgctrlState -> BufgctrlEvent -> BufgctrlState
applyEvent st = \case
  EvI0 b -> st{stI0 = b}
  EvI1 b -> st{stI1 = b}
  EvCtrl0 c -> st{stCtrl0 = c}
  EvCtrl1 c -> st{stCtrl1 = c}

-- | Level of the output of @BUFGCTRL@ in the given state
outputLevel :: Bool -> BufgctrlState -> Bool
outputLevel initOut st =
  case (stQb01 st, stQb11 st) of
    (True, False) -> stI0 st
    (False, True) -> stI1 st
    (False, False) -> initOut
    (True, True) -> False

{- | Let the transparent latches settle. In the UNISIM model, every latch has a
delay of 1 ps: a change of a latch's inputs at time /t/ changes its output at
/t + 1 ps/. This function does the same in steps: in every step, every
transparent latch takes the value its inputs had in the previous step, until
nothing changes anymore. Returns the output level after every step, so that the
caller can see glitches, and the final state.

For example, when switching from @I1@ to @I0@ while both @IGNORE@ inputs are
asserted, @I1@ gets deselected before @I0@ gets selected, so the output briefly
takes the value of @INIT_OUT@. And when @S1@ is deasserted and @CE1@ asserted at
the same time, @I1@ is selected for a single step, as @qb11@ still sees the old
value of @qb10@.

If the latches do not settle, which happens when both inputs get selected at
the same moment while ignoring their clocks, the UNISIM model oscillates. The
model raises an error then: the hardware's behaviour is undefined.
-}
settle :: Bool -> BufgctrlState -> ([Bool], BufgctrlState)
settle initOut = go (16 :: Int)
 where
  go 0 _ =
    error
      "bufgctrl: the latches do not settle, as both inputs got selected at the same time. The output is undefined."
  go n st
    | latches st1 == latches st = ([], st)
    | otherwise = let (ls, stEnd) = go (n - 1) st1 in (outputLevel initOut st1 : ls, stEnd)
   where
    c0 = stCtrl0 st
    c1 = stCtrl1 st
    -- Latch enables. The latches switch on the falling edges of the inputs if
    -- INIT_OUT is 0, and on their rising edges if INIT_OUT is 1.
    hi0 = stI0 st /= initOut
    hi1 = stI1 st /= initOut
    latch en new old = if en then new else old
    st1 =
      st
        { stQb00 = latch (bufgctrlIgnore c0 || hi0) (not (stQb11 st) && bufgctrlS c0) (stQb00 st)
        , stQb01 = latch (bufgctrlIgnore c0 || not hi0) (stQb00 st && bufgctrlCe c0) (stQb01 st)
        , stQb10 = latch (bufgctrlIgnore c1 || hi1) (not (stQb01 st) && bufgctrlS c1) (stQb10 st)
        , stQb11 = latch (bufgctrlIgnore c1 || not hi1) (stQb10 st && bufgctrlCe c1) (stQb11 st)
        }
    latches s = (stQb00 s, stQb01 s, stQb10 s, stQb11 s)

{- | Times (in femtoseconds) of the rising edges of the output of @BUFGCTRL@,
given the initial state and the events on its inputs. Events with the same time
and priority are applied together, after which the latches settle. Like in the
UNISIM model, step /k/ of 'settle' happens /k/ ps after the events. Every time
the output rises, during any of these steps, it produces an edge, so glitches
produce edges too.

This assumes that events of different times are further apart than the time
the latches need to settle, which is a few picoseconds.
-}
bufgctrlEdges :: Bool -> BufgctrlState -> [TimedEvent] -> [Int64]
bufgctrlEdges initOut st0 = go st0 (outputLevel initOut st0)
 where
  go _ _ [] = []
  go st o (ev@(t, p, _) : evs0) =
    [ t + 1000 * k
    | (k, prev, new) <- L.zip3 [0 ..] (o : levels) levels
    , not prev && new
    ]
      <> go st2 o2 evs1
   where
    (same, evs1) = L.span (\(t', p', _) -> t' == t && p' == p) evs0
    st1 = L.foldl' applyEvent st [e | (_, _, e) <- ev : same]
    (settleLevels, st2) = settle initOut st1
    levels = outputLevel initOut st1 : settleLevels
    o2 = L.last levels

{- | Simulation model of @BUFGCTRL@. See
'Clash.Cores.Xilinx.Unisim.ClockBuffer.bufgctrl'.

The output clock gets the periods between consecutive rising edges of the
output. Like every clock in Clash, it has an edge at the start of the
simulation; that edge stands in for the first rising edge of the output.
-}
bufgctrlModel ::
  forall dom0 domCtrl0 dom1 domCtrl1 domOut.
  ( KnownDomain dom0
  , KnownDomain domCtrl0
  , KnownDomain dom1
  , KnownDomain domCtrl1
  , KnownDomain domOut
  ) =>
  BufgctrlConfig ->
  Clock dom0 ->
  Clock domCtrl0 ->
  Signal domCtrl0 BufgctrlControl ->
  Clock dom1 ->
  Clock domCtrl1 ->
  Signal domCtrl1 BufgctrlControl ->
  Clock domOut
bufgctrlModel cfg clk0 clkCtrl0 ctrl0 clk1 clkCtrl1 ctrl1 =
  Clock SSymbol (Just (Femtoseconds <$> periods edges))
 where
  initOut = bufgctrlInitOut cfg == 1
  pre0 = bufgctrlPreselect cfg == Just BufgctrlI0
  pre1 = bufgctrlPreselect cfg == Just BufgctrlI1

  (c0, evsCtrl0) = signalEvents EvCtrl0 (clockPeriodsFs clkCtrl0) ctrl0
  (c1, evsCtrl1) = signalEvents EvCtrl1 (clockPeriodsFs clkCtrl1) ctrl1

  events =
    mergeEvents
      (mergeEvents (clockEvents EvI0 (clockPeriodsFs clk0)) (clockEvents EvI1 (clockPeriodsFs clk1)))
      (mergeEvents evsCtrl0 evsCtrl1)

  -- The clocks are low before the start of the simulation, and the latches
  -- have the values PRESELECT_I0 and PRESELECT_I1 give them.
  stInit =
    snd $
      settle initOut $
        BufgctrlState
          { stI0 = False
          , stI1 = False
          , stCtrl0 = c0
          , stCtrl1 = c1
          , stQb00 = pre0
          , stQb01 = pre0
          , stQb10 = pre1
          , stQb11 = pre1
          }

  edges = bufgctrlEdges initOut stInit events

  -- The first edge of the output is replaced by the edge Clash puts at the
  -- start of the simulation.
  periods :: [Int64] -> Signal domOut Int64
  periods [] = errorX "bufgctrl: the output clock has no edges"
  periods (_ : es) = go 0 es
   where
    go _ [] = errorX "bufgctrl: the output clock stopped"
    go prev (e : es0) = (e - prev) :- go e es0
