{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities shared by the UNISIM primitive wrappers.
-}

{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Unisim.Internal where

import Clash.Explicit.Prelude

import Clash.Signal.Internal (Clock (..), Femtoseconds (..), Signal ((:-)))
import Data.Int (Int64)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal (InstConfig (..), instConfig)

-- | Instantiation config for the UNISIM primitive with the given name
unisim :: String -> InstConfig
unisim name =
  (instConfig name)
    { library = Just "UNISIM"
    , libraryImport = Just "UNISIM.vcomponents.all"
    }

{- | Device family to simulate. Some UNISIM primitives behave slightly
differently on UltraScale and UltraScale+ devices; Vivado requires the
@SIM_DEVICE@ attribute of those primitives to match the targeted device.
-}
data SimDevice
  = -- | UltraScale (Kintex, Virtex)
    UltraScale
  | -- | UltraScale+ (Kintex, Virtex, Zynq, Artix)
    UltraScalePlus
  deriving (Show, Eq, Generic, NFDataX)

-- | Value of the @SIM_DEVICE@ attribute for a 'SimDevice'
simDeviceString :: SimDevice -> String
simDeviceString UltraScale = "ULTRASCALE"
simDeviceString UltraScalePlus = "ULTRASCALE_PLUS"

-- | Periods of a clock in femtoseconds, one per clock cycle
clockPeriodsFs :: forall dom. (KnownDomain dom) => Clock dom -> Signal dom Int64
clockPeriodsFs (Clock _ (Just periods)) = (\(Femtoseconds fs) -> fs) <$> periods
clockPeriodsFs (Clock _ Nothing) = pure (1000 * snatToNum (clockPeriod @dom))

{- | Simulation model of a clock buffer that gates and divides its input
clock, used for @BUFGCE@ (@divider = 1@), @BUFGCE_DIV@ and @IBUFDS_GTE4@.

Clash samples a signal at the active edges of its clock: sample /k/ is the
value present at edge /k/, where edge 0 is at the start of the simulation. The
clear and enable values present at input edge /k/ determine what happens at
that edge:

  * If clear is asserted, no output edge is produced and the divider restarts:
    the next counted input edge produces an output edge.

  * Otherwise, if enable is asserted, the input edge is counted. Every
    /divider/th counted edge, starting with the first one, produces an output
    edge.

The resulting clock has a dynamic period: the time between two output edges.
Like every clock in Clash, it has an edge at the start of the simulation; this
edge stands in for the first output edge. Hence, all output edges after the
first one happen at the right time, and the samples of the output domain are
right, but edges of the input clock before the first output edge sample the
output domain as if the first output edge happened at the start of the
simulation.

A clock that stops forever makes any computation that needs its next clock edge
hang.
-}
gatedClockModel ::
  forall domIn domOut.
  (KnownDomain domIn, KnownDomain domOut) =>
  -- | Divider, at least 1
  Int ->
  Clock domIn ->
  -- | Clear
  Signal domIn Bool ->
  -- | Enable
  Signal domIn Bool ->
  Clock domOut
gatedClockModel divider clkIn clr0 ena0 =
  Clock SSymbol (Just (Femtoseconds <$> goFirst 0 0 (clockPeriodsFs clkIn) clr0 ena0))
 where
  -- Whether input edge produces an output edge, and the next divider phase.
  -- The phase is the number of counted edges since the last output edge,
  -- modulo the divider.
  step :: Int -> Bool -> Bool -> (Bool, Int)
  step phase clr ena = (counted && phase == 0, phase1)
   where
    counted = ena && not clr
    phase1
      | clr = 0
      | counted = (phase + 1) `mod` divider
      | otherwise = phase

  -- Before the first output edge. @t@ is the time of the current input edge.
  goFirst ::
    Int -> Int64 ->
    Signal domIn Int64 -> Signal domIn Bool -> Signal domIn Bool ->
    Signal domOut Int64
  goFirst !phase !t (p :- ps) (c :- cs) (e :- es)
    | edge = go phase1 (t + p) ps cs es
    | otherwise = goFirst phase1 (t + p) ps cs es
   where
    (edge, phase1) = step phase c e

  -- After the first output edge. @acc@ is the time between the previous output
  -- edge and the current input edge.
  go ::
    Int -> Int64 ->
    Signal domIn Int64 -> Signal domIn Bool -> Signal domIn Bool ->
    Signal domOut Int64
  go !phase !acc (p :- ps) (c :- cs) (e :- es)
    | edge = acc :- go phase1 p ps cs es
    | otherwise = go phase1 (acc + p) ps cs es
   where
    (edge, phase1) = step phase c e
