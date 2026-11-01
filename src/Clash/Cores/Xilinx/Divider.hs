{-|
Copyright  :  (C) 2026,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [Xilinx Divider Generator LogiCORE IP v5.1](https://docs.amd.com/v/u/en-US/pg151-div-gen).

The functions in this module make it possible to use the Xilinx IP in Clash.
Compilation will instantiate the Xilinx IP. Clash will output a TCL script
named @div_gen@/.../@.clash.tcl@ which needs to be executed in the Vivado
project to create the proper entity. Simulation in Clash produces bit-identical
results to the IP, cycle for cycle.

All IP instantiated by this module has the following properties:

* Non-blocking AXI4-Stream interfaces. @TVALID@ on the inputs is always
  asserted, @TVALID@ on the output is ignored.
* 1 cycle per division: a new division can start every clock cycle, and the
  result of each comes out after a fixed latency.
* A clock enable, no reset input
* Division by zero detection. Results of a division by zero are 'Nothing'.
  The IP's quotient and remainder are undefined in that case.
* No @TLAST@, no @TUSER@ on the inputs

The latency of the IP is set through the delay argument of the 'DSignal'.
Other customization is done through the 'E.Config' argument of 'quotRemWith'.
For each operation, there also exists a function that uses the defaults: the
maximum (fully pipelined) latency of the IP and, for 'quotRem', the settings
from 'E.defConfig'. That latency is also defined as a type for delay
annotation in circuits.

De-asserting the 'Enable' signal of a function will stall the whole pipeline.

The IP supports the following operations, see PG151 for details:

* 'quotRem': integer division with remainder, using the IP's /Radix2/ or
  /LutMult/ algorithm and its /Remainder/ output
* 'quotFrac': division with a quotient and a fractional part, using the
  IP's /Radix2/ algorithm and its /Fractional/ output

Not supported:

* Blocking mode, back pressure (@TREADY@), reset
* Passing @TLAST@ and @TUSER@ from the inputs to the output
* /Radix2/ with more than one clock cycle per division
* A latency of zero: the IP then has no clock input
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Xilinx.Divider
  ( -- * Instantiating IP
    quotRemWith
  , quotRem
  , quotFracWith
  , quotFrac
    -- * Operands
  , E.DividerOperand(E.Fraction, E.FractionWidth, E.QuotRemDefDelay, E.QuotFracDefDelay)
    -- * Customizing IP
  , E.Config(..)
  , E.defConfig
  , E.Algorithm(..)
  ) where

import Clash.Prelude hiding (quotRem)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Cores.Xilinx.Divider.Explicit as E
import Clash.Cores.Xilinx.Divider.Explicit (DividerOperand(..))

-- | Customizable integer division with remainder. See 'E.quotRemWith'.
quotRemWith ::
  forall d a m n dom k.
  ( HiddenClock dom
  , HiddenEnable dom
  , DividerOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat d
  , 1 <= d
  , HasCallStack
  ) =>
  E.Config ->
  -- | Dividend
  DSignal dom k (a m) ->
  -- | Divisor
  DSignal dom k (a n) ->
  -- | Quotient and remainder
  DSignal dom (k + d) (Maybe (a m, a n))
quotRemWith cfg = withFrozenCallStack $ hideEnable $ hideClock $ E.quotRemWith @d cfg
{-# INLINE quotRemWith #-}

-- | Integer division with remainder, with default settings. See 'E.quotRemWith'.
quotRem ::
  forall a m n dom k.
  ( HiddenClock dom
  , HiddenEnable dom
  , DividerOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat (QuotRemDefDelay a m)
  , 1 <= QuotRemDefDelay a m
  , HasCallStack
  ) =>
  -- | Dividend
  DSignal dom k (a m) ->
  -- | Divisor
  DSignal dom k (a n) ->
  -- | Quotient and remainder
  DSignal dom (k + QuotRemDefDelay a m) (Maybe (a m, a n))
quotRem = withFrozenCallStack $ hideEnable $ hideClock E.quotRem
{-# INLINE quotRem #-}

-- | Customizable division with fractional output. See 'E.quotFracWith'.
quotFracWith ::
  forall d a m n f dom k.
  ( HiddenClock dom
  , HiddenEnable dom
  , DividerOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat f
  , KnownNat (FractionWidth a f)
  , KnownNat d
  , 1 <= d
  , HasCallStack
  ) =>
  -- | Dividend
  DSignal dom k (a m) ->
  -- | Divisor
  DSignal dom k (a n) ->
  -- | Quotient and fraction
  DSignal dom (k + d) (Maybe (a m, Fraction a f))
quotFracWith = withFrozenCallStack $ hideEnable $ hideClock $ E.quotFracWith @d
{-# INLINE quotFracWith #-}

-- | Division with fractional output, with the default (maximum) latency. See
-- 'E.quotFracWith'.
quotFrac ::
  forall a m n f dom k.
  ( HiddenClock dom
  , HiddenEnable dom
  , DividerOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat f
  , KnownNat (FractionWidth a f)
  , KnownNat (QuotFracDefDelay a m f)
  , 1 <= QuotFracDefDelay a m f
  , HasCallStack
  ) =>
  -- | Dividend
  DSignal dom k (a m) ->
  -- | Divisor
  DSignal dom k (a n) ->
  -- | Quotient and fraction
  DSignal dom (k + QuotFracDefDelay a m f) (Maybe (a m, Fraction a f))
quotFrac = withFrozenCallStack $ hideEnable $ hideClock E.quotFrac
{-# INLINE quotFrac #-}
