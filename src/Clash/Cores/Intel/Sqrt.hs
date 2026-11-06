{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's @altsqrt@ megafunction (the ALTSQRT IP), an integer square
root. See the
[Integer Arithmetic IP Cores User Guide (UG-01063)](https://www.intel.com/content/www/us/en/docs/programmable/683490/).

Like @lpm_divide@ (see "Clash.Cores.Intel.Divide"), @altsqrt@ is a
parameterized module that Clash instantiates directly. Its simulation models
are part of Intel's @altera_mf@ simulation library.

The latency is set through the delay argument of the 'DSignal'. A latency of
zero results in a combinational square root. The pipeline is stalled when the
'Enable' is deasserted, and cleared by the (asynchronous) reset. The outputs
during reset, and until the pipeline has filled again, are undefined in
simulation.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Sqrt
  ( sqrt
  , sqrtModel
  ) where

import Clash.Explicit.Prelude hiding (sqrt)

import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Signal.Delayed as D

import Clash.Cores.Intel.Floating.Internal (pipeline)
import Clash.Cores.Intel.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (unPort)

-- | Simulation model of @altsqrt@: the integer square root @q@ of the radical
-- @x@, rounded down, and the remainder @x - q * q@.
sqrtModel ::
  forall n.
  KnownNat n =>
  Unsigned n ->
  (Unsigned (DivRU n 2), Unsigned (DivRU n 2 + 1))
sqrtModel x = (fromInteger q, fromInteger (toInteger x - q * q))
 where
  q = isqrt (toInteger x)
  isqrt :: Integer -> Integer
  isqrt 0 = 0
  isqrt v = go v
   where
    go y = let y' = (y + v `quot` y) `quot` 2 in if y' >= y then y else go y'

-- | Integer square root using @altsqrt@. Returns the square root, rounded
-- down, and the remainder, see 'sqrtModel'.
sqrt ::
  forall d dom k n.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat n
  , 1 <= n
  , HasCallStack
  ) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Radical
  DSignal dom k (Unsigned n) ->
  -- | Square root and remainder
  DSignal dom (k + d) (Unsigned (DivRU n 2), Unsigned (DivRU n 2 + 1))
sqrt clk rst en x
  | clashSimulation = pipeline und clk rst en (sqrtModel <$> x)
  | otherwise =
      D.unsafeFromSignal (bundle (unpack <$> unPort q, unpack <$> unPort r))
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of sqrt"
  (q, r) = go
  go ::
    ( Port "q" dom (BitVector (DivRU n 2))
    , Port "remainder" dom (BitVector (DivRU n 2 + 1)) )
  go =
    inst
      (instConfig "altsqrt")
        { library = Just "altera_mf"
        , libraryImport = Just "altera_mf.altera_mf_components.all" }
      (Param @"q_port_width" @Integer (natToNum @(DivRU n 2)))
      (Param @"r_port_width" @Integer (natToNum @(DivRU n 2 + 1)))
      (Param @"width" @Integer (natToNum @n))
      (Param @"pipeline" @Integer (natToNum @d))
      (ClockPort @"clk" clk)
      (ResetPort @"aclr" @ActiveHigh rst)
      (Port @"ena" (boolToBit <$> fromEnable en))
      (Port @"radical" (pack <$> D.toSignal x))
{-# INLINE sqrt #-}
