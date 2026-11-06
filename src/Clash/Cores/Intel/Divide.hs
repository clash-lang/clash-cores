{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's @lpm_divide@ megafunction (the LPM_DIVIDE IP), an integer
divider. See the
[Integer Arithmetic IP Cores User Guide (UG-01063)](https://www.intel.com/content/www/us/en/docs/programmable/683490/).

Unlike the IP in "Clash.Cores.Intel.Floating", @lpm_divide@ is a parameterized
module that Quartus implements directly, so Clash simply instantiates it. Its
simulation models are part of Intel's simulation libraries: the @lpm@ library
(@220pack.vhd@ and @220model.vhd@ for VHDL, @220model.v@ for Verilog).

The latency is set through the delay argument of the 'DSignal'. A latency of
zero results in a combinational divider. The pipeline is stalled when the
'Enable' is deasserted, and cleared by the (asynchronous) reset. The outputs
during reset, and until the pipeline has filled again, are undefined in
simulation.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Divide
  ( divide
  , RemainderSign(..)
  , DivideRepresentation(..)
  , divideModel
  ) where

import Clash.Explicit.Prelude hiding (divide)
import Data.Kind (Type)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import qualified Clash.Signal.Delayed as D

import Clash.Cores.Intel.Floating.Internal (pipeline)
import Clash.Cores.Intel.Internal
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (unPort)

-- | Sign of the remainder of a division of signed numbers. For unsigned
-- numbers, both are the same.
data RemainderSign
  = RemainderPositive
  -- ^ The remainder is never negative: @0 <= r < abs d@. The quotient is
  -- @(n - r) / d@. This is the default of @lpm_divide@
  -- (@LPM_REMAINDERPOSITIVE=TRUE@).
  | RemainderSignOfNumerator
  -- ^ The quotient is rounded towards zero, and the remainder has the sign of
  -- the numerator, like Haskell's 'quotRem' (@LPM_REMAINDERPOSITIVE=FALSE@).
  deriving (Eq, Show)

-- | Number representations supported by @lpm_divide@: 'Signed' and
-- 'Unsigned'.
class DivideRepresentation (f :: Nat -> Type) where
  -- | Value of @lpm_nrepresentation@ and @lpm_drepresentation@
  representation :: String

instance DivideRepresentation Signed where
  representation = "SIGNED"

instance DivideRepresentation Unsigned where
  representation = "UNSIGNED"

-- | Simulation model of @lpm_divide@: the quotient and remainder of @n@
-- divided by @d@, see 'RemainderSign'. The quotient wraps around if it
-- doesn't fit (dividing 'minBound' by -1). The results of a division by zero
-- are undefined.
divideModel ::
  forall (f :: Nat -> Type) (n :: Nat) (m :: Nat).
  (Integral (f n), Integral (f m), NFDataX (f n), NFDataX (f m), HasCallStack) =>
  RemainderSign ->
  f n ->
  f m ->
  (f n, f m)
divideModel sign n d
  | d == 0 = withFrozenCallStack $ deepErrorX "divideModel: division by zero"
  | otherwise = (fromInteger q, fromInteger r)
 where
  (q0, r0) = toInteger n `quotRem` toInteger d
  (q, r) = case sign of
    RemainderPositive
      | r0 < 0 -> if d > 0 then (q0 - 1, r0 + toInteger d) else (q0 + 1, r0 - toInteger d)
    _ -> (q0, r0)

-- | Integer division using @lpm_divide@. Returns the quotient and the
-- remainder, see 'divideModel'.
divide ::
  forall d dom k f n m.
  ( KnownDomain dom
  , KnownNat d
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m
  , DivideRepresentation f
  , BitPack (f n)
  , BitPack (f m)
  , Integral (f n)
  , Integral (f m)
  , NFDataX (f n)
  , NFDataX (f m)
  , HasCallStack
  ) =>
  RemainderSign ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Numerator
  DSignal dom k (f n) ->
  -- | Denominator
  DSignal dom k (f m) ->
  -- | Quotient and remainder
  DSignal dom (k + d) (f n, f m)
divide sign clk rst en n dd
  | clashSimulation =
      pipeline und clk rst en (divideModel sign <$> n <*> dd)
  | otherwise =
      D.unsafeFromSignal (bundle (unpack <$> unPort q, unpack <$> unPort r))
 where
  und = withFrozenCallStack $ deepErrorX "Undefined output of divide"
  (q, r) = go
  go ::
    ( Port "quotient" dom (BitVector (BitSize (f n)))
    , Port "remain" dom (BitVector (BitSize (f m))) )
  go =
    inst
      (instConfig "lpm_divide")
        { library = Just "lpm"
        , libraryImport = Just "lpm.lpm_components.all" }
      (Param @"lpm_widthn" @Integer (natToNum @(BitSize (f n))))
      (Param @"lpm_widthd" @Integer (natToNum @(BitSize (f m))))
      (Param @"lpm_nrepresentation" @String (representation @f))
      (Param @"lpm_drepresentation" @String (representation @f))
      (Param @"lpm_pipeline" @Integer (natToNum @d))
      (Param @"lpm_hint" @String (remainderHint sign))
      (ClockPort @"clock" clk)
      (ResetPort @"aclr" @ActiveHigh rst)
      (Port @"clken" (boolToBit <$> fromEnable en))
      (Port @"numer" (pack <$> D.toSignal n))
      (Port @"denom" (pack <$> D.toSignal dd))
{-# INLINE divide #-}

-- | Value of @lpm_hint@
remainderHint :: RemainderSign -> String
remainderHint = \case
  RemainderPositive -> "LPM_REMAINDERPOSITIVE=TRUE"
  RemainderSignOfNumerator -> "LPM_REMAINDERPOSITIVE=FALSE"
