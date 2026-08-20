{-# language FlexibleContexts #-}

{-|
Module      : Clash.Signal.Extra
Description : Extra utility functions for working with signals.
-}
module Clash.Signal.Extra
  ( registerN
  , timer
  ) where

import Clash.Prelude


-- | a chain of registers of length n. So the delay from input -> output is n cycles.
registerN
  :: forall (dom :: Domain) (n :: Nat) (a :: Type)
   . HiddenClockResetEnable dom
  => NFDataX a
  => SNat n
  -- ^ The chain (or delay) length
  -> a
  -- ^ The initial value of the registers
  -> Signal dom a
  -> Signal dom a
registerN n@SNat initial inp = case compareSNat d1 n of
  SNatLE -> register initial $ registerN (SNat @(n - 1)) initial inp
  SNatGT -> inp

{- |
This register is @True@ exactly every @ps@ picoseconds. If @DomainPeriod dom@
does not divide @ps@, there will be a rounding error. We round the result down,
so the clock will tick slightly faster than intended. In this case, a faster
clock will be more accurate than a slower clock.

NB: @ps / DomainPeriod dom@ must be at least 2.
-}
timer :: forall dom ps.
  (HiddenClockResetEnable dom) =>
  SNat ps ->
  Signal dom Bool
timer SNat = case knownDomain @dom of
  SDomainConfiguration{} -> case compareSNat d2 (SNat @(ps `Div` (Max 1 (DomainPeriod dom)))) of
    SNatGT -> clashCompileError
      "timer: (ps / DomainPeriod dom) must be at least 2."
    SNatLE -> isRising 0 $ msb <$> counter
     where
      counter :: Signal dom (Index (ps `Div` (Max 1 (DomainPeriod dom))))
      counter = register maxBound (satPred SatWrap <$> counter)
