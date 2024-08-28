{-# language FlexibleContexts #-}

{-|
Module      : Clash.Signal.Extra
Description : Extra utility functions for working with signals.
-}
module Clash.Signal.Extra
  ( registerN
  , secondTimer
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


-- | This register is @True@ exactly every second if @DomainPeriod dom@ divides @10^12@.
--   If not, the accuracy depends on the clock frequency, because we round this division
--   down. In that case, the higher the clock frequency, the more accurate it is.
--   Does not support clock frequencies lower than 2 Hz.
secondTimer
  :: forall (dom :: Domain)
   . HiddenClockResetEnable dom
  => KnownNat (DomainPeriod dom)
  => 1 <= DomainPeriod dom
  => DomainPeriod dom <= 5 * 10^11
  => Signal dom Bool
secondTimer = case compareSNat d1 (SNat @(10^12 `Div` DomainPeriod dom)) of
  SNatLE -> isRising 0 $ msb <$> counter
    where
      counter :: Signal dom (Index (10^12 `Div` DomainPeriod dom))
      counter = register maxBound (satPred SatWrap <$> counter)
  SNatGT -> errorX "secondTimer: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
