{- |
  Copyright   :  (C) 2024-2025, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Top level module for the PCS transmit block, that combines the processes
  that are defined in the two submodules @CodeGroup@ and @OrderedSet@.
-}
module Clash.Cores.Sgmii.PcsTransmit
  ( pcsTransmit
  , inputDelayT
  )
where

import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.PcsTransmit.CodeGroup
import Clash.Cores.Sgmii.PcsTransmit.OrderedSet
import Clash.Prelude

-- | Tuple that contains a queue containing the most recent incoming octets and
--   an 'Index' that points to the current octet to be transmitted. The size has
--   been chosen to be 8, but this is a guess and could be too small.
type InputDelayState = (Index 8, Vec 8 (Bool, Bool, BitVector 8))

-- | Transition function for the input queue that outputs the correct octet when
--   the system is ready. Note that, when the 'Index' that points to the next
--   value to be outputted starts going down, it should act one time instance
--   earlier than when it goes up.
inputDelayT ::
  -- | Current state
  InputDelayState ->
  -- | New input values for @TX_EN@, @TX_ER@, the incoming data word and the
  --   ready signal
  (Bool, Bool, BitVector 8, Bool) ->
  -- | Output tuple without the ready signal
  (InputDelayState, (Bool, Bool, BitVector 8))
inputDelayT (cur, is) (txEn, txEr, dw, txRdy) = ((cur', is'), o')
 where
  cur'
    | (txEn || txEr) && txRdy = cur
    | (txEn || txEr) && cur < maxBound = cur + 1
    | txRdy && cur > minBound = cur - 1
    | otherwise = cur

  is'
    | txEn || txEr = (txEn, txEr, dw) +>> is
    | otherwise = is

  o'
    | not (txEn || txEr) && cur' < minBound + 2 = f (is' !! cur')
    | cur' < cur = is' !! cur'
    | otherwise = is' !! cur
   where
    f (_, _, a) = (False, False, a)

{-# OPAQUE inputDelayT #-}

-- | Takes the signals that are defined in IEEE 802.3 Clause 36 and runs them
--   through the state machines as defined for the PCS transmit block. These are
--   implemented in 'codeGroupT', 'codeGroupO' and 'orderedSetT'.
pcsTransmit ::
  (HiddenClockResetEnable dom) =>
  -- | The @TX_EN@ signal
  Signal dom Bool ->
  -- | The @TX_ER@ signal
  Signal dom Bool ->
  -- | The new data word that needs to be transmitted
  Signal dom (BitVector 8) ->
  -- | The 'Xmit' signal from 'Sgmii.autoNeg'
  Signal dom (Maybe Xmit) ->
  -- | The 'ConfReg' from 'Sgmii.autoNeg'
  Signal dom (Maybe ConfReg) ->
  -- | The 8b/10b encoded output value
  Signal dom CodeGroup
pcsTransmit txEn txEr dw xmit txConfReg = cg
 where
  (_, cg, txEven, txInd, txRdy) =
    mooreB
      codeGroupT
      codeGroupO
      (IdleDisparityOk False 0 0)
      (txOSet, dw', txConfReg)

  (_, txOSet) =
    mealyB
      orderedSetT
      (IdleS Idle False)
      (txEn', txEr', dw', xmit, txEven, txInd)

  (txEn', txEr', dw') =
    mealyB inputDelayT (0, repeat (False, False, 0)) (txEn, txEr, dw, txRdy)

{-# OPAQUE pcsTransmit #-}
