{-# LANGUAGE AllowAmbiguousTypes #-}
module Clash.Cores.SerDes
  ( serializeWithInitialState
  , serialize
  , deserialize
  ) where

import Clash.Prelude

import Data.Constraint (Dict(..))
import Unsafe.Coerce (unsafeCoerce)

-- These are propositions that are true, but GHC constraint solver cannot auto-infer
myProp :: forall m n. (1 <= m, 1 <= n) => Dict (1 <= m `DivRU` n)
myProp = unsafeCoerce (Dict :: Dict (0 ~ 0))

-- Note: It matters whether you do `((m-1)+1)` or `(1+(m-1))`
myProp2 :: forall m. (1 <= m) => Dict ((m - 1) + 1 ~ m)
myProp2 = unsafeCoerce (Dict :: Dict (0 ~ 0))


-- | Deserializes a Signal of bytes into an `a`. If `bitsize a` is not a
-- multiple of n, it is expected that the first byte will be left-padded to make
-- it a multiple of n (as is done by `resize`).
deserialize ::
  forall a n dom.
  BitPack a =>
  (KnownNat n, 1 <= n) =>
  HiddenClockResetEnable dom =>
  Signal dom (Maybe (BitVector n)) ->
  -- ^ Input bits
  Signal dom (Maybe a)
  -- ^ Output
deserialize byte = mealy step (0, repeat undefined) byte
 where
  step state Nothing = (state, Nothing)
  step (i :: Index (BitSize a `DivRU` n), v) (Just b)
    | i == maxBound = ((0, undefined), Just res)
    | otherwise = ((i+1, nv), Nothing)
   where
    nv :: Vec (BitSize a `DivRU` n) (BitVector n)
    nv = v <<+ b
    res = unpack (resize (pack nv))


-- | Send out an `a` in chunks of bits. Since the input (potentially) contains
--   more bits than the output, a `ready` signal lets the circuit know when to send
--   out the next byte.
--
--   The circuit will ignore any new inputs while it is serializing. It DOES NOT expose
--   a backpressure signal.
--
--   The circuit latches input data, so the data needs to only be input for one cycle.
serialize ::
  forall a n dom.
  (BitPack a, 1 <= BitSize a) =>
  (KnownNat n, 1 <= n) =>
  HiddenClockResetEnable dom =>
  Signal dom Bool ->
  -- ^ Ack signal from downstream circuit
  Signal dom (Maybe a) ->
  -- ^ Input
  Signal dom (Maybe (BitVector n))
  -- ^ Output
serialize = serializeWithInitialState (0, repeat undefined)

-- | This function works the same as `serialize`, with the addition of being able to
--   specify an initial state of the serializer. This feature can be used to send an
--   acknowledgement byte once `reset` has been de-asserted (by setting the initial
--   state to be that acknowledgement byte).
serializeWithInitialState ::
  forall a n dom.
  (BitPack a, 1 <= BitSize a) =>
  (KnownNat n, 1 <= n) =>
  HiddenClockResetEnable dom =>
  ( Index ((BitSize a `DivRU` n) + 1)
  , Vec (BitSize a `DivRU` n) (BitVector n)
  ) ->
  -- ^ Init state of the serializer
  Signal dom Bool ->
  -- ^ Ack signal from downstream circuit
  Signal dom (Maybe a) ->
  -- ^ Input
  Signal dom (Maybe (BitVector n))
  -- ^ Output
serializeWithInitialState initState isReady input
    | Dict <- myProp @(BitSize a) @n
    , Dict <- myProp2 @(BitSize a `DivRU` n)
  = mealyB step initState
  ( fmap (fmap (unpack . resize . pack)) input
  , isReady
  )
 where
  step state@(0, _) (Nothing, _) = (state, Nothing)
  step       (0, _) (Just h, _) = ((maxBound, h), Nothing)

  step state@(_, h) (_, False) = (state, Just $ head h)
  step       (n, h) (_, True) = ((n-1, h << d1), Just $ head h)
   where
    v << i = fst $ shiftOutFrom0 i v


