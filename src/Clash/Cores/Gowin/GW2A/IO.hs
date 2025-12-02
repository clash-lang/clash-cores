{-|
  Copyright   :  (C) 2020, Foamspace corp & Christoph Mayer
                     2022, QBayLogic B.V.
                     2025, Stijn Dijkstra @ QBayLogic B.V.
  License     :  BSD2

  GOWIN GW2A IO primitives, adapted from LATTICE ECP5 IO primitives made by Christoph Mayer.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Cores.Gowin.GW2A.IO
  ( bidirectionalBuffer
  ) where

import           Clash.Annotations.Primitive  (Primitive(..), HDL(..), hasBlackBox)
import           Clash.Prelude
import           Clash.Signal.BiSignal
import           Data.String.Interpolate      (__i)
import           GHC.Stack                    (HasCallStack())

-- | BB primitive
bidirectionalBuffer
  :: forall ds dom
   . ( HasCallStack
     , HasBiSignalDefault ds
     , KnownDomain dom
     )
  => Enable dom
  -- ^ output enable
  -> BiSignalIn ds dom 1
  -- ^ PKG_PIN output BiSignal
  -> Signal dom Bit
  -- ^ output bit
  -> ( BiSignalOut ds dom 1 -- PKG_PIN input BiSignal
     , Signal dom Bit       -- input bit
     )
bidirectionalBuffer en pkgPinOut output = (pkgPinIn, dIn)
  where
    (pkgPinIn,dIn) = -- the BB primitve has an active low enable signal
      bbGW2A intrinsicName pkgPinOut output invertedEnable
    invertedEnable = not <$> fromEnable en
    intrinsicName = "IOBUF"
-- {-# NOINLINE bidirectionalBuffer #-}

bbGW2A
  :: forall ds dom
   . ( HasCallStack
     , HasBiSignalDefault ds
     , KnownDomain dom
     )
  => String
  -> BiSignalIn ds dom 1
  -> Signal dom Bit
  -> Signal dom Bool
  -> ( BiSignalOut ds dom 1
     , Signal dom Bit
     )
bbGW2A _intrinsicName pkgPinIn output notOutputEnable
  = (pkgPinOut, dIn)
   where
     dIn :: Signal dom Bit
     dIn = readFromBiSignal pkgPinIn
     pkgPinOut = writeToBiSignal pkgPinIn (toMaybe . not <$> notOutputEnable <*> output)

     toMaybe :: Bool -> a -> Maybe a
     toMaybe True a  = Just a
     toMaybe False _ = Nothing
{-# OPAQUE bbGW2A #-}
{-# ANN bbGW2A hasBlackBox #-}
{-# ANN bbGW2A (InlineYamlPrimitive [VHDL,Verilog,SystemVerilog] [__i|
  BlackBox:
    name: Clash.Cores.Gowin.GW2A.IO.bbGW2A
    kind: Declaration
    format: Haskell
    templateFunction: Clash.Cores.Gowin.GW2A.Blackboxes.IO.bbTF
  |]) #-}
