{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tests of "Clash.Cores.Xilinx.Divider" against the Divider Generator IP: each
test bench plays stimuli from a ROM, including an enable that is low now and
then, and compares the output of the IP with the output of the simulation
model in every clock cycle, except those in which the model's output is
undefined.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module Divider where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import qualified Clash.Signal.Delayed as D
import Data.Maybe (fromMaybe)

import qualified Clash.Cores.Xilinx.Divider.Explicit as Div

import Divider.TH

-- | Play the samples from a ROM and compare the output of the function under
-- test with the expected output
divTB ::
  forall n d x y z.
  ( KnownNat n
  , KnownNat d
  , 1 <= n
  , BitPack x
  , BitPack y
  , BitPack z
  , Eq z
  , ShowX z
  ) =>
  ( Clock XilinxSystem ->
    Enable XilinxSystem ->
    DSignal XilinxSystem 0 x ->
    DSignal XilinxSystem 0 y ->
    DSignal XilinxSystem d z
  ) ->
  MemBlob n (BitSize (Sample x y z)) ->
  Signal XilinxSystem Bool
divTB dut content = done
 where
  -- No reset, so that the n-th sample is played in the n-th cycle, as in
  -- 'samples'
  cnt :: Signal XilinxSystem (Index n)
  cnt = delay clk enableGen 0 (satSucc SatBound <$> cnt)
  (en, x, y, expected) = unbundle (unpack . asyncRomBlob content <$> cnt)

  out = D.toSignal (dut clk (toEnable en) (D.unsafeFromSignal x) (D.unsafeFromSignal y))

  -- Compare only where the expected output is defined
  checked = fromMaybe <$> out <*> expected
  done0 = delay clk enableGen False ((== maxBound) <$> cnt)
  done = mux done0 done0 (assert clk rst "divTB" out checked done0)

  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# INLINE divTB #-}

------------------------------------------------------------------------------
-- quotRem
------------------------------------------------------------------------------

quotRemS8 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Signed 8) ->
  DSignal XilinxSystem 0 (Signed 8) ->
  DSignal XilinxSystem (Div.QuotRemDefDelay Signed 8) (Maybe (Signed 8, Signed 8))
quotRemS8 = quotRemS8'
{-# OPAQUE quotRemS8 #-}
{-# ANN quotRemS8 (topAnn "quotRemS8") #-}

quotRemS8TB :: Signal XilinxSystem Bool
quotRemS8TB = divTB quotRemS8 $(memBlobTH (Just 0) (samples quotRemS8' (stimuli 1)))
{-# ANN quotRemS8TB (TestBench 'quotRemS8) #-}

quotRemU16U9 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Unsigned 16) ->
  DSignal XilinxSystem 0 (Unsigned 9) ->
  DSignal XilinxSystem 5 (Maybe (Unsigned 16, Unsigned 9))
quotRemU16U9 = quotRemU16U9'
{-# OPAQUE quotRemU16U9 #-}
{-# ANN quotRemU16U9 (topAnn "quotRemU16U9") #-}

quotRemU16U9TB :: Signal XilinxSystem Bool
quotRemU16U9TB = divTB quotRemU16U9 $(memBlobTH (Just 0) (samples quotRemU16U9' (stimuli 2)))
{-# ANN quotRemU16U9TB (TestBench 'quotRemU16U9) #-}

quotRemS33S17 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Signed 33) ->
  DSignal XilinxSystem 0 (Signed 17) ->
  DSignal XilinxSystem 20 (Maybe (Signed 33, Signed 17))
quotRemS33S17 = quotRemS33S17'
{-# OPAQUE quotRemS33S17 #-}
{-# ANN quotRemS33S17 (topAnn "quotRemS33S17") #-}

quotRemS33S17TB :: Signal XilinxSystem Bool
quotRemS33S17TB = divTB quotRemS33S17 $(memBlobTH (Just 0) (samples quotRemS33S17' (stimuli 3)))
{-# ANN quotRemS33S17TB (TestBench 'quotRemS33S17) #-}

quotRemLutS9S6 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Signed 9) ->
  DSignal XilinxSystem 0 (Signed 6) ->
  DSignal XilinxSystem 10 (Maybe (Signed 9, Signed 6))
quotRemLutS9S6 = quotRemLutS9S6'
{-# OPAQUE quotRemLutS9S6 #-}
{-# ANN quotRemLutS9S6 (topAnn "quotRemLutS9S6") #-}

quotRemLutS9S6TB :: Signal XilinxSystem Bool
quotRemLutS9S6TB = divTB quotRemLutS9S6 $(memBlobTH (Just 0) (samples quotRemLutS9S6' (stimuli 4)))
{-# ANN quotRemLutS9S6TB (TestBench 'quotRemLutS9S6) #-}

quotRemLutU12U11 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Unsigned 12) ->
  DSignal XilinxSystem 0 (Unsigned 11) ->
  DSignal XilinxSystem 3 (Maybe (Unsigned 12, Unsigned 11))
quotRemLutU12U11 = quotRemLutU12U11'
{-# OPAQUE quotRemLutU12U11 #-}
{-# ANN quotRemLutU12U11 (topAnn "quotRemLutU12U11") #-}

quotRemLutU12U11TB :: Signal XilinxSystem Bool
quotRemLutU12U11TB = divTB quotRemLutU12U11 $(memBlobTH (Just 0) (samples quotRemLutU12U11' (stimuli 5)))
{-# ANN quotRemLutU12U11TB (TestBench 'quotRemLutU12U11) #-}

------------------------------------------------------------------------------
-- quotFrac
------------------------------------------------------------------------------

quotFracS12S5 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Signed 12) ->
  DSignal XilinxSystem 0 (Signed 5) ->
  DSignal XilinxSystem (Div.QuotFracDefDelay Signed 12 6) (Maybe (Signed 12, SFixed 1 6))
quotFracS12S5 = quotFracS12S5'
{-# OPAQUE quotFracS12S5 #-}
{-# ANN quotFracS12S5 (topAnn "quotFracS12S5") #-}

quotFracS12S5TB :: Signal XilinxSystem Bool
quotFracS12S5TB = divTB quotFracS12S5 $(memBlobTH (Just 0) (samples quotFracS12S5' (stimuli 6)))
{-# ANN quotFracS12S5TB (TestBench 'quotFracS12S5) #-}

quotFracU10U6 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Unsigned 10) ->
  DSignal XilinxSystem 0 (Unsigned 6) ->
  DSignal XilinxSystem 7 (Maybe (Unsigned 10, UFixed 0 4))
quotFracU10U6 = quotFracU10U6'
{-# OPAQUE quotFracU10U6 #-}
{-# ANN quotFracU10U6 (topAnn "quotFracU10U6") #-}

quotFracU10U6TB :: Signal XilinxSystem Bool
quotFracU10U6TB = divTB quotFracU10U6 $(memBlobTH (Just 0) (samples quotFracU10U6' (stimuli 7)))
{-# ANN quotFracU10U6TB (TestBench 'quotFracU10U6) #-}

------------------------------------------------------------------------------
-- divHighRadix
------------------------------------------------------------------------------

divHighRadixS8S8 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Signed 8) ->
  DSignal XilinxSystem 0 (Signed 8) ->
  DSignal XilinxSystem (Div.HighRadixDefDelay 8) (Maybe (SFixed 8 0))
divHighRadixS8S8 = divHighRadixS8S8'
{-# OPAQUE divHighRadixS8S8 #-}
{-# ANN divHighRadixS8S8 (topAnn "divHighRadixS8S8") #-}

divHighRadixS8S8TB :: Signal XilinxSystem Bool
divHighRadixS8S8TB =
  divTB divHighRadixS8S8
    $(memBlobTH (Just 0) (samples divHighRadixS8S8' (stimuli 8 <> tieStimuli)))
{-# ANN divHighRadixS8S8TB (TestBench 'divHighRadixS8S8) #-}

divHighRadixS6S12 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Signed 6) ->
  DSignal XilinxSystem 0 (Signed 12) ->
  DSignal XilinxSystem 2 (Maybe (SFixed 6 6))
divHighRadixS6S12 = divHighRadixS6S12'
{-# OPAQUE divHighRadixS6S12 #-}
{-# ANN divHighRadixS6S12 (topAnn "divHighRadixS6S12") #-}

divHighRadixS6S12TB :: Signal XilinxSystem Bool
divHighRadixS6S12TB =
  divTB divHighRadixS6S12
    $(memBlobTH (Just 0) (samples divHighRadixS6S12' (stimuli 9 <> tieStimuli)))
{-# ANN divHighRadixS6S12TB (TestBench 'divHighRadixS6S12) #-}

divHighRadixS4S40 ::
  Clock XilinxSystem ->
  Enable XilinxSystem ->
  DSignal XilinxSystem 0 (Signed 4) ->
  DSignal XilinxSystem 0 (Signed 40) ->
  DSignal XilinxSystem (Div.HighRadixDefDelay 40) (Maybe (SFixed 4 8))
divHighRadixS4S40 = divHighRadixS4S40'
{-# OPAQUE divHighRadixS4S40 #-}
{-# ANN divHighRadixS4S40 (topAnn "divHighRadixS4S40") #-}

divHighRadixS4S40TB :: Signal XilinxSystem Bool
divHighRadixS4S40TB =
  divTB divHighRadixS4S40
    $(memBlobTH (Just 0) (samples divHighRadixS4S40' (stimuli 10 <> tieStimuli)))
{-# ANN divHighRadixS4S40TB (TestBench 'divHighRadixS4S40) #-}
