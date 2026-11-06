{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tests of "Clash.Cores.Xilinx.Fir" against the FIR Compiler IP: each test bench
plays stimuli from a ROM, including an enable that is low now and then, and
compares the output of the IP with the output of the simulation model in every
clock cycle, except those in which the model's output is undefined.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module Fir where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import qualified Clash.Signal.Delayed as D
import Data.Maybe (fromMaybe)

import Fir.TH

-- | Play the samples from a ROM and compare the output of the function under
-- test with the expected output
firTB ::
  forall n d x y.
  ( KnownNat n
  , KnownNat d
  , 1 <= n
  , BitPack x
  , BitPack y
  , Eq y
  , ShowX y
  ) =>
  FirFun x d y ->
  MemBlob n (BitSize (Sample x y)) ->
  Signal XilinxSystem Bool
firTB dut content = done
 where
  -- No reset, so that the n-th sample is played in the n-th cycle, as in
  -- 'samples'
  cnt :: Signal XilinxSystem (Index n)
  cnt = delay clk enableGen 0 (satSucc SatBound <$> cnt)
  (en, x, expected) = unbundle (unpack . asyncRomBlob content <$> cnt)

  out = D.toSignal (dut clk (toEnable en) (D.unsafeFromSignal x))

  -- Compare only where the expected output is defined
  checked = fromMaybe <$> out <*> expected
  done0 = delay clk enableGen False ((== maxBound) <$> cnt)
  done = mux done0 done0 (assert clk rst "firTB" out checked done0)

  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# INLINE firTB #-}

firSym :: FirFun (Signed 16) 12 (Signed 31)
firSym = firSym'
{-# OPAQUE firSym #-}
{-# ANN firSym (topAnn "firSym") #-}

firSymTB :: Signal XilinxSystem Bool
firSymTB = firTB firSym $(memBlobTH (Just 0) (samples firSym' (stimuli 1)))
{-# ANN firSymTB (TestBench 'firSym) #-}

firConvEven :: FirFun (Signed 12) 13 (Signed 16)
firConvEven = firConvEven'
{-# OPAQUE firConvEven #-}
{-# ANN firConvEven (topAnn "firConvEven") #-}

firConvEvenTB :: Signal XilinxSystem Bool
firConvEvenTB = firTB firConvEven $(memBlobTH (Just 0) (samples firConvEven' (stimuli 2)))
{-# ANN firConvEvenTB (TestBench 'firConvEven) #-}

firUnsignedTrunc :: FirFun (Unsigned 10) 11 (Unsigned 12)
firUnsignedTrunc = firUnsignedTrunc'
{-# OPAQUE firUnsignedTrunc #-}
{-# ANN firUnsignedTrunc (topAnn "firUnsignedTrunc") #-}

firUnsignedTruncTB :: Signal XilinxSystem Bool
firUnsignedTruncTB = firTB firUnsignedTrunc $(memBlobTH (Just 0) (samples firUnsignedTrunc' (stimuli 3)))
{-# ANN firUnsignedTruncTB (TestBench 'firUnsignedTrunc) #-}

firMixedSymInf :: FirFun (Unsigned 12) 13 (Signed 14)
firMixedSymInf = firMixedSymInf'
{-# OPAQUE firMixedSymInf #-}
{-# ANN firMixedSymInf (topAnn "firMixedSymInf") #-}

firMixedSymInfTB :: Signal XilinxSystem Bool
firMixedSymInfTB = firTB firMixedSymInf $(memBlobTH (Just 0) (samples firMixedSymInf' (stimuli 4)))
{-# ANN firMixedSymInfTB (TestBench 'firMixedSymInf) #-}

firHalfBand :: FirFun (Signed 16) 10 (Signed 18)
firHalfBand = firHalfBand'
{-# OPAQUE firHalfBand #-}
{-# ANN firHalfBand (topAnn "firHalfBand") #-}

firHalfBandTB :: Signal XilinxSystem Bool
firHalfBandTB = firTB firHalfBand $(memBlobTH (Just 0) (samples firHalfBand' (stimuli 5)))
{-# ANN firHalfBandTB (TestBench 'firHalfBand) #-}

firHilbert :: FirFun (Signed 14) 11 (Signed 16)
firHilbert = firHilbert'
{-# OPAQUE firHilbert #-}
{-# ANN firHilbert (topAnn "firHilbert") #-}

firHilbertTB :: Signal XilinxSystem Bool
firHilbertTB = firTB firHilbert $(memBlobTH (Just 0) (samples firHilbert' (stimuli 6)))
{-# ANN firHilbertTB (TestBench 'firHilbert) #-}

firWide2 :: FirFun (Signed 30) 12 (Signed 32)
firWide2 = firWide2'
{-# OPAQUE firWide2 #-}
{-# ANN firWide2 (topAnn "firWide2") #-}

firWide2TB :: Signal XilinxSystem Bool
firWide2TB = firTB firWide2 $(memBlobTH (Just 0) (samples firWide2' (stimuli 7)))
{-# ANN firWide2TB (TestBench 'firWide2) #-}

firWide4 :: FirFun (Signed 30) 15 (Signed 40)
firWide4 = firWide4'
{-# OPAQUE firWide4 #-}
{-# ANN firWide4 (topAnn "firWide4") #-}

firWide4TB :: Signal XilinxSystem Bool
firWide4TB = firTB firWide4 $(memBlobTH (Just 0) (samples firWide4' (stimuli 8)))
{-# ANN firWide4TB (TestBench 'firWide4) #-}

firNonSymmetric :: FirFun (Signed 16) 14 (Signed 29)
firNonSymmetric = firNonSymmetric'
{-# OPAQUE firNonSymmetric #-}
{-# ANN firNonSymmetric (topAnn "firNonSymmetric") #-}

firNonSymmetricTB :: Signal XilinxSystem Bool
firNonSymmetricTB = firTB firNonSymmetric $(memBlobTH (Just 0) (samples firNonSymmetric' (stimuli 9)))
{-# ANN firNonSymmetricTB (TestBench 'firNonSymmetric) #-}

firOverflow :: FirFun (Signed 8) 7 (Signed 15)
firOverflow = firOverflow'
{-# OPAQUE firOverflow #-}
{-# ANN firOverflow (topAnn "firOverflow") #-}

firOverflowTB :: Signal XilinxSystem Bool
firOverflowTB = firTB firOverflow $(memBlobTH (Just 0) (samples firOverflow' (stimuli 10)))
{-# ANN firOverflowTB (TestBench 'firOverflow) #-}

firRoundOverflow :: FirFun (Signed 8) 7 (Signed 7)
firRoundOverflow = firRoundOverflow'
{-# OPAQUE firRoundOverflow #-}
{-# ANN firRoundOverflow (topAnn "firRoundOverflow") #-}

firRoundOverflowTB :: Signal XilinxSystem Bool
firRoundOverflowTB = firTB firRoundOverflow $(memBlobTH (Just 0) (samples firRoundOverflow' (stimuli 11)))
{-# ANN firRoundOverflowTB (TestBench 'firRoundOverflow) #-}

firTruncMax :: FirFun (Signed 16) 9 (Signed 20)
firTruncMax = firTruncMax'
{-# OPAQUE firTruncMax #-}
{-# ANN firTruncMax (topAnn "firTruncMax") #-}

firTruncMaxTB :: Signal XilinxSystem Bool
firTruncMaxTB = firTB firTruncMax $(memBlobTH (Just 0) (samples firTruncMax' (stimuli 12)))
{-# ANN firTruncMaxTB (TestBench 'firTruncMax) #-}

firUnsignedSym :: FirFun (Unsigned 16) 9 (Unsigned 34)
firUnsignedSym = firUnsignedSym'
{-# OPAQUE firUnsignedSym #-}
{-# ANN firUnsignedSym (topAnn "firUnsignedSym") #-}

firUnsignedSymTB :: Signal XilinxSystem Bool
firUnsignedSymTB = firTB firUnsignedSym $(memBlobTH (Just 0) (samples firUnsignedSym' (stimuli 13)))
{-# ANN firUnsignedSymTB (TestBench 'firUnsignedSym) #-}

firSymHalfBand :: FirFun (Signed 16) 12 (Signed 20)
firSymHalfBand = firSymHalfBand'
{-# OPAQUE firSymHalfBand #-}
{-# ANN firSymHalfBand (topAnn "firSymHalfBand") #-}

firSymHalfBandTB :: Signal XilinxSystem Bool
firSymHalfBandTB = firTB firSymHalfBand $(memBlobTH (Just 0) (samples firSymHalfBand' (stimuli 14)))
{-# ANN firSymHalfBandTB (TestBench 'firSymHalfBand) #-}

-- | Like 'firTB', for functions with valid-qualified samples: compares the
-- ready signal and the output in every cycle
validFirTB ::
  forall n d x y.
  ( KnownNat n
  , KnownNat d
  , 1 <= n
  , BitPack x
  , BitPack y
  , Eq y
  , ShowX y
  ) =>
  ValidFirFun x d y ->
  MemBlob n (BitSize (ValidSample x y)) ->
  Signal XilinxSystem Bool
validFirTB dut content = done
 where
  cnt :: Signal XilinxSystem (Index n)
  cnt = delay clk enableGen 0 (satSucc SatBound <$> cnt)
  (en, x, expectedReady, expected) = unbundle (unpack . asyncRomBlob content <$> cnt)

  (ready, out) = dut clk (toEnable en) (D.unsafeFromSignal x)

  done0 = delay clk enableGen False ((== maxBound) <$> cnt)
  done =
    mux done0 done0
      (assert clk rst "validFirTB ready" (D.toSignal ready) expectedReady
        (assert clk rst "validFirTB output" (D.toSignal out) expected done0))

  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# INLINE validFirTB #-}

firPeriod4 :: ValidFirFun (Signed 16) 12 (Signed 26)
firPeriod4 = firPeriod4'
{-# OPAQUE firPeriod4 #-}
{-# ANN firPeriod4 (topAnn "firPeriod4") #-}

firPeriod4TB :: Signal XilinxSystem Bool
firPeriod4TB = validFirTB firPeriod4 $(memBlobTH (Just 0) (validSamples firPeriod4' (validStimuli 4 20)))
{-# ANN firPeriod4TB (TestBench 'firPeriod4) #-}

firPeriod3Sym :: ValidFirFun (Signed 16) 12 (Signed 16)
firPeriod3Sym = firPeriod3Sym'
{-# OPAQUE firPeriod3Sym #-}
{-# ANN firPeriod3Sym (topAnn "firPeriod3Sym") #-}

firPeriod3SymTB :: Signal XilinxSystem Bool
firPeriod3SymTB = validFirTB firPeriod3Sym $(memBlobTH (Just 0) (validSamples firPeriod3Sym' (validStimuli 3 21)))
{-# ANN firPeriod3SymTB (TestBench 'firPeriod3Sym) #-}

firPeriod2HalfBand :: ValidFirFun (Signed 16) 11 (Signed 18)
firPeriod2HalfBand = firPeriod2HalfBand'
{-# OPAQUE firPeriod2HalfBand #-}
{-# ANN firPeriod2HalfBand (topAnn "firPeriod2HalfBand") #-}

firPeriod2HalfBandTB :: Signal XilinxSystem Bool
firPeriod2HalfBandTB = validFirTB firPeriod2HalfBand $(memBlobTH (Just 0) (validSamples firPeriod2HalfBand' (validStimuli 2 22)))
{-# ANN firPeriod2HalfBandTB (TestBench 'firPeriod2HalfBand) #-}

firPeriod1 :: ValidFirFun (Signed 12) 13 (Signed 16)
firPeriod1 = firPeriod1'
{-# OPAQUE firPeriod1 #-}
{-# ANN firPeriod1 (topAnn "firPeriod1") #-}

firPeriod1TB :: Signal XilinxSystem Bool
firPeriod1TB = validFirTB firPeriod1 $(memBlobTH (Just 0) (validSamples firPeriod1' (validStimuli 1 23)))
{-# ANN firPeriod1TB (TestBench 'firPeriod1) #-}

firPeriod8Wide :: ValidFirFun (Unsigned 24) 16 (Signed 30)
firPeriod8Wide = firPeriod8Wide'
{-# OPAQUE firPeriod8Wide #-}
{-# ANN firPeriod8Wide (topAnn "firPeriod8Wide") #-}

firPeriod8WideTB :: Signal XilinxSystem Bool
firPeriod8WideTB = validFirTB firPeriod8Wide $(memBlobTH (Just 0) (validSamples firPeriod8Wide' (validStimuli 8 24)))
{-# ANN firPeriod8WideTB (TestBench 'firPeriod8Wide) #-}

firPeriod16Long :: ValidFirFun (Signed 14) 23 (Signed 32)
firPeriod16Long = firPeriod16Long'
{-# OPAQUE firPeriod16Long #-}
{-# ANN firPeriod16Long (topAnn "firPeriod16Long") #-}

firPeriod16LongTB :: Signal XilinxSystem Bool
firPeriod16LongTB = validFirTB firPeriod16Long $(memBlobTH (Just 0) (validSamples firPeriod16Long' (validStimuli 16 25)))
{-# ANN firPeriod16LongTB (TestBench 'firPeriod16Long) #-}

firPeriod2SymPad :: ValidFirFun (Signed 16) 12 (Signed 30)
firPeriod2SymPad = firPeriod2SymPad'
{-# OPAQUE firPeriod2SymPad #-}
{-# ANN firPeriod2SymPad (topAnn "firPeriod2SymPad") #-}

firPeriod2SymPadTB :: Signal XilinxSystem Bool
firPeriod2SymPadTB = validFirTB firPeriod2SymPad $(memBlobTH (Just 0) (validSamples firPeriod2SymPad' (validStimuli 2 30)))
{-# ANN firPeriod2SymPadTB (TestBench 'firPeriod2SymPad) #-}

firPeriod3HalfBandPad :: ValidFirFun (Signed 16) 13 (Signed 31)
firPeriod3HalfBandPad = firPeriod3HalfBandPad'
{-# OPAQUE firPeriod3HalfBandPad #-}
{-# ANN firPeriod3HalfBandPad (topAnn "firPeriod3HalfBandPad") #-}

firPeriod3HalfBandPadTB :: Signal XilinxSystem Bool
firPeriod3HalfBandPadTB = validFirTB firPeriod3HalfBandPad $(memBlobTH (Just 0) (validSamples firPeriod3HalfBandPad' (validStimuli 3 31)))
{-# ANN firPeriod3HalfBandPadTB (TestBench 'firPeriod3HalfBandPad) #-}

firPeriod8SymRound :: ValidFirFun (Signed 16) 14 (Signed 16)
firPeriod8SymRound = firPeriod8SymRound'
{-# OPAQUE firPeriod8SymRound #-}
{-# ANN firPeriod8SymRound (topAnn "firPeriod8SymRound") #-}

firPeriod8SymRoundTB :: Signal XilinxSystem Bool
firPeriod8SymRoundTB = validFirTB firPeriod8SymRound $(memBlobTH (Just 0) (validSamples firPeriod8SymRound' (validStimuli 8 32)))
{-# ANN firPeriod8SymRoundTB (TestBench 'firPeriod8SymRound) #-}

firPeriod4Accumulator :: ValidFirFun (Signed 18) 37 (Signed 24)
firPeriod4Accumulator = firPeriod4Accumulator'
{-# OPAQUE firPeriod4Accumulator #-}
{-# ANN firPeriod4Accumulator (topAnn "firPeriod4Accumulator") #-}

firPeriod4AccumulatorTB :: Signal XilinxSystem Bool
firPeriod4AccumulatorTB = validFirTB firPeriod4Accumulator $(memBlobTH (Just 0) (validSamples firPeriod4Accumulator' (validStimuli 4 33)))
{-# ANN firPeriod4AccumulatorTB (TestBench 'firPeriod4Accumulator) #-}

firDecim2 :: ValidFirFun (Signed 16) 12 (Signed 26)
firDecim2 = firDecim2'
{-# OPAQUE firDecim2 #-}
{-# ANN firDecim2 (topAnn "firDecim2") #-}

firDecim2TB :: Signal XilinxSystem Bool
firDecim2TB = validFirTB firDecim2 $(memBlobTH (Just 0) (validSamples firDecim2' (validStimuli 1 40)))
{-# ANN firDecim2TB (TestBench 'firDecim2) #-}

firDecim3P2 :: ValidFirFun (Signed 16) 10 (Signed 16)
firDecim3P2 = firDecim3P2'
{-# OPAQUE firDecim3P2 #-}
{-# ANN firDecim3P2 (topAnn "firDecim3P2") #-}

firDecim3P2TB :: Signal XilinxSystem Bool
firDecim3P2TB = validFirTB firDecim3P2 $(memBlobTH (Just 0) (validSamples firDecim3P2' (validStimuli 2 41)))
{-# ANN firDecim3P2TB (TestBench 'firDecim3P2) #-}

firDecim2Sym :: ValidFirFun (Signed 16) 9 (Signed 22)
firDecim2Sym = firDecim2Sym'
{-# OPAQUE firDecim2Sym #-}
{-# ANN firDecim2Sym (topAnn "firDecim2Sym") #-}

firDecim2SymTB :: Signal XilinxSystem Bool
firDecim2SymTB = validFirTB firDecim2Sym $(memBlobTH (Just 0) (validSamples firDecim2Sym' (validStimuli 1 42)))
{-# ANN firDecim2SymTB (TestBench 'firDecim2Sym) #-}

firDecim2P2SymPad :: ValidFirFun (Signed 16) 11 (Signed 27)
firDecim2P2SymPad = firDecim2P2SymPad'
{-# OPAQUE firDecim2P2SymPad #-}
{-# ANN firDecim2P2SymPad (topAnn "firDecim2P2SymPad") #-}

firDecim2P2SymPadTB :: Signal XilinxSystem Bool
firDecim2P2SymPadTB = validFirTB firDecim2P2SymPad $(memBlobTH (Just 0) (validSamples firDecim2P2SymPad' (validStimuli 2 43)))
{-# ANN firDecim2P2SymPadTB (TestBench 'firDecim2P2SymPad) #-}

firDecim3Sym7 :: ValidFirFun (Signed 16) 9 (Signed 18)
firDecim3Sym7 = firDecim3Sym7'
{-# OPAQUE firDecim3Sym7 #-}
{-# ANN firDecim3Sym7 (topAnn "firDecim3Sym7") #-}

firDecim3Sym7TB :: Signal XilinxSystem Bool
firDecim3Sym7TB = validFirTB firDecim3Sym7 $(memBlobTH (Just 0) (validSamples firDecim3Sym7' (validStimuli 1 44)))
{-# ANN firDecim3Sym7TB (TestBench 'firDecim3Sym7) #-}

firDecim4P3SymRound :: ValidFirFun (Signed 16) 11 (Signed 16)
firDecim4P3SymRound = firDecim4P3SymRound'
{-# OPAQUE firDecim4P3SymRound #-}
{-# ANN firDecim4P3SymRound (topAnn "firDecim4P3SymRound") #-}

firDecim4P3SymRoundTB :: Signal XilinxSystem Bool
firDecim4P3SymRoundTB = validFirTB firDecim4P3SymRound $(memBlobTH (Just 0) (validSamples firDecim4P3SymRound' (validStimuli 3 45)))
{-# ANN firDecim4P3SymRoundTB (TestBench 'firDecim4P3SymRound) #-}

firDecim2HalfBandSym :: ValidFirFun (Signed 16) 11 (Signed 31)
firDecim2HalfBandSym = firDecim2HalfBandSym'
{-# OPAQUE firDecim2HalfBandSym #-}
{-# ANN firDecim2HalfBandSym (topAnn "firDecim2HalfBandSym") #-}

firDecim2HalfBandSymTB :: Signal XilinxSystem Bool
firDecim2HalfBandSymTB = validFirTB firDecim2HalfBandSym $(memBlobTH (Just 0) (validSamples firDecim2HalfBandSym' (validStimuli 2 46)))
{-# ANN firDecim2HalfBandSymTB (TestBench 'firDecim2HalfBandSym) #-}

firDecim5HalfBand :: ValidFirFun (Signed 16) 9 (Signed 20)
firDecim5HalfBand = firDecim5HalfBand'
{-# OPAQUE firDecim5HalfBand #-}
{-# ANN firDecim5HalfBand (topAnn "firDecim5HalfBand") #-}

firDecim5HalfBandTB :: Signal XilinxSystem Bool
firDecim5HalfBandTB = validFirTB firDecim5HalfBand $(memBlobTH (Just 0) (validSamples firDecim5HalfBand' (validStimuli 1 47)))
{-# ANN firDecim5HalfBandTB (TestBench 'firDecim5HalfBand) #-}

firDecim8Wide :: ValidFirFun (Unsigned 24) 9 (Signed 30)
firDecim8Wide = firDecim8Wide'
{-# OPAQUE firDecim8Wide #-}
{-# ANN firDecim8Wide (topAnn "firDecim8Wide") #-}

firDecim8WideTB :: Signal XilinxSystem Bool
firDecim8WideTB = validFirTB firDecim8Wide $(memBlobTH (Just 0) (validSamples firDecim8Wide' (validStimuli 1 48)))
{-# ANN firDecim8WideTB (TestBench 'firDecim8Wide) #-}

firDecim2P8Sym11 :: ValidFirFun (Signed 16) 11 (Signed 27)
firDecim2P8Sym11 = firDecim2P8Sym11'
{-# OPAQUE firDecim2P8Sym11 #-}
{-# ANN firDecim2P8Sym11 (topAnn "firDecim2P8Sym11") #-}

firDecim2P8Sym11TB :: Signal XilinxSystem Bool
firDecim2P8Sym11TB = validFirTB firDecim2P8Sym11 $(memBlobTH (Just 0) (validSamples firDecim2P8Sym11' (validStimuli 8 49)))
{-# ANN firDecim2P8Sym11TB (TestBench 'firDecim2P8Sym11) #-}

firDecimate4 :: ValidFirFun (Signed 14) 8 (Signed 24)
firDecimate4 = firDecimate4'
{-# OPAQUE firDecimate4 #-}
{-# ANN firDecimate4 (topAnn "firDecimate4") #-}

firDecimate4TB :: Signal XilinxSystem Bool
firDecimate4TB = validFirTB firDecimate4 $(memBlobTH (Just 0) (validSamples firDecimate4' (validStimuli 2 50)))
{-# ANN firDecimate4TB (TestBench 'firDecimate4) #-}
