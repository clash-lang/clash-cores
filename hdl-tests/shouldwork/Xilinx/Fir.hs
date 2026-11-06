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
