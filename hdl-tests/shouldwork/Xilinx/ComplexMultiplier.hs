{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tests of "Clash.Cores.Xilinx.ComplexMultiplier" against the Complex Multiplier
IP: each test bench plays stimuli from a ROM, including an enable that is low
now and then and a random round carry, and compares the output of the IP with
the output of the simulation model in every clock cycle, except those in which
the model's output is undefined.
-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 -Wall -Werror #-}

module ComplexMultiplier where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Data.Complex (Complex)
import Data.Maybe (fromMaybe)
import qualified Clash.Signal.Delayed as D

import qualified Clash.Cores.Xilinx.ComplexMultiplier.Explicit as Cmpy

import ComplexMultiplier.TH

-- | Play the samples from a ROM and compare the output of the function under
-- test with the expected output
cmpyTB ::
  forall n d a b o.
  (KnownNat n, KnownNat d, KnownNat a, KnownNat b, KnownNat o, 1 <= n) =>
  Dut d a b o ->
  MemBlob n (BitSize (Sample a b o)) ->
  Signal XilinxSystem Bool
cmpyTB dut content = done
 where
  -- No reset, so that the n-th sample is played in the n-th cycle, as in
  -- 'samples'
  cnt :: Signal XilinxSystem (Index n)
  cnt = delay clk enableGen 0 (satSucc SatBound <$> cnt)
  (en, cy, x, y, expected) = unbundle (unpack . asyncRomBlob content <$> cnt)

  out :: Signal XilinxSystem (Complex (Signed o))
  out =
    D.toSignal
      (dut clk (toEnable en)
        (D.unsafeFromSignal cy)
        (D.unsafeFromSignal x)
        (D.unsafeFromSignal y))

  -- Compare only where the expected output is defined
  checked = fromMaybe <$> out <*> expected
  done0 = delay clk enableGen False ((== maxBound) <$> cnt)
  done = mux done0 done0 (assert clk rst "cmpyTB" out checked done0)

  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# INLINE cmpyTB #-}

mulS16S16 :: Dut (Cmpy.ComplexMultiplyDefDelay 16 16) 16 16 33
mulS16S16 = mulS16S16'
{-# OPAQUE mulS16S16 #-}
{-# ANN mulS16S16 (topAnn "mulS16S16") #-}

mulS16S16TB :: Signal XilinxSystem Bool
mulS16S16TB = cmpyTB mulS16S16 $(memBlobTH (Just 0) (samples mulS16S16' (stimuli @16 @16 @33 1)))
{-# ANN mulS16S16TB (TestBench 'mulS16S16) #-}

mulResS18S25 :: Dut 3 18 25 20
mulResS18S25 = mulResS18S25'
{-# OPAQUE mulResS18S25 #-}
{-# ANN mulResS18S25 (topAnn "mulResS18S25") #-}

mulResS18S25TB :: Signal XilinxSystem Bool
mulResS18S25TB = cmpyTB mulResS18S25 $(memBlobTH (Just 0) (samples mulResS18S25' (stimuli @18 @25 @20 2)))
{-# ANN mulResS18S25TB (TestBench 'mulResS18S25) #-}

mulLutS35S27 :: Dut 12 35 27 2
mulLutS35S27 = mulLutS35S27'
{-# OPAQUE mulLutS35S27 #-}
{-# ANN mulLutS35S27 (topAnn "mulLutS35S27") #-}

mulLutS35S27TB :: Signal XilinxSystem Bool
mulLutS35S27TB = cmpyTB mulLutS35S27 $(memBlobTH (Just 0) (samples mulLutS35S27' (stimuli @35 @27 @2 3)))
{-# ANN mulLutS35S27TB (TestBench 'mulLutS35S27) #-}

mulS63S63 :: Dut (Cmpy.ComplexMultiplyDefDelay 63 63) 63 63 127
mulS63S63 = mulS63S63'
{-# OPAQUE mulS63S63 #-}
{-# ANN mulS63S63 (topAnn "mulS63S63") #-}

mulS63S63TB :: Signal XilinxSystem Bool
mulS63S63TB = cmpyTB mulS63S63 $(memBlobTH (Just 0) (samples mulS63S63' (stimuli @63 @63 @127 4)))
{-# ANN mulS63S63TB (TestBench 'mulS63S63) #-}

roundS16S16 :: Dut (Cmpy.ComplexMultiplyDefDelay 16 16) 16 16 16
roundS16S16 = roundS16S16'
{-# OPAQUE roundS16S16 #-}
{-# ANN roundS16S16 (topAnn "roundS16S16") #-}

roundS16S16TB :: Signal XilinxSystem Bool
roundS16S16TB = cmpyTB roundS16S16 $(memBlobTH (Just 0) (samples roundS16S16' (stimuli @16 @16 @16 5)))
{-# ANN roundS16S16TB (TestBench 'roundS16S16) #-}

roundLutS8S8 :: Dut 1 8 8 9
roundLutS8S8 = roundLutS8S8'
{-# OPAQUE roundLutS8S8 #-}
{-# ANN roundLutS8S8 (topAnn "roundLutS8S8") #-}

roundLutS8S8TB :: Signal XilinxSystem Bool
roundLutS8S8TB = cmpyTB roundLutS8S8 $(memBlobTH (Just 0) (samples roundLutS8S8' (stimuli @8 @8 @9 6)))
{-# ANN roundLutS8S8TB (TestBench 'roundLutS8S8) #-}

roundResS40S30 :: Dut 2 40 30 20
roundResS40S30 = roundResS40S30'
{-# OPAQUE roundResS40S30 #-}
{-# ANN roundResS40S30 (topAnn "roundResS40S30") #-}

roundResS40S30TB :: Signal XilinxSystem Bool
roundResS40S30TB = cmpyTB roundResS40S30 $(memBlobTH (Just 0) (samples roundResS40S30' (stimuli @40 @30 @20 7)))
{-# ANN roundResS40S30TB (TestBench 'roundResS40S30) #-}

roundS26S45 :: Dut 9 26 45 64
roundS26S45 = roundS26S45'
{-# OPAQUE roundS26S45 #-}
{-# ANN roundS26S45 (topAnn "roundS26S45") #-}

roundS26S45TB :: Signal XilinxSystem Bool
roundS26S45TB = cmpyTB roundS26S45 $(memBlobTH (Just 0) (samples roundS26S45' (stimuli @26 @45 @64 8)))
{-# ANN roundS26S45TB (TestBench 'roundS26S45) #-}
