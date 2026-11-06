{-# LANGUAGE TemplateHaskell #-}

module DifferentialBuffersTypes where

import Clash.Cores.LatticeSemi.Ecp5.IO (ilvds, obco, olvds)
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Data.Word (Word32)
import Language.Haskell.TH (ExpQ)
import qualified Prelude as P

import YosysSim (yosysSimModels)

$(yosysSimModels "ecp5SimModels" ["ecp5/common_sim.vh", "ecp5/cells_io.vh"] ("ecp5u", "ecp5u_components.vhdl"))

type Dom = System

-- | (positive and negative input pad of 'ilvds', input of 'olvds' and 'obco')
type Stimulus = (Bit, Bit, Bit)

-- | Outputs of 'ilvds', 'olvds' and 'obco'
dut :: KnownDomain dom => Signal dom Stimulus -> Signal dom (Vec 5 Bit)
dut stimulus = bundle (ilvds a an :> olvdsZ :> olvdsZN :> obcoOT :> obcoOC :> Nil)
 where
  (a, an, i) = unbundle stimulus
  (olvdsZ, olvdsZN) = olvds i
  (obcoOT, obcoOC) = obco i

-- | Random stimuli, including pads of an LVDS pair with the same level
stimuliList :: [Stimulus]
stimuliList = P.take 256 (P.map toStimulus (P.iterate lcg 0x5EED))
 where
  lcg :: Word32 -> Word32
  lcg x = 1664525 * x + 1013904223
  toStimulus x = (boolToBit (testBit x 31), boolToBit (testBit x 30), boolToBit (testBit x 29))

stimuli :: ExpQ
stimuli = listToVecTH stimuliList

{- | Outputs of the Haskell models, and 'Nothing' for the outputs we can't
compare against Yosys's models:

  * Yosys's @OLVDS@ (which its @OBCO@ is too) leaves the negative output
    undriven.

  * Yosys's @OLVDS@ outputs an undefined value on its positive output too. Its
    @TRELLIS_IO@ ties an unconnected @T@ input low in an @always \@(*)@ block,
    which an event-driven simulator never executes if @T@ is a constant (here:
    unconnected), so the output stays undefined. IVerilog and Vivado both show
    this.

That leaves the output of @ILVDS@. The other buffers are still instantiated, so
the test checks that they connect to Yosys's models.
-}
expected :: ExpQ
expected = listToVecTH (P.map mask actual)
 where
  actual = sampleN (P.length stimuliList) (dut @Dom (fromList stimuliList))
  mask :: Vec 5 Bit -> Vec 5 (Maybe Bit)
  mask v = Just (head v) :> repeat Nothing

tb ::
  forall n.
  (KnownNat n, 1 <= n) =>
  Vec n Stimulus ->
  Vec n (Vec 5 (Maybe Bit)) ->
  Signal Dom Bool
tb stimuliVec expectedVec = ecp5SimModels done
 where
  clk = tbClockGen (not <$> done)
  rst = resetGen

  actual = dut (stimuliGenerator clk rst stimuliVec)
  expectedMasked = stimuliGenerator clk rst expectedVec

  -- Replace the outputs we don't care about by their expected value
  actualMasked = zipWith maskBit <$> expectedMasked <*> actual
  maskBit e a = maybe 0 (const a) e

  done =
    outputVerifier' clk rst (map (map (maybe 0 id)) expectedVec) actualMasked
