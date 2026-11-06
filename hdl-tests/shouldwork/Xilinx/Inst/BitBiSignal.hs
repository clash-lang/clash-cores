{- | Instantiates UNISIM's bidirectional buffer @IOBUF@, whose @IO@ port is a
@std_ulogic@ in VHDL, through a 'BitBiSignalInPort'. Clash represents a 1-bit
bidirectional signal as a @std_logic_vector(0 downto 0)@, so a
'BiSignalInPort' can't be connected to it in VHDL (see "Iobuf").
-}
module BitBiSignal where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

import qualified Data.List as L

import BitBiSignalTypes (testVectors)

type Dom = XilinxSystem

-- | Bidirectional buffer. Drives @IO@ with @I@ if @T@ is low, and reads @IO@ to
-- @O@ regardless of @T@.
iobuf ::
  -- | IO
  BiSignalIn 'Floating Dom 1 ->
  -- | I
  Signal Dom Bit ->
  -- | T
  Signal Dom Bit ->
  -- | (IO, O)
  (BiSignalOut 'Floating Dom 1, Signal Dom Bit)
iobuf io i t = (unPort ioOut, unPort o)
 where
  (ioOut, o) = go

  go :: (BiSignalOutPort "IO" 'Floating Dom 1, Port "O" Dom Bit)
  go =
    inst
      (instConfig "IOBUF")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        }
      (BitBiSignalInPort @"IO" io)
      (Port @"I" i)
      (Port @"T" t)

-- | Two 'iobuf's sharing a single wire. Note that we can't read the wire
-- directly in VHDL: the buffers convert its level to @0@ or @1@ for us.
topEntity ::
  -- | (I and T of both buffers)
  Signal Dom (Bit, Bit, Bit, Bit) ->
  -- | (O of both buffers)
  Signal Dom (Bit, Bit)
topEntity input = bundle (oA, oB)
 where
  (iA, tA, iB, tB) = unbundle input
  bus = veryUnsafeToBiSignalIn (mergeBiSignalOuts (ioOutA :> ioOutB :> Nil))
  (ioOutA, oA) = iobuf bus iA tA
  (ioOutB, oB) = iobuf bus iB tB
{-# OPAQUE topEntity #-}

testBench :: Signal Dom Bool
testBench = done
 where
  input = stimuliGenerator clk rst $(listToVecTH (L.map fst testVectors))
  expected = $(listToVecTH (L.map snd testVectors))
  done = outputVerifier' clk rst expected (mux warmingUp (pure (0, 0)) (topEntity input))
  clk = tbClockGen (not <$> done)
  rst = resetGen

  -- The Verilog UNISIM models put all outputs in high impedance during the
  -- first 100 ns of the simulation (@glbl.GTS@), so don't check the outputs
  -- during the first 16 cycles.
  warmingUp = cnt .<. 16
  cnt = register clk rst enableGen (0 :: Index 32) (satSucc SatBound <$> cnt)
{-# OPAQUE testBench #-}
