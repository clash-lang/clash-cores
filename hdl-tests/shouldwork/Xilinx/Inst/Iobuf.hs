{- | Instantiates UNISIM's bidirectional buffer @IOBUF@ to check whether a
'BiSignalInPort' is connected to the bidirectional signal itself, so the
instance can drive it.

@IOBUF@'s @IO@ port is a @std_ulogic@ in VHDL, while Clash represents a 1-bit
bidirectional signal as a @std_logic_vector(0 downto 0)@, so we only test this
in (System)Verilog.
-}
module Iobuf where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

type Dom = XilinxSystem

-- | Bidirectional buffer. Drives @IO@ with @I@ if @T@ is low.
iobuf ::
  -- | IO
  BiSignalIn 'Floating Dom 1 ->
  -- | I
  Signal Dom Bit ->
  -- | T
  Signal Dom Bit ->
  -- | IO
  BiSignalOut 'Floating Dom 1
iobuf io i t = unPort go
 where
  go :: BiSignalOutPort "IO" 'Floating Dom 1
  go =
    inst
      (instConfig "IOBUF")
        { library = Just "UNISIM"
        , libraryImport = Just "UNISIM.vcomponents.all"
        , renderVoid = True
        }
      (BiSignalInPort @"IO" io)
      (Port @"I" i)
      (Port @"T" t)

topEntity ::
  -- | (I, T, value driven onto IO by another driver)
  Signal Dom (Bit, Bit, Maybe Bit) ->
  -- | Value read from IO
  Signal Dom Bit
topEntity input = readFromBiSignal bus
 where
  (i, t, otherDriver) = unbundle input
  bus = veryUnsafeToBiSignalIn (mergeBiSignalOuts (ioOut :> otherOut :> Nil))
  otherOut = writeToBiSignal bus (fmap pack <$> otherDriver)
  ioOut = iobuf bus i t
{-# OPAQUE topEntity #-}

testBench :: Signal Dom Bool
testBench = done
 where
  input =
    stimuliGenerator clk rst $
          (0, 0, Nothing)
      :>  (1, 0, Nothing)
      :>  (0, 1, Just 1)
      :>  (1, 1, Just 0)
      :>  (0, 1, Just 0)
      :>  (1, 1, Just 1)
      :>  (1, 0, Nothing)
      :>  Nil
  expected = 0 :> 1 :> 1 :> 0 :> 0 :> 1 :> 1 :> Nil
  done = outputVerifier' clk rst expected (topEntity input)
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# OPAQUE testBench #-}
