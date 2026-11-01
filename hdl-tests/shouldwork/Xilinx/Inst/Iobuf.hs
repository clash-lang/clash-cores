{- | Instantiates UNISIM's bidirectional buffer @IOBUF@ to check whether a
'BiSignalInPort' is connected to the bidirectional signal itself, so the
instance can drive it. It also checks whether a (zero-width) 'BiSignalOutPort'
can be combined with other output ports, see
https://github.com/clash-lang/clash-cores/issues/35.

@IOBUF@'s @IO@ port is a @std_ulogic@ in VHDL, while Clash represents a 1-bit
bidirectional signal as a @std_logic_vector(0 downto 0)@, so we only test this
in (System)Verilog.
-}
module Iobuf where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Xpm.Cdc.Internal

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
      (BiSignalInPort @"IO" io)
      (Port @"I" i)
      (Port @"T" t)

topEntity ::
  -- | (I, T, value driven onto IO by another driver)
  Signal Dom (Bit, Bit, Maybe Bit) ->
  -- | (O, value read from IO)
  Signal Dom (Bit, Bit)
topEntity input = bundle (o, readFromBiSignal bus)
 where
  (i, t, otherDriver) = unbundle input
  bus = veryUnsafeToBiSignalIn (mergeBiSignalOuts (ioOut :> otherOut :> Nil))
  otherOut = writeToBiSignal bus (fmap pack <$> otherDriver)
  (ioOut, o) = iobuf bus i t
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
  expected = (0, 0) :> (1, 1) :> (1, 1) :> (0, 0) :> (0, 0) :> (1, 1) :> (1, 1) :> Nil
  done = outputVerifier' clk rst expected (topEntity input)
  clk = tbClockGen (not <$> done)
  rst = resetGen
{-# OPAQUE testBench #-}
