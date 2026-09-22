{- |
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

UltraScale SelectIO primitives for a 1.25 Gb/s LVDS serializer and
deserializer, instantiated through "Clash.Cores.Xilinx.Xpm.Cdc.Internal", with
behavioural models for simulation. The serial line is modelled in the
'Line1250' domain, one bit per cycle.
-}
module Kcu105.Sgmii.Primitives where

import Clash.Cores.Xilinx.Xpm.Cdc.Internal (
  ClockPort (..),
  InstConfig (..),
  Param (..),
  Port (..),
  ResetPort (..),
  inst,
  instConfig,
  unPort,
 )
import Clash.Explicit.Prelude
import Kcu105.Sgmii.Domains

-- | Instantiation config for a UNISIM primitive
unisim :: String -> InstConfig
unisim name =
  (instConfig name)
    { library = Just "UNISIM"
    , libraryImport = Just "UNISIM.vcomponents.all"
    }

-- | Differential input buffer for a data signal. Simulation: the P channel.
ibufds :: forall dom. (KnownDomain dom) => Signal dom Bit -> Signal dom Bit -> Signal dom Bit
ibufds p n
  | clashSimulation = p
  | otherwise = unPort go
 where
  go :: Port "O" dom Bit
  go = inst (unisim "IBUFDS") (Port @"I" p) (Port @"IB" n)

-- | Differential output buffer: the P and N channel. Simulation: the input and
--   its complement.
obufds :: forall dom. (KnownDomain dom) => Signal dom Bit -> (Signal dom Bit, Signal dom Bit)
obufds i
  | clashSimulation = (i, complement <$> i)
  | otherwise = (unPort o, unPort ob)
 where
  (o, ob) = go
  go :: (Port "O" dom Bit, Port "OB" dom Bit)
  go = inst (unisim "OBUFDS") (Port @"I" i)

-- | 1:4 deserializer: ISERDESE3 in DDR mode with a 4-bit parallel output. The
-- 625 MHz clock drives both @CLK@ and, inverted inside the primitive, @CLK_B@.
-- Bit 0 of the output is assumed to be the bit received first; whether the
-- primitive really orders its bits this way is settled on hardware, see
-- 'Kcu105.Sgmii.Serdes.rxPath'.
iserdese3 ::
  Clock Serdes625 ->
  Clock Serdes312 ->
  Reset Serdes312 ->
  Signal Line1250 Bit ->
  Signal Serdes312 (BitVector 4)
iserdese3 clk clkDiv rst d
  | clashSimulation = deserializeModel clkDiv d
  | otherwise = truncateB <$> unPort q
 where
  (q, _fifoEmpty, _internalDivClk) = go
  go ::
    ( Port "Q" Serdes312 (BitVector 8)
    , Port "FIFO_EMPTY" Serdes312 Bit
    , Port "INTERNAL_DIVCLK" Serdes312 Bit
    )
  go =
    inst
      (unisim "ISERDESE3")
      (Param @"DATA_WIDTH" @Integer 4)
      (Param @"IS_CLK_B_INVERTED" @Bit 1)
      (ClockPort @"CLK" clk)
      (ClockPort @"CLK_B" clk)
      (ClockPort @"CLKDIV" clkDiv)
      (Port @"D" d)
      (ClockPort @"FIFO_RD_CLK" clkDiv)
      (Port @"FIFO_RD_EN" (pure 0 :: Signal Serdes312 Bit))
      (ResetPort @"RST" @'ActiveHigh rst)

-- | Simulation model of 'iserdese3': every four line bits form a nibble with
--   the first received bit at bit 0
deserializeModel :: Clock Serdes312 -> Signal Line1250 Bit -> Signal Serdes312 (BitVector 4)
deserializeModel clkDiv d = unsafeSynchronizer lineClk clkDiv nibble
 where
  lineClk = clockGen @Line1250
  nibble = register lineClk resetGen enableGen 0 (shiftIn <$> nibble <*> d)
  shiftIn acc b = (acc `shiftR` 1) .|. (resize (pack b) `shiftL` 3)

-- | 4:1 serializer: OSERDESE3 in DDR mode with a 4-bit parallel input. Bit 0
-- of the input is sent first.
oserdese3 ::
  Clock Serdes625 ->
  Clock Serdes312 ->
  Reset Serdes312 ->
  Signal Serdes312 (BitVector 4) ->
  Signal Line1250 Bit
oserdese3 clk clkDiv rst d
  | clashSimulation = serializeModel clkDiv d
  | otherwise = unPort oq
 where
  (oq, _tOut) = go
  go :: (Port "OQ" Line1250 Bit, Port "T_OUT" Line1250 Bit)
  go =
    inst
      (unisim "OSERDESE3")
      (Param @"DATA_WIDTH" @Integer 4)
      (ClockPort @"CLK" clk)
      (ClockPort @"CLKDIV" clkDiv)
      (Port @"D" (resize <$> d :: Signal Serdes312 (BitVector 8)))
      (ResetPort @"RST" @'ActiveHigh rst)
      (Port @"T" (pure 0 :: Signal Serdes312 Bit))

-- | Simulation model of 'oserdese3': the bits of each nibble appear on the
--   line one per cycle, bit 0 first
serializeModel :: Clock Serdes312 -> Signal Serdes312 (BitVector 4) -> Signal Line1250 Bit
serializeModel clkDiv d = (!) <$> held <*> counter
 where
  lineClk = clockGen @Line1250
  held = unsafeSynchronizer clkDiv lineClk d
  counter = register lineClk resetGen enableGen (0 :: Index 4) (satSucc SatWrap <$> counter)

-- | Variable input delay: IDELAYE3 in @COUNT@ mode with @VAR_LOAD@, so the tap
-- value can be loaded from the fabric without an IDELAYCTRL. Returns the delayed
-- line and the tap value the primitive reports. Simulation: no delay.
idelaye3 ::
  Clock Serdes312 ->
  Reset Serdes312 ->
  -- | Tap value to load
  Signal Serdes312 (Unsigned 9) ->
  -- | Load the tap value
  Signal Serdes312 Bool ->
  -- | Line from the input buffer
  Signal Line1250 Bit ->
  (Signal Line1250 Bit, Signal Serdes312 (Unsigned 9))
idelaye3 clk rst tap load din
  | clashSimulation = (din, tapModel)
  | otherwise = (unPort dataOut, unpack <$> unPort cntOut)
 where
  tapModel = register clk rst enableGen 0 (mux load tap tapModel)
  (_cascOut, cntOut, dataOut) = go
  go ::
    ( Port "CASC_OUT" Line1250 Bit
    , Port "CNTVALUEOUT" Serdes312 (BitVector 9)
    , Port "DATAOUT" Line1250 Bit
    )
  go =
    inst
      (unisim "IDELAYE3")
      (Param @"DELAY_FORMAT" @String "COUNT")
      (Param @"DELAY_TYPE" @String "VAR_LOAD")
      (Param @"DELAY_VALUE" @Integer 0)
      (Port @"CASC_IN" (pure 0 :: Signal Line1250 Bit))
      (Port @"CASC_RETURN" (pure 0 :: Signal Line1250 Bit))
      (Port @"CE" (pure 0 :: Signal Serdes312 Bit))
      (ClockPort @"CLK" clk)
      (Port @"CNTVALUEIN" (pack <$> tap))
      (Port @"DATAIN" (pure 0 :: Signal Line1250 Bit))
      (Port @"EN_VTC" (pure 0 :: Signal Serdes312 Bit))
      (Port @"IDATAIN" din)
      (Port @"INC" (pure 0 :: Signal Serdes312 Bit))
      (Port @"LOAD" (boolToBit <$> load))
      (ResetPort @"RST" @'ActiveHigh rst)
