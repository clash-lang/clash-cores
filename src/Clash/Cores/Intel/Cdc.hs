{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Clock domain crossing primitives of Intel FPGAs: the synchronizer
@altera_std_synchronizer@ and its multi-bit version
@altera_std_synchronizer_bundle@, and the Avalon streaming clock crosser of
Platform Designer. Quartus recognizes the registers of these
synchronizers as synchronizers: it reports them in its metastability analysis,
keeps them apart from other logic, and cuts the timing paths into them.

For more information see the section /Metastability Analysis/ of the
/Quartus Prime Standard Edition User Guide: Timing Analyzer/
(<https://www.intel.com/content/www/us/en/docs/programmable/683068/current/metastability-analysis.html>).

For FIFOs that cross clock domains, see "Clash.Cores.Intel.Fifo".
-}

{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Intel.Cdc
  ( -- * Synchronizers
    stdSynchronizer
  , stdSynchronizerBundle
    -- * Avalon streaming clock crosser
  , avalonStClockCrosser
  , ClockCrosserConfig (..)
  , defClockCrosserConfig
  ) where

import Clash.Explicit.Prelude

import Clash.Signal.Internal (ClockAB (..), Signal ((:-)), clockTicks)
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)

import qualified Prelude as P

import Clash.Cores.Intel.Internal (IntelIp (..), IpParam (..), instWithIntelIp)
import Clash.Cores.Intel.Primitive.Internal (alteraMf)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( ClockPort (..), Param (..), Port (..), ResetPort (..), inst, instConfig, unPort )

{- | Synchronizer (@altera_std_synchronizer@): a chain of @depth@ registers in
the destination domain, with an asynchronous reset to 0. The first register
samples the input, which may come from any domain, so its output may go
metastable; the other registers give it time to settle. Use it to cross single
bits that change slowly compared to the destination clock, such as status
flags. A synchronizer delays its input by @depth@ cycles of the destination
clock.

Simulation: @depth@ registers. The input from the source domain is sampled at
the active edges of the destination clock, see 'unsafeSynchronizer'.

__NB__: Intel's VHDL simulation model resets the registers synchronously,
while its Verilog model and the hardware reset them asynchronously.
-}
stdSynchronizer ::
  forall depth src dst.
  ( HasCallStack
  , KnownNat depth
  , 2 <= depth
  , KnownDomain src
  , KnownDomain dst
  , HasAsynchronousReset dst
  ) =>
  -- | Number of registers (@depth@), at least 2. Intel's default is 3.
  SNat depth ->
  -- | Destination clock (@clk@)
  Clock dst ->
  -- | Asynchronous reset (@reset_n@)
  Reset dst ->
  -- | Input (@din@)
  Signal src Bit ->
  -- | Output (@dout@)
  Signal dst Bit
stdSynchronizer depth clk rst din
  | clashSimulation = model depth clk rst din
  | otherwise = unPort go
 where
  go :: Port "dout" dst Bit
  go =
    inst
      (alteraMf "altera_std_synchronizer")
      (Param @"depth" @Integer (snatToInteger depth))
      (ClockPort @"clk" clk)
      (ResetPort @"reset_n" @'ActiveLow rst)
      (Port @"din" din)

{- | Synchronizer for a bundle of bits (@altera_std_synchronizer_bundle@): a
'stdSynchronizer' for every bit of the input.

__NB__: The bits are synchronized independently: when several bits change at
once, the output can show a combination of old and new bits for a cycle. Only
use it for values of which at most one bit changes at a time, such as Gray
codes, or that are stable while the destination reads them.
-}
stdSynchronizerBundle ::
  forall depth a src dst.
  ( HasCallStack
  , KnownNat depth
  , 2 <= depth
  , KnownDomain src
  , KnownDomain dst
  , HasAsynchronousReset dst
  , BitPack a
  , NFDataX a
  ) =>
  -- | Number of registers (@depth@), at least 2. Intel's default is 3.
  SNat depth ->
  -- | Destination clock (@clk@)
  Clock dst ->
  -- | Asynchronous reset (@reset_n@). Resets the output to all zeroes.
  Reset dst ->
  -- | Input (@din@)
  Signal src a ->
  -- | Output (@dout@)
  Signal dst a
stdSynchronizerBundle depth clk rst din
  | clashSimulation = unpack <$> model depth clk rst (pack <$> din)
  | otherwise = unpack <$> unPort go
 where
  go :: Port "dout" dst (BitVector (BitSize a))
  go =
    inst
      (alteraMf "altera_std_synchronizer_bundle")
      (Param @"depth" @Integer (snatToInteger depth))
      (Param @"width" @Integer (natToNum @(BitSize a)))
      (ClockPort @"clk" clk)
      (ResetPort @"reset_n" @'ActiveLow rst)
      (Port @"din" (pack <$> din))

-- | Simulation model of the synchronizers
model ::
  forall depth a src dst.
  (KnownNat depth, NFDataX a, Num a, KnownDomain src, KnownDomain dst) =>
  SNat depth ->
  Clock dst ->
  Reset dst ->
  Signal src a ->
  Signal dst a
model depth clk rst din =
  foldr
    (\_ s -> register clk rst enableGen 0 s)
    (unsafeSynchronizer clockGen clk din)
    (replicate depth ())

-- * Avalon streaming clock crosser

-- | Configuration of 'avalonStClockCrosser'
data ClockCrosserConfig = ClockCrosserConfig
  { validSyncDepth :: Int
  -- ^ Number of registers synchronizing a transfer to the output domain
  -- (@VALID_SYNC_DEPTH@), at least 2
  , readySyncDepth :: Int
  -- ^ Number of registers synchronizing its acknowledgement back to the input
  -- domain (@READY_SYNC_DEPTH@), at least 2
  , outputPipeline :: Bool
  -- ^ Add a pipeline stage to the output (@USE_OUTPUT_PIPELINE@): a register
  -- on the output and on the ready signal into the clock crosser, so that
  -- @out_ready@ and @out_valid@ don't go through logic
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Intel's default configuration: two synchronizer registers in both
-- directions, and an output pipeline stage
defClockCrosserConfig :: ClockCrosserConfig
defClockCrosserConfig =
  ClockCrosserConfig
    { validSyncDepth = 2
    , readySyncDepth = 2
    , outputPipeline = True
    }

{- | Avalon streaming clock crosser (@altera_avalon_st_handshake_clock_crosser@,
the /Avalon-ST Handshake Clock Crosser/ of Platform Designer): moves a stream
of values of type @a@ from one clock domain to another, with a handshake per
value. For more information see the /Platform Designer/ chapter of the
/Quartus Prime Standard Edition User Guide/
(<https://www.intel.com/content/www/us/en/docs/programmable/683364/current/avalon-st-handshake-clock-crosser.html>).

The input is an Avalon streaming sink with a ready latency of 0: the clock
crosser takes the value of a 'Just' in a cycle in which it asserts its ready
output (@in_ready@). The output is an Avalon streaming source: it presents a
value ('Just') until the cycle in which the ready input (@out_ready@) is
asserted. It takes a value at a time: it deasserts @in_ready@ until the
acknowledgement of the output domain has arrived, so its throughput is at most
one value per round trip through the synchronizers. For a higher throughput,
use a dual-clock FIFO, see "Clash.Cores.Intel.Fifo".

Both resets are asynchronous, and should be asserted together.

The clock crosser is Platform Designer IP: see "Clash.Cores.Intel.Internal" for
how to generate it. The simulation model matches the IP's simulation model
cycle by cycle, when the domains of the clocks describe them exactly.
-}
avalonStClockCrosser ::
  forall a inDom outDom.
  ( HasCallStack
  , KnownDomain inDom
  , KnownDomain outDom
  , HasAsynchronousReset inDom
  , HasAsynchronousReset outDom
  , BitPack a
  , NFDataX a
  , 1 <= BitSize a
  ) =>
  ClockCrosserConfig ->
  -- | Input clock (@in_clk@)
  Clock inDom ->
  -- | Input reset (@in_reset@)
  Reset inDom ->
  -- | Output clock (@out_clk@)
  Clock outDom ->
  -- | Output reset (@out_reset@)
  Reset outDom ->
  -- | Input: @in_valid@ and @in_data@
  Signal inDom (Maybe a) ->
  -- | Ready of the output (@out_ready@)
  Signal outDom Bool ->
  -- | Ready of the input (@in_ready@), and the output: @out_valid@ and
  -- @out_data@
  (Signal inDom Bool, Signal outDom (Maybe a))
avalonStClockCrosser cfg@ClockCrosserConfig{..} inClk inRst outClk outRst inp outReady
  | validSyncDepth < 2 || readySyncDepth < 2 =
      error "avalonStClockCrosser: the synchronizers need at least 2 registers"
  | clashSimulation =
      let
        (rdy, out) =
          clockCrosserModel @(BitSize a) cfg inClk inRst outClk outRst
            (fmap pack <$> inp) outReady
      in (rdy, fmap unpack <$> out)
  | otherwise =
      ( bitToBool <$> unPort inReadyPort
      , (\v d -> if bitToBool v then Just (unpack d) else Nothing)
          <$> unPort outValidPort <*> unPort outDataPort )
 where
  (inReadyPort, outValidPort, outDataPort) = ports
  ports ::
    ( Port "in_ready" inDom Bit
    , Port "out_valid" outDom Bit
    , Port "out_data" outDom (BitVector (BitSize a)) )
  ports =
    instWithIntelIp
      (instConfig "avalon_st_clock_crosser")
      IntelIp
        { ipKind = "altera_avalon_st_handshake_clock_crosser"
        , ipParams =
               ("DATA_WIDTH", IntegerParam (natToNum @(BitSize a)))
            :> ("BITS_PER_SYMBOL", IntegerParam (natToNum @(BitSize a)))
            :> ("VALID_SYNC_DEPTH", IntegerParam (toInteger validSyncDepth))
            :> ("READY_SYNC_DEPTH", IntegerParam (toInteger readySyncDepth))
            :> ("USE_OUTPUT_PIPELINE", IntegerParam (if outputPipeline then 1 else 0))
            :> Nil
        }
      (ClockPort @"in_clk" inClk)
      (ResetPort @"in_reset" @'ActiveHigh inRst)
      (ClockPort @"out_clk" outClk)
      (ResetPort @"out_reset" @'ActiveHigh outRst)
      (Port @"in_valid" (boolToBit . isJust <$> inp))
      (Port @"in_data" (maybe 0 pack <$> inp))
      (Port @"out_ready" (boolToBit <$> outReady))

-- | State of the simulation model of 'avalonStClockCrosser'
data ClockCrosserState w = ClockCrosserState
  { -- Input domain
    ccInToggle :: Bool
  , ccInBuffer :: BitVector w
  , ccBackSync :: [Bool]
  -- ^ Synchronizer of 'ccOutToggle' into the input domain, first register
  -- first
    -- Output domain
  , ccFwdSync :: [Bool]
  -- ^ Synchronizer of 'ccInToggle' into the output domain
  , ccOutToggle :: Bool
  , ccOutBuffer :: BitVector w
    -- Output pipeline stage: a skid buffer of two registers
  , ccFull0 :: Bool
  , ccFull1 :: Bool
  , ccData0 :: BitVector w
  , ccData1 :: BitVector w
  }

{- | Simulation model of 'avalonStClockCrosser'. The input domain toggles a
bit when it takes a value, which the output domain synchronizes; the output
domain presents the value until it is taken, and toggles a bit back, which the
input domain synchronizes to see that it can take the next value.
-}
clockCrosserModel ::
  forall w inDom outDom.
  (KnownNat w, KnownDomain inDom, KnownDomain outDom) =>
  ClockCrosserConfig ->
  Clock inDom ->
  Reset inDom ->
  Clock outDom ->
  Reset outDom ->
  Signal inDom (Maybe (BitVector w)) ->
  Signal outDom Bool ->
  (Signal inDom Bool, Signal outDom (Maybe (BitVector w)))
clockCrosserModel ClockCrosserConfig{..} inClk inRst outClk outRst inp outReady =
  case (unsafeToActiveHigh inRst, unsafeToActiveHigh outRst) of
    (ir0 :- irs, or0 :- ors) ->
      go (clockTicks inClk outClk) initState ir0 irs or0 ors inp outReady
 where
  initState =
    ClockCrosserState
      { ccInToggle = False
      , ccInBuffer = 0
      , ccBackSync = P.replicate readySyncDepth False
      , ccFwdSync = P.replicate validSyncDepth False
      , ccOutToggle = False
      , ccOutBuffer = 0
      , ccFull0 = False
      , ccFull1 = False
      , ccData0 = 0
      , ccData1 = 0
      }

  resetIn s =
    s{ccInToggle = False, ccInBuffer = 0, ccBackSync = ccBackSync initState}
  resetOut s =
    s { ccFwdSync = ccFwdSync initState, ccOutToggle = False, ccOutBuffer = 0
      , ccFull0 = False, ccFull1 = False, ccData0 = 0, ccData1 = 0 }

  inReady s = P.last (ccBackSync s) == ccInToggle s
  validInternal s = P.last (ccFwdSync s) /= ccOutToggle s

  outputs s
    | outputPipeline = if ccFull1 s then Just (ccData1 s) else Nothing
    | validInternal s = Just (ccOutBuffer s)
    | otherwise = Nothing

  -- An edge of the input clock: the input fields of the new state
  inEdge s i =
    s { ccInToggle = ccInToggle s /= takes
      , ccInBuffer = case i of
          Just d | takes -> d
          _ -> ccInBuffer s
      , ccBackSync = ccOutToggle s : P.init (ccBackSync s)
      }
   where
    takes = isJust i && inReady s

  -- An edge of the output clock: the output fields of the new state
  outEdge s rdy =
    s { ccFwdSync = ccInToggle s : P.init (ccFwdSync s)
      , ccOutBuffer = ccInBuffer s
      , ccOutToggle = if taken then P.last (ccFwdSync s) else ccOutToggle s
      , ccFull0 = full0
      , ccFull1 = full1
      , ccData0 = if ccFull0 s then ccData0 s else ccOutBuffer s
      , ccData1 =
          if not (ccFull1 s) || (rdy && ccFull1 s)
            then (if ccFull0 s then ccData0 s else ccOutBuffer s)
            else ccData1 s
      }
   where
    v = validInternal s
    readyInternal
      | outputPipeline = not (ccFull0 s)
      | otherwise = rdy
    taken = readyInternal && v
    f0 = ccFull0 s
    f1 = ccFull1 s
    full1
      | not f0 && not f1 && v = True
      | f1 && not f0 && not v && rdy = False
      | otherwise = f1
    full0
      | f1 && not f0 && v && not rdy = True
      | f1 && f0 && rdy = False
      | otherwise = f0

  -- The resets are asynchronous: a reset sample is the reset during the clock
  -- cycle that ends with the corresponding edge. While it is asserted, the
  -- state is the reset state, also after that edge.
  go [] _ _ _ _ _ _ _ = error "avalonStClockCrosser: clockTicks should be infinite"
  go (tick : ticks) s irNow irs@(~(irNext :- irs1)) orNow ors@(~(orNext :- ors1)) is@(~(i :- is1)) rs@(~(r :- rs1)) =
    case tick of
      ClockA -> (inReady s :- restIn, restOut)
       where
        s' = withIn irNext (if irNow then s else inEdge s i)
        (restIn, restOut) = go ticks s' irNext irs1 orNow ors is1 rs
      ClockB -> (restIn, outputs s :- restOut)
       where
        s' = withOut orNext (if orNow then s else outEdge s r)
        (restIn, restOut) = go ticks s' irNow irs orNext ors1 is rs1
      ClockAB -> (inReady s :- restIn, outputs s :- restOut)
       where
        sIn = if irNow then s else inEdge s i
        sOut = if orNow then s else outEdge s r
        s' =
          withOut orNext $ withIn irNext $
            sOut
              { ccInToggle = ccInToggle sIn
              , ccInBuffer = ccInBuffer sIn
              , ccBackSync = ccBackSync sIn
              }
        (restIn, restOut) = go ticks s' irNext irs1 orNext ors1 is1 rs1

  withIn rst s = if rst then resetIn s else s
  withOut rst s = if rst then resetOut s else s
