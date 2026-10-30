{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Configuration, output type and simulation model shared by
  "Clash.Cores.Xilinx.Xpm.Fifo.Sync" and "Clash.Cores.Xilinx.Xpm.Fifo.Async".

  The simulation models follow the register transfer level description of
  @xpm_fifo_base@ (and @xpm_fifo_rst@, @xpm_cdc_gray@, @xpm_cdc_sync_rst@ and the
  read path of @xpm_memory_base@) in Vivado 2022.1, register by register. This
  makes them cycle accurate with respect to the XPM, including reset sequencing
  and flag latencies.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Fifo.Internal.Base where

import Clash.Explicit.Prelude

import qualified Prelude as P

import Data.Maybe (fromMaybe, isJust)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import Clash.Signal.Internal (Signal((:-)))

import Clash.Cores.Xilinx.Xpm.Fifo.Internal

-- | Configuration of 'Clash.Cores.Xilinx.Xpm.Fifo.Sync.xpmFifoSyncWith' and
-- 'Clash.Cores.Xilinx.Xpm.Fifo.Async.xpmFifoAsyncWith'. See
-- 'defXpmFifoConfig' for the defaults, which are the defaults of the XPM.
--
-- The FIFO holds @2^depth@ words (@FIFO_WRITE_DEPTH@). The exact number of
-- words that can be written before the FIFO signals full depends on the macro
-- and the read mode, see the macro's documentation.
data XpmFifoConfig depth = XpmFifoConfig
  { fifoDepth :: SNat depth
  -- ^ Logarithm (base 2) of @FIFO_WRITE_DEPTH@, which must be a power of two
  -- between 16 and 4194304.
  , fifoMemoryType :: FifoMemoryType
  -- ^ @FIFO_MEMORY_TYPE@. 'Ultra' is only supported by
  -- 'Clash.Cores.Xilinx.Xpm.Fifo.Sync.xpmFifoSync'.
  , readMode :: ReadMode
  -- ^ @READ_MODE@ and @FIFO_READ_LATENCY@
  , fullResetValue :: Bool
  -- ^ @FULL_RESET_VALUE@: value of @full@, @almost_full@ and @prog_full@ while
  -- the FIFO is in reset.
  , progFullThresh :: Natural
  -- ^ @PROG_FULL_THRESH@: @prog_full@ asserts when the number of words in the
  -- FIFO is at least this value. Only checked if 'enProgFull' is set. See
  -- 'progFullThreshRange' for the legal values.
  , progEmptyThresh :: Natural
  -- ^ @PROG_EMPTY_THRESH@: @prog_empty@ asserts when the number of words in
  -- the FIFO is at most this value. Only checked if 'enProgEmpty' is set. See
  -- 'progEmptyThreshRange' for the legal values.
  , advFeatures :: AdvFeatures
  -- ^ @USE_ADV_FEATURES@: optional outputs. Outputs that are not enabled are
  -- 'XException' in simulation, and a constant in HDL.
  , cdcSyncStages :: Natural
  -- ^ @CDC_SYNC_STAGES@: number of synchronization stages used when crossing
  -- clock domains. Only used by
  -- 'Clash.Cores.Xilinx.Xpm.Fifo.Async.xpmFifoAsync'. Must be between 2 and 8,
  -- at most 4 if @depth@ is 4, and exactly 2 if 'relatedClocks' is set.
  , relatedClocks :: Bool
  -- ^ @RELATED_CLOCKS@: set if the write and read clock are derived from the
  -- same source and have a known phase relation, in which case pointers cross
  -- without synchronizers. Only used by
  -- 'Clash.Cores.Xilinx.Xpm.Fifo.Async.xpmFifoAsync'.
  }
  deriving (Show, Generic)

-- | Default configuration. All settings correspond to the defaults of the XPM:
--
-- * 'fifoMemoryType': 'Auto'
-- * 'readMode': @'Std' 1@
-- * 'fullResetValue': 'False'
-- * 'progFullThresh': 10
-- * 'progEmptyThresh': 10
-- * 'advFeatures': 'defAdvFeatures' (@\"0707\"@)
-- * 'cdcSyncStages': 2
-- * 'relatedClocks': 'False'
--
-- The depth can be given as a type application, e.g. @defXpmFifoConfig \@5@ for
-- a FIFO with a @FIFO_WRITE_DEPTH@ of 32.
defXpmFifoConfig :: forall depth . KnownNat depth => XpmFifoConfig depth
defXpmFifoConfig = XpmFifoConfig
  { fifoDepth = SNat
  , fifoMemoryType = Auto
  , readMode = Std 1
  , fullResetValue = False
  , progFullThresh = 10
  , progEmptyThresh = 10
  , advFeatures = defAdvFeatures
  , cdcSyncStages = 2
  , relatedClocks = False
  }

-- | Output of 'Clash.Cores.Xilinx.Xpm.Fifo.Sync.xpmFifoSync' and
-- 'Clash.Cores.Xilinx.Xpm.Fifo.Async.xpmFifoAsync'. Field names follow
-- 'Clash.Cores.Xilinx.DcFifo.FifoOut' where possible; the XPM port names are
-- given between brackets. All outputs are registered.
data XpmFifoOut write read depth a = XpmFifoOut
  { isFull :: Signal write Bool
  -- ^ (@full@) The FIFO is full. Writes are ignored while full.
  , isAlmostFull :: Signal write Bool
  -- ^ (@almost_full@) Only one more write can be performed before the FIFO is
  -- full. Requires 'enAlmostFull'.
  , isProgFull :: Signal write Bool
  -- ^ (@prog_full@) The number of words in the FIFO is at least
  -- 'progFullThresh'. Requires 'enProgFull'.
  , writeCount :: Signal write (Unsigned (depth + 1))
  -- ^ (@wr_data_count@) Number of words in the FIFO, as seen from the write
  -- side. Requires 'enWrDataCount'.
  , isOverflow :: Signal write Bool
  -- ^ (@overflow@) The write in the previous cycle was rejected, because the
  -- FIFO was full or busy resetting. Requires 'enOverflow'.
  , writeAck :: Signal write Bool
  -- ^ (@wr_ack@) The write in the previous cycle succeeded. Requires
  -- 'enWrAck'.
  , writeResetBusy :: Signal write Bool
  -- ^ (@wr_rst_busy@) The write side is busy resetting. Writes are ignored while
  -- busy.
  , fifoData :: Signal read a
  -- ^ (@dout@) Read data. In standard read mode, data appears the configured
  -- latency after a read. In FWFT mode, it is the word at the head of the
  -- FIFO, valid while not 'isEmpty'. Resets to all zeroes.
  , isEmpty :: Signal read Bool
  -- ^ (@empty@) The FIFO is empty. Reads are ignored while empty.
  , isAlmostEmpty :: Signal read Bool
  -- ^ (@almost_empty@) Only one more read can be performed before the FIFO is
  -- empty. Requires 'enAlmostEmpty'.
  , isProgEmpty :: Signal read Bool
  -- ^ (@prog_empty@) The number of words in the FIFO is at most
  -- 'progEmptyThresh'. Requires 'enProgEmpty'.
  , readCount :: Signal read (Unsigned (depth + 1))
  -- ^ (@rd_data_count@) Number of words in the FIFO, as seen from the read
  -- side. Requires 'enRdDataCount'.
  , isUnderflow :: Signal read Bool
  -- ^ (@underflow@) The read in the previous cycle was rejected, because the
  -- FIFO was empty or busy resetting. Requires 'enUnderflow'.
  , isDataValid :: Signal read Bool
  -- ^ (@data_valid@) 'fifoData' holds valid data. In standard read mode this
  -- asserts the configured latency after a successful read, in FWFT mode it
  -- is the inverse of 'isEmpty'. Requires 'enDataValid'.
  , readResetBusy :: Signal read Bool
  -- ^ (@rd_rst_busy@) The read side is busy resetting.
  }

-- | Check a configuration against the design rule checks of the XPM, and the
-- limitations of this wrapper. Returns an error message if the configuration
-- is invalid.
checkXpmFifoConfig ::
  forall depth .
  -- | Whether the configuration is used for an @xpm_fifo_async@
  Bool ->
  -- | Data width
  Integer ->
  XpmFifoConfig depth ->
  Maybe String
checkXpmFifoConfig isAsync width XpmFifoConfig{fifoDepth=fifoDepth@SNat, ..} =
  case P.filter (not . P.null) checks of
    [] -> Nothing
    errs -> Just (P.unlines errs)
 where
  depthLog = snatToInteger fifoDepth
  depth = 2 P.^ depthLog
  fwft = isFwft readMode
  cdcStages = toInteger cdcSyncStages
  inRange (lo, hi) v = lo <= v && v <= hi
  check b msg = if b then "" else msg
  showRange (lo, hi) = show lo <> " and " <> show hi
  checks =
    [ check (inRange (4, 22) depthLog) $
        "FIFO_WRITE_DEPTH must be between 2^4 and 2^22, but is 2^" <> show depthLog
    , check (width >= 1) "Data must be at least one bit wide"
    , check (clog2 width + depthLog <= 30) $
        "Data width (" <> show width <> ") times depth (" <> show depth <> ") "
        <> "exceeds the maximum FIFO size"
    , case readMode of
        Std n -> check (n >= 1) "FIFO_READ_LATENCY must be at least 1"
        Fwft -> ""
    , check (not (isAsync && fifoMemoryType == Ultra))
        "UltraRAM cannot be used for an asynchronous FIFO"
    , let rng = progFullThreshRange depth fwft (if isAsync then Just cdcStages else Nothing)
      in check (not (enProgFull advFeatures) || inRange rng (toInteger progFullThresh)) $
           "PROG_FULL_THRESH (" <> show progFullThresh <> ") must be between "
           <> showRange rng
    , let rng = progEmptyThreshRange depth fwft
      in check (not (enProgEmpty advFeatures) || inRange rng (toInteger progEmptyThresh)) $
           "PROG_EMPTY_THRESH (" <> show progEmptyThresh <> ") must be between "
           <> showRange rng
    , let rng = cdcSyncStagesRange depth relatedClocks
      in check (not isAsync || inRange rng cdcStages) $
           "CDC_SYNC_STAGES (" <> show cdcStages <> ") must be between "
           <> showRange rng
    ]

  clog2 :: Integer -> Integer
  clog2 v = go 0 where go i = if 2 P.^ i >= v then i else go (i + 1)

-- | Error out if a configuration is invalid, see 'checkXpmFifoConfig'.
assertXpmFifoConfig ::
  String -> Bool -> Integer -> XpmFifoConfig depth -> a -> a
assertXpmFifoConfig nm isAsync width cfg a =
  case checkXpmFifoConfig isAsync width cfg of
    Nothing -> a
    Just err -> P.error (nm <> ": invalid configuration:\n" <> err)

-- | Replace outputs that are not enabled by 'XException's
maskDisabled :: AdvFeatures -> XpmFifoOut write read depth a -> XpmFifoOut write read depth a
maskDisabled AdvFeatures{..} out = out
  { isAlmostFull = mask enAlmostFull "almost_full" (isAlmostFull out)
  , isProgFull = mask enProgFull "prog_full" (isProgFull out)
  , writeCount = mask enWrDataCount "wr_data_count" (writeCount out)
  , isOverflow = mask enOverflow "overflow" (isOverflow out)
  , writeAck = mask enWrAck "wr_ack" (writeAck out)
  , isAlmostEmpty = mask enAlmostEmpty "almost_empty" (isAlmostEmpty out)
  , isProgEmpty = mask enProgEmpty "prog_empty" (isProgEmpty out)
  , readCount = mask enRdDataCount "rd_data_count" (readCount out)
  , isUnderflow = mask enUnderflow "underflow" (isUnderflow out)
  , isDataValid = mask enDataValid "data_valid" (isDataValid out)
  }
 where
  mask :: Bool -> String -> Signal dom b -> Signal dom b
  mask True _ s = s
  mask False nm _ = pure (errorX (nm <> " not enabled in USE_ADV_FEATURES"))

-- * Simulation model

-- | Moore machine with an explicit initial state, independent of the domain's
-- initial value behavior: the XPM's registers always have initial values.
mooreS :: (s -> s) -> (s -> i -> s) -> s -> Signal dom i -> Signal dom s
mooreS force f = go
 where
  go s ~(i :- is) = s :- go (force (f s i)) is

-- | Evaluate a value to WHNF, unless it is an 'XException'. Used to prevent
-- thunks from building up in the state of the simulation models.
whnfX :: a -> ()
whnfX a = a `seqX` ()

-- | Shift a value into a register chain (head is the first register)
shiftIn :: a -> [a] -> [a]
shiftIn a as = P.take (P.length as) (a : as)

-- | Last element of a register chain, or the given value if the chain is empty
lastOr :: a -> [a] -> a
lastOr a [] = a
lastOr _ as = P.last as

-- | States of the FWFT state machine in @xpm_fifo_base@
data FwftState
  = FwftInvalid
  -- ^ No data in the output stages
  | FwftStage1
  -- ^ Data in the memory output latch
  | FwftStage2
  -- ^ Data in the output register
  | FwftBoth
  -- ^ Data in both
  deriving (Show, Eq, Generic, NFDataX)

-- | State of the read path: the FWFT state machine and the output registers
-- of the memory.
data ReadPath a = ReadPath
  { rpFwftState :: FwftState
  , rpEmptyFwft :: Bool
  , rpAemptyFwft :: Bool
  , rpDataValidFwft :: Bool
  , rpExtraWords :: Unsigned 2
  -- ^ Words read from memory, but still in the FWFT output stages
  , rpRegcePipe :: [Bool]
  -- ^ @ram_regce_pipe@ delay line (standard mode, latency > 1)
  , rpDoutReg :: a
  -- ^ Memory output latch (@doutb_reg@)
  , rpEnbPipe :: [Bool]
  -- ^ Memory enable pipeline (@enb_pipe@)
  , rpDoutPipe :: [a]
  -- ^ Memory output pipeline (@doutb_pipe@), the last one drives @dout@
  , rpDataValidStd :: Bool
  }

-- | Read latency of the memory used by the FIFO
memReadLatency :: ReadMode -> Int
memReadLatency = \case
  Std n -> fromIntegral n
  Fwft -> 2

initReadPath :: ReadMode -> a -> ReadPath a
initReadPath rm rstVal = ReadPath
  { rpFwftState = FwftInvalid
  , rpEmptyFwft = True
  , rpAemptyFwft = True
  , rpDataValidFwft = False
  , rpExtraWords = 0
  , rpRegcePipe = case rm of
      Std n -> P.replicate (fromIntegral n - 1) False
      Fwft -> []
  , rpDoutReg = rstVal
  , rpEnbPipe = P.replicate (lat - 1) False
  , rpDoutPipe = P.replicate (lat - 1) rstVal
  , rpDataValidStd = False
  }
 where
  lat = memReadLatency rm

forceReadPath :: ReadPath a -> ReadPath a
forceReadPath rp@ReadPath{..} =
  whnfX rpFwftState `seq` whnfX rpEmptyFwft `seq` whnfX rpAemptyFwft `seq`
  whnfX rpDataValidFwft `seq` whnfX rpExtraWords `seq`
  P.foldr (seq . whnfX) () rpRegcePipe `seq`
  P.foldr (seq . whnfX) () rpEnbPipe `seq`
  whnfX rpDataValidStd `seq` rp

-- | Output data of the read path
readPathDout :: ReadPath a -> a
readPathDout ReadPath{rpDoutReg, rpDoutPipe} = lastOr rpDoutReg rpDoutPipe

-- | User visible empty flag (@empty_i@)
readPathEmpty :: ReadMode -> Bool -> ReadPath a -> Bool
readPathEmpty rm ramEmpty rp = if isFwft rm then rpEmptyFwft rp else ramEmpty

-- | Read enable towards memory before the empty check (@rd_en_i@)
readPathRdEn ::
  ReadMode ->
  -- | @ram_empty_i@
  Bool ->
  -- | @rd_en@
  Bool ->
  ReadPath a ->
  Bool
readPathRdEn rm ramEmpty rdEn ReadPath{rpFwftState} = case rm of
  Std _ -> rdEn
  Fwft -> case rpFwftState of
    FwftBoth -> not ramEmpty && rdEn
    _ -> not ramEmpty

-- | @invalid_state@: used to reset the read data count
readPathInvalid :: ReadMode -> ReadPath a -> Bool
readPathInvalid rm ReadPath{rpFwftState} = isFwft rm && rpFwftState == FwftInvalid

-- | Almost empty flag of the read path in FWFT mode
readPathAlmostEmpty :: ReadMode -> Bool -> ReadPath a -> Bool
readPathAlmostEmpty rm ramAempty rp = if isFwft rm then rpAemptyFwft rp else ramAempty

-- | Data valid flag
readPathDataValid :: ReadMode -> ReadPath a -> Bool
readPathDataValid rm rp = if isFwft rm then rpDataValidFwft rp else rpDataValidStd rp

-- | Next state of the read path
readPathStep ::
  forall a .
  ReadMode ->
  -- | Reset value of the output (@DOUT_RESET_VALUE@)
  a ->
  -- | Read side reset (@rd_rst_i@)
  Bool ->
  -- | User read enable (@rd_en@)
  Bool ->
  -- | @ram_empty_i@
  Bool ->
  -- | @ram_rd_en_i@
  Bool ->
  -- | Memory contents at the read pointer
  a ->
  ReadPath a ->
  ReadPath a
readPathStep rm rstVal rst rdEn ramEmpty ramRdEn memOut rp@ReadPath{..} =
  case rm of
    Std n -> stdStep (fromIntegral n)
    Fwft -> fwftStep
 where
  stdStep :: Int -> ReadPath a
  stdStep lat = rp
    { rpRegcePipe = if rst then P.map (const False) rpRegcePipe else shiftIn ramRdEn rpRegcePipe
    , rpDoutReg =
        if lat == 1 && rst then rstVal else if ramRdEn then memOut else rpDoutReg
    , rpEnbPipe = shiftIn ramRdEn rpEnbPipe
    , rpDoutPipe = pipeStep regce
    , rpDataValidStd = not rst && (if lat == 1 then ramRdEn else regce)
    }
   where
    regce = lastOr False rpRegcePipe

  -- Memory output pipeline. All stages but the last are enabled by the memory's
  -- enable pipeline, the last one by the FIFO's regce. Only the last stage is
  -- reset.
  pipeStep :: Bool -> [a]
  pipeStep regce = case rpDoutPipe of
    [] -> []
    _ -> P.init inner <> [final]
   where
    prevs = rpDoutReg : rpDoutPipe
    inner = P.zipWith3 (\en prev cur -> if en then prev else cur) rpEnbPipe prevs rpDoutPipe
    final
      | rst = rstVal
      | regce = P.last (P.init prevs)
      | otherwise = P.last rpDoutPipe

  fwftStep :: ReadPath a
  fwftStep = rp
    { rpFwftState = if rst then FwftInvalid else next
    , rpEmptyFwft = rst || emptyNext
    , rpAemptyFwft = rst || goingAempty || (not leavingAempty && rpAemptyFwft)
    , rpDataValidFwft = not rst && not emptyNext
    , rpExtraWords =
        if countRst then 0
        else if countUp then rpExtraWords + 1
        else if countDown then rpExtraWords - 1
        else rpExtraWords
    , rpDoutReg = if ramRdEn then memOut else rpDoutReg
    , rpEnbPipe = shiftIn ramRdEn rpEnbPipe
    , rpDoutPipe = pipeStep regoutEn
    }
   where
    cur = rpFwftState
    next = case cur of
      FwftInvalid -> if ramEmpty then FwftInvalid else FwftStage1
      FwftStage1 -> if ramEmpty then FwftStage2 else FwftBoth
      FwftStage2
        | ramEmpty && rdEn -> FwftInvalid
        | not ramEmpty && rdEn -> FwftStage1
        | not ramEmpty && not rdEn -> FwftBoth
        | otherwise -> FwftStage2
      FwftBoth -> if ramEmpty && rdEn then FwftStage2 else FwftBoth
    regoutEn = case cur of
      FwftStage1 -> True
      FwftBoth -> rdEn
      _ -> False
    goingEmpty = cur == FwftStage2 && rdEn
    leavingEmpty = cur == FwftStage1
    emptyNext = goingEmpty || (not leavingEmpty && rpEmptyFwft)
    goingAempty = cur == FwftBoth && rdEn && ramEmpty
    leavingAempty = case cur of
      FwftStage1 -> not ramEmpty
      FwftStage2 -> not (rdEn || ramEmpty)
      _ -> False
    countUp =
         (cur == FwftInvalid && next == FwftStage1)
      || (cur == FwftStage1 && next == FwftBoth)
      || (cur == FwftStage2 && next == FwftBoth)
    countDown =
         (cur == FwftBoth && next == FwftStage2)
      || (cur == FwftStage2 && next == FwftInvalid)
    countRst = rst || (cur == FwftInvalid && next == FwftInvalid)

-- | Unpacked version of 'XpmFifoConfig', with everything the models need
data ModelConfig = ModelConfig
  { mReadMode :: ReadMode
  , mFullResetValue :: Bool
  , mProgFullThresh :: Integer
  , mProgEmptyThresh :: Integer
  , mCdcSyncStages :: Int
  , mRelatedClocks :: Bool
  }

toModelConfig :: XpmFifoConfig depth -> ModelConfig
toModelConfig XpmFifoConfig{..} = ModelConfig
  { mReadMode = readMode
  , mFullResetValue = fullResetValue
  , mProgFullThresh = toInteger progFullThresh
  , mProgEmptyThresh = toInteger progEmptyThresh
  , mCdcSyncStages = fromIntegral cdcSyncStages
  , mRelatedClocks = relatedClocks
  }

-- | @PF_THRESH_ADJ@
progFullThreshAdj :: ModelConfig -> Integer
progFullThreshAdj ModelConfig{..} =
  if isFwft mReadMode then mProgFullThresh - 2 else mProgFullThresh

-- | @PE_THRESH_ADJ@
progEmptyThreshAdj :: ModelConfig -> Integer
progEmptyThreshAdj ModelConfig{..} =
  if isFwft mReadMode then mProgEmptyThresh - 2 else mProgEmptyThresh

-- | Value of a register without an initial value, before the first clock edge
noInit :: NFDataX a => String -> a
noInit nm = deepErrorX (nm <> ": no initial value")

-- | Memory contents at a given address. The memory of the XPM is initialized
-- with zeroes, but the FIFO never reads a location that was not written.
memRead :: (KnownNat n, NFDataX a) => IntMap a -> Unsigned n -> a
memRead mem addr =
  fromMaybe (deepErrorX "xpmFifo: read from uninitialized memory")
    (IntMap.lookup (fromIntegral addr) mem)

-- | Drop the extra bit of a pointer
ptr :: KnownNat n => Unsigned (n + 1) -> Unsigned n
ptr = resize

-- ** Common clock model

-- | State of the common clock model (@xpm_fifo_sync@)
data CcState n a = CcState
  { ccPowerOnRst :: (Bool, Bool)
  -- ^ @power_on_rst[1:0]@
  , ccRstCc :: (Bool, Bool, Bool)
  -- ^ @fifo_wr_rst_cc[2:0]@
  , ccRstD1 :: Bool
  , ccWrPntr :: Unsigned (n + 1)
  , ccRdPntr :: Unsigned (n + 1)
  , ccMem :: IntMap a
  , ccFull :: Bool
  , ccAfull :: Bool
  , ccEmpty :: Bool
  , ccAempty :: Bool
  , ccWrEnPfQ :: Bool
  , ccRdEnPfQ :: Bool
  , ccDiffPf :: Unsigned n
  , ccProgFull :: Bool
  , ccReadOnlyQ :: Bool
  , ccWriteOnlyQ :: Bool
  , ccDiffPe :: Unsigned n
  , ccProgEmpty :: Bool
  , ccOverflow :: Bool
  , ccUnderflow :: Bool
  , ccWrAck :: Bool
  , ccWrCount :: Unsigned (n + 1)
  , ccRdCount :: Unsigned (n + 1)
  , ccRead :: ReadPath a
  }

-- | Cycle accurate model of @xpm_fifo_sync@
xpmFifoSyncModel ::
  forall n dom a .
  (KnownNat n, NFDataX a, BitPack a) =>
  ModelConfig ->
  -- | @rst@
  Signal dom Bool ->
  -- | @wr_en@ and @din@
  Signal dom (Maybe a) ->
  -- | @rd_en@
  Signal dom Bool ->
  XpmFifoOut dom dom n a
xpmFifoSyncModel cfg@ModelConfig{..} rst wrData rdEn = XpmFifoOut
  { isFull = ccFull <$> states
  , isAlmostFull = ccAfull <$> states
  , isProgFull = (\s -> if mProgFullThresh > 0 then ccProgFull s else False) <$> states
  , writeCount = ccWrCount <$> states
  , isOverflow = ccOverflow <$> states
  , writeAck = ccWrAck <$> states
  , writeResetBusy = rstBusy <$> states
  , fifoData = readPathDout . ccRead <$> states
  , isEmpty = (\s -> readPathEmpty mReadMode (ccEmpty s) (ccRead s)) <$> states
  , isAlmostEmpty = (\s -> readPathAlmostEmpty mReadMode (ccAempty s) (ccRead s)) <$> states
  , isProgEmpty = (\s -> if mProgEmptyThresh > 0 then ccProgEmpty s else True) <$> states
  , readCount = ccRdCount <$> states
  , isUnderflow = ccUnderflow <$> states
  , isDataValid = readPathDataValid mReadMode . ccRead <$> states
  , readResetBusy = rstBusy <$> states
  }
 where
  states = mooreS force step s0 (bundle (rst, wrData, rdEn))

  rstVal = unpack 0
  frv = mFullResetValue
  pfAdj = progFullThreshAdj cfg
  peAdj = progEmptyThreshAdj cfg

  rstBusy CcState{ccRstCc=(b, _, _), ccRstD1} = b || ccRstD1

  s0 :: CcState n a
  s0 = CcState
    { ccPowerOnRst = (True, True)
    , ccRstCc = (False, False, False)
    , ccRstD1 = False
    , ccWrPntr = 0
    , ccRdPntr = 0
    , ccMem = IntMap.empty
    , ccFull = frv
    , ccAfull = frv
    , ccEmpty = True
    , ccAempty = True
    , ccWrEnPfQ = False
    , ccRdEnPfQ = False
    , ccDiffPf = 0
    , ccProgFull = frv
    , ccReadOnlyQ = False
    , ccWriteOnlyQ = False
    , ccDiffPe = 0
    , ccProgEmpty = True
    , ccOverflow = False
    , ccUnderflow = False
    , ccWrAck = False
    , ccWrCount = noInit "wr_data_count"
    , ccRdCount = noInit "rd_data_count"
    , ccRead = initReadPath mReadMode rstVal
    }

  force s@CcState{..} =
    whnfX ccPowerOnRst `seq` whnfX ccRstCc `seq` whnfX ccRstD1 `seq`
    whnfX ccWrPntr `seq` whnfX ccRdPntr `seq` ccMem `seq`
    whnfX ccFull `seq` whnfX ccAfull `seq` whnfX ccEmpty `seq`
    whnfX ccAempty `seq` whnfX ccWrEnPfQ `seq` whnfX ccRdEnPfQ `seq`
    whnfX ccDiffPf `seq` whnfX ccProgFull `seq` whnfX ccReadOnlyQ `seq`
    whnfX ccWriteOnlyQ `seq` whnfX ccDiffPe `seq` whnfX ccProgEmpty `seq`
    whnfX ccOverflow `seq` whnfX ccUnderflow `seq` whnfX ccWrAck `seq`
    whnfX ccWrCount `seq` whnfX ccRdCount `seq`
    s{ccRead = forceReadPath ccRead}

  step :: CcState n a -> (Bool, Maybe a, Bool) -> CcState n a
  step s@CcState{..} (rstIn, wrDat, rdEnIn) = s
    { ccPowerOnRst = (snd ccPowerOnRst, False)
    , ccRstCc = if rstI then (True, True, True) else (rst1, rst0, False)
    , ccRstD1 = wrstBusy
    , ccWrPntr = if wrstBusy then 0 else ccWrPntr + b2n ramWrEn
    , ccRdPntr = if rdRst then 0 else ccRdPntr + b2n ramRdEn
    , ccMem = if ramWrEn then IntMap.insert (fromIntegral wp) din ccMem else ccMem
    , ccFull =
        if wrstBusy then frv
        else if frv && clrFull then False
        else goingFull || (not leavingFull && ccFull)
    , ccAfull =
        if wrstBusy then frv
        else if rstIn then ccAfull
        else if clrFull then False
        else goingAfull || (not leavingAfull && ccAfull)
    , ccEmpty = rdRst || goingEmpty || (not leavingEmpty && ccEmpty)
    , ccAempty = rdRst || goingAempty || (not leavingAempty && ccAempty)
    , ccWrEnPfQ = not wrstBusy && ramWrEn
    , ccRdEnPfQ = not wrstBusy && ramRdEn
    , ccDiffPf = if wrstBusy then 0 else wp - rp + b2n (ramWrEn && not ramRdEn)
    , ccProgFull =
        if wrstBusy then frv
        else if clrFull then False
        else if pfAtThresh && ccWrEnPfQ && not ccRdEnPfQ then True
        else if pfAtThresh && not ccWrEnPfQ && ccRdEnPfQ then False
        else ccProgFull
    , ccReadOnlyQ = not rdRst && readOnly
    , ccWriteOnlyQ = not rdRst && writeOnly
    , ccDiffPe =
        if rdRst then 0 else if readOnly then wp - rp - 1 else wp - rp
    , ccProgEmpty =
        if rdRst then True
        else if peAtThresh && ccReadOnlyQ then True
        else if peAtThresh && ccWriteOnlyQ then False
        else ccProgEmpty
    , ccOverflow = (wrstBusy || ccRstD1 || ccFull) && wrEn
    , ccUnderflow = (rdRst || emptyI) && rdEnIn
    , ccWrAck = not (rstIn || wrstBusy) && ramWrEn
    , ccWrCount = if wrstBusy then 0 else ccWrPntr - (ccRdPntr - extraWords)
    , ccRdCount =
        if rdRst || readPathInvalid mReadMode ccRead then 0
        else ccWrPntr - ccRdPntr + extraWords
    , ccRead =
        readPathStep mReadMode rstVal rdRst rdEnIn ccEmpty ramRdEn
          (memRead ccMem rp) ccRead
    }
   where
    (rst2, rst1, rst0) = ccRstCc
    rstI = fst ccPowerOnRst || rstIn
    wrstBusy = rst2
    rdRst = rst2
    wrEn = isJust wrDat
    din = fromMaybe (deepErrorX "xpmFifoSync: no write data") wrDat
    clrFull = not wrstBusy && ccRstD1 && not rstIn
    ramWrEn = wrEn && not ccFull && not (wrstBusy || ccRstD1)
    ramRdEn = readPathRdEn mReadMode ccEmpty rdEnIn ccRead && not ccEmpty
    emptyI = readPathEmpty mReadMode ccEmpty ccRead
    extraWords = resize (rpExtraWords ccRead)

    wp = ptr @n ccWrPntr
    rp = ptr @n ccRdPntr

    goingEmpty = wp == rp + 1 && not ramWrEn && ramRdEn
    leavingEmpty = wp == rp && ramWrEn
    goingAempty = wp == rp + 2 && not ramWrEn && ramRdEn
    leavingAempty = wp == rp + 1 && ramWrEn && not ramRdEn
    goingFull = rp == wp + 1 && ramWrEn && not ramRdEn
    leavingFull = rp == wp && ramRdEn
    goingAfull = rp == wp + 2 && ramWrEn && not ramRdEn
    leavingAfull = rp == wp + 1 && ramRdEn && not ramWrEn

    writeAllow = ramWrEn && not ccFull
    readAllow = ramRdEn && not emptyI
    writeOnly = writeAllow && not readAllow
    readOnly = readAllow && not writeAllow

    pfAtThresh = toInteger ccDiffPf == pfAdj
    peAtThresh = toInteger ccDiffPe == peAdj

-- ** Independent clock model

-- | States of the write side reset state machine in @xpm_fifo_rst@
data WrstState = WrstIdle | WrstIn | WrstOut | WrstExit | WrstGo2Idle
  deriving (Show, Eq, Generic, NFDataX)

-- | States of the read side reset state machine in @xpm_fifo_rst@
data RrstState = RrstIdle | RrstIn | RrstOut | RrstExit
  deriving (Show, Eq, Generic, NFDataX)

-- | State of the write side of the independent clock model
data IcWrState n a = IcWrState
  { icwPowerOnRst :: (Bool, Bool)
  , icwRstState :: WrstState
  , icwRstSeqReentered :: Bool
  , icwFifoWrRstIc :: Bool
  , icwWrRstBusyIc :: Bool
  , icwRdRstSync :: [Bool]
  -- ^ @xpm_cdc_sync_rst@ synchronizing @fifo_rd_rst_ic@
  , icwRstD1 :: Bool
  , icwWrPntr :: Unsigned (n + 1)
  , icwWrPntrSrc :: Unsigned n
  -- ^ Source register of @xpm_cdc_gray@ synchronizing @wr_pntr@
  , icwWrPntrDcSrc :: Unsigned (n + 1)
  -- ^ Source register of @xpm_cdc_gray@ synchronizing @wr_pntr_ext@
  , icwRdPntrSync :: [Unsigned n]
  , icwRdPntrWr :: Unsigned n
  , icwRdPntrDcSync :: [Unsigned (n + 1)]
  , icwRdPntrWrDc :: Unsigned (n + 1)
  , icwMem :: IntMap a
  , icwFull :: Bool
  , icwAfull :: Bool
  , icwDiffPf :: Unsigned n
  , icwProgFull :: Bool
  , icwOverflow :: Bool
  , icwWrAck :: Bool
  , icwWrCount :: Unsigned (n + 1)
  }

-- | State of the read side of the independent clock model
data IcRdState n a = IcRdState
  { icrRstState :: RrstState
  , icrFifoRdRstIc :: Bool
  , icrWrRstSync :: [Bool]
  -- ^ @xpm_cdc_sync_rst@ synchronizing @fifo_wr_rst_ic@
  , icrRdPntr :: Unsigned (n + 1)
  , icrRdPntrSrc :: Unsigned n
  , icrRdPntrDcSrc :: Unsigned (n + 1)
  , icrWrPntrSync :: [Unsigned n]
  , icrWrPntrRd :: Unsigned n
  , icrWrPntrDcSync :: [Unsigned (n + 1)]
  , icrWrPntrRdDc :: Unsigned (n + 1)
  , icrEmpty :: Bool
  , icrAempty :: Bool
  , icrDiffPe :: Unsigned n
  , icrProgEmpty :: Bool
  , icrUnderflow :: Bool
  , icrRdCount :: Unsigned (n + 1)
  , icrRead :: ReadPath a
  }

-- | Write side registers observed by the read side
data WrToRd n a = WrToRd
  { w2rFifoWrRst :: Bool
  , w2rWrPntr :: Unsigned n
  , w2rWrPntrDc :: Unsigned (n + 1)
  , w2rMem :: IntMap a
  }

-- | Read side registers observed by the write side
data RdToWr n = RdToWr
  { r2wFifoRdRst :: Bool
  , r2wRdPntr :: Unsigned n
  , r2wRdPntrDc :: Unsigned (n + 1)
  }

-- | Cycle accurate model of @xpm_fifo_async@
xpmFifoAsyncModel ::
  forall n write read a .
  (KnownNat n, KnownDomain write, KnownDomain read, NFDataX a, BitPack a) =>
  ModelConfig ->
  Clock write ->
  Clock read ->
  -- | @rst@
  Signal write Bool ->
  -- | @wr_en@ and @din@
  Signal write (Maybe a) ->
  -- | @rd_en@
  Signal read Bool ->
  XpmFifoOut write read n a
xpmFifoAsyncModel cfg@ModelConfig{..} clkWr clkRd rst wrData rdEn = XpmFifoOut
  { isFull = icwFull <$> wrStates
  , isAlmostFull = icwAfull <$> wrStates
  , isProgFull = (\s -> if mProgFullThresh > 0 then icwProgFull s else False) <$> wrStates
  , writeCount = icwWrCount <$> wrStates
  , isOverflow = icwOverflow <$> wrStates
  , writeAck = icwWrAck <$> wrStates
  , writeResetBusy = (\s -> icwWrRstBusyIc s || icwRstD1 s) <$> wrStates
  , fifoData = readPathDout . icrRead <$> rdStates
  , isEmpty = (\s -> readPathEmpty mReadMode (icrEmpty s) (icrRead s)) <$> rdStates
  , isAlmostEmpty = (\s -> readPathAlmostEmpty mReadMode (icrAempty s) (icrRead s)) <$> rdStates
  , isProgEmpty = (\s -> if mProgEmptyThresh > 0 then icrProgEmpty s else True) <$> rdStates
  , readCount = icrRdCount <$> rdStates
  , isUnderflow = icrUnderflow <$> rdStates
  , isDataValid = readPathDataValid mReadMode . icrRead <$> rdStates
  , readResetBusy = icrFifoRdRstIc <$> rdStates
  }
 where
  wrStates = mooreS forceWr stepWr w0 (bundle (rst, wrData, rdToWr))
  rdStates = mooreS forceRd stepRd r0 (bundle (rdEn, wrToRd))

  wrToRd = unsafeSynchronizer clkWr clkRd (toRd <$> wrStates)
  rdToWr = unsafeSynchronizer clkRd clkWr (toWr <$> rdStates)

  -- With related clocks, the pointers are registered in the other domain
  -- directly. Otherwise, they cross through an @xpm_cdc_gray@, which has a
  -- source register.
  toRd IcWrState{..} = WrToRd
    { w2rFifoWrRst = icwFifoWrRstIc
    , w2rWrPntr = if mRelatedClocks then ptr icwWrPntr else icwWrPntrSrc
    , w2rWrPntrDc = if mRelatedClocks then icwWrPntr else icwWrPntrDcSrc
    , w2rMem = icwMem
    }

  toWr IcRdState{..} = RdToWr
    { r2wFifoRdRst = icrFifoRdRstIc
    , r2wRdPntr = if mRelatedClocks then ptr icrRdPntr else icrRdPntrSrc
    , r2wRdPntrDc =
        if mRelatedClocks then icrRdPntr - resize (rpExtraWords icrRead) else icrRdPntrDcSrc
    }

  rstVal = unpack 0
  frv = mFullResetValue
  pfAdj = progFullThreshAdj cfg
  peAdj = progEmptyThreshAdj cfg
  stages = mCdcSyncStages
  ptrStages = if mRelatedClocks then 0 else stages
  -- The read side uses two more stages for the write pointer used for the data
  -- count in FWFT mode.
  ptrDcStagesRd
    | mRelatedClocks = 0
    | isFwft mReadMode = stages + 2
    | otherwise = stages

  w0 :: IcWrState n a
  w0 = IcWrState
    { icwPowerOnRst = (True, True)
    , icwRstState = WrstIdle
    , icwRstSeqReentered = False
    , icwFifoWrRstIc = False
    , icwWrRstBusyIc = False
    , icwRdRstSync = P.replicate stages False
    , icwRstD1 = False
    , icwWrPntr = 0
    , icwWrPntrSrc = 0
    , icwWrPntrDcSrc = 0
    , icwRdPntrSync = P.replicate ptrStages 0
    , icwRdPntrWr = 0
    , icwRdPntrDcSync = P.replicate ptrStages 0
    , icwRdPntrWrDc = 0
    , icwMem = IntMap.empty
    , icwFull = frv
    , icwAfull = frv
    , icwDiffPf = 0
    , icwProgFull = frv
    , icwOverflow = False
    , icwWrAck = False
    , icwWrCount = noInit "wr_data_count"
    }

  r0 :: IcRdState n a
  r0 = IcRdState
    { icrRstState = RrstIdle
    , icrFifoRdRstIc = False
    , icrWrRstSync = P.replicate stages False
    , icrRdPntr = 0
    , icrRdPntrSrc = 0
    , icrRdPntrDcSrc = 0
    , icrWrPntrSync = P.replicate ptrStages 0
    , icrWrPntrRd = 0
    , icrWrPntrDcSync = P.replicate ptrDcStagesRd 0
    , icrWrPntrRdDc = 0
    , icrEmpty = True
    , icrAempty = True
    , icrDiffPe = 0
    , icrProgEmpty = True
    , icrUnderflow = False
    , icrRdCount = noInit "rd_data_count"
    , icrRead = initReadPath mReadMode rstVal
    }

  forceList :: [b] -> ()
  forceList = P.foldr (seq . whnfX) ()

  forceWr s@IcWrState{..} =
    whnfX icwPowerOnRst `seq` whnfX icwRstState `seq`
    whnfX icwRstSeqReentered `seq` whnfX icwFifoWrRstIc `seq`
    whnfX icwWrRstBusyIc `seq` forceList icwRdRstSync `seq` whnfX icwRstD1 `seq`
    whnfX icwWrPntr `seq` whnfX icwWrPntrSrc `seq` whnfX icwWrPntrDcSrc `seq`
    forceList icwRdPntrSync `seq` whnfX icwRdPntrWr `seq`
    forceList icwRdPntrDcSync `seq` whnfX icwRdPntrWrDc `seq` icwMem `seq`
    whnfX icwFull `seq` whnfX icwAfull `seq` whnfX icwDiffPf `seq`
    whnfX icwProgFull `seq` whnfX icwOverflow `seq` whnfX icwWrAck `seq`
    whnfX icwWrCount `seq` s

  forceRd s@IcRdState{..} =
    whnfX icrRstState `seq` whnfX icrFifoRdRstIc `seq` forceList icrWrRstSync `seq`
    whnfX icrRdPntr `seq` whnfX icrRdPntrSrc `seq` whnfX icrRdPntrDcSrc `seq`
    forceList icrWrPntrSync `seq` whnfX icrWrPntrRd `seq`
    forceList icrWrPntrDcSync `seq` whnfX icrWrPntrRdDc `seq`
    whnfX icrEmpty `seq` whnfX icrAempty `seq` whnfX icrDiffPe `seq`
    whnfX icrProgEmpty `seq` whnfX icrUnderflow `seq` whnfX icrRdCount `seq`
    s{icrRead = forceReadPath icrRead}

  stepWr :: IcWrState n a -> (Bool, Maybe a, RdToWr n) -> IcWrState n a
  stepWr s@IcWrState{..} (rstIn, wrDat, RdToWr{..}) = s
    { icwPowerOnRst = (snd icwPowerOnRst, False)
    , icwRstState = nextRstState
    , icwRstSeqReentered =
        not rstI && (icwRstState == WrstGo2Idle || icwRstSeqReentered)
    , icwFifoWrRstIc = case icwRstState of
        WrstIdle -> rstI
        WrstIn -> True
        WrstOut -> False
        WrstExit -> False
        WrstGo2Idle -> True
    , icwWrRstBusyIc = case icwRstState of
        WrstIdle -> rstI
        WrstIn -> True
        WrstOut -> True
        WrstExit -> True
        WrstGo2Idle -> icwWrRstBusyIc
    , icwRdRstSync = shiftIn r2wFifoRdRst icwRdRstSync
    , icwRstD1 = wrstBusy
    , icwWrPntr = if wrstBusy then 0 else icwWrPntr + b2n ramWrEn
    , icwWrPntrSrc = wp
    , icwWrPntrDcSrc = icwWrPntr
    , icwRdPntrSync = shiftIn r2wRdPntr icwRdPntrSync
    , icwRdPntrWr = if wrstBusy then 0 else lastOr r2wRdPntr icwRdPntrSync
    , icwRdPntrDcSync = shiftIn r2wRdPntrDc icwRdPntrDcSync
    , icwRdPntrWrDc = if wrstBusy then 0 else lastOr r2wRdPntrDc icwRdPntrDcSync
    , icwMem = if ramWrEn then IntMap.insert (fromIntegral wp) din icwMem else icwMem
    , icwFull =
        if wrstBusy then frv
        else if frv && clrFull then False
        else goingFull || leavingFull
    , icwAfull =
        if wrstBusy then frv
        else if rstIn then icwAfull
        else if clrFull then False
        else if not icwFull then goingAfull || leavingAfull
        else icwAfull
    , icwDiffPf = if wrstBusy then 0 else wp - icwRdPntrWr + b2n ramWrEn
    , icwProgFull =
        if wrstBusy then frv
        else if clrFull then False
        else if not icwFull then toInteger icwDiffPf >= pfAdj
        else icwProgFull
    , icwOverflow = (wrstBusy || icwRstD1 || icwFull) && wrEn
    , icwWrAck = not (rstIn || wrRstI || wrstBusy) && ramWrEn
    , icwWrCount = if wrstBusy then 0 else icwWrPntr - icwRdPntrWrDc
    }
   where
    rstI = fst icwPowerOnRst || rstIn
    fifoRdRstWr = lastOr False icwRdRstSync
    nextRstState = case icwRstState of
      WrstIdle -> if rstI then WrstIn else WrstIdle
      WrstIn
        | rstI -> WrstIn
        | fifoRdRstWr -> WrstOut
        | otherwise -> WrstIn
      WrstOut
        | rstI -> WrstIn
        | not fifoRdRstWr -> WrstExit
        | otherwise -> WrstOut
      WrstExit
        | rstI -> WrstIn
        | not rstIn && not icwRstSeqReentered -> WrstGo2Idle
        | icwRstSeqReentered -> WrstIdle
        | otherwise -> WrstExit
      WrstGo2Idle -> WrstIn

    wrstBusy = icwWrRstBusyIc
    wrRstI = icwFifoWrRstIc || icwWrRstBusyIc
    wrEn = isJust wrDat
    din = fromMaybe (deepErrorX "xpmFifoAsync: no write data") wrDat
    clrFull = not wrstBusy && icwRstD1 && not rstIn
    ramWrEn = wrEn && not icwFull && not (wrstBusy || icwRstD1)

    wp = ptr @n icwWrPntr
    goingFull = icwRdPntrWr == wp + 2 && ramWrEn
    leavingFull = icwRdPntrWr == wp + 1
    goingAfull = icwRdPntrWr == wp + 3 && ramWrEn
    leavingAfull = icwRdPntrWr == wp + 2

  stepRd :: IcRdState n a -> (Bool, WrToRd n a) -> IcRdState n a
  stepRd s@IcRdState{..} (rdEnIn, WrToRd{..}) = s
    { icrRstState = case icrRstState of
        RrstIdle -> if fifoWrRstRd then RrstIn else RrstIdle
        RrstIn -> RrstOut
        RrstOut -> if fifoWrRstRd then RrstOut else RrstExit
        RrstExit -> RrstIdle
    , icrFifoRdRstIc = case icrRstState of
        RrstIdle -> fifoWrRstRd
        RrstIn -> True
        RrstOut -> True
        RrstExit -> False
    , icrWrRstSync = shiftIn w2rFifoWrRst icrWrRstSync
    , icrRdPntr = if rdRst then 0 else icrRdPntr + b2n ramRdEn
    , icrRdPntrSrc = rp
    , icrRdPntrDcSrc = icrRdPntr - extraWords
    , icrWrPntrSync = shiftIn w2rWrPntr icrWrPntrSync
    , icrWrPntrRd = if rdRst then 0 else lastOr w2rWrPntr icrWrPntrSync
    , icrWrPntrDcSync = shiftIn w2rWrPntrDc icrWrPntrDcSync
    , icrWrPntrRdDc = if rdRst then 0 else lastOr w2rWrPntrDc icrWrPntrDcSync
    , icrEmpty = rdRst || goingEmpty || leavingEmpty
    , icrAempty =
        if rdRst then True
        else if not icrEmpty then goingAempty || leavingAempty
        else icrAempty
    , icrDiffPe =
        if rdRst then 0
        else if ramRdEn then icrWrPntrRd - rp - 1
        else icrWrPntrRd - rp
    , icrProgEmpty =
        if rdRst then True
        else if not emptyI then toInteger icrDiffPe <= peAdj
        else icrProgEmpty
    , icrUnderflow = (rdRst || emptyI) && rdEnIn
    , icrRdCount =
        if rdRst || readPathInvalid mReadMode icrRead then 0
        else icrWrPntrRdDc - icrRdPntr + extraWords
    , icrRead =
        readPathStep mReadMode rstVal rdRst rdEnIn icrEmpty ramRdEn
          (memRead w2rMem rp) icrRead
    }
   where
    fifoWrRstRd = lastOr False icrWrRstSync
    rdRst = icrFifoRdRstIc
    ramRdEn = readPathRdEn mReadMode icrEmpty rdEnIn icrRead && not icrEmpty
    emptyI = readPathEmpty mReadMode icrEmpty icrRead
    extraWords = resize (rpExtraWords icrRead)

    rp = ptr @n icrRdPntr
    goingEmpty = icrWrPntrRd == rp + 1 && ramRdEn
    leavingEmpty = icrWrPntrRd == rp
    goingAempty = icrWrPntrRd == rp + 2 && ramRdEn
    leavingAempty = icrWrPntrRd == rp + 1

-- | 'Bool' to a number
b2n :: Num b => Bool -> b
b2n b = if b then 1 else 0
