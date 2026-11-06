{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

FIFOs of Intel FPGAs: the single-clock FIFO @scfifo@ and the dual-clock FIFO
@dcfifo@. For more information
see the /FIFO Intel FPGA IP User Guide/
(<https://www.intel.com/content/www/us/en/docs/programmable/683522/current/fifo-intel-fpga-ip-user-guide.html>).

The FIFOs are mapped to block RAM (@use_eab = \"ON\"@). Different read and write
widths, ECC and the @allow_rwcycle_when_full@ option are not supported.
-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Intel.Fifo
  ( -- * Single-clock FIFO
    scfifo
  , ScFifoConfig (..)
  , defScFifoConfig
  , ScFifoOut (..)
    -- * Dual-clock FIFO
  , dcfifo
  , DcFifoConfig (..)
  , defDcFifoConfig
  , DcFifoOut (..)
    -- * Configuration
  , ReadMode (..)
  ) where

import Clash.Explicit.Prelude

import Clash.Signal.Internal (ClockAB (..), Signal ((:-)), clockTicks)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Prelude as P

import Clash.Cores.Intel.Primitive.Internal (alteraMf)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( ClockPort (..), Param (..), Port (..), ResetPort (..), inst, unPort )

-- | How the FIFO presents its data (@lpm_showahead@)
data ReadMode
  = -- | Normal mode (@lpm_showahead = \"OFF\"@): a read request puts the word
    -- at the head of the FIFO on the output in the next cycle.
    Normal
  | -- | Show-ahead mode (@lpm_showahead = \"ON\"@): the word at the head of the
    -- FIFO is on the output while the FIFO isn't empty, and a read request
    -- removes it. This is also known as first word fall through.
    ShowAhead
  deriving (Show, Eq, Generic, NFDataX)

-- | Configuration of 'scfifo'
data ScFifoConfig = ScFifoConfig
  { readMode :: ReadMode
  -- ^ Read mode (@lpm_showahead@)
  , outputRegister :: Bool
  -- ^ Register the output of the RAM (@add_ram_output_register@), which
  -- Intel calls optimizing for speed rather than area. This delays the first
  -- word after the FIFO was empty by a cycle, see 'scfifo'.
  , overflowChecking :: Bool
  -- ^ Ignore write requests while the FIFO is full (@overflow_checking@). If
  -- this is disabled, you must not write to a full FIFO: the simulation model
  -- becomes undefined until the next reset if you do.
  , underflowChecking :: Bool
  -- ^ Ignore read requests while the FIFO is empty (@underflow_checking@). If
  -- this is disabled, you must not read from an empty FIFO: the simulation
  -- model becomes undefined until the next reset if you do.
  , almostFullValue :: Natural
  -- ^ 'isAlmostFull' is asserted while the FIFO holds at least this many
  -- words (@almost_full_value@). With 0, it is always asserted.
  , almostEmptyValue :: Natural
  -- ^ 'isAlmostEmpty' is asserted while the FIFO holds fewer than this many
  -- words (@almost_empty_value@). With 0, it is never asserted.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'scfifo': normal read mode, no output register,
-- overflow and underflow checking, and 'isAlmostFull' and 'isAlmostEmpty'
-- disabled (always and never asserted).
defScFifoConfig :: ScFifoConfig
defScFifoConfig =
  ScFifoConfig
    { readMode = Normal
    , outputRegister = False
    , overflowChecking = True
    , underflowChecking = True
    , almostFullValue = 0
    , almostEmptyValue = 0
    }

-- | Outputs of 'scfifo'
data ScFifoOut dom n a = ScFifoOut
  { fifoData :: Signal dom a
  -- ^ Read data (@q@)
  , isFull :: Signal dom Bool
  -- ^ The FIFO holds @n@ words (@full@)
  , isEmpty :: Signal dom Bool
  -- ^ There is no word to read (@empty@). This can be asserted while
  -- 'usedWords' is not zero, see 'scfifo'.
  , isAlmostFull :: Signal dom Bool
  -- ^ The FIFO holds at least 'almostFullValue' words (@almost_full@)
  , isAlmostEmpty :: Signal dom Bool
  -- ^ The FIFO holds fewer than 'almostEmptyValue' words (@almost_empty@)
  , usedWords :: Signal dom (Unsigned (CLog 2 n))
  -- ^ Number of words in the FIFO (@usedw@). It wraps to 0 when a FIFO of
  -- which @n@ is a power of two is full.
  }

{- | Single-clock FIFO (@scfifo@) holding @n@ words of type @a@, mapped to
block RAM.

__Writing__: a write request (@'Just' a@) is accepted when the FIFO isn't
'isFull' (or always, if 'overflowChecking' is disabled). A read request in the
same cycle doesn't make room for it.

__Reading__: a read request ('True') is accepted when the FIFO isn't 'isEmpty'
(or always, if 'underflowChecking' is disabled). In 'Normal' mode, the word
appears on 'fifoData' in the cycle after the request, and stays there until the
next accepted read. In 'ShowAhead' mode, 'fifoData' shows the word at the head
of the FIFO while it isn't 'isEmpty', and is undefined while it is.

__Latency__: 'usedWords', 'isFull', 'isAlmostFull' and 'isAlmostEmpty' follow
the accepted requests after one cycle. A written word becomes available to
read, i.e., deasserts 'isEmpty' and appears on 'fifoData' in show-ahead mode,
after a number of cycles that depends on the configuration:

+------------+----------------+-------------------+
| Read mode  | Output register | Latency (cycles) |
+============+================+===================+
| 'Normal'   | no             | 1                 |
+------------+----------------+-------------------+
| 'Normal'   | yes            | 2                 |
+------------+----------------+-------------------+
| 'ShowAhead'| no             | 2                 |
+------------+----------------+-------------------+
| 'ShowAhead'| yes            | 3                 |
+------------+----------------+-------------------+

So 'isEmpty' can be asserted while 'usedWords' counts words that aren't
available yet. A read that takes the last available word asserts 'isEmpty' in
the next cycle.

__Almost full and almost empty__: like in the hardware, 'isAlmostFull' and
'isAlmostEmpty' are state machines that look at 'usedWords' and the /requests/,
whether they're accepted or not. They follow the definitions of
'almostFullValue' and 'almostEmptyValue' as long as you don't request writes
to a full FIFO or reads from an empty one. For example, with an output
register, a read request while the FIFO holds one word that isn't available
yet deasserts 'isAlmostFull' for an 'almostFullValue' of 1, although the read
isn't accepted and 'usedWords' stays 1.

__Reset__: the reset empties the FIFO. It is asynchronous (@aclr@) or
synchronous (@sclr@), depending on the domain. 'fifoData' is undefined after a
reset, or 0 with 'outputRegister'.

The simulation model matches Intel's simulation model (@altera_mf@) cycle by
cycle, for all device families Quartus Prime Lite supports.
-}
scfifo ::
  forall n a dom deviceFamily.
  ( HasCallStack
  , KnownDomain dom
  , KnownNat n
  , 2 <= n
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  ScFifoConfig ->
  -- | Number of words (@lpm_numwords@)
  SNat n ->
  -- | @clock@
  Clock dom ->
  -- | @aclr@ or @sclr@, depending on the domain
  Reset dom ->
  -- | Write request: @wrreq@ and @data@
  Signal dom (Maybe a) ->
  -- | Read request: @rdreq@
  Signal dom Bool ->
  ScFifoOut dom n a
scfifo _devFam cfg@ScFifoConfig{..} SNat clk rst wr rd
  | clashSimulation = scfifoModel @n cfg rst wr rd
  | otherwise =
      ScFifoOut
        { fifoData = unpack <$> unPort q
        , isFull = bitToBool <$> unPort full
        , isEmpty = bitToBool <$> unPort empty_
        , isAlmostFull = bitToBool <$> unPort almostFull
        , isAlmostEmpty = bitToBool <$> unPort almostEmpty
        , usedWords = unpack <$> unPort usedw
        }
 where
  onOff b = if b then "ON" else "OFF"
  wrreq = boolToBit . isJust <$> wr
  dat = pack . fromJustX <$> wr
  rdreq = boolToBit <$> rd

  (q, full, empty_, almostFull, almostEmpty, usedw) = ports
  ports ::
    ( Port "q" dom (BitVector (BitSize a))
    , Port "full" dom Bit
    , Port "empty" dom Bit
    , Port "almost_full" dom Bit
    , Port "almost_empty" dom Bit
    , Port "usedw" dom (BitVector (CLog 2 n))
    )
  ports = case resetKind @dom of
    SAsynchronous ->
      inst
        (alteraMf "scfifo")
        (Param @"lpm_width" @Integer (natToNum @(BitSize a)))
        (Param @"lpm_widthu" @Integer (natToNum @(CLog 2 n)))
        (Param @"lpm_numwords" @Integer (natToNum @n))
        (Param @"lpm_showahead" @String (if readMode == ShowAhead then "ON" else "OFF"))
        (Param @"lpm_type" @String "scfifo")
        (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
        (Param @"underflow_checking" @String (onOff underflowChecking))
        (Param @"overflow_checking" @String (onOff overflowChecking))
        (Param @"allow_rwcycle_when_full" @String "OFF")
        (Param @"use_eab" @String "ON")
        (Param @"add_ram_output_register" @String (onOff outputRegister))
        (Param @"almost_full_value" @Integer (toInteger almostFullValue))
        (Param @"almost_empty_value" @Integer (toInteger almostEmptyValue))
        (ClockPort @"clock" clk)
        (ResetPort @"aclr" @'ActiveHigh rst)
        (Port @"sclr" @dom (pure (0 :: Bit)))
        (Port @"data" dat)
        (Port @"wrreq" wrreq)
        (Port @"rdreq" rdreq)
    SSynchronous ->
      inst
        (alteraMf "scfifo")
        (Param @"lpm_width" @Integer (natToNum @(BitSize a)))
        (Param @"lpm_widthu" @Integer (natToNum @(CLog 2 n)))
        (Param @"lpm_numwords" @Integer (natToNum @n))
        (Param @"lpm_showahead" @String (if readMode == ShowAhead then "ON" else "OFF"))
        (Param @"lpm_type" @String "scfifo")
        (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
        (Param @"underflow_checking" @String (onOff underflowChecking))
        (Param @"overflow_checking" @String (onOff overflowChecking))
        (Param @"allow_rwcycle_when_full" @String "OFF")
        (Param @"use_eab" @String "ON")
        (Param @"add_ram_output_register" @String (onOff outputRegister))
        (Param @"almost_full_value" @Integer (toInteger almostFullValue))
        (Param @"almost_empty_value" @Integer (toInteger almostEmptyValue))
        (ClockPort @"clock" clk)
        (Port @"aclr" @dom (pure (0 :: Bit)))
        (ResetPort @"sclr" @'ActiveHigh rst)
        (Port @"data" dat)
        (Port @"wrreq" wrreq)
        (Port @"rdreq" rdreq)

-- | State of the simulation model of 'scfifo'
data ScFifoState a = ScFifoState
  { sWords :: Seq.Seq (a, Int)
  -- ^ The words in the FIFO, oldest first, along with the number of cycles
  -- until they are available to read
  , sQ :: a
  -- ^ Output register in normal mode
  , sAlmostFull :: Bool
  , sAlmostEmpty :: Bool
  , sBroken :: Bool
  -- ^ An overflow or underflow happened without checking
  }

{- | Simulation model of 'scfifo'. A written word becomes available to read
after a fixed latency; the FIFO is empty when the word at its head isn't
available. The almost full and almost empty flags are state machines, see
'scfifo'.
-}
scfifoModel ::
  forall n a dom.
  (KnownDomain dom, KnownNat n, 1 <= n, BitPack a, NFDataX a) =>
  ScFifoConfig ->
  Reset dom ->
  Signal dom (Maybe a) ->
  Signal dom Bool ->
  ScFifoOut dom n a
scfifoModel ScFifoConfig{..} rst wr rd =
  ScFifoOut
    { fifoData = (\(q, _, _, _, _, _) -> q) <$> outs
    , isFull = (\(_, f, _, _, _, _) -> f) <$> outs
    , isEmpty = (\(_, _, e, _, _, _) -> e) <$> outs
    , isAlmostFull = (\(_, _, _, af, _, _) -> af) <$> outs
    , isAlmostEmpty = (\(_, _, _, _, ae, _) -> ae) <$> outs
    , usedWords = (\(_, _, _, _, _, u) -> u) <$> outs
    }
 where
  numWords = natToNum @n :: Int
  afv = toInteger almostFullValue
  aev = toInteger almostEmptyValue
  isAsync = case resetKind @dom of
    SAsynchronous -> True
    SSynchronous -> False

  latency :: Int
  latency = case (readMode, outputRegister) of
    (Normal, False) -> 1
    (Normal, True) -> 2
    (ShowAhead, False) -> 2
    (ShowAhead, True) -> 3

  qReset :: a
  qReset
    | outputRegister = unpack 0
    | otherwise = deepErrorX "scfifo: no data read since reset"

  resetState =
    ScFifoState
      { sWords = Seq.empty
      , sQ = qReset
      , sAlmostFull = False
      , sAlmostEmpty = True
      , sBroken = False
      }

  available :: Seq.Seq (a, Int) -> Bool
  available ws = case Seq.viewl ws of
    (_, 0) Seq.:< _ -> True
    _ -> False

  -- The value of @usedw@: the number of words, wrapped to its width
  usedw :: Seq.Seq (a, Int) -> Unsigned (CLog 2 n)
  usedw = fromIntegral . Seq.length

  outputs :: ScFifoState a -> (a, Bool, Bool, Bool, Bool, Unsigned (CLog 2 n))
  outputs ScFifoState{..}
    | sBroken =
        ( deepErrorX "scfifo: overflow or underflow"
        , errorX "scfifo: overflow or underflow"
        , errorX "scfifo: overflow or underflow"
        , errorX "scfifo: overflow or underflow"
        , errorX "scfifo: overflow or underflow"
        , errorX "scfifo: overflow or underflow" )
    | otherwise =
        ( q
        , full
        , not (available sWords)
        , if
            | afv == 0 -> True
            | afv == toInteger numWords -> full
            | afv > toInteger numWords -> False
            | otherwise -> sAlmostFull
        , if
            | aev == 0 -> False
            | aev == toInteger numWords -> not full
            | aev > toInteger numWords -> True
            | otherwise -> sAlmostEmpty
        , usedw sWords )
   where
    full = Seq.length sWords == numWords
    q = case readMode of
      Normal -> sQ
      ShowAhead
        | (a, 0) Seq.:< _ <- Seq.viewl sWords -> a
        | otherwise -> deepErrorX "scfifo: empty"

  step :: ScFifoState a -> Maybe a -> Bool -> ScFifoState a
  step s@ScFifoState{..} w r
    | sBroken = s
    | overflow || underflow = s{sBroken = True}
    | otherwise =
        ScFifoState
          { sWords = words2
          , sQ = q'
          , sAlmostFull = almostFull'
          , sAlmostEmpty = almostEmpty'
          , sBroken = False
          }
   where
    full = Seq.length sWords == numWords
    empty_ = not (available sWords)
    wrreq = isJust w
    overflow = wrreq && full && not overflowChecking
    underflow = r && empty_ && not underflowChecking
    validWrite = wrreq && not full
    validRead = r && not empty_

    -- Words that were in the FIFO, after the read and one more cycle of
    -- latency
    words1 = fmap (\(a, l) -> (a, P.max 0 (l - 1))) (if validRead then Seq.drop 1 sWords else sWords)
    words2 = case w of
      Just a | validWrite -> words1 Seq.|> (a, latency - 1)
      _ -> words1

    q'
      | validRead, (a, _) Seq.:< _ <- Seq.viewl sWords = a
      | otherwise = sQ

    -- The almost full and almost empty flags are state machines driven by
    -- the requests, whether they are accepted or not
    u = toInteger (usedw sWords)
    almostFull'
      | sAlmostFull = not (u == afv && not wrreq && r)
      | afv == 1 = u == 0 && wrreq
      | otherwise = u == afv - 1 && wrreq && not r
    almostEmpty'
      | sAlmostEmpty = not (if aev == 1 then u == 0 && wrreq else u == aev - 1 && wrreq && not r)
      | otherwise = u == aev && not wrreq && r

  outs = go resetState (unsafeToActiveHigh rst) wr rd
  go s ~(r :- rs) ~(w :- ws) ~(rq :- rqs) =
    out :- (r `seq` go s' rs ws rqs)
   where
    s0 = if isAsync && r then resetState else s
    out = outputs s0
    s'
      | r = resetState
      | otherwise = step s0 w rq

-- | Configuration of 'dcfifo'
data DcFifoConfig = DcFifoConfig
  { dcReadMode :: ReadMode
  -- ^ Read mode (@lpm_showahead@)
  , dcOverflowChecking :: Bool
  -- ^ Ignore write requests while 'isWriteFull' (@overflow_checking@). If
  -- this is disabled, you must not write to a full FIFO: the simulation model
  -- becomes undefined until the next reset if you do.
  , dcUnderflowChecking :: Bool
  -- ^ Ignore read requests while 'isReadEmpty' (@underflow_checking@). If
  -- this is disabled, you must not read from an empty FIFO: the simulation
  -- model becomes undefined until the next reset if you do.
  , dcReadSyncStages :: Natural
  -- ^ Number of synchronizer registers, at least 1, in the read domain for
  -- the write pointer (@rdsync_delaypipe@ minus 2). Use at least 2 for clocks
  -- that aren't related.
  , dcWriteSyncStages :: Natural
  -- ^ Number of synchronizer registers, at least 1, in the write domain for
  -- the read pointer (@wrsync_delaypipe@ minus 2). Use at least 2 for clocks
  -- that aren't related.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of 'dcfifo': normal read mode, overflow and
-- underflow checking, and two synchronizer registers in both directions,
-- like Intel's default for unrelated clocks.
defDcFifoConfig :: DcFifoConfig
defDcFifoConfig =
  DcFifoConfig
    { dcReadMode = Normal
    , dcOverflowChecking = True
    , dcUnderflowChecking = True
    , dcReadSyncStages = 2
    , dcWriteSyncStages = 2
    }

-- | Outputs of 'dcfifo'
data DcFifoOut write read depth a = DcFifoOut
  { readData :: Signal read a
  -- ^ Read data (@q@)
  , isReadEmpty :: Signal read Bool
  -- ^ There is no word to read (@rdempty@)
  , isReadFull :: Signal read Bool
  -- ^ The FIFO is full, as seen from the read domain (@rdfull@)
  , readUsedWords :: Signal read (Unsigned depth)
  -- ^ Number of words in the FIFO, as seen from the read domain (@rdusedw@).
  -- It wraps to 0 when the FIFO is full.
  , isWriteFull :: Signal write Bool
  -- ^ The FIFO is full (@wrfull@)
  , isWriteEmpty :: Signal write Bool
  -- ^ The FIFO is empty, as seen from the write domain (@wrempty@)
  , writeUsedWords :: Signal write (Unsigned depth)
  -- ^ Number of words in the FIFO, as seen from the write domain
  -- (@wrusedw@). It wraps to 0 when the FIFO is full.
  }

{- | Dual-clock FIFO (@dcfifo@) holding @2^depth@ words of type @a@, mapped to
block RAM.

The write side keeps a write pointer, the read side a read pointer, and each
side sees the pointer of the other side through a synchronizer. The flags and
counts of a side are derived from its own pointer and the synchronized pointer
of the other side, so they are pessimistic: the write side sees reads late,
and the read side sees writes late.

__Writing__: a write request (@'Just' a@) is accepted when the FIFO isn't
'isWriteFull' (or always, if 'dcOverflowChecking' is disabled).
'isWriteFull' and 'isWriteEmpty' follow accepted writes in the next write
cycle, 'writeUsedWords' a cycle later.

__Reading__: a read request ('True') is accepted when the FIFO isn't
'isReadEmpty' (or always, if 'dcUnderflowChecking' is disabled). In 'Normal'
mode, the word appears on 'readData' in the read cycle after the request, and
stays there until the next accepted read. In 'ShowAhead' mode, 'readData'
shows the word at the head of the FIFO while it isn't 'isReadEmpty', and is
undefined while it is. 'isReadEmpty' and 'isReadFull' follow accepted reads in
the next read cycle, 'readUsedWords' a cycle later.

__Crossing__: a write reaches the read side one write cycle plus
'dcReadSyncStages' read cycles later; a read reaches the write side after
'dcWriteSyncStages' write cycles.

__Reset__: the reset (@aclr@) is asynchronous, and empties the FIFO on both
sides at once. Although it is a reset of the write domain, it also applies to
the read side. Intel recommends asserting it for at least a few cycles of both
clocks. In 'Normal' mode, 'readData' resets to 0.

Not supported: @add_usedw_msb_bit@, @read_aclr_synch@, @write_aclr_synch@,
@delay_rdusedw@ and @delay_wrusedw@ (fixed to 1).

The simulation model matches Intel's simulation model (@altera_mf@) cycle by
cycle, for all device families Quartus Prime Lite supports.
-}
dcfifo ::
  forall depth a write read deviceFamily.
  ( HasCallStack
  , KnownDomain write
  , KnownDomain read
  , HasAsynchronousReset write
  , KnownNat depth
  , 2 <= depth
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  DcFifoConfig ->
  -- | Two-logarithm of the number of words (@lpm_widthu@)
  SNat depth ->
  -- | Write clock (@wrclk@)
  Clock write ->
  -- | Asynchronous reset (@aclr@)
  Reset write ->
  -- | Read clock (@rdclk@)
  Clock read ->
  -- | Write request: @wrreq@ and @data@
  Signal write (Maybe a) ->
  -- | Read request: @rdreq@
  Signal read Bool ->
  DcFifoOut write read depth a
dcfifo _devFam cfg@DcFifoConfig{..} SNat wClk rst rClk wr rd
  | clashSimulation = dcfifoModel @depth cfg wClk rst rClk wr rd
  | otherwise =
      DcFifoOut
        { readData = unpack <$> unPort q
        , isReadEmpty = bitToBool <$> unPort rdempty
        , isReadFull = bitToBool <$> unPort rdfull
        , readUsedWords = unpack <$> unPort rdusedw
        , isWriteFull = bitToBool <$> unPort wrfull
        , isWriteEmpty = bitToBool <$> unPort wrempty
        , writeUsedWords = unpack <$> unPort wrusedw
        }
 where
  onOff b = if b then "ON" else "OFF"

  (q, rdempty, rdfull, rdusedw, wrfull, wrempty, wrusedw) = ports
  ports ::
    ( Port "q" read (BitVector (BitSize a))
    , Port "rdempty" read Bit
    , Port "rdfull" read Bit
    , Port "rdusedw" read (BitVector depth)
    , Port "wrfull" write Bit
    , Port "wrempty" write Bit
    , Port "wrusedw" write (BitVector depth)
    )
  ports =
    inst
      (alteraMf "dcfifo")
      (Param @"lpm_width" @Integer (natToNum @(BitSize a)))
      (Param @"lpm_widthu" @Integer (natToNum @depth))
      (Param @"lpm_numwords" @Integer (natToNum @(2 ^ depth)))
      (Param @"lpm_showahead" @String (if dcReadMode == ShowAhead then "ON" else "OFF"))
      (Param @"lpm_type" @String "dcfifo")
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"underflow_checking" @String (onOff dcUnderflowChecking))
      (Param @"overflow_checking" @String (onOff dcOverflowChecking))
      (Param @"rdsync_delaypipe" @Integer (toInteger dcReadSyncStages + 2))
      (Param @"wrsync_delaypipe" @Integer (toInteger dcWriteSyncStages + 2))
      (Param @"clocks_are_synchronized" @String "FALSE")
      (Param @"use_eab" @String "ON")
      (Param @"add_usedw_msb_bit" @String "OFF")
      (Param @"read_aclr_synch" @String "OFF")
      (Param @"write_aclr_synch" @String "OFF")
      (Param @"delay_rdusedw" @Integer 1)
      (Param @"delay_wrusedw" @Integer 1)
      (ClockPort @"wrclk" wClk)
      (ClockPort @"rdclk" rClk)
      (ResetPort @"aclr" @'ActiveHigh rst)
      (Port @"data" (pack . fromJustX <$> wr))
      (Port @"wrreq" (boolToBit . isJust <$> wr))
      (Port @"rdreq" (boolToBit <$> rd))

-- | State of the simulation model of 'dcfifo'
data DcFifoState a = DcFifoState
  { dMem :: IntMap.IntMap a
  -- ^ The RAM, indexed by pointer. It keeps its contents on a reset.
  , dWrPtr :: Int
  , dDelayedWrPtr :: Int
  -- ^ The write pointer, delayed by a write cycle. This is what the read side
  -- synchronizes.
  , dWrSync :: [Int]
  -- ^ Synchronizer for the read pointer in the write domain, first register
  -- first
  , dWrUsed :: Int
  -- ^ Register for @wrusedw@
  , dRdPtr :: Int
  , dRdSync :: [Int]
  -- ^ Synchronizer for the (delayed) write pointer in the read domain, first
  -- register first
  , dRdUsed :: Int
  -- ^ Register for @rdusedw@
  , dQ :: a
  -- ^ Output register in normal mode
  , dBroken :: Bool
  -- ^ An overflow or underflow happened without checking
  }

{- | Simulation model of 'dcfifo'. It walks through the active edges of both
clocks in the order in which they happen. Edges that coincide see each other's
state from before the edges, like registers do.
-}
dcfifoModel ::
  forall depth a write read.
  ( KnownDomain write
  , KnownDomain read
  , KnownNat depth
  , BitPack a
  , NFDataX a
  ) =>
  DcFifoConfig ->
  Clock write ->
  Reset write ->
  Clock read ->
  Signal write (Maybe a) ->
  Signal read Bool ->
  DcFifoOut write read depth a
dcfifoModel DcFifoConfig{..} wClk rst rClk wr rd =
  DcFifoOut
    { readData = (\(q, _, _, _) -> q) <$> rOuts
    , isReadEmpty = (\(_, e, _, _) -> e) <$> rOuts
    , isReadFull = (\(_, _, f, _) -> f) <$> rOuts
    , readUsedWords = (\(_, _, _, u) -> u) <$> rOuts
    , isWriteFull = (\(f, _, _) -> f) <$> wOuts
    , isWriteEmpty = (\(_, e, _) -> e) <$> wOuts
    , writeUsedWords = (\(_, _, u) -> u) <$> wOuts
    }
 where
  -- The pointers count modulo the number of words plus one, so that a full
  -- FIFO can be told apart from an empty one
  cnt = natToNum @(2 ^ depth) + 1 :: Int
  rdStages = P.max 1 (fromIntegral dcReadSyncStages) :: Int
  wrStages = P.max 1 (fromIntegral dcWriteSyncStages) :: Int
  inc p = (p + 1) `mod` cnt

  resetState :: IntMap.IntMap a -> DcFifoState a
  resetState mem =
    DcFifoState
      { dMem = mem
      , dWrPtr = 0
      , dDelayedWrPtr = 0
      , dWrSync = P.replicate wrStages 0
      , dWrUsed = 0
      , dRdPtr = 0
      , dRdSync = P.replicate rdStages 0
      , dRdUsed = 0
      , dQ = unpack 0
      , dBroken = False
      }

  -- Before the first write, the RAM holds zeroes in Intel's simulation model.
  -- The model leaves it undefined.
  readMem :: IntMap.IntMap a -> Int -> a
  readMem mem p = IntMap.findWithDefault (deepErrorX "dcfifo: uninitialized word") p mem

  broken :: String
  broken = "dcfifo: overflow or underflow"

  wOutputs :: DcFifoState a -> (Bool, Bool, Unsigned depth)
  wOutputs DcFifoState{..}
    | dBroken = (errorX broken, errorX broken, errorX broken)
    | otherwise =
        ( inc dWrPtr == P.last dWrSync
        , dWrPtr == P.last dWrSync
        , fromIntegral dWrUsed )

  rOutputs :: DcFifoState a -> (a, Bool, Bool, Unsigned depth)
  rOutputs DcFifoState{..}
    | dBroken = (deepErrorX broken, errorX broken, errorX broken, errorX broken)
    | otherwise =
        ( case dcReadMode of
            Normal -> dQ
            ShowAhead
              | empty_ -> deepErrorX "dcfifo: empty"
              | otherwise -> readMem dMem dRdPtr
        , empty_
        , dRdPtr == inc (P.last dRdSync)
        , fromIntegral dRdUsed )
   where
    empty_ = dRdPtr == P.last dRdSync

  -- An active edge of the write clock, given the state before the edge(s)
  -- happening at this time
  wEdge :: DcFifoState a -> Maybe a -> DcFifoState a
  wEdge s@DcFifoState{..} w =
    s
      { dMem = case w of
          Just a | valid -> IntMap.insert dWrPtr a dMem
          _ -> dMem
      , dWrPtr = if valid then inc dWrPtr else dWrPtr
      , dDelayedWrPtr = dWrPtr
      , dWrSync = dRdPtr : P.init dWrSync
      , dWrUsed = (dWrPtr - P.last dWrSync) `mod` cnt
      , dBroken = dBroken || overflow
      }
   where
    full = inc dWrPtr == P.last dWrSync
    overflow = isJust w && full && not dcOverflowChecking
    valid = isJust w && not full

  -- An active edge of the read clock, given the state before the edge(s)
  -- happening at this time
  rEdge :: DcFifoState a -> Bool -> DcFifoState a
  rEdge s@DcFifoState{..} r =
    s
      { dRdPtr = if valid then inc dRdPtr else dRdPtr
      , dRdSync = dDelayedWrPtr : P.init dRdSync
      , dRdUsed = (P.last dRdSync - dRdPtr) `mod` cnt
      , dQ = if valid then readMem dMem dRdPtr else dQ
      , dBroken = dBroken || underflow
      }
   where
    empty_ = dRdPtr == P.last dRdSync
    underflow = r && empty_ && not dcUnderflowChecking
    valid = r && not empty_

  -- Both clocks have an active edge at the same time
  bothEdges :: DcFifoState a -> Maybe a -> Bool -> DcFifoState a
  bothEdges s w r =
    sW
      { dRdPtr = dRdPtr sR
      , dRdSync = dRdSync sR
      , dRdUsed = dRdUsed sR
      , dQ = dQ sR
      , dBroken = dBroken sW || dBroken sR
      }
   where
    sW = wEdge s w
    sR = rEdge s r

  -- Apply the asynchronous reset if it is asserted
  withReset :: Bool -> DcFifoState a -> DcFifoState a
  withReset r s = if r then resetState (dMem s) else s

  (wOuts, rOuts) = case unsafeToActiveHigh rst of
    r0 :- rs -> go (clockTicks wClk rClk) (withReset r0 (resetState IntMap.empty)) r0 rs wr rd

  -- The reset is a signal of the write domain: reset sample /j/ is present
  -- from write edge /j-1/ up to and including write edge /j/. While it is
  -- asserted, edges don't change the state.
  go ::
    [ClockAB] ->
    DcFifoState a ->
    -- Reset present now
    Bool ->
    -- Upcoming reset samples
    Signal write Bool ->
    Signal write (Maybe a) ->
    Signal read Bool ->
    ( Signal write (Bool, Bool, Unsigned depth)
    , Signal read (a, Bool, Bool, Unsigned depth) )
  go [] _ _ _ _ _ = error "dcfifo: clockTicks should be infinite"
  go (tick : ticks) s rNow rsNext@(~(rNext :- rsNext1)) ws@(~(w :- ws1)) rds@(~(r :- rds1)) =
    case tick of
      ClockA -> (wOutputs s :- wRest, rRest)
       where
        s' = withReset rNext (if rNow then s else wEdge s w)
        (wRest, rRest) = go ticks s' rNext rsNext1 ws1 rds
      ClockB -> (wRest, rOutputs s :- rRest)
       where
        s' = if rNow then s else rEdge s r
        (wRest, rRest) = go ticks s' rNow rsNext ws rds1
      ClockAB -> (wOutputs s :- wRest, rOutputs s :- rRest)
       where
        s' = withReset rNext (if rNow then s else bothEdges s w r)
        (wRest, rRest) = go ticks s' rNext rsNext1 ws1 rds1
