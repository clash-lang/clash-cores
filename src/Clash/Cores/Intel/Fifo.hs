{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

FIFOs of Intel FPGAs: the single-clock FIFO @scfifo@. For more information
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
    -- * Configuration
  , ReadMode (..)
  ) where

import Clash.Explicit.Prelude

import Clash.Signal.Internal (Signal ((:-)))
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)

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
