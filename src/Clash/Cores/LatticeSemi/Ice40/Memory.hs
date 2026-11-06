{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Memory primitives of the iCE40 FPGAs: the 4 kbit block RAM @SB_RAM40_4K@
  (and its variants with inverted clocks) and the 256 kbit single-port RAM
  @SB_SPRAM256KA@ of the iCE40 UltraPlus devices, see the
  <https://www.latticesemi.com/-/media/LatticeSemi/Documents/ApplicationNotes/MO/MemoryUsageGuideforiCE40Devices.ashx iCE40 Memory Usage Guide>
  (TN1250), the
  <https://www.latticesemi.com/-/media/LatticeSemi/Documents/ApplicationNotes/IK/FPGA-TN-02022-1-2-iCE40-SPRAM-Usage-Guide.ashx iCE40 SPRAM Usage Guide>
  (FPGA-TN-02022) and the
  <http://www.latticesemi.com/~/media/LatticeSemi/Documents/TechnicalBriefs/SBTICETechnologyLibrary201504.pdf iCE Technology Library>.

  Synthesis tools infer block RAMs from 'Clash.Explicit.Prelude.blockRam' and
  friends, so there's usually no need to instantiate them explicitly. Use
  these when you need control over the exact primitive, e.g. to use different
  read and write widths, or to have a predictable placement.

  The simulation models follow the community simulation models of Yosys
  (@share\/yosys\/ice40\/cells_sim.v@), against which they are tested with
  Yosys 0.70 (@\/nix\/store\/cfry5mn7l5qk6cy730gspaa51fahc108-yosys-0.70@).
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.LatticeSemi.Ice40.Memory
  ( -- * SB_RAM40_4K
    sbRam40_4k
  , RamMode (..)
    -- ** Initial contents
  , RamInit
  , ramInit
  , ramInitWords
  , ramInitZero
    -- * SB_SPRAM256KA
  , sbSpram256ka
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (ClockAB (..), Signal ((:-)), clockTicks)
import Clash.Sized.Internal.BitVector (undefined#)

import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)

import qualified Data.Sequence as Seq
import qualified Prelude as P

import Clash.Cores.LatticeSemi.Ice40.Internal (ice40Prim)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( BitVectorParam (..), ClockPort (..), Inst, Param (..), Port (..), inst
  , unPort )

{- | Read or write mode of an 'sbRam40_4k': the number of words (@depth@) and
their width. The read and write mode of a block RAM can differ.

In the narrow modes, a word occupies bits of the 16-bit words of the
@256x16@ mode: bit /j/ of word /a/ of a mode with width /w/ is bit
@(16 \/ w) * j + a \/ 256@ of word @a `mod` 256@ of the @256x16@ mode.
-}
data RamMode (depth :: Nat) (width :: Nat) where
  -- | @256x16@ (mode 0). Writes can be masked per bit.
  RamMode256x16 :: RamMode 256 16
  -- | @512x8@ (mode 1)
  RamMode512x8 :: RamMode 512 8
  -- | @1024x4@ (mode 2)
  RamMode1024x4 :: RamMode 1024 4
  -- | @2048x2@ (mode 3)
  RamMode2048x2 :: RamMode 2048 2

deriving instance Show (RamMode depth width)

-- | Value of the @READ_MODE@ and @WRITE_MODE@ parameters
modeNumber :: RamMode depth width -> Integer
modeNumber = \case
  RamMode256x16 -> 0
  RamMode512x8 -> 1
  RamMode1024x4 -> 2
  RamMode2048x2 -> 3

-- | Number of bits of a @256x16@ word between the bits of a word of the mode
modeStride :: RamMode depth width -> Int
modeStride = \case
  RamMode256x16 -> 1
  RamMode512x8 -> 2
  RamMode1024x4 -> 4
  RamMode2048x2 -> 8

{- | Position of bit /j/ of a word on the @WDATA@ and @RDATA@ ports of the
primitive. In the narrow modes, the primitive uses every second, fourth or
eighth bit of its data ports.
-}
modePortBit :: RamMode depth width -> Int -> Int
modePortBit mode j = case mode of
  RamMode256x16 -> j
  RamMode512x8 -> 2 * j
  RamMode1024x4 -> 4 * j + 1
  RamMode2048x2 -> 8 * j + 3

{- | Initial contents of an 'sbRam40_4k': the values of its parameters
@INIT_0@ to @INIT_F@. Bits @16*i+15@ to @16*i@ of @INIT_k@ hold word
@16*k + i@ of the @256x16@ mode. Use 'ramInit' or 'ramInitWords' to create
them.
-}
type RamInit = Vec 16 (BitVector 256)

-- | All zeroes. Unlike the results of 'ramInit' and 'ramInitWords', this
-- doesn't need Template Haskell to be a constant.
ramInitZero :: RamInit
ramInitZero =
  0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil

-- | Initial contents of an 'sbRam40_4k' from the 256 words of the @256x16@
-- mode
ramInitWords :: Vec 256 (BitVector 16) -> RamInit
ramInitWords ws = map (pack . reverse) (unconcatI @16 @16 ws)

-- | Words of the @256x16@ mode, inverse of 'ramInitWords'
initWords :: RamInit -> Vec 256 (BitVector 16)
initWords = concatMap (reverse . unpack)

{- | Initial contents of an 'sbRam40_4k' from the words of a mode: reading
word /a/ in that mode returns element /a/.

The contents must be a constant in HDL. Calculate them at compile time with
Template Haskell, e.g.:

@
initial = $(lift (ramInit RamMode512x8 (map fromIntegral (indicesI \@512))))
@
-}
ramInit ::
  forall depth width.
  (KnownNat depth, KnownNat width) =>
  RamMode depth width ->
  Vec depth (BitVector width) ->
  RamInit
ramInit mode contents = ramInitWords (map word indicesI)
 where
  stride = modeStride mode
  word :: Index 256 -> BitVector 16
  word a = pack (reverse (map (bitOf (fromIntegral a)) indicesI))
  bitOf :: Int -> Index 16 -> Bit
  bitOf a (fromIntegral -> b) =
    contents !! ((b `mod` stride) * 256 + a) ! (b `div` stride)

{- | 4 kbit block RAM @SB_RAM40_4K@, with a write port and a read port.

The domains of the clocks determine which variant gets instantiated: a domain
whose active edge is the falling edge uses the variant with that clock
inverted (@SB_RAM40_4KNR@, @SB_RAM40_4KNW@ or @SB_RAM40_4KNRNW@).

The read port has a latency of one clock cycle. If the same word is read and
written at the same time (with the same clock), the read returns the old
contents. With different clocks, the read returns the contents at the time of
the active edge of the read clock. The output starts out undefined.

Simulation: an undefined write enable or write address makes the whole memory
undefined.
-}
sbRam40_4k ::
  forall domW domR depthW widthW depthR widthR.
  ( HasCallStack
  , KnownDomain domW
  , KnownDomain domR
  , KnownNat depthW
  , KnownNat widthW
  , KnownNat depthR
  , KnownNat widthR
  ) =>
  -- | @WRITE_MODE@
  RamMode depthW widthW ->
  -- | @READ_MODE@
  RamMode depthR widthR ->
  -- | Initial contents (@INIT_0@ to @INIT_F@), see 'ramInit'. Must be a
  -- constant in HDL, see 'ramInit'.
  RamInit ->
  -- | Write clock (@WCLK@ or @WCLKN@)
  Clock domW ->
  -- | Write clock enable (@WCLKE@)
  Enable domW ->
  -- | Word to write: @WE@, @WADDR@ and @WDATA@
  Signal domW (Maybe (Index depthW, BitVector widthW)) ->
  -- | Write mask (@MASK@): bits that are set are /not/ written. Only used in
  -- 'RamMode256x16'.
  Signal domW (BitVector 16) ->
  -- | Read clock (@RCLK@ or @RCLKN@)
  Clock domR ->
  -- | Read clock enable (@RCLKE@)
  Enable domR ->
  -- | Read enable (@RE@)
  Signal domR Bool ->
  -- | Read address (@RADDR@)
  Signal domR (Index depthR) ->
  -- | Read data (@RDATA@)
  Signal domR (BitVector widthR)
sbRam40_4k wMode rMode initial wClk wEn write mask rClk rEn re rAddr
  | clashSimulation = rDataSim
  | otherwise = fromPortData rMode <$> unPort rDataPort
 where
  -- Simulation
  rEnable = (&&) <$> fromEnable rEn <*> re
  rDataSim =
    delay rClk (toEnable rEnable) (errorX "sbRam40_4k: RDATA undefined") $
      readWord rMode <$> rAddr <*> ramModel wClk rClk (initWords initial) wordWrites (wordAddr <$> rAddr)
  wordWrites = toWordWrite <$> fromEnable wEn <*> write <*> mask

  toWordWrite en w m = case (en, w) of
    (True, Just (addr, d)) -> Just (writeWord wMode addr d m)
    (False, _) -> Nothing
    (True, Nothing) -> Nothing

  -- HDL
  rDataPort :: Port "RDATA" domR (BitVector 16)
  rDataPort = case (activeEdge @domR, activeEdge @domW) of
    (SRising, SRising) ->
      ports (inst' "SB_RAM40_4K") (ClockPort @"RCLK" rClk) (ClockPort @"WCLK" wClk)
    (SFalling, SRising) ->
      ports (inst' "SB_RAM40_4KNR") (ClockPort @"RCLKN" rClk) (ClockPort @"WCLK" wClk)
    (SRising, SFalling) ->
      ports (inst' "SB_RAM40_4KNW") (ClockPort @"RCLK" rClk) (ClockPort @"WCLKN" wClk)
    (SFalling, SFalling) ->
      ports (inst' "SB_RAM40_4KNRNW") (ClockPort @"RCLKN" rClk) (ClockPort @"WCLKN" wClk)

  inst' :: Inst r => String -> r
  inst' nm =
    inst
      (ice40Prim nm)
      (Param @"WRITE_MODE" @Integer (modeNumber wMode))
      (Param @"READ_MODE" @Integer (modeNumber rMode))
      (BitVectorParam @"INIT_0" i0)
      (BitVectorParam @"INIT_1" i1)
      (BitVectorParam @"INIT_2" i2)
      (BitVectorParam @"INIT_3" i3)
      (BitVectorParam @"INIT_4" i4)
      (BitVectorParam @"INIT_5" i5)
      (BitVectorParam @"INIT_6" i6)
      (BitVectorParam @"INIT_7" i7)
      (BitVectorParam @"INIT_8" i8)
      (BitVectorParam @"INIT_9" i9)
      (BitVectorParam @"INIT_A" iA)
      (BitVectorParam @"INIT_B" iB)
      (BitVectorParam @"INIT_C" iC)
      (BitVectorParam @"INIT_D" iD)
      (BitVectorParam @"INIT_E" iE)
      (BitVectorParam @"INIT_F" iF)

  -- Match on the constructors rather than with the ':>' pattern synonym: Clash
  -- can't reduce the latter's 'head' and 'tail' to the constants 'inst' needs.
  (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, iA, iB, iC, iD, iE, iF) =
    case initial of
      Cons j0 (Cons j1 (Cons j2 (Cons j3 (Cons j4 (Cons j5 (Cons j6 (Cons j7 (Cons j8
        (Cons j9 (Cons jA (Cons jB (Cons jC (Cons jD (Cons jE (Cons jF Nil))))))))))))))) ->
          (j0, j1, j2, j3, j4, j5, j6, j7, j8, j9, jA, jB, jC, jD, jE, jF)

  ports ::
    ( rclk -> wclk ->
      Port "RCLKE" domR Bit ->
      Port "RE" domR Bit ->
      Port "RADDR" domR (BitVector 11) ->
      Port "WCLKE" domW Bit ->
      Port "WE" domW Bit ->
      Port "WADDR" domW (BitVector 11) ->
      Port "MASK" domW (BitVector 16) ->
      Port "WDATA" domW (BitVector 16) ->
      r ) ->
    rclk -> wclk -> r
  ports f rclk wclk =
    f rclk wclk
      (Port @"RCLKE" (boolToBit <$> fromEnable rEn))
      (Port @"RE" (boolToBit <$> re))
      (Port @"RADDR" (addr11 <$> rAddr))
      (Port @"WCLKE" (boolToBit <$> fromEnable wEn))
      (Port @"WE" (boolToBit . isJust <$> write))
      (Port @"WADDR" (addr11 . fst . fromJustX <$> write))
      (Port @"MASK" mask)
      (Port @"WDATA" (toPortData wMode . resize . snd . fromJustX <$> write))

  addr11 :: forall n. KnownNat n => Index n -> BitVector 11
  addr11 = resize . pack

-- | Word of the @256x16@ mode that contains a word of a mode
wordAddr :: KnownNat depth => Index depth -> Index 256
wordAddr a = fromIntegral (fromIntegral a `mod` (256 :: Int))

-- | Which of the bits of a @256x16@ word hold a word of a narrow mode
subWord :: KnownNat depth => Index depth -> Int
subWord a = fromIntegral a `div` 256

-- | A write to a word of the @256x16@ mode: word address, bits to write (set
-- bits are written) and data
type WordWrite = (Index 256, BitVector 16, BitVector 16)

-- | Translate a write in a mode to a write of a @256x16@ word
writeWord ::
  forall depth width.
  (KnownNat depth, KnownNat width) =>
  RamMode depth width ->
  Index depth ->
  BitVector width ->
  -- | MASK
  BitVector 16 ->
  WordWrite
writeWord mode addr d mask = case mode of
  RamMode256x16 -> (wordAddr addr, complement mask, d)
  _ -> (wordAddr addr, bitMask, spread)
 where
  stride = modeStride mode
  subIx = subWord addr
  bitMask = P.foldl setBit 0 [stride * j + subIx | j <- [0 .. natToNum @width - 1]] :: BitVector 16
  -- Every bit of the 256x16 word gets the bit of the narrow word it would
  -- hold, so that the bits to write can be selected with the mask.
  spread = pack (map (\(fromIntegral -> b) -> d ! (b `div` stride)) (reverse indicesI :: Vec 16 (Index 16)))

-- | Read a word of a mode from its @256x16@ word
readWord ::
  forall depth width.
  (KnownNat depth, KnownNat width) =>
  RamMode depth width ->
  Index depth ->
  BitVector 16 ->
  BitVector width
readWord mode addr w = case mode of
  RamMode256x16 -> resize w
  _ -> pack (map (\(fromIntegral -> j) -> w ! (stride * j + subIx)) (reverse indicesI :: Vec width (Index width)))
 where
  stride = modeStride mode
  subIx = subWord addr

-- | Place a word of a mode on the 16-bit data ports of the primitive. Unused
-- bits are zero.
toPortData :: forall depth width. KnownNat width => RamMode depth width -> BitVector 16 -> BitVector 16
toPortData mode d = case mode of
  RamMode256x16 -> d
  _ -> pack (map portBit (reverse indicesI :: Vec 16 (Index 16)))
 where
  stride = modeStride mode
  -- Position of bit 0 of a word on the ports
  offset = modePortBit mode 0
  portBit :: Index 16 -> Bit
  portBit (fromIntegral -> p)
    | p `mod` stride == offset = d ! (p `div` stride)
    | otherwise = 0

-- | Take a word of a mode from the 16-bit data ports of the primitive
fromPortData :: forall depth width. KnownNat width => RamMode depth width -> BitVector 16 -> BitVector width
fromPortData mode d = case mode of
  RamMode256x16 -> d
  _ -> pack (map (\(fromIntegral -> j) -> d ! modePortBit mode j) (reverse indicesI :: Vec width (Index width)))

{- | Model of the memory array of 256 16-bit words, with a write port and an
asynchronous read port in possibly different domains. A write in a clock cycle
is visible to reads from the next active edge of the write clock. When the
active edges of both clocks coincide, the read sees the old contents.
-}
ramModel ::
  forall domW domR.
  (KnownDomain domW, KnownDomain domR) =>
  Clock domW ->
  Clock domR ->
  Vec 256 (BitVector 16) ->
  Signal domW (Maybe WordWrite) ->
  Signal domR (Index 256) ->
  Signal domR (BitVector 16)
ramModel wClk rClk initial writes readAddrs =
  go (clockTicks wClk rClk) (Seq.fromList (toList initial)) writes readAddrs
 where
  go :: [ClockAB] -> Seq.Seq (BitVector 16) -> Signal domW (Maybe WordWrite) -> Signal domR (Index 256) -> Signal domR (BitVector 16)
  go [] _ _ _ = error "sbRam40_4k: clockTicks should be infinite"
  go (tick : ticks) !ram ws@(~(w :- wsRest)) rs@(~(r :- rsRest)) =
    case tick of
      ClockA -> go ticks (update ram w) wsRest rs
      ClockB ->
        let o = readRam ram r in o :- go ticks ram ws rsRest
      ClockAB -> go (ClockB : ClockA : ticks) ram ws rs

  readRam ram r = case maybeIsX r of
    Just a -> Seq.index ram (fromIntegral a)
    Nothing -> errorX "sbRam40_4k: read address undefined"

  update ram w = case maybeIsX w of
    Nothing -> fmap (const (errorX "sbRam40_4k: write enable undefined")) ram
    Just Nothing -> ram
    Just (Just (addr, bits, d)) -> case maybeIsX addr of
      Nothing -> fmap (const (errorX "sbRam40_4k: write address undefined")) ram
      Just a ->
        Seq.adjust (\old -> (old .&. complement bits) .|. (d .&. bits)) (fromIntegral a) ram

{- | 256 kbit single-port RAM @SB_SPRAM256KA@ of the iCE40 UltraPlus devices:
16384 words of 16 bits, with a write mask per nibble and three low-power
modes.

Every active clock edge with @CHIPSELECT@ asserted either reads or writes a
word. A read returns the word in the next clock cycle; a write makes the
output undefined. Without @CHIPSELECT@ the output holds its value.

The low-power modes:

* @STANDBY@: the memory ignores reads and writes, and its output becomes
  undefined at the next active clock edge.
* @SLEEP@: the output is 0, immediately (asynchronously). The memory keeps its
  contents.
* @POWEROFF@ (active low): the output is 0, immediately, and the contents of
  the memory are lost.

Leaving @SLEEP@ or @POWEROFF@ takes time in hardware (see the SPRAM usage
guide); neither the Haskell model nor Yosys' model simulate that.

The memory starts out undefined: it has no initial contents.
-}
sbSpram256ka ::
  forall dom.
  ( HasCallStack
  , KnownDomain dom
  , DomainActiveEdge dom ~ 'Rising
  ) =>
  -- | @CLOCK@
  Clock dom ->
  -- | @CHIPSELECT@
  Signal dom Bool ->
  -- | @ADDRESS@
  Signal dom (Index 16384) ->
  -- | Write (@WREN@): the nibbles to write (@MASKWREN@, bit /i/ enables
  -- writing bits @4*i+3@ to @4*i@) and the word (@DATAIN@), or 'Nothing' to
  -- read
  Signal dom (Maybe (BitVector 4, BitVector 16)) ->
  -- | @STANDBY@
  Signal dom Bool ->
  -- | @SLEEP@
  Signal dom Bool ->
  -- | Powered on (@POWEROFF@, active low)
  Signal dom Bool ->
  -- | @DATAOUT@
  Signal dom (BitVector 16)
sbSpram256ka clk cs addr write standby sleep powerOn
  | clashSimulation = dataOutSim
  | otherwise = unPort dataOutPort
 where
  -- Simulation
  off = (||) <$> sleep <*> (not <$> powerOn)
  dataOutSim = mux off 0 dataOutReg
  dataOutReg =
    delay clk enableGen (errorX "sbSpram256ka: DATAOUT undefined") $
      nextOut <$> off <*> standby <*> cs <*> write <*> memRead <*> dataOutSim
  nextOut off0 standby0 cs0 write0 memRead0 out0
    | off0 = 0
    | standby0 = undefined#
    | cs0 = case write0 of
        Nothing -> memRead0
        Just _ -> undefined#
    | otherwise = out0

  memRead = spramModel clk memOp addr
  memOp = memOp0 <$> powerOn <*> off <*> standby <*> cs <*> write
  memOp0 powerOn0 off0 standby0 cs0 write0
    | not powerOn0 = SpramClear
    | off0 || standby0 || not cs0 = SpramIdle
    | Just (m, d) <- write0 = SpramWrite m d
    | otherwise = SpramIdle

  -- HDL
  dataOutPort :: Port "DATAOUT" dom (BitVector 16)
  dataOutPort =
    inst
      (ice40Prim "SB_SPRAM256KA")
      (Port @"ADDRESS" (pack <$> addr))
      (Port @"DATAIN" (snd . fromJustX <$> write))
      (Port @"MASKWREN" (fst . fromJustX <$> write))
      (Port @"WREN" (boolToBit . isJust <$> write))
      (Port @"CHIPSELECT" (boolToBit <$> cs))
      (ClockPort @"CLOCK" clk)
      (Port @"STANDBY" (boolToBit <$> standby))
      (Port @"SLEEP" (boolToBit <$> sleep))
      (Port @"POWEROFF" (boolToBit <$> powerOn))

-- | What happens to the memory of an 'sbSpram256ka' at a clock edge
data SpramOp
  = SpramIdle
  | SpramWrite (BitVector 4) (BitVector 16)
  -- ^ Write the nibbles in the mask
  | SpramClear
  -- ^ Powered off: the contents are lost
  deriving (Generic, NFDataX)

-- | Model of the memory array of an 'sbSpram256ka'. Returns the word at the
-- address, before the operation of the same clock cycle.
spramModel ::
  forall dom.
  KnownDomain dom =>
  Clock dom ->
  Signal dom SpramOp ->
  Signal dom (Index 16384) ->
  Signal dom (BitVector 16)
spramModel _clk = go (Seq.replicate 16384 undefined#)
 where
  go :: Seq.Seq (BitVector 16) -> Signal dom SpramOp -> Signal dom (Index 16384) -> Signal dom (BitVector 16)
  go !mem ~(op :- ops) ~(a :- as) = out :- go mem1 ops as
   where
    out = case maybeIsX a of
      Just a1 -> Seq.index mem (fromIntegral a1)
      Nothing -> undefined#
    mem1 = case maybeIsX op of
      Nothing -> undefined# <$ mem
      Just SpramIdle -> mem
      Just SpramClear -> undefined# <$ mem
      Just (SpramWrite m d) -> case maybeIsX a of
        Nothing -> undefined# <$ mem
        Just a1 -> Seq.adjust (writeNibbles m d) (fromIntegral a1) mem

  writeNibbles :: BitVector 4 -> BitVector 16 -> BitVector 16 -> BitVector 16
  writeNibbles m d old = (old .&. complement bits) .|. (d .&. bits)
   where
    bits = pack (map (\(i :: Index 4) -> if testBit m (fromIntegral i) then 0xF else 0 :: BitVector 4) (reverse indicesI))
