{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Definitions shared by the @XPM_MEMORY@ wrappers in "Clash.Cores.Xilinx.Xpm.Memory":
  configuration types, helpers to render XPM parameters, and a simulation model
  shared by all memory macros. All six macros are thin wrappers around Xilinx's
  @xpm_memory_base@, so they share a single simulation model too.

  The simulation model follows the behavioral model in Vivado's @xpm_memory.sv@,
  which is what Vivado's simulator runs.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Clash.Cores.Xilinx.Xpm.Memory.Internal where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)), sameDomain)
import Clash.Sized.Internal.BitVector (BitVector(BV), undefined#)
import Clash.XException.MaybeX (MaybeX(IsX, IsDefined), toMaybeX)

import Data.Char (isHexDigit, digitToInt, isSpace, toLower)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (ExpQ, runIO)
import System.Directory (makeAbsolute)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Sequence as Seq
import qualified Prelude as P

-- $setup
-- >>> import Clash.Explicit.Prelude
-- >>> import Clash.Cores.Xilinx.Xpm.Memory.Internal

-- | Type of primitive the memory should be implemented in. This is what
-- [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_TDPRAM)
-- calls @MEMORY_PRIMITIVE@.
data MemoryPrimitive
  = -- | Let Vivado choose the primitive. Simulates like 'BlockRam'.
    AutoPrimitive
  | -- | Distributed RAM (LUTRAM). The only primitive supporting a read latency
    -- of zero. Ports writing to distributed RAM must use 'ReadFirst'.
    DistributedRam
  | -- | Block RAM
    BlockRam
  | -- | UltraRAM. Only supports a common clock, a reset value of zero, and
    -- 'NoChange' for true dual port RAMs. Only available on UltraScale+ devices.
    UltraRam
  deriving (Show, Eq)

-- | Behavior of a port's read output when that same port writes. This is what
-- [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_TDPRAM)
-- calls @WRITE_MODE@.
data WriteMode
  = -- | Output the data that is written. With byte enables, bytes that are not
    -- written show the memory contents.
    WriteFirst
  | -- | Output the contents of the memory before the write
    ReadFirst
  | -- | Keep the output unchanged
    NoChange
  deriving (Show, Eq)

-- | Initial contents of a memory
data MemoryInit
  = -- | All words zero
    NoInit
  | -- | Initialize the memory from a file in the hexadecimal @.mem@ format Xilinx
    -- uses, which is the format Verilog's @$readmemh@ reads. Each word in the
    -- file is the hexadecimal representation of a memory word ('pack'ed value),
    -- with the first word going to address /0/. Lines can contain comments
    -- (@\/\/@), and @\@addr@ (hexadecimal) moves to another address. Words not
    -- mentioned in the file are undefined. 'xpmMemFile' renders such a file.
    --
    -- The path is passed verbatim to the HDL instantiation (@MEMORY_INIT_FILE@)
    -- and is read by the Haskell simulation. It must end in @.mem@. When
    -- simulating in Haskell, a relative path is relative to the current working
    -- directory. In the HDL, it is relative to wherever Vivado looks: for
    -- synthesis, add the file to your project and pass just its file name;
    -- for simulation, Vivado looks in the simulation's working directory. An
    -- absolute path works for both. The path must be a constant Clash can
    -- render, such as a string literal.
    InitFile FilePath
  deriving (Show, Eq)

-- | Configuration shared by both ports of a memory. Use 'defaultXpmMemoryConfig'
-- and override fields as needed.
data XpmMemoryConfig = XpmMemoryConfig
  { -- | See 'MemoryPrimitive'. Ignored by
    -- 'Clash.Cores.Xilinx.Xpm.Memory.Dpdistram.xpmMemoryDpdistram', which
    -- always uses 'DistributedRam'.
    memoryPrimitive :: MemoryPrimitive
    -- | See 'MemoryInit'
  , memoryInit :: MemoryInit
  }
  deriving (Show, Eq)

-- | 'AutoPrimitive', 'NoInit'
defaultXpmMemoryConfig :: XpmMemoryConfig
defaultXpmMemoryConfig = XpmMemoryConfig
  { memoryPrimitive = AutoPrimitive
  , memoryInit = NoInit
  }

-- | Configuration of a single port of a memory. Use 'defaultXpmMemoryPortConfig'
-- and override fields as needed.
data XpmMemoryPortConfig latency a = XpmMemoryPortConfig
  { -- | Number of clock cycles between presenting an address (with the port
    -- enabled) and the read data appearing on the output. With a latency of
    -- /1/, the output register is the memory's internal read register. With
    -- larger latencies, the last register is enabled by @regce@ and the
    -- registers in between by a delayed version of the port enable. A latency
    -- of /0/ means an asynchronous read and is only supported by
    -- 'DistributedRam'.
    --
    -- This is what UG974 calls @READ_LATENCY@.
    readLatency :: SNat latency
    -- | See 'WriteMode'. Must be 'ReadFirst' for ports writing to
    -- 'DistributedRam'. Ignored for ports that can't write, except for port B
    -- of 'Clash.Cores.Xilinx.Xpm.Memory.Sdpram.xpmMemorySdpram', where it
    -- determines whether reading an address port A writes is a collision.
    --
    -- This is what UG974 calls @WRITE_MODE@.
  , writeMode :: WriteMode
    -- | Value of the output register after reset and at power-up. Must be
    -- zero (all bits low) when using 'UltraRam' or an asynchronous reset.
    --
    -- This is what UG974 calls @READ_RESET_VALUE@.
  , readResetValue :: a
  }

-- | Read latency /2/, 'NoChange', and a reset value of zero
defaultXpmMemoryPortConfig :: BitPack a => XpmMemoryPortConfig 2 a
defaultXpmMemoryPortConfig = XpmMemoryPortConfig
  { readLatency = d2
  , writeMode = NoChange
  , readResetValue = unpack 0
  }

-- | Kind of memory, determines which ports read and write. Corresponds to
-- @MEMORY_TYPE@ of @xpm_memory_base@.
data MemoryType
  = SinglePortRam
  | SimpleDualPortRam
  | TrueDualPortRam
  | SinglePortRom
  | DualPortRom
    -- | Port A reads and writes, port B only reads. Always distributed RAM.
  | DualPortDistributedRam
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- HDL parameters
------------------------------------------------------------------------------

-- | Render 'MemoryPrimitive' as a @MEMORY_PRIMITIVE@ parameter
primitiveParam :: MemoryPrimitive -> String
primitiveParam = \case
  AutoPrimitive -> "auto"
  DistributedRam -> "distributed"
  BlockRam -> "block"
  UltraRam -> "ultra"

-- | Render 'WriteMode' as a @WRITE_MODE@ parameter
writeModeParam :: WriteMode -> String
writeModeParam = \case
  WriteFirst -> "write_first"
  ReadFirst -> "read_first"
  NoChange -> "no_change"

-- | Render 'MemoryInit' as a @MEMORY_INIT_FILE@ parameter
initFileParam :: MemoryInit -> String
initFileParam = \case
  NoInit -> "none"
  InitFile path -> path

-- | Render a domain's reset kind as a @RST_MODE@ parameter
rstModeParam :: forall dom. KnownDomain dom => String
rstModeParam = case resetKind @dom of
  SAsynchronous -> "ASYNC"
  SSynchronous -> "SYNC"

-- | Whether a domain uses asynchronous resets
isAsyncReset :: forall dom. KnownDomain dom => Bool
isAsyncReset = case resetKind @dom of
  SAsynchronous -> True
  SSynchronous -> False

-- | Whether a domain's clock has a rising active edge
isRisingEdge :: forall dom. KnownDomain dom => Bool
isRisingEdge = case activeEdge @dom of
  SRising -> True
  SFalling -> False

-- | Whether two domains are the same. Ports in the same domain are driven by
-- the same clock, so the memory is instantiated with a common clock.
isCommonClock :: forall domA domB. (KnownDomain domA, KnownDomain domB) => Bool
isCommonClock = isJust (sameDomain @domA @domB)

-- | Render whether two domains are the same as a @CLOCKING_MODE@ parameter
clockingModeParam :: forall domA domB. (KnownDomain domA, KnownDomain domB) => String
clockingModeParam
  | isCommonClock @domA @domB = "common_clock"
  | otherwise = "independent_clock"

-- | Render the contents of a memory initialization file, suitable for
-- 'InitFile'. Each word is written on its own line as a hexadecimal number.
--
-- >>> putStr (xpmMemFile [1, 2, 0xABC :: Unsigned 12])
-- 001
-- 002
-- ABC
xpmMemFile :: forall a f. (BitPack a, Foldable f) => f a -> String
xpmMemFile = P.unlines . P.map (renderHex . pack) . P.foldr (:) []

-- | Write a memory initialization file at compile time, and return an
-- expression for an 'InitFile' referring to it by its absolute path. This makes
-- it possible to initialize a memory from a list, 'Vec', or
-- 'Clash.Explicit.BlockRam.Blob.MemBlob' (using
-- 'Clash.Explicit.BlockRam.Blob.unpackMemBlob'), as long as the contents can
-- be computed in Template Haskell, i.e., using definitions from other modules.
-- A relative path is relative to the working directory of the compiler.
--
-- For example:
--
-- @
-- rom = xpmMemorySprom $(initFileTH "sine.mem" (map sine [0..255 :: Int])) ...
-- @
--
-- Note that the generated HDL contains the absolute path, so the file needs to
-- be at the same location when using the HDL.
initFileTH :: (BitPack a, Foldable f) => FilePath -> f a -> ExpQ
initFileTH path contents = do
  absPath <- runIO $ do
    writeFile path (xpmMemFile contents)
    makeAbsolute path
  [| InitFile absPath |]

-- | Render a 'BitVector' as a hexadecimal number. Undefined nibbles are
-- rendered as @X@.
renderHex :: forall n. KnownNat n => BitVector n -> String
renderHex (BV mask val) = P.map go (P.reverse [0 .. nDigits - 1])
 where
  nDigits = P.max 1 ((natToNum @n + 3) `P.div` 4) :: Int
  nibble x i = fromIntegral ((x `shiftR` (4 * i)) .&. 0xF) :: Int
  go i
    | nibble mask i /= 0 = 'X'
    | otherwise = "0123456789ABCDEF" P.!! nibble val i

-- | Parse a memory initialization file, see 'InitFile'. Words that the file does
-- not mention are undefined.
--
-- >>> parseMemFile @8 4 "// comment\n01 02\n@3 ff"
-- fromList [0b0000_0001,0b0000_0010,0b...._....,0b1111_1111]
parseMemFile :: forall w. KnownNat w => Int -> String -> Seq (BitVector w)
parseMemFile depth contents =
  P.foldl set (Seq.replicate depth undefined#) (assign 0 (tokens contents))
 where
  set mem (addr, word)
    | addr < depth = Seq.update addr word mem
    | otherwise = mem

  assign :: Int -> [String] -> [(Int, BitVector w)]
  assign _ [] = []
  assign _ (('@':a):ts) = assign (readHexInt a) ts
  assign addr (t:ts) = (addr, parseWord t) : assign (addr + 1) ts

  readHexInt :: String -> Int
  readHexInt = P.foldl (\acc c -> acc * 16 + digitToInt c) 0 . P.filter (/= '_')

  -- Split into whitespace separated tokens, dropping comments
  tokens :: String -> [String]
  tokens [] = []
  tokens ('/':'/':rest) = tokens (P.dropWhile (/= '\n') rest)
  tokens ('/':'*':rest) = tokens (dropBlockComment rest)
  tokens s@(c:rest)
    | isSpace c = tokens rest
    | otherwise =
        let (tok, rest1) = P.break (\x -> isSpace x || x == '/') s
        in tok : tokens rest1

  dropBlockComment ('*':'/':rest) = rest
  dropBlockComment (_:rest) = dropBlockComment rest
  dropBlockComment [] = []

  parseWord :: String -> BitVector w
  parseWord s = BV (fromInteger maskI .&. allOnes) (fromInteger valI .&. allOnes)
   where
    (maskI, valI) = P.foldl step (0, 0) (P.filter (/= '_') s)
    step (m, v) c
      | isHexDigit c = (m * 16, v * 16 + toInteger (digitToInt c))
      | toLower c `P.elem` ("xz" :: String) = (m * 16 + 15, v * 16)
      | otherwise = error ("parseMemFile: unexpected character " <> show c <> " in " <> show s)
    allOnes = (1 `shiftL` natToNum @w) - 1 :: Natural

-- | Initial contents of a memory
initialContents :: forall w. KnownNat w => Int -> MemoryInit -> Seq (BitVector w)
initialContents depth = \case
  NoInit -> Seq.replicate depth 0
  InitFile path
    | P.reverse (P.take 4 (P.reverse path)) /= ".mem" ->
        error ("XPM_MEMORY: initialization file must have extension .mem: " <> path)
    | otherwise -> parseMemFile depth (unsafePerformIO (readFile path))
{-# NOINLINE initialContents #-}

------------------------------------------------------------------------------
-- Byte enables
------------------------------------------------------------------------------

-- | Expand a byte enable to a bit enable, i.e., repeat each bit of the byte
-- enable @BitSize a / nBytes@ times. Undefined bits stay undefined.
--
-- >>> expandByteEnable @2 @16 0b01
-- 0b0000_0000_1111_1111
expandByteEnable ::
  forall nBytes w.
  (KnownNat nBytes, KnownNat w) =>
  BitVector nBytes ->
  BitVector w
expandByteEnable (BV mask val) = BV (expand mask) (expand val)
 where
  byteWidth = natToNum @w `P.div` natToNum @nBytes :: Int
  byteOnes = (1 `shiftL` byteWidth) - 1 :: Natural
  expand :: Natural -> Natural
  expand x = P.foldl (.|.) 0
    [ byteOnes `shiftL` (i * byteWidth)
    | i <- [0 .. natToNum @nBytes - 1]
    , testBit x i ]

------------------------------------------------------------------------------
-- Simulation model
------------------------------------------------------------------------------

-- | Model configuration of a single port
data PortModelConfig w = PortModelConfig
  { pmLatency :: Int
  , pmWriteMode :: WriteMode
  , pmResetValue :: BitVector w
  , pmAsyncReset :: Bool
  }

-- | Model configuration
data ModelConfig w = ModelConfig
  { mcMemoryType :: MemoryType
  , mcPrimitive :: MemoryPrimitive
  , mcInit :: MemoryInit
  , mcDepth :: Int
  , mcPortA :: PortModelConfig w
  , mcPortB :: PortModelConfig w
  }

-- | Inputs of a single port, sampled at a clock edge
data PortIn w = PortIn
  { piEnable :: Bool
  , piRegce :: Bool
  , piReset :: Bool
  , piAddress :: Int
    -- | Write enable for each bit, i.e., an expanded byte enable
  , piBitEnable :: BitVector w
  , piData :: BitVector w
  }

-- | Read state of a single port
data PortState w = PortState
  { -- | Value of the read register (@douta_reg@ in @xpm_memory.sv@)
    psStage0 :: BitVector w
    -- | Whether the collision model forced the read register to undefined.
    -- This matters for a read latency of /1/: while forced, resets are
    -- ignored.
  , psForced :: Bool
    -- | Pipelined enables (@ena_pipe@), for read latencies larger than /1/
  , psEnaPipe :: [Bool]
    -- | Output pipeline (@douta_pipe@), for read latencies larger than /1/
  , psDataPipe :: [BitVector w]
  }

-- | A write by a port
data WriteReq w = WriteReq
  { -- | Whether the port (potentially) writes
    wrActive :: MaybeX Bool
  , wrAddress :: MaybeX Int
    -- | Bits written, combined with the port enable
  , wrMask :: BitVector w
  , wrData :: BitVector w
  }

-- | The last write of a port, used to detect collisions with independent clocks
data LastWrite w = LastWrite
  { lwTime :: Integer
  , lwAddress :: MaybeX Int
  , lwMask :: BitVector w
  }

-- | Which clocks have an active edge at some point in time
data Edge = EdgeA | EdgeB | EdgeAB
  deriving (Show, Eq)

-- | Turn an undefined 'BitVector' into one with all bits undefined, so the
-- model can work with the mask of a 'BitVector' instead of 'XException's.
sanitize :: KnownNat n => BitVector n -> BitVector n
sanitize bv = case isX bv of
  Left _ -> undefined#
  Right v -> v

-- | Whether a value potentially is 'True', i.e., is 'True' or undefined
possibly :: MaybeX Bool -> Bool
possibly = \case
  IsDefined b -> b
  IsX _ -> True

-- | Three-valued conjunction
andX :: MaybeX Bool -> MaybeX Bool -> MaybeX Bool
andX (IsDefined False) _ = pure False
andX _ (IsDefined False) = pure False
andX (IsDefined True) b = b
andX a _ = a

-- | Three-valued disjunction
orX :: MaybeX Bool -> MaybeX Bool -> MaybeX Bool
orX (IsDefined True) _ = pure True
orX _ (IsDefined True) = pure True
orX (IsDefined False) b = b
orX a _ = a

-- | Three-valued negation
notX :: MaybeX Bool -> MaybeX Bool
notX = fmap not

-- | If-then-else on 'BitVector's that returns bits that differ as undefined
-- when the condition is undefined
ifX :: KnownNat n => MaybeX Bool -> BitVector n -> BitVector n -> BitVector n
ifX c t e = case c of
  IsDefined True -> t
  IsDefined False -> e
  IsX _ -> mergeBits undefined# t e

-- | Update bits of a word. For each bit: if the enable is set, take the new
-- value; if it is not set, keep the old value. If the enable is undefined,
-- the bit is only defined if the new and old value are the same defined value.
mergeBits ::
  forall n.
  KnownNat n =>
  -- | Enable
  BitVector n ->
  -- | New
  BitVector n ->
  -- | Old
  BitVector n ->
  BitVector n
mergeBits (BV enaMask enaVal) (BV newMask newVal) (BV oldMask oldVal) =
  BV resMask (resVal .&. complementN resMask)
 where
  allOnes = (1 `shiftL` natToNum @n) - 1 :: Natural
  complementN x = allOnes `xor` x
  definedOn = complementN enaMask .&. enaVal
  definedOff = complementN enaMask .&. complementN enaVal
  differ = newMask .|. oldMask .|. (newVal `xor` oldVal)
  resMask =
        (definedOn .&. newMask)
    .|. (definedOff .&. oldMask)
    .|. (enaMask .&. differ)
  resVal = (definedOn .&. newVal) .|. (complementN definedOn .&. oldVal)

-- | Read a word from memory
readWord :: KnownNat w => Seq (BitVector w) -> MaybeX Int -> BitVector w
readWord mem = \case
  IsDefined i -> Seq.index mem i
  IsX _ -> undefined#

-- | Apply a write to memory
applyWrite :: KnownNat w => WriteReq w -> Seq (BitVector w) -> Seq (BitVector w)
applyWrite WriteReq{..} mem = case wrActive of
  IsDefined False -> mem
  _ -> case wrAddress of
    IsDefined i -> Seq.adjust' (mergeBits wrMask wrData) i mem
    IsX _ -> fmap (mergeBits (potentialMask wrMask) undefined#) mem
 where
  -- Bits that are potentially written
  potentialMask (BV m v) = BV 0 (m .|. v)

-- | Determine the write a port makes
writeRequest :: KnownNat w => Bool -> PortIn w -> WriteReq w
writeRequest canWrite PortIn{..}
  | not canWrite = WriteReq (pure False) addr 0 dat
  | otherwise = WriteReq active addr mask dat
 where
  addr = toMaybeX piAddress
  dat = sanitize piData
  bitEna@(BV beMask beVal) = sanitize piBitEnable
  mask = case toMaybeX piEnable of
    IsDefined True -> bitEna
    IsDefined False -> 0
    IsX _ -> BV (beMask .|. beVal) 0
  active = case mask of
    BV m v
      | v /= 0 -> pure True
      | m /= 0 -> toMaybeX (errorX "write enable undefined")
      | otherwise -> pure False

-- | Initial state of a port
initialPortState :: KnownNat w => PortModelConfig w -> PortState w
initialPortState PortModelConfig{..}
  | pmLatency <= 1 = PortState pmResetValue False [] []
  | otherwise = PortState
      { psStage0 = earlyStage
      , psForced = False
      , psEnaPipe = P.replicate (pmLatency - 1) False
      , psDataPipe = P.replicate (pmLatency - 2) earlyStage <> [pmResetValue]
      }
 where
  earlyStage = if pmWriteMode == NoChange then pmResetValue else 0

-- | Value a port's read register would get when enabled, given the memory
-- contents to read from and the write that port makes.
readResult ::
  KnownNat w =>
  WriteMode ->
  -- | Memory word at the port's address
  BitVector w ->
  -- | Write by this port
  WriteReq w ->
  -- | Old value of the read register
  BitVector w ->
  BitVector w
readResult mode memWord WriteReq{..} old = case mode of
  ReadFirst -> memWord
  WriteFirst -> mergeBits wrMask wrData memWord
  NoChange -> ifX wrActive old memWord

-- | Output of a port in the current cycle, given its inputs in the current
-- cycle. A read latency of zero is handled separately.
portOutput :: KnownNat w => PortModelConfig w -> PortState w -> PortIn w -> BitVector w
portOutput PortModelConfig{..} PortState{..} PortIn{piReset}
  | pmLatency == 1 = asyncReset (not psForced) psStage0
  | otherwise = asyncReset True (P.last psDataPipe)
 where
  asyncReset allowed v
    | pmAsyncReset && allowed = ifX (toMaybeX piReset) pmResetValue v
    | otherwise = v

-- | Advance the read state of a port on a clock edge
stepPort ::
  KnownNat w =>
  PortModelConfig w ->
  -- | Whether the collision model forces the read register to undefined
  Bool ->
  -- | Value the read register gets when enabled (see 'readResult')
  BitVector w ->
  PortIn w ->
  PortState w ->
  PortState w
stepPort PortModelConfig{..} forceUndefined newValue PortIn{..} s@PortState{..}
  | pmLatency == 0 = s
  | pmLatency == 1 =
      let
        forced = case en of
          IsDefined True -> forceUndefined
          IsDefined False -> psForced
          IsX _ -> psForced || forceUndefined
        enabled = ifX en newValue psStage0
        stage0
          | forced = undefined#
          | otherwise = ifX rst pmResetValue enabled
      in
        s{psStage0 = stage0, psForced = forced}
  | otherwise =
      let
        stage0 = ifX en (if forceUndefined then undefined# else newValue) psStage0
        enaPipe = piEnable : P.init psEnaPipe
        lastStage prev old = ifX rst pmResetValue (ifX regce prev old)
        dataPipe
          | pmLatency == 2 = [lastStage psStage0 (P.last psDataPipe)]
          | otherwise =
              let
                inputs = psStage0 : P.init psDataPipe
                enables = P.init psEnaPipe
                middle = P.zipWith3 (\e i o -> ifX (toMaybeX e) i o) enables (P.init inputs) (P.init psDataPipe)
              in
                middle <> [lastStage (P.last inputs) (P.last psDataPipe)]
      in
        s{psStage0 = stage0, psEnaPipe = enaPipe, psDataPipe = dataPipe}
 where
  en = toMaybeX piEnable
  rst = toMaybeX piReset
  regce = toMaybeX piRegce

-- | Collision window, in picoseconds, of the XPM's simulation model for
-- independent clocks: an access of port B to an address port A wrote at most
-- this long ago is a collision (and vice versa).
collisionWindow :: Integer -> Integer -> Integer
collisionWindow periodA periodB
  | halfA > 1500 && halfB > 1500 = 3000
  | halfA <= 1500 && halfA <= halfB = 2 * halfA
  | halfB <= 1500 && halfB <= halfA = 2 * halfB
  | otherwise = 500
 where
  halfA = periodA `P.div` 2
  halfB = periodB `P.div` 2

-- | Active clock edges of two independent clocks, with their time in
-- picoseconds. Both clocks have their first active edge at time zero.
clockEdges :: Integer -> Integer -> [(Integer, Edge)]
clockEdges periodA periodB = go 0 0
 where
  go tA tB = case compare tA tB of
    EQ -> (tA, EdgeAB) : go (tA + periodA) (tB + periodB)
    LT -> (tA, EdgeA) : go (tA + periodA) tB
    GT -> (tB, EdgeB) : go tA (tB + periodB)

-- | Simulation model of @xpm_memory_base@, see the module documentation of
-- "Clash.Cores.Xilinx.Xpm.Memory" for its behavior. Ports in the same domain
-- are considered to use a common clock.
--
-- Each port's output signal produces a value for each of that port's clock
-- cycles, even for ports that can't read.
xpmMemoryModel ::
  forall w domA domB.
  ( KnownNat w
  , KnownDomain domA
  , KnownDomain domB
  , HasCallStack
  ) =>
  ModelConfig w ->
  Clock domA ->
  Signal domA (PortIn w) ->
  Clock domB ->
  Signal domB (PortIn w) ->
  (Signal domA (BitVector w), Signal domB (BitVector w))
xpmMemoryModel ModelConfig{..} !_clkA insA0 !_clkB insB0 =
  go
    (initialContents mcDepth mcInit)
    (initialPortState mcPortA)
    (initialPortState mcPortB)
    Nothing
    Nothing
    edges
    insA0
    insB0
 where
  common = isCommonClock @domA @domB

  edges
    | common = P.map (0,) (P.repeat EdgeAB)
    | otherwise = clockEdges (snatToNum (clockPeriod @domA)) (snatToNum (clockPeriod @domB))

  collWindow = collisionWindow (snatToNum (clockPeriod @domA)) (snatToNum (clockPeriod @domB))

  canWriteA = mcMemoryType `P.elem` [SinglePortRam, SimpleDualPortRam, TrueDualPortRam, DualPortDistributedRam]
  canWriteB = mcMemoryType == TrueDualPortRam && mcPrimitive /= DistributedRam

  isUltra = mcPrimitive == UltraRam
  ultraTdp = isUltra && mcMemoryType == TrueDualPortRam
  ultraSdpWriteFirst =
    isUltra && mcMemoryType == SimpleDualPortRam && pmWriteMode mcPortB == WriteFirst

  -- Whether the XPM's simulation model makes collisions undefined. It does for
  -- block RAM. For independent clocks, we also do so for distributed RAM: the
  -- XPM's simulation model does not, but the hardware is undefined.
  modelsCollisions
    | common = mcPrimitive `P.elem` [AutoPrimitive, BlockRam]
    | otherwise = mcPrimitive /= UltraRam

  -- With a common clock and both ports in read first mode, reading an address
  -- the other port writes returns the old contents.
  collisionSafe =
       common
    && pmWriteMode mcPortA == ReadFirst
    && pmWriteMode mcPortB == ReadFirst

  sameAddress :: MaybeX Int -> MaybeX Int -> MaybeX Bool
  sameAddress a b = (==) <$> a <*> b

  -- Collision with the last write of the other port, for independent clocks
  windowCollision :: Integer -> Maybe (LastWrite w) -> MaybeX Int -> MaybeX Bool
  windowCollision _ Nothing _ = pure False
  windowCollision t (Just LastWrite{..}) addr
    | t - lwTime <= collWindow = sameAddress lwAddress addr
    | otherwise = pure False

  -- Make bits written by an earlier write undefined
  undefineWrite :: Maybe (LastWrite w) -> Seq (BitVector w) -> Seq (BitVector w)
  undefineWrite Nothing mem = mem
  undefineWrite (Just LastWrite{..}) mem =
    applyWrite (WriteReq (toMaybeX (errorX "collision")) lwAddress lwMask undefined#) mem

  lastWrite :: Integer -> WriteReq w -> Maybe (LastWrite w)
  lastWrite t WriteReq{..}
    | possibly wrActive = Just (LastWrite t wrAddress wrMask)
    | otherwise = Nothing

  go ::
    Seq (BitVector w) ->
    PortState w ->
    PortState w ->
    Maybe (LastWrite w) ->
    Maybe (LastWrite w) ->
    [(Integer, Edge)] ->
    Signal domA (PortIn w) ->
    Signal domB (PortIn w) ->
    (Signal domA (BitVector w), Signal domB (BitVector w))
  go _ _ _ _ _ [] _ _ = error "xpmMemoryModel: edges should be infinite"
  go mem0 sA0 sB0 lwA0 lwB0 ((t, edge):edges1) insA insB =
    case edge of
      EdgeA ->
        let
          (outA, mem1, sA1, lwA1) = single mcPortA canWriteA lwB0 inA sA0
          (outsA, outsB) = go mem1 sA1 sB0 lwA1 lwB0 edges1 insA1 insB
        in
          (outA :- outsA, outsB)
      EdgeB ->
        let
          (outB, mem1, sB1, lwB1) = single mcPortB canWriteB lwA0 inB sB0
          (outsA, outsB) = go mem1 sA0 sB1 lwA0 lwB1 edges1 insA insB1
        in
          (outsA, outB :- outsB)
      EdgeAB ->
        let
          (outA, outB, mem1, sA1, sB1, lwA1, lwB1) = both
          (outsA, outsB) = go mem1 sA1 sB1 lwA1 lwB1 edges1 insA1 insB1
        in
          (outA :- outsA, outB :- outsB)
   where
    ~(inA :- insA1) = insA
    ~(inB :- insB1) = insB

    -- Output of a port in the current cycle
    output cfg s portIn
      | pmLatency cfg == 0 = readWord mem0 (toMaybeX (piAddress portIn))
      | otherwise = portOutput cfg s portIn

    -- A single port's clock edge, only possible with independent clocks
    single cfg canWrite lwOther portIn s0 =
      (output cfg s0 portIn, mem2, s1, lastWrite t req1)
     where
      req0 = writeRequest canWrite portIn
      en = toMaybeX (piEnable portIn)
      addr = wrAddress req0
      collision
        | modelsCollisions = possibly (en `andX` windowCollision t lwOther addr)
        | otherwise = False
      writeCollision = collision && possibly (wrActive req0)
      req1 = if writeCollision then req0{wrData = undefined#} else req0
      mem1 = applyWrite req1 mem0
      mem2 = if writeCollision then undefineWrite lwOther mem1 else mem1
      newValue = readResult (pmWriteMode cfg) (readWord mem0 addr) req1 (psStage0 s0)
      s1 = stepPort cfg collision newValue portIn s0

    -- Both ports' clocks have an edge
    both = (output mcPortA sA0 inA, output mcPortB sB0 inB, mem3, sA1, sB1, lastWrite t reqA1, lastWrite t reqB1)
     where
      reqA0 = writeRequest canWriteA inA
      reqB0 = writeRequest canWriteB inB
      enA = toMaybeX (piEnable inA)
      enB = toMaybeX (piEnable inB)
      addrA = wrAddress reqA0
      addrB = wrAddress reqB0
      wrA = wrActive reqA0
      wrB = wrActive reqB0
      same = enA `andX` enB `andX` sameAddress addrA addrB

      -- Write/write collisions corrupt the data written by both ports
      writeWrite
        | modelsCollisions = possibly (same `andX` wrA `andX` wrB)
        | otherwise = False

      -- With independent clocks, also check the previous write of the other port
      windowA
        | common || not modelsCollisions = False
        | otherwise = possibly (enA `andX` windowCollision t lwB0 addrA)
      windowB
        | common || not modelsCollisions = False
        | otherwise = possibly (enB `andX` windowCollision t lwA0 addrB)

      reqA1 = if writeWrite || (windowA && possibly wrA) then reqA0{wrData = undefined#} else reqA0
      reqB1 = if writeWrite || (windowB && possibly wrB) then reqB0{wrData = undefined#} else reqB0

      memA = applyWrite reqA1 mem0
      memB = applyWrite reqB1 memA
      mem3 =
          (if windowA && possibly wrA then undefineWrite lwB0 else id)
        $ (if windowB && possibly wrB then undefineWrite lwA0 else id)
        $ memB

      -- Read/write collisions make the read data undefined
      forceA =
           windowA
        || (modelsCollisions && not collisionSafe && possibly
              (same `andX` ((notX wrA `andX` wrB) `orX` (wrA `andX` wrB))))
      forceB =
           windowB
        || (modelsCollisions && not collisionSafe && possibly
              (same `andX` ((wrA `andX` notX wrB) `orX` (wrA `andX` wrB))))

      -- UltraRAM in true dual port mode executes port A's operation before
      -- port B's. In simple dual port, write first mode, the read sees the
      -- write.
      memReadB
        | ultraTdp = memA
        | ultraSdpWriteFirst = memB
        | otherwise = mem0

      newA = readResult (pmWriteMode mcPortA) (readWord mem0 addrA) reqA1 (psStage0 sA0)
      newB = readResult (pmWriteMode mcPortB) (readWord memReadB addrB) reqB1 (psStage0 sB0)

      sA1 = stepPort mcPortA forceA newA inA sA0
      sB1 = stepPort mcPortB forceB newB inB sB0

------------------------------------------------------------------------------
-- Configuration checks
------------------------------------------------------------------------------

-- | Properties of a port, used by 'configError'
data PortCheck = PortCheck
  { pcName :: String
  , pcReads :: Bool
  , pcWrites :: Bool
  , pcLatency :: Int
  , pcWriteMode :: WriteMode
  , pcAsyncReset :: Bool
  , pcResetValueZero :: Bool
  , pcRisingEdge :: Bool
    -- | Number of bytes in the write enable
  , pcBytes :: Int
  }

-- | Check whether the XPM supports a configuration. Returns an error message if
-- it does not.
configError ::
  -- | Name of the function, for error messages
  String ->
  MemoryType ->
  MemoryPrimitive ->
  -- | Common clock
  Bool ->
  -- | Depth
  Int ->
  -- | Data width
  Int ->
  PortCheck ->
  PortCheck ->
  Maybe String
configError fName memType prim common depth width portA portB =
  case P.filter fst checks of
    ((_, msg):_) -> Just (fName <> ": " <> msg)
    [] -> Nothing
 where
  ports = [portA, portB]
  isUltra = prim == UltraRam
  checks =
    [ ( depth * width > 150994944
      , "memory too large, XPM_MEMORY supports up to 150994944 bits" )
    , ( isUltra && not common
      , "UltraRam requires both ports to be in the same domain" )
    , ( memType == TrueDualPortRam && prim == DistributedRam
      , "DistributedRam does not support two write ports, use xpmMemoryDpdistram" )
    , ( memType == SimpleDualPortRam && prim == BlockRam && pcWriteMode portB == WriteFirst
      , "BlockRam in simple dual port mode does not support WriteFirst" )
    , ( memType == SimpleDualPortRam && isUltra && pcWriteMode portB == NoChange
      , "UltraRam in simple dual port mode does not support NoChange" )
    , ( memType == SimpleDualPortRam && isUltra && pcWriteMode portB == WriteFirst && pcLatency portB < 3
      , "UltraRam in simple dual port, write first mode requires a read latency of at least 3" )
    , ( memType == SimpleDualPortRam && prim == DistributedRam && pcWriteMode portB /= ReadFirst
      , "DistributedRam requires ReadFirst" )
    ] <> P.concatMap portChecks ports

  portChecks PortCheck{..} =
    [ ( not pcRisingEdge
      , "port " <> pcName <> " requires a domain with a rising active edge" )
    , ( pcReads && pcLatency == 0 && prim /= DistributedRam
      , "port " <> pcName <> ": a read latency of 0 requires DistributedRam" )
    , ( pcWrites && prim == DistributedRam && pcWriteMode /= ReadFirst
      , "port " <> pcName <> ": DistributedRam requires ReadFirst" )
    , ( pcWrites && memType == TrueDualPortRam && isUltra && pcWriteMode /= NoChange
      , "port " <> pcName <> ": UltraRam in true dual port mode requires NoChange" )
    , ( pcReads && pcAsyncReset && not (prim `P.elem` [BlockRam, UltraRam])
      , "port " <> pcName <> ": asynchronous resets require BlockRam or UltraRam" )
    , ( pcReads && pcAsyncReset && not pcResetValueZero
      , "port " <> pcName <> ": asynchronous resets require a reset value of zero" )
    , ( pcReads && isUltra && not pcResetValueZero
      , "port " <> pcName <> ": UltraRam requires a reset value of zero" )
    , ( pcWrites && width `P.mod` pcBytes /= 0
      , "port " <> pcName <> ": the data width must be a multiple of the number of bytes" )
    , ( pcWrites && pcBytes > 1 && not (width `P.div` pcBytes `P.elem` [8, 9])
      , "port " <> pcName <> ": bytes must be 8 or 9 bits wide" )
    ]

------------------------------------------------------------------------------
-- Glue between the wrappers and the model
------------------------------------------------------------------------------

-- | Model configuration of a port
portModelConfig ::
  forall dom latency a.
  (KnownDomain dom, BitPack a) =>
  XpmMemoryPortConfig latency a ->
  PortModelConfig (BitSize a)
portModelConfig XpmMemoryPortConfig{..} = PortModelConfig
  { pmLatency = snatToNum readLatency
  , pmWriteMode = writeMode
  , pmResetValue = pack readResetValue
  , pmAsyncReset = isAsyncReset @dom
  }

-- | Model inputs of a port
portInputs ::
  forall dom depth nBytes a.
  ( KnownDomain dom
  , KnownNat depth
  , KnownNat nBytes
  , BitPack a ) =>
  Reset dom ->
  Enable dom ->
  -- | @regce@
  Signal dom Bool ->
  Signal dom (Index depth) ->
  Signal dom (BitVector nBytes) ->
  Signal dom a ->
  Signal dom (PortIn (BitSize a))
portInputs rst en regce addr we din =
  PortIn
    <$> fromEnable en
    <*> regce
    <*> unsafeToActiveHigh rst
    <*> fmap fromEnum addr
    <*> fmap expandByteEnable we
    <*> fmap pack din

-- | Properties of a port, for 'configError'
portCheck ::
  forall dom latency a.
  (KnownDomain dom, BitPack a) =>
  String ->
  -- | Reads
  Bool ->
  -- | Writes
  Bool ->
  -- | Number of bytes in write enable
  Int ->
  XpmMemoryPortConfig latency a ->
  PortCheck
portCheck name doesRead doesWrite nBytes XpmMemoryPortConfig{..} = PortCheck
  { pcName = name
  , pcReads = doesRead
  , pcWrites = doesWrite
  , pcLatency = snatToNum readLatency
  , pcWriteMode = writeMode
  , pcAsyncReset = isAsyncReset @dom
  , pcResetValueZero = pack readResetValue == 0
  , pcRisingEdge = isRisingEdge @dom
  , pcBytes = nBytes
  }
