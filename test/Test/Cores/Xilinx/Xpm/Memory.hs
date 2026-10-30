{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Tests for the simulation model of the @XPM_MEMORY@ wrappers. The HDL tests
-- compare the model with Vivado's simulation of the XPMs; these tests compare
-- it with a straightforward reference implementation and test helpers.
module Test.Cores.Xilinx.Xpm.Memory where

import Clash.Explicit.Prelude hiding (someNatVal)
import Clash.Sized.Internal.BitVector (undefined#)

import GHC.TypeNats (someNatVal)
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import Clash.Cores.Xilinx.Xpm.Memory
import Clash.Cores.Xilinx.Xpm.Memory.Internal

import qualified Data.Foldable as F
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P

createDomain vXilinxSystem{vName="Fast", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="Slow", vPeriod=hzToPeriod 30e6}

type Depth = 16

-- | Operation of a port in a single cycle: enable, regce, reset, address,
-- byte enable, data
type Op = (Bool, Bool, Bool, Index Depth, BitVector 2, BitVector 16)

genOp :: Index Depth -> Index Depth -> Gen Op
genOp lo hi =
  (,,,,,)
    <$> Gen.frequency [(3, pure True), (1, pure False)]
    <*> Gen.frequency [(5, pure True), (1, pure False)]
    <*> Gen.frequency [(1, pure True), (12, pure False)]
    <*> Gen.integral (Range.constant lo hi)
    <*> Gen.element [0, 0, 1, 2, 3]
    <*> (fromIntegral <$> Gen.integral (Range.constant (0 :: Int) 0xFFFF))

genOps :: Index Depth -> Index Depth -> Gen [Op]
genOps lo hi = Gen.list (Range.singleton 100) (genOp lo hi)

genWriteMode :: Gen WriteMode
genWriteMode = Gen.element [WriteFirst, ReadFirst, NoChange]

-- | Reference implementation of a single port of a memory, written with plain
-- Clash registers. The memory contents are the port's own, i.e., this only
-- matches a dual port memory if the ports access disjoint addresses.
reference ::
  forall dom.
  KnownDomain dom =>
  -- | Read latency
  Int ->
  WriteMode ->
  -- | Reset value
  BitVector 16 ->
  Clock dom ->
  Signal dom Op ->
  Signal dom (BitVector 16)
reference latency mode rstVal clk ops = out
 where
  (en, regce, rst, addr, we, din) = unbundle ops
  bitEna = expandByteEnable @2 @16 <$> we

  mem :: Signal dom (Vec Depth (BitVector 16))
  mem = register clk noReset enableGen (repeat 0) (write <$> mem <*> en <*> addr <*> bitEna <*> din)
  write m e a be d
    | e = replace a ((m !! a .&. complement be) .|. (d .&. be)) m
    | otherwise = m

  memWord = (!!) <$> mem <*> addr
  newWord = (\w be d -> (w .&. complement be) .|. (d .&. be)) <$> memWord <*> bitEna <*> din

  -- Value of the read register when enabled, given its current value
  readValue :: Signal dom (BitVector 16) -> Signal dom (BitVector 16)
  readValue current = case mode of
    ReadFirst -> memWord
    WriteFirst -> newWord
    NoChange -> mux ((/= 0) <$> we) current memWord

  stage0Init = if mode /= NoChange then 0 else rstVal
  stage0 = register clk noReset enableGen stage0Init (mux en (readValue stage0) stage0)

  out
    | latency == 1 =
        let o = register clk noReset enableGen rstVal (mux rst (pure rstVal) (mux en (readValue o) o))
        in o
    | otherwise = pipeline (latency - 1) stage0 en

  pipeline :: Int -> Signal dom (BitVector 16) -> Signal dom Bool -> Signal dom (BitVector 16)
  pipeline 1 prev _ =
    let o = register clk noReset enableGen rstVal (mux rst (pure rstVal) (mux regce prev o))
    in o
  pipeline n prev ena =
    let
      ena1 = register clk noReset enableGen False ena
      o = register clk noReset enableGen (if mode == NoChange then rstVal else 0) (mux ena1 prev o)
    in
      pipeline (n - 1) o ena1

-- | Run a true dual port RAM on two operation streams
tdpram ::
  forall domA domB.
  (KnownDomain domA, KnownDomain domB) =>
  MemoryPrimitive ->
  (Int, WriteMode) ->
  (Int, WriteMode) ->
  [Op] ->
  [Op] ->
  ([BitVector 16], [BitVector 16])
tdpram prim (latA, modeA) (latB, modeB) opsA opsB =
  case (someNatVal (fromIntegral latA), someNatVal (fromIntegral latB)) of
    (SomeNat (_ :: proxy latA), SomeNat (_ :: proxy latB)) ->
      let
        (outA, outB) =
          xpmMemoryTdpramWith
            defaultXpmMemoryConfig{memoryPrimitive = prim}
            (XpmMemoryPortConfig (SNat @latA) modeA (resetValueA prim))
            (XpmMemoryPortConfig (SNat @latB) modeB (resetValueB prim))
            clkA (rstOf insA) (enOf insA) (regceOf insA) (addrOf insA) (weOf insA) (dinOf insA)
            clkB (rstOf insB) (enOf insB) (regceOf insB) (addrOf insB) (weOf insB) (dinOf insB)
        insA = fromList (opsA <> P.repeat idle)
        insB = fromList (opsB <> P.repeat idle)
      in
        (sampleN (P.length opsA) outA, sampleN (P.length opsB) outB)
 where
  clkA = clockGen @domA
  clkB = clockGen @domB
  idle = (False, True, False, 0, 0, 0)

-- | Reset values used in the tests. UltraRAM only supports zero.
resetValueA, resetValueB :: MemoryPrimitive -> BitVector 16
resetValueA prim = if prim == UltraRam then 0 else 0xAAAA
resetValueB prim = if prim == UltraRam then 0 else 0xBBBB

rstOf :: KnownDomain dom => Signal dom Op -> Reset dom
rstOf = unsafeFromActiveHigh . fmap (\(_, _, r, _, _, _) -> r)

enOf :: KnownDomain dom => Signal dom Op -> Enable dom
enOf = toEnable . fmap (\(e, _, _, _, _, _) -> e)

regceOf :: Signal dom Op -> Signal dom Bool
regceOf = fmap (\(_, c, _, _, _, _) -> c)

addrOf :: Signal dom Op -> Signal dom (Index Depth)
addrOf = fmap (\(_, _, _, a, _, _) -> a)

weOf :: Signal dom Op -> Signal dom (BitVector 2)
weOf = fmap (\(_, _, _, _, w, _) -> w)

dinOf :: Signal dom Op -> Signal dom (BitVector 16)
dinOf = fmap (\(_, _, _, _, _, d) -> d)

-- | Ports accessing disjoint addresses behave like independent memories, which
-- behave like the reference implementation.
disjointPorts ::
  forall domA domB.
  (KnownDomain domA, KnownDomain domB) =>
  MemoryPrimitive ->
  Property
disjointPorts prim = property $ do
  latA <- forAll (Gen.int (Range.constant 1 4))
  latB <- forAll (Gen.int (Range.constant 1 4))
  modeA <- forAll (if prim == UltraRam then pure NoChange else genWriteMode)
  modeB <- forAll (if prim == UltraRam then pure NoChange else genWriteMode)
  opsA <- forAll (genOps 0 7)
  opsB <- forAll (genOps 8 15)
  let
    (outA, outB) = tdpram @domA @domB prim (latA, modeA) (latB, modeB) opsA opsB
    refA = sampleN (P.length opsA) (reference latA modeA (resetValueA prim) (clockGen @domA) (fromList (opsA <> P.repeat idle)))
    refB = sampleN (P.length opsB) (reference latB modeB (resetValueB prim) (clockGen @domB) (fromList (opsB <> P.repeat idle)))
    idle = (False, True, False, 0, 0, 0)
  outA === refA
  outB === refB

-- | A single port RAM behaves like the reference implementation
singlePort :: Property
singlePort = property $ do
  lat <- forAll (Gen.int (Range.constant 1 4))
  mode <- forAll genWriteMode
  ops <- forAll (genOps 0 15)
  case someNatVal (fromIntegral lat) of
    SomeNat (_ :: proxy lat) -> do
      let
        clk = clockGen @XilinxSystem
        ins = fromList (ops <> P.repeat (False, True, False, 0, 0, 0))
        out =
          xpmMemorySpramWith
            defaultXpmMemoryConfig
            (XpmMemoryPortConfig (SNat @lat) mode 0x1234)
            clk (rstOf ins) (enOf ins) (regceOf ins) (addrOf ins) (weOf ins) (dinOf ins)
        ref = reference lat mode 0x1234 clk ins
      sampleN (P.length ops) out === sampleN (P.length ops) ref

-- | Reading an address the other port writes at the same time is a collision,
-- unless both ports use 'ReadFirst'
collision :: Assertion
collision = do
  let
    write = (True, True, False, 3, 3, 0x1111)
    readOp = (True, True, False, 3, 0, 0)
    idle = (False, True, False, 0, 0, 0)
    run modeA modeB = tdpram @XilinxSystem @XilinxSystem BlockRam (1, modeA) (1, modeB) [idle, write, idle, idle] [idle, readOp, readOp, idle]
    (_, outB0) = run WriteFirst WriteFirst
    (_, outB1) = run ReadFirst ReadFirst
  -- Collision: undefined
  assertBool "collision should be undefined" (hasUndefined (outB0 P.!! 2))
  -- Read after the collision: new data
  outB0 P.!! 3 @?= 0x1111
  -- Collision safe: old data
  outB1 P.!! 2 @?= 0
  outB1 P.!! 3 @?= 0x1111

-- | Two ports writing the same address at the same time corrupt the memory
writeWriteCollision :: Assertion
writeWriteCollision = do
  let
    write d = (True, True, False, 5, 1, d)
    readOp = (True, True, False, 5, 0, 0)
    idle = (False, True, False, 0, 0, 0)
    (outA, _) =
      tdpram @XilinxSystem @XilinxSystem BlockRam (1, ReadFirst) (1, ReadFirst)
        [idle, write 0x00AA, readOp, idle] [idle, write 0x0055, idle, idle]
  -- Only the low byte was written, so only that byte is undefined
  show (outA P.!! 3) @?= show ((0 :: BitVector 8) ++# (undefined# :: BitVector 8))

-- | 'parseMemFile' parses what 'xpmMemFile' renders, and handles comments and
-- addresses
memFile :: Assertion
memFile = do
  let
    contents = [0x00001, 0x3FFFF, 0x12345] :: [BitVector 18]
  F.toList (parseMemFile @18 3 (xpmMemFile contents)) @?= contents
  show (F.toList (parseMemFile @8 4 "// comment\nAB /* block\ncomment */ cd\n@3 0_1"))
    @?= show [0xAB, 0xCD, undefined#, 0x01 :: BitVector 8]

-- | Unsupported configurations are rejected
configErrors :: Assertion
configErrors = do
  let
    port name latency mode = PortCheck
      { pcName = name
      , pcReads = True
      , pcWrites = True
      , pcLatency = latency
      , pcWriteMode = mode
      , pcAsyncReset = False
      , pcResetValueZero = True
      , pcRisingEdge = True
      , pcBytes = 1
      }
    chk prim common pA pB = configError "f" TrueDualPortRam prim common 16 16 pA pB
  chk BlockRam False (port "A" 1 NoChange) (port "B" 1 NoChange) @?= Nothing
  assertBool "latency 0" (isJust (chk BlockRam True (port "A" 0 NoChange) (port "B" 1 NoChange)))
  assertBool "UltraRAM independent clocks" (isJust (chk UltraRam False (port "A" 1 NoChange) (port "B" 1 NoChange)))
  assertBool "UltraRAM read first" (isJust (chk UltraRam True (port "A" 1 ReadFirst) (port "B" 1 NoChange)))
  assertBool "byte width" (isJust (chk BlockRam True (port "A" 1 NoChange){pcBytes = 4} (port "B" 1 NoChange)))
 where
  isJust = maybe False (const True)

tests :: TestTree
tests = testGroup "Xpm.Memory"
  [ testPropertyNamed "single port matches reference" "singlePort" singlePort
  , testPropertyNamed "disjoint ports, common clock, block RAM" "disjointCommonBlock"
      (disjointPorts @XilinxSystem @XilinxSystem BlockRam)
  , testPropertyNamed "disjoint ports, common clock, UltraRAM" "disjointCommonUltra"
      (disjointPorts @XilinxSystem @XilinxSystem UltraRam)
  , testPropertyNamed "disjoint ports, independent clocks" "disjointIndependent"
      (disjointPorts @Fast @Slow BlockRam)
  , testCase "read/write collision" collision
  , testCase "write/write collision" writeWriteCollision
  , testCase "memory file" memFile
  , testCase "configuration errors" configErrors
  ]
