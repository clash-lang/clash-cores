{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the simulation models in "Clash.Cores.Xilinx.Floating"
  against the bit-accurate C model of the Xilinx Floating-Point LogiCORE IP
  v7.1. Covers addition, subtraction, multiplication, division, comparison
  (condition code), and conversion from @Unsigned 32@ and @Signed 32@, all in
  single precision with round to nearest even, the configuration the module
  instantiates. The tests run the actual functions from
  "Clash.Cores.Xilinx.Floating.Explicit" in Clash simulation and compare the
  bits of their output with the output of the C model.

  = Running the tests

  These tests are only part of the unit tests if the Cabal flag
  @xilinx-floating-cmodel@ is enabled. It is disabled by default, and enabled
  on CI. Xilinx's C model cannot be redistributed, so the tests locate it at
  run time:

  1. If @CLASH_CORES_XILINX_FLOATING_CMODEL@ is set, it should point at either
     @floating_point_v7_1_bitacc_cmodel_lin64.zip@ (Vivado ships it in
     @data\/ip\/xilinx\/floating_point_v7_1\/cmodel@) or a directory holding its
     extracted contents.

  2. Otherwise, if @XILINX_VIVADO@ is set (by Vivado's @settings64.sh@), the
     zip file is taken from that installation.

  If neither is set, or the C model is not where they point, the tests fail.

  = Known mismatches

  The C model rounds an exact result to 24 significant bits first, and only
  then flushes it to zero if its magnitude is below the smallest normal number,
  2^-126. The Haskell models compute an IEEE result, rounding to the
  /subnormal/ grid near zero, and flush subnormal results afterwards. The two
  disagree when the exact result's magnitude lies in

  > [2^-126 - 2^-150, 2^-126 - 2^-151)

  In that window the Haskell models round up to the smallest normal number,
  @±0x00800000@, while the C model returns a zero of the same sign,
  @±0x00000000@. Results of 'F.add' and 'F.sub' never land in the window,
  because sums of normal numbers are exact multiples of 2^-149 there. Products
  can: for example @0x3f7fffff * 0x00800000@ (exact result 2^-126 - 2^-150).
  Quotients land in it only when the dividend's significand is all ones and the
  divisor is a power of two, for example @0x3fffffff / 0x7f000000@. The tests
  check that inputs in the window give exactly this mismatch, and equal results
  everywhere else. They do not change the models.
-}
module Test.Cores.Xilinx.Floating (tests) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, catch, onException, try)
import Control.Monad (unless, void, when)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Int (Int32)
import Data.String (fromString)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable, peek)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import Numeric (showHex)
import System.Directory
  ( createDirectory, doesDirectoryExist, doesFileExist, makeAbsolute
  , removeDirectoryRecursive )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (arch, os)
import System.Process (readProcessWithExitCode)

import Clash.Explicit.Prelude (Clock, Enable, KnownNat, System, natToNum)
import qualified Clash.Explicit.Prelude as C
import Clash.Signal.Delayed (DSignal)
import qualified Clash.Signal.Delayed as D

import qualified Clash.Cores.Xilinx.Floating.Explicit as F

import Hedgehog (Gen, Property, PropertyT, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

-- | Name of the environment variable pointing at the C model
cmodelEnvVar :: String
cmodelEnvVar = "CLASH_CORES_XILINX_FLOATING_CMODEL"

-- | Location of the zip file in a Vivado installation
cmodelZipInVivado :: FilePath
cmodelZipInVivado =
  "data/ip/xilinx/floating_point_v7_1/cmodel/floating_point_v7_1_bitacc_cmodel_lin64.zip"

-- | Number of random cases per property, unless overridden by
-- @--hedgehog-tests@.
nTests :: H.TestLimit
nTests = 20_000

-- | Number of random cases for the properties checking the known mismatch
nTestsKnownMismatch :: H.TestLimit
nTestsKnownMismatch = 2_000

tests :: TestTree
tests =
  withResource loadCModel unloadCModel $ \getCModel ->
    testGroup "Xilinx floating point vs. C model"
      [ binaryTest getCModel Add
      , binaryTest getCModel Sub
      , binaryTest getCModel Mul
      , binaryTest getCModel Div
      , testPropertyNamed "compare" "prop_compare" (prop_compare getCModel)
      , testPropertyNamed "fromU32" "prop_fromU32" (prop_fromU32 getCModel)
      , testPropertyNamed "fromS32" "prop_fromS32" (prop_fromS32 getCModel)
      , knownMismatchTest getCModel Mul genMulWindow
      , knownMismatchTest getCModel Div genDivWindow
      ]
 where
  binaryTest getCModel op =
    testPropertyNamed
      (opName op)
      (fromString ("prop_" <> opName op))
      (H.withTests nTests (prop_binary getCModel op (genBinaryOperands op) labels))
   where
    labels x y exact = do
      classifyOperands x y
      H.classify "exact result near smallest normal number" (maybe False nearMinNormal exact)
      H.classify "exact result in known mismatch window" (maybe False inKnownMismatchWindow exact)
    nearMinNormal r = abs r >= 2 ^^ (-128 :: Int) && abs r < 2 ^^ (-124 :: Int)

  knownMismatchTest getCModel op gen =
    testPropertyNamed
      ( opName op
          <> ", known mismatch: exact result in [2^-126 - 2^-150, 2^-126 - 2^-151)"
      )
      (fromString ("prop_" <> opName op <> "KnownMismatch"))
      (H.withTests nTestsKnownMismatch (prop_binary getCModel op gen labels))
   where
    -- Check the generator
    labels _ _ exact =
      H.cover 100 "exact result in known mismatch window" (maybe False inKnownMismatchWindow exact)

-- | Find the C model, see the module documentation. Returns a path to the zip
-- file or directory.
findCModel :: IO FilePath
findCModel
  | os /= "linux" || arch /= "x86_64" =
      fail "The C model is only available for x86_64 Linux."
  | otherwise = do
      explicit <- lookupEnv cmodelEnvVar
      vivado <- lookupEnv "XILINX_VIVADO"
      case (explicit, vivado) of
        (Just path, _) | not (null path) -> pure path
        (_, Just dir) | not (null dir) -> do
          let path = dir </> cmodelZipInVivado
          exists <- doesFileExist path
          unless exists $
            fail ("XILINX_VIVADO is set, but " <> path <> " does not exist.")
          pure path
        _ ->
          fail $
            "Set " <> cmodelEnvVar <> " to floating_point_v7_1_bitacc_cmodel_lin64.zip"
              <> " (or a directory with its extracted contents), or set XILINX_VIVADO."
              <> " See test/Test/Cores/Xilinx/Floating.hs."

-- | @xip_fpo_exc_t f(float *, float, float)@, for example @xip_fpo_add_flt@
type BinaryFlt = Ptr Float -> Float -> Float -> IO CInt

-- | @xip_fpo_exc_t f(int *, float, float)@, for example @xip_fpo_condcode_flt@
type CondCodeFlt = Ptr CInt -> Float -> Float -> IO CInt

-- | @xip_fpo_exc_t f(float *, a)@, for example @xip_fpo_fixtoflt_flt_int@
type FromIntFlt a = Ptr Float -> a -> IO CInt

foreign import ccall "dynamic" mkBinaryFlt :: FunPtr BinaryFlt -> BinaryFlt
foreign import ccall "dynamic" mkCondCodeFlt :: FunPtr CondCodeFlt -> CondCodeFlt
foreign import ccall "dynamic" mkFromS32Flt :: FunPtr (FromIntFlt CInt) -> FromIntFlt CInt
foreign import ccall "dynamic" mkFromU32Flt :: FunPtr (FromIntFlt CUInt) -> FromIntFlt CUInt

foreign import ccall unsafe "dlfcn.h dlopen" c_dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dlfcn.h dlsym" c_dlsym :: Ptr () -> CString -> IO (FunPtr a)
foreign import ccall unsafe "dlfcn.h dlclose" c_dlclose :: Ptr () -> IO CInt
foreign import ccall unsafe "dlfcn.h dlerror" c_dlerror :: IO CString

-- | Flags for 'c_dlopen', from glibc's @dlfcn.h@. The @unix@ package does not
-- offer @RTLD_DEEPBIND@.
rtldNow, rtldDeepBind :: CInt
rtldNow = 0x2
rtldDeepBind = 0x8

-- | Load a shared library, see the module documentation
loadLibrary :: FilePath -> IO (Ptr ())
loadLibrary path = do
  handle <- withCString path $ \cPath -> c_dlopen cPath (rtldNow .|. rtldDeepBind)
  when (handle == nullPtr) $ dlFail ("Could not load " <> path)
  pure handle

-- | Look up a function in a shared library
lookupFunction :: Ptr () -> String -> IO (FunPtr a)
lookupFunction handle name = do
  f <- withCString name (c_dlsym handle)
  when (f == nullFunPtr) $ dlFail ("Could not find " <> name <> " in the C model")
  pure f

dlFail :: String -> IO a
dlFail msg = do
  err <- c_dlerror
  reason <- if err == nullPtr then pure "" else (": " <>) <$> peekCString err
  fail (msg <> reason)

-- | The C model, loaded into the test process
data CModel = CModel
  { cmodelTmpDir :: FilePath
  , cmodelLibraries :: [Ptr ()]
  , cmodelLock :: MVar ()
  , cmodelAdd :: BinaryFlt
  , cmodelSub :: BinaryFlt
  , cmodelMul :: BinaryFlt
  , cmodelDiv :: BinaryFlt
  , cmodelCondCode :: CondCodeFlt
  , cmodelFromU32 :: FromIntFlt CUInt
  , cmodelFromS32 :: FromIntFlt CInt
  }

-- | Find the C model, prepare it and load it
loadCModel :: IO CModel
loadCModel = do
  path <- findCModel
  tmpRoot <- getCanonicalTemporaryDirectory
  tmp <- createTempDirectory tmpRoot "clash-cores-xilinx-floating"
  loadCModelIn tmp path `onException` removeDirectoryRecursive tmp

loadCModelIn :: FilePath -> FilePath -> IO CModel
loadCModelIn tmp path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  modelDir <-
    if
      | isDir -> makeAbsolute path
      | isFile -> do
          let dir = tmp </> "cmodel"
          createDirectory dir
          extractZip dir
          pure dir
      | otherwise ->
          fail (path <> " is neither a file nor a directory")

  -- In the order of their dependencies, see the module documentation
  gmp <- loadLibrary (modelDir </> "libgmp.so.11")
  mpfr <- loadLibrary (modelDir </> "libmpfr.so.4")
  lib <- loadLibrary (modelDir </> "libIp_floating_point_v7_1_bitacc_cmodel.so")

  cmodelAdd <- mkBinaryFlt <$> lookupFunction lib "xip_fpo_add_flt"
  cmodelSub <- mkBinaryFlt <$> lookupFunction lib "xip_fpo_sub_flt"
  cmodelMul <- mkBinaryFlt <$> lookupFunction lib "xip_fpo_mul_flt"
  cmodelDiv <- mkBinaryFlt <$> lookupFunction lib "xip_fpo_div_flt"
  cmodelCondCode <- mkCondCodeFlt <$> lookupFunction lib "xip_fpo_condcode_flt"
  cmodelFromU32 <- mkFromU32Flt <$> lookupFunction lib "xip_fpo_fixtoflt_flt_uint"
  cmodelFromS32 <- mkFromS32Flt <$> lookupFunction lib "xip_fpo_fixtoflt_flt_int"
  cmodelLock <- newMVar ()
  pure
    CModel
      { cmodelTmpDir = tmp
      , cmodelLibraries = [lib, mpfr, gmp]
      , cmodelLock
      , cmodelAdd
      , cmodelSub
      , cmodelMul
      , cmodelDiv
      , cmodelCondCode
      , cmodelFromU32
      , cmodelFromS32
      }
 where
  extractZip dir = do
    let args = ["-q", path, "-d", dir]
    (code, out, err) <-
      readProcessWithExitCode "unzip" args "" `catch` \(e :: IOException) ->
        fail $
          show e
            <> "\nExtracting the zip file requires unzip. Alternatively, set "
            <> cmodelEnvVar <> " to a directory with its extracted contents."
    case code of
      ExitSuccess -> pure ()
      ExitFailure n ->
        fail $
          unlines
            [ "Command failed with exit code " <> show n <> ": " <> unwords ("unzip" : args)
            , out
            , err
            ]

unloadCModel :: CModel -> IO ()
unloadCModel CModel{cmodelTmpDir, cmodelLibraries} = do
  mapM_ c_dlclose cmodelLibraries
  void (try @SomeException (removeDirectoryRecursive cmodelTmpDir))

-- | Call a function of the C model that writes its result to the given pointer.
-- Returns the result and the exception flags (bit 0: underflow, bit 1:
-- overflow, bit 2: invalid operation, bit 3: divide by zero, bit 4: operation
-- not supported); they are informational only.
callCModel :: Storable a => CModel -> (Ptr a -> IO CInt) -> IO (a, Int)
callCModel CModel{cmodelLock} f =
  withMVar cmodelLock $ \() ->
    alloca $ \resultPtr -> do
      exceptions <- f resultPtr
      result <- peek resultPtr
      pure (result, fromIntegral exceptions)

-- | Run a binary operation on the C model
cmodelBinary :: CModel -> BinOp -> Float -> Float -> IO (Float, Int)
cmodelBinary cmodel op x y = callCModel cmodel (\result -> f result x y)
 where
  f = case op of
    Add -> cmodelAdd cmodel
    Sub -> cmodelSub cmodel
    Mul -> cmodelMul cmodel
    Div -> cmodelDiv cmodel

-- | Float shown by its bit pattern and its value
newtype FloatBits = FloatBits Word32
  deriving (Eq)

instance Show FloatBits where
  show (FloatBits w) = showBits w <> " (" <> show (castWord32ToFloat w) <> ")"

showBits :: Word32 -> String
showBits w = "0x" <> pad (showHex w "")
 where
  pad s = replicate (8 - length s) '0' <> s

floatBits :: Float -> FloatBits
floatBits = FloatBits . castFloatToWord32

bitsFloat :: FloatBits -> Float
bitsFloat (FloatBits w) = castWord32ToFloat w

data BinOp = Add | Sub | Mul | Div
  deriving (Eq, Show)

-- | Name of the operation, used in the names of the tests
opName :: BinOp -> String
opName = \case
  Add -> "add"
  Sub -> "sub"
  Mul -> "mul"
  Div -> "div"

-- | Output of a pipelined operator, run on constant inputs, once its pipeline
-- has filled up.
afterPipeline :: forall d a. KnownNat d => DSignal System d a -> a
afterPipeline s = C.sample_lazy (D.toSignal s) !! natToNum @d

-- | Constant input, at delay 0
constant :: a -> DSignal System 0 a
constant = pure

clk :: Clock System
clk = C.clockGen

en :: Enable System
en = C.enableGen

-- | The Haskell model under test: the simulation of the Clash function
haskellBinary :: BinOp -> Float -> Float -> Float
haskellBinary op x y = case op of
  Add -> afterPipeline (F.add clk en (constant x) (constant y))
  Sub -> afterPipeline (F.sub clk en (constant x) (constant y))
  Mul -> afterPipeline (F.mul clk en (constant x) (constant y))
  Div -> afterPipeline (F.div clk en (constant x) (constant y))

-- | Exact result of an operation on finite operands, after flushing subnormal
-- operands to zero like the IP does.
exactResult :: BinOp -> Float -> Float -> Maybe Rational
exactResult op (flushSubnormal -> x) (flushSubnormal -> y)
  | any (\z -> isNaN z || isInfinite z) [x, y] = Nothing
  | op == Div && y == 0 = Nothing
  | otherwise = Just (f (toRational x) (toRational y))
 where
  f = case op of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> (/)

-- | Flush subnormal numbers to zero (of either sign: we only use it to compare
-- values).
flushSubnormal :: Float -> Float
flushSubnormal z = if isDenormalized z then 0 else z

{- | Whether the magnitude of an exact result lies in
@[2^-126 - 2^-150, 2^-126 - 2^-151)@, the window in which the Haskell models
and the C model are known to disagree. See the module documentation.
-}
inKnownMismatchWindow :: Rational -> Bool
inKnownMismatchWindow r = lo <= abs r && abs r < hi
 where
  lo = 2 ^^ (-126 :: Int) - 2 ^^ (-150 :: Int)
  hi = 2 ^^ (-126 :: Int) - 2 ^^ (-151 :: Int)

prop_binary ::
  IO CModel ->
  BinOp ->
  Gen (FloatBits, FloatBits) ->
  -- | Labels for the coverage report, given the operands and the exact result
  (Float -> Float -> Maybe Rational -> PropertyT IO ()) ->
  Property
prop_binary getCModel op genOperands labels = H.property $ do
  model <- H.evalIO getCModel
  (a, b) <- H.forAll genOperands
  let
    x = bitsFloat a
    y = bitsFloat b
    exact = exactResult op x y
  labels x y exact

  (cResult, exceptions) <- H.evalIO (cmodelBinary model op x y)
  let
    haskell = floatBits (haskellBinary op x y)
    cmodel = floatBits cResult
  H.footnote ("C model exception flags: " <> show exceptions)

  case exact of
    Just r | inKnownMismatchWindow r -> do
      -- Known mismatch, see module documentation. Check that it is exactly
      -- what we expect, so the test notices when either side changes.
      let negative = r < 0
      H.footnote "Exact result in [2^-126 - 2^-150, 2^-126 - 2^-151): known mismatch"
      cmodel === FloatBits (if negative then 0x8000_0000 else 0x0000_0000)
      haskell === FloatBits (if negative then 0x8080_0000 else 0x0080_0000)
    _ ->
      haskell === cmodel

{- | Labels for the coverage report. We do not use 'H.cover' here: it fails
the test when the percentages are not met, which happens when running only a
few tests with @--hedgehog-tests@.
-}
classifyOperands :: Monad m => Float -> Float -> PropertyT m ()
classifyOperands x y = do
  H.classify "an operand is zero" (any isZero [x, y])
  H.classify "an operand is subnormal" (any isDenormalized [x, y])
  H.classify "an operand is infinite" (any isInfinite [x, y])
  H.classify "an operand is NaN" (any isNaN [x, y])
  H.classify "both operands are normal" (all isNormal [x, y])
 where
  isZero z = z == 0
  isNormal z = not (isZero z || isDenormalized z || isInfinite z || isNaN z)

prop_compare :: IO CModel -> Property
prop_compare getCModel = H.withTests nTests $ H.property $ do
  model <- H.evalIO getCModel
  (a, b) <- H.forAll genCompareOperands
  let
    x = bitsFloat a
    y = bitsFloat b
  classifyOperands x y
  H.classify "operands equal after flushing subnormals" $
    not (isNaN x || isNaN y) && flushSubnormal x == flushSubnormal y

  (cmodel, exceptions) <-
    H.evalIO (callCModel model (\result -> cmodelCondCode model result x y))
  let haskell = afterPipeline (F.compare clk en (constant x) (constant y))
  H.footnote ("C model exception flags: " <> show exceptions)
  H.footnote ("Haskell model: " <> show haskell)
  -- The IP's condition code goes through 'unpack', so compare bits
  fromIntegral (C.pack haskell) === cmodel

prop_fromU32 :: IO CModel -> Property
prop_fromU32 getCModel = H.withTests nTests $ H.property $ do
  model <- H.evalIO getCModel
  w <- H.forAll genWord32
  (cmodel, _) <-
    H.evalIO (callCModel model (\result -> cmodelFromU32 model result (fromIntegral w)))
  let haskell = afterPipeline (F.fromU32 clk en (constant (fromIntegral w)))
  floatBits haskell === floatBits cmodel

prop_fromS32 :: IO CModel -> Property
prop_fromS32 getCModel = H.withTests nTests $ H.property $ do
  model <- H.evalIO getCModel
  i <- H.forAll genInt32
  (cmodel, _) <-
    H.evalIO (callCModel model (\result -> cmodelFromS32 model result (fromIntegral i)))
  let haskell = afterPipeline (F.fromS32 clk en (constant (fromIntegral i)))
  floatBits haskell === floatBits cmodel

------------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------------

-- | Float with the given sign, biased exponent and fraction
mkFloat :: Bool -> Word32 -> Word32 -> FloatBits
mkFloat s e m =
  FloatBits ((if s then 0x8000_0000 else 0) .|. (e `shiftL` 23) .|. (m .&. 0x7f_ffff))

-- | Fraction (stored significand bits), biased towards edge cases
genFraction :: Gen Word32
genFraction =
  Gen.frequency
    [ (6, Gen.word32 (Range.constant 0 0x7f_ffff))
    , (1, Gen.element [0, 1, 2, 0x40_0000, 0x40_0001, 0x3f_ffff, 0x7f_fffe, 0x7f_ffff])
    , (1, Gen.word32 (Range.constant 0 16))
    , (1, (0x7f_ffff -) <$> Gen.word32 (Range.constant 0 16))
    ]

genZero, genSubnormal, genNormal, genInfinity, genNaN :: Gen FloatBits
genZero = mkFloat <$> Gen.bool <*> pure 0 <*> pure 0
genSubnormal = mkFloat <$> Gen.bool <*> pure 0 <*> Gen.filter (/= 0) genFraction
genNormal = mkFloat <$> Gen.bool <*> genExponent <*> genFraction
 where
  genExponent =
    Gen.frequency
      [ (4, Gen.word32 (Range.constant 1 254))
      , (1, Gen.word32 (Range.constant 1 30))
      , (1, Gen.word32 (Range.constant 225 254))
      , (1, Gen.word32 (Range.constant 100 154))
      ]
genInfinity = mkFloat <$> Gen.bool <*> pure 255 <*> pure 0
genNaN = mkFloat <$> Gen.bool <*> pure 255 <*> genPayload
 where
  genPayload =
    Gen.choice
      [ -- Quiet NaNs
        (0x40_0000 .|.) <$> Gen.word32 (Range.constant 0 0x3f_ffff)
      , -- Signaling NaNs
        Gen.word32 (Range.constant 1 0x3f_ffff)
      , Gen.element [0x40_0000, 0x7f_ffff, 1, 0x20_0000]
      ]

-- | Float of any class, both signs
genFloat :: Gen FloatBits
genFloat =
  Gen.frequency
    [ (1, genZero)
    , (2, genSubnormal)
    , (6, genNormal)
    , (1, genInfinity)
    , (2, genNaN)
    , (2, FloatBits <$> Gen.word32 Range.constantBounded)
    ]

-- | Float with an unbiased exponent in the given range, clamped to the normal
-- range
genNormalWithExponent :: Int -> Int -> Gen FloatBits
genNormalWithExponent lo hi = do
  e <- Gen.int (Range.constant lo hi)
  mkFloat <$> Gen.bool <*> pure (fromIntegral (max 1 (min 254 (e + 127)))) <*> genFraction

-- | Pair of floats that are related: equal, negated, adjacent or of similar
-- magnitude. Exercises cancellation and rounding.
genRelated :: Gen (FloatBits, FloatBits)
genRelated = do
  FloatBits a <- genFloat
  let e = fromIntegral ((a `shiftR` 23) .&. 0xff) :: Int
  b <-
    Gen.choice
      [ pure (FloatBits a)
      , pure (FloatBits (a `xor` 0x8000_0000))
      , (\d -> FloatBits (a + d)) <$> Gen.word32 (Range.constant 1 4)
      , (\d -> FloatBits (a - d)) <$> Gen.word32 (Range.constant 1 4)
      , genNormalWithExponent (e - 127 - 26) (e - 127 + 26)
      ]
  Gen.element [(FloatBits a, b), (b, FloatBits a)]

{- | Operands for which the exact result of the operation is close to plus or
minus the smallest normal number, 2^-126. Picks a target result and one
operand, computes the other operand and rounds it to a nearby float.
-}
genNearMinNormal :: BinOp -> Gen (FloatBits, FloatBits)
genNearMinNormal op = do
  negative <- Gen.bool
  k <-
    Gen.frequency
      [ (3, Gen.integral (Range.constant (-16) 16))
      , (1, Gen.integral (Range.constant (-(2 ^ (24 :: Int))) (2 ^ (24 :: Int))))
      ]
  let
    t0 = 2 ^^ (-126 :: Int) * (1 + fromInteger k / 2 ^ (26 :: Int)) :: Rational
    t = if negative then negate t0 else t0
  (a, b) <- case op of
    Add -> do
      x <- genNormalWithExponent (-126) (-100)
      pure (x, approx (t - toRational (bitsFloat x)))
    Sub -> do
      x <- genNormalWithExponent (-126) (-100)
      pure (x, approx (toRational (bitsFloat x) - t))
    Mul -> do
      x <- genNormalWithExponent (-100) 0
      pure (x, approx (t / toRational (bitsFloat x)))
    Div -> do
      y <- genNormalWithExponent 0 100
      pure (approx (t * toRational (bitsFloat y)), y)
  -- Nudge one of the operands by a few ulps
  d <- Gen.integral (Range.constant (-3) 3)
  Gen.element
    [ (nudge d a, b)
    , (a, nudge d b)
    ]
 where
  approx :: Rational -> FloatBits
  approx = floatBits . fromRational
  nudge :: Int -> FloatBits -> FloatBits
  nudge d (FloatBits w) = FloatBits (w + fromIntegral d)

-- | Operands for the binary operations: a mix of all of the above
genBinaryOperands :: BinOp -> Gen (FloatBits, FloatBits)
genBinaryOperands op =
  Gen.frequency
    [ (5, (,) <$> genFloat <*> genFloat)
    , (2, genRelated)
    , (3, genNearMinNormal op)
    ]

-- | Operands for comparison: a mix of independent and related floats, and
-- zeros and subnormals (which compare equal).
genCompareOperands :: Gen (FloatBits, FloatBits)
genCompareOperands =
  Gen.frequency
    [ (4, (,) <$> genFloat <*> genFloat)
    , (3, genRelated)
    , (1, (,) <$> tiny <*> tiny)
    ]
 where
  -- Zeros, subnormals and the normal numbers closest to them
  tiny =
    Gen.choice
      [ genZero
      , genSubnormal
      , (\s d -> mkFloat s 1 d) <$> Gen.bool <*> Gen.word32 (Range.constant 0 4)
      ]

{- | Products in the known mismatch window. With @x = mx * 2^ex@ and
@y = my * 2^ey@ for 24-bit integer significands @mx@ and @my@, the exact
product is in the window iff @mx * my@ is in @[2^47 - 2^23, 2^47 - 2^22)@ and
@ex + ey = -173@.
-}
genMulWindow :: Gen (FloatBits, FloatBits)
genMulWindow = do
  (mx, my) <-
    Gen.filter (\(mx, my) -> mx * my < hi) $ do
      mx <- Gen.integral (Range.constant (2 ^ (23 :: Int)) (2 ^ (24 :: Int) - 1))
      pure (mx, (lo + mx - 1) `quot` mx)
  ex <- Gen.int (Range.constant (-149) (-24))
  let ey = -173 - ex
  x <- signed mx ex
  y <- signed my ey
  Gen.element [(x, y), (y, x)]
 where
  lo, hi :: Integer
  lo = 2 ^ (47 :: Int) - 2 ^ (23 :: Int)
  hi = 2 ^ (47 :: Int) - 2 ^ (22 :: Int)

{- | Quotients in the known mismatch window. There is only one pair of
significands for which this happens: the dividend's is all ones and the
divisor's is one, and then @x / y = 2^-126 - 2^-150@ exactly.
-}
genDivWindow :: Gen (FloatBits, FloatBits)
genDivWindow = do
  ey <- Gen.int (Range.constant (-22) 104)
  x <- signed (2 ^ (24 :: Int) - 1) (ey - 127)
  y <- signed (2 ^ (23 :: Int)) ey
  pure (x, y)

-- | @m * 2^e@ with a random sign
signed :: Integer -> Int -> Gen FloatBits
signed m e = do
  negative <- Gen.bool
  pure (floatBits (encodeFloat (if negative then negate m else m) e))

-- | Integers, biased towards rounding edge cases: values just below, at and
-- just above halfway between two floats.
genWord32 :: Gen Word32
genWord32 =
  Gen.frequency
    [ (3, Gen.word32 Range.constantBounded)
    , (2, Gen.word32 Range.exponentialBounded)
    , (3, genRoundingCase)
    , (1, Gen.element [0, 1, 2 ^ (24 :: Int), 2 ^ (24 :: Int) + 1, 2 ^ (31 :: Int), maxBound])
    ]
 where
  genRoundingCase = do
    k <- Gen.int (Range.constant 1 8)
    m <- Gen.word32 (Range.constant (2 ^ (23 :: Int)) (2 ^ (24 :: Int) - 1))
    let half = 2 ^ (k - 1)
    r <- Gen.element [half - 1, half, half + 1, 0, 2 * half - 1]
    pure ((m `shiftL` k) + r)

genInt32 :: Gen Int32
genInt32 = do
  w <- genWord32
  Gen.element
    [ fromIntegral w
    , fromIntegral (w `shiftR` 1)
    , negate (fromIntegral (w `shiftR` 1))
    , fromIntegral (complement w)
    ]
