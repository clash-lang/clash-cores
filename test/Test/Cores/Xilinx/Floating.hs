{-# LANGUAGE AllowAmbiguousTypes #-}
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

  Differential tests of the simulation models in "Clash.Cores.Xilinx.Floating"
  and "Clash.Cores.Xilinx.Floating.Generic.Explicit" against the bit-accurate C
  model of the Xilinx Floating-Point LogiCORE IP v7.1. Covers addition,
  subtraction, multiplication, division, comparison (condition code), square
  root, absolute value and conversion to every precision in half, single and
  double precision, and conversions from @Unsigned 32@, @Signed 32@,
  @Unsigned 64@ and @Signed 64@ to single and double precision and from those
  to @Signed 32@ (the IP cannot convert between integers and half precision),
  all with round to nearest even, the configuration the modules
  instantiate. The tests run the actual functions in Clash simulation and
  compare the bits of their output with the output of the C model. Single
  precision mostly runs the functions from
  "Clash.Cores.Xilinx.Floating.Explicit", half and double precision the ones
  from "Clash.Cores.Xilinx.Floating.Generic.Explicit",
  "Clash.Cores.Xilinx.Floating.Half.Explicit" and
  "Clash.Cores.Xilinx.Floating.Double.Explicit".

  = Locating the C model

  Xilinx's C model cannot be redistributed, so it is located at run time:

  1. If @CLASH_CORES_XILINX_FLOATING_CMODEL@ is set, it should point at either
     @floating_point_v7_1_bitacc_cmodel_lin64.zip@ (Vivado ships it in
     @data\/ip\/xilinx\/floating_point_v7_1\/cmodel@) or a directory holding its
     extracted contents. If it points at something unusable, the tests fail.

  2. Otherwise, if @XILINX_VIVADO@ is set (by Vivado's @settings64.sh@), the
     zip file is taken from that installation.

  3. Otherwise, the tests are skipped: a single test passes and reports why.

  For example:

  > export CLASH_CORES_XILINX_FLOATING_CMODEL=/opt/tools/Xilinx/Vivado/2022.1/data/ip/xilinx/floating_point_v7_1/cmodel/floating_point_v7_1_bitacc_cmodel_lin64.zip
  > cabal run unit-tests -- -p '/Xilinx floating point vs. C model/'

  Extracting the zip file requires @unzip@ on the @PATH@.

  Each operation is checked on 20,000 random inputs per precision by default
  ('nTests'), which takes a few seconds.
  Use tasty-hedgehog's @--hedgehog-tests@ to check more, for example
  @--hedgehog-tests 1000000@.

  Every test fails after ten minutes ('cmodelTimeout'), so that a hanging C
  model cannot block the test suite. Checking many more inputs can take longer:
  tasty's @--timeout@ sets another limit.

  = Bridge to the C model

  A small C program (@test\/Test\/Cores\/Xilinx\/Floating\/CModelDriver.c@,
  embedded into this module at compile time) is compiled at test time with the
  system C compiler (@$CC@, or @cc@) against the C model's shared library. Every
  property starts its own instance, so that properties running in parallel do
  not wait for each other. An instance reads one operation per line on stdin
  and writes the result on stdout. See the C file for the protocol, and for how
  it gets bit patterns in and out of the C model.

  We use a separate process instead of the FFI because:

  * The C model is not available when the test suite is built, so we cannot
    link against it. Loading it with @dlopen@ would put Xilinx's bundled GMP and
    MPFR libraries in the same process as the GMP that GHC uses for 'Integer',
    and the C model is a C++ library which is not documented to be thread safe.
    A separate process isolates the C model from the test suite, including
    any crashes.

  * GHC needs a C compiler to link anyway, so compiling the driver adds no
    dependencies.

  = NaN

  The C model's NaNs have no sign or payload: in single and double precision,
  it returns @0x7FC00000@ and @0x7FF8000000000000@, positive quiet NaNs with an
  otherwise empty payload. It cannot return half precision numbers, so the
  driver converts them from double precision, which turns NaN into @0x7E00@
  (see the C file). The binary operations compare all NaN results bit for bit,
  and a separate property checks that @xilinxNaN@ is the C model's NaN.

  = Results just below the smallest normal number

  The IP rounds an exact result to the precision's number of significant bits
  @p@ (11, 24 or 53) first, and only then flushes it to zero if its magnitude is
  below the smallest normal number, @2^e@ (@e@ is -14, -126 or -1022). IEEE
  rounding instead rounds to the /subnormal/ grid near zero, which gives the
  smallest normal number where the IP gives a zero of the same sign when the
  exact result's magnitude lies in

  > [2^e - 2^(e-p), 2^e - 2^(e-p-1))

  For single precision that is @[2^-126 - 2^-150, 2^-126 - 2^-151)@. Results of
  addition and subtraction never land in this window, because sums of normal
  numbers are exact multiples of @2^(e-p+1)@ there. Products can: for example
  @0x3f7fffff * 0x00800000@ in single precision (exact result 2^-126 - 2^-150).
  Quotients land in it only when the dividend's significand is all ones and the
  divisor is a power of two, for example @0x3fffffff / 0x7f000000@. Random
  inputs almost never hit the window, so separate properties generate products
  and quotients in it.

  Results in @[2^e - 2^(e-p-1), 2^e)@ round up to the smallest normal number,
  in the IP and in IEEE rounding. This includes its lower bound, which is
  halfway between the smallest normal number and the number below it. Another
  property generates products in this window, including ones exactly at its
  lower bound. Quotients never land in it.

  = Absolute value

  The IP only clears the sign bit, also of subnormal numbers and NaNs (checked
  in Vivado). The C model's NaNs have no payload, but its functions working on
  the bits of a @float@ or @double@ keep them, see the C file.

  = Inputs

  Besides random bit patterns, the binary operations get operands of every
  class (zeros, subnormal numbers, normal numbers, infinities and NaNs with
  varied payloads, of both signs), related operands (equal, negated, adjacent
  or of similar magnitude), and operands whose exact result is close to the
  smallest normal number, close to the overflow threshold (where results round
  to infinity), or (close to) halfway between two representable numbers.

  The operations with one input get inputs of every class too. The square root
  also gets exact squares and inputs whose square root is very close to halfway
  between two numbers, see 'genSqrtOperand'. Conversions to a narrower
  precision get inputs close to halfway between two numbers of that precision,
  and close to its smallest normal number and its overflow threshold.
  Conversions to @Signed 32@ get inputs close to halfway between two integers
  and close to the bounds, and conversions from 64-bit integers get integers
  just below, at and just above halfway between two numbers.

  Half precision has only 65,536 numbers, so the operations with one input in
  half precision are also tested on all of them.
-}
module Test.Cores.Xilinx.Floating (mkTests) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, catch, onException, try)
import Control.Monad (forM, unless, void, when)
import Data.Bits (bit, clearBit, complement, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Int (Int32, Int64)
import Data.List (isPrefixOf, nub, subsequences)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.String (fromString)
import Data.Word (Word32, Word64)
import GHC.Float
  ( castDoubleToWord64, castFloatToWord32, castWord32ToFloat
  , castWord64ToDouble )
import Numeric (readHex, showHex)
import Numeric.Half (Half)
import System.Directory
  ( createDirectory, doesDirectoryExist, doesFileExist, makeAbsolute
  , removeDirectoryRecursive )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStr)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (arch, os)
import System.Process
  ( CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc
  , readProcessWithExitCode, waitForProcess )

import qualified Language.Haskell.TH.Syntax as TH

import Clash.Explicit.Prelude (Clock, Enable, KnownNat, System, natToNum)
import qualified Clash.Explicit.Prelude as C
import Clash.Signal.Delayed (DSignal)
import qualified Clash.Signal.Delayed as D

import qualified Clash.Cores.Xilinx.Floating.Double.Explicit as DE
import qualified Clash.Cores.Xilinx.Floating.Explicit as F
import qualified Clash.Cores.Xilinx.Floating.Generic.Explicit as G
import qualified Clash.Cores.Xilinx.Floating.Half.Explicit as HE

import Hedgehog (Gen, Property, PropertyT, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.HUnit (assertFailure, testCase, testCaseInfo)

-- | Name of the environment variable pointing at the C model
cmodelEnvVar :: String
cmodelEnvVar = "CLASH_CORES_XILINX_FLOATING_CMODEL"

-- | Location of the zip file in a Vivado installation
cmodelZipInVivado :: FilePath
cmodelZipInVivado =
  "data/ip/xilinx/floating_point_v7_1/cmodel/floating_point_v7_1_bitacc_cmodel_lin64.zip"

-- | Shared library of the C model
cmodelLibrary :: FilePath
cmodelLibrary = "libIp_floating_point_v7_1_bitacc_cmodel.so"

-- | Header of the C model
cmodelHeader :: FilePath
cmodelHeader = "floating_point_v7_1_bitacc_cmodel.h"

-- | Number of random cases per property, unless overridden by
-- @--hedgehog-tests@.
nTests :: H.TestLimit
nTests = 20_000

-- | Number of random cases for the properties generating results just below
-- the smallest normal number, and for the property checking NaN
nTestsTargeted :: H.TestLimit
nTestsTargeted = 2_000

-- | Ten minutes, the time after which a test fails, unless tasty's
-- @--timeout@ sets another limit. No test takes nearly that long with the
-- default numbers of inputs, also on a busy machine.
cmodelTimeout :: Timeout -> Timeout
cmodelTimeout NoTimeout = mkTimeout (10 * 60 * 1_000_000)
cmodelTimeout timeout = timeout

{- | Build the test tree. Needs 'IO' to find out whether the C model is
available. Finding it is cheap: preparing the C model happens when the tests
run, and only if they are selected.
-}
mkTests :: IO TestTree
mkTests = do
  location <- findCModel
  pure . adjustOption cmodelTimeout $ case location of
    Left reason ->
      testGroup groupName
        [testCaseInfo "C model not available, tests skipped" (pure reason)]
    Right path ->
      withResource (prepareDriver path) cleanupDriver $ \getPrepared ->
        testGroup groupName
          [ precisionTests @Float getPrepared (integerTests @Float getPrepared)
          , precisionTests @Half getPrepared (exhaustiveHalfTests getPrepared)
          , precisionTests @Double getPrepared (integerTests @Double getPrepared)
          ]
 where
  groupName = "Xilinx floating point vs. C model"

-- | Tests of one precision, followed by the given extra tests
precisionTests :: forall a. TestFloat a => IO Prepared -> [TestTree] -> TestTree
precisionTests getPrepared extraTests =
  testGroup (precisionName @a) $
    [ binaryTest Add
    , binaryTest Sub
    , binaryTest Mul
    , binaryTest Div
    , property "compare" "compare" (prop_compare @a)
    , property "sqrt" "sqrt" (prop_sqrt @a)
    , property "abs" "abs" (prop_abs @a)
    , property "convert to half" "toHalf" (prop_convert @a @Half "tohalf" modelToHalf)
    , property "convert to single" "toSingle" (prop_convert @a @Float "tosingle" modelToSingle)
    , property "convert to double" "toDouble" (prop_convert @a @Double "todouble" modelToDouble)
    ]
      <> extraTests
      <> [ property "NaN" "nan" (prop_nan @a)
         , belowMinNormalTest Mul (genMulWindow @a)
         , belowMinNormalTest Div (genDivWindow @a)
         , roundsUpTest
         ]
 where
  property name propName prop =
    withDriver getPrepared $ \getDriver ->
      testPropertyNamed name (propertyName @a propName) (prop getDriver)

  binaryTest op =
    property (opName op) (opName op) $ \getDriver ->
      H.withTests nTests (prop_binary @a getDriver op (genBinaryOperands @a op) labels)
   where
    labels _ _ exact = do
      let is f = maybe False f exact
      H.classify "exact result near smallest normal number" (is (nearMinNormal @a))
      H.classify "exact result just below smallest normal number" (is (justBelowMinNormal @a))
      H.classify "exact result just below smallest normal number, rounds up to it" (is (roundsUpToMinNormal @a))
      H.classify "exact result near overflow threshold" (is (nearOverflow @a))
      H.classify "exact result rounds to infinity" (is (roundsToInfinity @a))
      H.classify "exact result halfway between two numbers" (is (halfway @a))
      H.classify "exact result very close to halfway between two numbers" (is (nearHalfway @a))

  belowMinNormalTest op gen =
    property (opName op <> ", exact result in " <> belowMinNormalWindow @a) (opName op <> "BelowMinNormal") $ \getDriver ->
      H.withTests nTestsTargeted (prop_binary @a getDriver op gen labels)
   where
    -- Check the generator
    labels _ _ exact =
      H.cover 100 "exact result just below smallest normal number" (maybe False (justBelowMinNormal @a) exact)

  roundsUpTest =
    property ("mul, exact result in " <> roundsUpWindow @a) "mulRoundsUpToMinNormal" $ \getDriver ->
      H.withTests nTestsTargeted (prop_binary @a getDriver Mul (genMulRoundsUp @a) labels)
   where
    labels _ _ exact = do
      let is f = maybe False f exact
      -- Check the generator
      H.cover 100 "exact result just below smallest normal number, rounds up to it" (is (roundsUpToMinNormal @a))
      H.classify "exact result halfway between two numbers" (is (halfway @a))

-- | Tests of conversions from and to integers
integerTests :: forall a. TestInteger a => IO Prepared -> [TestTree]
integerTests getPrepared =
  [ property "fromU32" (prop_fromU32 @a)
  , property "fromS32" (prop_fromS32 @a)
  , property "fromU64" (prop_fromU64 @a)
  , property "fromS64" (prop_fromS64 @a)
  , property "toS32" (prop_toS32 @a)
  ]
 where
  property name prop =
    withDriver getPrepared $ \getDriver ->
      testPropertyNamed name (propertyName @a name) (prop getDriver)

{- | Tests of the operations with one input in half precision on every
possible input. There are only 65,536 of them.
-}
exhaustiveHalfTests :: IO Prepared -> [TestTree]
exhaustiveHalfTests getPrepared =
  [ exhaustive "sqrt" (toWord . modelSqrt @Half)
  , exhaustive "abs" (toWord . modelAbs @Half)
  , exhaustive "tohalf" (toWord . modelToHalf @Half)
  , exhaustive "tosingle" (toWord . modelToSingle @Half)
  , exhaustive "todouble" (toWord . modelToDouble @Half)
  ]
 where
  exhaustive op model =
    withDriver getPrepared $ \getDriver ->
      testCase (op <> ", every input") $ do
        driver <- getDriver
        mismatches <- fmap catMaybes . forM [0 .. wordMask @Half] $ \w -> do
          (cmodel, _) <- query driver (precisionName @Half) op w 0
          let haskell = model (fromWord w)
          pure (if haskell == cmodel then Nothing else Just (w, haskell, cmodel))
        case mismatches of
          [] -> pure ()
          (w, haskell, cmodel) : _ ->
            assertFailure $
              show (length mismatches) <> " mismatches. The first: " <> op <> " "
                <> showBits @Half w <> " gives 0x" <> showHex haskell ""
                <> " in the model, but 0x" <> showHex cmodel "" <> " in the C model"

-- | Name of a property, to rerun it
propertyName :: forall a. TestFloat a => String -> H.PropertyName
propertyName name = fromString ("prop_" <> precisionName @a <> "_" <> name)

{- | Find the C model, see the module documentation. Returns either a path to
the zip file or directory, or the reason the tests are skipped.
-}
findCModel :: IO (Either String FilePath)
findCModel
  | os /= "linux" || arch /= "x86_64" =
      pure (Left "The C model is only available for x86_64 Linux.")
  | otherwise = do
      explicit <- lookupEnv cmodelEnvVar
      vivado <- lookupEnv "XILINX_VIVADO"
      case (explicit, vivado) of
        (Just path, _) | not (null path) -> pure (Right path)
        (_, Just dir) | not (null dir) -> do
          let path = dir </> cmodelZipInVivado
          exists <- doesFileExist path
          pure $
            if exists
              then Right path
              else Left ("XILINX_VIVADO is set, but " <> path <> " does not exist.")
        _ ->
          pure . Left $
            "Set " <> cmodelEnvVar <> " to floating_point_v7_1_bitacc_cmodel_lin64.zip"
              <> " (or a directory with its extracted contents), or set XILINX_VIVADO."

------------------------------------------------------------------------------
-- C driver
------------------------------------------------------------------------------

-- | The C driver, compiled against the C model
data Prepared = Prepared
  { preparedTmpDir :: FilePath
  , preparedExecutable :: FilePath
  , preparedEnvironment :: [(String, String)]
  -- ^ Environment to run the driver in
  }

-- | A running instance of the C driver
data Driver = Driver
  { driverProcess :: ProcessHandle
  , driverPipes :: MVar (Handle, Handle)
  }

-- | Source of the C driver, embedded at compile time
driverSource :: String
driverSource =
  $( do
      path <- TH.runIO (makeAbsolute "test/Test/Cores/Xilinx/Floating/CModelDriver.c")
      TH.addDependentFile path
      TH.lift =<< TH.runIO (readFile path)
   )

-- | Prepare the C model and compile the driver
prepareDriver :: FilePath -> IO Prepared
prepareDriver path = do
  tmpRoot <- getCanonicalTemporaryDirectory
  tmp <- createTempDirectory tmpRoot "clash-cores-xilinx-floating"
  prepareDriverIn tmp path `onException` removeDirectoryRecursive tmp

prepareDriverIn :: FilePath -> FilePath -> IO Prepared
prepareDriverIn tmp path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  modelDir <-
    if
      | isDir -> makeAbsolute path
      | isFile -> do
          let dir = tmp </> "cmodel"
          createDirectory dir
          run
            ( "Extracting the zip file requires unzip. Alternatively, set "
                <> cmodelEnvVar <> " to a directory with its extracted contents."
            )
            "unzip" ["-q", path, "-d", dir]
          pure dir
      | otherwise ->
          fail (cmodelEnvVar <> ": " <> path <> " is neither a file nor a directory")
  let lib = modelDir </> cmodelLibrary
  hasLib <- doesFileExist lib
  hasHeader <- doesFileExist (modelDir </> cmodelHeader)
  unless (hasLib && hasHeader) $
    fail ("C model not found in " <> path <> ": expected " <> cmodelLibrary <> " and " <> cmodelHeader)

  let src = tmp </> "driver.c"
      exe = tmp </> "driver"
  writeFile src driverSource
  -- CC can include arguments, as in "ccache gcc" or "gcc -m64"
  ccWords <- words . fromMaybe "" <$> lookupEnv "CC"
  let
    (cc, ccArgs) = case ccWords of
      prog : args -> (prog, args)
      [] -> ("cc", [])
  -- The driver links against the C model's library by its path, as it has no
  -- SONAME. That library needs the C model's GMP and MPFR libraries: tell the
  -- linker where to find them, and the driver through LD_LIBRARY_PATH.
  run "Compiling the C model driver requires a C compiler: set CC or install cc." cc $ ccArgs <>
    ["-O1", "-I", modelDir, src, lib, "-Xlinker", "-rpath-link", "-Xlinker", modelDir, "-lm", "-o", exe]

  environment <- getEnvironment
  let
    ldPath = modelDir <> maybe "" (":" <>) (lookup "LD_LIBRARY_PATH" environment)
    environment1 =
      ("LD_LIBRARY_PATH", ldPath) : filter ((/= "LD_LIBRARY_PATH") . fst) environment
  pure
    Prepared
      { preparedTmpDir = tmp
      , preparedExecutable = exe
      , preparedEnvironment = environment1
      }
 where
  run hint cmd args = do
    (code, out, err) <-
      readProcessWithExitCode cmd args "" `catch` \(e :: IOException) ->
        fail (show e <> "\n" <> hint)
    case code of
      ExitSuccess -> pure ()
      ExitFailure n ->
        fail $
          unlines
            [ "Command failed with exit code " <> show n <> ": " <> unwords (cmd : args)
            , out
            , err
            ]

cleanupDriver :: Prepared -> IO ()
cleanupDriver Prepared{preparedTmpDir} =
  void (try @SomeException (removeDirectoryRecursive preparedTmpDir))

-- | Start an instance of the driver
startDriver :: Prepared -> IO Driver
startDriver Prepared{preparedExecutable, preparedEnvironment} = do
  (Just hIn, Just hOut, Nothing, ph) <-
    createProcess
      (proc preparedExecutable [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , env = Just preparedEnvironment
        }
  ready <- hGetLine hOut
  unless ("ready " `isPrefixOf` ready) $
    fail ("Unexpected start-up message from C model driver: " <> show ready)
  pipes <- newMVar (hIn, hOut)
  pure Driver{driverProcess = ph, driverPipes = pipes}

stopDriver :: Driver -> IO ()
stopDriver Driver{driverProcess, driverPipes} = do
  withMVar driverPipes (hClose . fst)
  void (waitForProcess driverProcess)

-- | Run tests with their own instance of the driver
withDriver :: IO Prepared -> (IO Driver -> TestTree) -> TestTree
withDriver getPrepared = withResource (getPrepared >>= startDriver) stopDriver

-- | Ask the C model. Returns the result and the exception flags.
query :: Driver -> String -> String -> Word64 -> Word64 -> IO (Word64, Int)
query Driver{driverPipes} precision op a b =
  withMVar driverPipes $ \(hIn, hOut) -> do
    hPutStr hIn (unwords [precision, op, showHex a "", showHex b ""] <> "\n")
    hFlush hIn
    response <- hGetLine hOut
    case words response of
      [readHex -> [(result, "")], read -> exceptions] -> pure (result, exceptions)
      _ -> fail ("Unexpected response from C model driver: " <> show response)

------------------------------------------------------------------------------
-- Precisions
------------------------------------------------------------------------------

data BinOp = Add | Sub | Mul | Div
  deriving (Eq, Show)

-- | Name of the operation, as understood by the C driver
opName :: BinOp -> String
opName = \case
  Add -> "add"
  Sub -> "sub"
  Mul -> "mul"
  Div -> "div"

-- | A precision under test: its format, and the Haskell models under test,
-- which are the simulations of the Clash functions.
class (RealFloat a, Show a) => TestFloat a where
  -- | Name of the precision, as understood by the C driver
  precisionName :: String

  -- | Width of the exponent field
  exponentWidth :: Int

  -- | Width of the fraction field, excluding the hidden bit
  fractionWidth :: Int

  toWord :: a -> Word64
  fromWord :: Word64 -> a

  modelBinary :: BinOp -> a -> a -> a
  modelCompare :: a -> a -> G.Ordering
  modelSqrt :: a -> a
  modelAbs :: a -> a
  modelToHalf :: a -> Half
  modelToSingle :: a -> Float
  modelToDouble :: a -> Double

  -- | The NaN the models produce, @xilinxNaN@
  modelNaN :: a

  -- | Prime factors of @2^(p+1) - 1@, see 'minNormalTies'
  minNormalTieFactors :: [Integer]

-- | Precisions the IP can convert integers to and from
class TestFloat a => TestInteger a where
  modelFromU32 :: Word32 -> a
  modelFromS32 :: Int32 -> a
  modelFromU64 :: Word64 -> a
  modelFromS64 :: Int64 -> a
  modelToS32 :: a -> Int32

-- | Single precision runs the functions from
-- "Clash.Cores.Xilinx.Floating.Explicit"
instance TestFloat Float where
  precisionName = "single"
  exponentWidth = 8
  fractionWidth = 23
  toWord = fromIntegral . castFloatToWord32
  fromWord = castWord32ToFloat . fromIntegral
  modelBinary op x y = case op of
    Add -> afterPipeline (F.add clk en (constant x) (constant y))
    Sub -> afterPipeline (F.sub clk en (constant x) (constant y))
    Mul -> afterPipeline (F.mul clk en (constant x) (constant y))
    Div -> afterPipeline (F.div clk en (constant x) (constant y))
  modelCompare x y = afterPipeline (F.compare clk en (constant x) (constant y))
  modelSqrt x = afterPipeline (F.sqrt clk en (constant x))
  modelAbs x = afterPipeline (F.abs (constant x))
  modelToHalf x = afterPipeline (F.toHalf clk en (constant x))
  modelToSingle x = afterPipeline (F.convert clk en (constant x))
  modelToDouble x = afterPipeline (F.toDouble clk en (constant x))
  modelNaN = F.xilinxNaN
  minNormalTieFactors = [31, 601, 1801]

instance TestInteger Float where
  modelFromU32 w = afterPipeline (F.fromU32 clk en (constant (fromIntegral w)))
  modelFromS32 i = afterPipeline (F.fromS32 clk en (constant (fromIntegral i)))
  modelFromU64 w = afterPipeline (F.fromU64 clk en (constant (fromIntegral w)))
  modelFromS64 i = afterPipeline (F.fromS64 clk en (constant (fromIntegral i)))
  modelToS32 x = fromIntegral (afterPipeline (F.toS32 clk en (constant x)))

instance TestFloat Half where
  precisionName = "half"
  exponentWidth = 5
  fractionWidth = 10
  toWord = fromIntegral . C.pack
  fromWord = C.unpack . fromIntegral
  modelBinary = genericBinary
  modelCompare = genericCompare
  modelSqrt x = afterPipeline (HE.sqrt clk en (constant x))
  modelAbs x = afterPipeline (HE.abs (constant x))
  modelToHalf x = afterPipeline (HE.convert clk en (constant x))
  modelToSingle x = afterPipeline (HE.toFloat clk en (constant x))
  modelToDouble x = afterPipeline (HE.toDouble clk en (constant x))
  modelNaN = G.xilinxNaN
  minNormalTieFactors = [3, 3, 5, 7, 13]

instance TestFloat Double where
  precisionName = "double"
  exponentWidth = 11
  fractionWidth = 52
  toWord = castDoubleToWord64
  fromWord = castWord64ToDouble
  modelBinary = genericBinary
  modelCompare = genericCompare
  modelSqrt x = afterPipeline (G.sqrt clk en (constant x))
  modelAbs x = afterPipeline (G.abs (constant x))
  modelToHalf x = afterPipeline (DE.toHalf clk en (constant x))
  modelToSingle x = afterPipeline (DE.toFloat clk en (constant x))
  modelToDouble x = afterPipeline (G.convert clk en (constant x))
  modelNaN = G.xilinxNaN
  minNormalTieFactors = [3, 3, 3, 3, 7, 19, 73, 87211, 262657]

instance TestInteger Double where
  modelFromU32 w = afterPipeline (G.fromU32 clk en (constant (fromIntegral w)))
  modelFromS32 i = afterPipeline (G.fromS32 clk en (constant (fromIntegral i)))
  modelFromU64 w = afterPipeline (DE.fromU64 clk en (constant (fromIntegral w)))
  modelFromS64 i = afterPipeline (G.fromS64 clk en (constant (fromIntegral i)))
  modelToS32 x = fromIntegral (afterPipeline (DE.toS32 clk en (constant x)))

-- | Binary operations from "Clash.Cores.Xilinx.Floating.Generic.Explicit"
genericBinary ::
  forall a.
  ( G.XilinxFloat a
  , KnownNat (G.AddDefDelay a)
  , KnownNat (G.SubDefDelay a)
  , KnownNat (G.MulDefDelay a)
  , KnownNat (G.DivDefDelay a)
  ) =>
  BinOp -> a -> a -> a
genericBinary op x y = case op of
  Add -> afterPipeline (G.add clk en (constant x) (constant y))
  Sub -> afterPipeline (G.sub clk en (constant x) (constant y))
  Mul -> afterPipeline (G.mul clk en (constant x) (constant y))
  Div -> afterPipeline (G.div clk en (constant x) (constant y))

-- | Comparison from "Clash.Cores.Xilinx.Floating.Generic.Explicit"
genericCompare ::
  forall a. (G.XilinxFloat a, KnownNat (G.CompareDefDelay a)) => a -> a -> G.Ordering
genericCompare x y = afterPipeline (G.compare clk en (constant x) (constant y))

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

-- | Width of the bit pattern
totalWidth :: forall a. TestFloat a => Int
totalWidth = 1 + exponentWidth @a + fractionWidth @a

-- | Number of significant bits, including the hidden bit
significandWidth :: forall a. TestFloat a => Int
significandWidth = fractionWidth @a + 1

-- | All bits of the bit pattern
wordMask :: forall a. TestFloat a => Word64
wordMask = bit (totalWidth @a) - 1

signBit :: forall a. TestFloat a => Word64
signBit = bit (totalWidth @a - 1)

fractionMask :: forall a. TestFloat a => Word64
fractionMask = bit (fractionWidth @a) - 1

-- | Biased exponent of the largest finite numbers
maxBiasedExponent :: forall a. TestFloat a => Word64
maxBiasedExponent = bit (exponentWidth @a) - 2

bias :: forall a. TestFloat a => Int
bias = 2 ^ (exponentWidth @a - 1) - 1

-- | Exponent of the smallest normal number, @2^minExponent@
minExponent :: forall a. TestFloat a => Int
minExponent = 1 - bias @a

-- | Exponent of the largest finite numbers
maxExponent :: forall a. TestFloat a => Int
maxExponent = bias @a

-- | Biased exponent of a bit pattern
biasedExponent :: forall a. TestFloat a => Word64 -> Int
biasedExponent w = fromIntegral ((w `shiftR` fractionWidth @a) .&. (bit (exponentWidth @a) - 1))

------------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------------

-- | Floating point number shown by its bit pattern and its value
newtype FloatBits a = FloatBits Word64
  deriving (Eq)

instance TestFloat a => Show (FloatBits a) where
  show (FloatBits w) = showBits @a w <> " (" <> show (fromWord @a w) <> ")"

showBits :: forall a. TestFloat a => Word64 -> String
showBits w = "0x" <> pad (showHex w "")
 where
  pad s = replicate (totalWidth @a `div` 4 - length s) '0' <> s

floatBits :: TestFloat a => a -> FloatBits a
floatBits = FloatBits . toWord

bitsFloat :: TestFloat a => FloatBits a -> a
bitsFloat (FloatBits w) = fromWord w

-- | Exact result of an operation on finite operands, after flushing subnormal
-- operands to zero like the IP does.
exactResult :: RealFloat a => BinOp -> a -> a -> Maybe Rational
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
flushSubnormal :: RealFloat a => a -> a
flushSubnormal z = if isDenormalized z then 0 else z

-- | Exponent of the most significant bit of a positive number: @e@ such that
-- @2^e <= r < 2^(e+1)@
exponentOf :: Rational -> Int
exponentOf r = adjust (bitLength (numerator r) - bitLength (denominator r))
 where
  bitLength :: Integer -> Int
  bitLength = length . takeWhile (> 0) . iterate (`shiftR` 1)
  adjust e
    | 2 ^^ e > r = adjust (e - 1)
    | 2 ^^ (e + 1) <= r = adjust (e + 1)
    | otherwise = e

-- | Whether the magnitude of an exact result lies near the smallest normal
-- number
nearMinNormal :: forall a. TestFloat a => Rational -> Bool
nearMinNormal r = abs r >= 2 ^^ (minExponent @a - 2) && abs r < 2 ^^ (minExponent @a + 2)

{- | Whether the magnitude of an exact result lies in
@[2^e - 2^(e-p), 2^e - 2^(e-p-1))@, where the IP flushes results to zero
that IEEE rounding would round up to the smallest normal number. See the
module documentation.
-}
justBelowMinNormal :: forall a. TestFloat a => Rational -> Bool
justBelowMinNormal r = lo <= abs r && abs r < hi
 where
  e = minExponent @a
  p = significandWidth @a
  lo = 2 ^^ e - 2 ^^ (e - p)
  hi = 2 ^^ e - 2 ^^ (e - p - 1)

-- | The window of 'justBelowMinNormal', as text
belowMinNormalWindow :: forall a. TestFloat a => String
belowMinNormalWindow =
  "[" <> pow e <> " - " <> pow (e - p) <> ", " <> pow e <> " - " <> pow (e - p - 1) <> ")"
 where
  e = minExponent @a
  p = significandWidth @a
  pow n = "2^" <> show n

-- | Whether the magnitude of an exact result lies in
-- @[2^e - 2^(e-p-1), 2^e)@, where the IP rounds it up to the smallest normal
-- number. The lower bound is halfway between the smallest normal number and
-- the number below it, which has an odd significand, so it rounds up too.
roundsUpToMinNormal :: forall a. TestFloat a => Rational -> Bool
roundsUpToMinNormal r = lo <= abs r && abs r < 2 ^^ e
 where
  e = minExponent @a
  lo = 2 ^^ e - 2 ^^ (e - significandWidth @a - 1)

-- | The window of 'roundsUpToMinNormal', as text
roundsUpWindow :: forall a. TestFloat a => String
roundsUpWindow = "[" <> pow e <> " - " <> pow (e - p - 1) <> ", " <> pow e <> ")"
 where
  e = minExponent @a
  p = significandWidth @a
  pow n = "2^" <> show n

-- | Whether the magnitude of an exact result lies near the threshold above
-- which results round to infinity
nearOverflow :: forall a. TestFloat a => Rational -> Bool
nearOverflow r = abs r >= t * (1 - d) && abs r < t * (1 + d)
 where
  t = 2 ^^ (maxExponent @a + 1)
  d = 2 ^^ (5 - significandWidth @a)

-- | Whether an exact result rounds to infinity: its magnitude is at least
-- halfway between the largest finite number and @2^(emax+1)@
roundsToInfinity :: forall a. TestFloat a => Rational -> Bool
roundsToInfinity r = abs r >= 2 ^^ (e + 1) - 2 ^^ (e - significandWidth @a)
 where
  e = maxExponent @a

-- | Whether an exact result lies exactly halfway between two numbers of the
-- precision. The IP rounds to the precision as if the exponent were unbounded,
-- so this also considers results below the smallest normal number.
halfway :: forall a. TestFloat a => Rational -> Bool
halfway r = r /= 0 && distanceToHalfway @a r == 0

-- | Whether an exact result is not halfway between two numbers of the
-- precision, but very close to it: within @2^(3-p)@ times half an ulp
nearHalfway :: forall a. TestFloat a => Rational -> Bool
nearHalfway r = r /= 0 && d > 0 && d < 2 ^^ (3 - significandWidth @a)
 where
  d = distanceToHalfway @a r

-- | Distance of a non-zero number to the nearest number halfway between two
-- numbers of the precision, in units of half an ulp
distanceToHalfway :: forall a. TestFloat a => Rational -> Rational
distanceToHalfway r = abs (m - fromInteger (2 * floor (m / 2) + 1))
 where
  -- Significand with one more bit than the precision has: numbers halfway
  -- between two numbers of the precision have an odd one
  m = abs r * 2 ^^ (significandWidth @a - exponentOf (abs r))

prop_binary ::
  forall a.
  TestFloat a =>
  IO Driver ->
  BinOp ->
  Gen (FloatBits a, FloatBits a) ->
  -- | Labels for the coverage report, given the operands and the exact result
  (a -> a -> Maybe Rational -> PropertyT IO ()) ->
  Property
prop_binary getDriver op genOperands labels = H.property $ do
  driver <- H.evalIO getDriver
  (a, b) <- H.forAll genOperands
  let
    x = bitsFloat a
    y = bitsFloat b
    exact = exactResult op x y
  classifyOperands x y
  labels x y exact

  (cBits, exceptions) <- H.evalIO (query driver (precisionName @a) (opName op) (toWord x) (toWord y))
  let
    haskell = floatBits (modelBinary op x y)
    cmodel = FloatBits cBits
  H.footnote ("C model exception flags: " <> show exceptions)

  haskell === cmodel

{- | Labels for the coverage report. We do not use 'H.cover' here: it fails
the test when the percentages are not met, which happens when running only a
few tests with @--hedgehog-tests@.
-}
classifyOperands :: (RealFloat a, Monad m) => a -> a -> PropertyT m ()
classifyOperands x y = do
  H.classify "an operand is zero" (any isZero [x, y])
  H.classify "an operand is subnormal" (any isDenormalized [x, y])
  H.classify "an operand is infinite" (any isInfinite [x, y])
  H.classify "an operand is NaN" (any isNaN [x, y])
  H.classify "both operands are normal" (all isNormal [x, y])
 where
  isZero z = z == 0
  isNormal z = not (isZero z || isDenormalized z || isInfinite z || isNaN z)

prop_compare :: forall a. TestFloat a => IO Driver -> Property
prop_compare getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  (a, b) <- H.forAll (genCompareOperands @a)
  let
    x = bitsFloat a
    y = bitsFloat b
  classifyOperands x y
  H.classify "operands equal after flushing subnormals" $
    not (isNaN x || isNaN y) && flushSubnormal x == flushSubnormal y

  (cmodel, exceptions) <- H.evalIO (query driver (precisionName @a) "cmp" (toWord x) (toWord y))
  let haskell = modelCompare x y
  H.footnote ("C model exception flags: " <> show exceptions)
  H.footnote ("Haskell model: " <> show haskell)
  -- The IP's condition code goes through 'unpack', so compare bits
  fromIntegral (C.pack haskell) === cmodel

{- | Operations that give NaN: the C model's result is the NaN the models
produce ('modelNaN', which is @xilinxNaN@), bit for bit, and so is the result
of the model of the operation.
-}
prop_nan :: forall a. TestFloat a => IO Driver -> Property
prop_nan getDriver = H.withTests nTestsTargeted $ H.property $ do
  driver <- H.evalIO getDriver
  (op, (a, b)) <- H.forAll (genNaNOperation @a)
  let
    x = bitsFloat a
    y = bitsFloat b
  H.classify "an operand is NaN" (isNaN x || isNaN y)
  H.classify "invalid operation" (not (isNaN x || isNaN y))
  (cBits, exceptions) <- H.evalIO (query driver (precisionName @a) (opName op) (toWord x) (toWord y))
  H.footnote ("C model exception flags: " <> show exceptions)
  H.footnote ("Haskell model of " <> opName op <> ": " <> show (floatBits (modelBinary op x y)))
  FloatBits cBits === floatBits (modelNaN @a)
  floatBits (modelBinary op x y) === FloatBits cBits

prop_fromU32 :: forall a. TestInteger a => IO Driver -> Property
prop_fromU32 getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  w <- H.forAll genWord32
  (cmodel, _) <- H.evalIO (query driver (precisionName @a) "u32" (fromIntegral w) 0)
  floatBits (modelFromU32 @a w) === FloatBits cmodel

prop_fromS32 :: forall a. TestInteger a => IO Driver -> Property
prop_fromS32 getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  i <- H.forAll genInt32
  (cmodel, _) <- H.evalIO (query driver (precisionName @a) "s32" (fromIntegral (fromIntegral i :: Word32)) 0)
  floatBits (modelFromS32 @a i) === FloatBits cmodel

prop_fromU64 :: forall a. TestInteger a => IO Driver -> Property
prop_fromU64 getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  w <- H.forAll genWord64
  (cmodel, _) <- H.evalIO (query driver (precisionName @a) "u64" w 0)
  floatBits (modelFromU64 @a w) === FloatBits cmodel

prop_fromS64 :: forall a. TestInteger a => IO Driver -> Property
prop_fromS64 getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  i <- H.forAll genInt64
  (cmodel, _) <- H.evalIO (query driver (precisionName @a) "s64" (fromIntegral i) 0)
  floatBits (modelFromS64 @a i) === FloatBits cmodel

prop_toS32 :: forall a. TestInteger a => IO Driver -> Property
prop_toS32 getDriver =
  H.withTests nTests $
    prop_unary @a getDriver "tos32" genToS32Operand labels model intResult
 where
  model x = fromIntegral (fromIntegral (modelToS32 x) :: Word32)
  intResult w = Result w (show (fromIntegral w :: Int32))
  labels x = do
    let r = toRational x
        fraction = r - fromInteger (floor r)
    H.classify "in range" (not (isNaN x) && r >= -(2 ^ (31 :: Int)) && r < 2 ^ (31 :: Int))
    H.classify "out of range" (not (isNaN x || isInfinite x) && (r < -(2 ^ (31 :: Int)) || r >= 2 ^ (31 :: Int)))
    H.classify "halfway between two integers" (not (isNaN x || isInfinite x) && fraction == 1 / 2)

prop_sqrt :: forall a. TestFloat a => IO Driver -> Property
prop_sqrt getDriver =
  H.withTests nTests $
    prop_unary @a getDriver "sqrt" genSqrtOperand labels (toWord . modelSqrt) (floatResult @a)
 where
  labels x = do
    H.classify "square root very close to halfway between two numbers" (sqrtNearHalfway x)
    H.classify "square root exact" (sqrtExact x)

prop_abs :: forall a. TestFloat a => IO Driver -> Property
prop_abs getDriver =
  H.withTests nTests $
    prop_unary @a getDriver "abs" genFloat (\_ -> pure ()) (toWord . modelAbs) (floatResult @a)

{- | Conversion from precision @a@ to precision @b@. The arguments are the name
of the operation, as understood by the C driver, and the model.
-}
prop_convert ::
  forall a b. (TestFloat a, TestFloat b) => String -> (a -> b) -> IO Driver -> Property
prop_convert op model getDriver =
  H.withTests nTests $
    prop_unary @a getDriver op (genConvertOperand @a @b) labels (toWord . model) (floatResult @b)
 where
  labels x = when (significandWidth @b < significandWidth @a) $ do
    let
      exact = toRational x
      finite = not (isNaN x || isInfinite x || isDenormalized x || x == 0)
      is f = finite && f exact
    H.classify "halfway between two numbers of the target precision" (is (halfway @b))
    H.classify "very close to halfway between two numbers of the target precision" (is (nearHalfway @b))
    H.classify "just below the smallest normal number of the target precision" (is (justBelowMinNormal @b))
    H.classify "rounds up to the smallest normal number of the target precision" (is (roundsUpToMinNormal @b))
    H.classify "rounds to infinity in the target precision" (is (roundsToInfinity @b))
    H.classify "below the smallest normal number of the target precision" (is (\r -> abs r < 2 ^^ minExponent @b))

-- | Result of an operation, compared by its bit pattern and shown as text
data Result = Result Word64 String

instance Eq Result where
  Result x _ == Result y _ = x == y

instance Show Result where
  show (Result _ s) = s

-- | A floating point result of precision @b@, from its bit pattern
floatResult :: forall b. TestFloat b => Word64 -> Result
floatResult w = Result w (show (FloatBits w :: FloatBits b))

-- | Compare an operation with one floating point input with the C model
prop_unary ::
  forall a.
  TestFloat a =>
  IO Driver ->
  -- | Name of the operation, as understood by the C driver
  String ->
  -- | Inputs
  Gen (FloatBits a) ->
  -- | Labels for the coverage report, given the input
  (a -> PropertyT IO ()) ->
  -- | The model, giving the bit pattern the C driver would give
  (a -> Word64) ->
  -- | Interpret a bit pattern returned by the C driver or the model
  (Word64 -> Result) ->
  Property
prop_unary getDriver op genInput labels model result = H.property $ do
  driver <- H.evalIO getDriver
  a <- H.forAll genInput
  let x = bitsFloat a
  classifyOperand x
  labels x
  (cmodel, exceptions) <- H.evalIO (query driver (precisionName @a) op (toWord x) 0)
  H.footnote ("C model exception flags: " <> show exceptions)
  result (model x) === result cmodel

-- | Labels for the coverage report of operations with one input
classifyOperand :: forall a m. (TestFloat a, Monad m) => a -> PropertyT m ()
classifyOperand x = do
  H.classify "zero" (x == 0)
  H.classify "subnormal" (isDenormalized x)
  H.classify "normal" (not (x == 0 || isDenormalized x || isInfinite x || isNaN x))
  H.classify "infinite" (isInfinite x)
  H.classify "NaN" (isNaN x)
  H.classify "sign bit set" (testBit (toWord x) (totalWidth @a - 1))

-- | Integer square root: the largest integer whose square is at most @n@
isqrt :: Integer -> Integer
isqrt n
  | n < 2 = n
  | otherwise = go (bit ((bitLength n + 1) `div` 2))
 where
  -- Newton's iteration from above
  go x =
    let y = (x + n `div` x) `div` 2
     in if y >= x then x else go y
  bitLength :: Integer -> Int
  bitLength = length . takeWhile (> 0) . iterate (`shiftR` 1)

-- | Whether the square root of a positive normal number is exact: whether it
-- has at most @p@ significant bits
sqrtExact :: forall a. TestFloat a => a -> Bool
sqrtExact x
  | not (x > 0) || isInfinite x || isDenormalized x = False
  | otherwise = even e && isqrt m * isqrt m == m
 where
  (m0, e0) = decodeFloat x
  -- Make the exponent even
  (m, e) = if even e0 then (m0, e0) else (2 * m0, e0 - 1)

{- | Whether the square root of a positive normal number is very close to
halfway between two numbers of the precision: within @2^(4-p)@ units in the
last place.
-}
sqrtNearHalfway :: forall a. TestFloat a => a -> Bool
sqrtNearHalfway x
  | not (x > 0) || isInfinite x || isDenormalized x = False
  | otherwise = abs (r - midpoint * midpoint) < 2 * midpoint * delta
 where
  p = significandWidth @a
  r = toRational x
  -- Units in the last place of the square root
  ulpExponent = exponentOf r `div` 2 - (p - 1)
  ulp = 2 ^^ ulpExponent :: Rational
  -- The square root lies between @scaled * ulp@ and @(scaled + 1) * ulp@, so
  -- the nearest midpoint is halfway between those.
  scaled = isqrt (floor (r / (ulp * ulp)))
  midpoint = (fromInteger scaled + 1 / 2) * ulp
  -- sqrt(r) - midpoint is about (r - midpoint^2) / (2 midpoint)
  delta = ulp * 2 ^^ (4 - p)

------------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------------

-- | Number with the given sign, biased exponent and fraction
mkFloat :: forall a. TestFloat a => Bool -> Word64 -> Word64 -> FloatBits a
mkFloat s e m =
  FloatBits
    ( (if s then signBit @a else 0)
        .|. (e `shiftL` fractionWidth @a)
        .|. (m .&. fractionMask @a)
    )

-- | Fraction (stored significand bits), biased towards edge cases
genFraction :: forall a. TestFloat a => Gen Word64
genFraction =
  Gen.frequency
    [ (6, Gen.word64 (Range.constant 0 mask))
    , (1, Gen.element [0, 1, 2, half, half + 1, half - 1, mask - 1, mask])
    , (1, Gen.word64 (Range.constant 0 16))
    , (1, (mask -) <$> Gen.word64 (Range.constant 0 16))
    ]
 where
  mask = fractionMask @a
  half = bit (fractionWidth @a - 1)

genZero, genSubnormal, genNormal, genInfinity, genNaN :: forall a. TestFloat a => Gen (FloatBits a)
genZero = mkFloat <$> Gen.bool <*> pure 0 <*> pure 0
genSubnormal = mkFloat <$> Gen.bool <*> pure 0 <*> Gen.filter (/= 0) (genFraction @a)
genNormal = mkFloat <$> Gen.bool <*> genExponent <*> genFraction @a
 where
  maxE = maxBiasedExponent @a
  b = fromIntegral (bias @a)
  -- Width of the ranges near the smallest and largest exponents
  edge = min 30 (maxE `div` 4)
  -- Exponents for which results of operations on numbers of magnitude one
  -- interact
  around1 = fromIntegral (significandWidth @a + 3)
  genExponent =
    Gen.frequency
      [ (4, Gen.word64 (Range.constant 1 maxE))
      , (1, Gen.word64 (Range.constant 1 edge))
      , (1, Gen.word64 (Range.constant (maxE + 1 - edge) maxE))
      , (1, Gen.word64 (Range.constant (max 1 (b - around1)) (min maxE (b + around1))))
      ]
genInfinity = mkFloat <$> Gen.bool <*> pure (maxBiasedExponent @a + 1) <*> pure 0
genNaN = mkFloat <$> Gen.bool <*> pure (maxBiasedExponent @a + 1) <*> genPayload
 where
  quiet = bit (fractionWidth @a - 1)
  genPayload =
    Gen.choice
      [ -- Quiet NaNs
        (quiet .|.) <$> Gen.word64 (Range.constant 0 (quiet - 1))
      , -- Signaling NaNs
        Gen.word64 (Range.constant 1 (quiet - 1))
      , Gen.element [quiet, fractionMask @a, 1, quiet `shiftR` 1]
      ]

-- | Number of any class, both signs
genFloat :: forall a. TestFloat a => Gen (FloatBits a)
genFloat =
  Gen.frequency
    [ (1, genZero)
    , (2, genSubnormal)
    , (6, genNormal)
    , (1, genInfinity)
    , (2, genNaN)
    , (2, FloatBits <$> Gen.word64 (Range.constant 0 (wordMask @a)))
    ]

-- | Normal number with an unbiased exponent in the given range, clamped to the
-- normal range
genNormalWithExponent :: forall a. TestFloat a => Int -> Int -> Gen (FloatBits a)
genNormalWithExponent lo hi = do
  e <- Gen.int (Range.constant lo hi)
  let biased = max 1 (min (fromIntegral (maxBiasedExponent @a)) (e + bias @a))
  mkFloat <$> Gen.bool <*> pure (fromIntegral biased) <*> genFraction @a

-- | Add a (small) integer to a bit pattern, which gives an adjacent number
nudge :: forall a. TestFloat a => Int -> FloatBits a -> FloatBits a
nudge d (FloatBits w) = FloatBits ((w + fromIntegral d) .&. wordMask @a)

-- | Pair of numbers that are related: equal, negated, adjacent or of similar
-- magnitude. Exercises cancellation and rounding.
genRelated :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a)
genRelated = do
  a <- genFloat
  let
    e = biasedExponent @a (let FloatBits w = a in w) - bias @a
    p = significandWidth @a + 2
  b <-
    Gen.choice
      [ pure a
      , pure (let FloatBits w = a in FloatBits (w `xor` signBit @a))
      , (\d -> nudge d a) <$> Gen.int (Range.constant 1 4)
      , (\d -> nudge (negate d) a) <$> Gen.int (Range.constant 1 4)
      , genNormalWithExponent (e - p) (e + p)
      ]
  Gen.element [(a, b), (b, a)]

-- | Nearest number to a rational
approx :: TestFloat a => Rational -> FloatBits a
approx = floatBits . fromRational

-- | Offset from a target, in units of @2^-(p+2)@ relative to the target: mostly
-- a few quarter ulps, sometimes up to a quarter of the target.
genOffset :: forall a. TestFloat a => Gen Rational
genOffset = do
  k <-
    Gen.frequency
      [ (3, Gen.integral (Range.constant (-16) 16))
      , (1, Gen.integral (Range.constant (-(2 ^ p)) (2 ^ p)))
      ]
  pure (fromInteger k / 2 ^ (p + 2))
 where
  p = significandWidth @a

{- | Operands for which the exact result of the operation is close to the
target. Picks one operand, computes the other operand and rounds it to a nearby
number, and nudges one of them by a few ulps. The arguments are the ranges of
the unbiased exponents of the picked operand: the first operand for addition,
subtraction and multiplication, the second for division.
-}
genNearTarget ::
  forall a.
  TestFloat a =>
  -- | Exponents for addition and subtraction
  (Int, Int) ->
  -- | Exponents for multiplication
  (Int, Int) ->
  -- | Exponents for division
  (Int, Int) ->
  BinOp ->
  Rational ->
  Gen (FloatBits a, FloatBits a)
genNearTarget addExps mulExps divExps op t0 = do
  negative <- Gen.bool
  let t = if negative then negate t0 else t0
  (a, b) <- case op of
    Add -> do
      x <- uncurry genNormalWithExponent addExps
      pure (x, approx (t - toRational (bitsFloat x)))
    Sub -> do
      x <- uncurry genNormalWithExponent addExps
      pure (x, approx (toRational (bitsFloat x) - t))
    Mul -> do
      x <- uncurry genNormalWithExponent mulExps
      pure (x, approx (t / toRational (bitsFloat x)))
    Div -> do
      y <- uncurry genNormalWithExponent divExps
      pure (approx (t * toRational (bitsFloat y)), y)
  -- Nudge one of the operands by a few ulps
  d <- Gen.integral (Range.constant (-3) 3)
  Gen.element
    [ (nudge d a, b)
    , (a, nudge d b)
    ]

{- | Operands for which the exact result of the operation is close to plus or
minus the smallest normal number, @2^e@.
-}
genNearMinNormal :: forall a. TestFloat a => BinOp -> Gen (FloatBits a, FloatBits a)
genNearMinNormal op = do
  offset <- genOffset @a
  genNearTarget @a
    (e, e + p + 2)
    (min (e + p + 2) (e `quot` 2), 0)
    (0, max (negate (e + p + 2)) (negate (e `quot` 2)))
    op
    (2 ^^ e * (1 + offset))
 where
  e = minExponent @a
  p = significandWidth @a

{- | Operands for which the exact result of the operation is close to plus or
minus @2^(emax+1)@, near which results start to round to infinity.
-}
genNearOverflow :: forall a. TestFloat a => BinOp -> Gen (FloatBits a, FloatBits a)
genNearOverflow op = do
  offset <- genOffset @a
  genNearTarget @a
    (e - 2, e)
    (0, e `quot` 2 + 1)
    (negate (e `quot` 2), -1)
    op
    (2 ^^ (e + 1) * (1 + offset))
 where
  e = maxExponent @a

-- | Odd number in @[2^n, 2^(n+1))@
genOdd :: Int -> Gen Integer
genOdd n
  | n == 0 = pure 1
  | otherwise = (\k -> 2 * k + 1) <$> Gen.integral (Range.constant (2 ^ (n - 1)) (2 ^ n - 1))

-- | Unbiased exponent of an operand, mostly such that results of operations
-- on two of them are normal numbers
genOperandExponent :: forall a. TestFloat a => Gen Int
genOperandExponent =
  Gen.frequency
    [ (3, Gen.int (Range.constant (negate small) small))
    , (1, Gen.int (Range.constant (negate large) large))
    ]
 where
  large = maxExponent @a `quot` 2
  small = min (significandWidth @a) large

-- | @m * 2^e@ with a random sign
signed :: TestFloat a => Integer -> Int -> Gen (FloatBits a)
signed m e = do
  negative <- Gen.bool
  pure (floatBits (encodeFloat (if negative then negate m else m) e))

{- | Operands for which the exact result is (close to) halfway between two
numbers of the precision, where rounding to nearest even decides.

* Addition and subtraction: a number @x@ and a number that is an odd multiple
  of half the ulp of @x@, or adjacent to one. Their sum is halfway, or close to
  it, if it has the exponent of @x@.

* Multiplication: products of significands @a * 2^i@ and @b * 2^j@, with @a@
  and @b@ odd, are halfway if @i + j@ is the number of bits rounded off minus
  one.

* Division: quotients are never halfway, so this generates quotients very
  close to halfway: @mx / my = (k + r / my) / 2^(p+1)@, for odd @k@ and a small
  odd @r@.
-}
genHalfway :: forall a. TestFloat a => BinOp -> Gen (FloatBits a, FloatBits a)
genHalfway op = case op of
  Add -> genAddHalfway
  Sub -> genAddHalfway
  Mul -> do
    (mx, my) <- Gen.just $ do
      -- Products of two significands of p bits have 2p-1 or 2p bits, of which
      -- p-1 or p are rounded off
      long <- Gen.bool
      let roundedOff = if long then p else p - 1
      i <- Gen.int (Range.constant 0 (roundedOff - 1))
      let j = roundedOff - 1 - i
      mx <- (* 2 ^ i) <$> genOdd (p - 1 - i)
      my <- (* 2 ^ j) <$> genOdd (p - 1 - j)
      pure $
        if (mx * my >= 2 ^ (2 * p - 1)) == long
          then Just (mx, my)
          else Nothing
    signedPair mx my
  Div -> do
    (mx, my) <- Gen.just $ do
      my <- genOdd (p - 1)
      r <- Gen.element [-3, -1, 1, 3]
      let
        modulus = 2 ^ (p + 1) :: Integer
        k = (negate r * inverse my) `mod` modulus
        (mx, rest) = (k * my + r) `divMod` modulus
      pure $
        if k < 2 ^ p || rest /= 0 || mx < 2 ^ (p - 1) || mx >= 2 ^ p
          then Nothing
          else Just (mx, my)
    signedPair mx my
 where
  p = significandWidth @a

  -- Inverse of an odd number modulo 2^(p+1), by Newton's iteration: every
  -- step doubles the number of correct bits, starting with 3.
  inverse :: Integer -> Integer
  inverse m = iterate (\v -> (v * (2 - m * v)) `mod` (2 ^ (p + 1))) m !! 6

  -- Operands with the given significands, mostly with a normal result
  signedPair mx my = do
    ex <- genOperandExponent @a
    ey <- genOperandExponent @a
    x <- signed mx (ex - (p - 1))
    y <- signed my (ey - (p - 1))
    pure (x, y)

  genAddHalfway = do
    FloatBits x <- genNormal @a
    let
      -- Exponent of the ulp of x
      ulpExponent = biasedExponent @a x - bias @a - fractionWidth @a
    j <- Gen.integral (Range.constant 0 7)
    k <- Gen.int (Range.constant 0 (fractionWidth @a - 4))
    d <- Gen.element [0, 0, -1, 1]
    let c = (2 * j + 1) * 2 ^ k + d
    y <- signed c (ulpExponent - 1 - k)
    Gen.element [(FloatBits x, y), (y, FloatBits x)]

-- | Operands for the binary operations: a mix of all of the above
genBinaryOperands :: forall a. TestFloat a => BinOp -> Gen (FloatBits a, FloatBits a)
genBinaryOperands op =
  Gen.frequency
    [ (5, (,) <$> genFloat <*> genFloat)
    , (2, genRelated)
    , (3, genNearMinNormal op)
    , (2, genNearOverflow op)
    , (2, genHalfway op)
    ]

-- | Operands for comparison: a mix of independent and related numbers, and
-- zeros and subnormals (which compare equal).
genCompareOperands :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a)
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
      , (\s d -> mkFloat s 1 d) <$> Gen.bool <*> Gen.word64 (Range.constant 0 4)
      ]

-- | Operations that give NaN: an operand is NaN, or the operation is invalid
genNaNOperation :: forall a. TestFloat a => Gen (BinOp, (FloatBits a, FloatBits a))
genNaNOperation =
  Gen.choice
    [ (,) <$> Gen.element [Add, Sub, Mul, Div] <*> eitherOrder genNaN genFloat
    , (\s -> (Add, (inf s, inf (not s)))) <$> Gen.bool
    , (\s -> (Sub, (inf s, inf s))) <$> Gen.bool
    , (,) Mul <$> eitherOrder zeroish genInfinity
    , (,) Div <$> ((,) <$> zeroish <*> zeroish)
    , (,) Div <$> ((,) <$> genInfinity <*> genInfinity)
    ]
 where
  inf s = mkFloat s (maxBiasedExponent @a + 1) 0
  -- Zeros and subnormal numbers, which the IP flushes to zero
  zeroish = Gen.choice [genZero, genSubnormal]
  eitherOrder genA genB = do
    a <- genA
    b <- genB
    Gen.element [(a, b), (b, a)]

{- | Products just below the smallest normal number, see
'justBelowMinNormal'. With @x = mx * 2^ex@ and
@y = my * 2^ey@ for integer significands @mx@ and @my@ of @p@ bits, the exact
product is in the window iff @mx * my@ is in
@[2^(2p-1) - 2^(p-1), 2^(2p-1) - 2^(p-2))@ and @ex + ey = e - 2p + 1@.
-}
genMulWindow :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a)
genMulWindow =
  genSignificandsWithProduct @a
    (2 ^ (2 * p - 1) - 2 ^ (p - 1))
    (2 ^ (2 * p - 1) - 2 ^ (p - 2))
    >>= placeNearMinNormal @a
 where
  p = significandWidth @a

{- | Products that round up to the smallest normal number, see
'roundsUpToMinNormal'. Like 'genMulWindow', but with @mx * my@ in
@[2^(2p-1) - 2^(p-2), 2^(2p-1))@, including products exactly at its lower
bound ('minNormalTies'). Quotients never land in this window: quotients
below a power of two @2^k@ are more than @2^(k-p-1)@ below it.
-}
genMulRoundsUp :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a)
genMulRoundsUp =
  Gen.frequency
    [ (3, genSignificandsWithProduct @a (2 ^ (2 * p - 1) - 2 ^ (p - 2)) (2 ^ (2 * p - 1)))
    , (1, Gen.element (minNormalTies @a))
    ]
    >>= placeNearMinNormal @a
 where
  p = significandWidth @a

-- | Pair of significands of @p@ bits whose product lies in @[lo, hi)@. Picks
-- one and computes the smallest other one that makes the product at least
-- @lo@, so the interval must be wide enough for that to often work.
genSignificandsWithProduct :: forall a. TestFloat a => Integer -> Integer -> Gen (Integer, Integer)
genSignificandsWithProduct lo hi =
  Gen.filter (\(mx, my) -> mx * my < hi && my < 2 ^ p) $ do
    mx <- Gen.integral (Range.constant (2 ^ (p - 1)) (2 ^ p - 1))
    pure (mx, (lo + mx - 1) `quot` mx)
 where
  p = significandWidth @a

-- | Operands with the given significands of @p@ bits, whose exact product is
-- @mx * my * 2^(e - 2p + 1)@: close to the smallest normal number @2^e@ if
-- @mx * my@ is close to @2^(2p-1)@.
placeNearMinNormal :: forall a. TestFloat a => (Integer, Integer) -> Gen (FloatBits a, FloatBits a)
placeNearMinNormal (mx, my) = do
  -- Both operands must be normal numbers
  ex <- Gen.int (Range.constant (e - p + 1) (negate p))
  let ey = e - 2 * p + 1 - ex
  x <- signed mx ex
  y <- signed my ey
  Gen.element [(x, y), (y, x)]
 where
  e = minExponent @a
  p = significandWidth @a

{- | All pairs of significands of @p@ bits whose product is
@2^(2p-1) - 2^(p-2)@. Multiplying numbers with these significands can give
exactly @2^e - 2^(e-p-1)@, halfway between the smallest normal number and the
number below it. Then @mx = a * 2^i@ and @my = b * 2^j@ with @i + j = p - 2@
and @a * b = 2^(p+1) - 1@, so we find them from the divisors of @2^(p+1) - 1@.
-}
minNormalTies :: forall a. TestFloat a => [(Integer, Integer)]
minNormalTies
  | product factors /= n = error ("minNormalTieFactors: not the prime factors of " <> show n)
  | otherwise =
      [ (mx, my)
      | a <- nub (map product (subsequences factors))
      , i <- [0 .. p - 2]
      , let
          mx = a * 2 ^ i
          my = (n `div` a) * 2 ^ (p - 2 - i)
      , all isSignificand [mx, my]
      ]
 where
  p = significandWidth @a
  n = 2 ^ (p + 1) - 1
  factors = minNormalTieFactors @a
  isSignificand m = 2 ^ (p - 1) <= m && m < 2 ^ p

{- | Quotients just below the smallest normal number, see
'justBelowMinNormal'. There is only one pair of
significands for which this happens: the dividend's is all ones and the
divisor's is one, and then @x / y = 2^e - 2^(e-p)@ exactly.
-}
genDivWindow :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a)
genDivWindow = do
  ey <- Gen.int (Range.constant (2 - p) (maxExponent @a - p + 1))
  x <- signed (2 ^ p - 1) (ey + minExponent @a - 1)
  y <- signed (2 ^ (p - 1)) ey
  pure (x, y)
 where
  p = significandWidth @a

-- | Integers, biased towards rounding edge cases in single precision: values
-- just below, at and just above halfway between two floats.
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

-- | 64-bit integers, biased towards rounding edge cases in single and double
-- precision: values just below, at and just above halfway between two
-- numbers.
genWord64 :: Gen Word64
genWord64 =
  Gen.frequency
    [ (3, Gen.word64 Range.constantBounded)
    , (2, Gen.word64 Range.exponentialBounded)
    , (3, genRoundingCase 24)
    , (3, genRoundingCase 53)
    , ( 1
      , Gen.element
          [ 0, 1, 2 ^ (24 :: Int), 2 ^ (24 :: Int) + 1, 2 ^ (53 :: Int)
          , 2 ^ (53 :: Int) + 1, 2 ^ (63 :: Int), 2 ^ (63 :: Int) + 1
          , maxBound, maxBound - 1 ]
      )
    ]
 where
  -- A significand of p bits, shifted left by k bits, plus a remainder of k bits
  genRoundingCase p = do
    k <- Gen.int (Range.constant 1 (64 - p))
    m <- Gen.word64 (Range.constant (2 ^ (p - 1)) (2 ^ p - 1))
    let half = 2 ^ (k - 1)
    r <- Gen.element [half - 1, half, half + 1, 0, 1, 2 * half - 1]
    pure ((m `shiftL` k) + r)

genInt64 :: Gen Int64
genInt64 = do
  w <- genWord64
  Gen.element
    [ fromIntegral w
    , fromIntegral (w `shiftR` 1)
    , negate (fromIntegral (w `shiftR` 1))
    , fromIntegral (complement w)
    , minBound
    ]

{- | Inputs of the square root. Besides numbers of all classes, these are
positive normal numbers, exact squares, and numbers whose square root is very
close to halfway between two numbers of the precision.

Square roots are never exactly halfway. To get very close, pick an odd
significand @s@ of @p+1@ bits, the significand of a number halfway between
two numbers of the precision, and a small @c = 1 (mod 8)@. If @s^2 = c (mod
2^t)@, where @t@ is the number of bits of @s^2@ beyond @p@, then @s^2 - c@ has
at most @p@ significant bits, and its square root is within about @c / (2s)@ of
@s@: very close to halfway. Such an @s@ is a square root of @c@ modulo @2^t@.
-}
genSqrtOperand :: forall a. TestFloat a => Gen (FloatBits a)
genSqrtOperand =
  Gen.frequency
    [ (4, genFloat)
    , (3, positive <$> genNormal)
    , (1, genSquare)
    , (3, genNearHalfway)
    ]
 where
  p = significandWidth @a
  positive :: FloatBits a -> FloatBits a
  positive (FloatBits w) = FloatBits (clearBit w (totalWidth @a - 1))

  -- A number with an even exponent, so that its square root is a power of two
  -- times the square root of the integer. The integer must have at most 'p'
  -- significant bits.
  withEvenExponent :: Integer -> Gen (FloatBits a)
  withEvenExponent m = do
    let n = bitLength m
    -- The unbiased exponent of the result is about n - 1 + 2k
    k <- Gen.int (Range.constant ((minExponent @a - n + 2) `div` 2) ((maxExponent @a - n) `div` 2))
    pure (floatBits (encodeFloat m (2 * k)))

  genSquare = do
    -- Squares of odd numbers of at most (p - 2) / 2 + 1 bits have at most p
    -- bits
    m <- genOdd =<< Gen.int (Range.constant 0 ((p - 2) `div` 2))
    withEvenExponent (m * m)

  genNearHalfway = do
    (s, c) <- Gen.just $ do
      t <- Gen.element [p + 1, p + 2]
      j <- Gen.integral (Range.constant (-4) 4)
      let c = 8 * j + 1
      root <- Gen.element (sqrtsMod2 c t)
      pure $
        if root >= 2 ^ p && root < 2 ^ (p + 1) && bitLength (root * root - c) <= p + t
          then Just (root, c)
          else Nothing
    withEvenExponent (s * s - c)

  bitLength :: Integer -> Int
  bitLength = length . takeWhile (> 0) . iterate (`shiftR` 1)

{- | All square roots of @c@ modulo @2^t@, for @c = 1 (mod 8)@ and @t >= 3@.
Starts with a square root modulo 8 and adds one bit at a time: if @r^2 = c
(mod 2^k)@ for @k >= 3@, then @r@ or @r + 2^(k-1)@ is a square root modulo
@2^(k+1)@.
-}
sqrtsMod2 :: Integer -> Int -> [Integer]
sqrtsMod2 c t = nub [x `mod` modulus | x <- [r, negate r, r + half, half - r]]
 where
  modulus = 2 ^ t
  half = 2 ^ (t - 1)
  r = foldl lift 1 [3 .. t - 1]
  lift x k
    | (x * x - c) `mod` (2 ^ (k + 1)) == 0 = x
    | otherwise = x + 2 ^ (k - 1)

{- | Inputs of the conversion from precision @a@ to precision @b@. Besides
numbers of all classes, conversions to a narrower precision get numbers close
to halfway between two numbers of precision @b@: anywhere in the range of @b@,
near and below its smallest normal number, and near its overflow threshold.
-}
genConvertOperand :: forall a b. (TestFloat a, TestFloat b) => Gen (FloatBits a)
genConvertOperand
  | pb < significandWidth @a =
      Gen.frequency
        [ (4, genFloat)
        , (4, genNearTie (minExponent @b - pb - 2) (maxExponent @b + 1))
        , (2, genNearTie (minExponent @b - 2) (minExponent @b + 1))
        , (2, genNearTie (maxExponent @b - 1) (maxExponent @b + 1))
        , (3, genNearBoundary)
        , (1, genNormalWithExponent (minExponent @b - 3) (maxExponent @b + 3))
        ]
  | otherwise = genFloat
 where
  pb = significandWidth @b
  eMin = minExponent @b
  eMax = maxExponent @b

  -- Within a few units in the last place of the boundaries of the windows
  -- near the smallest normal number of precision b (see 'justBelowMinNormal'
  -- and 'roundsUpToMinNormal'), of the smallest normal number itself, and of
  -- the largest finite number of precision b and the threshold above which
  -- numbers round to infinity
  genNearBoundary = do
    t <-
      Gen.element
        [ 2 ^^ eMin - 2 ^^ (eMin - pb)
        , 2 ^^ eMin - 2 ^^ (eMin - pb - 1)
        , 2 ^^ eMin
        , 2 ^^ (eMax + 1) - 2 ^^ (eMax + 1 - pb)
        , 2 ^^ (eMax + 1) - 2 ^^ (eMax - pb)
        ]
    negative <- Gen.bool
    d <- Gen.integral (Range.constant (-3) 3)
    pure (nudge d (approx (if negative then negate t else t)))

  -- A number of precision a, with an unbiased exponent in the given range,
  -- within a few units in the last place of halfway between two numbers of
  -- precision b (as if its exponent were unbounded)
  genNearTie lo hi = do
    e <- Gen.int (Range.constant (max lo (minExponent @a)) (min hi (maxExponent @a)))
    m <-
      Gen.frequency
        [ (3, Gen.integral (Range.constant (2 ^ (pb - 1)) (2 ^ pb - 1)))
        , (1, Gen.element [2 ^ (pb - 1), 2 ^ (pb - 1) + 1, 2 ^ pb - 2, 2 ^ pb - 1])
        ]
    x <- signed (2 * m + 1) (e - pb)
    d <- Gen.integral (Range.constant (-3) 3)
    pure (nudge d x)

-- | Inputs of the conversion to @Signed 32@: numbers of all classes, numbers
-- close to halfway between two integers, and numbers close to the bounds.
genToS32Operand :: forall a. TestInteger a => Gen (FloatBits a)
genToS32Operand =
  Gen.frequency
    [ (3, genFloat)
    , (3, genNormalWithExponent (-3) 33)
    , (3, genNearHalfInteger)
    , (2, genNearBound)
    ]
 where
  p = significandWidth @a
  -- Close to @k + 1/2@, for an integer @k@ of fewer than 'p' bits
  genNearHalfInteger = do
    n <- Gen.int (Range.constant 0 (min 31 (p - 1)))
    k <- Gen.integral (Range.constant 0 (2 ^ n - 1 :: Integer))
    x <- signed (2 * k + 1) (-1)
    d <- Gen.integral (Range.constant (-2) 2)
    pure (nudge d x)
  -- Close to the bounds of @Signed 32@
  genNearBound = do
    t <- Gen.element [2 ^ (31 :: Int), 2 ^ (31 :: Int) - 1 / 2, 2 ^ (31 :: Int) - 1, 2 ^ (31 :: Int) + 1 / 2, 2 ^ (31 :: Int) + 1]
    negative <- Gen.bool
    d <- Gen.integral (Range.constant (-3) 3)
    pure (nudge d (approx (if negative then negate t else t)))
