{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Differential tests of the simulation models in "Clash.Cores.Xilinx.Floating"
  and "Clash.Cores.Xilinx.Floating.Generic.Explicit" against the bit-accurate C
  model of the Xilinx Floating-Point LogiCORE IP v7.1. Covers addition,
  subtraction, multiplication, division, comparison (condition code), square
  root, absolute value, conversion to every precision, reciprocal, reciprocal
  square root, exponential, natural logarithm, fused multiply-add and the
  accumulator in half, single and double precision, and conversions from
  @Unsigned 32@, @Signed 32@, @Unsigned 64@ and @Signed 64@ to single and double
  precision and from those to @Signed 32@ (the IP cannot convert between
  integers and half precision), all with round to nearest even, the
  configuration the modules instantiate. The tests run the actual functions in
  Clash simulation and compare the bits of their output with the output of the
  C model. Single precision mostly runs the functions from
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

  Some NaN checks compare with the driver or the models rather than with the
  IP: the half precision NaN @0x7E00@ is the driver's conversion, and so are
  the NaN payloads of the half precision absolute value (see the C file). The
  tests of the fused multiply-add and the accumulator replace every NaN from
  the C model by @xilinxNaN@ ('normaliseNaN'), so they only check that the
  models give @xilinxNaN@ where the C model gives a NaN. The HDL tests check
  these NaNs against the IP.

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

  = Reciprocal, reciprocal square root, exponential and logarithm

  The IP computes these only to an accuracy of one unit in the last place
  (except for the reciprocal and reciprocal square root in half precision,
  which it rounds correctly). The models reproduce the IP's algorithms, so the
  tests check that ('checkElementary'):

  * all results match the C model bit for bit;

  * the results of special cases (NaN, zero, infinity and negative inputs, the
    logarithm of one) are as expected;

  * the reciprocal and reciprocal square root in half precision are correctly
    rounded, which the test checks independently with exact arithmetic: the
    exact result must lie between the numbers halfway to the neighbours of the
    model's result.

  The tests on every half precision input report how many finite, non-zero
  results they compared.

  = Fused multiply-add

  The fused multiply-add is compared bit for bit. Besides numbers of every
  class, it gets operands whose exact result cancels (exactly or nearly), lies
  (close to) halfway between two numbers, has a product exactly halfway whose
  tie the addend breaks, lies near or in the windows just below the smallest
  normal number or near the overflow threshold, has a product beyond the
  largest finite number while the result is not, or depends on flushing a
  subnormal operand; and the special cases, see 'genFmaOperands'.

  = Accumulator

  The C model's accumulator works on a sequence of inputs; the driver starts a
  new accumulation after each input with its last flag set. The tests run the
  accumulator with random, valid bounds ('genAccumConfig') on random sequences
  with random last flags and deasserted enables ('genAccumCycles'), and compare
  every output and the flags the IP outputs (invalid operation, accumulator
  input overflow and accumulator overflow) with the C model, see
  'prop_accum'.

  = Exception flags

  For the other operations, the flags follow from the inputs and the result
  (see the documentation of "Clash.Cores.Xilinx.Floating.Generic"). The
  @flags@ tests check these rules against the C model's exception flags,
  using the C model's results, for every operation in every precision
  ('derivedFlags').
-}
module Test.Cores.Xilinx.Floating (mkTests) where

import Prelude

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, catch, onException, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits (bit, complement, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Char (toUpper)
import Data.Int (Int32, Int64)
import Data.List (isPrefixOf, nub, subsequences)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Ratio (denominator, numerator)
import Data.String (fromString)
import Data.Word (Word32, Word64)
import GHC.Float
  ( castDoubleToWord64, castFloatToWord32, castWord32ToFloat
  , castWord64ToDouble )
import GHC.Num.Integer (integerLog2)
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
  , getProcessExitCode, readProcessWithExitCode, waitForProcess )

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
          [ precisionTests @Float getPrepared (integerTests @Float getPrepared <> [integerFlags @Float getPrepared])
          , precisionTests @Half getPrepared (exhaustiveHalfTests getPrepared)
          , precisionTests @Double getPrepared (integerTests @Double getPrepared <> [integerFlags @Double getPrepared])
          ]
 where
  groupName = "Xilinx floating point vs. C model"
  integerFlags :: forall a. TestInteger a => IO Prepared -> TestTree
  integerFlags = testGroup "flags of integer conversions" . integerFlagTests @a

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
    , property "recip" "recip" (prop_elementary @a Recip)
    , property "rsqrt" "rsqrt" (prop_elementary @a RSqrt)
    , property "exp" "exp" (prop_elementary @a Exp)
    , property "log" "log" (prop_elementary @a Log)
    , property "fma" "fma" (prop_fma @a)
    , property "accum" "accum" (prop_accum @a)
    , testGroup "flags" (flagTests @a getPrepared)
    ]
      <> extraTests
      <> [ property "NaN" "nan" (prop_nan @a)
         , belowMinNormalTest Mul (genMulWindow @a)
         , belowMinNormalTest Div (genDivWindow @a)
         , roundsUpTest
         ]
 where
  property = driverProperty @a getPrepared

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
  map (\c -> property (fromIntName c) (prop_fromInt c)) (fromIntConversions @a)
    <> [property "toS32" (prop_toS32 @a)]
 where
  property name = driverProperty @a getPrepared name name

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
    <> map exhaustiveElementary [minBound .. maxBound]
 where
  -- Also counts the inputs with a finite, non-zero result
  exhaustiveElementary op =
    withDriver getPrepared $ \getDriver ->
      testCaseInfo (elemOpName op <> ", every input") $ do
        driver <- getDriver
        results <- forM [0 .. wordMask @Half] $ \w -> do
          (cmodel, _) <- query driver (precisionName @Half) (elemOpName op) w 0
          let haskell = toWord (modelElementary op (fromWord w :: Half))
          pure (w, checkElementary @Half op (fromWord w) haskell cmodel)
        let
          failures = [(w, msg) | (w, Left msg) <- results]
          -- Inputs that are no special case, with a finite, non-zero result
          rounded =
            length
              [ ()
              | w <- [0 .. wordMask @Half]
              , isNothing (specialElementary op (condition (fromWord w :: Half)))
              , not (isZeroOrInfinite (modelElementary op (fromWord w :: Half)))
              ]
        case failures of
          [] ->
            pure $
              "All " <> show rounded <> " finite, non-zero results of inputs that are no special case match"
                <> " the C model"
          (w, msg) : _ ->
            assertFailure $
              show (length failures) <> " failures. The first: " <> elemOpName op <> " "
                <> showBits @Half w <> ": " <> msg
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

-- | A property of precision @a@ with its own instance of the driver, given the
-- name of the test and the name to rerun it by ('propertyName')
driverProperty ::
  forall a.
  TestFloat a =>
  IO Prepared -> TestName -> String -> (IO Driver -> Property) -> TestTree
driverProperty getPrepared name propName prop =
  withDriver getPrepared $ \getDriver ->
    testPropertyNamed name (propertyName @a propName) (prop getDriver)

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
  , driverPipes :: MVar (Either String (Handle, Handle))
  -- ^ The driver's stdin and stdout, or why it can no longer be used
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
  pipes <- newMVar (Right (hIn, hOut))
  pure Driver{driverProcess = ph, driverPipes = pipes}

stopDriver :: Driver -> IO ()
stopDriver Driver{driverProcess, driverPipes} = do
  -- A failed request closed stdin already
  withMVar driverPipes (mapM_ (hClose . fst))
  void (waitForProcess driverProcess)

-- | Run tests with their own instance of the driver
withDriver :: IO Prepared -> (IO Driver -> TestTree) -> TestTree
withDriver getPrepared = withResource (getPrepared >>= startDriver) stopDriver

{- | Send a request, one line, to the driver and read its reply. If that fails,
for example because the C model crashed, this and every later request to this
driver fail with the request and the driver's exit code. So when Hedgehog
shrinks a counterexample after such a failure, it still shows the request that
failed first.
-}
request :: Driver -> String -> IO String
request Driver{driverProcess, driverPipes} line = do
  reply <- modifyMVar driverPipes $ \case
    Left failure -> pure (Left failure, Left failure)
    Right (hIn, hOut) ->
      try (hPutStr hIn (line <> "\n") >> hFlush hIn >> hGetLine hOut) >>= \case
        Right reply -> pure (Right (hIn, hOut), Right reply)
        Left (e :: IOException) -> do
          void (try @IOException (hClose hIn))
          code <- exitCodeWithin driverProcess
          let
            failure =
              "The C model driver failed on the request " <> show line <> " (" <> show e
                <> "), exit code: " <> maybe "none, it is still running" show code
          pure (Left failure, Left failure)
  either fail pure reply

-- | The exit code of a process, if it exits within a second
exitCodeWithin :: ProcessHandle -> IO (Maybe ExitCode)
exitCodeWithin ph = go (10 :: Int)
 where
  go n =
    getProcessExitCode ph >>= \case
      Nothing | n > 0 -> threadDelay 100_000 >> go (n - 1)
      code -> pure code

-- | Ask the C model. Returns the result and the exception flags.
query :: Driver -> String -> String -> Word64 -> Word64 -> IO (Word64, Int)
query driver precision op a b = queryOps driver precision op [a, b]

-- | Ask the C model, for an operation with any number of operands (the driver
-- expects two, except for @fma@, which has three). Returns the result and the
-- exception flags.
queryOps :: Driver -> String -> String -> [Word64] -> IO (Word64, Int)
queryOps driver precision op operands = do
  response <- request driver (unwords (precision : op : map (`showHex` "") operands))
  case words response of
    [readHex -> [(result, "")], read -> exceptions] -> pure (result, exceptions)
    _ -> fail ("Unexpected response from C model driver: " <> show response)

-- | Run a sequence of inputs, with their last flags, through the C model's
-- accumulator with the given bounds. Returns the result and the exception
-- flags for every input.
queryAccum :: Driver -> String -> G.AccumConfig -> [(Word64, Bool)] -> IO [(Word64, Int)]
queryAccum driver precision G.AccumConfig{..} samples = do
  response <-
    request driver . unwords $
      [ precision, "accum", show accumMsb, show accumInputMsb, show accumLsb
      , show (length samples) ]
        <> concat [[showHex w "", if l then "1" else "0"] | (w, l) <- samples]
  let
    pairs (r : e : rest) = (,) <$> readHexWord r <*> readMaybeInt e >>= \p -> (p :) <$> pairs rest
    pairs [] = Just []
    pairs _ = Nothing
    readHexWord w = case readHex w of
      [(v, "")] -> Just v
      _ -> Nothing
    readMaybeInt e = case reads e of
      [(v, "")] -> Just v
      _ -> Nothing
  case pairs (words response) of
    Just results | length results == length samples -> pure results
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
  modelElementary :: ElemOp -> a -> a
  modelFma :: a -> a -> a -> a

  -- | Model of the accumulator with exception flags, with a latency of two
  -- cycles, run on the enables, last flags and inputs of every cycle. Gives the
  -- output of every cycle.
  modelAccum :: G.AccumConfig -> [Bool] -> [Bool] -> [a] -> [(a, G.AccumFlags)]

  -- | The default bounds of the accumulator, @defAccumConfig@
  modelAccumDefaults :: G.AccumConfig

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
  modelElementary op x = case op of
    Recip -> afterPipeline (F.recip clk en (constant x))
    RSqrt -> afterPipeline (F.rsqrt clk en (constant x))
    Exp -> afterPipeline (F.exp clk en (constant x))
    Log -> afterPipeline (F.log clk en (constant x))
  modelFma x y z = afterPipeline (F.fma clk en (constant x) (constant y) (constant z))
  modelAccum cfg = runAccum (F.accumWithFlags @2 cfg)
  modelAccumDefaults = F.defAccumConfig
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
  modelElementary op x = case op of
    Recip -> afterPipeline (HE.recip clk en (constant x))
    RSqrt -> afterPipeline (HE.rsqrt clk en (constant x))
    Exp -> afterPipeline (HE.exp clk en (constant x))
    Log -> afterPipeline (HE.log clk en (constant x))
  modelFma x y z = afterPipeline (HE.fma clk en (constant x) (constant y) (constant z))
  modelAccum cfg = runAccum (HE.accumWithFlags @2 cfg)
  modelAccumDefaults = HE.defAccumConfig
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
  modelElementary op x = case op of
    Recip -> afterPipeline (DE.recip clk en (constant x))
    RSqrt -> afterPipeline (G.rsqrt clk en (constant x))
    Exp -> afterPipeline (DE.exp clk en (constant x))
    Log -> afterPipeline (G.log clk en (constant x))
  modelFma x y z = afterPipeline (G.fma clk en (constant x) (constant y) (constant z))
  modelAccum cfg = runAccum (G.accumWithFlags @Double @2 cfg)
  modelAccumDefaults = DE.defAccumConfig
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

-- | Run an accumulator with a latency of two cycles on lists of enables, last
-- flags and inputs, one per cycle. The output is lazy: the outputs of the first
-- cycles are undefined.
runAccum ::
  C.NFDataX a =>
  ( Clock System ->
    Enable System ->
    DSignal System 0 Bool ->
    DSignal System 0 a ->
    DSignal System 2 (a, G.AccumFlags)
  ) ->
  [Bool] ->
  [Bool] ->
  [a] ->
  [(a, G.AccumFlags)]
runAccum f ens lasts xs =
  take (length xs) . C.sample_lazy . D.toSignal $
    f clk (C.toEnable (C.fromList (ens <> repeat False)))
      (D.fromSignal (C.fromList (lasts <> repeat False)))
      (D.fromSignal (C.fromList (xs <> repeat (last xs))))

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

-- | The largest finite number
maxFinite :: forall a. TestFloat a => Rational
maxFinite = 2 ^^ (maxExponent @a + 1) - 2 ^^ (maxExponent @a + 1 - significandWidth @a)

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
  adjust e
    | 2 ^^ e > r = adjust (e - 1)
    | 2 ^^ (e + 1) <= r = adjust (e + 1)
    | otherwise = e

-- | Number of bits of a positive integer, or zero
bitLength :: Integer -> Int
bitLength n
  | n <= 0 = 0
  | otherwise = fromIntegral (integerLog2 n) + 1

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

-- | A conversion from an integer type to precision @a@: the name of the
-- operation, as understood by the C driver, a generator of inputs, the bit
-- pattern of an input as the C driver expects it, and the model
data FromInt a = forall i. Show i => FromInt String (Gen i) (i -> Word64) (i -> a)

-- | The conversions from integers
fromIntConversions :: TestInteger a => [FromInt a]
fromIntConversions =
  [ FromInt "u32" genWord32 fromIntegral modelFromU32
  , FromInt "s32" genInt32 (fromIntegral . (fromIntegral :: Int32 -> Word32)) modelFromS32
  , FromInt "u64" genWord64 id modelFromU64
  , FromInt "s64" genInt64 fromIntegral modelFromS64
  ]

-- | Name of the test of a conversion from integers, like @fromU32@
fromIntName :: FromInt a -> String
fromIntName (FromInt op _ _ _) = "from" <> map toUpper op

prop_fromInt :: forall a. TestInteger a => FromInt a -> IO Driver -> Property
prop_fromInt (FromInt op gen bits model) getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  i <- H.forAll gen
  (cmodel, _) <- H.evalIO (query driver (precisionName @a) op (bits i) 0)
  floatBits (model i) === FloatBits cmodel

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

-- | Zeros and subnormal numbers, which the IP flushes to zero
genZeroish :: forall a. TestFloat a => Gen (FloatBits a)
genZeroish = Gen.choice [genZero, genSubnormal]

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

-- | A number with the given sign: negative if 'True'
withSign :: forall a. TestFloat a => Bool -> FloatBits a -> FloatBits a
withSign negative (FloatBits w)
  | negative = FloatBits (w .|. signBit @a)
  | otherwise = FloatBits (w .&. complement (signBit @a))

-- | A number with its sign bit cleared
positive :: TestFloat a => FloatBits a -> FloatBits a
positive = withSign False

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
    , (,) Mul <$> eitherOrder genZeroish genInfinity
    , (,) Div <$> ((,) <$> genZeroish <*> genZeroish)
    , (,) Div <$> ((,) <$> genInfinity <*> genInfinity)
    ]
 where
  inf s = mkFloat s (maxBiasedExponent @a + 1) 0
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

------------------------------------------------------------------------------
-- Reciprocal, reciprocal square root, exponential and logarithm
------------------------------------------------------------------------------

{- | Operations that the IP only computes to an accuracy of one unit in the last
place (except for 'Recip' and 'RSqrt' in half precision, which the IP rounds
correctly), and whose models reproduce the IP's algorithms
-}
data ElemOp = Recip | RSqrt | Exp | Log
  deriving (Eq, Show, Enum, Bounded)

-- | Name of the operation, as understood by the C driver
elemOpName :: ElemOp -> String
elemOpName = \case
  Recip -> "recip"
  RSqrt -> "rsqrt"
  Exp -> "exp"
  Log -> "log"

-- | Whether the operation in precision @a@ gives the correctly rounded results:
-- only the reciprocal and reciprocal square root in half precision
elemOpCorrectlyRounded :: forall a. TestFloat a => ElemOp -> Bool
elemOpCorrectlyRounded op = precisionName @a == "half" && op `elem` [Recip, RSqrt]

{- | Compare the model of an operation with the C model, see 'checkElementary',
and label the cases for the coverage report.
-}
prop_elementary :: forall a. TestFloat a => ElemOp -> IO Driver -> Property
prop_elementary op getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  a <- H.forAll (genElemOperand @a op)
  let
    x = bitsFloat a
    haskell = toWord (modelElementary op x)
  classifyOperand x
  (cmodel, exceptions) <- H.evalIO (query driver (precisionName @a) (elemOpName op) (toWord x) 0)
  H.footnote ("C model exception flags: " <> show exceptions)
  H.footnote ("C model: " <> show (FloatBits cmodel :: FloatBits a))
  H.footnote ("Haskell model: " <> show (FloatBits haskell :: FloatBits a))
  let special = isJust (specialElementary op (condition x))
  H.classify "special case" special
  H.classify "result rounded to zero or infinity" $
    not special && isZeroOrInfinite (fromWord haskell :: a)
  case checkElementary @a op x haskell cmodel of
    Left msg -> do
      H.annotate msg
      H.failure
    Right () -> pure ()

{- | Check the result of the model of an operation on an input against the C
model's result, and where the IP rounds correctly, check that it is the
correctly rounded result:

* NaN: the model must give @xilinxNaN@, and so must the C model.

* Other results: equal bit patterns.

* For special inputs, the model's result must be as in 'specialElementary'.
  Otherwise, if the operation is correctly rounded ('elemOpCorrectlyRounded'),
  the exact result must lie in the interval of numbers that round to it
  ('roundingInterval'), which is checked with exact arithmetic
  ('exactCompare').

Returns what is wrong, if anything.
-}
checkElementary :: forall a. TestFloat a => ElemOp -> a -> Word64 -> Word64 -> Either String ()
checkElementary op x haskell cmodel = do
  againstCModel
  correctlyRounded
 where
  nan = toWord (modelNaN @a)
  y = fromWord haskell :: a
  cy = fromWord cmodel :: a
  cx = condition x

  againstCModel
    | isNaN y || isNaN cy =
        unless (haskell == nan && cmodel == nan) $
          Left "NaN results differ"
    | otherwise = unless (haskell == cmodel) $ Left "results differ from the C model"

  correctlyRounded = case specialElementary op cx of
    Just expected ->
      unless (toWord expected == haskell) $
        Left ("special case: expected " <> show (floatBits expected))
    Nothing
      | isNaN y -> Left "NaN for an input that is no special case"
      | not (elemOpCorrectlyRounded @a op) -> pure ()
      | otherwise -> do
          let
            r = toRational cx
            (lo, hi) = roundingInterval @a y
          forM_ lo $ \b ->
            unless (exactCompare op r b == GT) $
              Left ("not correctly rounded: the exact result is not above " <> show (fromRational b :: Double))
          forM_ hi $ \b ->
            unless (exactCompare op r b == LT) $
              Left ("not correctly rounded: the exact result is not below " <> show (fromRational b :: Double))

isZeroOrInfinite :: RealFloat a => a -> Bool
isZeroOrInfinite z = z == 0 || isInfinite z

-- | Condition an input like the IP does: subnormal numbers become a zero of
-- the same sign
condition :: RealFloat a => a -> a
condition z
  | isDenormalized z = if z < 0 then -0 else 0
  | otherwise = z

{- | The result of an operation for an input that the IP treats as a special
case (after 'condition'), or 'Nothing' if the exact result needs rounding.
These follow IEEE 754.
-}
specialElementary :: forall a. TestFloat a => ElemOp -> a -> Maybe a
specialElementary op x
  | isNaN x = Just (modelNaN @a)
  | otherwise = case op of
      Recip
        | x == 0 -> Just (if isNegativeZero x then -inf else inf)
        | isInfinite x -> Just (if x < 0 then -0 else 0)
      RSqrt
        | x == 0 -> Just (if isNegativeZero x then -inf else inf)
        | x < 0 -> Just (modelNaN @a)
        | isInfinite x -> Just 0
      Exp
        | x == 0 -> Just 1
        | isInfinite x -> Just (if x > 0 then inf else 0)
      Log
        | x == 0 -> Just (-inf)
        | x < 0 -> Just (modelNaN @a)
        | isInfinite x -> Just inf
        | x == 1 -> Just 0
      _ -> Nothing
 where
  inf = 1 / 0

{- | The open interval of exact results that the IP's rounding takes to the
given result: round to the precision as if the exponent were unbounded, then
flush results below the smallest normal number to a zero of the same sign, and
results beyond the largest finite number to an infinity of the same sign.
'Nothing' stands for an unbounded side. (Exact results are never halfway
between two numbers for these operations, so whether the bounds belong to the
interval does not matter.)
-}
roundingInterval :: forall a. TestFloat a => a -> (Maybe Rational, Maybe Rational)
roundingInterval y
  | isInfinite y = if y > 0 then (Just big, Nothing) else (Nothing, Just (negate big))
  | y == 0 = if isNegativeZero y then (Just (negate small), Just 0) else (Just 0, Just small)
  | y > 0 = (Just lo, Just hi)
  | otherwise = (Just (negate hi), Just (negate lo))
 where
  p = significandWidth @a
  e = minExponent @a
  -- Halfway between the smallest normal number and the number below it, with
  -- the precision's number of significant bits
  small = 2 ^^ e - 2 ^^ (e - p - 1)
  -- Halfway between the largest finite number and 2^(emax+1)
  big = 2 ^^ (maxExponent @a + 1) - 2 ^^ (maxExponent @a - p)
  (m, ex) = decodeFloat (abs y)
  -- Below a power of two, the numbers are twice as close together
  lo = (fromInteger m - (if m == 2 ^ (p - 1) then 1 / 4 else 1 / 2)) * 2 ^^ ex
  hi = (fromInteger m + 1 / 2) * 2 ^^ ex

{- | Compare the exact result of an operation on a finite, non-zero input
@r@, which is not a special case, with a number @b@, exactly.
-}
exactCompare :: ElemOp -> Rational -> Rational -> Ordering
exactCompare op r b = case op of
  Recip -> compare (1 / r) b
  -- 1 / sqrt r > b  <=>  b <= 0 or 1 / r > b^2
  RSqrt
    | b <= 0 -> GT
    | otherwise -> compare 1 (b * b * r)
  _ -> error "exactCompare: the IP does not round this operation correctly"

{- | Inputs of the operations: numbers of every class, and per operation:

* 'Recip': numbers with reciprocals close to the smallest normal number, and
  powers of two (exact reciprocals).

* 'RSqrt': positive numbers, with even and odd exponents, and powers of two
  (exact for even exponents).

* 'Exp': numbers with finite, non-zero exponentials, numbers close to the
  thresholds of overflow and of flushing to zero, and tiny numbers with
  exponentials close to one, near the numbers next to one and halfway to them.

* 'Log': positive numbers, numbers close to one (with logarithms that need
  many more bits) and powers of two.
-}
genElemOperand :: forall a. TestFloat a => ElemOp -> Gen (FloatBits a)
genElemOperand op =
  Gen.frequency $
    [ (4, genFloat)
    , (2, genNormal)
    ]
      <> case op of
        Recip ->
          [ (3, genNormalWithExponent (negate eMin - 3) eMax)
          , (1, powerOfTwo (negate eMax) eMax)
          ]
        RSqrt ->
          [ (6, positive <$> genNormal)
          , (1, powerOfTwo eMin eMax)
          ]
        Exp ->
          [ (6, genNormalWithExponent (negate p - 3) (bitLength (toInteger eMax) - 1))
          , (2, near (fromIntegral (eMax + 1) * ln2) 16)
          , (2, near (fromIntegral eMin * ln2) 16)
          , (2, near (fromIntegral (eMin - 1) * ln2) 16)
          , (2, genNormalWithExponent (negate p - 4) (2 - p))
          , (1, (\k -> nudge k (floatBits (2 ^^ negate p :: a))) <$> Gen.int (Range.constant (-4) 4))
          ]
        Log ->
          [ (4, positive <$> genNormal)
          , (3, (\k -> nudge k (floatBits (1 :: a))) <$> Gen.int (Range.constant (-64) 64))
          , (2, nearOne)
          , (1, powerOfTwo eMin eMax)
          ]
 where
  p = significandWidth @a
  eMin = minExponent @a
  eMax = maxExponent @a
  ln2 = toRational (log 2 :: Double)
  -- 2^k, with a random sign, for k in the given range, clamped to the normal
  -- range
  powerOfTwo lo hi = do
    k <- Gen.int (Range.constant lo hi)
    signed 1 (max eMin (min eMax k))
  -- Within a few units in the last place of plus or minus t
  near :: Rational -> Int -> Gen (FloatBits a)
  near t n = do
    d <- Gen.int (Range.constant (negate n) n)
    s <- Gen.element [1, -1]
    pure (nudge d (approx (s * t)))
  -- 1 + r or 1 - r for a small r
  nearOne = do
    k <- Gen.int (Range.constant 1 (p + 4))
    r <- Gen.integral (Range.constant 0 (2 ^ p :: Integer))
    s <- Gen.element [1, -1]
    pure (approx (1 + s * fromInteger r / 2 ^ (p + k)))

------------------------------------------------------------------------------
-- Fused multiply-add
------------------------------------------------------------------------------

{- | Exact result of a fused multiply-add on finite operands, after flushing
subnormal operands to zero like the IP does.
-}
fmaExact :: RealFloat a => a -> a -> a -> Maybe Rational
fmaExact (flushSubnormal -> x) (flushSubnormal -> y) (flushSubnormal -> z)
  | any (\v -> isNaN v || isInfinite v) [x, y, z] = Nothing
  | otherwise = Just (toRational x * toRational y + toRational z)

-- | Any NaN from the C model as the model's NaN ('prop_nan' checks that they
-- are the same)
normaliseNaN :: forall a. TestFloat a => Word64 -> Word64
normaliseNaN w = if isNaN (fromWord @a w) then toWord (modelNaN @a) else w

{- | Compare the fused multiply-add with the C model, bit for bit. The coverage
report labels the hard cases.
-}
prop_fma :: forall a. TestFloat a => IO Driver -> Property
prop_fma getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  (a, b, c) <- H.forAll (genFmaOperands @a)
  let
    x = bitsFloat a
    y = bitsFloat b
    z = bitsFloat c
    exact = fmaExact x y z
    is f = maybe False f exact
    productR = toRational (flushSubnormal x) * toRational (flushSubnormal y)
    finite v = not (isNaN v || isInfinite v)
  H.classify "an operand is zero" (any (== 0) [x, y, z])
  H.classify "an operand is subnormal" (any isDenormalized [x, y, z])
  H.classify "an operand is infinite" (any isInfinite [x, y, z])
  H.classify "an operand is NaN" (any isNaN [x, y, z])
  H.classify "exact result zero" (is (== 0))
  H.classify "exact result zero, product not zero" (is (== 0) && productR /= 0)
  H.classify "cancellation: exact result below 2^-p times the product" $
    is (\r -> r /= 0 && abs r * 2 ^^ significandWidth @a < abs productR)
  H.classify "exact result near smallest normal number" (is (nearMinNormal @a))
  H.classify "exact result just below smallest normal number" (is (justBelowMinNormal @a))
  H.classify "exact result rounds up to smallest normal number" (is (roundsUpToMinNormal @a))
  H.classify "exact result near overflow threshold" (is (nearOverflow @a))
  H.classify "exact result rounds to infinity" (is (roundsToInfinity @a))
  H.classify "product beyond largest finite number, result finite" $
    all finite [x, y, z] && abs productR > maxFinite @a && is (not . roundsToInfinity @a)
  H.classify "exact result halfway between two numbers" (is (halfway @a))
  H.classify "exact result very close to halfway between two numbers" (is (nearHalfway @a))

  (cBits, exceptions) <-
    H.evalIO (queryOps driver (precisionName @a) "fma" [toWord x, toWord y, toWord z])
  H.footnote ("C model exception flags: " <> show exceptions)
  floatBits (modelFma x y z) === FloatBits (normaliseNaN @a cBits)

{- | Operands of the fused multiply-add: numbers of every class, and operands
for which the exact result cancels (to zero or almost), lies (close to)
halfway between two numbers, has a halfway product whose tie the addend breaks,
lies near the smallest normal number or near the overflow threshold, or for
which flushing a subnormal operand matters; and the special cases.
-}
genFmaOperands :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaOperands =
  Gen.frequency
    [ (4, (,,) <$> genFloat <*> genFloat <*> genFloat)
    , (3, genFmaCancel)
    , (2, genFmaZero)
    , (3, genFmaHalfway)
    , (2, genFmaTieBreak)
    , (2, genFmaNearTarget (2 ^^ minExponent @a) (minExponent @a - 2, minExponent @a + p + 2) (minExponent @a `quot` 2, 0))
    , (2, genFmaNearTarget (2 ^^ (maxExponent @a + 1)) (maxExponent @a - p - 2, maxExponent @a) (0, maxExponent @a `quot` 2 + 1))
    , (2, genFmaMinNormalWindows)
    , (1, genFmaBigProduct)
    , (1, genFmaSubnormal)
    , (1, genFmaSpecial)
    ]
 where
  p = significandWidth @a

{- | The exact result is in or near the windows just below the smallest normal
number @2^e@, where it is flushed to zero ('justBelowMinNormal') or rounded up
to @2^e@ ('roundsUpToMinNormal'): products in these windows (see
'genMulWindow' and 'genMulRoundsUp') plus a zero, and targets in steps of
@2^(e-p-3)@ from @2^e - 2^(e-p)@ up to @2^e@, reached approximately by a
product plus a normal addend.
-}
genFmaMinNormalWindows :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaMinNormalWindows =
  Gen.choice
    [ do
        (x, y) <- Gen.choice [genMulWindow @a, genMulRoundsUp @a]
        z <- genZero
        pure (x, y, z)
    , do
        k <- Gen.integral (Range.constant 0 8)
        negative <- Gen.bool
        let t0 = 2 ^^ e - 2 ^^ (e - p) + fromInteger k * 2 ^^ (e - p - 3)
            t = if negative then negate t0 else t0
        z <- genNormalWithExponent @a e (e + 3)
        x <- genNormalWithExponent @a (e `quot` 2) 0
        let y = approx @a ((t - toRational (bitsFloat z)) / toRational (bitsFloat x))
        d <- Gen.integral (Range.constant (-2) 2)
        swapFactors (x, nudge d y, z)
    ]
 where
  e = minExponent @a
  p = significandWidth @a

{- | A product beyond the largest finite number, between @2^(emax+1)@ and
@1.9 * 2^(emax+1)@, plus a finite addend of the opposite sign, which brings the
result back in range: close to a number of at least @2^(emax-2)@ with the sign
of the product.
-}
genFmaBigProduct :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaBigProduct = do
  ex <- Gen.int (Range.constant 1 (eMax - 1))
  x <- genNormalWithExponent @a ex ex
  r <- Gen.integral (Range.constant 0 (2 ^ p :: Integer))
  let target = 2 ^^ (eMax + 1) * (1 + 0.9 * fromInteger r / 2 ^ p)
      y = approx @a (target / toRational (abs (bitsFloat x)))
      productR = toRational (bitsFloat x) * toRational (bitsFloat y)
  t <- genNormalWithExponent @a (eMax - 2) eMax
  let
    -- The magnitude of the result, large enough that the addend is finite
    result = max (toRational (abs (bitsFloat t))) (abs productR - maxFinite @a)
    z = approx @a (signum productR * result - productR)
  swapFactors (x, y, z)
 where
  eMax = maxExponent @a
  p = significandWidth @a

-- | Shuffle the factors of a fused multiply-add
swapFactors :: (b, b, c) -> Gen (b, b, c)
swapFactors (x, y, z) = Gen.element [(x, y, z), (y, x, z)]

{- | An addend close to minus the product, or equal to it. Products of
significands with few bits are exact, so then the result can be exactly zero.
-}
genFmaCancel :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaCancel = do
  x <- Gen.choice [genNormalWithExponent (negate large) large, genShort]
  y <- Gen.choice [genNormalWithExponent (negate large) large, genShort]
  d <- Gen.frequency [(2, pure 0), (3, Gen.int (Range.constant (-3) 3))]
  let z = nudge d (approx @a (negate (toRational (bitsFloat x) * toRational (bitsFloat y))))
  pure (x, y, z)
 where
  large = maxExponent @a `quot` 2
  -- A number with at most half the significant bits of the precision
  genShort = do
    m <- genOdd =<< Gen.int (Range.constant 0 (significandWidth @a `quot` 2 - 1))
    e <- Gen.int (Range.constant (-20) 20)
    signed m e

-- | Zero products and addends (zeros and subnormal numbers, of both signs)
genFmaZero :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaZero =
  Gen.choice
    [ swapFactors =<< ((,,) <$> genZeroish <*> genFloat <*> genZeroish)
    , swapFactors =<< ((,,) <$> genZeroish <*> genNormal <*> genNormal)
    , swapFactors =<< ((,,) <$> genZeroish <*> genZeroish <*> genZeroish)
    ]

{- | The exact result is (close to) halfway between two numbers: the addend
@z = m * 2^e@, with an integer significand @m@ of @p@ bits, plus a product of
@(2j + 1) * 2^(e - 1)@, so that the sum is halfway if it has the exponent of
@z@. Nudging the first factor by an ulp makes the sum close to halfway.
-}
genFmaHalfway :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaHalfway = do
  (x, y, z) <- Gen.just $ do
    z <- genNormalWithExponent (minExponent @a + 2 * p) (maxExponent @a - 4)
    let (_, e) = decodeFloat (bitsFloat z)
    j <- Gen.integral (Range.constant 0 7)
    eb <- Gen.int (Range.constant (-8) 8)
    negative <- Gen.bool
    let
      xv = encodeFloat ((if negative then negate else id) (2 * j + 1)) (e - 1 - eb) :: a
      yv = encodeFloat 1 eb :: a
    pure $
      if isDenormalized xv || xv == 0 || isInfinite xv
        then Nothing
        else Just (floatBits xv, floatBits yv, z)
  d <- Gen.element [0, 0, -1, 1]
  swapFactors (nudge d x, y, z)
 where
  p = significandWidth @a

{- | A product exactly halfway between two numbers (see 'genHalfway'), plus a
small addend that breaks the tie, or a zero.
-}
genFmaTieBreak :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaTieBreak = do
  (x, y) <- genHalfway @a Mul
  let pr = abs (toRational (bitsFloat x) * toRational (bitsFloat y))
  k <- Gen.int (Range.constant 1 (2 * p + 4))
  s <- Gen.element [1, -1]
  z <-
    Gen.frequency
      [ (4, pure (approx (s * 2 ^^ (exponentOf pr - p - k))))
      , (1, genZero)
      ]
  pure (x, y, z)
 where
  p = significandWidth @a

{- | The exact result is close to plus or minus the target: an addend (or
zero) with an unbiased exponent in the given range, and a product close to the
target minus the addend, see 'genNearTarget'. The second range is that of the
exponent of the first factor.
-}
genFmaNearTarget ::
  forall a. TestFloat a => Rational -> (Int, Int) -> (Int, Int) -> Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaNearTarget t0 zExps xExps = do
  offset <- genOffset @a
  negative <- Gen.bool
  let t = (if negative then negate t0 else t0) * (1 + offset)
  z <- Gen.frequency [(1, genZero), (4, uncurry genNormalWithExponent zExps)]
  x <- uncurry genNormalWithExponent xExps
  let y = approx @a ((t - toRational (bitsFloat z)) / toRational (bitsFloat x))
  d <- Gen.integral (Range.constant (-3) 3)
  (x', y') <- Gen.element [(nudge d x, y), (x, nudge d y)]
  swapFactors (x', y', z)

{- | A subnormal operand where flushing it to zero matters: times a large
power of two, or as an addend to a product close to the smallest normal
number.
-}
genFmaSubnormal :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaSubnormal =
  Gen.choice
    [ do
        s <- genSubnormal
        k <- Gen.int (Range.constant p (maxExponent @a))
        big <- signed 1 k
        z <- Gen.choice [genZero, genNormalWithExponent (minExponent @a) (minExponent @a + p + 4)]
        swapFactors (s, big, z)
    , do
        x <- genNormalWithExponent (minExponent @a) (minExponent @a + 2)
        y <- Gen.element [floatBits (1 :: a), floatBits (-1 :: a), floatBits (0.5 :: a)]
        s <- genSubnormal
        swapFactors (x, y, s)
    ]
 where
  p = significandWidth @a

-- | NaN operands, infinities times zeros, and infinite products plus
-- infinities
genFmaSpecial :: forall a. TestFloat a => Gen (FloatBits a, FloatBits a, FloatBits a)
genFmaSpecial =
  Gen.choice
    [ swapFactors =<< ((,,) <$> genInfinity <*> genZeroish <*> genFloat)
    , swapFactors =<< ((,,) <$> genInfinity <*> genNormal <*> genInfinity)
    , swapFactors =<< ((,,) <$> genNormal <*> genNormal <*> genInfinity)
    , do
        (n, o1, o2) <- (,,) <$> genNaN <*> genFloat <*> genFloat
        Gen.element [(n, o1, o2), (o1, n, o2), (o1, o2, n)]
    ]

------------------------------------------------------------------------------
-- Accumulator
------------------------------------------------------------------------------

-- | Number of random sequences for the accumulator, unless overridden by
-- @--hedgehog-tests@
nTestsAccum :: H.TestLimit
nTestsAccum = 5_000

{- | Bounds of the accumulator that Vivado accepts (see
'Clash.Cores.Xilinx.Floating.Generic.Explicit.accumWith'), mostly narrow ones,
so that inputs get truncated and sums overflow, and the defaults.
-}
genAccumConfig :: forall a. TestFloat a => Gen G.AccumConfig
genAccumConfig =
  Gen.frequency
    [ (1, pure (modelAccumDefaults @a))
    , (6, genBounds)
    , (1, genLarge)
    , (1, genSmall)
    ]
 where
  emin = toInteger (minExponent @a)
  emax = toInteger (maxExponent @a)
  p = toInteger (significandWidth @a)
  genBounds = do
    lsb <-
      Gen.frequency
        [ (3, Gen.integral (Range.constant (emin - p + 1) (emax - 1)))
        , (3, Gen.integral (Range.constant (max (emin - p + 1) (-40)) (min (emax - 1) 20)))
        , (1, pure (emin - p + 1))
        ]
    imsb <- Gen.integral (Range.constant (lsb + 1) (min emax (lsb + 2 * p + 8)))
    let lo = max emin imsb
    msb <-
      Gen.frequency
        [ (3, Gen.integral (Range.constant lo (max lo (min (emax + 54) (imsb + 4)))))
        , (2, Gen.integral (Range.constant lo (max lo (min (emax + 54) (imsb + 2 * p)))))
        , (1, pure (emax + 54))
        ]
    mk msb lsb imsb
  -- Sums beyond the largest finite number
  genLarge = do
    lsb <- Gen.integral (Range.constant (emax - 2 * p) (emax - 1))
    msb <- Gen.integral (Range.constant (emax + 1) (min (emax + 54) (emax + 4)))
    mk msb lsb emax
  -- Sums below the smallest normal number
  genSmall = do
    lsb <- Gen.integral (Range.constant (emin - p + 1) (emin - 1))
    imsb <- Gen.integral (Range.constant (lsb + 1) (emin + 2))
    msb <- Gen.integral (Range.constant (max emin imsb) (emin + 4))
    mk msb lsb imsb
  mk msb lsb imsb = do
    dsp <- Gen.element [G.NoDspUsage, G.MediumDspUsage, G.FullDspUsage]
    pure G.AccumConfig{accumMsb = msb, accumLsb = lsb, accumInputMsb = imsb, accumDspUsage = dsp}

{- | The cycles of a test of the accumulator: enables, last flags and inputs.
The inputs are mostly numbers between the bounds of the accumulator, with
large numbers of one sign, which make it overflow, numbers below its least
significant bit, which get truncated, and sometimes input overflows,
infinities, NaNs, zeros and subnormal numbers.
-}
genAccumCycles :: forall a. TestFloat a => G.AccumConfig -> Gen [(Bool, Bool, FloatBits a)]
genAccumCycles G.AccumConfig{..} = do
  negative <- Gen.bool
  Gen.list (Range.linear 1 64) $ do
    enabled <- Gen.frequency [(6, pure True), (1, pure False)]
    l <- Gen.frequency [(9, pure False), (1, pure True)]
    x <-
      Gen.frequency
        [ (12, genNormalWithExponent @a (lsb - 3) imsb)
        , (4, fmap (withSign negative) (genNormalWithExponent @a (imsb - 2) imsb))
        , (2, genNormalWithExponent @a (lsb - p) (lsb - 1))
        , (2, genZero @a)
        , (1, genSubnormal @a)
        , (1, genNormalWithExponent @a (imsb + 1) (imsb + 3))
        , (1, genInfinity @a)
        , (1, genNaN @a)
        ]
    pure (enabled, l, x)
 where
  lsb = fromInteger accumLsb
  imsb = fromInteger accumInputMsb
  p = significandWidth @a

-- | The accumulator's flags as the C model's exception bits: invalid
-- operation (4), accumulator input overflow (32) and accumulator overflow (64)
accumFlagBits :: G.AccumFlags -> Int
accumFlagBits G.AccumFlags{..} =
  (if accumInvalid then 4 else 0)
    .|. (if accumInputOverflow then 32 else 0)
    .|. (if accumOverflow then 64 else 0)

{- | Compare the accumulator, cycle by cycle, with the C model: the results and
the flags. The model runs with a latency of two cycles and random enables, so
the result of the @k@-th enabled input appears in the cycles after the
@(k + 2)@-th enabled clock edge. Two more enabled cycles after the inputs show
the results of the last two enabled inputs. The C model gets the enabled
inputs only, and starts a new accumulation after each input with its last flag
set. Of its
exception bits, only those that the IP can output are compared: the C model
also reports underflow (1) and overflow (2), for sums below the smallest
normal number or beyond the largest finite number.
-}
prop_accum :: forall a. TestFloat a => IO Driver -> Property
prop_accum getDriver = H.withTests nTestsAccum $ H.property $ do
  driver <- H.evalIO getDriver
  cfg <- H.forAll (genAccumConfig @a)
  cycles <- H.forAll (genAccumCycles @a cfg)
  let
    ens = map (\(e, _, _) -> e) cycles
    lasts = map (\(_, l, _) -> l) cycles
    xs = map (\(_, _, x) -> bitsFloat x) cycles
    enabled = [(w, l) | (True, l, FloatBits w) <- cycles]
  cmodel <- H.evalIO (queryAccum driver (precisionName @a) cfg enabled)
  let
    padded = ens <> [True, True]
    outs = modelAccum @a cfg padded (lasts <> [False, False]) (xs <> [0, 0])
    -- The number of enabled clock edges before each cycle
    edges = scanl (\n e -> if e then n + 1 else n) (0 :: Int) padded
    checked = [(t, k - 2, out) | (t, k, out) <- zip3 [0 :: Int ..] edges outs, k >= 2]
    haskell = [(t, floatBits r, accumFlagBits fl) | (t, _, (r, fl)) <- checked]
    expected =
      [ (t, FloatBits (normaliseNaN @a w), e .&. (4 .|. 32 .|. 64))
      | (t, k, _) <- checked
      , let (w, e) = cmodel !! k
      ]
    excs = map snd cmodel
  H.classify "an enable deasserted" (not (and ens))
  H.classify "more than one accumulation" (or (init lasts))
  H.classify "accumulator overflow" (any (\e -> e .&. 64 /= 0) excs)
  H.classify "accumulator input overflow" (any (\e -> e .&. 32 /= 0) excs)
  H.classify "invalid operation" (any (\e -> e .&. 4 /= 0) excs)
  H.classify "underflow (C model only)" (any (\e -> e .&. 1 /= 0) excs)
  H.classify "overflow (C model only)" (any (\e -> e .&. 2 /= 0) excs)
  H.classify "a finite, non-zero result" $
    any (\(w, _) -> let v = fromWord @a w in not (isNaN v || isInfinite v || v == 0)) cmodel
  H.classify "default bounds" (cfg == modelAccumDefaults @a)
  haskell === expected

------------------------------------------------------------------------------
-- Exception flags of the other operations
------------------------------------------------------------------------------

-- | The operations whose exception flags 'derivedFlags' derives
data FlagOp
  = FlagBinary BinOp
  | FlagFma
  | FlagSqrt
  | FlagElem ElemOp
  | FlagConvert
  | FlagCompare
  | FlagAbs
  deriving (Show)

{- | Derive the flags of an operation from its (conditioned) inputs and its
result (of precision @b@, which is the precision of the inputs except for
conversions), like the documentation of "Clash.Cores.Xilinx.Floating.Generic"
describes, as the C model's exception bits: underflow (1), overflow (2),
invalid operation (4) and divide by zero (8).
-}
derivedFlags :: forall a b. (TestFloat a, TestFloat b) => FlagOp -> [a] -> b -> Int
derivedFlags op inputs r =
  (if underflow then 1 else 0)
    .|. (if overflow then 2 else 0)
    .|. (if invalid then 4 else 0)
    .|. (if divByZero then 8 else 0)
 where
  xs = map condition inputs
  finite v = not (isNaN v || isInfinite v)
  allFinite = all finite xs
  invalid = case op of
    FlagCompare -> False
    FlagAbs -> False
    _ -> isNaN r && not (any isNaN xs)
  divByZero = case (op, xs) of
    (FlagBinary Div, [x, y]) -> finite x && x /= 0 && y == 0
    (FlagElem e, [x]) -> e `elem` [Recip, RSqrt, Log] && x == 0
    _ -> False
  overflow = case op of
    FlagCompare -> False
    FlagAbs -> False
    _ -> allFinite && isInfinite r && not divByZero
  underflow =
    allFinite && r == 0 && case (op, xs) of
      (FlagBinary Add, [x, y]) -> x /= negate y
      (FlagBinary Sub, [x, y]) -> x /= y
      (FlagBinary Mul, [x, y]) -> x /= 0 && y /= 0
      (FlagBinary Div, [x, _]) -> x /= 0
      (FlagFma, [x, y, z]) -> toRational x * toRational y + toRational z /= 0
      (FlagElem Recip, _) -> True
      (FlagElem Exp, _) -> True
      (FlagConvert, [x]) -> x /= 0
      _ -> False

{- | Check that the flags derived from the inputs and the C model's result
('derivedFlags') are the C model's exception flags. The C model matches the
IP bit for bit, so this checks the rules in the documentation.
-}
prop_flags ::
  forall a b.
  (TestFloat a, TestFloat b) =>
  -- | Name of the operation, as understood by the C driver
  String ->
  FlagOp ->
  -- | Inputs
  Gen [FloatBits a] ->
  IO Driver ->
  Property
prop_flags opName' op genInputs getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  inputs <- H.forAll genInputs
  let
    xs = map bitsFloat inputs
    operands = map toWord xs <> replicate (2 - length xs) 0
  (cBits, exceptions) <- H.evalIO (queryOps driver (precisionName @a) opName' operands)
  let
    -- The comparison's result is a condition code, not a number
    r = case op of
      FlagCompare -> 0
      _ -> fromWord @b cBits
    flags = exceptions .&. 15
  H.classify "underflow" (flags .&. 1 /= 0)
  H.classify "overflow" (flags .&. 2 /= 0)
  H.classify "invalid operation" (flags .&. 4 /= 0)
  H.classify "divide by zero" (flags .&. 8 /= 0)
  H.footnote ("C model result: " <> show (FloatBits cBits :: FloatBits b))
  derivedFlags @a @b op xs r === flags

-- | Flags of the conversion to @Signed 32@: invalid operation for NaN and the
-- infinities, overflow for inputs that round to a number out of range, which
-- are those of at least @2^31 - 1/2@ and those below @-2^31 - 1/2@ (ties round
-- to even), infinities included.
prop_toS32Flags :: forall a. TestInteger a => IO Driver -> Property
prop_toS32Flags getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  a <- H.forAll (genToS32Operand @a)
  let x = condition (bitsFloat a)
  (_, exceptions) <- H.evalIO (query driver (precisionName @a) "tos32" (toWord x) 0)
  let
    -- Infinities raise both
    invalid = isNaN x || isInfinite x
    overflow =
      not (isNaN x)
        && (x >= 2 ^ (31 :: Int) - 0.5 || x < negate (2 ^ (31 :: Int)) - 0.5)
    derived = (if overflow then 2 else 0) .|. (if invalid then 4 else 0)
  H.classify "overflow" overflow
  H.classify "invalid operation" invalid
  derived === exceptions .&. 15

-- | The conversions from integers never raise flags
prop_fromIntFlags :: forall a. TestInteger a => IO Driver -> Property
prop_fromIntFlags getDriver = H.withTests nTests $ H.property $ do
  driver <- H.evalIO getDriver
  (op, w) <-
    H.forAll . Gen.choice $
      map (\(FromInt name gen bits _) -> (,) name . bits <$> gen) (fromIntConversions @a)
  (_, exceptions) <- H.evalIO (query driver (precisionName @a) op w 0)
  exceptions === 0

-- | Tests of the flags of the operations of one precision
flagTests :: forall a. TestFloat a => IO Prepared -> [TestTree]
flagTests getPrepared =
  [ binary Add
  , binary Sub
  , binary Mul
  , binary Div
  , property "fma" (prop_flags @a @a "fma" FlagFma (triple <$> genFmaOperands @a))
  , property "sqrt" (prop_flags @a @a "sqrt" FlagSqrt (pure <$> genSqrtOperand @a))
  , elementary Recip
  , elementary RSqrt
  , elementary Exp
  , elementary Log
  , property "convert to half" (prop_flags @a @Half "tohalf" FlagConvert (pure <$> genConvertOperand @a @Half))
  , property "convert to single" (prop_flags @a @Float "tosingle" FlagConvert (pure <$> genConvertOperand @a @Float))
  , property "convert to double" (prop_flags @a @Double "todouble" FlagConvert (pure <$> genConvertOperand @a @Double))
  , property "compare" (prop_flags @a @a "cmp" FlagCompare (pair <$> genCompareOperands @a))
  , property "abs" (prop_flags @a @a "abs" FlagAbs (pure <$> genFloat @a))
  ]
 where
  binary op =
    property (opName op) . prop_flags @a @a (opName op) (FlagBinary op) $
      Gen.frequency
        [ (6, pair <$> genBinaryOperands @a op)
        , (1, pair . snd <$> Gen.filter ((== op) . fst) (genNaNOperation @a))
        ]
  elementary op =
    property (elemOpName op) (prop_flags @a @a (elemOpName op) (FlagElem op) (pure <$> genElemOperand @a op))
  pair (x, y) = [x, y]
  triple (x, y, z) = [x, y, z]
  property name = driverProperty @a getPrepared name ("flags_" <> name)

-- | Tests of the flags of the conversions from and to integers
integerFlagTests :: forall a. TestInteger a => IO Prepared -> [TestTree]
integerFlagTests getPrepared =
  [ property "toS32" (prop_toS32Flags @a)
  , property "fromU32, fromS32, fromU64, fromS64" (prop_fromIntFlags @a)
  ]
 where
  property name = driverProperty @a getPrepared name ("flags_" <> name)
