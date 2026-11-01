{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the simulation models in "Clash.Cores.Xilinx.Divider" against the
  bit-accurate C model of the Xilinx Divider Generator LogiCORE IP v5.1. The
  tests run the actual functions from "Clash.Cores.Xilinx.Divider.Explicit" in
  Clash simulation, on a stream of operands, and compare their output with the
  output of the C model, for many configurations: operand widths, signedness,
  algorithm and output type.

  The C model's quotient and remainder are undefined for a division by zero
  (PG151, chapter 5), and so are the IP's. We only check that the simulation
  model gives 'Nothing' exactly when the C model reports a division by zero.

  = Running the tests

  These tests are only part of the unit tests if the Cabal flag
  @xilinx-div-gen-cmodel@ is enabled. It is disabled by default, and enabled
  on CI. Xilinx's C model cannot be redistributed, so the tests locate it at
  run time:

  1. If @CLASH_CORES_XILINX_DIV_GEN_CMODEL@ is set, it should point at either
     @div_gen_v5_1_bitacc_cmodel_lin64.zip@ (Vivado ships it in
     @data\/ip\/xilinx\/div_gen_v5_1\/cmodel@) or a directory holding its
     extracted contents.

  2. Otherwise, if @XILINX_VIVADO@ is set (by Vivado's @settings64.sh@), the
     zip file is taken from that installation.

  If neither is set, or the C model is not where they point, the tests fail.

  Small configurations are tested exhaustively. For the others, each
  Hedgehog test case is a batch of 'batchSize' random divisions; use
  @--hedgehog-tests@ to change the number of batches from 'nBatches'.
-}
module Test.Cores.Xilinx.Divider (tests) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, bracket, catch, onException, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits (shiftL, (.|.))
import Data.Char (isSpace)
import Data.List (find)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, pokeByteOff)
import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Numeric (readHex, showHex)
import System.Directory
  ( createDirectory, doesDirectoryExist, doesFileExist, makeAbsolute
  , removeDirectoryRecursive )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (arch, os)
import System.Process (readProcessWithExitCode)

import Clash.Explicit.Prelude (BitPack (..), Clock, Enable, Signed, System, Unsigned)
import qualified Clash.Explicit.Prelude as C
import qualified Clash.Signal.Delayed as D

import Clash.Cores.Xilinx.Divider.Explicit (Algorithm (..), Config (..), DividerOperand (..))
import qualified Clash.Cores.Xilinx.Divider.Explicit as Div

import Hedgehog (Gen)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Hedgehog (testPropertyNamed)

------------------------------------------------------------------------------
-- Configuration of the tests
------------------------------------------------------------------------------

-- | Name of the environment variable pointing at the C model
cmodelEnvVar :: String
cmodelEnvVar = "CLASH_CORES_XILINX_DIV_GEN_CMODEL"

-- | The C model in a Vivado installation
divGenCModel :: CModelLocation
divGenCModel =
  CModelLocation
    { cmlEnvVar = cmodelEnvVar
    , cmlZipInVivado = "data/ip/xilinx/div_gen_v5_1/cmodel/div_gen_v5_1_bitacc_cmodel_lin64.zip"
    , cmlTestModule = "test/Test/Cores/Xilinx/Divider.hs"
    }

-- | Number of divisions per test case
batchSize :: Int
batchSize = 1_000

-- | Number of batches per configuration, unless overridden by
-- @--hedgehog-tests@
nBatches :: H.TestLimit
nBatches = 100

tests :: TestTree
tests =
  withResource (loadDivGen divGenCModel) unloadDivGen $ \getCModel ->
    testGroup "Xilinx divider vs. C model"
      [ testGroup "quotRem Radix2"
          [ quotRemTest @Signed @8 @8 Exhaustive getCModel Radix2
          , quotRemTest @Unsigned @8 @8 Exhaustive getCModel Radix2
          , quotRemTest @Signed @2 @2 Exhaustive getCModel Radix2
          , quotRemTest @Unsigned @2 @2 Exhaustive getCModel Radix2
          , quotRemTest @Signed @16 @5 Random getCModel Radix2
          , quotRemTest @Unsigned @5 @16 Random getCModel Radix2
          , quotRemTest @Signed @32 @32 Random getCModel Radix2
          , quotRemTest @Unsigned @32 @17 Random getCModel Radix2
          , quotRemTest @Signed @64 @64 Random getCModel Radix2
          , quotRemTest @Unsigned @64 @64 Random getCModel Radix2
          , quotRemTest @Signed @64 @3 Random getCModel Radix2
          , quotRemTest @Unsigned @3 @64 Random getCModel Radix2
          , quotRemTest @Signed @41 @23 Random getCModel Radix2
          ]
      , testGroup "quotRem LutMult"
          [ quotRemTest @Signed @8 @8 Exhaustive getCModel LutMult
          , quotRemTest @Unsigned @8 @8 Exhaustive getCModel LutMult
          , quotRemTest @Signed @2 @2 Exhaustive getCModel LutMult
          , quotRemTest @Unsigned @2 @2 Exhaustive getCModel LutMult
          , quotRemTest @Signed @17 @6 Random getCModel LutMult
          , quotRemTest @Unsigned @17 @6 Random getCModel LutMult
          , quotRemTest @Signed @11 @12 Random getCModel LutMult
          , quotRemTest @Unsigned @12 @11 Random getCModel LutMult
          , quotRemTest @Signed @5 @12 Random getCModel LutMult
          ]
      , testGroup "quotFrac"
          [ quotFracTest @Signed @8 @8 @7 Exhaustive getCModel
          , quotFracTest @Unsigned @8 @8 @8 Exhaustive getCModel
          , quotFracTest @Signed @2 @2 @1 Exhaustive getCModel
          , quotFracTest @Unsigned @2 @2 @2 Exhaustive getCModel
          , quotFracTest @Signed @12 @5 @6 Random getCModel
          , quotFracTest @Unsigned @10 @6 @4 Random getCModel
          , quotFracTest @Signed @2 @16 @15 Random getCModel
          , quotFracTest @Unsigned @2 @16 @16 Random getCModel
          , quotFracTest @Signed @32 @16 @31 Random getCModel
          , quotFracTest @Unsigned @16 @32 @20 Random getCModel
          , quotFracTest @Signed @64 @64 @63 Random getCModel
          , quotFracTest @Unsigned @64 @64 @64 Random getCModel
          , quotFracTest @Signed @3 @64 @2 Random getCModel
          ]
      , testGroup "divHighRadix"
          [ highRadixTest @8 @8 @0 Exhaustive getCModel
          , highRadixTest @6 @8 @4 Exhaustive getCModel
          , highRadixTest @4 @6 @8 Exhaustive getCModel
          , highRadixTest @7 @7 @5 Exhaustive getCModel
          , highRadixTest @2 @4 @10 Exhaustive getCModel
          , highRadixTest @12 @12 @0 Random getCModel
          , highRadixTest @11 @11 @1 Random getCModel
          , highRadixTest @6 @12 @6 Random getCModel
          , highRadixTest @12 @4 @0 Random getCModel
          , highRadixTest @4 @20 @8 Random getCModel
          , highRadixTest @2 @40 @10 Random getCModel
          , highRadixTest @10 @64 @2 Random getCModel
          , highRadixTest @12 @64 @0 Random getCModel
          , highRadixTest @3 @33 @9 Random getCModel
          ]
      ]

------------------------------------------------------------------------------
-- Generic: locating, extracting and loading a Xilinx C model
--
-- Nothing in this section is specific to the Divider Generator. The C models
-- of the Floating-Point IP (test/Test/Cores/Xilinx/Floating.hs) and other
-- LogiCORE IP are loaded in the same way, so this section could move to a
-- shared module.
------------------------------------------------------------------------------

-- | Where to find a C model
data CModelLocation = CModelLocation
  { cmlEnvVar :: String
  -- ^ Environment variable pointing at the zip file or a directory with its
  -- extracted contents
  , cmlZipInVivado :: FilePath
  -- ^ Location of the zip file relative to @$XILINX_VIVADO@
  , cmlTestModule :: FilePath
  -- ^ Test module documenting the above, for error messages
  }

-- | A C model, extracted and loaded into the test process
data LoadedCModel = LoadedCModel
  { lcmTmpDir :: FilePath
  , lcmLibraries :: [Ptr ()]
  -- ^ In reverse order of loading
  }

-- | Find the C model, see the module documentation. Returns a path to the zip
-- file or directory.
findCModel :: CModelLocation -> IO FilePath
findCModel CModelLocation{cmlEnvVar, cmlZipInVivado, cmlTestModule}
  | os /= "linux" || arch /= "x86_64" =
      fail "The C model is only available for x86_64 Linux."
  | otherwise = do
      explicit <- lookupEnv cmlEnvVar
      vivado <- lookupEnv "XILINX_VIVADO"
      case (explicit, vivado) of
        (Just path, _) | not (null path) -> pure path
        (_, Just dir) | not (null dir) -> do
          let path = dir </> cmlZipInVivado
          exists <- doesFileExist path
          unless exists $
            fail ("XILINX_VIVADO is set, but " <> path <> " does not exist.")
          pure path
        _ ->
          fail $
            "Set " <> cmlEnvVar <> " to the C model's zip file"
              <> " (or a directory with its extracted contents), or set XILINX_VIVADO."
              <> " See " <> cmlTestModule <> "."

{- | Find the C model, extract it if needed, and load the given shared
libraries from it, in order. A library should come after the libraries it
depends on: we load them with @RTLD_DEEPBIND@, so that the C model uses the
libraries it ships with (for example GMP) rather than the ones the test
process already uses.
-}
loadCModel :: CModelLocation -> [FilePath] -> IO LoadedCModel
loadCModel loc libs = do
  path <- findCModel loc
  tmpRoot <- getCanonicalTemporaryDirectory
  tmp <- createTempDirectory tmpRoot "clash-cores-xilinx-cmodel"
  flip onException (removeDirectoryRecursive tmp) $ do
    modelDir <- extractCModel loc tmp path
    handles <- mapM (\lib -> loadLibrary (modelDir </> lib)) libs
    pure LoadedCModel{lcmTmpDir = tmp, lcmLibraries = reverse handles}

-- | Give a directory with the C model's files, extracting the zip file into
-- the given temporary directory if needed
extractCModel :: CModelLocation -> FilePath -> FilePath -> IO FilePath
extractCModel CModelLocation{cmlEnvVar} tmp path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if
    | isDir -> makeAbsolute path
    | isFile -> do
        let dir = tmp </> "cmodel"
        createDirectory dir
        extractZip dir
        pure dir
    | otherwise ->
        fail (path <> " is neither a file nor a directory")
 where
  extractZip dir = do
    let args = ["-q", path, "-d", dir]
    (code, out, err) <-
      readProcessWithExitCode "unzip" args "" `catch` \(e :: IOException) ->
        fail $
          show e
            <> "\nExtracting the zip file requires unzip. Alternatively, set "
            <> cmlEnvVar <> " to a directory with its extracted contents."
    case code of
      ExitSuccess -> pure ()
      ExitFailure n ->
        fail $
          unlines
            [ "Command failed with exit code " <> show n <> ": " <> unwords ("unzip" : args)
            , out
            , err
            ]

unloadCModel :: LoadedCModel -> IO ()
unloadCModel LoadedCModel{lcmTmpDir, lcmLibraries} = do
  mapM_ c_dlclose lcmLibraries
  void (try @SomeException (removeDirectoryRecursive lcmTmpDir))

foreign import ccall unsafe "dlfcn.h dlopen" c_dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dlfcn.h dlsym" c_dlsym :: Ptr () -> CString -> IO (FunPtr a)
foreign import ccall unsafe "dlfcn.h dlclose" c_dlclose :: Ptr () -> IO CInt
foreign import ccall unsafe "dlfcn.h dlerror" c_dlerror :: IO CString

-- | Flags for 'c_dlopen', from glibc's @dlfcn.h@. The @unix@ package does not
-- offer @RTLD_DEEPBIND@.
rtldNow, rtldDeepBind :: CInt
rtldNow = 0x2
rtldDeepBind = 0x8

-- | Load a shared library, see 'loadCModel'
loadLibrary :: FilePath -> IO (Ptr ())
loadLibrary path = do
  handle <- withCString path $ \cPath -> c_dlopen cPath (rtldNow .|. rtldDeepBind)
  when (handle == nullPtr) $ dlFail ("Could not load " <> path)
  pure handle

-- | Look up a function in a shared library or the libraries it depends on
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

------------------------------------------------------------------------------
-- The Divider Generator C model
------------------------------------------------------------------------------

-- | Opaque C types
data XipDivGen
data XipArray
data Mpz

-- | Size of GMP's @mpz_t@: two @int@s and a pointer
mpzSize :: Int
mpzSize = 16

-- | Size of @xip_div_gen_v5_1_config@: a pointer and eight @int@s
configSize :: Int
configSize = 40

type DefaultConfig = Ptr () -> IO CInt
type Create = Ptr () -> FunPtr () -> Ptr () -> IO (Ptr XipDivGen)
type Destroy = Ptr XipDivGen -> IO CInt
type ArrayAlloc = CSize -> IO (Ptr XipArray)
type ArrayDestroy = Ptr XipArray -> IO (Ptr XipArray)
type ArrayMpzData = Ptr XipArray -> Ptr Mpz -> CSize -> IO CInt
type ArrayUIntGet = Ptr XipArray -> Ptr CUInt -> CSize -> IO CInt
type DataDo =
  Ptr XipDivGen -> Ptr XipArray -> Ptr XipArray -> Ptr XipArray -> Ptr XipArray ->
  Ptr XipArray -> CUInt -> IO CInt
type MpzInit = Ptr Mpz -> IO ()
type MpzSetStr = Ptr Mpz -> CString -> CInt -> IO CInt
type MpzGetStr = CString -> CInt -> Ptr Mpz -> IO CString
type MpzSizeInBase = Ptr Mpz -> CInt -> IO CSize

foreign import ccall "dynamic" mkDefaultConfig :: FunPtr DefaultConfig -> DefaultConfig
foreign import ccall "dynamic" mkCreate :: FunPtr Create -> Create
foreign import ccall "dynamic" mkDestroy :: FunPtr Destroy -> Destroy
foreign import ccall "dynamic" mkArrayAlloc :: FunPtr ArrayAlloc -> ArrayAlloc
foreign import ccall "dynamic" mkArrayDestroy :: FunPtr ArrayDestroy -> ArrayDestroy
foreign import ccall "dynamic" mkArrayMpzData :: FunPtr ArrayMpzData -> ArrayMpzData
foreign import ccall "dynamic" mkArrayUIntGet :: FunPtr ArrayUIntGet -> ArrayUIntGet
foreign import ccall "dynamic" mkDataDo :: FunPtr DataDo -> DataDo
foreign import ccall "dynamic" mkMpzInit :: FunPtr MpzInit -> MpzInit
foreign import ccall "dynamic" mkMpzSetStr :: FunPtr MpzSetStr -> MpzSetStr
foreign import ccall "dynamic" mkMpzGetStr :: FunPtr MpzGetStr -> MpzGetStr
foreign import ccall "dynamic" mkMpzSizeInBase :: FunPtr MpzSizeInBase -> MpzSizeInBase

-- | The functions of the C model, and of the GMP library it uses
data DivGen = DivGen
  { dgLoaded :: LoadedCModel
  , dgLock :: MVar ()
  , dgDefaultConfig :: DefaultConfig
  , dgCreate :: Create
  , dgDestroy :: Destroy
  , dgMpzAlloc :: ArrayAlloc
  , dgUIntAlloc :: ArrayAlloc
  , dgMpzDestroy :: ArrayDestroy
  , dgUIntDestroy :: ArrayDestroy
  , dgMpzSet :: ArrayMpzData
  , dgMpzGet :: ArrayMpzData
  , dgUIntGet :: ArrayUIntGet
  , dgDataDo :: DataDo
  , dgMpzInit :: MpzInit
  , dgMpzClear :: MpzInit
  , dgMpzSetStr :: MpzSetStr
  , dgMpzGetStr :: MpzGetStr
  , dgMpzSizeInBase :: MpzSizeInBase
  }

loadDivGen :: CModelLocation -> IO DivGen
loadDivGen loc = do
  dgLoaded <- loadCModel loc ["libgmp.so.11", "libIp_div_gen_v5_1_bitacc_cmodel.so"]
  lib <- case lcmLibraries dgLoaded of
    l : _ -> pure l
    [] -> fail "No libraries loaded"
  let fun :: (FunPtr a -> a) -> String -> IO a
      fun mk name = mk <$> lookupFunction lib name
  dgLock <- newMVar ()
  dgDefaultConfig <- fun mkDefaultConfig "xip_div_gen_v5_1_default_config"
  dgCreate <- fun mkCreate "xip_div_gen_v5_1_create"
  dgDestroy <- fun mkDestroy "xip_div_gen_v5_1_destroy"
  dgMpzAlloc <- fun mkArrayAlloc "xip_div_gen_v5_1_xip_array_mpz_alloc"
  dgUIntAlloc <- fun mkArrayAlloc "xip_div_gen_v5_1_xip_array_uint_alloc"
  dgMpzDestroy <- fun mkArrayDestroy "xip_div_gen_v5_1_xip_array_mpz_destroy"
  dgUIntDestroy <- fun mkArrayDestroy "xip_div_gen_v5_1_xip_array_uint_destroy"
  dgMpzSet <- fun mkArrayMpzData "xip_div_gen_v5_1_xip_array_mpz_set_data"
  dgMpzGet <- fun mkArrayMpzData "xip_div_gen_v5_1_xip_array_mpz_get_data"
  dgUIntGet <- fun mkArrayUIntGet "xip_div_gen_v5_1_xip_array_uint_get_data"
  dgDataDo <- fun mkDataDo "xip_div_gen_v5_1_data_do"
  -- GMP's mpz_* functions are macros for these
  dgMpzInit <- fun mkMpzInit "__gmpz_init"
  dgMpzClear <- fun mkMpzInit "__gmpz_clear"
  dgMpzSetStr <- fun mkMpzSetStr "__gmpz_set_str"
  dgMpzGetStr <- fun mkMpzGetStr "__gmpz_get_str"
  dgMpzSizeInBase <- fun mkMpzSizeInBase "__gmpz_sizeinbase"
  pure DivGen{..}

unloadDivGen :: DivGen -> IO ()
unloadDivGen DivGen{dgLoaded} = unloadCModel dgLoaded

-- | Configuration of the C model, see @div_gen_v5_1_bitacc_cmodel.h@
data CConfig = CConfig
  { ccAlgorithm :: CAlgorithm
  , ccSigned :: Bool
  , ccDividendWidth :: Int
  , ccDivisorWidth :: Int
  , ccFractional :: Maybe Int
  -- ^ Fractional width, or 'Nothing' for a remainder output
  }
  deriving (Show)

data CAlgorithm = CRadix2 | CHighRadix | CLutMult
  deriving (Show)

-- | Result of a division in the C model
data CResult = CResult
  { crQuotient :: Integer
  -- ^ Including the fractional part, if any
  , crRemainder :: Integer
  , crDivByZero :: Bool
  }
  deriving (Show)

{- | Run the C model on a list of dividends and divisors. Operands are given as
integers in their range, results as the raw bits of the output fields: the
model gives some negative results as negative numbers and others as their
two's complement bits, so we normalize them to bits ourselves.
-}
runDivGen :: DivGen -> CConfig -> [(Integer, Integer)] -> IO [CResult]
runDivGen DivGen{..} cfg@CConfig{..} operands =
  withMVar dgLock $ \() ->
  allocaBytes configSize $ \cfgPtr ->
  allocaBytes mpzSize $ \tmp -> do
    check "default_config" =<< dgDefaultConfig cfgPtr
    let pokeInt off v = pokeByteOff cfgPtr off (fromIntegral v :: CInt)
    pokeInt 8 $ case ccAlgorithm of
      CRadix2 -> 1 :: Int
      CHighRadix -> 3
      CLutMult -> 4
    pokeInt 12 ccDivisorWidth
    pokeInt 16 ccDividendWidth
    pokeInt 20 (if ccSigned then 1 else 0 :: Int)
    pokeInt 24 (maybe 0 (const 1) ccFractional :: Int)
    pokeInt 28 (maybe ccDivisorWidth id ccFractional)
    pokeInt 32 (1 :: Int) -- clocks per division
    pokeInt 36 (0 :: Int) -- debug
    bracket
      (dgCreate cfgPtr nullFunPtr nullPtr)
      (\h -> when (h /= nullPtr) (void (dgDestroy h)))
      $ \handle -> do
        when (handle == nullPtr) $
          fail ("Could not create the C model for " <> show cfg)
        let n = length operands
            alloc f = bracket (f (fromIntegral n))
        alloc dgMpzAlloc dgMpzDestroy $ \divisors ->
          alloc dgMpzAlloc dgMpzDestroy $ \dividends ->
          alloc dgMpzAlloc dgMpzDestroy $ \quotients ->
          alloc dgMpzAlloc dgMpzDestroy $ \remainders ->
          alloc dgUIntAlloc dgUIntDestroy $ \divByZeros ->
          bracket (dgMpzInit tmp) (const (dgMpzClear tmp)) $ \() -> do
            forM_ (zip [0 ..] operands) $ \(i, (x, y)) -> do
              setMpz tmp x
              check "set_data" =<< dgMpzSet dividends tmp i
              setMpz tmp y
              check "set_data" =<< dgMpzSet divisors tmp i
            check "data_do"
              =<< dgDataDo handle divisors dividends quotients remainders divByZeros (fromIntegral n)
            forM [0 .. fromIntegral n - 1] $ \i -> do
              check "get_data" =<< dgMpzGet quotients tmp i
              q <- getMpz tmp
              check "get_data" =<< dgMpzGet remainders tmp i
              r <- getMpz tmp
              z <- alloca $ \p -> do
                check "get_data" =<< dgUIntGet divByZeros p i
                peek p
              pure
                CResult
                  { crQuotient = q `mod` (1 `shiftL` quotientWidth cfg)
                  , crRemainder = r `mod` (1 `shiftL` ccDivisorWidth)
                  , crDivByZero = z /= 0
                  }
 where
  check :: String -> CInt -> IO ()
  check what status =
    unless (status == 0) $
      fail ("C model function " <> what <> " failed with status " <> show status)

  setMpz :: Ptr Mpz -> Integer -> IO ()
  setMpz p v =
    withCString (sign <> showHex (abs v) "") $ \s ->
      check "mpz_set_str" =<< dgMpzSetStr p s 16
   where
    sign = if v < 0 then "-" else ""

  getMpz :: Ptr Mpz -> IO Integer
  getMpz p = do
    len <- dgMpzSizeInBase p 16
    -- Room for a minus sign and the terminating null character
    allocaBytes (fromIntegral len + 2) $ \buf -> do
      _ <- dgMpzGetStr buf 16 p
      parseHex <$> peekCString buf

  parseHex :: String -> Integer
  parseHex ('-' : s) = negate (parseHex s)
  parseHex s = case readHex (filter (not . isSpace) s) of
    [(v, "")] -> v
    _ -> error ("Cannot parse C model output " <> show s)

-- | Width of the C model's quotient output, including the fractional part
quotientWidth :: CConfig -> Int
quotientWidth CConfig{ccDividendWidth, ccFractional} =
  ccDividendWidth + maybe 0 id ccFractional

------------------------------------------------------------------------------
-- Running the Haskell models
------------------------------------------------------------------------------

clk :: Clock System
clk = C.clockGen

en :: Enable System
en = C.enableGen

-- | Run a pipelined function with latency @d@ on a list of inputs, and return
-- one output per input
runPipeline ::
  forall d a b c.
  (KnownNat d, C.NFDataX a, C.NFDataX b) =>
  (D.DSignal System 0 a -> D.DSignal System 0 b -> D.DSignal System d c) ->
  [(a, b)] ->
  [c]
runPipeline f xys =
  take (length xys) $
    drop (fromInteger (natVal (Proxy @d))) $
      C.sample_lazy (D.toSignal (f (D.unsafeFromSignal (C.fromList xs)) (D.unsafeFromSignal (C.fromList ys))))
 where
  -- Keep the inputs defined after the end of the list
  (xs, ys) = unzip (xys <> repeat (last xys))

------------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------------

-- | Integers of an operand type, as the C model takes them
class DividerOperand a => TestOperand a where
  isSigned :: Bool
  -- | Range of the operand type with the given width
  operandRange :: Int -> (Integer, Integer)

instance TestOperand Signed where
  isSigned = True
  operandRange w = (negate (1 `shiftL` (w - 1)), (1 `shiftL` (w - 1)) - 1)

instance TestOperand Unsigned where
  isSigned = False
  operandRange w = (0, (1 `shiftL` w) - 1)

natInt :: forall n. KnownNat n => Int
natInt = fromInteger (natVal (Proxy @n))

-- | A function under test, and how to compare it with the C model
data Subject (a :: Nat -> Type) (m :: Nat) (n :: Nat) r = Subject
  { sName :: String
  , sConfig :: CConfig
  , sHaskell :: [(a m, a n)] -> [Maybe r]
  -- ^ Run the Haskell model on a list of operands
  , sHaskellBits :: r -> Integer
  -- ^ Bits of a result of the Haskell model
  , sCBits :: CResult -> Integer
  -- ^ Bits of a result of the C model
  }

-- | How to pick operands
data Operands = Random | Exhaustive

-- | Test 'Div.quotRemWith', with a small latency
quotRemTest ::
  forall a m n.
  ( TestOperand a
  , KnownNat m
  , KnownNat n
  , C.NFDataX (a m)
  , C.NFDataX (a n)
  , BitPack (a m)
  , BitPack (a n)
  , C.BitSize (a m) ~ m
  , C.BitSize (a n) ~ n
  , Show (a m)
  , Show (a n)
  ) =>
  Operands ->
  IO DivGen ->
  Algorithm ->
  TestTree
quotRemTest operands getCModel algo =
  divisionTest @a @m @n operands getCModel $
    Subject
      { sName = operandsName @a @m @n
      , sConfig =
          CConfig
            { ccAlgorithm = case algo of Radix2 -> CRadix2; LutMult -> CLutMult
            , ccSigned = isSigned @a
            , ccDividendWidth = natInt @m
            , ccDivisorWidth = natInt @n
            , ccFractional = Nothing
            }
      , sHaskell = runPipeline @3 (Div.quotRemWith @3 (Config{algorithm = algo}) clk en)
      , -- The C model gives the quotient and the remainder separately
        sHaskellBits = \(q, r) -> toBits q `shiftL` natInt @n .|. toBits r
      , sCBits = \CResult{crQuotient, crRemainder} ->
          crQuotient `shiftL` natInt @n .|. crRemainder
      }

-- | Test 'Div.quotFracWith', with a small latency
quotFracTest ::
  forall a m n f.
  ( TestOperand a
  , KnownNat m
  , KnownNat n
  , KnownNat f
  , KnownNat (FractionWidth a f)
  , C.NFDataX (a m)
  , C.NFDataX (a n)
  , BitPack (a m)
  , BitPack (a n)
  , BitPack (Fraction a f)
  , C.BitSize (Fraction a f) ~ FractionWidth a f
  , C.BitSize (a m) ~ m
  , C.BitSize (a n) ~ n
  , Show (a m)
  , Show (a n)
  , Show (Fraction a f)
  ) =>
  Operands ->
  IO DivGen ->
  TestTree
quotFracTest operands getCModel =
  divisionTest @a @m @n operands getCModel $
    Subject
      { sName = operandsName @a @m @n <> ", " <> show fracWidth <> "-bit fraction"
      , sConfig =
          CConfig
            { ccAlgorithm = CRadix2
            , ccSigned = isSigned @a
            , ccDividendWidth = natInt @m
            , ccDivisorWidth = natInt @n
            , ccFractional = Just fracWidth
            }
      , sHaskell = runPipeline @3 (Div.quotFracWith @3 @a @m @n @f clk en)
      , sHaskellBits = \(q, fr) -> toBits q `shiftL` fracWidth .|. toBits fr
      , sCBits = crQuotient
      }
 where
  fracWidth = natInt @(FractionWidth a f)

-- | Test 'Div.divHighRadixWith', with its smallest latency
highRadixTest ::
  forall m n f.
  ( KnownNat m
  , KnownNat n
  , KnownNat f
  , (m C.+ f) C.<= 12
  ) =>
  Operands ->
  IO DivGen ->
  TestTree
highRadixTest operands getCModel =
  divisionTest @Signed @m @n operands getCModel $
    Subject
      { sName = operandsName @Signed @m @n <> ", " <> show (natInt @f) <> "-bit fraction"
      , sConfig =
          CConfig
            { ccAlgorithm = CHighRadix
            , ccSigned = True
            , ccDividendWidth = natInt @m
            , ccDivisorWidth = natInt @n
            , ccFractional = Just (natInt @f)
            }
      , sHaskell = runPipeline @2 (Div.divHighRadixWith @2 @m @n @f clk en)
      , sHaskellBits = toBits
      , sCBits = crQuotient
      }

-- | Name of a test, without slashes: Tasty patterns would split names at them
operandsName :: forall a m n. (DividerOperand a, KnownNat m, KnownNat n) => String
operandsName =
  Div.operandSign @a <> ", " <> show (natInt @m) <> "-bit dividend, "
    <> show (natInt @n) <> "-bit divisor"

-- | Unsigned integer value of the bits of a value
toBits :: (BitPack a, KnownNat (C.BitSize a)) => a -> Integer
toBits = toInteger . pack

-- | Compare a function with the C model, on batches of random operands or on
-- all operands
divisionTest ::
  forall a m n r.
  (TestOperand a, KnownNat m, KnownNat n, Show (a m), Show (a n), Show r) =>
  Operands ->
  IO DivGen ->
  Subject a m n r ->
  TestTree
divisionTest operands getCModel subject@Subject{sName} =
  case operands of
    Random ->
      testPropertyNamed sName (fromString ("prop_" <> sName)) $
        -- Shrinking a batch takes long, and the failure shows the failing division
        H.withShrinks 0 $ H.withTests nBatches $ H.property $ do
          batch <- H.forAllWith (const "<batch>") (Gen.list (Range.singleton batchSize) genOperands)
          cmodel <- H.evalIO getCModel
          (failure, divByZero) <- H.evalIO (compareBatch cmodel subject batch)
          H.classify "contains a division by zero" divByZero
          mapM_ (\msg -> H.footnote msg >> H.failure) failure
    Exhaustive ->
      -- Not a Hedgehog property, so that @--hedgehog-tests@ does not repeat it
      testCase (sName <> ", exhaustive") $ do
        cmodel <- getCModel
        forM_ (chunksOf (10 * batchSize) allOperands) $ \batch -> do
          (failure, _) <- compareBatch cmodel subject batch
          mapM_ assertFailure failure
 where
  allOperands =
    [ (operandFromInteger x, operandFromInteger y)
    | let (xLo, xHi) = operandRange @a (natInt @m)
    , let (yLo, yHi) = operandRange @a (natInt @n)
    , x <- [xLo .. xHi]
    , y <- [yLo .. yHi]
    ]
  chunksOf k xs = case splitAt k xs of
    (c, []) -> [c]
    (c, rest) -> c : chunksOf k rest

{- | Run a batch of divisions on both the C model and the Haskell model, and
compare the results. On division by zero, only check that the Haskell model
gives 'Nothing'. Gives a description of the first mismatch, if any, and
whether the batch contains a division by zero.
-}
compareBatch ::
  forall a m n r.
  (TestOperand a, KnownNat m, KnownNat n, Show (a m), Show (a n), Show r) =>
  DivGen ->
  Subject a m n r ->
  [(a m, a n)] ->
  IO (Maybe String, Bool)
compareBatch cmodel Subject{sConfig, sHaskell, sHaskellBits, sCBits} operands = do
  cResults <-
    runDivGen cmodel sConfig
      [(operandToInteger x, operandToInteger y) | (x, y) <- operands]
  let
    compared = zip3 operands (sHaskell operands) cResults
    expected c
      | crDivByZero c = Nothing
      | otherwise = Just (sCBits c)
    ok (_, h, c) = (sHaskellBits <$> h) == expected c
    mismatch ((x, y), h, c) =
      unlines
        [ "Dividend: " <> show x <> ", divisor: " <> show y
        , "Haskell model: " <> show h <> ", bits: " <> show (showHex' . sHaskellBits <$> h)
        , "C model: " <> show c <> ", bits: " <> show (showHex' <$> expected c)
        ]
  pure
    ( mismatch <$> find (not . ok) compared
    , any (\(_, _, c) -> crDivByZero c) compared
    )
 where
  showHex' v = "0x" <> showHex v ""

------------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------------

-- | Operands, biased towards edge cases: zero, plus and minus one, the extremes
-- of their ranges, powers of two, a division of the most negative number by
-- -1, and divisors much smaller than the dividend
genOperands ::
  forall a m n.
  (TestOperand a, KnownNat m, KnownNat n) =>
  Gen (a m, a n)
genOperands =
  Gen.frequency
    [ (6, (,) <$> genOperand @a @m <*> genOperand @a @n)
    , (1, (,) <$> genOperand @a @m <*> pure (operandFromInteger 0))
    , (1, (,) <$> pure (operandFromInteger lo) <*> pure (operandFromInteger (-1)))
    ]
 where
  (lo, _) = operandRange @a (natInt @m)

-- | Operand, biased towards edge cases
genOperand :: forall a n. (TestOperand a, KnownNat n) => Gen (a n)
genOperand =
  operandFromInteger
    <$> Gen.frequency
      [ (4, Gen.integral (Range.constant lo hi))
      , (2, Gen.integral (Range.exponentialFrom 0 lo hi))
      , (1, Gen.element (filter inRange edges))
      , (1, powerOfTwo)
      ]
 where
  w = natInt @n
  (lo, hi) = operandRange @a w
  inRange v = lo <= v && v <= hi
  edges = [0, 1, -1, 2, -2, lo, lo + 1, hi, hi - 1]
  powerOfTwo = do
    e <- Gen.int (Range.constant 0 (w - 1))
    d <- Gen.element [-1, 0, 1]
    s <- Gen.element (if lo < 0 then [1, -1] else [1])
    let v = s * (1 `shiftL` e) + d
    pure (if inRange v then v else 0)
