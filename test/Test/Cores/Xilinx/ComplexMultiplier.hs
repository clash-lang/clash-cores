{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the simulation models in "Clash.Cores.Xilinx.ComplexMultiplier"
  against the bit-accurate C model of the Xilinx Complex Multiplier LogiCORE IP
  v6.0. The tests run the actual functions from
  "Clash.Cores.Xilinx.ComplexMultiplier.Explicit" in Clash simulation, on a
  stream of operands, and compare their output with the output of the C model,
  for many operand and output widths, with truncation and with rounding.

  The C model does not know about the multiplier construction and optimization
  goal of the IP: they do not affect the results. The HDL tests in
  @hdl-tests/shouldwork/Xilinx/ComplexMultiplier.hs@ check that.

  = Running the tests

  These tests are only part of the unit tests if the Cabal flag
  @xilinx-cmpy-cmodel@ is enabled. It is disabled by default, and enabled on CI.
  Xilinx's C model cannot be redistributed, so the tests locate it at run time:

  1. If @CLASH_CORES_XILINX_CMPY_CMODEL@ is set, it should point at either
     @cmpy_v6_0_bitacc_cmodel_lin64.zip@ (Vivado ships it in
     @data\/ip\/xilinx\/cmpy_v6_0\/cmodel@) or a directory holding its
     extracted contents.

  2. Otherwise, if @XILINX_VIVADO@ is set (by Vivado's @settings64.sh@), the
     zip file is taken from that installation.

  If neither is set, or the C model is not where they point, the tests fail.

  Each Hedgehog test case is a batch of 'batchSize' random multiplications; use
  @--hedgehog-tests@ to change the number of batches from 'nBatches'.
-}
module Test.Cores.Xilinx.ComplexMultiplier (tests) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, bracket, catch, onException, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Char (isSpace)
import Data.Complex (Complex (..))
import Data.List (find)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, poke, pokeByteOff)
import GHC.TypeLits (KnownNat, natVal)
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

import Clash.Explicit.Prelude (Bit, Clock, Enable, Signed, System, type (+), type (<=))
import qualified Clash.Explicit.Prelude as C
import qualified Clash.Signal.Delayed as D

import qualified Clash.Cores.Xilinx.ComplexMultiplier.Explicit as Cmpy
import Clash.Cores.Xilinx.ComplexMultiplier.Explicit (NaturalWidth)

import Hedgehog (Gen)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

------------------------------------------------------------------------------
-- Configuration of the tests
------------------------------------------------------------------------------

-- | The C model in a Vivado installation
cmpyCModel :: CModelLocation
cmpyCModel =
  CModelLocation
    { cmlEnvVar = "CLASH_CORES_XILINX_CMPY_CMODEL"
    , cmlZipInVivado = "data/ip/xilinx/cmpy_v6_0/cmodel/cmpy_v6_0_bitacc_cmodel_lin64.zip"
    , cmlTestModule = "test/Test/Cores/Xilinx/ComplexMultiplier.hs"
    }

-- | Number of multiplications per test case
batchSize :: Int
batchSize = 1_000

-- | Number of batches per configuration, unless overridden by
-- @--hedgehog-tests@
nBatches :: H.TestLimit
nBatches = 100

tests :: TestTree
tests =
  withResource (loadCmpy cmpyCModel) unloadCmpy $ \getCModel ->
    testGroup "Xilinx complex multiplier vs. C model"
      [ testGroup "truncate"
          [ truncateTest @8 @8 @17 getCModel
          , truncateTest @8 @8 @16 getCModel
          , truncateTest @8 @8 @9 getCModel
          , truncateTest @8 @8 @2 getCModel
          , truncateTest @16 @16 @33 getCModel
          , truncateTest @16 @16 @16 getCModel
          , truncateTest @18 @25 @44 getCModel
          , truncateTest @25 @18 @20 getCModel
          , truncateTest @27 @35 @17 getCModel
          , truncateTest @35 @27 @50 getCModel
          , truncateTest @63 @8 @40 getCModel
          , truncateTest @8 @63 @3 getCModel
          , truncateTest @63 @63 @127 getCModel
          , truncateTest @63 @63 @64 getCModel
          , truncateTest @63 @63 @2 getCModel
          ]
      , testGroup "round"
          [ roundTest @8 @8 @16 getCModel
          , roundTest @8 @8 @9 getCModel
          , roundTest @8 @8 @2 getCModel
          , roundTest @16 @16 @32 getCModel
          , roundTest @16 @16 @16 getCModel
          , roundTest @18 @25 @30 getCModel
          , roundTest @25 @18 @43 getCModel
          , roundTest @40 @30 @20 getCModel
          , roundTest @8 @63 @40 getCModel
          , roundTest @63 @63 @126 getCModel
          , roundTest @63 @63 @64 getCModel
          , roundTest @63 @63 @2 getCModel
          ]
      ]

------------------------------------------------------------------------------
-- Generic: locating, extracting and loading a Xilinx C model
--
-- Nothing in this section is specific to the Complex Multiplier. The C models
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
-- The Complex Multiplier C model
------------------------------------------------------------------------------

-- | Opaque C types
data XipCmpy
data XipArray
data Mpz

-- | Size of GMP's @mpz_t@: two @int@s and a pointer
mpzSize :: Int
mpzSize = 16

-- | Size of @xip_mpz_complex@: two @mpz_t@s, the real part first
mpzComplexSize :: Int
mpzComplexSize = 2 * mpzSize

-- | Size of @xip_cmpy_v6_0_config@: a pointer and five @int@s, padded
configSize :: Int
configSize = 32

-- | Offsets of the fields of an @xip_array_*@ structure (see
-- @xip_common_bitacc_cmodel.h@) that we set ourselves: the number of data
-- elements, the pointer to the dimensions, and the number of dimensions
arrayDataSizeOffset, arrayDimOffset, arrayDimSizeOffset :: Int
arrayDataSizeOffset = 8
arrayDimOffset = 24
arrayDimSizeOffset = 32

type DefaultConfig = Ptr () -> IO CInt
type Create = Ptr () -> FunPtr () -> Ptr () -> IO (Ptr XipCmpy)
type Destroy = Ptr XipCmpy -> IO CInt
type ArrayCreate = IO (Ptr XipArray)
type ArrayReserve = Ptr XipArray -> CSize -> IO CInt
type ArrayDestroy = Ptr XipArray -> IO (Ptr XipArray)
type ArrayElement = Ptr XipArray -> Ptr () -> CSize -> IO CInt
type DataDo = Ptr XipCmpy -> Ptr XipArray -> Ptr XipArray -> Ptr XipArray -> Ptr XipArray -> IO CInt
type MpzInit = Ptr Mpz -> IO ()
type MpzSetStr = Ptr Mpz -> CString -> CInt -> IO CInt
type MpzGetStr = CString -> CInt -> Ptr Mpz -> IO CString
type MpzSizeInBase = Ptr Mpz -> CInt -> IO CSize

foreign import ccall "dynamic" mkDefaultConfig :: FunPtr DefaultConfig -> DefaultConfig
foreign import ccall "dynamic" mkCreate :: FunPtr Create -> Create
foreign import ccall "dynamic" mkDestroy :: FunPtr Destroy -> Destroy
foreign import ccall "dynamic" mkArrayCreate :: FunPtr ArrayCreate -> ArrayCreate
foreign import ccall "dynamic" mkArrayReserve :: FunPtr ArrayReserve -> ArrayReserve
foreign import ccall "dynamic" mkArrayDestroy :: FunPtr ArrayDestroy -> ArrayDestroy
foreign import ccall "dynamic" mkArrayElement :: FunPtr ArrayElement -> ArrayElement
foreign import ccall "dynamic" mkDataDo :: FunPtr DataDo -> DataDo
foreign import ccall "dynamic" mkMpzInit :: FunPtr MpzInit -> MpzInit
foreign import ccall "dynamic" mkMpzSetStr :: FunPtr MpzSetStr -> MpzSetStr
foreign import ccall "dynamic" mkMpzGetStr :: FunPtr MpzGetStr -> MpzGetStr
foreign import ccall "dynamic" mkMpzSizeInBase :: FunPtr MpzSizeInBase -> MpzSizeInBase

-- | Functions to create and access one type of @xip_array_*@
data ArrayFuns = ArrayFuns
  { afCreate :: ArrayCreate
  , afReserveDim :: ArrayReserve
  , afReserveData :: ArrayReserve
  , afDestroy :: ArrayDestroy
  , afSetElement :: ArrayElement
  , afGetElement :: ArrayElement
  }

-- | The functions of the C model, and of the GMP library it uses
data Cmpy = Cmpy
  { cmLoaded :: LoadedCModel
  , cmLock :: MVar ()
  , cmDefaultConfig :: DefaultConfig
  , cmCreate :: Create
  , cmDestroy :: Destroy
  , cmMpzComplex :: ArrayFuns
  , cmUInt :: ArrayFuns
  , cmDataDo :: DataDo
  , cmMpzInit :: MpzInit
  , cmMpzClear :: MpzInit
  , cmMpzSetStr :: MpzSetStr
  , cmMpzGetStr :: MpzGetStr
  , cmMpzSizeInBase :: MpzSizeInBase
  }

loadCmpy :: CModelLocation -> IO Cmpy
loadCmpy loc = do
  cmLoaded <- loadCModel loc ["libgmp.so.11", "libIp_cmpy_v6_0_bitacc_cmodel.so"]
  lib <- case lcmLibraries cmLoaded of
    l : _ -> pure l
    [] -> fail "No libraries loaded"
  let fun :: (FunPtr a -> a) -> String -> IO a
      fun mk name = mk <$> lookupFunction lib name
      arrayFuns ty = do
        let prefix = "xip_cmpy_v6_0_xip_array_" <> ty <> "_"
        afCreate <- fun mkArrayCreate (prefix <> "create")
        afReserveDim <- fun mkArrayReserve (prefix <> "reserve_dim")
        afReserveData <- fun mkArrayReserve (prefix <> "reserve_data")
        afDestroy <- fun mkArrayDestroy (prefix <> "destroy")
        afSetElement <- fun mkArrayElement (prefix <> "set_element")
        afGetElement <- fun mkArrayElement (prefix <> "get_element")
        pure ArrayFuns{..}
  cmLock <- newMVar ()
  cmDefaultConfig <- fun mkDefaultConfig "xip_cmpy_v6_0_default_config"
  cmCreate <- fun mkCreate "xip_cmpy_v6_0_create"
  cmDestroy <- fun mkDestroy "xip_cmpy_v6_0_destroy"
  cmMpzComplex <- arrayFuns "mpz_complex"
  cmUInt <- arrayFuns "uint"
  cmDataDo <- fun mkDataDo "xip_cmpy_v6_0_mpz_data_do"
  -- GMP's mpz_* functions are macros for these
  cmMpzInit <- fun mkMpzInit "__gmpz_init"
  cmMpzClear <- fun mkMpzInit "__gmpz_clear"
  cmMpzSetStr <- fun mkMpzSetStr "__gmpz_set_str"
  cmMpzGetStr <- fun mkMpzGetStr "__gmpz_get_str"
  cmMpzSizeInBase <- fun mkMpzSizeInBase "__gmpz_sizeinbase"
  pure Cmpy{..}

unloadCmpy :: Cmpy -> IO ()
unloadCmpy Cmpy{cmLoaded} = unloadCModel cmLoaded

-- | Configuration of the C model, see @cmpy_v6_0_bitacc_cmodel.h@
data CConfig = CConfig
  { ccAWidth :: Int
  , ccBWidth :: Int
  , ccOutputWidth :: Int
  , ccRound :: Bool
  }
  deriving (Show)

-- | Inputs of one multiplication: operands A and B, and the round carry
type CInput = (Complex Integer, Complex Integer, Integer)

{- | Run the C model on a list of operands and round carries. Operands are
given as integers in their range, results as integers too.
-}
runCmpy :: Cmpy -> CConfig -> [CInput] -> IO [Complex Integer]
runCmpy Cmpy{..} cfg@CConfig{..} inputs =
  withMVar cmLock $ \() ->
  allocaBytes configSize $ \cfgPtr ->
  allocaBytes mpzComplexSize $ \tmp -> do
    check "default_config" =<< cmDefaultConfig cfgPtr
    let pokeInt off v = pokeByteOff cfgPtr off (fromIntegral v :: CInt)
    pokeInt 8 ccAWidth
    pokeInt 12 ccBWidth
    pokeInt 16 ccOutputWidth
    pokeInt 20 (if ccRound then 1 else 0 :: Int)
    pokeInt 24 (0 :: Int) -- debug
    bracket
      (cmCreate cfgPtr nullFunPtr nullPtr)
      (\h -> when (h /= nullPtr) (void (cmDestroy h)))
      $ \handle -> do
        when (handle == nullPtr) $
          fail ("Could not create the C model for " <> show cfg)
        let n = length inputs
            re = castMpz tmp
            im = castMpz (tmp `plusPtr` mpzSize)
        withArray cmMpzComplex n $ \as ->
          withArray cmMpzComplex n $ \bs ->
          withArray cmUInt n $ \ctrls ->
          withArray cmMpzComplex n $ \outs ->
          bracket (cmMpzInit re >> cmMpzInit im) (const (cmMpzClear re >> cmMpzClear im)) $ \() ->
          allocaBytes 8 $ \cy -> do
            forM_ (zip [0 ..] inputs) $ \(i, (a, b, c)) -> do
              setComplex re im a
              check "set_element" =<< afSetElement cmMpzComplex as (castPtr tmp) i
              setComplex re im b
              check "set_element" =<< afSetElement cmMpzComplex bs (castPtr tmp) i
              poke cy (fromInteger c :: CUInt)
              check "set_element" =<< afSetElement cmUInt ctrls (castPtr cy) i
            check "data_do" =<< cmDataDo handle as bs ctrls outs
            forM [0 .. fromIntegral n - 1] $ \i -> do
              check "get_element" =<< afGetElement cmMpzComplex outs (castPtr tmp) i
              (:+) <$> getMpz re <*> getMpz im
 where
  castMpz :: Ptr a -> Ptr Mpz
  castMpz = castPtr

  check :: String -> CInt -> IO ()
  check what status =
    unless (status == 0) $
      fail ("C model function " <> what <> " failed with status " <> show status)

  -- A one-dimensional array of n elements
  withArray :: ArrayFuns -> Int -> (Ptr XipArray -> IO b) -> IO b
  withArray ArrayFuns{..} n act =
    bracket afCreate afDestroy $ \arr -> do
      when (arr == nullPtr) $ fail "Could not create a C model array"
      check "reserve_dim" =<< afReserveDim arr 1
      pokeByteOff arr arrayDimSizeOffset (1 :: CSize)
      dims <- peekByteOff arr arrayDimOffset
      poke dims (fromIntegral n :: CSize)
      pokeByteOff arr arrayDataSizeOffset (fromIntegral n :: CSize)
      check "reserve_data" =<< afReserveData arr (fromIntegral n)
      act arr

  setComplex :: Ptr Mpz -> Ptr Mpz -> Complex Integer -> IO ()
  setComplex pRe pIm (r :+ i) = setMpz pRe r >> setMpz pIm i

  setMpz :: Ptr Mpz -> Integer -> IO ()
  setMpz p v =
    withCString (sign <> showHex (abs v) "") $ \s ->
      check "mpz_set_str" =<< cmMpzSetStr p s 16
   where
    sign = if v < 0 then "-" else ""

  getMpz :: Ptr Mpz -> IO Integer
  getMpz p = do
    len <- cmMpzSizeInBase p 16
    -- Room for a minus sign and the terminating null character
    allocaBytes (fromIntegral len + 2) $ \buf -> do
      _ <- cmMpzGetStr buf 16 p
      parseHex <$> peekCString buf

  parseHex :: String -> Integer
  parseHex ('-' : s) = negate (parseHex s)
  parseHex s = case readHex (filter (not . isSpace) s) of
    [(v, "")] -> v
    _ -> error ("Cannot parse C model output " <> show s)

------------------------------------------------------------------------------
-- Running the Haskell models
------------------------------------------------------------------------------

clk :: Clock System
clk = C.clockGen

en :: Enable System
en = C.enableGen

-- | Latency used for the tests. It does not affect the results.
type TestDelay = 3

-- | Run a pipelined function with latency @d@ on a list of inputs, and return
-- one output per input
runPipeline ::
  forall d a c.
  (KnownNat d, C.NFDataX a) =>
  (D.DSignal System 0 a -> D.DSignal System d c) ->
  [a] ->
  [c]
runPipeline f xs =
  take (length xs) $
    drop (fromInteger (natVal (Proxy @d))) $
      C.sample_lazy (D.toSignal (f (D.unsafeFromSignal (C.fromList xs'))))
 where
  -- Keep the inputs defined after the end of the list
  xs' = xs <> repeat (last xs)

------------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------------

natInt :: forall n. KnownNat n => Int
natInt = fromInteger (natVal (Proxy @n))

-- | A function under test
data Subject a b o = Subject
  { sName :: String
  , sConfig :: CConfig
  , sHaskell :: [(Bit, Complex (Signed a), Complex (Signed b))] -> [Complex (Signed o)]
  -- ^ Run the Haskell model on a list of round carries and operands
  }

-- | Test 'Cmpy.complexMultiplyWith'
truncateTest ::
  forall a b o.
  ( KnownNat a, KnownNat b, KnownNat o
  , 8 <= a, a <= 63, 8 <= b, b <= 63, 2 <= o, o <= NaturalWidth a b
  ) =>
  IO Cmpy ->
  TestTree
truncateTest getCModel =
  multiplyTest @a @b @o getCModel $
    Subject
      { sName = widthsName @a @b @o
      , sConfig = cConfig @a @b @o False
      , sHaskell =
          runPipeline @TestDelay $ \inp ->
            Cmpy.complexMultiplyWith @TestDelay Cmpy.defConfig clk en
              ((\(_, x, _) -> x) <$> inp)
              ((\(_, _, y) -> y) <$> inp)
      }

-- | Test 'Cmpy.complexMultiplyRoundWith'
roundTest ::
  forall a b o.
  ( KnownNat a, KnownNat b, KnownNat o
  , 8 <= a, a <= 63, 8 <= b, b <= 63, 2 <= o, o + 1 <= NaturalWidth a b
  ) =>
  IO Cmpy ->
  TestTree
roundTest getCModel =
  multiplyTest @a @b @o getCModel $
    Subject
      { sName = widthsName @a @b @o
      , sConfig = cConfig @a @b @o True
      , sHaskell =
          runPipeline @TestDelay $ \inp ->
            Cmpy.complexMultiplyRoundWith @TestDelay Cmpy.defConfig clk en
              ((\(c, _, _) -> c) <$> inp)
              ((\(_, x, _) -> x) <$> inp)
              ((\(_, _, y) -> y) <$> inp)
      }

cConfig :: forall a b o. (KnownNat a, KnownNat b, KnownNat o) => Bool -> CConfig
cConfig rnd =
  CConfig
    { ccAWidth = natInt @a
    , ccBWidth = natInt @b
    , ccOutputWidth = natInt @o
    , ccRound = rnd
    }

-- | Name of a test, without slashes: Tasty patterns would split names at them
widthsName :: forall a b o. (KnownNat a, KnownNat b, KnownNat o) => String
widthsName =
  show (natInt @a) <> "-bit A, " <> show (natInt @b) <> "-bit B, "
    <> show (natInt @o) <> "-bit output"

-- | Compare a function with the C model, on batches of random operands
multiplyTest ::
  forall a b o.
  (KnownNat a, KnownNat b, KnownNat o) =>
  IO Cmpy ->
  Subject a b o ->
  TestTree
multiplyTest getCModel subject@Subject{sName} =
  testPropertyNamed sName (fromString ("prop_" <> sName)) $
    -- Shrinking a batch takes long, and the failure shows the failing input
    H.withShrinks 0 $ H.withTests nBatches $ H.property $ do
      batch <- H.forAllWith (const "<batch>") (Gen.list (Range.singleton batchSize) (genInput @a @b @o))
      cmodel <- H.evalIO getCModel
      (failure, ties) <- H.evalIO (compareBatch cmodel subject batch)
      -- Without discarded bits, there are no ties
      when (natInt @o < natInt @a + natInt @b + 1) $
        H.cover 10 "has a tie" ties
      mapM_ (\msg -> H.footnote msg >> H.failure) failure

{- | Run a batch of multiplications on both the C model and the Haskell model,
and compare the results. Gives a description of the first mismatch, if any, and
whether the batch contains a product that lies exactly halfway between two
outputs.
-}
compareBatch ::
  forall a b o.
  (KnownNat a, KnownNat b, KnownNat o) =>
  Cmpy ->
  Subject a b o ->
  [(Bit, Complex (Signed a), Complex (Signed b))] ->
  IO (Maybe String, Bool)
compareBatch cmodel Subject{sConfig, sHaskell} inputs = do
  cResults <-
    runCmpy cmodel sConfig
      [ (toInteger <$> x, toInteger <$> y, toInteger c) | (c, x, y) <- inputs ]
  let
    compared = zip3 inputs (sHaskell inputs) cResults
    ok (_, h, c) = (toInteger <$> h) == c
    mismatch ((c, x, y), h, cm) =
      unlines
        [ "A: " <> show x <> ", B: " <> show y <> ", round carry: " <> show c
        , "Exact product: " <> show (exact x y)
        , "Haskell model: " <> show h
        , "C model: " <> show cm
        ]
  pure
    ( mismatch <$> find (not . ok) compared
    , any (\(_, x, y) -> any isTie (exact x y)) inputs
    )
 where
  exact x y = cProduct (toInteger <$> x) (toInteger <$> y)
  k = ccAWidth sConfig + ccBWidth sConfig + 1 - ccOutputWidth sConfig
  isTie p = k >= 1 && (p `mod` (1 `shiftL` k)) == 1 `shiftL` (k - 1)

-- | Exact complex product, computed independently of the simulation model
cProduct :: Complex Integer -> Complex Integer -> Complex Integer
cProduct (ar :+ ai) (br :+ bi) = (ar * br - ai * bi) :+ (ar * bi + ai * br)

------------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------------

{- | Round carry and operands. Besides uniformly random operands, the
generator picks:

* Edge cases: zero, plus and minus one, the extremes of the operand ranges,
  such as the most negative number times itself, which needs all bits of the
  natural width.

* Operands with many trailing zeros, so that the bits of the product that the
  IP discards are often all zero, or exactly one half: the products where
  truncation and rounding differ the most, and where the round carry matters.
  The trailing zeros of the operands add up to @k - 1@ or @k@, with
  @k = a + b + 1 - o@ the number of discarded bits.

* Operands just next to those, so that the discarded bits are just below or
  above one half.
-}
genInput ::
  forall a b o.
  (KnownNat a, KnownNat b, KnownNat o) =>
  Gen (Bit, Complex (Signed a), Complex (Signed b))
genInput = do
  cy <- Gen.element [0, 1]
  (x, y) <-
    Gen.frequency
      [ (3, (,) <$> genComplex @a 0 <*> genComplex @b 0)
      , (1, (,) <$> genEdge @a <*> genEdge @b)
      , (4, genScaled)
      ]
  pure (cy, x, y)
 where
  k = natInt @a + natInt @b + 1 - natInt @o
  genScaled = do
    -- Trailing zeros of A and B, adding up to k - 1 or k
    total <- Gen.element [max 0 (k - 1), k]
    sa <- Gen.int (Range.constant (max 0 (total - (natInt @b - 1))) (min (natInt @a - 1) total))
    let sb = max 0 (min (natInt @b - 1) (total - sa))
    (,) <$> genComplex @a sa <*> genComplex @b sb

-- | A complex number whose components are multiples of @2^s@, give or take
-- one now and then
genComplex :: forall n. KnownNat n => Int -> Gen (Complex (Signed n))
genComplex s = (:+) <$> component <*> component
 where
  component = do
    v <- Gen.integral (Range.constantFrom 0 (lo `shiftR` s) (hi `shiftR` s))
    d <- Gen.frequency [(6, pure 0), (1, pure 1), (1, pure (-1))]
    pure (fromInteger (clamp ((v `shiftL` s) + d)))
  (lo, hi) = signedRange @n
  clamp = max lo . min hi

-- | A complex number with edge cases as components
genEdge :: forall n. KnownNat n => Gen (Complex (Signed n))
genEdge = (:+) <$> component <*> component
 where
  component = fromInteger <$> Gen.element [0, 1, -1, lo, lo + 1, hi, hi - 1]
  (lo, hi) = signedRange @n

signedRange :: forall n. KnownNat n => (Integer, Integer)
signedRange = (negate (1 `shiftL` (w - 1)), (1 `shiftL` (w - 1)) - 1)
 where
  w = natInt @n
