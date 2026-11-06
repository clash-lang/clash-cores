{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the simulation model in "Clash.Cores.Xilinx.Fir" against the
  bit-accurate C model of the Xilinx FIR Compiler LogiCORE IP v7.2. Each test
  case is a random filter: random data and coefficient types and widths, a
  random number of coefficients with random values (biased towards symmetric,
  half-band and sparse sets and towards extreme values), a random rounding
  mode and output width. The test runs 'Clash.Cores.Xilinx.Fir.Explicit.firWith'
  in Clash simulation on a batch of random samples (biased towards extremes
  and runs of extremes) and compares its outputs with the outputs of the C
  model for the same filter and samples.

  The C model does not model timing, so these tests only check the numbers.
  The HDL tests check the timing against the IP itself.

  = Running the tests

  These tests are only part of the unit tests if the Cabal flag
  @xilinx-fir-cmodel@ is enabled. It is disabled by default, and enabled on
  CI. Xilinx's C model cannot be redistributed, so the tests locate it at run
  time:

  1. If @CLASH_CORES_XILINX_FIR_CMODEL@ is set, it should point at either
     @fir_compiler_v7_2_bitacc_cmodel_lin64.zip@ (Vivado ships it in
     @data\/ip\/xilinx\/fir_compiler_v7_2\/cmodel@) or a directory holding its
     extracted contents.

  2. Otherwise, if @XILINX_VIVADO@ is set (by Vivado's @settings64.sh@), the
     zip file is taken from that installation.

  If neither is set, or the C model is not where they point, the tests fail.

  The property runs 'nTests' filters of 'batchSize' samples each. The number of
  filters can be set with @--hedgehog-tests@.

  The C model's shared library uses GMP, which is also linked into Haskell
  programs. It is loaded with @RTLD_DEEPBIND@, so that it uses its own copy of
  GMP (which ships with the C model) instead of the one in the test program.

  = Differences between the C model and the IP

  * The C model knows no signedness: it takes the data and coefficients as
    numbers. It computes the full precision output width from the data width
    as if the data were signed. For unsigned data and signed coefficients, the
    IP's output has an extra (sign) bit, so the tests give the C model a data
    width of one more bit for those.

  * The C model computes the full precision output width in floating point,
    which makes it one bit too wide or too narrow for some coefficients of
    which the absolute values add up to (nearly) a large power of two, such as
    @[2^28, 2^28]@ or @[2^48 - 1, 2^48 - 1, 1, 1, 1]@. Vivado computes the
    width the IP uses exactly. The tests read the full precision width of the
    C model, and adjust the output width they give it accordingly, so that it
    discards the same bits as the IP.

  * The C model does not wrap outputs that do not fit the output width, the
    IP does (see the HDL test @firOverflow@). The tests wrap the outputs of the
    C model.
-}
module Test.Cores.Xilinx.Fir (tests) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, bracket, catch, onException, try)
import Control.Monad (forM, unless, void, when)
import Data.Bits (shiftL, (.|.))
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Word (Word32, Word64)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CDouble (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray, withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.TypeNats (SomeNat (..), someNatVal)
import System.Directory
  ( createDirectory, doesDirectoryExist, doesFileExist, makeAbsolute
  , removeDirectoryRecursive )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (arch, os)
import System.Process (readProcessWithExitCode)

import Clash.Explicit.Prelude (KnownNat, Signed, System, Unsigned, Vec, natToNum)
import qualified Clash.Explicit.Prelude as C
import qualified Clash.Sized.Vector as V
import Clash.Signal.Delayed (DSignal)
import qualified Clash.Signal.Delayed as D

import Clash.Cores.Xilinx.Fir.Explicit
  (Config (..), CoefficientStructure (..), DspSlice (..), Rounding (..), FirNumber, FirOutput)
import qualified Clash.Cores.Xilinx.Fir.Explicit as X
import Clash.Cores.Xilinx.Fir.Internal (Structure (..), Params (..), mkParams, latency)
import Data.Maybe (catMaybes)

import Hedgehog (Gen, Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

-- Locating and loading Xilinx C models
--
-- Nothing in this section is specific to the FIR Compiler. It can be shared
-- with the tests of other IP cores with a C model, once there are more of them.
------------------------------------------------------------------------------

-- | Where to find a C model
data CModelLocation = CModelLocation
  { envVar :: String
    -- ^ Environment variable that can point at the zip file or a directory
    -- with its extracted contents
  , zipInVivado :: FilePath
    -- ^ Location of the zip file in a Vivado installation
  }

-- | A C model's shared libraries, loaded into the test process
data LoadedCModel = LoadedCModel
  { tmpDir :: FilePath
  , libraries :: [Ptr ()]
    -- ^ In reverse order of loading
  , lock :: MVar ()
    -- ^ Xilinx does not document whether the C models are thread safe, so we
    -- call them from one thread at a time.
  }

-- | Find a C model, see the module documentation. Returns a path to the zip
-- file or directory.
findCModel :: CModelLocation -> IO FilePath
findCModel CModelLocation{envVar, zipInVivado}
  | os /= "linux" || arch /= "x86_64" =
      fail "The C model is only available for x86_64 Linux."
  | otherwise = do
      explicit <- lookupEnv envVar
      vivado <- lookupEnv "XILINX_VIVADO"
      case (explicit, vivado) of
        (Just path, _) | not (null path) -> pure path
        (_, Just dir) | not (null dir) -> do
          let path = dir </> zipInVivado
          exists <- doesFileExist path
          unless exists $
            fail ("XILINX_VIVADO is set, but " <> path <> " does not exist.")
          pure path
        _ ->
          fail $
            "Set " <> envVar <> " to the C model's zip file"
              <> " (or a directory with its extracted contents), or set XILINX_VIVADO."

-- | Find a C model, extract it if needed, and load its shared libraries in
-- the given order: dependencies first.
loadCModel :: CModelLocation -> [FilePath] -> IO LoadedCModel
loadCModel loc libs = do
  path <- findCModel loc
  tmpRoot <- getCanonicalTemporaryDirectory
  tmp <- createTempDirectory tmpRoot "clash-cores-xilinx-cmodel"
  (`onException` removeDirectoryRecursive tmp) $ do
    modelDir <- extractCModel loc tmp path
    handles <- forM libs $ \lib -> loadLibrary (modelDir </> lib)
    lock <- newMVar ()
    pure LoadedCModel{tmpDir = tmp, libraries = reverse handles, lock}

unloadCModel :: LoadedCModel -> IO ()
unloadCModel LoadedCModel{tmpDir, libraries} = do
  mapM_ c_dlclose libraries
  void (try @SomeException (removeDirectoryRecursive tmpDir))

-- | Return the directory with the C model, extracting the zip file to a
-- temporary directory if the path points at it.
extractCModel :: CModelLocation -> FilePath -> FilePath -> IO FilePath
extractCModel CModelLocation{envVar} tmp path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if
    | isDir -> makeAbsolute path
    | isFile -> do
        let dir = tmp </> "cmodel"
            args = ["-q", path, "-d", dir]
        createDirectory dir
        (code, out, err) <-
          readProcessWithExitCode "unzip" args "" `catch` \(e :: IOException) ->
            fail $
              show e
                <> "\nExtracting the zip file requires unzip. Alternatively, set "
                <> envVar <> " to a directory with its extracted contents."
        case code of
          ExitSuccess -> pure dir
          ExitFailure n ->
            fail $
              unlines
                [ "Command failed with exit code " <> show n <> ": " <> unwords ("unzip" : args)
                , out
                , err
                ]
    | otherwise ->
        fail (path <> " is neither a file nor a directory")

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


------------------------------------------------------------------------------
-- The FIR Compiler C model
------------------------------------------------------------------------------

firLocation :: CModelLocation
firLocation = CModelLocation
  { envVar = "CLASH_CORES_XILINX_FIR_CMODEL"
  , zipInVivado = "data/ip/xilinx/fir_compiler_v7_2/cmodel/fir_compiler_v7_2_bitacc_cmodel_lin64.zip"
  }

-- | @xip_array_real*@ or @xip_array_mpz*@
type XipArray = Ptr ()

-- | @xip_fir_v7_2*@
type XipFir = Ptr ()

type ArrayCreate = IO XipArray
type ArrayReserve = XipArray -> CSize -> IO CInt
type ArrayDestroy = XipArray -> IO XipArray
-- | @xip_fir_v7_2_default_config(config)@
type DefaultConfig = Ptr () -> IO CInt
-- | @xip_fir_v7_2_create(config, handler, handle)@
type Create = Ptr () -> FunPtr () -> Ptr () -> IO XipFir
type Destroy = XipFir -> IO CInt
-- | @xip_fir_v7_2_get_config(model, config)@
type GetConfig = XipFir -> Ptr () -> IO CInt
-- | @xip_fir_v7_2_calc_size_mpz(model, data_in, data_out, cmplx_data_out)@
type CalcSize = XipFir -> XipArray -> XipArray -> Ptr () -> IO CInt
-- | @xip_fir_v7_2_data_send(model, data_in)@
type DataSend = XipFir -> XipArray -> IO CInt
-- | @xip_fir_v7_2_data_get_mpz(model, data_out, cmplx_data_out)@
type DataGet = XipFir -> XipArray -> Ptr () -> IO CInt

foreign import ccall "dynamic" mkArrayCreate :: FunPtr ArrayCreate -> ArrayCreate
foreign import ccall "dynamic" mkArrayReserve :: FunPtr ArrayReserve -> ArrayReserve
foreign import ccall "dynamic" mkArrayDestroy :: FunPtr ArrayDestroy -> ArrayDestroy
foreign import ccall "dynamic" mkDefaultConfig :: FunPtr DefaultConfig -> DefaultConfig
foreign import ccall "dynamic" mkCreate :: FunPtr Create -> Create
foreign import ccall "dynamic" mkDestroy :: FunPtr Destroy -> Destroy
foreign import ccall "dynamic" mkGetConfig :: FunPtr GetConfig -> GetConfig
foreign import ccall "dynamic" mkCalcSize :: FunPtr CalcSize -> CalcSize
foreign import ccall "dynamic" mkDataSend :: FunPtr DataSend -> DataSend
foreign import ccall "dynamic" mkDataGet :: FunPtr DataGet -> DataGet

-- | Functions for one kind of array (real or mpz)
data ArrayFuns = ArrayFuns
  { arrCreate :: ArrayCreate
  , arrReserveData :: ArrayReserve
  , arrReserveDim :: ArrayReserve
  , arrDestroy :: ArrayDestroy
  }

data FirCModel = FirCModel
  { loaded :: LoadedCModel
  , realArray :: ArrayFuns
  , mpzArray :: ArrayFuns
  , defaultConfig :: DefaultConfig
  , create :: Create
  , destroy :: Destroy
  , getConfig :: GetConfig
  , calcSize :: CalcSize
  , dataSend :: DataSend
  , dataGet :: DataGet
  }

loadFir :: IO FirCModel
loadFir = do
  loaded@LoadedCModel{libraries} <-
    loadCModel firLocation ["libgmp.so.11", "libIp_fir_compiler_v7_2_bitacc_cmodel.so"]
  lib <- case libraries of
    l : _ -> pure l
    [] -> fail "No libraries loaded"
  let
    fun :: (FunPtr a -> a) -> String -> IO a
    fun mk name = mk <$> lookupFunction lib ("xip_fir_v7_2_" <> name)
    arrayFuns kind =
      ArrayFuns
        <$> fun mkArrayCreate ("xip_array_" <> kind <> "_create")
        <*> fun mkArrayReserve ("xip_array_" <> kind <> "_reserve_data")
        <*> fun mkArrayReserve ("xip_array_" <> kind <> "_reserve_dim")
        <*> fun mkArrayDestroy ("xip_array_" <> kind <> "_destroy")
  FirCModel loaded
    <$> arrayFuns "real"
    <*> arrayFuns "mpz"
    <*> fun mkDefaultConfig "default_config"
    <*> fun mkCreate "create"
    <*> fun mkDestroy "destroy"
    <*> fun mkGetConfig "get_config"
    <*> fun mkCalcSize "calc_size_mpz"
    <*> fun mkDataSend "data_send"
    <*> fun mkDataGet "data_get_mpz"

unloadFir :: FirCModel -> IO ()
unloadFir = unloadCModel . loaded

-- | Byte offsets in @xip_array_real@ and @xip_array_mpz@
offData, offDataSize, offDim, offDimSize :: Int
offData = 0
offDataSize = 8
offDim = 24
offDimSize = 32

-- | Byte offsets in @xip_fir_v7_2_config@
offName, offFilterType, offCoeff, offNumCoeffs, offCoeffSets, offQuantization,
  offCoeffWidth, offNumChannels, offNumPaths, offDataWidth, offRounding,
  offOutputWidth :: Int
offName = 0
offFilterType = 8
offCoeff = 32
offNumCoeffs = 44
offCoeffSets = 48
offQuantization = 60
offCoeffWidth = 64
offNumChannels = 76
offNumPaths = 84
offDataWidth = 88
offRounding = 96
offOutputWidth = 100

-- | Size of @xip_fir_v7_2_config@
configSize :: Int
configSize = 112

-- | Create an empty array, run an action with it and destroy it
withXipArray :: ArrayFuns -> (XipArray -> IO a) -> IO a
withXipArray ArrayFuns{..} act = bracket acquire arrDestroy act
 where
  acquire = do
    arr <- arrCreate
    when (arr == nullPtr) $ fail "Could not create C model array"
    checkStatus "reserve_dim" =<< arrReserveDim arr 3
    pure arr

checkStatus :: String -> CInt -> IO ()
checkStatus what status =
  unless (status == 0) $ fail ("C model call failed: " <> what)

-- | Read an @mpz_t@: its size (negative for negative numbers) and its limbs
peekMpz :: Ptr () -> IO Integer
peekMpz p = do
  size <- peekByteOff p 4 :: IO Int32
  limbsPtr <- peekByteOff p 8 :: IO (Ptr Word64)
  limbs <- peekArray (fromIntegral (abs size)) limbsPtr
  let magnitude = foldr (\l acc -> acc `shiftL` 64 .|. toInteger l) 0 limbs
  pure (if size < 0 then negate magnitude else magnitude)

-- | Run the C model of a filter on a list of samples. Gives the outputs of the
-- C model, which need wrapping to the output width ('wrapOutput'), or
-- 'Nothing' if the C model cannot discard the same bits as the IP (see the
-- module documentation).
runCModel :: FirCModel -> TestConfig -> [Integer] -> IO (Maybe [Integer])
runCModel model@FirCModel{loaded = LoadedCModel{lock}} tc@TestConfig{..} xs =
  withMVar lock $ \() ->
  allocaBytes configSize $ \cfgPtr ->
  withCString "clash-cores" $ \name ->
  withArrayLen (map fromInteger tcCoefficients :: [CDouble]) $ \nCoefs coefPtr -> do
    let
      pokeConfig rnd outputWidth = do
        checkStatus "default_config" =<< defaultConfig model cfgPtr
        pokeByteOff cfgPtr offName name
        pokeByteOff cfgPtr offFilterType (0 :: Word32)
        pokeByteOff cfgPtr offCoeff coefPtr
        pokeByteOff cfgPtr offNumCoeffs (fromIntegral nCoefs :: Word32)
        pokeByteOff cfgPtr offCoeffSets (1 :: Word32)
        pokeByteOff cfgPtr offQuantization (0 :: Word32)
        pokeByteOff cfgPtr offCoeffWidth (fromIntegral tcCoefWidth :: Word32)
        pokeByteOff cfgPtr offNumChannels (1 :: Word32)
        pokeByteOff cfgPtr offNumPaths (1 :: Word32)
        pokeByteOff cfgPtr offDataWidth (fromIntegral cDataWidth :: Word32)
        pokeByteOff cfgPtr offRounding (roundingCode rnd)
        pokeByteOff cfgPtr offOutputWidth (fromIntegral outputWidth :: Word32)
      withModel :: (XipFir -> IO a) -> IO a
      withModel act =
        bracket (create model cfgPtr nullFunPtr nullPtr) (destroy model) $ \h -> do
          when (h == nullPtr) $ fail ("Could not create C model for " <> show tc)
          act h
    -- The full precision output width of the C model, see the module
    -- documentation
    pokeConfig FullPrecision 0
    cFullWidth <- withModel $ \h -> do
      checkStatus "get_config" =<< getConfig model h cfgPtr
      fromIntegral <$> (peekByteOff cfgPtr offOutputWidth :: IO Word32)
    let
      extraBits = cFullWidth - fullWidth (tcParams tc)
      cOutputWidth = tcOutputWidth + extraBits
    unless (extraBits `elem` [-1, 0, 1]) $
      fail ("Unexpected full precision output width of the C model: " <> show cFullWidth)
    if tcRounding /= FullPrecision && cOutputWidth < 1 then pure Nothing else do
     pokeConfig tcRounding cOutputWidth
     fmap Just $ withModel $ \h ->
      withXipArray (realArray model) $ \din ->
        withXipArray (mpzArray model) $ \dout -> do
          checkStatus "reserve_data" =<< arrReserveData (realArray model) din (fromIntegral n)
          pokeByteOff din offDimSize (3 :: CSize)
          dim <- peekByteOff din offDim :: IO (Ptr CSize)
          pokeArray dim [1, 1, fromIntegral n]
          pokeByteOff din offDataSize (fromIntegral n :: CSize)
          dat <- peekByteOff din offData :: IO (Ptr CDouble)
          pokeArray dat (map fromInteger xs)
          pokeByteOff dout offDimSize (3 :: CSize)
          checkStatus "calc_size" =<< calcSize model h din dout nullPtr
          nOut <- peekByteOff dout offDataSize :: IO CSize
          checkStatus "reserve_data" =<< arrReserveData (mpzArray model) dout nOut
          checkStatus "data_send" =<< dataSend model h din
          checkStatus "data_get" =<< dataGet model h dout nullPtr
          outPtr <- peekByteOff dout offData :: IO (Ptr ())
          forM [0 .. fromIntegral nOut - 1] $ \i -> peekMpz (outPtr `plusPtr` (16 * i))
 where
  n = length xs
  -- See the module documentation
  cDataWidth
    | not tcDataSigned && any (< 0) tcCoefficients = tcDataWidth + 1
    | otherwise = tcDataWidth
  roundingCode :: Rounding -> Word32
  roundingCode = \case
    FullPrecision -> 0
    TruncateLsbs -> 1
    SymmetricRoundingToZero -> 2
    SymmetricRoundingToInfinity -> 3
    ConvergentRoundingToEven -> 4
    ConvergentRoundingToOdd -> 5
    NonSymmetricRoundingDown -> 6
    NonSymmetricRoundingUp -> 7

------------------------------------------------------------------------------
-- Running the Clash function
------------------------------------------------------------------------------

-- | A filter under test
data TestConfig = TestConfig
  { tcDataWidth :: Int
  , tcDataSigned :: Bool
  , tcCoefWidth :: Int
  , tcCoefSigned :: Bool
    -- ^ The type of the coefficients
  , tcCoefficients :: [Integer]
  , tcRounding :: Rounding
  , tcOutputWidth :: Int
  , tcConfig :: Config
  } deriving (Show)

tcParams :: TestConfig -> Params
tcParams TestConfig{..} =
  mkParams tcConfig{rounding = tcRounding} tcCoefficients tcDataWidth tcDataSigned
    tcCoefWidth tcCoefSigned tcOutputWidth 1

withNat :: Int -> (forall n. KnownNat n => Proxy n -> r) -> r
withNat i f
  | i < 0 = error ("withNat: negative " <> show i)
  | otherwise = case someNatVal (fromIntegral i) of SomeNat p -> f p

-- | Run a Clash function on a list of samples and return its outputs: one per
-- sample. The function ignores the sample of the first clock cycle, so a dummy
-- sample goes first.
simulate ::
  forall d a b.
  (KnownNat d, C.NFDataX a, Num a) =>
  (DSignal System 0 a -> DSignal System d b) ->
  [a] ->
  [b]
simulate f xs =
  take (length xs) $ drop (1 + natToNum @d) $
    C.sample_lazy (D.toSignal (f (D.fromSignal (C.fromList (0 : xs <> repeat 0)))))

clk :: C.Clock System
clk = C.clockGen

en :: C.Enable System
en = C.enableGen

-- | Run 'X.firWith' for a configuration on a list of samples
runClash :: TestConfig -> [Integer] -> [Integer]
runClash tc@TestConfig{..} xs =
  withNat (latency (tcParams tc)) $ \(Proxy :: Proxy d) ->
  withNat tcDataWidth $ \(Proxy :: Proxy wd) ->
  withNat tcCoefWidth $ \(Proxy :: Proxy wc) ->
  withNat tcOutputWidth $ \(Proxy :: Proxy wo) ->
  withNat (length tcCoefficients) $ \(Proxy :: Proxy n) ->
    case (tcDataSigned, tcCoefSigned) of
      (True, True) -> go @d @Signed @Signed @wd @wc @wo @n
      (True, False) -> go @d @Signed @Unsigned @wd @wc @wo @n
      (False, True) -> go @d @Unsigned @Signed @wd @wc @wo @n
      (False, False) -> go @d @Unsigned @Unsigned @wd @wc @wo @n
 where
  go ::
    forall d a c wd wc wo n.
    ( KnownNat d, KnownNat wd, KnownNat wc, KnownNat wo, KnownNat n
    , FirNumber a, FirNumber c, FirNumber (FirOutput a c)
    , C.NFDataX (a wd) ) =>
    [Integer]
  go =
    map toInteger $
      simulate
        (X.firWith @d @a @c @wd @wc @wo tcConfig{rounding = tcRounding} coefs clk en)
        (map fromInteger xs :: [a wd])
   where
    coefs = V.unsafeFromList (map fromInteger tcCoefficients) :: Vec n (c wc)

-- | Run 'X.firValidWith' for a configuration and sample period on a list of
-- valid-qualified samples. Gives the samples the IP accepted, and the valid
-- outputs.
runClashValid :: TestConfig -> Int -> [Maybe Integer] -> ([Integer], [Integer])
runClashValid tc@TestConfig{..} p xs =
  withNat (latency (tcParamsP tc p)) $ \(Proxy :: Proxy d) ->
  withNat p $ \(Proxy :: Proxy p) ->
  withNat tcDataWidth $ \(Proxy :: Proxy wd) ->
  withNat tcCoefWidth $ \(Proxy :: Proxy wc) ->
  withNat tcOutputWidth $ \(Proxy :: Proxy wo) ->
  withNat (length tcCoefficients) $ \(Proxy :: Proxy n) ->
    case C.compareSNat (C.SNat @1) (C.SNat @p) of
      C.SNatGT -> error "runClashValid: sample period 0"
      C.SNatLE -> case (tcDataSigned, tcCoefSigned) of
        (True, True) -> go @d @p @Signed @Signed @wd @wc @wo @n
        (True, False) -> go @d @p @Signed @Unsigned @wd @wc @wo @n
        (False, True) -> go @d @p @Unsigned @Signed @wd @wc @wo @n
        (False, False) -> go @d @p @Unsigned @Unsigned @wd @wc @wo @n
 where
  go ::
    forall d p a c wd wc wo n.
    ( KnownNat d, KnownNat p, 1 C.<= p, KnownNat wd, KnownNat wc, KnownNat wo, KnownNat n
    , FirNumber a, FirNumber c, FirNumber (FirOutput a c)
    , C.NFDataX (a wd), C.NFDataX (FirOutput a c wo) ) =>
    ([Integer], [Integer])
  go = (accepted, map toInteger (catMaybes outs))
   where
    coefs = V.unsafeFromList (map fromInteger tcCoefficients) :: Vec n (c wc)
    -- Enough cycles to flush the last output
    nCycles = length xs + natToNum @d + 1
    ins = map (fmap fromInteger) xs <> repeat Nothing :: [Maybe (a wd)]
    (ready, out) =
      X.firValidWith @d @p @a @c @wd @wc @wo tcConfig{rounding = tcRounding} C.SNat coefs clk en
        (D.fromSignal (C.fromList ins))
    readys = C.sampleN nCycles (D.toSignal ready)
    outs = C.sampleN nCycles (D.toSignal out)
    accepted = [ x | (r, Just x) <- zip readys xs, r ]

tcParamsP :: TestConfig -> Int -> Params
tcParamsP TestConfig{..} p =
  mkParams tcConfig{rounding = tcRounding} tcCoefficients tcDataWidth tcDataSigned
    tcCoefWidth tcCoefSigned tcOutputWidth p

-- | Wrap a C model output to the output width and signedness of the IP
wrapOutput :: TestConfig -> Integer -> Integer
wrapOutput TestConfig{..} y
  | outputSigned = ((y + half) `mod` (2 * half)) - half
  | otherwise = y `mod` (2 * half)
 where
  outputSigned = tcDataSigned || tcCoefSigned
  half = 1 `shiftL` (tcOutputWidth - 1)

------------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------------

-- | Number of samples per Hedgehog test case
batchSize :: Int
batchSize = 300

-- | Number of filters, unless overridden by @--hedgehog-tests@
nTests :: H.TestLimit
nTests = 300

prop_cmodel :: IO FirCModel -> Property
prop_cmodel getModel = H.withTests nTests $ H.property $ do
  model <- H.evalIO getModel
  tc <- H.forAll genTestConfig
  xs <- H.forAllWith showBatch (genSamples tc)
  cOut <- H.evalIO (runCModel model tc xs) >>= maybe H.discard pure
  let
    hOut = runClash tc xs
    expected = map (wrapOutput tc) cOut
    mismatches =
      [ (i, e, h) | (i, e, h) <- zip3 [0 :: Int ..] expected hOut, e /= h ]
  H.annotateShow (structure (tcParams tc), latency (tcParams tc), fullWidth (tcParams tc))
  H.label (fromString (show (tcRounding tc)))
  H.label (fromString (show (structure (tcParams tc))))
  length cOut === length xs
  case mismatches of
    [] -> pure ()
    ((i, e, h) : _) -> do
      H.footnote ("first mismatch at sample " <> show i <> " of " <> show (length mismatches))
      H.footnote ("samples before it: " <> show (reverse (take 8 (reverse (take (i + 1) xs)))))
      H.footnote ("C model outputs before it: " <> show (reverse (take 8 (reverse (take (i + 1) expected)))))
      H.footnote ("Clash outputs before it: " <> show (reverse (take 8 (reverse (take (i + 1) hOut)))))
      e === h
 where
  showBatch xs = show (length xs) <> " samples"

-- | 'X.firValidWith' with random sample periods and valid-qualified samples:
-- its outputs are the outputs of the C model for the samples it accepted,
-- delayed by the 'frontPadding' of the IP
prop_cmodelValid :: IO FirCModel -> Property
prop_cmodelValid getModel = H.withTests (nTests `div` 3) $ H.property $ do
  model <- H.evalIO getModel
  tc <- H.forAll genTestConfig
  p <- H.forAll (Gen.frequency [(1, pure 1), (4, Gen.int (Range.constant 2 16))])
  xs <- H.forAllWith showBatch (genSamples tc)
  valids <- H.forAllWith showBatch (Gen.list (Range.singleton (length xs)) (Gen.frequency [(1, pure True), (p, pure False)]))
  let
    (accepted, hOut) = runClashValid tc p (zipWith (\v x -> if v then Just x else Nothing) valids xs)
    padding = frontPadding (tcParamsP tc p)
  cOut <- H.evalIO (runCModel model tc accepted) >>= maybe H.discard pure
  let expected = take (length accepted) (replicate padding 0 <> map (wrapOutput tc) cOut)
  H.annotateShow (structure (tcParamsP tc p), madds (tcParamsP tc p), oversampling (tcParamsP tc p), padding)
  H.label (fromString ("padding " <> show (min 2 padding)))
  length hOut === length accepted
  hOut === expected
 where
  showBatch xs = show (length xs) <> " elements"

------------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------------

-- | Integer in a range, biased towards the ends and zero
genIntegerIn :: Integer -> Integer -> Gen Integer
genIntegerIn lo hi =
  Gen.frequency
    [ (8, Gen.integral (Range.constant lo hi))
    , (1, Gen.integral (Range.constant lo (min hi (lo + 4))))
    , (1, Gen.integral (Range.constant (max lo (hi - 4)) hi))
    , (1, Gen.integral (Range.constant (max lo (-4)) (min hi 4)))
    ]

range :: Bool -> Int -> (Integer, Integer)
range signed w
  | signed = (negate (1 `shiftL` (w - 1)), (1 `shiftL` (w - 1)) - 1)
  | otherwise = (0, (1 `shiftL` w) - 1)

-- | A filter: types, coefficients, rounding and output width
genTestConfig :: Gen TestConfig
genTestConfig = do
  tcCoefWidth <- Gen.frequency [(3, Gen.int (Range.constant 2 18)), (1, Gen.int (Range.constant 2 48))]
  let maxData = if tcCoefWidth > 34 then 34 else 48
  tcDataWidth <- Gen.frequency [(3, Gen.int (Range.constant 2 25)), (1, Gen.int (Range.constant 2 maxData))]
  tcDataSigned <- Gen.bool
  tcCoefSigned <- Gen.bool
  n <- Gen.frequency
    [ (6, Gen.int (Range.constant 2 16))
    , (3, Gen.int (Range.constant 2 64))
    , (1, Gen.int (Range.constant 2 200))
    ]
  coefs0 <- genCoefficients n tcCoefWidth tcCoefSigned
  -- With unsigned data, signed coefficients need a negative coefficient
  let
    (lo, _) = range tcCoefSigned tcCoefWidth
    tcCoefficients
      | not tcDataSigned && tcCoefSigned && all (>= 0) coefs0 = lo : drop 1 coefs0
      | otherwise = coefs0
  tcRounding <- Gen.element [minBound .. maxBound]
  dsp <- Gen.element [Dsp48E1, Dsp48E2]
  let
    cfg0 = X.defConfig{dspSlice = dsp}
    full = mkParams cfg0 tcCoefficients tcDataWidth tcDataSigned tcCoefWidth tcCoefSigned 0 1
  -- The IP turns Hilbert transformers into a filter with two outputs
  structure0 <- Gen.element [Inferred, Inferred, Inferred, NonSymmetric]
  let
    tcConfig
      | structure full == HilbertStructure = cfg0{coefficientStructure = NegativeSymmetric}
      | otherwise = cfg0{coefficientStructure = structure0}
    w = fullWidth full
  tcOutputWidth <-
    if tcRounding == FullPrecision
    then pure w
    else Gen.frequency
      [ (4, Gen.int (Range.constant 1 (w - 1)))
      , (1, Gen.int (Range.constant (max 1 (w - 3)) (w - 1)))
      ]
  pure TestConfig{..}

-- | Coefficients: random, symmetric, negative symmetric, half-band patterns,
-- sparse, small, and sets of which the absolute values add up to a power of
-- two (the full precision output then just fits)
genCoefficients :: Int -> Int -> Bool -> Gen [Integer]
genCoefficients n w signed = do
  kind <- Gen.int (Range.constant 0 7)
  cs <- case kind of
    0 -> Gen.list (Range.singleton n) value
    1 -> symmetric id
    2 -> symmetric negate
    3 -> halfBand
    4 -> Gen.list (Range.singleton n) (Gen.frequency [(3, pure 0), (1, value)])
    5 -> Gen.list (Range.singleton n) (genIntegerIn (max lo (-3)) (min hi 3))
    6 -> powerOfTwoSum
    _ -> Gen.list (Range.singleton n) (Gen.element [lo, hi, 0, 1])
  -- At least one coefficient must be non-zero
  pure (if all (== 0) cs then 1 : drop 1 cs else cs)
 where
  (lo, hi) = range signed w
  value = genIntegerIn lo hi
  symmetric f = do
    -- Avoid negating the most negative value
    h <- Gen.list (Range.singleton ((n + 1) `div` 2)) (genIntegerIn (if signed then lo + 1 else lo) hi)
    let
      mirror = map f (reverse (take (n `div` 2) h))
      cs = take (n `div` 2) h <> (if odd n then [if f 1 == 1 then last h else 0] else []) <> mirror
    pure (if signed || f 1 == 1 then cs else map abs cs)
  halfBand = do
    let m = n `div` 2
    cs <- forM [0 .. n - 1] $ \i ->
      if i /= m && even (i - m) then pure 0 else value
    pure [ if i <= m then cs !! i else cs !! (n - 1 - i) | i <- [0 .. n - 1] ]
  powerOfTwoSum = do
    let p = 1 `shiftL` (if signed then w - 2 else w - 1) :: Integer
    sign <- if signed then Gen.element [1, -1] else pure 1
    pure (map (* sign) (take n (p : p : repeat 0)))

-- | Samples, biased towards extremes and runs of extremes
genSamples :: TestConfig -> Gen [Integer]
genSamples TestConfig{..} = concat <$> Gen.list (Range.singleton (batchSize `div` 4)) chunk
 where
  (lo, hi) = range tcDataSigned tcDataWidth
  chunk = Gen.frequency
    [ (6, Gen.list (Range.singleton 4) (genIntegerIn lo hi))
    , (1, pure (replicate 4 lo))
    , (1, pure (replicate 4 hi))
    , (1, pure [lo, hi, lo, hi])
    ]

tests :: TestTree
tests =
  testGroup "Xilinx FIR Compiler"
    [ withResource loadFir unloadFir $ \getModel ->
        testGroup "vs. C model"
          [ testPropertyNamed "firWith" (fromString "prop_cmodel") (prop_cmodel getModel)
          , testPropertyNamed "firValidWith" (fromString "prop_cmodelValid") (prop_cmodelValid getModel)
          ]
    ]
