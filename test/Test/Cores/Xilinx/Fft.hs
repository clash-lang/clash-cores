{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the simulation models in "Clash.Cores.Xilinx.Fft" against the
  bit-accurate C model of the Xilinx Fast Fourier Transform LogiCORE IP v9.1,
  for its /Pipelined, Streaming I\/O/ architecture.

  There are two kinds of tests:

  1. Random configurations: each test case picks a transform length, data and
     phase factor widths, scaling, rounding, direction and scaling schedule at
     random, and compares 'fftFrameInteger' with the C model on a batch of
     frames.

  2. Fixed configurations: these run 'Fft.fft' itself in Clash simulation, on
     frames that follow each other without gaps, and compare its output with
     the C model.

  The frames are random samples of several amplitudes (small amplitudes hit
  rounding ties often), impulses, full-scale tones, constant frames and frames
  that alternate between the extremes.

  The C model is not bit accurate for frames that overflow (PG109, chapter 5),
  and the IP's output is unusable then. For those frames, we only check that
  the models agree that the frame overflowed.

  = Running the tests

  These tests are only part of the unit tests if the Cabal flag
  @xilinx-fft-cmodel@ is enabled. It is disabled by default, and enabled on CI.
  Xilinx's C model cannot be redistributed, so the tests locate it at run time:

  1. If @CLASH_CORES_XILINX_FFT_CMODEL@ is set, it should point at either
     @xfft_v9_1_bitacc_cmodel_lin64.zip@ (Vivado ships it in
     @data\/ip\/xilinx\/xfft_v9_1\/cmodel@) or a directory holding its
     extracted contents.

  2. Otherwise, if @XILINX_VIVADO@ is set (by Vivado's @settings64.sh@), the
     zip file is taken from that installation.

  If neither is set, or the C model is not where they point, the tests fail.

  The C model passes structures by value, which Haskell's FFI cannot do, so we
  call it through the C functions in @test/cbits/xfft_cmodel.c@. Use
  @--hedgehog-tests@ to change the number of test cases from 'nTests'.
-}
module Test.Cores.Xilinx.Fft (tests) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, bracket, catch, onException, try)
import Control.Monad (forM, unless, void, when)
import Data.Bits ((.|.))
import Data.Complex (Complex (..))
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (peek)
import GHC.TypeLits (KnownNat, natVal, type (+), type (<=), type (^))
import System.Directory
  ( createDirectory, doesDirectoryExist, doesFileExist, makeAbsolute
  , removeDirectoryRecursive )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (arch, os)
import System.Process (readProcessWithExitCode)

import Clash.Explicit.Prelude (System)
import qualified Clash.Explicit.Prelude as C
import qualified Clash.Sized.Vector as Vec

import Clash.Cores.Xilinx.Fft.Explicit
  ( Config (..), Direction (..), FftOutput (..), FrameConfig (..), KnownFft
  , OutputOrdering (..), Rounding (..), Scaling (..), KnownScaling (..) )
import qualified Clash.Cores.Xilinx.Fft.Explicit as Fft
import Clash.Cores.Xilinx.Fft.Internal
  (ArithScaling (..), FrameResult (..), fftFrameInteger)

import Hedgehog (Gen)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

------------------------------------------------------------------------------
-- Configuration of the tests
------------------------------------------------------------------------------

-- | Name of the environment variable pointing at the C model
cmodelEnvVar :: String
cmodelEnvVar = "CLASH_CORES_XILINX_FFT_CMODEL"

-- | The C model in a Vivado installation
fftCModel :: CModelLocation
fftCModel =
  CModelLocation
    { cmlEnvVar = cmodelEnvVar
    , cmlZipInVivado = "data/ip/xilinx/xfft_v9_1/cmodel/xfft_v9_1_bitacc_cmodel_lin64.zip"
    , cmlTestModule = "test/Test/Cores/Xilinx/Fft.hs"
    }

-- | Number of samples per test case: a test case transforms a batch of
-- @max 1 (batchSamples / N)@ frames
batchSamples :: Int
batchSamples = 4_096

-- | Number of test cases per property, unless overridden by @--hedgehog-tests@
nTests :: H.TestLimit
nTests = 100

tests :: TestTree
tests =
  withResource (loadXfft fftCModel) unloadXfft $ \getCModel ->
    testGroup "Xilinx FFT vs. C model"
      [ testPropertyNamed "random configurations" "prop_randomConfigurations" $
          H.withShrinks 0 $ H.withTests (5 * nTests) $ H.property $ do
            cfg <- H.forAll genArithConfig
            cmodel <- H.evalIO getCModel
            frames <- H.forAllWith (const "<frames>") (genFrames cfg)
            failure <- H.evalIO (compareArith cmodel cfg frames)
            H.classify "scaled" (isScaled (acScaling cfg))
            mapM_ (\msg -> H.footnote msg >> H.failure) failure
      , testGroup "fft"
          [ fftTest @'Scaled @3 @16 getCModel Fft.defConfig
          , fftTest @'Scaled @4 @16 getCModel Fft.defConfig{rounding = ConvergentRounding}
          , fftTest @'Scaled @6 @12 getCModel Fft.defConfig{phaseFactorWidth = 20, outputOrdering = NaturalOrder}
          , fftTest @'Scaled @9 @18 getCModel Fft.defConfig{phaseFactorWidth = 18, rounding = ConvergentRounding}
          , fftTest @'Unscaled @5 @16 getCModel Fft.defConfig
          , fftTest @'Unscaled @7 @24 getCModel Fft.defConfig{phaseFactorWidth = 12, rounding = ConvergentRounding, outputOrdering = NaturalOrder}
          , fftTest @'BlockFloatingPoint @6 @16 getCModel Fft.defConfig
          , fftTest @'BlockFloatingPoint @8 @10 getCModel Fft.defConfig{phaseFactorWidth = 25, rounding = ConvergentRounding}
          , fftTest @'Scaled @10 @34 getCModel Fft.defConfig{phaseFactorWidth = 34}
          , fftTest @'Unscaled @12 @8 getCModel Fft.defConfig{phaseFactorWidth = 8}
          ]
      ]

------------------------------------------------------------------------------
-- Generic: locating, extracting and loading a Xilinx C model
--
-- Nothing in this section is specific to the FFT. The C models
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
-- The FFT C model
------------------------------------------------------------------------------

-- | @clash_cores_xfft_create@ from @test/cbits/xfft_cmodel.c@
foreign import ccall unsafe "clash_cores_xfft_create"
  c_xfftCreate :: FunPtr () -> Ptr CInt -> IO (Ptr ())

-- | @clash_cores_xfft_simulate@ from @test/cbits/xfft_cmodel.c@
foreign import ccall unsafe "clash_cores_xfft_simulate"
  c_xfftSimulate ::
    FunPtr () -> Ptr () -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> CInt -> CInt ->
    Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt

type Destroy = Ptr () -> IO ()
foreign import ccall "dynamic" mkDestroy :: FunPtr Destroy -> Destroy

-- | The functions of the C model
data Xfft = Xfft
  { xfLoaded :: LoadedCModel
  , xfLock :: MVar ()
  , xfCreate :: FunPtr ()
  , xfSimulate :: FunPtr ()
  , xfDestroy :: Destroy
  }

loadXfft :: CModelLocation -> IO Xfft
loadXfft loc = do
  xfLoaded <- loadCModel loc ["libgmp.so.11", "libIp_xfft_v9_1_bitacc_cmodel.so"]
  lib <- case lcmLibraries xfLoaded of
    l : _ -> pure l
    [] -> fail "No libraries loaded"
  xfLock <- newMVar ()
  xfCreate <- lookupFunction lib "xilinx_ip_xfft_v9_1_create_state"
  xfSimulate <- lookupFunction lib "xilinx_ip_xfft_v9_1_bitacc_simulate"
  xfDestroy <- mkDestroy <$> lookupFunction lib "xilinx_ip_xfft_v9_1_destroy_state"
  pure Xfft{..}

unloadXfft :: Xfft -> IO ()
unloadXfft Xfft{xfLoaded} = unloadCModel xfLoaded

-- | Configuration of the IP, at the term level
data ArithConfig = ArithConfig
  { acNfft :: Int
  , acInputWidth :: Int
  , acPhaseFactorWidth :: Int
  , acScaling :: Scaling'
  , acRounding :: Rounding
  }
  deriving (Show)

-- | 'Scaling' at the term level
data Scaling' = Unscaled' | Scaled' | BlockFloatingPoint'
  deriving (Show, Eq)

isScaled :: Scaling' -> Bool
isScaled = (== Scaled')

-- | A frame: its direction, its scaling schedule, and its samples
data Frame = Frame
  { fDirection :: Direction
  , fSchedule :: [Int]
  , fSamples :: [(Integer, Integer)]
  }

-- | Run the C model of the /Pipelined, Streaming I\/O/ architecture on a list
-- of frames
runXfft :: Xfft -> ArithConfig -> [Frame] -> IO [FrameResult]
runXfft Xfft{..} cfg@ArithConfig{..} frames =
  withMVar xfLock $ \() ->
  withArray generics $ \gens ->
  bracket
    (c_xfftCreate xfCreate gens)
    (\st -> when (st /= nullPtr) (xfDestroy st))
    $ \st -> do
      when (st == nullPtr) $ fail ("Could not create the C model for " <> show cfg)
      forM frames $ \Frame{..} ->
        let schedule = if null fSchedule then replicate groups 0 else fSchedule in
        withArray (map (toDouble . fst) fSamples) $ \xnRe ->
        withArray (map (toDouble . snd) fSamples) $ \xnIm ->
        withArray (map fromIntegral schedule) $ \sch ->
        allocaArray n $ \xkRe ->
        allocaArray n $ \xkIm ->
        alloca $ \blkExp ->
        alloca $ \overflow -> do
          status <-
            c_xfftSimulate xfSimulate st (fromIntegral acNfft) xnRe xnIm sch
              (fromIntegral groups) (if fDirection == Forward then 1 else 0)
              xkRe xkIm blkExp overflow
          unless (status == 0) $
            fail ("The C model failed with status " <> show status <> " for " <> show cfg)
          re <- peekArray n xkRe
          im <- peekArray n xkIm
          e <- peek blkExp
          o <- peek overflow
          pure FrameResult
            { frSamples = zip (map fromDouble re) (map fromDouble im)
            , frBlockExponent = fromIntegral e
            , frOverflow = o /= 0
            }
 where
  n = 2 ^ acNfft :: Int
  groups = (acNfft + 1) `div` 2
  generics :: [CInt]
  generics =
    [ fromIntegral acNfft
    , 3 -- Pipelined, Streaming I/O
    , 0 -- fixed transform length
    , 0 -- fixed-point data
    , fromIntegral acInputWidth
    , fromIntegral acPhaseFactorWidth
    , if acScaling == Unscaled' then 0 else 1
    , if acScaling == BlockFloatingPoint' then 1 else 0
    , if acRounding == ConvergentRounding then 1 else 0
    ]
  -- The C model takes values in [-1, 1), with the binary point after the sign
  -- bit, and gives its output with the same binary point
  unit = 2 ^^ (acInputWidth - 1) :: Double
  toDouble v = realToFrac (fromInteger v / unit)
  fromDouble :: CDouble -> Integer
  fromDouble d =
    let v = realToFrac d * unit :: Double
        (i, f) = properFraction v
    in if f == 0 then i else error ("C model output is not an integer: " <> show v)

------------------------------------------------------------------------------
-- Random configurations
------------------------------------------------------------------------------

arithScaling' :: ArithConfig -> Frame -> ArithScaling
arithScaling' ArithConfig{acScaling} Frame{fSchedule} = case acScaling of
  Unscaled' -> ArithUnscaled
  Scaled' -> ArithScaled fSchedule
  BlockFloatingPoint' -> ArithBlockFloatingPoint

-- | Compare 'fftFrameInteger' with the C model on a list of frames. Gives a
-- description of the first mismatch, if any.
compareArith :: Xfft -> ArithConfig -> [Frame] -> IO (Maybe String)
compareArith cmodel cfg@ArithConfig{..} frames = do
  cResults <- runXfft cmodel cfg frames
  let
    haskell f =
      fftFrameInteger acInputWidth acPhaseFactorWidth acRounding
        (arithScaling' cfg f) (fDirection f) (fSamples f)
    mismatch (i, f, h, c) = compareResult cfg (show i) f h c
  pure (firstJust (map mismatch (zip4 [0 :: Int ..] frames (map haskell frames) cResults)))
 where
  zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
  zip4 _ _ _ _ = []

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (\x acc -> maybe acc Just x) Nothing

-- | Compare a result of the Haskell model with the C model's. For a frame that
-- overflowed, compare only the overflow flag.
compareResult :: ArithConfig -> String -> Frame -> FrameResult -> FrameResult -> Maybe String
compareResult cfg name Frame{..} h c
  | frOverflow h /= frOverflow c = Just (describe "overflow flags differ")
  | frOverflow h = Nothing
  | frBlockExponent h /= frBlockExponent c = Just (describe "block exponents differ")
  | otherwise =
      case [ (k, a, b) | (k, a, b) <- zip3 [0 :: Int ..] (frSamples h) (frSamples c), a /= b ] of
        [] -> Nothing
        (k, a, b) : _ ->
          Just (describe ("output " <> show k <> ": Haskell model " <> show a <> ", C model " <> show b))
 where
  describe msg =
    unlines
      [ "Frame " <> name <> " of " <> show cfg <> ": " <> msg
      , "Direction: " <> show fDirection <> ", schedule: " <> show fSchedule
      , "Haskell model: block exponent " <> show (frBlockExponent h) <> ", overflow " <> show (frOverflow h)
      , "C model: block exponent " <> show (frBlockExponent c) <> ", overflow " <> show (frOverflow c)
      , "Input: " <> show fSamples
      ]

genArithConfig :: Gen ArithConfig
genArithConfig = do
  acNfft <- Gen.frequency [(6, Gen.int (Range.constant 3 8)), (3, Gen.int (Range.constant 9 12)), (1, Gen.int (Range.constant 13 16))]
  acInputWidth <- genWidth
  acPhaseFactorWidth <- genWidth
  acScaling <- Gen.element [Unscaled', Scaled', Scaled', BlockFloatingPoint']
  acRounding <- Gen.element [Truncation, ConvergentRounding]
  pure ArithConfig{..}
 where
  genWidth = Gen.frequency [(3, Gen.int (Range.constant 8 34)), (1, Gen.element [8, 16, 18, 24, 25, 32, 34])]

-- | A batch of frames for a configuration
genFrames :: ArithConfig -> Gen [Frame]
genFrames cfg@ArithConfig{acNfft} =
  Gen.list (Range.singleton (max 1 (batchSamples `div` (2 ^ acNfft)))) (genFrame cfg)

genFrame :: ArithConfig -> Gen Frame
genFrame ArithConfig{..} = do
  fDirection <- Gen.element [Forward, Inverse]
  fSchedule <-
    if acScaling /= Scaled'
      then pure []
      else Gen.frequency
        [ (1, pure conservative)
        , (2, mapM (\g -> Gen.int (Range.constant 0 (maxShift g))) [0 .. groups - 1])
        ]
  fSamples <- genSamples acNfft acInputWidth
  pure Frame{..}
 where
  groups = (acNfft + 1) `div` 2
  maxShift g = if odd acNfft && g == groups - 1 then 1 else 3
  -- Never overflows, see PG109
  conservative = [if g == 0 then 3 else maxShift g `min` 2 | g <- [0 .. groups - 1]]

-- | Samples of a frame, of several kinds
genSamples :: Int -> Int -> Gen [(Integer, Integer)]
genSamples nfft bw =
  Gen.frequency
    [ (4, do a <- amplitude; Gen.list (Range.singleton n) ((,) <$> value a <*> value a))
    , (1, do i <- Gen.int (Range.constant 0 (n - 1))
             v <- (,) <$> extreme <*> extreme
             pure [if j == i then v else (0, 0) | j <- [0 .. n - 1]])
    , (1, do k0 <- Gen.int (Range.constant 0 (n - 1))
             phase <- Gen.double (Range.constant 0 1)
             a <- Gen.element [1, 0.5, 0.999, 1 / 3]
             pure [tone a k0 phase j | j <- [0 .. n - 1]])
    , (1, do v <- (,) <$> extreme <*> extreme; pure (replicate n v))
    , (1, do v <- (,) <$> extreme <*> extreme; pure (take n (cycle [v, neg v])))
    , (1, Gen.list (Range.singleton n) ((,) <$> small <*> small))
    ]
 where
  n = 2 ^ nfft :: Int
  lo = negate (2 ^ (bw - 1)) :: Integer
  hi = 2 ^ (bw - 1) - 1 :: Integer
  amplitude = Gen.int (Range.constant 1 (bw - 1))
  value a = Gen.integral (Range.constant (max lo (negate (2 ^ a))) (min hi (2 ^ a - 1)))
  extreme = Gen.element [lo, hi, 0, lo + 1, hi - 1, 1, -1]
  small = Gen.integral (Range.constant (-4) 4)
  neg (a, b) = (clamp (negate a), clamp (negate b))
  clamp v = max lo (min hi v)
  tone :: Double -> Int -> Double -> Int -> (Integer, Integer)
  tone a k0 phase j =
    let theta = 2 * pi * (fromIntegral (k0 * j) / fromIntegral n + phase)
        r = a * fromInteger hi
    in (clamp (round (r * cos theta)), clamp (round (r * sin theta)))

------------------------------------------------------------------------------
-- Fixed configurations, through 'Fft.fft'
------------------------------------------------------------------------------

-- | Scaling options at the term level, and their scaling schedules
class KnownScaling s => TestScaling (s :: Scaling) where
  scaling' :: Scaling'
  toSchedule :: KnownNat nfft => [Int] -> ScaleSchedule s nfft

instance TestScaling 'Unscaled where
  scaling' = Unscaled'
  toSchedule _ = ()

instance TestScaling 'Scaled where
  scaling' = Scaled'
  toSchedule [] = C.repeat 0
  toSchedule sch = Vec.unsafeFromList (map toEnum sch)

instance TestScaling 'BlockFloatingPoint where
  scaling' = BlockFloatingPoint'
  toSchedule _ = ()

natInt :: forall n. KnownNat n => Int
natInt = fromInteger (natVal (Proxy @n))

-- | Test 'Fft.fft' in Clash simulation on frames that follow each other
-- without gaps
fftTest ::
  forall s nfft bw.
  ( TestScaling s
  , KnownFft s nfft bw
  , KnownNat (2 ^ nfft + 20)
  , 7 <= 2 ^ nfft + 20
  , C.ShowX (Fft.ScaleSchedule s nfft)
  , C.NFDataX (Fft.ScaleSchedule s nfft)
  ) =>
  IO Xfft ->
  Config ->
  TestTree
fftTest getCModel cfg@Config{phaseFactorWidth, rounding, outputOrdering} =
  testPropertyNamed name (fromString ("prop_" <> name)) $
    H.withShrinks 0 $ H.withTests (nTests `div` 5) $ H.property $ do
      frames <- H.forAllWith (const "<frames>") (genFrames acfg)
      cmodel <- H.evalIO getCModel
      cResults <- H.evalIO (runXfft cmodel acfg frames)
      let outFrames = runFft frames
      H.assert (length outFrames == length frames)
      mapM_ (\msg -> H.footnote msg >> H.failure) $
        firstJust (zipWith3 (checkFrame acfg) [0 :: Int ..] (zip frames outFrames) cResults)
 where
  nfft = natInt @nfft
  n = 2 ^ nfft :: Int
  acfg =
    ArithConfig
      { acNfft = nfft
      , acInputWidth = natInt @bw
      , acPhaseFactorWidth = phaseFactorWidth
      , acScaling = scaling' @s
      , acRounding = rounding
      }
  name =
    show (scaling' @s) <> ", N = " <> show n <> ", " <> show (natInt @bw) <> "-bit data, "
      <> show phaseFactorWidth <> "-bit phase factors, " <> show rounding <> ", " <> show outputOrdering

  -- Run the model with a reset of three cycles, a pause of three cycles, and
  -- the frames in consecutive cycles. Gives the output, split into frames.
  runFft :: [Frame] -> [[FftOutput s nfft (Fft.OutputWidth s nfft bw)]]
  runFft frames =
    splitFrames (catMaybes (C.sampleN nCycles (snd (Fft.fft @(2 ^ nfft + 20) @s @nfft @bw cfg clk rst frameCfgs input))))
   where
    nCycles = 6 + length frames * n + n + 40
    rst = C.unsafeFromActiveHigh (C.fromList (replicate 3 True <> repeat False))
    input = C.fromList (replicate 6 Nothing <> concatMap (map (Just . toSample) . fSamples) frames <> repeat Nothing)
    frameCfgs =
      C.fromList (replicate 6 dflt <> concatMap (replicate n . toFrameConfig) frames <> repeat dflt)
    dflt = FrameConfig Forward (toSchedule @s @nfft [])
    toFrameConfig Frame{fDirection, fSchedule} = FrameConfig fDirection (toSchedule @s @nfft fSchedule)
    toSample (a, b) = fromInteger a :+ fromInteger b

  clk = C.clockGen @System
  splitFrames xs = case break lastSample xs of
    (a, l : rest) -> (a <> [l]) : splitFrames rest
    _ -> []

  -- Check the order and status of the output samples, and compare their values
  -- with the C model
  checkFrame :: ArithConfig -> Int -> (Frame, [FftOutput s nfft (Fft.OutputWidth s nfft bw)]) -> FrameResult -> Maybe String
  checkFrame ac i (f, outs) c
    | map (fromEnum . index) outs /= expectedOrder = Just ("Wrong output order in frame " <> show i)
    | otherwise = compareResult ac (show i) f h c
   where
    expectedOrder = case outputOrdering of
      NaturalOrder -> [0 .. n - 1]
      BitReversedOrder -> map (bitRev nfft) [0 .. n - 1]
    -- Rebuild a 'FrameResult' from the output samples; the samples of a frame
    -- that overflowed are undefined
    h = FrameResult
      { frSamples =
          if statusOverflow (map status outs) then [] else
          map (\(a :+ b) -> (toInteger a, toInteger b)) (map value (sortOn (fromEnum . index) outs))
      , frBlockExponent = statusBlockExponent (map status outs)
      , frOverflow = statusOverflow (map status outs)
      }

  statusOverflow :: [Fft.FrameStatus s] -> Bool
  statusOverflow sts = case scaling' @s of
    Scaled' -> lastOverflow sts
    _ -> False
  statusBlockExponent :: [Fft.FrameStatus s] -> Int
  statusBlockExponent sts = case scaling' @s of
    BlockFloatingPoint' -> blockExp sts
    _ -> 0

  lastOverflow = statusBits @s (\b -> b /= 0)
  blockExp = statusBits @s fromInteger

-- | Interpret the statuses of a frame through their bits: the last one's for
-- the overflow flag, and the first one's for the block exponent (they should
-- all be equal)
statusBits :: forall s a. KnownScaling s => (Integer -> a) -> [Fft.FrameStatus s] -> a
statusBits f sts = f (toInteger (C.pack (last sts)))

bitRev :: Int -> Int -> Int
bitRev m i = foldl (\acc b -> 2 * acc + (if C.testBit i b then 1 else 0)) 0 [0 .. m - 1]
