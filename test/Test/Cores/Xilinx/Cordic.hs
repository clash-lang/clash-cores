{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- |
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the simulation models in "Clash.Cores.Xilinx.Cordic" against the
  bit-accurate C model of the Xilinx CORDIC LogiCORE IP v6.0. The tests run
  the functions from "Clash.Cores.Xilinx.Cordic.Explicit" in Clash simulation,
  on batches of random inputs, and compare their outputs with the outputs of
  the C model for the same configuration of the IP. They cover vector rotation
  and translation, sine and cosine, and arctangent, with a range of input and
  output widths, all rounding modes, phase formats, coarse rotation,
  compensation scaling, and explicit numbers of iterations and internal
  precisions.

  = Running the tests

  These tests are only part of the unit tests if the Cabal flag
  @xilinx-cordic-cmodel@ is enabled. It is disabled by default, and enabled on
  CI. Xilinx's C model cannot be redistributed, so the tests locate it at run
  time:

  1. If @CLASH_CORES_XILINX_CORDIC_CMODEL@ is set, it should point at either
     @cordic_v6_0_bitacc_cmodel_lin64.zip@ (Vivado ships it in
     @data\/ip\/xilinx\/cordic_v6_0\/cmodel@) or a directory holding its
     extracted contents.

  2. Otherwise, if @XILINX_VIVADO@ is set (by Vivado's @settings64.sh@), the
     zip file is taken from that installation.

  If neither is set, or the C model is not where they point, the tests fail.

  Each property runs batches of 'batchSize' inputs. The number of batches can
  be set with @--hedgehog-tests@; @--hedgehog-tests 2000@ runs a million cases
  per configuration.

  The C model's shared library uses GMP, which is also linked into Haskell
  programs. It is loaded with @RTLD_DEEPBIND@, so that it uses its own copy of
  GMP (which ships with the C model) instead of the one in the test program.
-}
module Test.Cores.Xilinx.Cordic (tests) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, bracket, catch, onException, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits (shiftL, (.|.))
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CDouble (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
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

import Clash.Explicit.Prelude (KnownNat, SFixed, System, natToNum)
import qualified Clash.Explicit.Prelude as C
import Clash.Signal.Delayed (DSignal)
import qualified Clash.Signal.Delayed as D

import Clash.Cores.Xilinx.Cordic.Explicit (Config (..), RoundMode (..), PhaseFormat (..), CompensationScaling (..))
import qualified Clash.Cores.Xilinx.Cordic.Explicit as X
import Clash.Cores.Xilinx.Cordic.Internal (CordicFunction (..), Params (..), latency, mkParams)

import Hedgehog (Gen, Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

------------------------------------------------------------------------------
-- Locating and loading Xilinx C models
--
-- Nothing in this section is specific to the CORDIC IP. It can be shared with
-- the tests of other IP cores with a C model, once there are more of them.
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
-- The CORDIC C model
------------------------------------------------------------------------------

cordicLocation :: CModelLocation
cordicLocation = CModelLocation
  { envVar = "CLASH_CORES_XILINX_CORDIC_CMODEL"
  , zipInVivado = "data/ip/xilinx/cordic_v6_0/cmodel/cordic_v6_0_bitacc_cmodel_lin64.zip"
  }

-- | @xip_array_real*@ or @xip_array_complex*@
type XipArray = Ptr ()

-- | @xip_cordic_v6_0*@
type XipCordic = Ptr ()

type ArrayCreate = IO XipArray
type ArrayReserve = XipArray -> CSize -> IO CInt
type ArrayDestroy = XipArray -> IO XipArray
-- | @xip_cordic_v6_0_create(config, handler, handle)@
type Create = Ptr () -> FunPtr () -> Ptr () -> IO XipCordic
type Destroy = XipCordic -> IO CInt
-- | All execution functions take the model, two or three arrays and the
-- number of samples.
type Execute2 = XipCordic -> XipArray -> XipArray -> CSize -> IO CInt
type Execute3 = XipCordic -> XipArray -> XipArray -> XipArray -> CSize -> IO CInt

foreign import ccall "dynamic" mkArrayCreate :: FunPtr ArrayCreate -> ArrayCreate
foreign import ccall "dynamic" mkArrayReserve :: FunPtr ArrayReserve -> ArrayReserve
foreign import ccall "dynamic" mkArrayDestroy :: FunPtr ArrayDestroy -> ArrayDestroy
foreign import ccall "dynamic" mkCreate :: FunPtr Create -> Create
foreign import ccall "dynamic" mkDestroy :: FunPtr Destroy -> Destroy
foreign import ccall "dynamic" mkExecute2 :: FunPtr Execute2 -> Execute2
foreign import ccall "dynamic" mkExecute3 :: FunPtr Execute3 -> Execute3

-- | Functions for one kind of array (real or complex)
data ArrayFuns = ArrayFuns
  { arrCreate :: ArrayCreate
  , arrReserveData :: ArrayReserve
  , arrReserveDim :: ArrayReserve
  , arrDestroy :: ArrayDestroy
  }

data CordicCModel = CordicCModel
  { loaded :: LoadedCModel
  , realArray :: ArrayFuns
  , complexArray :: ArrayFuns
  , create :: Create
  , destroy :: Destroy
  , rotateFun :: Execute3
  , translateFun :: Execute3
  , sinCosFun :: Execute2
  , arcTanFun :: Execute2
  }

loadCordic :: IO CordicCModel
loadCordic = do
  loaded@LoadedCModel{libraries} <-
    loadCModel cordicLocation ["libgmp.so.11", "libIp_cordic_v6_0_bitacc_cmodel.so"]
  lib <- case libraries of
    l : _ -> pure l
    [] -> fail "No libraries loaded"
  let
    fun :: (FunPtr a -> a) -> String -> IO a
    fun mk name = mk <$> lookupFunction lib ("xip_cordic_v6_0_" <> name)
    arrayFuns kind =
      ArrayFuns
        <$> fun mkArrayCreate ("xip_array_" <> kind <> "_create")
        <*> fun mkArrayReserve ("xip_array_" <> kind <> "_reserve_data")
        <*> fun mkArrayReserve ("xip_array_" <> kind <> "_reserve_dim")
        <*> fun mkArrayDestroy ("xip_array_" <> kind <> "_destroy")
  CordicCModel loaded
    <$> arrayFuns "real"
    <*> arrayFuns "complex"
    <*> fun mkCreate "create"
    <*> fun mkDestroy "destroy"
    <*> fun mkExecute3 "rotate"
    <*> fun mkExecute3 "translate"
    <*> fun mkExecute2 "sin_cos"
    <*> fun mkExecute2 "atan"

unloadCordic :: CordicCModel -> IO ()
unloadCordic = unloadCModel . loaded

-- | Byte offsets in @xip_array_real@ and @xip_array_complex@
offData, offDataSize, offDim, offDimSize :: Int
offData = 0
offDataSize = 8
offDim = 24
offDimSize = 32

-- | Create a one-dimensional array of @n@ elements of @k@ doubles each, run an
-- action with it and destroy it.
withArray :: ArrayFuns -> Int -> (XipArray -> IO a) -> IO a
withArray ArrayFuns{..} n act = bracket acquire arrDestroy act
 where
  acquire = do
    arr <- arrCreate
    when (arr == nullPtr) $ fail "Could not create C model array"
    checkStatus "reserve_dim" =<< arrReserveDim arr 1
    checkStatus "reserve_data" =<< arrReserveData arr (fromIntegral (max 1 n))
    pokeByteOff arr offDimSize (1 :: CSize)
    dim <- peekByteOff arr offDim :: IO (Ptr CSize)
    pokeByteOff dim 0 (fromIntegral n :: CSize)
    pokeByteOff arr offDataSize (fromIntegral n :: CSize)
    pure arr

checkStatus :: String -> CInt -> IO ()
checkStatus what status =
  unless (status == 0) $ fail ("C model call failed: " <> what)

-- | Write raw integer values to an array, as doubles
writeArray :: XipArray -> [Integer] -> IO ()
writeArray arr vals = do
  dat <- peekByteOff arr offData :: IO (Ptr CDouble)
  pokeArray dat (map fromIntegral vals)

-- | Read @n@ raw integer values from an array
readArray :: XipArray -> Int -> IO [Integer]
readArray arr n = do
  dat <- peekByteOff arr offData :: IO (Ptr CDouble)
  map toIntegerExact <$> peekArray n dat
 where
  toIntegerExact :: CDouble -> Integer
  toIntegerExact d =
    let i = truncate d :: Integer
    in if fromIntegral i == d then i else error ("C model returned a non-integer: " <> show d)

-- | Inputs of the IP, as raw integers. Unused inputs are zero.
data Inputs = Inputs { inX :: Integer, inY :: Integer, inPhase :: Integer }
  deriving (Eq, Show)

-- | Outputs of the IP, as raw integers. Unused outputs are zero.
data Outputs = Outputs { outX :: Integer, outY :: Integer, outPhase :: Integer }
  deriving (Eq, Show)

-- | A configuration of the IP under test
data TestConfig = TestConfig
  { tcFunction :: CordicFunction
  , tcInputWidth :: Int
  , tcOutputWidth :: Int
  , tcConfig :: Config
  } deriving (Show)

tcParams :: TestConfig -> Params
tcParams TestConfig{..} = mkParams tcFunction tcConfig tcInputWidth tcOutputWidth

-- | Run the C model on a batch of inputs
runCModel :: CordicCModel -> TestConfig -> [Inputs] -> IO [Outputs]
runCModel model@CordicCModel{loaded = LoadedCModel{lock}} tc@TestConfig{..} inputs =
  withMVar lock $ \() ->
  allocaBytes 56 $ \cfgPtr ->
  withCString "clash-cores" $ \name -> do
    pokeConfig cfgPtr
    pokeByteOff cfgPtr 0 name
    bracket (create model cfgPtr nullFunPtr nullPtr) (destroy model) $ \h -> do
      when (h == nullPtr) $ fail ("Could not create C model for " <> show tc)
      case tcFunction of
        Rotate ->
          withCartesian $ \cart -> withPhase $ \ph -> withComplex $ \out -> do
            checkStatus "rotate" =<< rotateFun model h cart ph out n
            xys <- readPairs out
            pure [Outputs x y 0 | (x, y) <- xys]
        Translate ->
          withCartesian $ \cart -> withReal $ \mag -> withReal $ \ph -> do
            checkStatus "translate" =<< translateFun model h cart mag ph n
            ms <- readArray mag nInt
            ps <- readArray ph nInt
            pure (zipWith (\m p -> Outputs m 0 p) ms ps)
        SinCos -> withPhase $ \ph -> withComplex $ \out -> do
          checkStatus "sin_cos" =<< sinCosFun model h ph out n
          map (\(x, y) -> Outputs x y 0) <$> readPairs out
        ArcTan -> withCartesian $ \cart -> withReal $ \ph -> do
          checkStatus "atan" =<< arcTanFun model h cart ph n
          map (Outputs 0 0) <$> readArray ph nInt
 where
  nInt = length inputs
  n = fromIntegral nInt
  withReal = withArray (realArray model) nInt
  withComplex = withArray (complexArray model) nInt
  withCartesian act = withComplex $ \arr -> do
    writeArray arr (concatMap (\i -> [inX i, inY i]) inputs)
    act arr
  withPhase act = withReal $ \arr -> do
    writeArray arr (map inPhase inputs)
    act arr
  readPairs arr = pairs <$> readArray arr (2 * nInt)
  pairs (a : b : rest) = (a, b) : pairs rest
  pairs _ = []

  pokeConfig :: Ptr () -> IO ()
  pokeConfig p = do
    let
      fields :: [Word32]
      fields =
        [ functionCode tcFunction
        -- The IP disables coarse rotation for functions that do not support
        -- it, but the C model does not
        , if coarse (tcParams tc) then 1 else 0
        , 0 -- Data format, only for the square root
        , case phaseFormat tcConfig of
            Radians -> 0
            ScaledRadians -> 1
        , fromIntegral tcInputWidth
        , fromIntegral tcOutputWidth
        , maybe 0 fromIntegral (iterations tcConfig)
        , maybe 0 fromIntegral (precision tcConfig)
        , case roundMode tcConfig of
            Truncate -> 0
            RoundPosInf -> 1
            RoundPosNegInf -> 2
            RoundNearestEven -> 3
        , case compensationScaling tcConfig of
            NoScaleCompensation -> 0
            LutBased -> 1
            BramBased -> 2
        ]
    forM_ (zip [8, 12 ..] fields) $ \(off, v) -> pokeByteOff p off v
    pokeByteOff p 48 (0 :: CInt)

  functionCode = \case
    Rotate -> 0
    Translate -> 1
    SinCos -> 2
    ArcTan -> 3

------------------------------------------------------------------------------
-- Running the Clash functions
------------------------------------------------------------------------------

withNat :: Int -> (forall n. KnownNat n => Proxy n -> r) -> r
withNat i f
  | i < 0 = error ("withNat: negative " <> show i)
  | otherwise = case someNatVal (fromIntegral i) of SomeNat p -> f p

toSF :: forall int frac. (KnownNat int, KnownNat frac) => Integer -> SFixed int frac
toSF = C.sf (C.SNat @frac) . fromInteger

fromSF :: forall int frac. (KnownNat int, KnownNat frac) => SFixed int frac -> Integer
fromSF = toInteger . C.unSF

-- | Run a Clash function on a list of inputs and return its outputs, skipping
-- the first @d@ cycles.
simulate ::
  forall d a b.
  (KnownNat d, C.NFDataX a) =>
  (DSignal System 0 a -> DSignal System d b) ->
  [a] ->
  [b]
simulate f xs =
  take (length xs) $ drop (natToNum @d) $
    C.sample_lazy (D.toSignal (f (D.fromSignal (C.fromList (xs <> repeat (last xs))))))

clk :: C.Clock System
clk = C.clockGen

en :: C.Enable System
en = C.enableGen

-- | Run the Clash function for a configuration on a batch of inputs
runClash :: TestConfig -> [Inputs] -> [Outputs]
runClash tc@TestConfig{..} inputs =
  withNat (latency (tcParams tc)) $ \(Proxy :: Proxy d) ->
  case tcFunction of
    Rotate ->
      withNat (tcInputWidth - 3) $ \(Proxy :: Proxy fIn) ->
      withNat (tcOutputWidth - 3) $ \(Proxy :: Proxy fOut) ->
        let
          f = \inp -> X.rotateWith @d @System @0 @fIn @fOut tcConfig clk en
                        (fmap fst inp) (fmap snd inp)
          ins = [ ((toSF inX, toSF inY), toSF inPhase) | Inputs{..} <- inputs ]
        in [ Outputs (fromSF x) (fromSF y) 0 | (x, y) <- simulate f ins ]
    Translate ->
      withNat (tcInputWidth - 2) $ \(Proxy :: Proxy fIn) ->
      withNat (tcOutputWidth - 3) $ \(Proxy :: Proxy fOut) ->
        let
          f = X.translateWith @d @System @0 @fIn @fOut tcConfig clk en
          ins = [ (toSF inX, toSF inY) | Inputs{..} <- inputs ]
        in [ Outputs (fromSF m) 0 (fromSF p) | (m, p) <- simulate f ins ]
    SinCos ->
      withNat (tcInputWidth - 3) $ \(Proxy :: Proxy fIn) ->
      withNat (tcOutputWidth - 2) $ \(Proxy :: Proxy fOut) ->
        let f = X.sinCosWith @d @System @0 @fIn @fOut tcConfig clk en
        in [ Outputs (fromSF x) (fromSF y) 0 | (x, y) <- simulate f (map (toSF . inPhase) inputs) ]
    ArcTan ->
      withNat (tcInputWidth - 2) $ \(Proxy :: Proxy fIn) ->
      withNat (tcOutputWidth - 3) $ \(Proxy :: Proxy fOut) ->
        let
          f = X.arcTanWith @d @System @0 @fIn @fOut tcConfig clk en
          ins = [ (toSF inX, toSF inY) | Inputs{..} <- inputs ]
        in [ Outputs 0 0 (fromSF p) | p <- simulate f ins ]

------------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------------

-- | Number of inputs per Hedgehog test case
batchSize :: Int
batchSize = 500

-- | Number of batches per property, unless overridden by @--hedgehog-tests@
nTests :: H.TestLimit
nTests = 40

prop_cmodel :: IO CordicCModel -> TestConfig -> Property
prop_cmodel getModel tc = H.withTests nTests $ H.property $ do
  model <- H.evalIO getModel
  inputs <- H.forAllWith showBatch (Gen.list (Range.singleton batchSize) (genInputs tc))
  cOut <- H.evalIO (runCModel model tc inputs)
  let
    hOut = runClash tc inputs
    mismatches =
      [ (i, inp, c, h) | (i, inp, c, h) <- zip4 [0 :: Int ..] inputs cOut hOut, c /= h ]
  H.annotateShow (tcParams tc)
  case mismatches of
    [] -> pure ()
    ((i, inp, c, h) : _) -> do
      H.footnote ("first mismatch at index " <> show i <> " of " <> show (length mismatches))
      H.footnote ("input: " <> show inp)
      c === h
 where
  showBatch xs = show (length xs) <> " inputs"
  zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
  zip4 _ _ _ _ = []

------------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------------

-- | Integer in a range, biased towards the ends and the middle
genIntegerIn :: Integer -> Integer -> Gen Integer
genIntegerIn lo hi =
  Gen.frequency
    [ (8, Gen.integral (Range.constant lo hi))
    , (1, Gen.integral (Range.constant lo (min hi (lo + 16))))
    , (1, Gen.integral (Range.constant (max lo (hi - 16)) hi))
    , (1, Gen.integral (Range.constant (max lo (-16)) (min hi 16)))
    ]

-- | A value close to one of the given points, within a range
genNear :: Integer -> Integer -> [Integer] -> Gen Integer
genNear lo hi points = do
  p <- Gen.element points
  d <- Gen.integral (Range.constant (-8) 8)
  pure (max lo (min hi (p + d)))

-- | @floor(c * 2^k)@
scaleFloor :: Double -> Int -> Integer
scaleFloor c k = floor (c * 2 ^^ k)

-- | Phase with @k@ fractional bits, in the range the IP supports
genPhase :: TestConfig -> Int -> Gen Integer
genPhase TestConfig{tcConfig} k =
  Gen.frequency [(6, genIntegerIn lo hi), (2, genNear lo hi edges)]
 where
  unit
    | phaseFormat tcConfig == ScaledRadians = 1
    | otherwise = pi
  full
    | coarseRotation tcConfig = unit
    | otherwise = unit / 4
  hi = scaleFloor full k
  lo = negate hi
  -- Quadrant boundaries, 0 and +-1
  edges = 0 : concat [ [scaleFloor (q * unit / 4) k, negate (scaleFloor (q * unit / 4) k)] | q <- [1, 2, 3] ]
        <> [1 `shiftL` k, negate (1 `shiftL` k), hi, lo]

-- | Inputs of the IP, in the ranges described in PG105
genInputs :: TestConfig -> Gen Inputs
genInputs tc@TestConfig{..} = case tcFunction of
  Rotate -> Inputs <$> genXY <*> genXY <*> genPhase tc (tcInputWidth - 3)
  SinCos -> Inputs 0 0 <$> genPhase tc (tcInputWidth - 3)
  Translate -> genVector
  ArcTan -> genVector
 where
  one = 1 `shiftL` (tcInputWidth - 2)
  genXY = Gen.frequency
    [ (6, genIntegerIn (negate one) one)
    , (1, Gen.element [0, one, negate one, 1, -1])
    ]
  genVector
    | coarseRotation tcConfig = Inputs <$> genXY <*> genXY <*> pure 0
    | otherwise = do
        -- Angle between -45 and 45 degrees
        x <- genIntegerIn 0 one
        y <- genIntegerIn (negate x) x
        pure (Inputs x y 0)

------------------------------------------------------------------------------
-- Configurations under test
------------------------------------------------------------------------------

testConfigs :: [TestConfig]
testConfigs =
  -- Default configuration, several widths
  [ TestConfig f wIn wOut X.defConfig
  | f <- [Rotate, Translate, SinCos, ArcTan]
  , (wIn, wOut) <- widths
  ]
  <>
  -- Rounding, phase formats and coarse rotation
  [ TestConfig f 16 16
      X.defConfig{roundMode = rnd, phaseFormat = pf, coarseRotation = cr}
  | f <- [Rotate, Translate, SinCos, ArcTan]
  , rnd <- roundModes
  , pf <- [Radians, ScaledRadians]
  , cr <- [True, False]
  ]
  <>
  -- Compensation scaling
  [ TestConfig f wIn wOut
      X.defConfig{compensationScaling = cs, roundMode = rnd}
  | f <- [Rotate, Translate]
  , cs <- [LutBased, BramBased]
  , rnd <- [Truncate, RoundNearestEven]
  , (wIn, wOut) <- [(16, 16), (24, 12), (8, 48)]
  ]
  <>
  -- Explicit iterations and precision
  [ TestConfig f 16 16
      X.defConfig{iterations = it, precision = pr, roundMode = RoundPosNegInf}
  | f <- [Rotate, Translate, SinCos, ArcTan]
  , (it, pr) <- [(Just 8, Nothing), (Just 24, Just 30), (Nothing, Just 40)]
  ]
 where
  roundModes = [Truncate, RoundPosInf, RoundPosNegInf, RoundNearestEven]
  widths = [(8, 8), (16, 16), (12, 20), (20, 12), (32, 32), (48, 48), (48, 8), (8, 48)]

testName :: TestConfig -> String
testName TestConfig{..} =
  show tcFunction
    <> " " <> show tcInputWidth <> "->" <> show tcOutputWidth
    <> " " <> show tcConfig

-- | The type families for the latencies with the default configuration agree
-- with 'latency'. This does not need the C model.
prop_defaultDelays :: Property
prop_defaultDelays = H.withTests 1 $ H.property $
  forM_ [8 .. 48] $ \w -> do
    let lat f = latency (mkParams f X.defConfig 16 w)
    H.annotateShow w
    withNat (w - 3) $ \(Proxy :: Proxy f) -> do
      natToNum @(X.RotateDefDelay f) === lat Rotate
      natToNum @(X.TranslateDefDelay f) === lat Translate
      natToNum @(X.ArcTanDefDelay f) === lat ArcTan
    withNat (w - 2) $ \(Proxy :: Proxy f) ->
      natToNum @(X.SinCosDefDelay f) === lat SinCos

tests :: TestTree
tests =
  testGroup "Xilinx CORDIC"
    [ testPropertyNamed "default delays" (fromString "prop_defaultDelays") prop_defaultDelays
    , withResource loadCordic unloadCordic $ \getModel ->
        testGroup "vs. C model"
          [ testPropertyNamed (testName tc) (fromString ("prop_cordic_" <> show i)) (prop_cmodel getModel tc)
          | (i, tc) <- zip [0 :: Int ..] (nubOn testName testConfigs)
          ]
    ]
 where
  nubOn f = go []
   where
    go _ [] = []
    go seen (x : xs)
      | f x `elem` seen = go seen xs
      | otherwise = x : go (f x : seen) xs
