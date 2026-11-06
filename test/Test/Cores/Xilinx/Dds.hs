{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
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

  Tests of the simulation models in "Clash.Cores.Xilinx.Dds" against the
  bit-accurate C model of the Xilinx DDS Compiler LogiCORE IP v6.0. The tests
  run the functions from "Clash.Cores.Xilinx.Dds.Explicit" in Clash simulation,
  on batches of random inputs, and compare their outputs with the outputs of
  the C model for the same configuration of the IP. They cover all three
  configurations of the IP (complete DDS, phase generator only, SIN\/COS
  lookup table only), all ways to set the phase increment and offset, output
  selection, negation, both amplitude modes, and a range of phase and output
  widths.

  The C model is not cycle accurate: it computes one output per input sample,
  and applies new values of programmable increments and offsets between calls.
  The tests split each batch at the CONFIG channel transfers accordingly. The
  HDL tests check the timing against the IP itself.

  = Running the tests

  These tests are only part of the unit tests if the Cabal flag
  @xilinx-dds-cmodel@ is enabled. It is disabled by default, and enabled on
  CI. Xilinx's C model cannot be redistributed, so the tests locate it at run
  time:

  1. If @CLASH_CORES_XILINX_DDS_CMODEL@ is set, it should point at either
     @dds_compiler_v6_0_bitacc_cmodel_lin64.zip@ (Vivado ships it in
     @data\/ip\/xilinx\/dds_compiler_v6_0\/cmodel@) or a directory holding its
     extracted contents.

  2. Otherwise, if @XILINX_VIVADO@ is set (by Vivado's @settings64.sh@), the
     zip file is taken from that installation.

  If neither is set, or the C model is not where they point, the tests fail.

  Each property runs batches of 'batchSize' inputs. The number of batches can
  be set with @--hedgehog-tests@; @--hedgehog-tests 2000@ runs a million cases
  per configuration.

  The C model hangs for some configurations the IP does not support, so the
  tests only use supported ones.
-}
module Test.Cores.Xilinx.Dds (tests) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, SomeException, bracket, catch, onException, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CDouble (..), CInt (..), CSize (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (peek, peekByteOff, pokeByteOff)
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

import Clash.Explicit.Prelude (KnownNat, NFDataX, System, Unsigned, bitCoerce, natToNum)
import qualified Clash.Explicit.Prelude as C
import Clash.Signal.Delayed (DSignal)
import qualified Clash.Signal.Delayed as D

import Clash.Cores.Xilinx.Dds.Explicit
  ( AmplitudeMode (..), Config (..), DataOutput, Increment (..), MemoryType (..)
  , Offset (..), Output (..), Phase, Sample )
import qualified Clash.Cores.Xilinx.Dds.Explicit as X
import Clash.Cores.Xilinx.Dds.Internal
  ( Params (..), PartsPresent (..), PhaseSource (..), checkWidths, minLatency, mkParams
  , tableQuirk )

import Hedgehog (Gen, Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)

------------------------------------------------------------------------------
-- Locating and loading Xilinx C models
--
-- Nothing in this section is specific to the DDS Compiler IP. It is a copy of
-- the corresponding section of the tests of the other IP cores with a C model,
-- to be shared once there is a common module for it.
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
-- offer @RTLD_DEEPBIND@, which makes a C model use its own copies of
-- libraries (such as GMP) that are also linked into Haskell programs.
rtldNow, rtldDeepBind :: CInt
rtldNow = 0x2
rtldDeepBind = 0x8

-- | Load a shared library
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
-- The DDS Compiler C model
------------------------------------------------------------------------------

ddsLocation :: CModelLocation
ddsLocation = CModelLocation
  { envVar = "CLASH_CORES_XILINX_DDS_CMODEL"
  , zipInVivado = "data/ip/xilinx/dds_compiler_v6_0/cmodel/dds_compiler_v6_0_bitacc_cmodel_lin64.zip"
  }

-- | @xip_array_real*@
type XipArray = Ptr ()

-- | @xip_dds_v6_0*@
type XipDds = Ptr ()

type ArrayCreate = IO XipArray
type ArrayReserve = XipArray -> CSize -> IO CInt
type ArrayDestroy = XipArray -> IO XipArray
-- | @xip_dds_v6_0_xip_array_real_set_data(array, value, sample, channel, field)@
type ArraySet = XipArray -> CDouble -> CSize -> CSize -> CSize -> IO CInt
-- | @xip_dds_v6_0_xip_array_real_get_data(array, &value, sample, channel, field)@
type ArrayGet = XipArray -> Ptr CDouble -> CSize -> CSize -> CSize -> IO CInt
type DefaultConfig = Ptr () -> IO CInt
-- | @xip_dds_v6_0_create(config, handler, handle)@
type Create = Ptr () -> FunPtr () -> Ptr () -> IO XipDds
type Destroy = XipDds -> IO CInt
type AllocConfigPkt = Ptr (Ptr ()) -> CUInt -> CUInt -> IO CInt
type FreeConfigPkt = Ptr (Ptr ()) -> IO CInt
type ConfigDo = XipDds -> Ptr () -> IO CInt
-- | @xip_dds_v6_0_data_do(model, in, out, samples, channels, in_fields, out_fields)@
type DataDo = XipDds -> XipArray -> XipArray -> CSize -> CSize -> CSize -> CSize -> IO CInt

foreign import ccall "dynamic" mkArrayCreate :: FunPtr ArrayCreate -> ArrayCreate
foreign import ccall "dynamic" mkArrayReserve :: FunPtr ArrayReserve -> ArrayReserve
foreign import ccall "dynamic" mkArrayDestroy :: FunPtr ArrayDestroy -> ArrayDestroy
foreign import ccall "dynamic" mkArraySet :: FunPtr ArraySet -> ArraySet
foreign import ccall "dynamic" mkArrayGet :: FunPtr ArrayGet -> ArrayGet
foreign import ccall "dynamic" mkDefaultConfig :: FunPtr DefaultConfig -> DefaultConfig
foreign import ccall "dynamic" mkCreate :: FunPtr Create -> Create
foreign import ccall "dynamic" mkDestroy :: FunPtr Destroy -> Destroy
foreign import ccall "dynamic" mkAllocConfigPkt :: FunPtr AllocConfigPkt -> AllocConfigPkt
foreign import ccall "dynamic" mkFreeConfigPkt :: FunPtr FreeConfigPkt -> FreeConfigPkt
foreign import ccall "dynamic" mkConfigDo :: FunPtr ConfigDo -> ConfigDo
foreign import ccall "dynamic" mkDataDo :: FunPtr DataDo -> DataDo

data DdsCModel = DdsCModel
  { loaded :: LoadedCModel
  , arrCreate :: ArrayCreate
  , arrReserveData :: ArrayReserve
  , arrReserveDim :: ArrayReserve
  , arrDestroy :: ArrayDestroy
  , arrSet :: ArraySet
  , arrGet :: ArrayGet
  , defaultConfig :: DefaultConfig
  , create :: Create
  , destroy :: Destroy
  , allocConfigPkt :: AllocConfigPkt
  , freeConfigPkt :: FreeConfigPkt
  , configDo :: ConfigDo
  , dataDo :: DataDo
  }

loadDds :: IO DdsCModel
loadDds = do
  loaded@LoadedCModel{libraries} <-
    loadCModel ddsLocation ["libIp_dds_compiler_v6_0_bitacc_cmodel.so"]
  lib <- case libraries of
    l : _ -> pure l
    [] -> fail "No libraries loaded"
  let
    fun :: (FunPtr a -> a) -> String -> IO a
    fun mk name = mk <$> lookupFunction lib ("xip_dds_v6_0_" <> name)
  DdsCModel loaded
    <$> fun mkArrayCreate "xip_array_real_create"
    <*> fun mkArrayReserve "xip_array_real_reserve_data"
    <*> fun mkArrayReserve "xip_array_real_reserve_dim"
    <*> fun mkArrayDestroy "xip_array_real_destroy"
    <*> fun mkArraySet "xip_array_real_set_data"
    <*> fun mkArrayGet "xip_array_real_get_data"
    <*> fun mkDefaultConfig "default_config"
    <*> fun mkCreate "create"
    <*> fun mkDestroy "destroy"
    <*> fun mkAllocConfigPkt "alloc_config_pkt"
    <*> fun mkFreeConfigPkt "free_config_pkt"
    <*> fun mkConfigDo "config_do"
    <*> fun mkDataDo "data_do"

unloadDds :: DdsCModel -> IO ()
unloadDds = unloadCModel . loaded

-- | Byte offsets in @xip_array_real@
offDataSize, offDim, offDimSize :: Int
offDataSize = 8
offDim = 24
offDimSize = 32

-- | Size of @xip_dds_v6_0_config@, and byte offsets of its fields, from
-- @dds_compiler_v6_0_bitacc_cmodel.h@
configSize :: Int
configSize = 504

-- | Unsigned fields of @xip_dds_v6_0_config@ and their offsets
data ConfigField
  = PartsPresentF | ChannelsF | ModeOfOperationF | ParameterEntryF | NoiseShapingF
  | PhaseIncrementF | ResyncF | PhaseOffsetF | OutputSelectionF | NegativeSineF
  | NegativeCosineF | AmplitudeModeF | MemoryTypeF | OptimizationGoalF | Dsp48UseF
  | HasTreadyF | OutputFormF | LatencyConfigurationF | HasAresetnF | PhaseWidthF
  | OutputWidthF

fieldOffset :: ConfigField -> Int
fieldOffset = \case
  PartsPresentF -> 8
  ChannelsF -> 24
  ModeOfOperationF -> 28
  ParameterEntryF -> 36
  NoiseShapingF -> 56
  PhaseIncrementF -> 60
  ResyncF -> 64
  PhaseOffsetF -> 68
  OutputSelectionF -> 72
  NegativeSineF -> 76
  NegativeCosineF -> 80
  AmplitudeModeF -> 84
  MemoryTypeF -> 88
  OptimizationGoalF -> 92
  Dsp48UseF -> 96
  HasTreadyF -> 100
  OutputFormF -> 108
  LatencyConfigurationF -> 112
  HasAresetnF -> 116
  PhaseWidthF -> 380
  OutputWidthF -> 384

-- | Offsets of the first elements of the @PINC@ and @POFF@ arrays (doubles)
offPinc, offPoff :: Int
offPinc = 120
offPoff = 248

-- | Create a two- or three-dimensional array (samples, channels, fields), run
-- an action with it and destroy it.
withArray :: DdsCModel -> Int -> Int -> (XipArray -> IO a) -> IO a
withArray DdsCModel{..} n fields act = bracket acquire arrDestroy act
 where
  acquire = do
    arr <- arrCreate
    when (arr == nullPtr) $ fail "Could not create C model array"
    checkStatus "reserve_dim" =<< arrReserveDim arr 3
    checkStatus "reserve_data" =<< arrReserveData arr (fromIntegral (max 1 (n * fields)))
    pokeByteOff arr offDimSize (3 :: CSize)
    dim <- peekByteOff arr offDim :: IO (Ptr CSize)
    pokeByteOff dim 0 (fromIntegral n :: CSize)
    pokeByteOff dim 8 (1 :: CSize)
    pokeByteOff dim 16 (fromIntegral fields :: CSize)
    pokeByteOff arr offDataSize (fromIntegral (n * fields) :: CSize)
    pure arr

checkStatus :: String -> CInt -> IO ()
checkStatus what status =
  unless (status == 0) $ fail ("C model call failed: " <> what)

------------------------------------------------------------------------------
-- Configurations and inputs
------------------------------------------------------------------------------

-- | How the phase increment is set; values are raw (unsigned) phases
data IncrementMode = IncFixed Integer | IncProgrammable Integer | IncStreaming | IncStreamingResync
  deriving (Eq, Show)

-- | How the phase offset is set; values are raw (unsigned) phases
data OffsetMode = OffNone | OffFixed Integer | OffProgrammable Integer | OffStreaming
  deriving (Eq, Show)

data OutputSelection = SineOnly | CosineOnly | SineAndCosine
  deriving (Eq, Show)

-- | A configuration of the IP under test
data TestConfig = TestConfig
  { tcParts :: PartsPresent
  , tcPhaseWidth :: Int
  , tcOutputWidth :: Int
  , tcIncrement :: IncrementMode
  , tcOffset :: OffsetMode
  , tcOutput :: OutputSelection
  , tcConfig :: Config
  } deriving (Show)

incSourceOf :: IncrementMode -> PhaseSource
incSourceOf = \case
  IncFixed _ -> FixedSource
  IncProgrammable _ -> ProgrammableSource
  _ -> StreamingSource

offSourceOf :: OffsetMode -> PhaseSource
offSourceOf = \case
  OffNone -> NoSource
  OffFixed _ -> FixedSource
  OffProgrammable _ -> ProgrammableSource
  OffStreaming -> StreamingSource

tcParams :: TestConfig -> Params
tcParams TestConfig{..} =
  mkParams tcParts (incSourceOf tcIncrement) (offSourceOf tcOffset) tcConfig
    tcPhaseWidth tcOutputWidth

-- | One sample of the input PHASE channel, as raw integers. For the SIN\/COS
-- LUT, the phase is in 'inIncrement'.
data InputSample = InputSample { inIncrement :: Integer, inOffset :: Integer, inResync :: Bool }
  deriving (Eq, Show, Generic, NFDataX)

-- | A batch of inputs: samples, and CONFIG channel transfers @(i, (increment,
-- offset))@, which apply from sample @i@ on (@i > 0@).
data Batch = Batch { samples :: [InputSample], updates :: [(Int, (Integer, Integer))] }
  deriving (Show)

-- | Outputs of the IP for one sample, as raw integers (phase unsigned, sine
-- and cosine signed). Absent outputs are 'Nothing'.
data Outputs = Outputs { outPhase :: Maybe Integer, outSine :: Maybe Integer, outCosine :: Maybe Integer }
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Running the C model
------------------------------------------------------------------------------

-- | Run the C model on a batch of inputs
runCModel :: DdsCModel -> TestConfig -> Batch -> IO [Outputs]
runCModel model@DdsCModel{loaded = LoadedCModel{lock}} tc@TestConfig{..} Batch{..} =
  withMVar lock $ \() ->
  allocaBytes configSize $ \cfgPtr ->
  withCString "clash-cores" $ \name -> do
    checkStatus "default_config" =<< defaultConfig model cfgPtr
    pokeConfig cfgPtr
    pokeByteOff cfgPtr 0 name
    bracket (create model cfgPtr nullFunPtr nullPtr) (destroy model) $ \h -> do
      when (h == nullPtr) $ fail ("Could not create C model for " <> show tc)
      concat <$> forM segments (runSegment h)
 where
  isLut = tcParts == SinCosLutOnly
  hasLut = tcParts /= PhaseGeneratorOnly
  streamInc = not isLut && incSourceOf tcIncrement == StreamingSource
  resync = not isLut && tcIncrement == IncStreamingResync
  streamOff = not isLut && tcOffset == OffStreaming
  inFields =
    if isLut then [inIncrement] else
      [inIncrement | streamInc] <> [inOffset | streamOff] <> [boolToInteger . inResync | resync]
  sineOut = hasLut && tcOutput /= CosineOnly
  cosineOut = hasLut && tcOutput /= SineOnly
  nOut = fromEnum (not isLut) + fromEnum sineOut + fromEnum cosineOut
  ow = tcOutputWidth

  -- Samples between CONFIG channel transfers, each with the transfer before
  -- it
  segments = go 0 Nothing updates
   where
    go start upd [] = [(upd, drop start samples)]
    go start upd ((i, u) : us) = (upd, take (i - start) (drop start samples)) : go i (Just u) us

  runSegment h (upd, xs) = do
    forM_ upd $ \(pinc, poff) ->
      alloca $ \pktPtr -> do
        checkStatus "alloc_config_pkt" =<< allocConfigPkt model pktPtr 1 1
        pkt <- peek pktPtr
        pincs <- peekByteOff pkt 0 :: IO (Ptr CDouble)
        poffs <- peekByteOff pkt 8 :: IO (Ptr CDouble)
        pokeByteOff pincs 0 (fromInteger pinc :: CDouble)
        pokeByteOff poffs 0 (fromInteger poff :: CDouble)
        checkStatus "config_do" =<< configDo model h pkt
        checkStatus "free_config_pkt" =<< freeConfigPkt model pktPtr
    if null xs then pure [] else do
      let n = length xs
          nIn = length inFields
      withArray model n nIn $ \din -> withArray model n nOut $ \dout -> do
        forM_ (zip [0 ..] xs) $ \(i, x) ->
          forM_ (zip [0 ..] inFields) $ \(f, field) ->
            checkStatus "set_data" =<< arrSet model din (fromInteger (field x)) i 0 f
        checkStatus "data_do" =<<
          dataDo model h din dout (fromIntegral n) 1 (fromIntegral nIn) (fromIntegral nOut)
        forM [0 .. fromIntegral n - 1] $ \i -> do
          vals <- forM [0 .. fromIntegral nOut - 1] $ \f -> alloca $ \v -> do
            checkStatus "get_data" =<< arrGet model dout v i 0 f
            toIntegerExact <$> peek v
          let
            (ph, rest) = if isLut then (Nothing, vals) else (Just (headOr vals), drop 1 vals)
            (s, rest') = if sineOut then (Just (signedOf (headOr rest)), drop 1 rest) else (Nothing, rest)
            c = if cosineOut then Just (signedOf (headOr rest')) else Nothing
          pure Outputs{outPhase = if hasLut then Nothing else ph, outSine = s, outCosine = c}

  headOr (x : _) = x
  headOr [] = error "C model returned too few fields"
  signedOf v =
    let m = v .&. ((1 `shiftL` ow) - 1)
    in if m >= 1 `shiftL` (ow - 1) then m - (1 `shiftL` ow) else m
  toIntegerExact :: CDouble -> Integer
  toIntegerExact d =
    let i = truncate d :: Integer
    in if fromIntegral i == d then i else error ("C model returned a non-integer: " <> show d)
  boolToInteger b = if b then 1 else 0

  pokeConfig :: Ptr () -> IO ()
  pokeConfig p = do
    let
      fields :: [(ConfigField, Word32)]
      fields =
        [ (PartsPresentF, case tcParts of
            PhaseGeneratorAndSinCosLut -> 0
            PhaseGeneratorOnly -> 1
            SinCosLutOnly -> 2)
        , (ChannelsF, 1)
        , (ModeOfOperationF, 0)
        , (ParameterEntryF, 1)
        , (NoiseShapingF, 0)
        , (PhaseIncrementF, sourceCode (incSourceOf tcIncrement))
        , (ResyncF, if tcIncrement == IncStreamingResync then 1 else 0)
        , (PhaseOffsetF, sourceCode (offSourceOf tcOffset))
        , (OutputSelectionF, case tcOutput of
            SineOnly -> 0
            CosineOnly -> 1
            SineAndCosine -> 2)
        , (NegativeSineF, if negativeSine tcConfig then 1 else 0)
        , (NegativeCosineF, if negativeCosine tcConfig then 1 else 0)
        , (AmplitudeModeF, case amplitudeMode tcConfig of
            FullRange -> 0
            UnitCircle -> 1)
        , (MemoryTypeF, case memoryType tcConfig of
            AutoMemory -> 0
            BlockRom -> 1
            DistributedRom -> 2)
        , (OptimizationGoalF, 0)
        , (Dsp48UseF, 0)
        , (HasTreadyF, 0)
        , (OutputFormF, 0)
        , (LatencyConfigurationF, 0)
        , (HasAresetnF, 0)
        , (PhaseWidthF, fromIntegral tcPhaseWidth)
        , (OutputWidthF, fromIntegral (if hasLut then tcOutputWidth else 12))
        ]
    forM_ fields $ \(f, v) -> pokeByteOff p (fieldOffset f) v
    pokeByteOff p offPinc (fromInteger (incValue tcIncrement) :: CDouble)
    pokeByteOff p offPoff (fromInteger (offValue tcOffset) :: CDouble)

  sourceCode = \case
    NoSource -> 0
    ProgrammableSource -> 1
    FixedSource -> 2
    StreamingSource -> 3

incValue :: IncrementMode -> Integer
incValue = \case
  IncFixed v -> v
  IncProgrammable v -> v
  _ -> 0

offValue :: OffsetMode -> Integer
offValue = \case
  OffFixed v -> v
  OffProgrammable v -> v
  _ -> 0

------------------------------------------------------------------------------
-- Running the Clash functions
------------------------------------------------------------------------------

withNat :: Int -> (forall n. KnownNat n => Proxy n -> r) -> r
withNat i f
  | i < 0 = error ("withNat: negative " <> show i)
  | otherwise = case someNatVal (fromIntegral i) of SomeNat p -> f p

toPhase :: forall pw. KnownNat pw => Integer -> Phase pw
toPhase = bitCoerce . (fromInteger :: Integer -> Unsigned pw)

fromPhase :: forall pw. KnownNat pw => Phase pw -> Integer
fromPhase = toInteger . (bitCoerce :: Phase pw -> Unsigned pw)

fromSample :: forall f. KnownNat f => Sample f -> Integer
fromSample = toInteger . C.unSF

-- | Run a Clash function on a list of inputs and return its outputs, skipping
-- the first @d@ cycles.
simulate ::
  forall d a b.
  (KnownNat d, NFDataX a) =>
  (C.Signal System (Maybe a) -> C.Signal System b) ->
  [Maybe a] ->
  [b]
simulate f xs = take (length xs) $ drop (natToNum @d) $ C.sample_lazy (f (C.fromList (xs <> repeat Nothing)))

clk :: C.Clock System
clk = C.clockGen

en :: C.Enable System
en = C.enableGen

-- | An 'Increment' with functions that build its stream and CONFIG channel
-- values from raw integers
withIncrement ::
  forall pw r.
  KnownNat pw =>
  IncrementMode ->
  ( forall is ic.
    (X.IncrementField is, X.Field ic, NFDataX is, NFDataX ic) =>
    Increment pw is ic -> (InputSample -> is) -> (Integer -> ic) -> r ) ->
  r
withIncrement m k = case m of
  IncFixed v -> k (FixedIncrement (toPhase v)) (const ()) (const ())
  IncProgrammable v -> k (ProgrammableIncrement (toPhase v)) (const ()) toPhase
  IncStreaming -> k StreamingIncrement (toPhase . inIncrement) (const ())
  IncStreamingResync -> k StreamingIncrementResync (\s -> (toPhase (inIncrement s), inResync s)) (const ())

withOffset ::
  forall pw r.
  KnownNat pw =>
  OffsetMode ->
  ( forall os oc.
    (X.Field os, X.Field oc, NFDataX os, NFDataX oc) =>
    Offset pw os oc -> (InputSample -> os) -> (Integer -> oc) -> r ) ->
  r
withOffset m k = case m of
  OffNone -> k NoOffset (const ()) (const ())
  OffFixed v -> k (FixedOffset (toPhase v)) (const ()) (const ())
  OffProgrammable v -> k (ProgrammableOffset (toPhase v)) (const ()) toPhase
  OffStreaming -> k StreamingOffset (toPhase . inOffset) (const ())

withOutput ::
  forall f r.
  KnownNat f =>
  OutputSelection ->
  (forall o. DataOutput o => Output f o -> (o -> Outputs) -> r) ->
  r
withOutput sel k = case sel of
  SineOnly -> k SineOutput (\s -> Outputs Nothing (Just (fromSample s)) Nothing)
  CosineOnly -> k CosineOutput (\c -> Outputs Nothing Nothing (Just (fromSample c)))
  SineAndCosine ->
    k SineCosineOutput (\(c, s) -> Outputs Nothing (Just (fromSample s)) (Just (fromSample c)))

-- | Run the Clash function for a configuration on a batch of inputs
runClash :: TestConfig -> Batch -> [Outputs]
runClash tc@TestConfig{..} Batch{..} =
  withNat (minLatency (tcParams tc)) $ \(Proxy :: Proxy d) ->
  withNat tcPhaseWidth $ \(Proxy :: Proxy pw) ->
  withNat (tcOutputWidth - 1) $ \(Proxy :: Proxy f) ->
  case tcParts of
    SinCosLutOnly ->
      withOutput @f tcOutput $ \out toOutputs ->
        let
          f inp = D.toSignal $
            X.sinCosLutWith @d @System @0 @pw @f tcConfig out clk en
              (D.fromSignal (maybe 0 (toPhase . inIncrement) <$> inp))
        in map toOutputs (simulate @d f (map Just samples))
    PhaseGeneratorOnly ->
      withIncrement @pw tcIncrement $ \inc incStream incCfg ->
      withOffset @pw tcOffset $ \off offStream offCfg ->
        let
          f inp = D.toSignal $
            X.phaseGeneratorWith @d @System @0 inc off clk en
              (fmap (\(i, o) -> (incCfg i, offCfg o)) <$> configIn inp)
              (phaseIn incStream offStream inp)
        in [ Outputs (Just (fromPhase p)) Nothing Nothing | p <- simulate @d f inputs ]
    PhaseGeneratorAndSinCosLut ->
      withIncrement @pw tcIncrement $ \inc incStream incCfg ->
      withOffset @pw tcOffset $ \off offStream offCfg ->
      withOutput @f tcOutput $ \out toOutputs ->
        let
          f inp = D.toSignal $
            X.ddsWith @d @System @0 tcConfig inc off out clk en
              (fmap (\(i, o) -> (incCfg i, offCfg o)) <$> configIn inp)
              (phaseIn incStream offStream inp)
        in map toOutputs (simulate @d f inputs)
 where
  -- Each cycle: the sample, and the CONFIG channel transfer in that cycle.
  -- A transfer applies from the next cycle on, so the transfer that applies
  -- from sample i happens in cycle i - 1.
  inputs = [ Just (s, lookup (i + 1) updates) | (i, s) <- zip [0 :: Int ..] samples ]
  configIn = fmap (>>= snd)
  phaseIn ::
    (InputSample -> is) -> (InputSample -> os) -> C.Signal System (Maybe (InputSample, a)) ->
    DSignal System 0 (is, os)
  phaseIn incStream offStream inp =
    D.fromSignal ((\s -> (incStream s, offStream s)) . maybe (InputSample 0 0 False) fst <$> inp)

------------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------------

-- | Number of input samples per Hedgehog test case
batchSize :: Int
batchSize = 500

-- | Number of batches per property, unless overridden by @--hedgehog-tests@
nTests :: H.TestLimit
nTests = 40

prop_cmodel :: IO DdsCModel -> TestConfig -> Property
prop_cmodel getModel tc = H.withTests nTests $ H.property $ do
  model <- H.evalIO getModel
  batch <- H.forAllWith showBatch (genBatch tc)
  cOut <- H.evalIO (runCModel model tc batch)
  let
    hOut = runClash tc batch
    mismatches =
      [ (i, inp, c, h) | (i, inp, c, h) <- zip4 [0 :: Int ..] (samples batch) cOut hOut, c /= h ]
  H.annotateShow (tcParams tc)
  length hOut === length cOut
  case mismatches of
    [] -> pure ()
    ((i, inp, c, h) : _) -> do
      H.footnote ("first mismatch at index " <> show i <> " of " <> show (length mismatches))
      H.footnote ("input: " <> show inp)
      H.footnote ("CONFIG channel transfers: " <> show (updates batch))
      c === h
 where
  showBatch b =
    show (length (samples b)) <> " samples, " <> show (length (updates b)) <> " CONFIG transfers"
  zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
  zip4 _ _ _ _ = []

------------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------------

-- | A raw phase of @pw@ bits, biased towards multiples of an eighth of a
-- circle (and the phases next to them) and the smallest phases
genPhase :: Int -> Gen Integer
genPhase pw =
  Gen.frequency $
    [ (6, Gen.integral (Range.constant 0 (full - 1)))
    , (2, near <$> Gen.integral (Range.constant 0 7) <*> Gen.integral (Range.constant (-2) 2))
    , (1, Gen.integral (Range.constant 0 (min (full - 1) 16)))
    ]
    <> [ (1, Gen.element quirks) | not (null quirks) ]
 where
  full = 1 `shiftL` pw
  near k d = (k * full `div` 8 + d) `mod` full
  -- The table entries that round differently from the formula, in all four
  -- quadrants
  quirks =
    [ (q * (full `div` 4) + s * k) `mod` full
    | (_, a, k, _) <- tableQuirk, a == pw, q <- [0 .. 3], s <- [1, -1] ]

genSample :: Int -> Gen InputSample
genSample pw =
  InputSample <$> genPhase pw <*> genPhase pw <*> Gen.frequency [(15, pure False), (1, pure True)]

genBatch :: TestConfig -> Gen Batch
genBatch tc@TestConfig{..} = do
  xs <- Gen.list (Range.singleton batchSize) (genSample tcPhaseWidth)
  us <-
    if programmable
      then do
        k <- Gen.int (Range.constant 0 8)
        is <- Gen.list (Range.singleton k) (Gen.int (Range.constant 1 (batchSize - 1)))
        forM (dedup (sortInts is)) $ \i -> (,) i <$> ((,) <$> genPhase tcPhaseWidth <*> genPhase tcPhaseWidth)
      else pure []
  pure Batch{samples = xs, updates = us}
 where
  p = tcParams tc
  programmable = incrementSource p == ProgrammableSource || offsetSource p == ProgrammableSource
  dedup (a : b : rest) | a == b = dedup (b : rest)
  dedup (a : rest) = a : dedup rest
  dedup [] = []
  sortInts = foldr insert []
  insert x [] = [x]
  insert x (y : ys) | x <= y = x : y : ys | otherwise = y : insert x ys

------------------------------------------------------------------------------
-- Configurations under test
------------------------------------------------------------------------------

-- | All ways to set the increment and offset, with some (raw) values
incrementModes :: Int -> [IncrementMode]
incrementModes pw =
  [IncFixed (3 `mod` full), IncProgrammable ((full `div` 3) + 1), IncStreaming, IncStreamingResync]
 where
  full = 1 `shiftL` pw

offsetModes :: Int -> [OffsetMode]
offsetModes pw = [OffNone, OffFixed ((full * 5) `div` 7), OffProgrammable (full `div` 4 + 3), OffStreaming]
 where
  full = 1 `shiftL` pw

defaultTest :: PartsPresent -> Int -> Int -> TestConfig
defaultTest parts pw ow =
  TestConfig parts pw ow IncStreaming OffStreaming SineAndCosine X.defConfig

testConfigs :: [TestConfig]
testConfigs =
  -- SIN/COS lookup table: widths, then output options
  [ defaultTest SinCosLutOnly pw ow
  | (pw, ow) <- [(3, 3), (4, 26), (5, 8), (8, 8), (10, 16), (12, 12), (14, 20), (16, 16), (16, 26), (16, 4)]
  ]
  <>
  [ (defaultTest SinCosLutOnly pw ow){tcOutput = sel, tcConfig = cfg}
  | (pw, ow) <- [(10, 12), (16, 18)]
  , sel <- [SineOnly, CosineOnly, SineAndCosine]
  , cfg <- outputConfigs
  ]
  <>
  -- Phase generator: all increment and offset modes, several widths
  [ (defaultTest PhaseGeneratorOnly pw 3){tcIncrement = inc, tcOffset = off}
  | pw <- [3, 12, 32, 48]
  , inc <- incrementModes pw
  , off <- offsetModes pw
  ]
  <>
  -- DDS: all increment and offset modes
  [ (defaultTest PhaseGeneratorAndSinCosLut 16 12){tcIncrement = inc, tcOffset = off}
  | inc <- incrementModes 16
  , off <- offsetModes 16
  ]
  <>
  -- DDS: widths, with output narrower than, equal to, and wider than the phase
  [ defaultTest PhaseGeneratorAndSinCosLut pw ow
  | (pw, ow) <-
      [ (3, 3), (3, 26), (4, 12), (8, 8), (10, 14), (12, 10), (16, 16), (16, 26), (20, 8)
      , (24, 16), (32, 12), (48, 16), (48, 3) ]
  ]
  <>
  -- DDS: output options
  [ (defaultTest PhaseGeneratorAndSinCosLut pw ow){tcOutput = sel, tcConfig = cfg, tcIncrement = IncStreamingResync}
  | (pw, ow) <- [(14, 10), (12, 16)]
  , sel <- [SineOnly, CosineOnly, SineAndCosine]
  , cfg <- outputConfigs
  ]
 where
  outputConfigs =
    [ X.defConfig{negativeSine = ns, negativeCosine = nc, amplitudeMode = am}
    | (ns, nc) <- [(False, False), (True, False), (False, True), (True, True)]
    , am <- [FullRange, UnitCircle]
    ]

testName :: TestConfig -> String
testName TestConfig{..} =
  show tcParts <> " " <> show tcPhaseWidth
    <> (if tcParts == PhaseGeneratorOnly then "" else "->" <> show tcOutputWidth)
    <> (if tcParts == SinCosLutOnly then "" else " " <> show tcIncrement <> " " <> show tcOffset)
    <> (if tcParts == PhaseGeneratorOnly then "" else " " <> show tcOutput <> " " <> show tcConfig)

tests :: TestTree
tests =
  testGroup "Xilinx DDS Compiler"
    [ withResource loadDds unloadDds $ \getModel ->
        testGroup "vs. C model"
          [ testPropertyNamed (testName tc) (fromString ("prop_dds_" <> show i)) (prop_cmodel getModel tc)
          | (i, tc) <- zip [0 :: Int ..] (nubOn testName testConfigs)
          , checkWidths (tcParams tc) == Nothing
          ]
    ]
 where
  nubOn f = go []
   where
    go _ [] = []
    go seen (x : xs)
      | f x `elem` seen = go seen xs
      | otherwise = x : go (f x : seen) xs
