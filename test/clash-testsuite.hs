{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Clash.Util.Interpolate    as I

import           Clash.Annotations.Primitive (HDL(..))
import qualified Data.Text                 as Text
import           Data.Default              (def)
import           Data.List                 ((\\), intercalate)
import           Data.List.Extra           (trim)
import           Data.Version              (versionBranch)
import           System.Directory
  (getCurrentDirectory, doesDirectoryExist, makeAbsolute, setCurrentDirectory)
import           System.Environment
import           System.Info
import           System.Process            (readProcess)
import           GHC.Conc                  (numCapabilities)
import           GHC.Stack
import           GHC.IO.Unsafe             (unsafePerformIO)
import           Text.Printf               (printf)

import           Test.Tasty
import           Test.Tasty.Common
import           Test.Tasty.Clash

-- | GHC version as major.minor.patch1. For example: 8.10.2.
ghcVersion3 :: String
ghcVersion3 =
#ifdef __GLASGOW_HASKELL_PATCHLEVEL2__
  let ghc_p1 = __GLASGOW_HASKELL_PATCHLEVEL1__
      ghc_p2 = __GLASGOW_HASKELL_PATCHLEVEL2__ in
  case ghc_p2 of
    0 ->
      intercalate "." (map show (versionBranch compilerVersion <> [ghc_p1]))
    _ ->
      intercalate "." (map show (versionBranch compilerVersion <> [ghc_p1,ghc_p2]))
#else
  let ghc_p1 = __GLASGOW_HASKELL_PATCHLEVEL1__ in
  intercalate "." (map show (versionBranch compilerVersion <> [ghc_p1]))
#endif

-- Directory clash binary is expected to live in
cabalClashBinDir :: IO String
cabalClashBinDir = makeAbsolute rel_path
 where
  rel_path = printf templ platform ghcVersion3 (VERSION_clash_ghc :: String)
  platform :: String -- XXX: Hardcoded
  platform = case os of
     "mingw32" -> arch <> "-windows"
     _ -> arch <> "-" <> os
  templ = "dist-newstyle/build/%s/ghc-%s/clash-ghc-%s/x/clash/build/clash/" :: String

-- | Set GHC_PACKAGE_PATH for local Cabal install. Currently hardcoded for Unix;
-- override by setting @store_dir@ to point to local cabal installation.
setCabalPackagePaths :: IO ()
setCabalPackagePaths = do
  ch <- lookupEnv "store_dir"
  storeDir <- case ch of
    Just dir -> pure dir
    Nothing -> case os of
      "mingw32" -> pure "C:/cabal/store" -- default ghcup location
      _ ->  (<> "/.cabal/store") <$> getEnv "HOME"
  here <- getCurrentDirectory
  setEnv "GHC_PACKAGE_PATH" $
       storeDir <> "/ghc-" <> ghcVersion3 <> "/package.db"
    <> ":"
    <> here <> "/dist-newstyle/packagedb/ghc-" <> ghcVersion3
    <> ":"

-- | See 'compiledWith'
data RunWith
  = Stack
  | Cabal
  | Global
  deriving (Show, Eq)

-- | Detects Clash binary the testsuite should use (in order):
--
--     * If USE_GLOBAL_CLASH=1, use globally installed Clash
--     * If STACK_EXE is present, use Stack's Clash
--     * If dist-newstyle is present, use Cabal's Clash
--     * Use globally installed Clash
--
compiledWith :: RunWith
compiledWith = unsafePerformIO $ do
  clash_global <- lookupEnv "USE_GLOBAL_CLASH"
  stack_exe <- lookupEnv "STACK_EXE"
  distNewstyleExists <- doesDirectoryExist "dist-newstyle"

  pure $ case (clash_global, stack_exe, distNewstyleExists) of
    (Just "1", Just _, _   ) -> error "Can't use global clash with 'stack run'"
    (Just "1", _,      _   ) -> Global
    (_,        Just _, _   ) -> Stack
    (_,        _     , True) -> Cabal
    (_,        _     , _   ) -> Global
{-# NOINLINE compiledWith #-}

-- | Set environment variables that allow Clash to be executed by simply calling
-- 'clash' without extra arguments.
setClashEnvs :: HasCallStack => RunWith -> IO ()
setClashEnvs Global = setEnv "GHC_ENVIRONMENT" "-"
setClashEnvs Stack = pure ()
setClashEnvs Cabal = do
  binDir <- cabalClashBinDir
  path <- getEnv "PATH"
  let seperator = case os of { "mingw32" -> ";"; _ -> ":" }
  setEnv "PATH" (binDir <> seperator <> path)
  setCabalPackagePaths

clashTestRoot
  :: [[TestName] -> TestTree]
  -> TestTree
clashTestRoot testTrees =
  clashTestGroup "." testTrees []

-- | `clashTestGroup` and `clashTestRoot` make sure that each test knows its
-- fully qualified test name at construction time. This is used to pass -i flags
-- to Clash as the test layout matches the layout in @shouldwork/@.
clashTestGroup
  :: TestName
  -> [[TestName] -> TestTree]
  -> ([TestName] -> TestTree)
clashTestGroup testName testTrees =
  \parentNames ->
    testGroup testName $
      zipWith ($) testTrees (repeat (testName : parentNames))

runClashTest :: IO ()
runClashTest = defaultMain $ clashTestRoot
  [ clashTestGroup "test"
    [ clashTestGroup "shouldfail"
      [ clashTestGroup "Cores"
        [ clashTestGroup "Xilinx"
          [ clashTestGroup "VIO"
            [ runTest "DuplicateOutputNames" def{
                hdlTargets=[VHDL]
              , expectClashFail=Just (def, "Tried create a signal called 'a', but identifier generation returned")
              }
            , runTest "DuplicateInputNames" def{
                hdlTargets=[VHDL]
              , expectClashFail=Just (def, "Tried create a signal called 'a', but identifier generation returned")
              }
            , runTest "DuplicateInputOutputNames" def{
                hdlTargets=[VHDL]
              , expectClashFail=Just (def, "Tried create a signal called 'a', but identifier generation returned")
              }
            , runTest "OutputBusWidthExceeded" def{
                hdlTargets=[VHDL, Verilog, SystemVerilog]
              , expectClashFail=Just (def, "Probe signals must be been between 1 and 256 bits wide.")
              }
            , runTest "OutputProbesExceeded" def{
                hdlTargets=[VHDL, Verilog, SystemVerilog]
              , expectClashFail=Just (def, "At most 256 input/output probes are supported.")
              }
            , runTest "InputBusWidthExceeded" def{
                hdlTargets=[VHDL, Verilog, SystemVerilog]
              , expectClashFail=Just (def, "Probe signals must be been between 1 and 256 bits wide.")
              }
            , runTest "InputProbesExceeded" def{
                hdlTargets=[VHDL, Verilog, SystemVerilog]
              , expectClashFail=Just (def, "At most 256 input/output probes are supported.")
              }
            ]
          ]
        ]
      ]
    , clashTestGroup "shouldwork"
      [ clashTestGroup "Cores"
        [ clashTestGroup "Xilinx"
          [ runTest "TdpBlockRam" def
            { -- Compiling with VHDL gives:
              --   https://github.com/clash-lang/clash-compiler/issues/2446
              hdlTargets = [Verilog]
            , hdlLoad = [Vivado]
            , hdlSim = [Vivado]
            , clashFlags=["-fclash-hdlsyn", "Vivado"]
            , buildTargets=BuildSpecific [ "normalWritesTB", "writeEnableWritesTB" ]
            }
          , let _opts = def{ hdlTargets=[VHDL, Verilog]
                           , hdlLoad=[Vivado]
                           , hdlSim=[Vivado]
                             -- addShortPLTB now segfaults :-(
                           , buildTargets=BuildSpecific [ "addBasicTB"
                                                        , "addEnableTB"
                                                        -- , "addShortPLTB"
                                                        , "subBasicTB"
                                                        , "mulBasicTB"
                                                        , "divBasicTB"
                                                        , "compareBasicTB"
                                                        , "compareEnableTB"
                                                        , "fromUBasicTB"
                                                        , "fromUEnableTB"
                                                        , "fromSBasicTB"
                                                        , "fromSEnableTB"
                                                        ]
                           }
            in runTest "Floating" _opts
          , runTest "XpmCdcArraySingle" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
              }
          , runTest "XpmCdcGray" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
              }
          , runTest "XpmCdcHandshake" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..6]]
              }
          , runTest "XpmCdcPulse" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
              }
          , runTest "XpmCdcSingle" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
              }
          , runTest "XpmCdcSyncRst" $ def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
              }
          , runTest "DnaPortE2" def
              { hdlTargets=[VHDL, Verilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
          , clashTestGroup "DcFifo"
            [ let _opts =
                    def{ hdlTargets=[VHDL, Verilog]
                       , hdlLoad=[Vivado]
                       , hdlSim=[Vivado]
                       }
              in runTest "Basic" _opts
            , let _opts = def{ hdlTargets=[VHDL, Verilog]
                             , hdlLoad=[Vivado]
                             , hdlSim=[Vivado]
                             , buildTargets=BuildSpecific [ "testBench_17_2"
                                                          , "testBench_2_17"
                                                          , "testBench_2_2"
                                                          ]
                             }
              in runTest "Lfsr" _opts
            ]
          , let _opts =
                  def{ hdlTargets=[VHDL, Verilog, SystemVerilog]
                     , hdlLoad=[Vivado]
                     , hdlSim=[Vivado]
                     , buildTargets=BuildSpecific [ "noInputTrue"
                                                  , "noInputFalse"
                                                  , "noInputLow"
                                                  , "noInputHigh"
                                                  , "noInputSigned"
                                                  , "noInputUnsigned"
                                                  , "noInputBitVector"
                                                  , "noInputPair"
                                                  , "noInputVec"
                                                  , "noInputCustom"
                                                  , "noInputNested"
                                                  , "singleInputBool"
                                                  , "singleInputBit"
                                                  , "singleInputSigned"
                                                  , "singleInputUnsigned"
                                                  , "singleInputBitVector"
                                                  , "singleInputPair"
                                                  , "singleInputVec"
                                                  , "singleInputCustom"
                                                  , "singleInputNested"
                                                  , "multipleInputs"
                                                  , "inputsAndOutputs"
                                                  , "withSetName"
                                                  , "withSetNameNoResult"
                                                  ]
                     }
            in runTest "VIO" _opts
          , let _opts =
                  def{ hdlTargets=[VHDL, Verilog, SystemVerilog]
                     , hdlLoad=[Vivado]
                     , hdlSim=[Vivado]
                     , buildTargets=BuildSpecific [ "testWithDefaultsOne"
                                                  , "testWithDefaultsThree"
                                                  , "testWithLefts"
                                                  , "testWithRights"
                                                  , "testWithRightsSameCu"
                                                  ]
                     }
            in runTest "Ila" _opts
          , let _opts =
                  def{ hdlTargets=[VHDL, Verilog, SystemVerilog]
                     , buildTargets=BuildSpecific [ "testWithDefaultsOne"
                                                  , "testWithDefaultsThree"
                                                  , "testWithLefts"
                                                  , "testWithRights"
                                                  , "testWithRightsSameCu"
                                                  ]
                     }
            in outputTest "Ila" _opts
          , outputTest "VIO" def{
              hdlTargets=[VHDL]
            , buildTargets=BuildSpecific ["withSetName", "withSetNameNoResult"]
            }
          , runTest "T2549" def{hdlTargets=[Verilog],hdlSim=[]}
          ]
        ]
      ] -- end shouldwork
    ] -- end tests
  ] -- end .

main :: IO ()
main = do
  projectRoot <- trim <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
  setCurrentDirectory projectRoot
  setEnv "TASTY_NUM_THREADS" (show numCapabilities)
  setClashEnvs compiledWith
  runClashTest
