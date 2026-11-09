{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude

import           Clash.Annotations.Primitive (HDL(..))
import           Data.Default              (def)
import           Data.List                 (intercalate)
import           Data.List.Extra           (trim)
import           Data.Version              (versionBranch)
import           System.Directory
  (getCurrentDirectory, doesDirectoryExist, setCurrentDirectory)
import           System.Environment
import           System.FilePath           (takeDirectory)
import           System.Info
import           System.Process            (readProcess)
import           GHC.Conc                  (numCapabilities)
import           GHC.Stack
import           GHC.IO.Unsafe             (unsafePerformIO)

import           Test.Tasty
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
cabalClashBinDir =
  (takeDirectory . trim) <$>
    readProcess "cabal" ["list-bin", ":package:clash-ghc:executable:clash"] ""

-- | Set GHC_PACKAGE_PATH for local Cabal install. Currently hardcoded for Unix;
-- override by setting @store_dir@ to point to local cabal installation.
setCabalPackagePaths :: IO ()
setCabalPackagePaths = do
  here <- getCurrentDirectory
  setEnv "GHC_PACKAGE_PATH" $
       here <> "/dist-newstyle/packagedb/ghc-" <> ghcVersion3
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

-- | Options for the tests of the Intel primitives and megafunctions, see
-- @hdl-tests/shouldwork/Intel/IntelSimLib.hs@. VHDL is tested with Vivado only:
-- GHDL needs @-fsynopsys@ and @-fexplicit@ to compile Intel's VHDL models.
-- Verilog is tested with Icarus Verilog and Vivado, SystemVerilog with Vivado.
intelOpts :: TestOptions
intelOpts = def
  { hdlTargets=[VHDL, Verilog, SystemVerilog]
  , hdlLoad=[IVerilog, Vivado]
  , hdlSim=[IVerilog, Vivado]
  , clashFlags=["-ihdl-tests/shouldwork/Intel"]
  }

runClashTest ::
  -- | Whether Quartus is available, see 'setIntelEnvs'
  Bool ->
  IO ()
runClashTest quartusAvailable = defaultMain $ clashTestRoot
  [ clashTestGroup "hdl-tests"
    [ clashTestGroup "shouldfail"
      [ clashTestGroup "Xilinx"
        [ clashTestGroup "Vio"
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
    , clashTestGroup "shouldwork"
      [ clashTestGroup "I2c"
        [ runTest "WriteRead" def
            { hdlTargets=[VHDL, Verilog, SystemVerilog]
            , hdlLoad=[GHDL, IVerilog, Verilator]
            , hdlSim=[GHDL, IVerilog, Verilator]
            }
        ]
      , clashTestGroup "Intel" $ if not quartusAvailable then [] else
          -- Need Quartus for its simulation libraries and to generate the
          -- simulation models of IP, see 'setIntelEnvs'
        [ runTest "IoBuffer" intelOpts
            { buildTargets=BuildSpecific
                [ "tbIobufFloating", "tbIobufPullUp", "tbOutbufTri", "tbOpndrn"
                , "tbOutbufDiff", "tbIobufDiff"
                ]
            }
        , runTest "DdioBidir" intelOpts
            { buildTargets=BuildSpecific
                [ "tbCycloneVAsync", "tbCycloneVSync", "tbCycloneIVEAsync"
                , "tbMax10Async"
                ]
            }
        , runTest "Synchronizer" intelOpts
            { buildTargets=BuildSpecific
                ["tbSingle2", "tbSingle3", "tbBundle3", "tbBundle5"]
            }
        , runTest "ScFifo" intelOpts
            { buildTargets=BuildSpecific
                [ "tbNormalArea", "tbNormalSpeed", "tbShowAheadArea"
                , "tbShowAheadSpeed", "tbShowAheadSpeedSync", "tbUncheckedNormal"
                , "tbUncheckedShowAhead"
                ]
            }
        , runTest "DcFifo" intelOpts
            { buildTargets=BuildSpecific
                [ "tbNormalFast", "tbShowAheadSlow", "tbShowAheadSame"
                , "tbUncheckedNormal", "tbUncheckedShowAhead"
                ]
            }
        , runTest "Altsyncram" intelOpts
            { buildTargets=BuildSpecific
                [ "tbSpCycloneV", "tbSpCycloneVReg", "tbSpCycloneIVOld"
                , "tbSpCycloneIVNbe", "tbSdpCycloneVOld", "tbSdpCycloneVDontCare"
                , "tbSdpCycloneIVOld"
                , "tbTdpCycloneV", "tbTdpCycloneIV"
                ]
            }
        , runTest "AltsyncramDualClock" intelOpts
            { buildTargets=BuildSpecific
                [ "tbRomCycloneV", "tbRomMax10Reg", "tbDpRomCycloneIV"
                , "tbDpRomCycloneV", "tbSdpCycloneV", "tbSdpCycloneIVReg"
                , "tbSdpMax10Same", "tbTdpCycloneV", "tbTdpCycloneIV"
                , "tbTdpMax10Same"
                ]
            }
        , runTest "ClockCrosser" intelOpts
            { buildTargets=BuildSpecific
                [ "tbDefaultFast", "tbDefaultSlow", "tbNoPipelineSame"
                , "tbNoPipelineFast", "tbDeepSlow"
                ]
            }
        , runTest "ShiftTaps" intelOpts
            { buildTargets=BuildSpecific ["tbThreeTaps", "tbTwoTaps", "tbSingleTap"]
            }
        , runTest "ClockControl" intelOpts
            { buildTargets=BuildSpecific ["tbCycloneIVE", "tbCycloneIVE2", "tbMax10"]
            }
        , -- IP whose simulation models Quartus generates, see 'setIntelEnvs'.
          -- The models are VHDL only, so Verilog is tested
          -- with Vivado's mixed language simulator. GHDL doesn't bind the
          -- components in the models in strict VHDL-93 mode, see
          -- shouldwork/Intel/IntelSimLib.hs.
          runTest "Floating" def
            { hdlTargets=[VHDL, Verilog]
            , hdlLoad=[Vivado]
            , hdlSim=[Vivado]
            , buildTargets=BuildSpecific
                [ "addTB", "subTB", "mulTB", "divTB", "mulAddTB", "compareTB"
                , "predicateTB", "fromS32TB", "fromU32TB", "toS32TB", "toU32TB"
                , "toDoubleTB", "fromDoubleTB" ]
            }
          -- Megafunctions that Clash instantiates directly. Their simulation
          -- models exist in Verilog too, so Icarus can simulate Verilog.
        , runTest "Integer" def
            { hdlTargets=[VHDL, Verilog]
            , hdlLoad=[Vivado, IVerilog]
            , hdlSim=[Vivado, IVerilog]
            , buildTargets=BuildSpecific ["divideUnsignedTB", "divideSignedTB", "sqrtTB"]
            }
          -- No simulation model exists: the test compares against the netlists
          -- Quartus generates for the megafunction, see
          -- shouldwork/Intel/ComplexMultiplierSim.hs
        , runTest "ComplexMultiplier" def
            { hdlTargets=[VHDL, Verilog]
            , hdlLoad=[Vivado, IVerilog]
            , hdlSim=[Vivado, IVerilog]
            , clashFlags=["-ihdl-tests/shouldwork/Intel"]
            , buildTargets=BuildSpecific
                [ "signedTB", "signedNarrowTB", "signedWideTB", "unsignedTB"
                , "unsignedNarrowTB", "mixedTB", "tinyTB" ]
            }
        ]
      , clashTestGroup "Xilinx"
        [ clashTestGroup "Inst"
          [ outputTest "BiSignal" def{hdlLoad=[], hdlSim=[]}
          , outputTest "BiSignalTuple" def{hdlLoad=[], hdlSim=[]}
          , runTest "BitVectorParam" def
              { hdlTargets=[VHDL, Verilog, SystemVerilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
          , runTest "BoolStringParam" def
              { hdlTargets=[VHDL, Verilog, SystemVerilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
            -- The Verilog simulation model of IBUF ends the simulation on an
            -- illegal parameter value, which the test harness doesn't notice.
            -- Check the rendered parameters too.
          , outputTest "BoolStringParam" def
              { hdlTargets=[VHDL, Verilog, SystemVerilog]
              , hdlLoad=[]
              , hdlSim=[]
              }
          , outputTest "DiffClockOutputTuple" def{hdlLoad=[], hdlSim=[]}
          , runTest "DiffClockOutput" def
              { hdlTargets=[VHDL, Verilog, SystemVerilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
          , runTest "Iobuf" def
              { -- UNISIM's IOBUF has a @std_ulogic@ IO port, while Clash
                -- renders a 1-bit BiSignal as a @std_logic_vector@
                hdlTargets=[Verilog, SystemVerilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
          , runTest "NoInput" def
              { hdlTargets=[VHDL, Verilog, SystemVerilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
          , runTest "ResetOutputs" def
              { hdlTargets=[VHDL, Verilog, SystemVerilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
          , runTest "ZeroWidthPorts" def
              { hdlTargets=[VHDL, Verilog, SystemVerilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
          , runTest "BitBiSignal" def
              { hdlTargets=[VHDL, Verilog, SystemVerilog]
              , hdlLoad=[Vivado]
              , hdlSim=[Vivado]
              }
          ]
        , runTest "TdpBlockRam" def
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
        , runTest "Ibufds" def
          { hdlLoad = [Vivado]
          , hdlSim = [Vivado]
          , buildTargets=BuildSpecific ["testBench"]
          }

-- "Unmatchable constant as case subject"
-- https://github.com/clash-lang/clash-compiler/issues/2806
#if MIN_VERSION_clash_lib(1,9,0)
        , runTest "XpmCdcArraySingle" $ def
            { hdlTargets=[VHDL, Verilog]
            , hdlLoad=[Vivado]
            , hdlSim=[Vivado]
            , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
            }
#endif

        , runTest "XpmCdcGray" $ def
            { hdlTargets=[VHDL, Verilog]
            , hdlLoad=[Vivado]
            , hdlSim=[Vivado]
            , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..7]]
            }
        , runTest "XpmCdcHandshake" $ def
            { hdlTargets=
                [

-- (Vivado) ERROR: [VRFC 10-2989] 'tuple3_0_sel0_std_logic_vector' is not declared [/tmp/clash-test_XpmCdcHandshake-71223bd02c6e132d/vivado-tb0/XpmCdcHandshake.tb0/top_4.vhdl:122]
-- https://github.com/clash-lang/clash-compiler/issues/2807
#if MIN_VERSION_clash_lib(1,9,0)
                  VHDL,
#endif

                  Verilog
                ]
            , hdlLoad=[Vivado]
            , hdlSim=[Vivado]
            , buildTargets=BuildSpecific ["tb" <> show n | n <- [(0::Int)..6]]
            }

-- "Unmatchable constant as case subject"
-- https://github.com/clash-lang/clash-compiler/issues/2806
#if MIN_VERSION_clash_lib(1,9,0)
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
#endif

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
          in runTest "Vio" _opts
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
-- Pattern match failure in 'do' block at /home/peter/src/clash/clash-cores/test-suite/shouldwork/Xilinx/Ila.hs:103:3-8
-- https://github.com/clash-lang/clash-cores/issues/9
#if MIN_VERSION_clash_lib(1,9,0)
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
#endif
        , outputTest "Vio" def{
            hdlTargets=[VHDL]
          , buildTargets=BuildSpecific ["withSetName", "withSetNameNoResult"]
          }
        , runTest "T2549" def{hdlTargets=[Verilog],hdlSim=[]}
        , runTest "T2781" def{hdlLoad=[],hdlSim=[],hdlTargets=[VHDL]}
        ]
      ] -- end shouldwork
    ]
  ] -- end .

-- | Make Clash generate simulation models of Intel IP (see
-- "Clash.Cores.Intel.Internal"), for the tests in @shouldwork/Intel@. Uses the
-- Quartus installation in @QUARTUS_ROOTDIR@ or, if it isn't set, the default
-- Quartus Prime Lite 21.1 installation on our machines. The device family is
-- taken from @CLASH_INTEL_SIM_FAMILY@, and defaults to MAX 10. See
-- shouldwork/Intel/IntelSimLib.hs for why not Cyclone V. Returns whether
-- Quartus is available: the tests are skipped if it isn't.
setIntelEnvs :: IO Bool
setIntelEnvs = do
  let defaultRoot = "/opt/tools/Intel/intelFPGA_lite/21.1/quartus"
  root <- lookupEnv "QUARTUS_ROOTDIR"
  defaultRootExists <- doesDirectoryExist defaultRoot
  case root of
    Just (_:_) -> pure ()
    _ | defaultRootExists -> setEnv "QUARTUS_ROOTDIR" defaultRoot
      | otherwise -> pure ()
  family <- lookupEnv "CLASH_INTEL_SIM_FAMILY"
  case family of
    Just (_:_) -> pure ()
    _ -> setEnv "CLASH_INTEL_SIM_FAMILY" "MAX 10 FPGA"
  root1 <- lookupEnv "QUARTUS_ROOTDIR"
  case root1 of
    Just r@(_:_) -> doesDirectoryExist r
    _ -> pure False

main :: IO ()
main = do
  projectRoot <- trim <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
  setCurrentDirectory projectRoot
  setEnv "TASTY_NUM_THREADS" (show numCapabilities)
  setClashEnvs compiledWith
  quartusAvailable <- setIntelEnvs
  runClashTest quartusAvailable
