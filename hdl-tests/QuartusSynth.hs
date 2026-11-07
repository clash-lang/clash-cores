{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- | Synthesis tests of Intel IP with Quartus. The simulation models of some
Intel hard blocks (like the chip ID block and the internal oscillator of MAX
10 devices) are encrypted for Questa only, so the HDL Clash generates for them
can't be simulated with the simulators of the test suite. These tests run
Clash on a small design using them, compile it with Quartus, and check
Quartus's reports, e.g. that the hard block is used.

The tests use the Quartus installation in @QUARTUS_ROOTDIR@, see
'Main.setIntelEnvs'.
-}
module QuartusSynth
  ( quartusTest
  , Flow(..)
  , Check(..)
  ) where

import Prelude

import Clash.Annotations.Primitive (HDL(..))
import Control.Monad (forM, forM_, unless)
import Data.List (isInfixOf, isPrefixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory, getCurrentDirectory)
import System.Environment (getEnv, getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, latin1, withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(cwd, env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

-- | How far to compile
data Flow
  = Synthesis
  -- ^ Analysis & Synthesis (@quartus_map@)
  | FullCompile
  -- ^ The full compilation flow, including the fitter

-- | Check of a report
data Check = Check
  { checkReport :: String
  -- ^ Report, e.g. @map@ for @topEntity.map.rpt@
  , checkLine :: String
  -- ^ Start of a line of the report
  , checkContains :: String
  -- ^ What that line should contain
  }

-- | Generate HDL for a top entity named @topEntity@ in a module in
-- @shouldwork/Intel/Quartus@, compile it with Quartus, and check the reports
quartusTest ::
  -- | Module
  String ->
  -- | Device family, e.g. @MAX 10@
  String ->
  -- | Device, e.g. @10M08SAE144C8G@
  String ->
  Flow ->
  [Check] ->
  HDL ->
  TestTree
quartusTest modName family device flow checks hdl =
  testCase (show hdl) $ withSystemTempDirectory ("quartus-" <> modName) $ \tmp -> do
    here <- getCurrentDirectory
    -- Quartus generates the IP from the @.qsys@ files Clash writes: Clash
    -- shouldn't add simulation models of the IP
    env0 <- getEnvironment
    let
      hdlDir = tmp </> "hdl"
      clashEnv = Just [(k, v) | (k, v) <- env0, k /= "CLASH_INTEL_SIM_FAMILY"]
    runWith clashEnv here "clash"
      [ case hdl of { VHDL -> "--vhdl"; Verilog -> "--verilog"; SystemVerilog -> "--systemverilog" }
      , "-i" <> here </> "hdl-tests" </> "shouldwork" </> "Intel" </> "Quartus"
      , modName
      , "-fclash-hdldir", hdlDir, "-odir", tmp </> "obj", "-hidir", tmp </> "obj" ]
    files <- sort <$> listFilesRecursive hdlDir
    let
      ofExt es = filter ((`elem` es) . takeExtension) files
      -- Clash puts the types of a VHDL design in packages, which must come
      -- first
      vhdlFiles =
        filter (isInfixOf "_types.vhdl") (ofExt [".vhdl"])
          <> filter (not . isInfixOf "_types.vhdl") (ofExt [".vhdl"])
      assignments =
           [("VHDL_FILE", f) | f <- vhdlFiles]
        <> [("SYSTEMVERILOG_FILE", f) | f <- ofExt [".v", ".sv"]]
        <> [("QSYS_FILE", f) | f <- ofExt [".qsys"]]
      setup = unlines $
           [ "project_new topEntity -overwrite"
           , "set_global_assignment -name FAMILY \"" <> family <> "\""
           , "set_global_assignment -name DEVICE " <> device
           , "set_global_assignment -name TOP_LEVEL_ENTITY topEntity"
           , "set_global_assignment -name NUM_PARALLEL_PROCESSORS 1" ]
        <> [ "set_global_assignment -name " <> a <> " {" <> f <> "}" | (a, f) <- assignments ]
        <> [ "project_close" ]
    writeFile (tmp </> "setup.tcl") setup
    root <- getEnv "QUARTUS_ROOTDIR"
    let bin = root </> "bin"
    run tmp (bin </> "quartus_sh") ["-t", "setup.tcl"]
    case flow of
      Synthesis -> run tmp (bin </> "quartus_map") ["topEntity"]
      FullCompile -> run tmp (bin </> "quartus_sh") ["--flow", "compile", "topEntity"]
    forM_ checks $ \Check{..} -> do
      report <- readLatin1 (tmp </> "topEntity." <> checkReport <> ".rpt")
      let matching = filter (checkLine `isPrefixOf`) (lines report)
      unless (any (checkContains `isInfixOf`) matching) $
        assertFailure $
          "Expected a line starting with " <> show checkLine <> " containing "
            <> show checkContains <> " in the " <> checkReport <> " report, but got:\n"
            <> unlines matching
 where
  run = runWith Nothing
  runWith environment dir exe args = do
    (code, out, err) <-
      readCreateProcessWithExitCode ((proc exe args) { cwd = Just dir, env = environment }) ""
    case code of
      ExitSuccess -> pure ()
      ExitFailure c ->
        assertFailure $
          unwords (exe : args) <> " failed with exit code " <> show c <> ":\n"
            <> unlines (lastN 40 (lines out)) <> unlines (lastN 40 (lines err))

  lastN n xs = drop (length xs - n) xs

  readLatin1 f = withFile f ReadMode $ \h -> do
    hSetEncoding h latin1
    s <- hGetContents h
    length s `seq` pure s

  listFilesRecursive d = do
    es <- listDirectory d
    fmap concat $ forM es $ \e -> do
      let p = d </> e
      isDir <- doesDirectoryExist p
      if isDir then listFilesRecursive p else pure [p]
