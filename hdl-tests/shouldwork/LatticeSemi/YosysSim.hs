{-# LANGUAGE TemplateHaskell #-}

{- | Makes Yosys's community simulation models of Lattice primitives available
to HDL simulators in the test suite.

Yosys ships behavioural Verilog models of (some) Lattice primitives in its data
directory, e.g. @share\/yosys\/ecp5\/cells_io.vh@. The tests use them as the
reference to compare the Haskell simulation models of @clash-cores@ against.
'yosysSimModels' declares a function that passes a 'Signal' through unchanged,
but also adds a Verilog file to the generated HDL that @`include@s the given
Yosys model files, plus a VHDL package with component declarations. Using that
function somewhere in a test bench (e.g. on its @done@ signal) makes the models
part of the HDL, which then gets picked up by the simulators: IVerilog compiles
all @.v@ files in the HDL directories, Vivado reads all files listed in the
Clash manifests. GHDL can't simulate Verilog, so VHDL is tested with Vivado
only.

This needs no changes to the test harness (@clash-testsuite@), other than
passing @-ihdl-tests\/shouldwork\/LatticeSemi@ to Clash so tests can import
this module.

The data directory is taken from the environment variable @YOSYS_DATDIR@ or,
if it isn't set, from @yosys-config --datdir@. The Nix-wrapped test suite has
Yosys on its @PATH@. When running the test suite through Cabal, use something
like:

> nix shell nixpkgs#yosys -c cabal run cores-hdl-tests -- -p Ecp5

The Yosys version is recorded in a comment in the generated Verilog file.
-}
module YosysSim (yosysSimModels, yosysDataDir) where

import Prelude

import Clash.Annotations.Primitive (HDL (..), Primitive (..), hasBlackBox)
import Clash.Annotations.TopEntity (PortName (..), TopEntity (..))
import Clash.Signal (Signal, System)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.Char (isAlphaNum, isSpace)
import Data.List (dropWhileEnd)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcess)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Yosys's data directory, see module documentation.
yosysDataDir :: IO FilePath
yosysDataDir = do
  fromEnv <- lookupEnv "YOSYS_DATDIR"
  case fromEnv of
    Just dir -> pure dir
    Nothing -> do
      res <- try (readProcess "yosys-config" ["--datdir"] "")
      case res of
        Right dir -> pure (trim dir)
        Left (e :: SomeException) ->
          fail $
            "Could not find Yosys's simulation models: set YOSYS_DATDIR or put "
              <> "yosys-config on the PATH (" <> show e <> ")"

{- | Declare a function with the given name and type

> Signal System Bool -> Signal System Bool

that behaves like 'id', but renders to HDL that includes the given Yosys
simulation model files. The files are given relative to Yosys's data
directory, e.g. @["ecp5\/common_sim.vh", "ecp5\/cells_io.vh"]@.

Some model files include others by a relative path (e.g. ECP5's
@common_sim.vh@ includes @cells_io.vh@), which the simulators can't resolve as
they don't know Yosys's data directory. Such includes are guarded by
@NO_INCLUDES@ in Yosys's models, which the generated file defines while
including the given files. List all files the test needs instead.

VHDL needs component declarations for the Verilog models. Wrappers in
@clash-cores@ import them from a vendor library, e.g. @ecp5u.components@. The
last argument is the name of that library and a VHDL file, relative to the
directory of the module calling 'yosysSimModels', that declares the components.
The declared function is a separate top entity with that library's name, which
makes Vivado compile the VHDL file into a library with that name.
-}
yosysSimModels ::
  -- | Name of the function to declare
  String ->
  -- | Yosys simulation models, relative to Yosys's data directory
  [FilePath] ->
  -- | VHDL library and the file with its component declarations
  (String, FilePath) ->
  Q [Dec]
yosysSimModels nm files (vhdlLib, vhdlFile) = do
  datDir <- runIO yosysDataDir
  let paths = map (datDir </>) files
  runIO $ forM_ paths $ \p -> do
    exists <- doesFileExist p
    unless exists (fail ("Yosys simulation model not found: " <> p))
  version <- runIO $ do
    res <- try (readProcess "yosys" ["-V"] "")
    pure $ case res of
      Right v -> trim v
      Left (_ :: SomeException) -> "unknown version"

  loc <- location
  let vhdlPath = takeDirectory (loc_filename loc) </> vhdlFile
  addDependentFile vhdlPath
  vhdlComponents <- runIO (readFile vhdlPath)

  let
    name = mkName nm
    primName = mkName (nm <> "Primitive")
    primFullName = loc_module loc <> "." <> nameBase primName
    -- Every test bench using the primitive gets a copy of this file. IVerilog
    -- compiles the files of all test benches of a test together, so guard
    -- against defining the models twice.
    guard = "YOSYS_SIM_MODELS_" <> map (\c -> if isAlphaNum c then c else '_') nm
    verilogInclude =
      unlines $
        [ "// Simulation models from " <> datDir
        , "// " <> version
        , "`ifndef " <> guard
        , "`define " <> guard
        , "`define NO_INCLUDES"
        ]
          <> ["`include \"" <> p <> "\"" | p <- paths]
          <> ["`undef NO_INCLUDES", "`endif"]
    -- YAML's double-quoted strings understand the escapes 'show' uses for
    -- ASCII strings
    include incName ext contents =
      [ "  - name: " <> incName
      , "    extension: " <> ext
      , "    template: " <> show contents
      ]
    yaml template incs =
      unlines $
        [ "BlackBox:"
        , "  name: " <> primFullName
        , "  kind: Declaration"
        , "  template: " <> show template
        , "  includes:"
        ]
          <> concat incs
    verilogYaml =
      yaml
        "assign ~RESULT = ~ARG[0];"
        [include "yosys_sim_models" "v" verilogInclude]
    vhdlYaml =
      yaml
        "~RESULT <= ~ARG[0];"
        [ include "yosys_sim_models" "v" verilogInclude
        , include "components" "vhdl" vhdlComponents
        ]

  sig <- [t|Signal System Bool -> Signal System Bool|]
  idBody <- [|id|]
  topBody <- varE primName
  bb <- [|hasBlackBox|]
  annVerilog <- [|InlineYamlPrimitive [Verilog, SystemVerilog] $(lift verilogYaml)|]
  annVhdl <- [|InlineYamlPrimitive [VHDL] $(lift vhdlYaml)|]
  synth <-
    [| Synthesize
        { t_name = $(lift vhdlLib)
        , t_inputs = [PortName "i"]
        , t_output = PortName "o"
        } |]

  pure
    [ SigD primName sig
    , ValD (VarP primName) (NormalB idBody) []
    , PragmaD (OpaqueP primName)
    , PragmaD (AnnP (ValueAnnotation primName) bb)
    , PragmaD (AnnP (ValueAnnotation primName) annVerilog)
    , PragmaD (AnnP (ValueAnnotation primName) annVhdl)
    , SigD name sig
    , ValD (VarP name) (NormalB topBody) []
    , PragmaD (OpaqueP name)
    , PragmaD (AnnP (ValueAnnotation name) synth)
    ]
