{-# LANGUAGE TemplateHaskell #-}

{- | Makes Intel's simulation libraries @altera_mf@ and @altera_primitives@
available to the HDL simulators of the test suite, without changes to the test
harness (@clash-testsuite@).

Quartus ships plain Verilog and VHDL simulation models of its primitives and
megafunctions in @quartus\/eda\/sim_lib@. The test benches compare the Haskell
simulation models of @clash-cores@ against them. 'intelSimLibraries' declares
two top entities, named after the VHDL libraries Quartus puts the models in:

  * @altera_mf@: @altera_mf_components.vhd@ and @altera_mf.vhd@, for the
    megafunctions (@altsyncram@, @dcfifo@, ...)
  * @altera@: @altera_primitives_components.vhd@ and @altera_primitives.vhd@,
    for the primitives (@alt_iobuf@, @opndrn@, ...)

Both behave like 'id', but their black boxes add the models to the generated
HDL. For VHDL, the models are copied into the HDL output (see
'writeVhdlModels'), so that Vivado
compiles them into a library with the name of the top entity, which is the
library the wrappers in @clash-cores@ import (e.g.
@library altera_mf; use altera_mf.altera_mf_components.all;@). For Verilog, a
file that @`include@s the Verilog models is added to the HDL output; Icarus
Verilog compiles all @.v@ files in the HDL directories, and Vivado reads all
files listed in the Clash manifests.

A test bench passes its result through 'IntelTestCommon.withIntelSim', which
uses both top entities, to make the models part of its HDL.

GHDL is not supported: it needs @-fsynopsys@ and @-fexplicit@ to compile
@altera_mf.vhd@ (it uses @ieee.std_logic_arith@ and @ieee.std_logic_unsigned@),
and the test harness doesn't pass those. So VHDL is tested with Vivado only.

The simulation libraries are taken from the directory in the environment
variable @INTEL_SIM_LIB@ or, if it isn't set, from
@$QUARTUS_ROOTDIR\/eda\/sim_lib@, or else from the default Quartus Prime Lite
21.1 installation on our machines. The Quartus version is recorded in a comment
in the generated HDL.
-}
module IntelSimLib (intelSimLibraries, intelSimLibrary, simLibDir) where

import Prelude

import Clash.Annotations.Primitive (HDL (..), Primitive (..), hasBlackBox)
import Clash.Annotations.TopEntity (PortName (..), TopEntity (..))
import Clash.Sized.BitVector (Bit)
import Control.Monad (forM_, unless)
import Data.Bits (xor)
import qualified Data.ByteString.Char8 as B
import Data.Char (isPrint)
import Data.Word (Word64)
import Numeric (showHex)
import System.IO (hClose, openTempFile)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , getTemporaryDirectory, renameFile )
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Default location of the simulation libraries on our machines
defaultSimLibDir :: FilePath
defaultSimLibDir = "/opt/tools/Intel/intelFPGA_lite/21.1/quartus/eda/sim_lib"

-- | Directory with Intel's simulation libraries, see module documentation
simLibDir :: IO FilePath
simLibDir = do
  fromEnv <- lookupEnv "INTEL_SIM_LIB"
  quartusRoot <- lookupEnv "QUARTUS_ROOTDIR"
  let dir = case (fromEnv, quartusRoot) of
        (Just d, _) -> d
        (Nothing, Just r) -> r </> "eda" </> "sim_lib"
        (Nothing, Nothing) -> defaultSimLibDir
  exists <- doesDirectoryExist dir
  unless exists $
    fail $
      "Could not find Intel's simulation libraries in " <> dir
        <> ": set INTEL_SIM_LIB or QUARTUS_ROOTDIR"
  pure dir

{- | Read a model file, replacing the few non-ASCII characters (in comments)
by spaces and dropping carriage returns. Clash requires black box templates
and the files it copies to be valid UTF-8.
-}
readModel :: FilePath -> IO String
readModel p = do
  exists <- doesFileExist p
  unless exists (fail ("Intel simulation model not found: " <> p))
  contents <- B.readFile p
  pure (map sanitize (filter (/= '\r') (B.unpack contents)))
 where
  sanitize c
    | c == '\n' || c == '\t' = c
    | isPrint c && c < '\DEL' = c
    | otherwise = ' '

-- | The line with the Quartus version in the header of a model file
quartusVersion :: String -> String
quartusVersion contents =
  case filter (\l -> take 3 (words l) == ["//", "Quartus", "Prime"]) (take 40 (lines contents)) of
    l : _ -> unwords (drop 3 (words l))
    [] -> "// unknown Quartus version"

{- | Declare the top entities @altera_mf@ and @altera@, see the module
documentation, as functions with the given names and type @'Bit' -> 'Bit'@.
-}
intelSimLibraries ::
  -- | Name of the function for @altera_mf@
  String ->
  -- | Name of the function for @altera@
  String ->
  Q [Dec]
intelSimLibraries mfName primName = do
  dir <- runIO simLibDir
  mf <-
    library dir mfName "altera_mf" "altera_mf.v" ["altera_mf_components.vhd", "altera_mf.vhd"]
  prim <-
    library dir primName "altera" "altera_primitives.v"
      ["altera_primitives_components.vhd", "altera_primitives.vhd"]
  pure (mf <> prim)

{- | Declare a top entity for a VHDL library with the given name, as a function
with the given name and type @'Bit' -> 'Bit'@, like 'intelSimLibraries' does
for @altera_mf@ and @altera@. Used for the atoms of a device family, e.g.
@intelSimLibrary \"cycloneiveLib\" \"cycloneive\" \"cycloneive_atoms.v\"
[\"cycloneive_atoms.vhd\", \"cycloneive_components.vhd\"]@.
-}
intelSimLibrary ::
  -- | Name of the function
  String ->
  -- | Name of the VHDL library
  String ->
  -- | Verilog model, relative to the simulation library directory
  FilePath ->
  -- | VHDL models, relative to the simulation library directory
  [FilePath] ->
  Q [Dec]
intelSimLibrary nm lib verilogFile vhdlFiles = do
  dir <- runIO simLibDir
  library dir nm lib verilogFile vhdlFiles

library :: FilePath -> String -> String -> FilePath -> [FilePath] -> Q [Dec]
library dir nm lib verilogFile vhdlFiles = do
  let verilogPath = dir </> verilogFile
  addDependentFile verilogPath
  version <- runIO (quartusVersion <$> readModel verilogPath)
  forM_ vhdlFiles (addDependentFile . (dir </>))
  vhdlPath <- runIO (writeVhdlModels dir lib version vhdlFiles)
  loc <- location

  let
    name = mkName nm
    primName = mkName (nm <> "Primitive")
    primFullName = loc_module loc <> "." <> nameBase primName
    -- Every test bench using the models gets a copy of this file, and Icarus
    -- Verilog compiles the files of all test benches of a test together. So
    -- guard against defining the models twice.
    guard = "INTEL_SIM_LIB_" <> lib
    verilogInclude =
      unlines
        [ "// Intel simulation models from " <> dir
        , "// Quartus Prime " <> version
        , "`ifndef " <> guard
        , "`define " <> guard
        , "`include \"" <> verilogPath <> "\""
        , "`endif"
        ]
    -- YAML's double-quoted strings understand the escapes 'show' uses for
    -- (printable) ASCII strings
    verilogYaml =
      unlines
        [ "BlackBox:"
        , "  name: " <> primFullName
        , "  kind: Declaration"
        , "  template: " <> show "assign ~RESULT = ~ARG[1];"
        , "  includes:"
        , "  - name: " <> lib <> "_models"
        , "    extension: v"
        , "    template: " <> show verilogInclude
        ]
    -- The VHDL models are too large to put in a black box template: Clash
    -- would need tens of gigabytes of memory to process it. Instead, '~FILE'
    -- copies them into the HDL output.
    vhdlYaml =
      unlines
        [ "BlackBox:"
        , "  name: " <> primFullName
        , "  kind: Declaration"
        , "  template: "
            <> show
              ( "-- Intel simulation models (Quartus Prime " <> version <> "): ~FILE[~LIT[0]]\n"
                  <> "~RESULT <= ~ARG[1];"
              )
        ]

  sig <- [t|Bit -> Bit|]
  primSig <- [t|String -> Bit -> Bit|]
  primBody <- [|\_path -> id|]
  topBody <- [|$(varE primName) $(lift vhdlPath)|]
  bb <- [|hasBlackBox|]
  annVerilog <- [|InlineYamlPrimitive [Verilog, SystemVerilog] $(lift verilogYaml)|]
  annVhdl <- [|InlineYamlPrimitive [VHDL] $(lift vhdlYaml)|]
  synth <-
    [|
      Synthesize
        { t_name = $(lift lib)
        , t_inputs = [PortName "i"]
        , t_output = PortName "o"
        }
      |]

  pure
    [ SigD primName primSig
    , ValD (VarP primName) (NormalB primBody) []
    , PragmaD (OpaqueP primName)
    , PragmaD (AnnP (ValueAnnotation primName) bb)
    , PragmaD (AnnP (ValueAnnotation primName) annVerilog)
    , PragmaD (AnnP (ValueAnnotation primName) annVhdl)
    , SigD name sig
    , ValD (VarP name) (NormalB topBody) []
    , PragmaD (OpaqueP name)
    , PragmaD (AnnP (ValueAnnotation name) synth)
    ]

{- | Write the given VHDL model files, sanitized (see 'readModel') and
concatenated, to a file @<lib>_models.vhdl@ in a directory in the temporary directory.
The directory name contains a hash of the contents, and the file is written
atomically, so concurrent Clash runs don't trip over each other. Returns the
path of the file.
-}
writeVhdlModels :: FilePath -> String -> String -> [FilePath] -> IO FilePath
writeVhdlModels dir lib version files = do
  models <- mapM (readModel . (dir </>)) files
  let
    contents =
      unlines
        ( [ "-- Intel simulation models from " <> dir
          , "-- Quartus Prime " <> version
          ]
            <> models
        )
    bytes = B.pack contents
  tmp <- getTemporaryDirectory
  let
    outDir = tmp </> ("clash-cores-intel-sim-lib-" <> showHex (fnv1a bytes) "")
    outFile = outDir </> (lib <> "_models.vhdl")
  createDirectoryIfMissing True outDir
  exists <- doesFileExist outFile
  unless exists $ do
    (tmpFile, h) <- openTempFile outDir (lib <> "_models.vhdl.tmp")
    B.hPut h bytes
    hClose h
    renameFile tmpFile outFile
  pure outFile

-- | 64-bit FNV-1a hash
fnv1a :: B.ByteString -> Word64
fnv1a = B.foldl' (\h c -> (h `xor` fromIntegral (fromEnum c)) * 1099511628211) 14695981039346656037
