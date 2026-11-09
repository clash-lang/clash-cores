{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Reference for the tests of "Clash.Cores.Intel.ComplexMultiplier".

Intel's simulation libraries have no model of @altmult_complex@. When Quartus
synthesizes a design, its @clearbox@ tool generates a netlist of the
megafunction for the instance's parameters and device family, built from the
family's atoms (embedded multipliers and logic cells). These netlists are
plain Verilog, and so are the atom models of Cyclone IV E, MAX 10 and
Cyclone 10 LP, so they make a reference: simulated as black boxes, they
behave like the hardware Quartus builds.

'referenceFiles' runs @clearbox@ at compile time (from Template Haskell), for
the device family in @CLASH_INTEL_SIM_FAMILY@ (see @hdl-tests/Main.hs@) and
the given configurations. Next to the netlists, it writes:

* @altmult_complex.v@, a Verilog module @altmult_complex@ with the parameters
  and ports of the megafunction, which instantiates the netlist matching its
  parameters. It stops the simulation with an error if there is none.

* @altera_mf_components.vhd@, a package @altera_mf_components@ with the
  component declaration of @altmult_complex@ (as in Quartus's synthesis
  library, and the user guide). The wrapper's VHDL uses this component;
  Vivado's simulator binds it to the Verilog module.

The test module adds these files, the atom models of the family, and
@altera_primitives.v@ to its HDL with 'Clash.Cores.Intel.Internal.simLibraryFiles',
in a top entity named @altera_mf@, so that Vivado compiles them into a
library named @altera_mf@. The test benches must not use
"IntelSimLib"'s @altera_mf@ library as well.

The files are cached under @$CLASH_INTEL_SIM_CACHE/altmult_complex@ (or
@$XDG_CACHE_HOME\/clash-cores\/altmult_complex@), in a directory named after
a hash of the family, the Quartus installation and the configurations.
-}
module ComplexMultiplierSim
  ( RefConfig (..)
  , referenceFiles
  ) where

import Prelude

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (intercalate, isPrefixOf)
import Data.Word (Word64)
import Language.Haskell.TH (ExpQ, runIO)
import Numeric (showHex)
import System.Directory
  ( XdgDirectory (XdgCache), createDirectoryIfMissing, doesDirectoryExist
  , doesFileExist, getXdgDirectory, removeDirectoryRecursive, renameDirectory )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Process (proc, readCreateProcessWithExitCode, cwd)

import Clash.Sized.Vector (listToVecTH)

-- | Parameters of an @altmult_complex@ instance
data RefConfig = RefConfig
  { widthA :: Int
  , widthB :: Int
  , widthResult :: Int
  , representationA :: String
  -- ^ @SIGNED@ or @UNSIGNED@
  , representationB :: String
  , pipeline :: Int
  , implementationStyle :: String
  -- ^ @AUTO@, @CANONICAL@ or @CONVENTIONAL@
  }
  deriving (Show)

-- | Device family for @clearbox@ and the name of its atoms library
familyAtoms :: String -> IO (String, String)
familyAtoms fam
  | "MAX 10" `isPrefixOf` fam = pure ("MAX 10", "fiftyfivenm")
  | fam == "Cyclone IV E" = pure (fam, "cycloneive")
  | fam == "Cyclone 10 LP" = pure (fam, "cyclone10lp")
  | otherwise =
      fail $
        "ComplexMultiplierSim: no plain atom models for " <> show fam
          <> ", use MAX 10, Cyclone IV E or Cyclone 10 LP"

-- | FNV-1a
hash :: String -> Word64
hash = foldl (\h c -> (h `xor` fromIntegral (ord c)) * 1099511628211) 14695981039346656037

-- | Generate the reference files for the configurations, and return a
-- @Vec n String@ of their paths, for 'Clash.Cores.Intel.Internal.simLibraryFiles'
referenceFiles :: [RefConfig] -> ExpQ
referenceFiles cfgs = do
  files <- runIO (generate cfgs)
  listToVecTH files

generate :: [RefConfig] -> IO [FilePath]
generate cfgs = do
  root <- lookupEnv "QUARTUS_ROOTDIR"
  quartus <- case root of
    Just r@(_ : _) -> pure r
    _ -> fail "ComplexMultiplierSim: QUARTUS_ROOTDIR is not set"
  famEnv <- lookupEnv "CLASH_INTEL_SIM_FAMILY"
  (family, atoms) <- familyAtoms (maybe "MAX 10" id famEnv)
  cacheEnv <- lookupEnv "CLASH_INTEL_SIM_CACHE"
  cacheRoot <- case cacheEnv of
    Just c@(_ : _) -> pure (c </> "altmult_complex")
    _ -> getXdgDirectory XdgCache ("clash-cores" </> "altmult_complex")
  createDirectoryIfMissing True cacheRoot
  let
    key = showHex (hash (show (family, quartus, cfgs, version))) ""
    dir = cacheRoot </> key
    names = ["altmult_complex_ref" <> show i | i <- [0 .. length cfgs - 1]]
    files =
      [dir </> "altera_mf_components.vhd", dir </> "altmult_complex.v"]
        <> [dir </> n <> ".v" | n <- names]
        <> [atoms <> "_atoms.v", "altera_primitives.v"]
  done <- doesDirectoryExist dir
  unless done $ do
    tmp <- createTempDirectory cacheRoot "tmp"
    forM_ (zip names cfgs) $ \(n, c) -> clearbox quartus family tmp n c
    writeFile (tmp </> "altmult_complex.v") (dispatcher family (zip names cfgs))
    writeFile (tmp </> "altera_mf_components.vhd") componentPackage
    -- Another Clash run may have generated the same files concurrently
    r <- try (renameDirectory tmp dir) :: IO (Either SomeException ())
    case r of
      Right () -> pure ()
      Left _ -> removeDirectoryRecursive tmp
  pure files
 where
  -- Bump to regenerate cached files after changing this module
  version = 2 :: Int

-- | Run @clearbox@ to generate the netlist of a configuration, in module @name@
clearbox :: FilePath -> String -> FilePath -> String -> RefConfig -> IO ()
clearbox quartus family dir name RefConfig{..} = do
  let
    args =
      [ "altmult_complex"
      , "DEVICE_FAMILY=" <> family
      , "WIDTH_A=" <> show widthA
      , "WIDTH_B=" <> show widthB
      , "WIDTH_RESULT=" <> show widthResult
      , "REPRESENTATION_A=" <> representationA
      , "REPRESENTATION_B=" <> representationB
      , "PIPELINE=" <> show pipeline
      , "IMPLEMENTATION_STYLE=" <> implementationStyle
      , "CBX_SINGLE_OUTPUT_FILE=ON"
      , "CBX_FILE=" <> name <> ".v"
      , "dataa_real", "dataa_imag", "datab_real", "datab_imag"
      , "result_real", "result_imag"
      ]
        <> (if pipeline > 0 then ["clock", "aclr", "ena"] else [])
  (code, out, err) <-
    readCreateProcessWithExitCode
      (proc (quartus </> "bin" </> "clearbox") args){cwd = Just dir}
      ""
  ok <- doesFileExist (dir </> name <> ".v")
  when (code /= ExitSuccess || not ok || "Error" `isInfix` (out <> err)) $
    fail ("ComplexMultiplierSim: clearbox failed:\n" <> unwords args <> "\n" <> out <> err)
 where
  isInfix n h = any (n `isPrefixOf`) (tails' h)
  tails' [] = [[]]
  tails' xs@(_ : t) = xs : tails' t

-- | Verilog module @altmult_complex@ that instantiates the netlist matching
-- its parameters
dispatcher :: String -> [(String, RefConfig)] -> String
dispatcher family refs =
  unlines $
    [ "// Generated by hdl-tests/shouldwork/Intel/ComplexMultiplierSim.hs for " <> family
    , "// Configurations:"
    ]
      <> ["//   " <> n <> ": " <> show c | (n, c) <- refs]
      <> [ "module altmult_complex #("
         , "  parameter intended_device_family = \"unused\","
         , "  parameter implementation_style = \"AUTO\","
         , "  parameter pipeline = 4,"
         , "  parameter representation_a = \"SIGNED\","
         , "  parameter representation_b = \"SIGNED\","
         , "  parameter width_a = 1,"
         , "  parameter width_b = 1,"
         , "  parameter width_result = 1,"
         , "  parameter lpm_hint = \"UNUSED\","
         , "  parameter lpm_type = \"altmult_complex\""
         , ") ("
         , "  input wire aclr,"
         , "  input wire clock,"
         , "  input wire complex,"
         , "  input wire [width_a-1:0] dataa_imag,"
         , "  input wire [width_a-1:0] dataa_real,"
         , "  input wire [width_b-1:0] datab_imag,"
         , "  input wire [width_b-1:0] datab_real,"
         , "  input wire ena,"
         , "  output wire [width_result-1:0] result_imag,"
         , "  output wire [width_result-1:0] result_real,"
         , "  input wire sclr"
         , ");"
         , "// The netlists declare clock and aclr as tri0, and ena as tri1. Vivado's"
         , "// simulator resolves these pulls with the values driven from VHDL to x, so"
         , "// drive the netlists from separate nets."
         , "wire clock_b, aclr_b, ena_b;"
         , "assign clock_b = clock;"
         , "assign aclr_b = aclr;"
         , "assign ena_b = ena;"
         , "generate"
         ]
      <> zipWith instance_ [0 :: Int ..] refs
      <> [ "  else begin : no_reference"
         , "    initial begin"
         , "      $display(\"Error: no reference netlist for altmult_complex with width_a=%0d width_b=%0d width_result=%0d pipeline=%0d %s %s %s\","
         , "               width_a, width_b, width_result, pipeline, representation_a, representation_b, implementation_style);"
         , "      $finish;"
         , "    end"
         , "  end"
         , "endgenerate"
         , "endmodule"
         ]
 where
  instance_ i (n, RefConfig{..}) =
    (if i == 0 then "  if (" else "  else if (")
      <> intercalate " && "
        [ "width_a == " <> show widthA
        , "width_b == " <> show widthB
        , "width_result == " <> show widthResult
        , "pipeline == " <> show pipeline
        , "representation_a == " <> show representationA
        , "representation_b == " <> show representationB
        , "implementation_style == " <> show implementationStyle
        ]
      <> ") begin : ref" <> show i <> "\n"
      <> "    " <> n <> " u ("
      <> intercalate ", "
        ( [ ".dataa_real(dataa_real)", ".dataa_imag(dataa_imag)"
          , ".datab_real(datab_real)", ".datab_imag(datab_imag)"
          , ".result_real(result_real)", ".result_imag(result_imag)" ]
            <> (if pipeline > 0 then [".clock(clock_b)", ".aclr(aclr_b)", ".ena(ena_b)"] else [])
        )
      <> ");\n  end"

-- | Package with the component declaration of @altmult_complex@, as in the
-- user guide
componentPackage :: String
componentPackage =
  unlines
    [ "-- Generated by hdl-tests/shouldwork/Intel/ComplexMultiplierSim.hs"
    , "library ieee;"
    , "use ieee.std_logic_1164.all;"
    , ""
    , "package altera_mf_components is"
    , "  component altmult_complex"
    , "    generic ("
    , "      intended_device_family : string := \"unused\";"
    , "      implementation_style : string := \"AUTO\";"
    , "      pipeline : natural := 4;"
    , "      representation_a : string := \"SIGNED\";"
    , "      representation_b : string := \"SIGNED\";"
    , "      width_a : natural;"
    , "      width_b : natural;"
    , "      width_result : natural;"
    , "      lpm_hint : string := \"UNUSED\";"
    , "      lpm_type : string := \"altmult_complex\""
    , "    );"
    , "    port ("
    , "      aclr : in std_logic := '0';"
    , "      clock : in std_logic := '0';"
    , "      complex : in std_logic := '1';"
    , "      dataa_imag : in std_logic_vector(width_a-1 downto 0);"
    , "      dataa_real : in std_logic_vector(width_a-1 downto 0);"
    , "      datab_imag : in std_logic_vector(width_b-1 downto 0);"
    , "      datab_real : in std_logic_vector(width_b-1 downto 0);"
    , "      ena : in std_logic := '1';"
    , "      result_imag : out std_logic_vector(width_result-1 downto 0);"
    , "      result_real : out std_logic_vector(width_result-1 downto 0);"
    , "      sclr : in std_logic := '0'"
    , "    );"
    , "  end component;"
    , "end package;"
    ]
