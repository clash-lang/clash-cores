{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests on the primitive definitions ('InlineYamlPrimitive' annotations) of this
  package.

  A @BlackBoxHaskell@ primitive should declare which of its arguments it uses:
  if it does not, clash-lib assumes /all/ of them are used and will therefore
  normalize arguments that only exist at the Haskell level - class dictionaries,
  @HasCallStack@ - producing warnings for the non-representable ones. See
  @WARNINGS_FINDINGS.md@ in the root of this repository.
-}

{-# LANGUAGE TemplateHaskell #-}

module Test.Primitives (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import Clash.Cores.Xilinx.DcFifo (dcFifo)
import Clash.Cores.Xilinx.DcFifo.Internal.BlackBoxes (dcFifoUsedArguments)
import Clash.Cores.Xilinx.Ila.Internal (ilaIgnoredArguments)
import Clash.Cores.Xilinx.Vio.Internal.BlackBoxes (vioProbeIgnoredArguments)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (inst, instWithXilinxWizard#)
import Clash.Primitives.Types (UnresolvedPrimitive, UsedArguments(..))
import Clash.Primitives.Util (decodeOrErrYaml)

import qualified Clash.Primitives.Types as P
import qualified Data.ByteString.Lazy.Char8 as LazyByteString

import Test.Primitives.TH (blackBoxHaskellBlocks, primitiveYamls)

tests :: TestTree
tests = testGroup "Primitives"
  [ testCase
      "Every BlackBoxHaskell declares its used or ignored arguments"
      case_declaresArguments
  , testCase
      "dcFifo's primitive definition uses the same arguments as its template functions"
      case_dcFifoUsedArguments
  , testCase
      "The polyvariadic primitives only ignore constraint arguments"
      case_polyvariadicIgnoredArguments
  ]

-- | The YAML of the primitives in this package that we can get at, see
-- 'primitiveYamls'
primitiveDefinitions :: [(String, String)]
primitiveDefinitions =
  $(primitiveYamls ['dcFifo, 'inst, 'instWithXilinxWizard#])

-- | Every @BlackBoxHaskell@ primitive definition in this package's sources, as
-- @(file, line, yaml)@
blackBoxHaskellPrimitives :: [(FilePath, Int, [String])]
blackBoxHaskellPrimitives = $(blackBoxHaskellBlocks "src")

-- | Parse the primitive definition of the given primitive the way clash-lib
-- does, and yield the arguments it declares to use
usedArgumentsOf :: String -> UsedArguments
usedArgumentsOf name =
  case [yaml | (nm, yaml) <- primitiveDefinitions, nm == name] of
    [yaml] ->
      let prim = decodeOrErrYaml name (LazyByteString.pack yaml) :: UnresolvedPrimitive
      in case prim of
           P.BlackBoxHaskell{} -> P.usedArguments prim
           _ -> error (name <> " is not a BlackBoxHaskell:\n" <> show prim)
    [] -> error ("No 'InlineYamlPrimitive' annotation found for " <> name)
    yamls ->
      error (show (length yamls) <> " primitive definitions found for " <> name)

-- | Guards against new primitives silently relying on clash-lib's default,
-- which is that all arguments are used
case_declaresArguments :: Assertion
case_declaresArguments = do
  assertBool
    "No BlackBoxHaskell primitives found in 'src'; is the scanner broken?"
    (not (null blackBoxHaskellPrimitives))

  sequence_
    [ assertBool (message file line yaml) (any declares yaml)
    | (file, line, yaml) <- blackBoxHaskellPrimitives
    ]
 where
  declares line =
    "usedArguments:" `elem` words line || "ignoredArguments:" `elem` words line

  message file line yaml = unlines $
    [ file <> ":" <> show line <> ": this BlackBoxHaskell does not declare"
    , "'usedArguments' or 'ignoredArguments', making clash-lib assume all its"
    , "arguments are used. Its constraint arguments will therefore survive"
    , "normalization, which is wasteful at best and warning-worthy at worst."
    , "Primitive definition:"
    , ""
    ] <> map ("  " <>) yaml

-- | 'dcFifo' declares its used arguments both in its primitive definition (used
-- during normalization) and in its template functions (used during netlist
-- generation). This makes sure the two do not drift apart.
case_dcFifoUsedArguments :: Assertion
case_dcFifoUsedArguments =
  usedArgumentsOf (show 'dcFifo) @?= UsedArguments dcFifoUsedArguments

-- | The number of arguments of the polyvariadic primitives depends on their
-- instantiation, hence they declare the arguments they /do not/ use: their
-- constraints. Note that none of these lists may grow to cover an argument that
-- the black box function parses, such as an @IlaConfig@ or a @KnownNat@ a
-- 'Clash.Cores.Xilinx.Xpm.Cdc.Internal.XilinxWizard' is indexed by.
case_polyvariadicIgnoredArguments :: Assertion
case_polyvariadicIgnoredArguments = do
  -- KnownDomain, Ila, 1 <= n
  ilaIgnoredArguments @?= [0, 1, 2]
  -- KnownDomain, Vio
  vioProbeIgnoredArguments @?= [0, 1]
  -- Inst
  usedArgumentsOf (show 'inst) @?= IgnoredArguments [0]
  -- Inst, but /not/ the KnownNat: 'instWithXilinxWizardBBF' reads it
  usedArgumentsOf (show 'instWithXilinxWizard#) @?= IgnoredArguments [0]
