{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Template Haskell helpers for "Test.Primitives".
-}

module Test.Primitives.TH
  ( primitiveYamls
  , blackBoxHaskellBlocks
  ) where

import Prelude

import Control.Monad (filterM, forM)
import Data.List (isInfixOf, isSuffixOf, sort)
import Language.Haskell.TH (Exp, Name, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.IO (readFile')

import Clash.Annotations.Primitive (Primitive(InlineYamlPrimitive))

import qualified Language.Haskell.TH as TH
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

-- | The YAML of every 'InlineYamlPrimitive' annotation on the given names, as
-- @[(primitive name, yaml)]@. This is the very same YAML Clash parses when it
-- compiles a design using these primitives.
--
-- Note that annotations can only be reified for names that are in scope, hence
-- primitives that are not exported (such as
-- @Clash.Cores.Xilinx.Ila.ila#@) cannot be tested this way.
primitiveYamls :: [Name] -> Q Exp
primitiveYamls names = do
  yamls <- forM names $ \nm -> do
    prims <- TH.reifyAnnotations (TH.AnnLookupName nm)
    pure [(show nm, yaml) | InlineYamlPrimitive _hdls yaml <- prims]
  lift (concat yamls)

-- | Every @BlackBoxHaskell@ primitive definition in the Haskell sources found
-- (recursively) in the given directory, as @[(file, line, block)]@ where
-- @block@ are the lines of the primitive's YAML.
--
-- The sources are read at compile time, so tests using this do not depend on
-- the working directory they are run in. A block is taken to run from the line
-- mentioning @BlackBoxHaskell:@ up to (excluding) the line closing the
-- quasiquote holding it.
blackBoxHaskellBlocks :: FilePath -> Q Exp
blackBoxHaskellBlocks dir = do
  files <- runIO (haskellSources dir)
  mapM_ addDependentFile files
  blocks <- runIO (concat <$> mapM blocksOf files)
  lift blocks
 where
  blocksOf :: FilePath -> IO [(FilePath, Int, [String])]
  blocksOf file = do
    ls <- lines <$> readFile' file
    pure
      [ (file, n, takeWhile (not . isInfixOf "|]") (drop (n - 1) ls))
      | (n, l) <- zip [1 ..] ls
      , "BlackBoxHaskell:" `elem` words l
      ]

-- | All Haskell sources in the given directory, recursively
haskellSources :: FilePath -> IO [FilePath]
haskellSources dir = do
  entries <- map (dir FilePath.</>) . sort <$> Directory.listDirectory dir
  dirs <- filterM Directory.doesDirectoryExist entries
  nested <- mapM haskellSources dirs
  pure (filter (".hs" `isSuffixOf`) entries <> concat nested)
