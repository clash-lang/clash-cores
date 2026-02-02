{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, (>>=))
import qualified Clash.Main
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= Clash.Main.defaultMain
