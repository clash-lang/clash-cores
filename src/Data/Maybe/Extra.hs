{-|
Module      : Data.Maybe.Extra
Description : Utility module, only for very small util functions.
-}
module Data.Maybe.Extra
  ( toMaybe
  ) where

import Clash.Prelude


-- | Wrap a value in a Just if True
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing
