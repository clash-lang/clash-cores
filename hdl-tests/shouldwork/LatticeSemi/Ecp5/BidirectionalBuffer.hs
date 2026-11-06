{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell model of 'Clash.Cores.LatticeSemi.Ecp5.IO.bidirectionalBuffer'
(@BB@, @BBPU@ and @BBPD@) against Yosys's simulation models, with random
stimuli for the buffer and another driver of the pad. See 'Types.expected' for
why we don't compare cycles in which nothing drives the pad.
-}
module BidirectionalBuffer where

import Clash.Explicit.Prelude
import Data.Proxy

import qualified BidirectionalBufferTypes as Types

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbFloating :: Signal Types.Dom Bool
tbFloating =
  Types.tb (Proxy @'Floating) $(Types.stimuli) $(Types.expected (Proxy @'Floating))
{-# ANN tbFloating (TestBench 'topEntity) #-}

tbPullUp :: Signal Types.Dom Bool
tbPullUp =
  Types.tb (Proxy @'PullUp) $(Types.stimuli) $(Types.expected (Proxy @'PullUp))
{-# ANN tbPullUp (TestBench 'topEntity) #-}

tbPullDown :: Signal Types.Dom Bool
tbPullDown =
  Types.tb (Proxy @'PullDown) $(Types.stimuli) $(Types.expected (Proxy @'PullDown))
{-# ANN tbPullDown (TestBench 'topEntity) #-}
