{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell models of 'Clash.Cores.LatticeSemi.Ecp5.IO.ilvds',
'Clash.Cores.LatticeSemi.Ecp5.IO.olvds' and 'Clash.Cores.LatticeSemi.Ecp5.IO.obco'
against Yosys's simulation models, with random stimuli. Yosys's models leave
the negative outputs of @OLVDS@ and @OBCO@ undriven, so we can only compare
their positive outputs.
-}
module DifferentialBuffers where

import Clash.Explicit.Prelude

import qualified DifferentialBuffersTypes as Types

testBench :: Signal Types.Dom Bool
testBench = Types.tb $(Types.stimuli) $(Types.expected)
{-# OPAQUE testBench #-}
