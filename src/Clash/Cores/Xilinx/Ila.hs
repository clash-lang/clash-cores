{-|
Copyright  :  (C) 2023, Google Inc,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for [Xilinx Integrated Logic Analyzer v6.2](https://docs.xilinx.com/v/u/en-US/pg172-ila).
An Integrated Logic Analyzer (ILA) is a feature provided by Xilinx in its design
tools, notably Vivado, that allows designers to debug their FPGA logic in
real-time. It stores the signals it samples in a ring buffer, allowing users to
see values before and after a trigger point.

It is necessary to read the product guide linked above in order to effectively
use the IP. Clash simulation is not applicable for this IP.

When using the generated ILAs make sure you have set the correct JTAG clock speed:
[/"For non-Versal architectures, if your design contains debug cores, ensure that the JTAG clock is 2.5 times slower than the debug hub clock."/](https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2022_2/ug908-vivado-programming-debugging.pdf)

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns#-}

-- See [Note: eta port names for trueDualPortBlockRam]
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}

module Clash.Cores.Xilinx.Ila
  ( ila
  , ilaWith

  , probe
  , probeWith
  , dataProbe
  , triggerProbe

  , probeTh
  , probeWithTh
  , dataProbeTh
  , triggerProbeTh

  -- * Config
  , IlaConfig(..)
  , ilaConfig
  , ProbeConfig(..)
  , probeConfig
  , ProbeType(..)
  , Depth(..)

  -- * Utilities
  , Ila(..)
  , Probe(..)
  ) where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive))

import Data.String.Interpolate (__i)

import Clash.Cores.Xilinx.Ila.Internal

import qualified Language.Haskell.TH as TH

-- | A default ILA config that:
--
--  * Configures no pipeline registers
--  * Stores 4096 samples
--  * Enables capture control
--
-- See 'IlaConfig' for more information.
ilaConfig :: IlaConfig
ilaConfig = IlaConfig
  { stages = 0
  , depth = D4096
  , captureControl = True
  , advancedTriggers = False
  , exactNames = True
  }


class Ila (dom :: Domain) a where
  ilaX :: a

instance Ila dom (Signal dom ()) where
  ilaX = pure ()

instance Ila dom a => Ila dom (Probe (Signal dom i) -> a) where
  ilaX !_i = ilaX @dom @a

-- | Probe with default config, see 'probeConfig', but with its 'probeType'
-- set to 'Data'.
dataProbe ::
  forall dom a.
  -- | Probe name
  String ->
  -- | Signal to capture
  Signal dom a ->
  -- | Probe structure to give to 'Clash.Cores.Xilinx.Ila.ila'
  Probe (Signal dom a)
dataProbe name = probeWith name probeConfig{probeType=Data}

-- | Probe with default config, see 'probeConfig', but with its 'probeType'
-- set to 'DataAndTrigger'.
triggerProbe ::
  forall dom a.
  -- | Probe name
  String ->
  -- | Signal to capture
  Signal dom a ->
  -- | Probe structure to give to 'Clash.Cores.Xilinx.Ila.ila'
  Probe (Signal dom a)
triggerProbe name = probeWith name probeConfig{probeType=DataAndTrigger}

-- | Like 'probeWith', but can be used to make sure probe names correspond to
-- names in the Clash source code. For example, instead of writing:
--
-- > probeWith myConfig "foo" foo
--
-- You can write:
--
-- > $(probeWithTH myConfig 'foo)
probeWithTh ::
  -- | Custom config, see 'probeConfig' for defaults
  ProbeConfig ->
  -- | Signal name to capture
  TH.Name ->
  -- | Probe structure to give to 'Clash.Cores.Xilinx.Ila.ila'
  TH.Q TH.Exp
probeWithTh = mkProbeTh 'probe

-- | Like 'probe', but can be used to make sure probe names correspond to
-- name in the Clash source code. For example, instead of writing:
--
-- > probe "foo" foo
--
-- You can write:
--
-- > $(probeTh 'foo)
probeTh ::
  -- | Signal name to capture
  TH.Name ->
  -- | Probe structure to give to 'Clash.Cores.Xilinx.Ila.ila'
  TH.Q TH.Exp
probeTh = mkProbeTh 'probe probeConfig

-- | Like 'dataProbe', but can be used to make sure probe names correspond to
-- name in the Clash source code. For example, instead of writing:
--
-- > dataProbe "foo" foo
--
-- You can write:
--
-- > $(dataProbeTh 'foo)
dataProbeTh ::
  -- | Signal name to capture
  TH.Name ->
  -- | Probe structure to give to 'Clash.Cores.Xilinx.Ila.ila'
  TH.Q TH.Exp
dataProbeTh = mkProbeTh 'dataProbe probeConfig

-- | Like 'triggerProbe', but can be used to make sure probe names correspond to
-- name in the Clash source code. For example, instead of writing:
--
-- > triggerProbe "foo" foo
--
-- You can write:
--
-- > $(triggerProbeTh 'foo)
triggerProbeTh ::
  -- | Signal name to capture
  TH.Name ->
  -- | Probe structure to give to 'Clash.Cores.Xilinx.Ila.ila'
  TH.Q TH.Exp
triggerProbeTh = mkProbeTh 'triggerProbe probeConfig

-- | A [polyvariadic](https://github.com/AJFarmar/haskell-polyvariadic) function
-- that instantiates a Xilinx Integrated Logic Analyzer (ILA).
--
-- Example invocation:
--
-- @
-- myAdder ::
--   forall dom .
--   'Signal' dom ('Unsigned' 8) ->
--   'Signal' dom ('Unsigned' 8) ->
--   'Signal' dom ('Unsigned' 8)
-- myAdder a b = ilaOut \`'hwSeqX'\` c
--  where
--   c = a + b
--
--   ilaOut :: Signal dom ()
--   ilaOut =
--     'ila'
--       clk
--       ('probe' "a" a)
--       ('probe' "b" b)
--       ('probe' "c" c)
-- @
--
-- Note that signal names do not have to correspond to names passed to the ILA.
--
-- __N.B.__ Use 'Clash.XException.hwSeqX' to make sure the ILA does not get
--          optimized away by GHC.
ila ::
  forall dom a .
  (KnownDomain dom, Ila dom a) =>
  -- | Clock to sample inputs on. Note that this is not necessarily the clock
  -- Xilinx's debug hub will run at, if multiple ILAs are instantiated.
  Clock dom ->
  -- | Any number of 'Signal' arguments. The result will always be
  -- @Signal dom ()@. You need to make sure this does not get optimized away by
  -- GHC by using 'Clash.XException.hwSeqX'.
  a
ila clk = ilaWith ilaConfig clk
{-# INLINE ila #-}

-- | Like 'ila', but takes an 'IlaConfig'.
ilaWith ::
  forall dom a .
  (KnownDomain dom, Ila dom a) =>
  IlaConfig ->
  -- | Clock to sample inputs on. Note that this is not necessarily the clock
  -- Xilinx's debug hub will run at, if multiple ILAs are instantiated.
  Clock dom ->
  -- | Any number of 'Signal' arguments. The result will always be
  -- @Signal dom ()@. You need to make sure this does not get optimized away by
  -- GHC by using 'Clash.XException.hwSeqX'.
  a
ilaWith config clk = ila# @dom @a config clk
{-# OPAQUE ilaWith #-}

-- | Primitive for 'ila'. Defining a wrapper like this makes the ILA
-- instantiation be rendered in its own module to reduce naming collision
-- probabilities.
ila# ::
  forall dom a .
  (KnownDomain dom, Ila dom a) =>
  IlaConfig ->
  Clock dom ->
  a
ila# !_conf !_clk = ilaX @dom @a
{-# OPAQUE ila# #-}
{-# ANN ila# (
    let primName = 'ila#
        tfName = 'ilaBbf
    in InlineYamlPrimitive [minBound..] [__i|
         BlackBoxHaskell:
             name: #{primName}
             templateFunction: #{tfName}
             workInfo: Always
         |]) #-}
