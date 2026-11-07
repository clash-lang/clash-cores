{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's In-System Sources and Probes IP (@altsource_probe@), the
Intel counterpart of "Clash.Cores.Xilinx.Vio". See the
[In-System Sources and Probes chapter](https://www.intel.com/content/www/us/en/docs/programmable/683819/21-3/design-debugging-using-in-system-sources-and-54484.html)
of the /Quartus Prime Standard Edition User Guide: Debug Tools/.

The IP lets you read the /probes/ and drive the /sources/ of a running design
over JTAG, with the In-System Sources and Probes Editor in Quartus or with the
Tcl commands in @quartus_stp@. Clash simulation is not applicable to the
interactive part: in simulation, the sources keep their initial value, which
is also their value in hardware until they are written over JTAG. Intel's own
simulation models of the IP don't do more: they drive the sources with zeros
(the Platform Designer model) or not at all (the model in @altera_mf@).

Clash instantiates the @altsource_probe@ megafunction directly, like the IP
Parameter Editor does. It is part of every Quartus edition, and its (empty)
simulation models are part of Intel's simulation library @altera_mf@.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Intel.Debug.SourcesProbes
  ( sourcesProbes
    -- * Internals
  , sourcesProbes#
  , sourcesProbesBBF
  , sourcesProbesTF
  , hexValue
  ) where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Control.Monad.State (State)
import qualified Data.Foldable as Foldable
import Data.String.Interpolate (__i)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import Text.Show.Pretty (ppShow)

import qualified Data.Text.Lazy as LazyText

import Clash.Annotations.Primitive (Primitive(InlineYamlPrimitive), HDL(..))
import Clash.Backend (Backend, hdlKind)
import Clash.Netlist.BlackBox.Types
  ( BlackBoxFunction, BlackBoxMeta(..), RenderVoid(..), TemplateKind(TDecl)
  , emptyBlackBoxMeta )
import Clash.Netlist.Expr (bits)
import Clash.Netlist.Types
  ( BlackBox(BBFunction), BlackBoxContext(..), EntityOrComponent(Empty)
  , Expr, HWType(..), TemplateFunction(..), backend, fromSomeBackend )

import qualified Clash.Netlist.BlackBox.Types as BlackBox
import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL

import Control.Lens (use)

{- | In-System Sources and Probes. Lets you observe the probes and set the
sources over JTAG, while the design runs on the FPGA. Both can be any type
with a 'BitPack' instance, of up to 512 bits. Use a tuple or a record to probe
several signals, or @()@ to only have sources (the instance then has a single
probe, which is always low). Example:

@
debug ::
  'KnownDomain' dom =>
  'Clock' dom ->
  'Signal' dom ('Unsigned' 8, 'Bool') ->
  'Signal' dom ('Bool', 'Unsigned' 4)
debug = 'sourcesProbes' \"CTRL\" ('False', 3)
@

The sources are synchronized to the given clock, with two registers per bit
(the IP's /Use Source Clock/ option). The probes are sampled with the JTAG
clock, so they should be stable for a while when you read them. The probes are
connected to the instance, so they are not optimized away by Quartus.

In Clash simulation, the sources are the initial value. Intel's simulation
models don't simulate the JTAG connection either.

>>> sampleN 3 (sourcesProbes "X" (5 :: Unsigned 4) systemClockGen (pure (0 :: Bit)))
[5,5,5]

The initial value must be a constant that Clash can determine when generating
HDL, and its type must not have a custom bit representation.
-}
sourcesProbes ::
  forall dom i o.
  ( KnownDomain dom
  , BitPack i
  , BitPack o
  , BitSize i <= 512
  , 1 <= BitSize o
  , BitSize o <= 512
  ) =>
  -- | Instance ID: a name of up to four characters, which identifies the
  -- instance in the In-System Sources and Probes Editor. An empty string
  -- leaves it unset.
  String ->
  -- | Initial value of the sources
  o ->
  -- | Clock to synchronize the sources to
  Clock dom ->
  -- | Probes
  Signal dom i ->
  -- | Sources
  Signal dom o
sourcesProbes = sourcesProbes#
{-# INLINE sourcesProbes #-}

-- | Primitive for 'sourcesProbes'
sourcesProbes# ::
  forall dom i o.
  (KnownDomain dom, BitPack i, BitPack o) =>
  String ->
  o ->
  Clock dom ->
  Signal dom i ->
  Signal dom o
sourcesProbes# !_instanceId initial !_clk !_probes = pure initial
{-# OPAQUE sourcesProbes# #-}
{-# ANN sourcesProbes# (
  let
    primName = show 'sourcesProbes#
    tfName = show 'sourcesProbesBBF
  in
    InlineYamlPrimitive [minBound..] [__i|
      BlackBoxHaskell:
        name: #{primName}
        templateFunction: #{tfName}
        workInfo: Always
    |]) #-}

-- | 'BlackBoxFunction' for 'sourcesProbes#'
sourcesProbesBBF :: HasCallStack => BlackBoxFunction
sourcesProbesBBF _isD _primName _args _resTys = do
  hdl <- use (backend . fromSomeBackend hdlKind)
  let
    vhdlOnly :: String -> [BlackBox.BlackBoxTemplate]
    vhdlOnly s
      | hdl == VHDL = [[BlackBox.Text (LazyText.pack s)]]
      | otherwise = []
    meta = emptyBlackBoxMeta
      { bbKind = TDecl
      , bbRenderVoid = NoRenderVoid
      , bbLibrary = vhdlOnly "altera_mf"
      , bbImports = vhdlOnly "altera_mf.altera_mf_components.all"
      }
  pure (Right (meta, BBFunction (show 'sourcesProbesTF) 0 sourcesProbesTF))

-- | Template function for 'sourcesProbes#'
sourcesProbesTF :: HasCallStack => TemplateFunction
sourcesProbesTF = TemplateFunction [3, 4, 5, 6] (const True) sourcesProbesBBTF

sourcesProbesBBTF ::
  forall s.
  (HasCallStack, Backend s) =>
  BlackBoxContext ->
  State s Doc
sourcesProbesBBTF bbCtx
  | [ _knownDomain, _bitPackI, _bitPackO
    , DSL.getStr -> Just instanceId
    , initial
    , clk
    , probes ] <- P.map fst (DSL.tInputs bbCtx)
  , [result] <- DSL.tResults bbCtx
  = if P.length instanceId > 4 then
      error ("sourcesProbes: the instance ID can't be longer than four characters: " <> show instanceId)
    else do
      let
        sourceWidth = DSL.tySize (DSL.ety result)
        probeWidth = DSL.tySize (DSL.ety probes) :: Int
        initialHex = hexValue sourceWidth (DSL.ety initial) (DSL.eex initial)
      instName <- Id.makeBasic "sources_probes_inst"
      DSL.declarationReturn bbCtx "sources_probes_block" $ do
        -- The VHDL component declaration of @altsource_probe@ has no default
        -- for the probes, so tie a single probe low if there are none
        probesBv <-
          if probeWidth > 0 then DSL.toBV "probe" probes else pure (DSL.bvLit 1 0)
        sourcesBv <- DSL.declare "source" (BitVector sourceWidth)
        DSL.instDecl Empty (Id.unsafeMake "altsource_probe") instName
          [ ("sld_auto_instance_index", DSL.litTExpr (DSL.S "YES"))
          , ("sld_instance_index", DSL.litTExpr (DSL.I 0))
          , ("instance_id", DSL.litTExpr (DSL.S (if P.null instanceId then "NONE" else instanceId)))
          , ("probe_width", DSL.litTExpr (DSL.I (toInteger (max 1 probeWidth))))
          , ("source_width", DSL.litTExpr (DSL.I (toInteger sourceWidth)))
          , ("source_initial_value", DSL.litTExpr (DSL.S initialHex))
          , ("enable_metastability", DSL.litTExpr (DSL.S "YES"))
          ]
          ( [ ("source_clk", clk)
            , ("source_ena", DSL.High) ]
         <> [ ("probe", probesBv) ] )
          [ ("source", sourcesBv) ]
        sources <- DSL.fromBV "sources" (DSL.ety initial) sourcesBv
        pure [sources]
  | otherwise = error ("sourcesProbes: unexpected arguments: " <> ppShow (DSL.tInputs bbCtx))

-- | Render a constant as the hexadecimal string @altsource_probe@ expects
hexValue :: HasCallStack => Int -> HWType -> Expr -> String
hexValue width ty expr = case bits width expr of
  Left failed -> error [__i|
    sourcesProbes: Clash failed to determine a constant value for the initial
    value of the sources. The failing expression is:

      #{ppShow failed}

    The complete expression, of type #{ppShow ty}, is:

      #{ppShow expr}
    |]
  Right bs ->
    let n = P.foldl (\acc b -> 2 * acc + (if b then 1 else 0)) (0 :: Integer) (Foldable.fold bs)
    in showHex n ""
