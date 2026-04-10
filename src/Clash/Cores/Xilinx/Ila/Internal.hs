{-|
  Copyright   :  (C) 2022-2023, Google Inc
                     2022,      QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Black box implementation for primitives in "Clash.Cores.Xilinx.Ila".
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Ila.Internal where

import Prelude
import qualified Clash.Prelude as C

import Clash.Annotations.SynthesisAttributes (Attr(StringAttr))
import Clash.Backend (Backend)
import Clash.Core.Term (Term)
import Clash.Core.TermLiteral (TermLiteral(..), deriveTermLiteral)
import Clash.Core.TermLiteral.Compat (termToDataError)
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types
import Control.Monad (zipWithM)
import Control.Monad.State (State)
import Data.Either (lefts)
import Data.List (zip4)
import Data.List.Infinite((...), Infinite((:<)))
import Data.Maybe (isJust)
import Data.String.Interpolate (__i)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Lift)
import Text.Show.Pretty (ppShow)

import qualified Data.List.Infinite as Infinite
import qualified Data.Text as T
import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL

import Clash.Cores.Xilinx.Internal
  ( TclPurpose(..)
  , IpConfig(..)
  , defIpConfig
  , property
  , renderTcl
  )

-- | Number of samples to store
data Depth
  = D1024
  | D2048
  | D4096
  | D8192
  | D16384
  | D32768
  | D65536
  | D131072
  deriving (Show, Lift)

depthToWord :: Depth -> Word
depthToWord = \case
  D1024   -> 1024
  D2048   -> 2048
  D4096   -> 4096
  D8192   -> 8192
  D16384  -> 16384
  D32768  -> 32768
  D65536  -> 65536
  D131072 -> 131072

data ProbeType
  = DataAndTrigger
  -- ^ Probe can be used for data collection and to trigger data capture
  | Data
  -- ^ Probe can only be used for data collection
  | Trigger
  -- ^ Probe can only be used to trigger data capture
  deriving (Eq, Show, Lift, Enum)

data ProbeConfig = ProbeConfig
  { comparators :: Word
  -- ^ Number of comparators to instantiate for trigger probes. Should from 1 up to
  -- and including 16. This limits the number of conditions that can be used to
  -- trigger capture.
  , probeType :: ProbeType
  -- ^ Capabilities of the probe
  }
  deriving (Eq, Show, Lift)

-- | Default probe config. Probes can be used for both data and trigger. The number
-- of comparators is set to two.
probeConfig :: ProbeConfig
probeConfig = ProbeConfig{comparators=2, probeType=DataAndTrigger}

data Probe a = Probe
  { -- XXX: Keep signal as the first field! The blackbox implementation relies on it.
    signal :: a
  -- ^ Signal that the probe is attached to
  , name :: String
  -- ^ Name of ILA probe. This is the name that shows up when querying the ILA through
  -- the GUI or TCL interface.
  , config :: ProbeConfig
  -- ^ Probe specific configuration options
  }
  deriving (Eq, Show, Lift, Functor)

-- | Probe with default config, see 'probeConfig'.
probe ::
  forall dom a.
  -- | Probe name
  String ->
  -- | Signal to capture
  C.Signal dom a ->
  -- | Probe structure to give to 'Clash.Cores.Xilinx.Ila.ila'
  Probe (C.Signal dom a)
probe name = probeWith name probeConfig

-- | Like 'probe', but with a custom configuration
probeWith ::
  forall dom a.
  -- | Probe name
  String ->
  -- | Custom config, see 'probeConfig' for defaults
  ProbeConfig ->
  -- | Signal to capture
  C.Signal dom a ->
  -- | Probe structure to give to 'Clash.Cores.Xilinx.Ila.ila'
  Probe (C.Signal dom a)
probeWith name config signal = Probe{name, config, signal}

-- | Configures the static properties of an 'Clash.Cores.Xilinx.Ila.ila'. Note
-- that most properties (triggers, number of samples before/after trigger, ...)
-- are configured at runtime using Vivado. When applicable, configuration fields
-- will refer to the names of configuration labels mentioned in the product
-- guide.
--
-- Use 'Clash.Cores.Xilinx.Ila.ilaConfig' to construct this with some sensible
-- defaults.
data IlaConfig = IlaConfig
  { depth :: Depth
  -- ^ Number of samples to store. Corresponds to @C_DATA_DEPTH@.
  , captureControl :: Bool
  -- ^ Whether probes marked 'Trigger' or 'DataAndTrigger' can be used to control
  -- data capture. That is, a trigger marks the start of data collection, while
  -- capture control marks when to sample. Corresponds to @C_EN_STRG_QUAL@.
  , stages :: C.Index 7
  -- ^ Number of registers to insert at each probe. Supported values: 0-6.
  -- Corresponds to @C_INPUT_PIPE_STAGES@.
  , advancedTriggers :: Bool
  -- ^  Whether state machines can be used to describe trigger logic.
  -- Corresponds to @C_ADV_TRIGGER@.
  }
  deriving (Show, Lift)

-- XXX: I'd move this 'deriveTermLiteral' up, but Template Haskell complains..
deriveTermLiteral ''ProbeType
deriveTermLiteral ''ProbeConfig
deriveTermLiteral ''Probe
deriveTermLiteral ''Depth
deriveTermLiteral ''IlaConfig

ilaBbf :: HasCallStack => BlackBoxFunction
ilaBbf _isD _primName args _resTys = pure $
  case lefts args of
    (_:_:config:_clock:userArgs) ->
      case termToDataError @IlaConfig config of
        Left s -> Left ("ilaBbf, bad config:\n" <> s)
        Right c ->
          case traverse (termToDataError @(Probe Term)) userArgs of
            Left s -> Left $ ("ilaBbf, bad probes:\n" <> s)
            Right probes -> Right (bbMeta c (map eraseTerm probes), bb c (map eraseTerm probes))
    _ ->
      Left $ "ilaBbf, bad args:\n" <> ppShow args
 where

  bbMeta :: IlaConfig -> [Probe ()] -> BlackBoxMeta
  bbMeta config probes = emptyBlackBoxMeta
    { bbKind = TDecl
    , bbRenderVoid = RenderVoid
    , bbIncludes =
        [ ( ("ila", "clash.tcl")
          , BBFunction (show 'ilaTclTf) 0 (ilaTclTf config probes)
          )
        ]
    }

  eraseTerm :: Probe Term -> Probe ()
  eraseTerm p = const () <$> p

  bb :: IlaConfig -> [Probe ()] -> BlackBox
  bb config probes = BBFunction (show 'ilaTf) 0 (ilaTf config probes)

usedArguments :: [Int]
usedArguments = ilaConfig : clock : inputProbes
 where
  (    _knownDomain
    :< _ilaConstraint
    :< ilaConfig
    :< clock
    :< (Infinite.take 8096 -> inputProbes)
    ) = (0...) -- This function is polyvariadic so in theory it supports an
               -- unlimited number of arguments. To prevent evaluation loops
               -- when forcing this argument to NF we limit it to a modest
               -- 8096 input ports.

ilaTf :: HasCallStack => IlaConfig -> [Probe ()] -> TemplateFunction
ilaTf config probes = TemplateFunction usedArguments (const True) (ilaBbTf config probes)

ilaTclTf :: HasCallStack => IlaConfig -> [Probe ()] -> TemplateFunction
ilaTclTf config probes = TemplateFunction usedArguments (const True) (ilaTclBbTf config probes)

-- | Are all values in a list equal? If so, return the element.
areEqual :: Eq a => [a] -> Maybe a
areEqual [] = Nothing
areEqual (ref:as) = go as
 where
  go [] = Just ref
  go (a:rest)
    | ref == a = go rest
    | otherwise = Nothing

--  \case { (x:_):_ -> Just x; _ -> Nothing } . group

checkNameCollision :: HasCallStack => T.Text -> DSL.TExpr -> DSL.TExpr
checkNameCollision userName tExpr@(DSL.TExpr _ (Identifier (Id.toText -> name) Nothing))
  | userName == name = tExpr
  | otherwise = error [__i|
      Tried create a signal called '#{userName}', but identifier generation
      returned '#{name}'. Refusing to instantiate ILA with unreliable probe
      names.
  |]
checkNameCollision _ tExpr = error [__i|
  Internal error: Expected 'TExpr' with the following form:

    TExpr _ (Identifier _ Nothing)

  got:

    #{ppShow tExpr}
|]

-- | Return user-friendly ILA instance name given a context name hint.
-- We ignore @__VOID_TDECL_NOOP__@, created by @mkPrimitive@ whenever a user
-- hint is not given and the primitive returns a zero-width type.
getIlaName :: Maybe T.Text -> T.Text
getIlaName Nothing = "ila_inst"
getIlaName (Just "result") = getIlaName Nothing
getIlaName (Just "__VOID_TDECL_NOOP__") = getIlaName Nothing
getIlaName (Just s) = s

-- | Extract the signal 'TExpr' from a 'Probe' product in the blackbox context.
-- 'signal' is the first field of 'Probe', so it's the first expression in the
-- DataCon application.
toProbeExpr :: DSL.TExpr -> DSL.TExpr
toProbeExpr (DSL.TExpr{eex=DataCon (Product _ _ (signalType:_)) _ (signalExpr:_)}) =
  DSL.TExpr{eex=signalExpr, ety=signalType}
toProbeExpr tExpr =
  error $ "toProbeExpr: Unexpected probe expression: " <> ppShow tExpr

ilaBbTf ::
  forall s .
  (Backend s, HasCallStack) =>
  IlaConfig ->
  [Probe ()] ->
  BlackBoxContext ->
  State s Doc
ilaBbTf _config probes bbCtx
  | (   _knownDomainDom
      : _ilaConstraint
      : _ilaConfig
      : clk
      : (map toProbeExpr -> inPs)
      ) <- map fst $ DSL.tInputs bbCtx
  , [ilaName] <- bbQsysIncName bbCtx
  = do
      ilaInstName <- Id.makeBasic (getIlaName (bbCtxName bbCtx))

      let
        inNames = map (T.pack . ("probe" <>) . show) [(0 :: Int)..]
        inBVs   = map (BitVector . fromInteger . DSL.tySize . DSL.ety) inPs
        userProbeNames = map (T.pack . (\Probe{name=n} -> n)) probes

      DSL.declarationReturn bbCtx "ila_inst_block" $ do
        DSL.compInBlock ilaName (("clk", Bit) : zip inNames inBVs) []

        inProbes   <- zipWithM DSL.assign inNames inPs
        inProbesBV <- zipWithM toNameCheckedBv userProbeNames inProbes

        DSL.instDecl
          Empty
          (Id.unsafeMake ilaName)
          ilaInstName
          [] -- Generics / parameters
          (("clk", clk) : zip inNames inProbesBV)
          [] -- outputs

        pure []

  | otherwise = error "ilaBbTf: bad bbCtx"
 where
  -- The HDL attribute 'KEEP' is added to signals connected to probe ports so
  -- they are not optimized away by the synthesis tool.
  keepAttrs = [StringAttr "KEEP" "true"]

  toNameCheckedBv nameHint inProbe =
    checkNameCollision nameHint <$>
      DSL.toBvWithAttrs keepAttrs nameHint inProbe

ilaTclBbTf ::
  forall s .
  (HasCallStack, Backend s) =>
  IlaConfig ->
  [Probe ()] ->
  BlackBoxContext ->
  State s Doc
ilaTclBbTf IlaConfig{depth, captureControl, advancedTriggers, stages} probes bbCtx
  | [ilaName] <- bbQsysIncName bbCtx
  = pure $ renderTcl $ pure $ IpConfigPurpose $
      (defIpConfig "ila" "6.2" ilaName){properties = properties}
  | otherwise = error $ "ilaTclBbTf: bad bbCtx:\n\n" <> ppShow bbCtx
 where
  probeConfigs = map (\Probe{config=c} -> c) probes
  comps        = map comparators probeConfigs
  types        = map probeType   probeConfigs
  sameMu       = areEqual comps

  properties = globalProperties <> portProperties

  globalProperties =
    [ property @Int  "C_NUM_OF_PROBES" (length probes)
    , property @Word "C_INPUT_PIPE_STAGES" (fromIntegral stages)
    , property @Word "C_DATA_DEPTH" (depthToWord depth)
    , property @Bool "ALL_PROBE_SAME_MU" (isJust sameMu)
    , property @Int  "C_EN_STRG_QUAL" (if captureControl then 1 else 0)
    , property @Bool "C_TRIGIN_EN" False
    , property @Bool "C_ADV_TRIGGER" advancedTriggers
    ] <>
    [ property @Word "ALL_PROBE_SAME_MU_CNT" mu | Just mu <- [sameMu] ]

  portProperties = concat
    [ [ property @Int  [__i|C_PROBE#{i}_WIDTH|]  width
      , property @Int  [__i|C_PROBE#{i}_TYPE|]   (fromEnum pt)
      , property @Word [__i|C_PROBE#{i}_MU_CNT|] comp
      ]
    | (i, tExpr, pt, comp) <-
        zip4
          [(0 :: Int)..]
          (map toProbeExpr . drop 4 . map fst $ DSL.tInputs bbCtx)
          types
          comps
    , let width = DSL.tySize (DSL.ety tExpr)
    ]
