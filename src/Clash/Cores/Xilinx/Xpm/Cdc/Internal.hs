{-|
Copyright   :  (C) 2023-2024, Martijn Bastiaan
                   2023-2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

An experimental way to provide frictionless black box instances. It's currently
housed here, but should gradually move its way up the module hierarchy and
eventually made public.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- We usually hide @Internal@ modules, but this one is meant as an experimental API.
-- {-# OPTIONS_HADDOCK hide #-}

-- Clash considers `newtype`s transparent, so we need to use `data` instead. Also
-- note that we don't use a proper pragma to prevent an "unrecognized pragma"
-- warning by GHC.
{- HLINT ignore "Use newtype instead of data" #-}

-- We want to export everything in this module, but we also want to use Haddock
-- sections.
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( inst
  , InstConfig(..)

  -- * Params and ports
  , Param(..)
  , ClockPort(..)
  , ResetPort(..)
  , Port(..)

  -- * /Internals/
  , module Clash.Cores.Xilinx.Xpm.Cdc.Internal

  ) where


#if MIN_VERSION_base(4,18,0)
import Clash.Explicit.Prelude hiding (someNatVal, withSomeSNat)
#else
import Clash.Explicit.Prelude hiding (someNatVal)
#endif
import qualified Prelude as P

import Control.Monad (when)
import Control.Monad.State (State)
import Data.Proxy (Proxy)
import Data.Either (lefts, partitionEithers, rights)
import Data.Maybe (maybeToList, isJust, catMaybes)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (someNatVal)
import Prettyprinter.Interpolate (__di)
import Text.Show.Pretty (ppShow)

import qualified Control.Lens as Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

import Clash.Annotations.Primitive
import Clash.Backend (Backend, hdlKind)
import Clash.Core.DataCon ( DataCon(MkData, dcName) )
import Clash.Core.Name ( Name(Name, nameOcc) )
import Clash.Core.Term ( Term(Data), collectArgs )
import Clash.Core.TermLiteral (TermLiteral(..), deriveTermLiteral)
import Clash.Core.TermLiteral.TH (deriveTermToData)
import Clash.Core.TyCon(isTupleTyConLike)
import Clash.Core.Type (Type(..), LitTy (SymTy, NumTy), ConstTy(TyCon), splitTyConAppM, isIntegerTy)
import Clash.Driver.Types (DomainMap, envDomains)
import Clash.Netlist.BlackBox.Types
  ( BlackBoxFunction, BlackBoxMeta(bbKind, bbRenderVoid), TemplateKind(TDecl), emptyBlackBoxMeta
  , bbLibrary, bbImports, bbIncludes, RenderVoid (..) )
import Clash.Netlist.Types

import qualified Clash.Netlist.BlackBox.Types as BlackBox
import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL
import qualified Clash.Util.Interpolate as I

import Clash.Core.TermLiteral.Compat (termToDataError)

-- | VHDL generic or Verilog parameter. Contents should be able to render to an
-- HDL constant.
data Param (name :: Symbol) a = Param a

-- | Port with clock (bit) type, where @portName@ is the name of the port that
-- is mapped in the target HDL.
data ClockPort (portName :: Symbol) dom = ClockPort (Clock dom)

-- | Port with reset (bit) type, where @portName@ is the name of the port that
-- is mapped in the target HDL. The @polarity@ is used to automatically insert
-- a @not@ if the reset polarity does not match the reset polarity of the domain.
data ResetPort (portName :: Symbol) (polarity :: ResetPolarity) dom = ResetPort (Reset dom)

-- | 'BiSignalIn' port carrying a bitvector of size /n/ type, where @portName@
-- is the name of the port that is mapped in the target HDL.
data BiSignalInPort (portName :: Symbol) (ds :: BiSignalDefault) (dom :: Domain) (n :: Nat) =
  BiSignalInPort (BiSignalIn ds dom n)

-- | 'BiSignalOut' port carrying a bitvector of size /n/ type, where @portName@
-- is the name of the port that is mapped in the target HDL.
data BiSignalOutPort (portName :: Symbol) (ds :: BiSignalDefault) (dom :: Domain) (n :: Nat) =
  BiSignalOutPort (BiSignalOut ds dom n)

-- | A generic port, where @portName@ is the name of the port that is mapped in
-- the target HDL.
data Port (portName :: Symbol) dom a = Port (Signal dom a)

-- | A term level, post-normalization version of 'Param'. Used internally by
-- this module.
data PrimParam a = PrimParam
  { paramName :: Text
  , paramAsInteger :: Maybe Integer
  , paramMeta :: a
  }
  deriving (Show, Functor)

-- | A term level, post-normalization version of 'ClockPort', 'ResetPort', 'Port',
-- 'BiSignalInPort', or 'BiSignalOutPort'. Used internally by this module.
data PrimPort a
  = PrimClockPort       { name :: Text, dom :: Text, meta :: a }
  | PrimResetPort       { name :: Text, dom :: Text, meta :: a, polarity :: ResetPolarity }
  | PrimSignalPort      { name :: Text, dom :: Text, meta :: a }
  | PrimBiSignalInPort  { name :: Text, dom :: Text, meta :: a, n :: Integer }
  | PrimBiSignalOutPort { name :: Text, dom :: Text, meta :: a, n :: Integer }
  deriving (Show, Functor)

-- | Add meta information to a 'PrimPort'
addPrimPortMeta :: PrimPort () -> a -> PrimPort a
addPrimPortMeta p a = const a <$> p

-- | Either 'PrimParam' or 'PrimPort'
data PrimPortOrParam a
  = MkPrimParam (PrimParam a)
  | MkPrimPort (PrimPort a)
  deriving (Show, Functor)

-- | Add meta information to a 'PrimPort' or 'PrimParam'
addPrimPortOrParamMeta :: PrimPortOrParam () -> a -> PrimPortOrParam a
addPrimPortOrParamMeta p a = const a <$> p

-- | Partition based on constructors of 'PrimPortOrParam'.
partitionPortOrPrims :: [PrimPortOrParam a] -> ([PrimParam a], [PrimPort a])
partitionPortOrPrims = partitionEithers . P.map go
 where
  go :: PrimPortOrParam a -> Either (PrimParam a) (PrimPort a)
  go (MkPrimParam p) = Left p
  go (MkPrimPort p) = Right p

-- | Name of 'PrimPort' or 'PrimParam'
primPortOrParmName :: PrimPortOrParam a -> Text
primPortOrParmName = \case
  MkPrimParam p -> paramName p
  MkPrimPort p -> name p

collectDataArgs :: Term -> Maybe (String, [Either Term Type])
collectDataArgs (collectArgs -> (f, args))
  | Data (MkData{dcName=Name{nameOcc}}) <- f
  = Just (Text.unpack nameOcc, args)
  | otherwise
  = Nothing

-- | Interprets any of 'Param', 'ClockPort', 'ResetPort', or 'Port' into a single
-- data type 'PrimPortOrParam'.
instance TermLiteral (PrimPortOrParam ()) where
  termToData (collectDataArgs -> Just (constrName, args))
    | constrName == show 'Param
    , [Right (LitTy (SymTy nm)), Right ty, Left arg] <- args
    = do
      integerParam <-
        if isIntegerTy ty
        then Just <$> termToData arg
        else pure Nothing

      pure (MkPrimParam (PrimParam
        { paramName = Text.pack nm
        , paramMeta = ()
        , paramAsInteger = integerParam
        }))

    | constrName == show 'Port
    , (LitTy (SymTy nm) : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimSignalPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}))

    | constrName == show 'ClockPort
    , (LitTy (SymTy nm) : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimClockPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}))

    | constrName == show 'BiSignalInPort
    , ( LitTy (SymTy nm)
      : ConstTy _dflt
      : LitTy (SymTy domNm)
      : LitTy (NumTy width)
      : _
      ) <- rights args
    = pure (MkPrimPort (PrimBiSignalInPort
        { name=Text.pack nm
        , dom=Text.pack domNm
        , meta=()
        , n = width
        }
      ))

    | constrName == show 'ResetPort
    , (LitTy (SymTy nm) : pol : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimResetPort
        { name = Text.pack nm
        , dom = Text.pack domNm
        , meta = ()
        , polarity = getPolarity nm pol
        }))

  termToData t = Left t

-- | Get the 'ResetPolarity' from a type. This is used to automatically insert
-- a @not@ if the reset polarity does not match the reset polarity of the domain.
getPolarity ::
  HasCallStack =>
  -- | Domain name, used for error reporting
  String ->
  -- | Type to inspect
  Type ->
  ResetPolarity
getPolarity _ (ConstTy (TyCon (Text.unpack . nameOcc -> nm)))
  | nm == show 'ActiveHigh = ActiveHigh
  | nm == show 'ActiveLow  = ActiveLow
getPolarity nm ty = error ("Could not determine ResetPolarity for ResetPort @\""<> nm <> "\" from\n " <> show ty)

type OptionName = String

data XilinxWizardOption
  = StrOpt String
  | IntegerOpt Integer
  | BoolOpt Bool
  deriving (Show)
deriveTermLiteral ''XilinxWizardOption

xilinxWizardOptionToTcl :: XilinxWizardOption -> Text
xilinxWizardOptionToTcl = \case
  StrOpt s -> '{' `Text.cons` (Text.pack s) <> "}"
  IntegerOpt i -> Text.pack $ show i
  BoolOpt True -> "true"
  BoolOpt False -> "false"

data XilinxWizard n = XilinxWizard
  { wiz_name :: String
  -- ^ E.g., @floating_point@
  , wiz_vendor :: String
  -- ^ Usually @xilinx.com@
  , wiz_library :: String
  -- ^ Usually @ip@
  , wiz_version :: String
  -- ^ E.g., @7.1@
  , wiz_options :: Vec n (OptionName, XilinxWizardOption)
  } deriving (Show)

-- | Config for 'inst'
data InstConfig = InstConfig
  { compName :: String
    -- ^ Component to instantiate. Instantiation label will be a (unique) derivative
    -- of this.
  , library :: Maybe String
    -- ^ Whether to add a @library <name>@. This is only used for VHDL.
  , libraryImport :: Maybe String
    -- ^ Whether to add a @import <name>@. This is only used for VHDL.
  , renderVoid :: Bool
    -- ^ Whether to render an instance that has no output ports.
  } deriving (Show)
deriveTermLiteral ''InstConfig

instance KnownNat n => TermLiteral (XilinxWizard n) where
  termToData = $(deriveTermToData ''XilinxWizard)

-- | Empty config, with mandatory 'compName' set.
instConfig :: String -> InstConfig
instConfig nm = InstConfig
  { compName = nm
  , library = Nothing
  , libraryImport = Nothing
  , renderVoid = False
  }

-- | Type class to facilitate /polyvariadic/ behavior of 'inst'. A type @a@
-- constrained as @Inst a@ can be interpreted as a function:
--
-- > a_0 -> a_1 -> ... -> a_n -> r
--
-- where @n@ is the number of arguments (which can be zero) and @r@ is the
-- result type. Each argument can be a 'Port', 'ClockPort', 'ResetPort', or
-- 'Param'. The result type, @r@ can be a 'Port', 'ClockPort', 'ResetPort', or
-- a tuple of these.
--
-- See 'inst' for examples.
class Inst a where
  instX :: a

  default instX :: a
  instX = errorX "Internal error: this is not supposed to end up in simulation"

class IsPort a where
  type PortType a
  unPort :: a -> PortType a

instance KnownDomain dom => IsPort (ClockPort name dom) where
  type PortType (ClockPort name dom) = Clock dom
  unPort (ClockPort clk) = clk

instance KnownDomain dom => IsPort (ResetPort name polarity dom) where
  type PortType (ResetPort name polarity dom) = Reset dom
  unPort (ResetPort rst) = rst

instance KnownDomain dom => IsPort (Port name dom a) where
  type PortType (Port name dom a) = Signal dom a
  unPort (Port sig) = sig

instance KnownDomain dom => IsPort (BiSignalOutPort s ds dom n) where
  type PortType (BiSignalOutPort s ds dom n) = BiSignalOut ds dom n
  unPort (BiSignalOutPort sig) = sig

-- "Terminating" instances for 'Inst'
instance Inst () -- Inst with only inputs
instance KnownDomain dom => Inst (Port s dom a) -- Inst with a single port output
instance KnownDomain dom => Inst (ClockPort s dom) -- Inst with a single clock output
instance KnownDomain dom => Inst (ResetPort s polarity dom) -- Inst with a single reset output
instance KnownDomain dom => Inst (BiSignalOutPort s ds dom n) -- Inst with a single bi-signal output

-- A generous number of tuple instances: users typically don't have any choice
-- in how many output ports a primitive has. I.e., the only workaround would be
-- to use @-flarge-tuples@, which would be a pretty high price to pay. By writing
-- them out (instead of using TH) we limit the time GHC spends on it.
instance (IsPort p0, IsPort p1) => Inst (p0, p1)
instance (IsPort p0, IsPort p1, IsPort p2) => Inst (p0, p1, p2)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3) => Inst (p0, p1, p2, p3)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4) => Inst (p0, p1, p2, p3, p4)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5) => Inst (p0, p1, p2, p3, p4, p5)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6) => Inst (p0, p1, p2, p3, p4, p5, p6)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7) => Inst (p0, p1, p2, p3, p4, p5, p6, p7)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23, IsPort p24) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23, IsPort p24, IsPort p25) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23, IsPort p24, IsPort p25, IsPort p26) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23, IsPort p24, IsPort p25, IsPort p26, IsPort p27) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23, IsPort p24, IsPort p25, IsPort p26, IsPort p27, IsPort p28) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23, IsPort p24, IsPort p25, IsPort p26, IsPort p27, IsPort p28, IsPort p29) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23, IsPort p24, IsPort p25, IsPort p26, IsPort p27, IsPort p28, IsPort p29, IsPort p30) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30)
instance (IsPort p0, IsPort p1, IsPort p2, IsPort p3, IsPort p4, IsPort p5, IsPort p6, IsPort p7, IsPort p8, IsPort p9, IsPort p10, IsPort p11, IsPort p12, IsPort p13, IsPort p14, IsPort p15, IsPort p16, IsPort p17, IsPort p18, IsPort p19, IsPort p20, IsPort p21, IsPort p22, IsPort p23, IsPort p24, IsPort p25, IsPort p26, IsPort p27, IsPort p28, IsPort p29, IsPort p30, IsPort p31) => Inst (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31)

-- Instances accepting another argument
instance Inst a => Inst (Port s dom i -> a) where
  instX !_i = instX @a

instance Inst a => Inst (ClockPort s dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (ResetPort s polarity dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (BiSignalInPort s ds dom n -> a) where
  instX !_i = instX @a

instance Inst a => Inst (Param s const -> a) where
  instX !_i = instX @a

-- | /Polyvariadic/ function representing the instantiation of a Verilog/VHDL
-- component. You can interpret this function as:
--
-- > inst :: Inst a => InstConfig -> a_0 -> a_1 -> ... -> a_n -> r
--
-- where @n@ is the number of arguments (which can be zero) and @r@ is the
-- result type. Each argument can be a 'Port', 'ClockPort', 'ResetPort', or
-- 'Param'. The result type, @r@ can be a 'Port', 'ClockPort', 'ResetPort', a
-- tuple of these, or unit (@()@).
--
-- Example usage:
--
-- > go :: Port "dest_out_bin" dst (BitVector n)
-- > go =
-- >   inst
-- >     (instConfig "xpm_cdc_gray")
-- >       { library = Just "xpm"
-- >       , libraryImport = Just "xpm.vcomponents.all" }
-- >
-- >     (Param @"DEST_SYNC_FF"          @Integer (natToNum @stages))
-- >     (Param @"INIT_SYNC_FF"          @Integer (if initialValues then 1 else 0))
-- >     (Param @"REG_OUTPUT"            @Integer 0)
-- >     (Param @"SIM_ASSERT_CHK"        @Integer 0)
-- >     (Param @"SIM_LOSSLESS_GRAY_CHK" @Integer 0)
-- >     (Param @"WIDTH"                 @Integer (natToNum @n))
-- >
-- >     (ClockPort @"src_clk"    clkSrc)
-- >     (ClockPort @"dest_clk"   clkDst)
-- >     (Port      @"src_in_bin" (pack <$> srcIn))
--
-- This will instantiate a component named @xpm_cdc_gray@ from the @xpm@ library
-- and import the @xpm.vcomponents.all@ package. The component has 6 parameters
-- and 3 ports. Note that Clash/Haskell constructs can be used as arguments, such
-- as @if ... then ... else ...@ or @pack@.
--
-- This can be used in combination with 'clashSimulation' to pick a simulation
-- model during Haskell evaluation, and the black box instantiation when Clash
-- translates the design to Verilog/VHDL.
inst :: forall a. Inst a => InstConfig -> a
inst !_ = instX
{-# OPAQUE inst #-}
{-# ANN inst (
  let
    primName = show 'inst
    tfName = show 'instBBF
  in
    InlineYamlPrimitive [minBound..] [__i|
      BlackBoxHaskell:
        name: #{primName}
        templateFunction: #{tfName}
        workInfo: Always
    |]) #-}

-- | Like 'inst', but also generates TCL files invoking a Xilinx Wizard.
instWithXilinxWizard ::
  forall a n.
  (Inst a, KnownNat n) =>
  InstConfig ->
  XilinxWizard n ->
  a
instWithXilinxWizard i x = instWithXilinxWizard# x i
{-# INLINE instWithXilinxWizard #-}

-- | Worker for 'instWithXilinxWizard'. It flips the arguments to make writing
-- a common black box for 'inst' and this function easier.
instWithXilinxWizard# ::
  forall a n.
  (Inst a, KnownNat n) =>
  XilinxWizard n ->
  InstConfig ->
  a
instWithXilinxWizard# !_ !_ = instX
{-# OPAQUE instWithXilinxWizard# #-}
{-# ANN instWithXilinxWizard# (
  let
    primName = show 'instWithXilinxWizard#
    tfName = show 'instWithXilinxWizardBBF
  in
    InlineYamlPrimitive [minBound..] [__i|
      BlackBoxHaskell:
        name: #{primName}
        templateFunction: #{tfName}
        workInfo: Always
    |]) #-}

-- | Interpret arguments as 'PrimPortOrParam's
argsToPrimPortOrParams :: [Term] -> Either String [PrimPortOrParam ()]
argsToPrimPortOrParams [] = pure []
argsToPrimPortOrParams (t:ts) = do
  arg <- termToDataError t
  args <- argsToPrimPortOrParams ts
  pure (arg:args)

-- | Interpret result type as 'PrimPort's
tyToPrimPort :: Type -> Either String [PrimPort ()]
tyToPrimPort (splitTyConAppM -> Just (tyConName'@(Name{nameOcc=Text.unpack -> tyConName}), args))
    | tyConName == show 'Param
    , LitTy (SymTy nm) : _ <- args
    = Left ("Can't translate Param " <> nm <> " to PrimPort. Did you define a Param in a result type?")

    | tyConName == show 'Port
    , LitTy (SymTy nm) : LitTy (SymTy domNm) : _ <- args
    = Right [PrimSignalPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}]

    | tyConName == show 'ClockPort
    , LitTy (SymTy nm) : LitTy (SymTy domNm) : _ <- args
    = Right [PrimClockPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}]

    | tyConName == show 'ResetPort
    , LitTy (SymTy nm) : pol : LitTy (SymTy domNm) : _ <- args
    = Right [PrimResetPort
        { name=Text.pack nm
        , dom=Text.pack domNm
        , meta=()
        , polarity = getPolarity nm pol
        }]

    | tyConName == show 'BiSignalOutPort
    , LitTy (SymTy nm) : _ds : LitTy (SymTy domNm) : LitTy (NumTy width) : _ <- args
    = Right [PrimBiSignalOutPort
        { name=Text.pack nm
        , dom=Text.pack domNm
        , meta=()
        , n = width
        }]

    | isTupleTyConLike tyConName' = do
      xs <- mapM tyToPrimPort args
      pure $ P.concat xs
tyToPrimPort ty = error (ppShow ty)

hwtyToPortTypes :: HWType -> [HWType]
hwtyToPortTypes (Annotated _ ty) = hwtyToPortTypes ty
hwtyToPortTypes (Product _ _ tys) =
  -- Note that we don't have to check _what_ kind of product we're splitting here:
  -- the type classes enforce this can only be a tuple.
  tys
hwtyToPortTypes hwty = [hwty]

-- | 'BlackBoxFunction' for @inst@
instBBF :: HasCallStack => BlackBoxFunction
instBBF = instBBFWorker @0 0 Nothing

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

-- | 'BlackBoxFunction' for @inst@
instWithXilinxWizardBBF :: HasCallStack => BlackBoxFunction
instWithXilinxWizardBBF isD primName args resTys
  | _instConstraint
  : (either error id . termToDataError -> n)
  : wizardAsTerm
  : _config
  : _userArgs <- lefts args
  = withSomeSNat n $ \(SNat :: SNat n)-> do
      let
        !(wizard :: XilinxWizard n)= either error id $ termToDataError wizardAsTerm

        -- 'instWithXilinxWizard' takes two extra arguments: a @KnownNat n@
        -- constraint, and a 'XilinxWizard'. These arguments should be dropped
        -- in the general black box functions.
        nExtraArgs = 2
      instBBFWorker nExtraArgs (Just wizard) isD primName args resTys
instWithXilinxWizardBBF _ _ args resTys = error $
     show 'instWithXilinxWizardBBF
  <> ", bad args:\n\n"
  <> ppShow args
  <> "\n\nor result types:\n\n"
  <> ppShow resTys


-- | 'BlackBoxFunction' for @inst@ and @instWithXilinxWizard@
instBBFWorker :: (HasCallStack, KnownNat n) => Int -> Maybe (XilinxWizard n) -> BlackBoxFunction
instBBFWorker nExtraArgs maybeXilinxWizard _isD _primName args [resTy]
  | _instConstraint:config:userArgs <- P.drop nExtraArgs $ lefts args
  = do
      doms <- Lens.view (clashEnv . Lens.to envDomains)
      hdl <- Lens.use (backend . fromSomeBackend hdlKind)
      case go config userArgs of
        Left s -> error ("instBBF, bad context:\n\n" <> s)
        Right (c, a, r) -> do
          pure $ Right (bbMeta hdl c, bb doms c a r)
 where
  go :: Term -> [Term] -> Either String (InstConfig, [PrimPortOrParam ()], [PrimPort ()])
  go config0 userArgs = do
    config1 <- termToDataError config0
    argPorts <- argsToPrimPortOrParams userArgs
    resPorts <- tyToPrimPort resTy
    pure (config1, argPorts, resPorts)

  bbMeta :: HDL -> InstConfig -> BlackBoxMeta
  bbMeta hdl InstConfig{..} = emptyBlackBoxMeta
    { bbKind = TDecl
    , bbLibrary = libToBBTemplate hdl library
    , bbImports = libToBBTemplate hdl libraryImport
    , bbRenderVoid = if renderVoid then RenderVoid else NoRenderVoid
    , bbIncludes =
      case maybeXilinxWizard of
        Nothing -> []
        Just w ->
          [ ( (Text.pack compName, "clash.tcl")
            , BBFunction
              (show 'instWizardBBTF)
              0
              (TemplateFunction [0..] (const True) (instWizardBBTF w))
            )
          ]
    }

  libToBBTemplate hdl
    | hdl == VHDL = P.map (pure . BlackBox.Text . LazyText.pack) . maybeToList
    | otherwise = const []

  bb :: DomainMap -> InstConfig -> [PrimPortOrParam ()] -> [PrimPort ()] -> BlackBox
  bb doms config primArgs primRes =
    BBFunction (show 'instTF) 0 (instTF nExtraArgs doms config maybeXilinxWizard primArgs primRes)

instBBFWorker _ _ _ _ args resTys = error $
     show 'instBBFWorker
  <> ", bad args:\n\n"
  <> ppShow args
  <> "\n\nor result types:\n\n"
  <> ppShow resTys

instTF ::
  (HasCallStack, KnownNat n) =>
  Int ->
  DomainMap ->
  InstConfig ->
  Maybe (XilinxWizard n) ->
  [PrimPortOrParam ()] ->
  [PrimPort ()] ->
  TemplateFunction
instTF nExtraArgs doms config maybeXilinxWizard primArgs primRes =
  TemplateFunction
    (usedArguments primArgs)
    (const True)
    (instBBTF nExtraArgs doms config maybeXilinxWizard primArgs primRes)

usedArguments :: [PrimPortOrParam a] -> [Int]
usedArguments (P.length -> nUserArgs) = 1 : [2..nUserArgs+2]

instWizardBBTF ::
  forall s n .
  (Backend s, KnownNat n, HasCallStack) =>
  XilinxWizard n ->
  BlackBoxContext ->
  State s Doc
instWizardBBTF xilinxWizard bbCtx | [compName] <- bbQsysIncName bbCtx =
  pure [__di|
    namespace eval $tclIface {
      variable api 1
      variable scriptPurpose createIp
      variable ipName {#{compName}}

      proc createIp {ipName0 args} {
        create_ip #{backslash}
          -name floating_point #{backslash}
          -vendor xilinx.com #{backslash}
          -library ip #{backslash}
          -version 7.1 #{backslash}
          -module_name $ipName0 #{backslash}
          {*}$args

        set_property -dict [list #{backslash}
    #{props}
                           ] [get_ips $ipName0]
        return
      }
    }
  |]
 where
  backslash = "\\" :: Text
  props = Text.intercalate "\n" (toList (map ppProp (wiz_options xilinxWizard)))
  ppProp (name, value) =
    Text.replicate 29 " " <> Text.pack name <> " " <>
      xilinxWizardOptionToTcl value <> " \\"

instWizardBBTF _ bbCtx = error (show 'instWizardBBTF <> ", bad bbCtx:\n\n" <> ppShow bbCtx)

instBBTF ::
  forall s n .
  (Backend s, KnownNat n, HasCallStack) =>
  Int ->
  DomainMap ->
  InstConfig ->
  Maybe (XilinxWizard n) ->
  [PrimPortOrParam ()] ->
  [PrimPort ()] ->
  BlackBoxContext ->
  State s Doc
instBBTF nExtraArgs doms config maybeXilinxWizard primArgs0 primResults0 bbCtx
  | (   _instConstraint
      : _instConfig
      : userArgs
      ) <- P.drop nExtraArgs $ P.map fst $ DSL.tInputs bbCtx
  , [resultTy] <- DSL.tResults bbCtx
  , outPortTys <- hwtyToPortTypes (DSL.ety resultTy)
  = do
      DSL.declarationReturn bbCtx "inst_block" $ do
        when (P.length primResults0 /= P.length outPortTys) $
          error [I.i|
            Internal error, mismatching result lengths:

              #{ppShow primResults0}

            and:

              #{ppShow outPortTys}
          |]

        when (P.length primArgs0 /= P.length userArgs) $
          error [I.i|
            Internal error, mismatching arg lengths:

              #{ppShow primArgs0}

            and:

              #{ppShow userArgs}
          |]

        outPorts <- catMaybes <$> mapM mkOutPort (P.zipWith addPrimPortMeta primResults0 outPortTys)
        let
          -- Note that this critically depends on the Clash Netlist backed treating
          -- a data type with a single constructor with a single field as the
          -- same type as the field. I.e., @hwTy (Port a) ~ hwTy a@.
          primArgs1 = P.zipWith addPrimPortOrParamMeta primArgs0 userArgs
          (params0, inPorts0) = partitionPortOrPrims primArgs1
          params1 = P.map mkParam params0

        inPorts1 <- mapM mkInPort inPorts0
        instLabel <- Id.make (Text.pack (compName config) <> "_inst")

        let compName1 = case maybeXilinxWizard of
              Just _ ->
                -- Use Clash's generated name when instantiating the wizard
                bbQsysIncName bbCtx P.!! 0
              Nothing ->
                -- Use exact name the user supplied
                Text.pack (compName config)

        DSL.instDecl
          (if isJust maybeXilinxWizard then Comp else Empty)
          (Id.unsafeMake compName1)
          instLabel
          params1
          inPorts1
          outPorts

        case outPorts of
          []  -> pure []
          [p] -> pure [snd p]
          ps  -> pure [DSL.tuple (snd <$> ps)]
 where
  mkParam :: PrimParam DSL.TExpr -> (Text, DSL.TExpr)
  mkParam (PrimParam{paramName, paramMeta, paramAsInteger}) =
    case paramAsInteger of
      Just i -> (paramName, DSL.TExpr Integer (Literal Nothing (NumLit i)))
      Nothing -> (paramName, paramMeta)

  mkInPort :: PrimPort DSL.TExpr -> State (DSL.BlockState s) (Text, DSL.TExpr)
  mkInPort = \case
    PrimResetPort{name, polarity, dom, meta=rstIn} -> do
      rst <- case HashMap.lookup dom doms of
              Just (vResetPolarity -> domPolarity)
                | polarity == domPolarity -> pure rstIn
                | otherwise -> DSL.notExpr name rstIn
              Nothing ->
                error ("Internal error: could not find domain " <> Text.unpack dom)
      pure (name, rst)
    p -> pure (name p, meta p)

  mkOutPort :: PrimPort HWType -> State (DSL.BlockState s) (Maybe (Text, DSL.TExpr))
  mkOutPort = \case
    PrimBiSignalOutPort{} -> pure Nothing

    PrimResetPort{name, polarity, dom, meta=ty} -> do
      var <- DSL.declare name ty
      rst <-
        case HashMap.lookup dom doms of
          Just (vResetPolarity -> domPolarity)
            | polarity == domPolarity -> pure var
            | otherwise -> DSL.notExpr name var
          Nothing ->
            error ("Internal error: could not find domain " <> Text.unpack dom)
      pure (Just (name, rst))

    port -> do
      let ty = meta port
      var <- DSL.declare (name port) ty
      pure (Just (name port, var))

instBBTF _nExtraArgs _doms _config _maybeXilinxWizard _primArgs _primResults bbCtx =
  error (show 'instBBTF <> ", bad bbCtx:\n\n" <> ppShow bbCtx)

#if !MIN_VERSION_clash_prelude(1,9,0)
onSomeBackend :: (forall b. Backend b => b -> a) -> SomeBackend -> a
onSomeBackend f (SomeBackend b) = f b

fromSomeBackend :: (forall b. Backend b => b -> a) -> Lens.Getter SomeBackend a
fromSomeBackend f = Lens.to (onSomeBackend f)
#endif
