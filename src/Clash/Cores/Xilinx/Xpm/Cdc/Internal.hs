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
{-# LANGUAGE OverloadedRecordDot #-}
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
  , BitVectorParam(..)
  , BoolStringParam(..)
  , ClockPort(..)
  , DiffClockPort (..)
  , NamedDiffClockPort (..)
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
import Control.Monad.State (State, gets)
import Data.Proxy (Proxy)
import Data.Either (lefts, partitionEithers, rights)
import Data.Maybe (maybeToList, isJust)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Magic (lazy)
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
import Clash.Core.Term ( Term(Data, Prim), PrimInfo(..), collectArgs, stripAllTicks )
import Clash.Core.TermLiteral (TermLiteral(..), deriveTermLiteral)
import Clash.Core.TermLiteral.TH (deriveTermToData)
import Clash.Core.Pretty (showPpr)
import Clash.Core.TyCon(isTupleTyConLike)
import Clash.Core.Type (Type(..), LitTy (SymTy, NumTy), ConstTy(TyCon), normalizeType, splitTyConAppM, isIntegerTy)
import Clash.Core.Util (undefinedPrims, undefinedXPrims)
import Clash.Driver.Types (DomainMap, envDomains)
import Clash.Netlist.BlackBox.Types
  ( BlackBoxFunction, BlackBoxMeta(bbKind, bbRenderVoid), TemplateKind(TDecl), emptyBlackBoxMeta
  , bbLibrary, bbImports, bbIncludes, RenderVoid (..) )
import Clash.Netlist.Types
import Clash.Netlist.Util (isVoid, unsafeCoreTypeToHWTypeM')

import qualified Clash.Netlist.BlackBox.Types as BlackBox
import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL
import qualified Clash.Util.Interpolate as I

import qualified Clash.Core.TermLiteral.Compat as Compat

-- | VHDL generic or Verilog parameter. Contents should be able to render to an
-- HDL constant. Use 'BitVectorParam' for VHDL generics of type @bit_vector@,
-- and 'BoolStringParam' for parameters that are a @boolean@ in VHDL but a
-- string in Verilog.
data Param (name :: Symbol) a = Param a

-- | Like @'Param' name ('Clash.Explicit.Prelude.BitVector' n)@, but rendered
-- as a bit string literal that is not qualified with a type in VHDL. This makes
-- it fit generics of type @bit_vector@, such as the @INIT@ generic of Xilinx's
-- @LUT6@, as well as generics of type @std_logic_vector@. In Verilog and
-- SystemVerilog it renders like 'Param' does, as a sized literal such as
-- @4'b1001@.
--
-- The 'Clash.Explicit.Prelude.BitVector' must be a constant without undefined
-- bits.
data BitVectorParam (name :: Symbol) n = BitVectorParam (BitVector n)

-- | Boolean parameter in the style of Xilinx primitives, such as the
-- @IBUF_LOW_PWR@ parameter of @IBUF@: a @boolean@ in VHDL (@true@ or @false@),
-- but a string in Verilog and SystemVerilog (@\"TRUE\"@ or @\"FALSE\"@). Note
-- that @'Param' name t'Bool'@ renders as a bit in Verilog and SystemVerilog.
--
-- The t'Bool' must be a constant.
data BoolStringParam (name :: Symbol) = BoolStringParam Bool

-- | Port with clock (bit) type, where @portName@ is the name of the port that
-- is mapped in the target HDL.
data ClockPort (portName :: Symbol) dom = ClockPort (Clock dom)

-- | Port with differential clock type, which gets 2 ports on hardware for a possitive
-- and a negative phase. The @portName@ gets the suffix @_p@ and @_n@ respectively
-- for the ports in the target HDL. For custom suffixes use 'NamedDiffClockPort'.
data DiffClockPort (portName :: Symbol) dom = DiffClockPort (DiffClock dom)

-- | Port with differential clock type, which gets 2 ports on hardware for a possitive
-- and a negative phase. The @portNameP@ and @portNameN@ are the names of these ports
-- in the target HDL.
data NamedDiffClockPort (portNameP :: Symbol) (portNameN :: Symbol) dom =
  NamedDiffClockPort (DiffClock dom)

-- | Port with reset (bit) type, where @portName@ is the name of the port that
-- is mapped in the target HDL. The @polarity@ is used to automatically insert
-- a @not@ if the reset polarity does not match the reset polarity of the domain.
data ResetPort (portName :: Symbol) (polarity :: ResetPolarity) dom = ResetPort (Reset dom)

-- | 'BiSignalIn' port carrying a bitvector of size /n/ type, where @portName@
-- is the name of the port that is mapped in the target HDL.
data BiSignalInPort (portName :: Symbol) (ds :: BiSignalDefault) (dom :: Domain) (n :: Nat) =
  BiSignalInPort (BiSignalIn ds dom n)

-- | Like @'BiSignalInPort' portName ds dom 1@, but for an HDL port that is a
-- single bit instead of a vector of one bit, such as the @IO@ port of Xilinx's
-- @IOBUF@. Clash represents a 1-bit 'BiSignalIn' as a @std_logic_vector(0
-- downto 0)@ in VHDL, which can't be connected to a @std_logic@ port. In VHDL,
-- this port connects the instance to the single element of that vector
-- instead. In Verilog and SystemVerilog it is the same as 'BiSignalInPort'.
data BitBiSignalInPort (portName :: Symbol) (ds :: BiSignalDefault) (dom :: Domain) =
  BitBiSignalInPort (BiSignalIn ds dom 1)

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
  , paramRender :: ParamRender
  , paramMeta :: a
  }
  deriving (Show, Functor)

-- | How to render a 'PrimParam'
data ParamRender
  = DefaultRender
  -- ^ 'Param': render the argument like Clash would render it anywhere else
  | BitVectorRender Int Integer
  -- ^ 'BitVectorParam': width and value
  | BoolStringRender Bool
  -- ^ 'BoolStringParam'
  deriving (Show)

-- | A term level, post-normalization version of 'ClockPort', 'DiffClockPort',
-- 'NamedDiffClockPort', 'ResetPort', 'Port', 'BiSignalInPort',
-- 'BitBiSignalInPort', or 'BiSignalOutPort'. Used internally by this module.
data PrimPort a
  = PrimClockPort       { name :: Text, dom :: Text, meta :: a }
  | PrimDiffClockPort   { nameP :: Text, nameN :: Text, dom :: Text, meta :: a }
  | PrimResetPort       { name :: Text, dom :: Text, meta :: a, polarity :: ResetPolarity }
  | PrimSignalPort      { name :: Text, dom :: Text, meta :: a }
  | PrimBiSignalInPort  { name :: Text, dom :: Text, meta :: a, n :: Integer, bitPort :: Bool }
  -- ^ 'BiSignalInPort', or 'BitBiSignalInPort' if 'bitPort' is set
  | PrimBiSignalOutPort { name :: Text, dom :: Text, meta :: a, n :: Integer }
  deriving (Show, Functor)

-- | Add meta information to a 'PrimPort'
addPrimPortMeta :: PrimPort b -> a -> PrimPort a
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

-- | Interprets any of 'Param', 'ClockPort', 'DiffClockPort', 'NamedDiffClockPort',
-- 'ResetPort', or 'Port' into a single data type 'PrimPortOrParam'.
#if MIN_VERSION_clash_lib(1,11,0)
#define TERM_TO_DATA termToData#
#else
#define TERM_TO_DATA termToData
#endif
instance TermLiteral (PrimPortOrParam ()) where
  TERM_TO_DATA (collectDataArgs -> Just (constrName, args))
    | constrName == show 'Param
    , [Right (LitTy (SymTy nm)), Right ty, Left arg] <- args
    = do
      integerParam <-
        if isIntegerTy ty
        then Just <$> Compat.termToData arg
        else pure Nothing

      pure (MkPrimParam (PrimParam
        { paramName = Text.pack nm
        , paramMeta = ()
        , paramAsInteger = integerParam
        , paramRender = DefaultRender
        }))

    | constrName == show 'BitVectorParam
    , [Right (LitTy (SymTy nm)), Right _width, Left (stripAllTicks -> arg)] <- args
    = let
        param = "BitVectorParam " <> show nm
        expected = "a constant BitVector without undefined bits"
      in case termToBitVector arg of
        Just (width, 0, val) ->
          pure (MkPrimParam (PrimParam
            { paramName = Text.pack nm
            , paramMeta = ()
            , paramAsInteger = Nothing
            , paramRender = BitVectorRender (fromInteger width) val
            }))
        Just _ -> paramError param expected "a BitVector with undefined bits" "" arg
        Nothing -> nonLiteralParamError param expected arg

    | constrName == show 'BoolStringParam
    , [Right (LitTy (SymTy nm)), Left (stripAllTicks -> arg)] <- args
    = case Compat.termToData arg of
        Right b ->
          pure (MkPrimParam (PrimParam
            { paramName = Text.pack nm
            , paramMeta = ()
            , paramAsInteger = Nothing
            , paramRender = BoolStringRender b
            }))
        Left _ -> nonLiteralParamError ("BoolStringParam " <> show nm) "a constant Bool" arg

    | constrName == show 'Port
    , (LitTy (SymTy nm) : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimSignalPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}))

    | constrName == show 'ClockPort
    , (LitTy (SymTy nm) : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimClockPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}))

    | constrName == show 'DiffClockPort
    , (LitTy (SymTy nm) : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimDiffClockPort
        { nameP=(Text.pack nm <> "_p")
        , nameN=(Text.pack nm <> "_n")
        , dom=Text.pack domNm
        , meta=()
        }))

    | constrName == show 'NamedDiffClockPort
    , (LitTy (SymTy nmP) : LitTy (SymTy nmN) : LitTy (SymTy domNm) : _) <- rights args
    = pure (MkPrimPort (PrimDiffClockPort
        { nameP=(Text.pack nmP)
        , nameN=(Text.pack nmN)
        , dom=Text.pack domNm
        , meta=()
        }))

    | constrName == show 'BiSignalInPort
    , ( LitTy (SymTy nm)
      : ConstTy _dflt
      : LitTy (SymTy domNm)
      : widthTy
      : _
      ) <- rights args
    = pure (MkPrimPort (PrimBiSignalInPort
        { name=Text.pack nm
        , dom=Text.pack domNm
        , meta=()
          -- The width can be an unreduced type family application, such as
          -- @BitSize a@ when the port is passed down from a polymorphic
          -- function. We don't need the width of input ports, so we don't
          -- insist on a literal.
        , n = case widthTy of
            LitTy (NumTy width) -> width
            _ -> 0
        , bitPort = False
        }
      ))

    | constrName == show 'BitBiSignalInPort
    , ( LitTy (SymTy nm)
      : ConstTy _dflt
      : LitTy (SymTy domNm)
      : _
      ) <- rights args
    = pure (MkPrimPort (PrimBiSignalInPort
        { name=Text.pack nm
        , dom=Text.pack domNm
        , meta=()
        , n = 1
        , bitPort = True
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

  TERM_TO_DATA t = Left t

-- | Report an argument of a parameter that 'inst' can't render
paramError ::
  HasCallStack =>
  -- | Parameter, e.g. @BitVectorParam \"INIT\"@
  String ->
  -- | What 'inst' expected
  String ->
  -- | What the argument is instead
  String ->
  -- | Hint
  String ->
  -- | Argument
  Term ->
  a
paramError param expected what hint arg = error [I.i|
  #{param}: expected #{expected}, but got #{what}:

    #{showPpr arg}

  #{hint}
|]

-- | Report an argument of a parameter that 'inst' expected to be a literal
nonLiteralParamError ::
  HasCallStack =>
  -- | Parameter, e.g. @BitVectorParam \"INIT\"@
  String ->
  -- | What 'inst' expected
  String ->
  -- | Argument
  Term ->
  a
nonLiteralParamError param expected arg
  | (Prim PrimInfo{primName}, _) <- collectArgs arg
  , primName `P.elem` (undefinedPrims <> undefinedXPrims)
  = paramError param expected "an undefined value" "" arg
  | otherwise
  = paramError param expected "an expression Clash could not reduce to a literal" [I.i|
      If it is a constant, consider calculating it at compile time using
      Template Haskell, e.g. $(lift (...)).
    |] arg

-- | Interpret a term as a 'Clash.Explicit.Prelude.BitVector' literal:
-- @(width, mask, value)@.
termToBitVector :: Term -> Maybe (Integer, Integer, Integer)
termToBitVector t = case collectArgs t of
  (Prim PrimInfo{primName}, lefts -> [knownNat, mask, val])
    | primName == "Clash.Sized.Internal.BitVector.fromInteger#"
    , Right n <- Compat.termToData @Natural knownNat
    , Right m <- Compat.termToData @Natural mask
    , Right v <- Compat.termToData @Integer val
    -> Just (toInteger n, toInteger m, v)
  -- 'pack' wraps its result in 'xToBV' to catch 'XException's
  (Prim PrimInfo{primName}, lefts -> args@(_:_))
    | "xToBV" `Text.isSuffixOf` primName
    -> termToBitVector (stripAllTicks (P.last args))
  _ -> Nothing

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
  TERM_TO_DATA = $(deriveTermToData ''XilinxWizard)

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
-- result type. Each argument can be a 'Port', 'ClockPort', 'DiffClockPort',
-- 'NamedDiffClockPort', 'ResetPort', 'BiSignalInPort', 'BitBiSignalInPort',
-- 'Param', 'BitVectorParam', or 'BoolStringParam'. The result type, @r@ can be a
-- 'Port', 'ClockPort', 'DiffClockPort', 'NamedDiffClockPort', 'ResetPort',
-- 'BiSignalOutPort', a tuple of these, or unit (@()@).
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

instance KnownDomain dom => IsPort (DiffClockPort name dom) where
  type PortType (DiffClockPort name dom) = DiffClock dom
  unPort (DiffClockPort clk) = clk

instance KnownDomain dom => IsPort (NamedDiffClockPort nameP nameN dom) where
  type PortType (NamedDiffClockPort nameP nameN dom) = DiffClock dom
  unPort (NamedDiffClockPort clk) = clk

-- "Terminating" instances for 'Inst'
instance Inst () -- Inst with only inputs
instance KnownDomain dom => Inst (Port s dom a) -- Inst with a single port output
instance KnownDomain dom => Inst (ClockPort s dom) -- Inst with a single clock output
instance KnownDomain dom => Inst (ResetPort s polarity dom) -- Inst with a single reset output
instance KnownDomain dom => Inst (BiSignalOutPort s ds dom n) -- Inst with a single bi-signal output
instance KnownDomain dom => Inst (DiffClockPort s dom) -- Inst with a single differential clock output
instance KnownDomain dom => Inst (NamedDiffClockPort sP sN dom) -- Inst with a single differential clock output

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

instance Inst a => Inst (DiffClockPort s dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (NamedDiffClockPort sP sN dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (ResetPort s polarity dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (BiSignalInPort s ds dom n -> a) where
  instX !_i = instX @a

instance Inst a => Inst (BitBiSignalInPort s ds dom -> a) where
  instX !_i = instX @a

instance Inst a => Inst (Param s const -> a) where
  instX !_i = instX @a

instance Inst a => Inst (BitVectorParam s n -> a) where
  instX !_i = instX @a

instance Inst a => Inst (BoolStringParam s -> a) where
  instX !_i = instX @a

{- Note [inst and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'inst' returns its 'Inst' dictionary, so GHC infers that it is strict in it.
For an 'Inst' instance that terminates the polyvariadic chain, such as
@Inst (Port s dom a)@, the dictionary is the (newtype-wrapped) default 'instX',
i.e., 'errorX'. When 'inst' is not applied to any port or param, the
dictionary passed to it is exactly such a bottoming value. Seeing a strict
function applied to a definitely diverging argument, GHC's simplifier replaces
the whole call by that argument, before Clash ever sees the call to the
primitive. Clash then renders the "result" of 'inst' as an undefined value
instead of an instantiation. 'lazy' hides the strictness from GHC's demand
analysis.

See https://github.com/clash-lang/clash-cores/issues/80.
-}

-- | /Polyvariadic/ function representing the instantiation of a Verilog/VHDL
-- component. You can interpret this function as:
--
-- > inst :: Inst a => InstConfig -> a_0 -> a_1 -> ... -> a_n -> r
--
-- where @n@ is the number of arguments (which can be zero) and @r@ is the
-- result type. Each argument can be a 'Port', 'ClockPort', 'DiffClockPort',
-- 'NamedDiffClockPort', 'ResetPort', 'BiSignalInPort', 'BitBiSignalInPort',
-- 'Param', 'BitVectorParam', or 'BoolStringParam'. The result type, @r@ can be a
-- 'Port', 'ClockPort', 'DiffClockPort', 'NamedDiffClockPort', 'ResetPort',
-- 'BiSignalOutPort', a tuple of these, or unit (@()@). Like arguments, a
-- 'DiffClockPort' or 'NamedDiffClockPort' in the result maps to two HDL ports:
-- one for the positive and one for the negative phase.
--
-- Ports with a zero-width type, such as a 'BiSignalOutPort' or a 'Port' carrying
-- @()@, have no HDL representation and are left out of the instantiation. If an
-- instantiation has no other output ports, it is only rendered if 'renderVoid'
-- is set.
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
--
-- __NB__: An instantiation without any input ports, or one whose inputs are all
-- constants, is a constant term as far as Clash is concerned. If its result is
-- used more than once at the top level of a design (e.g., both elements of a
-- tuple result are projected out in the top entity itself), Clash might render
-- one instance per use. Defining the instantiation in a function that is
-- polymorphic in its domain (i.e., one with a t'KnownDomain' constraint) and
-- calling that function once avoids this.
inst :: forall a. Inst a => InstConfig -> a
inst !_ = lazy instX -- See Note [inst and strictness]
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
instWithXilinxWizard# !_ !_ = lazy instX -- See Note [inst and strictness]
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
  arg <- Compat.termToDataError t
  args <- argsToPrimPortOrParams ts
  pure (arg:args)

-- | Interpret result type as 'PrimPort's. Each port is paired with its type,
-- e.g. @Port "foo" dom Bool@.
tyToPrimPort :: Type -> Either String [(PrimPort (), Type)]
tyToPrimPort ty@(splitTyConAppM -> Just (tyConName'@(Name{nameOcc=Text.unpack -> tyConName}), args))
    | tyConName == show 'Param
    , LitTy (SymTy nm) : _ <- args
    = Left ("Can't translate Param " <> nm <> " to PrimPort. Did you define a Param in a result type?")

    | tyConName == show 'Port
    , LitTy (SymTy nm) : LitTy (SymTy domNm) : _ <- args
    = Right [(PrimSignalPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}, ty)]

    | tyConName == show 'ClockPort
    , LitTy (SymTy nm) : LitTy (SymTy domNm) : _ <- args
    = Right [(PrimClockPort{name=Text.pack nm, dom=Text.pack domNm, meta=()}, ty)]

    | tyConName == show 'DiffClockPort
    , LitTy (SymTy nm) : LitTy (SymTy domNm) : _ <- args
    = Right [(PrimDiffClockPort
        { nameP=Text.pack nm <> "_p"
        , nameN=Text.pack nm <> "_n"
        , dom=Text.pack domNm
        , meta=()
        }, ty)]

    | tyConName == show 'NamedDiffClockPort
    , LitTy (SymTy nmP) : LitTy (SymTy nmN) : LitTy (SymTy domNm) : _ <- args
    = Right [(PrimDiffClockPort
        { nameP=Text.pack nmP
        , nameN=Text.pack nmN
        , dom=Text.pack domNm
        , meta=()
        }, ty)]

    | tyConName == show 'ResetPort
    , LitTy (SymTy nm) : pol : LitTy (SymTy domNm) : _ <- args
    = Right [(PrimResetPort
        { name=Text.pack nm
        , dom=Text.pack domNm
        , meta=()
        , polarity = getPolarity nm pol
        }, ty)]

    | tyConName == show 'BiSignalOutPort
    , LitTy (SymTy nm) : _ds : LitTy (SymTy domNm) : LitTy (NumTy width) : _ <- args
    = Right [(PrimBiSignalOutPort
        { name=Text.pack nm
        , dom=Text.pack domNm
        , meta=()
        , n = width
        }, ty)]

    | isTupleTyConLike tyConName' = do
      xs <- mapM tyToPrimPort args
      pure $ P.concat xs
tyToPrimPort ty = error (ppShow ty)

-- | Strip any 'Annotated' constructors from a 'HWType'
stripAnnotated :: HWType -> HWType
stripAnnotated (Annotated _ ty) = stripAnnotated ty
stripAnnotated ty = ty

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
  : (either error id . Compat.termToDataError -> n)
  : wizardAsTerm
  : _config
  : _userArgs <- lefts args
  = withSomeSNat n $ \(SNat :: SNat n)-> do
      let
        !(wizard :: XilinxWizard n)= either error id $ Compat.termToDataError wizardAsTerm

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
      tcm <- Lens.view tcCache
      -- Reduce type families in the result type, such as the width @BitSize a@
      -- of a 'BiSignalOutPort' passed up from a polymorphic function
      case go config userArgs (normalizeType tcm resTy) of
        Left s -> error ("instBBF, bad context:\n\n" <> s)
        Right (c, a, r0) -> do
          -- Determine which output ports have a zero-width type, see
          -- 'instBBTF'.
          r1 <- mapM
            (\(p, ty) -> addPrimPortMeta p . isVoid <$> unsafeCoreTypeToHWTypeM' "instBBF" ty)
            r0
          pure $ Right (bbMeta hdl c, bb doms c a r1)
 where
  go :: Term -> [Term] -> Type -> Either String (InstConfig, [PrimPortOrParam ()], [(PrimPort (), Type)])
  go config0 userArgs resTy1 = do
    config1 <- Compat.termToDataError config0
    argPorts <- argsToPrimPortOrParams userArgs
    resPorts <- tyToPrimPort resTy1
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

  bb :: DomainMap -> InstConfig -> [PrimPortOrParam ()] -> [PrimPort Bool] -> BlackBox
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
  -- | Output ports, annotated with whether they have a zero-width type
  [PrimPort Bool] ->
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
          -name #{wizardName} #{backslash}
          -vendor #{wizardVendor} #{backslash}
          -library #{wizardLibrary} #{backslash}
          -version #{wizardVersion} #{backslash}
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
  wizardName = Text.pack xilinxWizard.wiz_name
  wizardVendor = Text.pack xilinxWizard.wiz_vendor
  wizardLibrary = Text.pack xilinxWizard.wiz_library
  wizardVersion = Text.pack xilinxWizard.wiz_version
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
  -- | Output ports, annotated with whether they have a zero-width type
  [PrimPort Bool] ->
  BlackBoxContext ->
  State s Doc
instBBTF nExtraArgs doms config maybeXilinxWizard primArgs0 primResults0 bbCtx
  | (   _instConstraint
      : _instConfig
      : userArgs
      ) <- P.drop nExtraArgs $ P.map fst $ DSL.tInputs bbCtx
  , [resultTy] <- DSL.tResults bbCtx
  = do
      hdl <- gets hdlKind
      DSL.declarationReturn bbCtx "inst_block" $ do
        let
          -- Output ports with a zero-width type (e.g. 'BiSignalOutPort', or a
          -- 'Port' carrying '()') don't exist in HDL: Clash removes them from
          -- the result type. Leave them out of the port map too.
          primResults1 = P.filter (not . meta) primResults0
          resultHwTy = DSL.ety resultTy

          (outPortTys, mkResult) = case primResults1 of
            [] -> ([], const [])
            -- If a single non-zero-width output is left, Clash represents the
            -- result by the type of that output, even if it is part of a tuple.
            [_] -> ([resultHwTy], P.id)
            _ | Product _ _ tys <- stripAnnotated resultHwTy ->
              -- Note that we don't have to check _what_ kind of product we're
              -- splitting here: the type classes enforce this can only be a
              -- tuple.
              (tys, \es -> [DSL.constructProduct (stripAnnotated resultHwTy) es])
            _ -> ([resultHwTy], P.id) -- Reported by the length check below

        when (P.length primResults1 /= P.length outPortTys) $
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

        (P.concat -> outPorts, outExprs) <-
          P.unzip <$> mapM mkOutPort (P.zipWith addPrimPortMeta primResults1 outPortTys)
        let
          -- Note that this critically depends on the Clash Netlist backed treating
          -- a data type with a single constructor with a single field as the
          -- same type as the field. I.e., @hwTy (Port a) ~ hwTy a@.
          primArgs1 = P.zipWith addPrimPortOrParamMeta primArgs0 userArgs
          (params0, inPorts0) = partitionPortOrPrims primArgs1
          params1 = P.map (mkParam hdl) params0

        -- Like output ports, input ports with a zero-width type don't exist in
        -- HDL. Leave them out of the port map.
        inPorts1 <-
          P.filter (not . isVoid . DSL.ety . snd) . P.concat <$> mapM (mkInPort hdl) inPorts0
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

        pure (mkResult outExprs)
 where
  mkParam :: HDL -> PrimParam DSL.TExpr -> (Text, DSL.TExpr)
  mkParam hdl (PrimParam{paramName, paramMeta, paramAsInteger, paramRender}) =
    case (paramAsInteger, paramRender) of
      (Just i, _) -> (paramName, DSL.TExpr Integer (Literal Nothing (NumLit i)))
      (Nothing, DefaultRender) -> (paramName, paramMeta)
      (Nothing, BitVectorRender n v)
          -- Clash renders a literal of type @Vec n Bit@ as an unqualified bit
          -- string literal in VHDL, which fits both @bit_vector@ and
          -- @std_logic_vector@ generics.
        | hdl == VHDL ->
            (paramName, DSL.TExpr (BitVector n) (Literal (Just (Vector n Bit, n)) (NumLit v)))
        | otherwise ->
            (paramName, DSL.TExpr (BitVector n) (Literal (Just (BitVector n, n)) (NumLit v)))
      (Nothing, BoolStringRender b)
        | hdl == VHDL -> (paramName, DSL.TExpr Bool (Literal Nothing (BoolLit b)))
        | otherwise ->
            (paramName, DSL.TExpr String (Literal Nothing (StringLit (if b then "TRUE" else "FALSE"))))

  mkInPort :: HDL -> PrimPort DSL.TExpr -> State (DSL.BlockState s) [(Text, DSL.TExpr)]
  mkInPort hdl = \case
    PrimDiffClockPort{nameP, nameN, meta=diffClkExpr} -> do
      clockPN <- DSL.deconstructProduct diffClkExpr [nameP, nameN]
      let
        (clockP, clockN) = case clockPN of
          [p, n] -> (p, n)
          _ -> error $ "mkInPort: Internal error: expected 2 elements: " <> show clockPN

      pure
        [ (nameP, clockP)
        , (nameN, clockN)
        ]
    PrimResetPort{name, polarity, dom, meta=rstIn} -> do
      rst <- case HashMap.lookup dom doms of
              Just (vResetPolarity -> domPolarity)
                | polarity == domPolarity -> pure rstIn
                | otherwise -> DSL.notExpr name rstIn
              Nothing ->
                error ("Internal error: could not find domain " <> Text.unpack dom)
      pure [(name, rst)]
    PrimBiSignalInPort{name, bitPort, meta=DSL.TExpr ty e} ->
      -- 'DSL.instDecl' assigns any expression that is not an identifier to an
      -- intermediate signal first. That would turn the inout port into an
      -- input, so make sure we pass the identifier of the bidirectional signal.
      case stripNewtypeDataCon e of
        Identifier i Nothing
          | bitPort, hdl == VHDL ->
              -- Connect the single element of the @std_logic_vector(0 downto
              -- 0)@. The VHDL backend has no modifier rendering as an element
              -- of a vector without converting it, which would make it an
              -- expression instead of a name, so we write the name ourselves.
              pure [(name, DSL.TExpr Bit (Identifier (Id.unsafeMake (Id.toText i <> "(0)")) Nothing))]
        e1
          | bitPort, hdl == VHDL ->
              error [I.i|
                BitBiSignalInPort #{show name}: expected the bidirectional
                signal to be an identifier, but got:

                  #{ppShow e1}
              |]
          | otherwise ->
              pure [(name, DSL.TExpr ty e1)]
    p -> pure [(name p, meta p)]

  -- The ports passed to 'inst' are wrapped in data types with a single
  -- constructor with a single field, such as 'BiSignalInPort'. Clash
  -- represents the application of such a constructor as a 'DataCon' with a
  -- special 'DC', which the backends render as just its field.
  stripNewtypeDataCon :: Expr -> Expr
  stripNewtypeDataCon = \case
    DataCon _ (DC (Void Nothing, -1)) [e] -> stripNewtypeDataCon e
    e -> e

  -- | Declare signals for the HDL port(s) of an output port. Returns the port
  -- map entries and the expression for the corresponding (tuple element of
  -- the) result.
  mkOutPort ::
    PrimPort HWType ->
    State (DSL.BlockState s) ([(Text, DSL.TExpr)], DSL.TExpr)
  mkOutPort = \case
    PrimDiffClockPort{nameP, nameN, meta=ty} ->
      case stripAnnotated ty of
        diffClkTy@(Product _ _ [clkTyP, clkTyN]) -> do
          clkP <- DSL.declare nameP clkTyP
          clkN <- DSL.declare nameN clkTyN
          diffClk <- DSL.assign "diff_clock" (DSL.constructProduct diffClkTy [clkP, clkN])
          pure ([(nameP, clkP), (nameN, clkN)], diffClk)
        _ ->
          error ("Internal error: unexpected type for differential clock: " <> show ty)

    PrimBiSignalOutPort{} ->
      error "Internal error: unreachable code: zero-width PrimBiSignalOutPort in mkOutPort"

    PrimResetPort{name, polarity, dom, meta=ty} -> do
      var <- DSL.declare name ty
      rst <-
        case HashMap.lookup dom doms of
          Just (vResetPolarity -> domPolarity)
            | polarity == domPolarity -> pure var
            | otherwise -> DSL.notExpr name var
          Nothing ->
            error ("Internal error: could not find domain " <> Text.unpack dom)
      pure ([(name, var)], rst)

    port -> do
      let ty = meta port
      var <- DSL.declare (name port) ty
      pure ([(name port, var)], var)

instBBTF _nExtraArgs _doms _config _maybeXilinxWizard _primArgs _primResults bbCtx =
  error (show 'instBBTF <> ", bad bbCtx:\n\n" <> ppShow bbCtx)

#if !MIN_VERSION_clash_prelude(1,9,0)
onSomeBackend :: (forall b. Backend b => b -> a) -> SomeBackend -> a
onSomeBackend f (SomeBackend b) = f b

fromSomeBackend :: (forall b. Backend b => b -> a) -> Lens.Getter SomeBackend a
fromSomeBackend f = Lens.to (onSomeBackend f)
#endif
