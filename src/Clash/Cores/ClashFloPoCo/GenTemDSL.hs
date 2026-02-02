module Clash.Cores.ClashFloPoCo.GenTemDSL
    ( getPipeDep
    , flopocoPrim
    , genFloPoCoInfoEntity
    , genBlackBoxFunction
    , genTemplateFunction
    , genBlackBoxTemplateFunction
    , genBlackBox
    -- Temporarily disabled: genBlackBoxProd has TH errors
    -- , genBlackBoxProd
    ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromMaybe )
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Text.PrettyPrint.HughesPJClass ( Pretty(..) )
import qualified Clash.Backend
import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.BlackBox.Types
import qualified Data.Text.Prettyprint.Doc.Extra
import qualified Prelude
import           Prelude                        ( error, Maybe(..), Int, String, length, map, toEnum, fromEnum, toInteger, maybe, (>=), (&&), (<=), otherwise, (+), (-), Char, Bool(..), (<>), (++), (||), return )
import           Text.Show.Unicode              ( ushow )
import qualified Clash.Cores.ClashFloPoCo.DSL as DSL
import           Clash.Cores.ClashFloPoCo.DSL  ( InfoEntity(..) )
import           Clash.Netlist.Types (BlackBoxContext, TemplateFunction(..))
import           Clash.Netlist.BlackBox.Types (BlackBoxFunction, emptyBlackBoxMeta)
import qualified Clash.Netlist.Id as Id
import qualified Clash.Cores.ClashFloPoCo.NT   as NT
import qualified Data.List.NonEmpty             as NE
import           GHC.Stack                      ( HasCallStack )
import Data.Text (Text)
import Prelude (Either(..), Bool(..), const, zip, otherwise, ($))

-- | Extract pipeline depth from InfoEntity and generate a type-level Nat
--
-- ==== __Example:__
-- @
-- infoEn = InfoEntity {name = Just "adder", pipedep = Just 3, ...}
-- type N = $(getPipeDep infoEn)  -- N ~ 3
-- @
getPipeDep :: InfoEntity -> Q Type
getPipeDep infoen = case DSL.pipedep infoen of
  Just n -> litT (numTyLit (toInteger n))
  Nothing -> error "InfoEntity has no pipeline depth specified"

-- Placeholder implementations for other exported functions
-- These need to be implemented based on actual requirements

flopocoPrim :: a
flopocoPrim = error "flopocoPrim: Not yet implemented"

genFloPoCoInfoEntity :: a
genFloPoCoInfoEntity = error "genFloPoCoInfoEntity: Not yet implemented"

genBlackBoxFunction :: a
genBlackBoxFunction = error "genBlackBoxFunction: Not yet implemented"

genTemplateFunction :: a
genTemplateFunction = error "genTemplateFunction: Not yet implemented"

genBlackBoxTemplateFunction :: a
genBlackBoxTemplateFunction = error "genBlackBoxTemplateFunction: Not yet implemented"

genBlackBox :: a
genBlackBox = error "genBlackBox: Not yet implemented"

-- Helper functions used by genBlackBoxProd

lengthMaybeStrings :: Maybe [a] -> Int
lengthMaybeStrings = maybe 0 length

toLowercaseList :: [String] -> [String]
toLowercaseList = map (map toLower)
  where
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c



-- | Generate a BlackBoxTemplateFunction, TemplateFunction and BlackBoxFunction
--
--  Please see the source file if cannot see the content of the example
--
-- ==== __Example:__
--
-- |
-- @
-- infoen = InfoEntity {name = Just "vga_controller",
--          freq = Nothing,
--          pipedep = Nothing,
--          insig = Just ["clk_100MHz", "reset"],
--          outsig = Just  ["video_on", "hsync", "vsync", "p_tick", "x", "y"] }
-- \$(genBlackBoxProd infoen)
-- @
--
-- is equivalent to
--
-- @
-- \$(genBlackBoxTemplateFunctionProd infoen)
-- \$(genTemplateFunction infoen)
-- \$(genBlackBoxFunction infoen)
-- @
--
-- which will generate this code during compile time
--
-- @
-- vga_controllerBBTF ::
--       forall s. Backend s => Text -> BlackBoxContext -> State s Doc
-- vga_controllerBBTF entityName bbCtx
--       | [clk_100MHz, reset] <- L.map fst (DSL.tInputs bbCtx),
--         [result] <- DSL.tResults bbCtx,
--         NT.Product _ _ resTyps <- DSL.ety result
--       = do vga_controllerInstName <- Id.makeBasic "vga_controller_inst"
--            let compInps
--                  = [("clk_100MHz", DSL.ety clk_100MHz), ("reset", DSL.ety reset)]
--                compOuts
--                  = L.zip ["video_on", "hsync", "vsync", "p_tick", "x", "y"] resTyps
--            (DSL.declarationReturn bbCtx "vga_controller_inst_block"
--               $ (do declares <- mapM
--                                   (\ (outname, typ) -> DSL.declare outname typ)
--                                   (L.zip ["video_on", "hsync", "vsync", "p_tick", "x", "y"] resTyps)
--                     let [video_on, hsync, vsync, p_tick, x, y] = declares
--                     let inps = [("clk_100MHz", clk_100MHz), ("reset", reset)]
--                         outs
--                           = [("video_on", video_on), ("hsync", hsync), ("vsync", vsync),
--                              ("p_tick", p_tick), ("x", x), ("y", y)]
--                     DSL.compInBlock entityName compInps compOuts
--                     DSL.instDecl
--                       Empty (Id.unsafeMake entityName) vga_controllerInstName [] inps
--                       outs
--                     pure
--                       [DSL.constructProduct
--                          (DSL.ety result) [video_on, hsync, vsync, p_tick, x, y]]))
--       | otherwise = error (ppShow bbCtx)
-- vga_controllerTF :: HasCallStack => Text -> TemplateFunction
-- vga_controllerTF entityName
--       = TemplateFunction
--           [0, 1, 2] (const True) (vga_controllerBBTF entityName)
-- vga_controllerBBF :: BlackBoxFunction
-- vga_controllerBBF _ _ _ _
--       = pure
--           (Right
--              (emptyBlackBoxMeta {bbKind = TDecl},
--               BBFunction
--                 "vga_controllerTF" 0 (vga_controllerTF "vga_controller")))
-- @

{- NOTE: genBlackBoxProd is currently incomplete and has TH generation issues.
   It needs to be rewritten to properly generate the blackbox functions.
   For now, use the manual blackbox approach shown in FloPoCoExample.hs

genBlackBoxProd :: InfoEntity -> Q [Dec]
genBlackBoxProd infoen = error "genBlackBoxProd: Not yet implemented correctly"
-}
    -- Extract entity and signal names
    let entityNamestr = fromMaybe "" (name infoen)
        bbtfName = mkName (entityNamestr <> "BBTF")
        tfName = mkName (entityNamestr <> "TF")
        bbfName = mkName (entityNamestr <> "BBF")
        s = mkName "s"
        lensignal = lengthMaybeStrings (insig infoen)
        inputNamesListstr = fromMaybe [] (insig infoen)
        outputNamesListstr = fromMaybe [] (outsig infoen)
        inputNamesList = map mkName (toLowercaseList inputNamesListstr)
        outputNamesList = map mkName (toLowercaseList outputNamesListstr)
        entityNameName = mkName "entityName"
        bbCtx = mkName "bbCtx"
        result = mkName "result"
        resTyps = mkName "resTyps"
        declares = mkName "declares"
        outname = mkName "outname"
        typ = mkName "typ"
        entityNameInstName = mkName (entityNamestr <> "InstName")
        entityNameInststr = entityNamestr <> "_inst"
        entityNameInstBlockstr = entityNamestr <> "_inst_block"
        compInps = mkName "compInps"
        compOuts = mkName "compOuts"
        inps = mkName "inps"
        outs = mkName "outs"
        tfNamestr = entityNamestr <> "TF"
    -- TH quotes for type signatures and function bodies
    bbtffunSig <- sigD bbtfName [t| forall s. Clash.Backend.Backend s => Text -> Clash.Netlist.Types.BlackBoxContext -> State s Data.Text.Prettyprint.Doc.Extra.Doc |]
    tffuncSig  <- sigD tfName [t| HasCallStack => Text -> TemplateFunction |]
    bbffuncSig <- sigD bbfName [t| BlackBoxFunction |]

    bbtffunDec <- funD bbtfName [clause [varP entityNameName, varP bbCtx]
        (guardedB
            [ (patG
                [ bindS (listP (map varP inputNamesList)) (appE (appE (varE 'Prelude.map) (varE 'Prelude.fst)) (appE (varE 'DSL.tInputs) (varE bbCtx))),
                  bindS (listP [varP result]) (appE (varE 'DSL.tResults) (varE bbCtx)),
                  bindS (conP 'NT.Product [] [wildP, wildP, varP resTyps]) (appE (varE 'DSL.ety) (varE result))
                ],
                doE
                    [ bindS (varP entityNameInstName) (appE (varE 'Id.makeBasic) (litE (stringL entityNameInststr))),
                      letS
                        [ valD (varP compInps) (normalB (listE [tupE [litE (stringL x), appE (varE 'DSL.ety) (varE y)] | (x, y) <- zip inputNamesListstr inputNamesList])) [],
                          valD (varP compOuts) (normalB (appE (appE (varE 'zip) (listE [litE (stringL x) | x <- outputNamesListstr])) (varE resTyps))) []
                        ],
                          noBindS (infixE (Just (appE (appE (varE 'DSL.declarationReturn) (varE bbCtx)) (litE (stringL entityNameInstBlockstr)))) (varE '($)) (Just (doE
                        [ bindS (varP declares) (appE (appE (varE 'Prelude.mapM) (lamE [tupP [varP outname, varP typ]] (appE (appE (varE 'DSL.declare) (varE outname)) (varE typ)))) (appE (appE (varE 'Prelude.zip) (listE [litE (stringL x) | x <- outputNamesListstr])) (varE resTyps))),
                          letS [valD (listP (map varP outputNamesList)) (normalB (varE declares)) []],
                          letS [ valD (varP inps) (normalB (listE [tupE [litE (stringL x), varE y] | (x, y) <- zip inputNamesListstr inputNamesList])) [],
                                  valD (varP outs) (normalB (listE [tupE [litE (stringL x), varE y] | (x, y) <- zip outputNamesListstr outputNamesList])) [] ],
                          noBindS (appE (appE (appE (varE 'DSL.compInBlock) (varE entityNameName)) (varE compInps)) (varE compOuts)),
                          noBindS (appE (appE (appE (appE (appE (appE (varE 'DSL.instDecl) (conE 'N.Empty)) (appE (varE 'Id.unsafeMake) (varE entityNameName))) (varE entityNameInstName)) (conE '[])) (varE inps)) (varE outs)),
                          noBindS (appE (varE 'pure) (listE [appE (appE (varE 'DSL.constructProduct) (appE (varE 'DSL.ety) (varE result))) (listE (map varE outputNamesList))]))
                        ])))
                    ]
                ),
              (normalG (varE 'otherwise), appE (varE 'error) (appE (varE 'ushow) (varE bbCtx)))
            ]
        )
        []
      ]

    tffuncDec <- funD tfName [clause [varP entityNameName]
        (normalB (appE (appE (appE (conE 'N.TemplateFunction) (listE [litE (integerL (toInteger i)) | i <- [0 .. lensignal]])) (appE (varE 'const) (conE 'True))) (appE (varE bbtfName) (varE entityNameName)))) []]

    bbffuncDec <- funD bbfName
      [clause [wildP, wildP, wildP, wildP]
        (normalB (appE (varE 'pure) 
          (appE (conE 'Right) 
            (tupE [Just $ recUpdE (varE 'emptyBlackBoxMeta) [fieldExp (mkName "bbKind") (conE 'Clash.Netlist.BlackBox.Types.TDecl)],
                   Just $ appE (appE (appE (conE 'N.BBFunction) (litE (stringL tfNamestr))) (litE (integerL 0))) (appE (varE tfName) (litE (stringL entityNamestr)))]))))
        []
      ]

    return [bbtffunSig, tffuncSig, bbffuncSig, bbtffunDec, tffuncDec, bbffuncDec]
