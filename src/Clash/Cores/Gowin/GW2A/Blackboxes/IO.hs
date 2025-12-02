{-|
  Copyright   :  (C) 2020, Foamspace corp & Christoph Mayer
                     2022, QBayLogic B.V.
                     2025, Stijn Dijkstra @ QBayLogic B.V.
  License     :  BSD2

  GOWIN GW2A IO primitives, adapted from LATTICE ECP5 IO primitives made by Christoph Mayer.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Gowin.GW2A.Blackboxes.IO (bbTF) where

import           Clash.Backend
import           Clash.Netlist.BlackBox.Util
import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.Types
import           Control.Monad.State             (State())
import           Data.Monoid                     (Ap(getAp))
import           Data.Text as TextS
import           Data.Text.Prettyprint.Doc.Extra
import           Prelude

-- | Generates HDL for ECP5 bidirectional buffer BB
--   FPGA TN 02032 sysIO
--     BB buf7 (.I(Q_out7), .T(Q_tri7), .O(buf_Data7), .B(Data[7]));
bbTF :: TemplateFunction
bbTF = TemplateFunction used valid bbTemplate
 where
  used = [3..6]
  valid = const True

bbTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
bbTemplate bbCtx
  | [  _HasCallStack
    , _HasBiSignalDefault
    , _KnownDomain
    , (intrinsicName, String, _)
    , (packagePin, packagePinTy, _)
    , (dOut, Bit, _)
    , (outputEnable, Bool, _)
    ] <- bbInputs bbCtx
  , Just compName' <- exprToString intrinsicName
  , [(Identifier result Nothing,_)] <- bbResults bbCtx
  = do
  bb      <- Id.makeBasic "bb"
  bb_inst <- Id.makeBasic "bb_inst"
  dIn     <- Id.makeBasic "dIn"

  compName <- Id.addRaw (TextS.pack compName')

  getAp $ blockDecl bb $
    [ NetDecl Nothing dIn Bit
    , InstDecl Comp Nothing [] compName bb_inst
      [
      ]
      (NamedPortMap
        [ -- NOTE: Direction is set to 'In', but will be rendered as inout due to
          -- its type packagePinTy
          (instPort "IO", In, packagePinTy, packagePin)
        , (instPort "OEN", In,  Bool, outputEnable)
        , (instPort "I", In,  Bit, dOut)
        , (instPort "O", Out, Bit, Identifier dIn Nothing)
        ])
    , Assignment result Cont (Identifier dIn Nothing)
    ]
 where
  instPort pn = Identifier (Id.unsafeMake pn) Nothing

bbTemplate bbCtx = error ("GW2A.bidirectionalBuffer, bad bbCtx: " <> show bbCtx)
