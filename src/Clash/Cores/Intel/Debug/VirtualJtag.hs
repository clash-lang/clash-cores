{-|
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for Intel's Virtual JTAG IP (@sld_virtual_jtag@), the Intel
counterpart of Xilinx's @BSCANE2@: a JTAG data register in the fabric, which a
JTAG host (such as @quartus_stp@ or System Console) can scan. See the
[Virtual JTAG Intel FPGA IP Core User Guide (UG-SLDVRTL)](https://www.intel.com/content/www/us/en/docs/programmable/683705/).

Unlike @BSCANE2@, the IP doesn't share the JTAG TAP controller's instruction
register with the design: it gives every instance its own /virtual/
instruction register ('vjIrIn'). A virtual IR scan writes it, after which
virtual DR scans scan the data register that the design selects with it.
Quartus connects all instances to its JTAG hub (@sld_hub@).

= Simulation

Intel's simulation models of the IP (in @altera_mf@) simulate a JTAG host,
which performs a list of scans that is given as a parameter. 'virtualJtag'
passes the scans it is given to the HDL instance, and has a Clash simulation
model that performs the same scans: in Clash simulation, the outputs are the
same as in HDL simulation, cycle by cycle of TCK. This was tested against the
Verilog and VHDL models of Quartus Prime Lite 21.1 (@altera_mf@). The scans
only affect simulation: on hardware, the JTAG host performs the scans.

Like the HDL models, the Clash model:

  * starts by resetting the TAP controller and loading the @USER1@
    instruction, and resets it again after the scans. After that, the TAP
    controller stays in Test-Logic-Reset.
  * performs every IR scan as two DR scans with the @USER1@ instruction, and
    every DR scan with the @USER0@ instruction, switching between them when
    needed.
  * requires the first scan to be an 'IrScan': like on hardware, a virtual
    DR scan needs a virtual IR scan to select the instance first. (The HDL
    models would perform a first DR scan as an IR scan.)
  * ignores the IR value and TDO that the design returns: the host doesn't
    look at what it scans out.

The HDL models stop TCK during an 'Idle' scan and after all scans. Clash can't
simulate a clock that stops: in Clash simulation, TCK runs continuously, and
the TAP controller stays in its state while the host is idle (Run-Test/Idle,
Update-DR or Update-IR, or Test-Logic-Reset after the scans).

TCK of the HDL models has a period of 100 ns (10 MHz). On hardware, the
frequency of TCK depends on the JTAG cable.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Intel.Debug.VirtualJtag
  ( virtualJtag
  , VirtualJtag(..)
  , VirtualJtagScan(..)
  , VirtualJtagState(..)
    -- * Internals
  , virtualJtag#
  , virtualJtagBBF
  , virtualJtagTF
  ) where

import Clash.Explicit.Prelude hiding (withSomeSNat)
import qualified Prelude as P

import Control.Lens (use)
import Control.Monad.State (State)
import Data.Either (lefts)
import Data.String.Interpolate (__i)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import qualified Data.Text.Lazy as LazyText

import Clash.Annotations.Primitive (Primitive(InlineYamlPrimitive), HDL(..))
import Clash.Backend (Backend, hdlKind)
import Clash.Netlist.BlackBox.Types
  ( BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TDecl), emptyBlackBoxMeta )
import Clash.Netlist.Types
  ( BlackBox(BBFunction), BlackBoxContext(..), EntityOrComponent(Empty)
  , HWType(..), TemplateFunction(..), backend, fromSomeBackend )

import qualified Clash.Netlist.BlackBox.Types as BlackBox
import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL

import qualified Clash.Core.TermLiteral.Compat as Compat
import Clash.Cores.Xilinx.Xpm.Cdc.Internal (withSomeSNat)
import Clash.Cores.Intel.Debug.VirtualJtag.Internal

-- | Outputs of 'virtualJtag'
data VirtualJtag tck n = VirtualJtag
  { vjTck :: Clock tck
  -- ^ JTAG clock (@tck@). Use it for the logic that implements the virtual
  -- data registers.
  , vjTdi :: Signal tck Bit
  -- ^ Data to shift into a virtual data register in 'ShiftDr' (@tdi@)
  , vjIrIn :: Signal tck (BitVector n)
  -- ^ Virtual instruction register (@ir_in@). It changes in the cycle before
  -- 'UpdateIr', and is reset to zero in Test-Logic-Reset.
  , vjState :: Signal tck (Maybe VirtualJtagState)
  -- ^ State of the virtual JTAG state machine (@virtual_state_*@): the state
  -- of the TAP controller during a virtual IR or DR scan, 'Nothing' otherwise
  , vjTestLogicReset :: Signal tck Bool
  -- ^ Whether the TAP controller is in Test-Logic-Reset (@jtag_state_tlr@),
  -- which resets the virtual instruction register. It can reset the logic of
  -- the virtual data registers too.
  }

{- | Virtual JTAG. Instantiates a virtual JTAG node with an instruction
register of @n@ bits (1 to 24, @sld_ir_width@).

The design implements the virtual data registers in the domain of
'vjTck', for example:

@
-- A 16 bit data register: capture the input, shift, and output what was shifted in
register16 ::
  'KnownDomain' tck =>
  'VirtualJtag' tck 2 ->
  'Signal' tck ('BitVector' 16) ->
  ('Signal' tck 'Bit', 'Signal' tck ('BitVector' 16))
register16 'VirtualJtag'{..} input = (lsb '<$>' shiftReg, output)
 where
  shiftReg = 'register' vjTck 'noReset' 'enableGen' 0 (go '<$>' vjState '<*>' vjTdi '<*>' input '<*>' shiftReg)
  go ('Just' 'CaptureDr') _ i _ = i
  go ('Just' 'ShiftDr') tdi _ sr = tdi '+>>.' sr
  go _ _ _ sr = sr
  output = 'regEn' vjTck 'noReset' 'enableGen' 0 ((== 'Just' 'UpdateDr') '<$>' vjState) shiftReg
@

where the TDO of the register is passed to 'virtualJtag' (for every virtual
instruction, select which register drives TDO), as in:

@
let vj = 'virtualJtag' 'Nothing' scans ('pure' 0) tdo
    (tdo, output) = register16 vj input
@

The scans are performed in simulation (see the module documentation), they
don't affect hardware.
-}
virtualJtag ::
  forall tck n m.
  (KnownDomain tck, KnownNat n, 1 <= n, n <= 24, KnownNat m) =>
  -- | Instance index (@sld_instance_index@), with which JTAG hosts address the
  -- instance. 'Nothing' lets Quartus assign one.
  Maybe Natural ->
  -- | Scans to perform in simulation
  Vec m VirtualJtagScan ->
  -- | Value of the virtual instruction register to capture in a virtual IR
  -- scan (@ir_out@)
  Signal tck (BitVector n) ->
  -- | TDO of the selected virtual data register (@tdo@)
  Signal tck Bit ->
  VirtualJtag tck n
virtualJtag index scans irOut tdo =
  VirtualJtag
    { vjTck = pTck
    , vjTdi = pTdi
    , vjIrIn = pIrIn
    , vjState = decodeState <$> bundle states
    , vjTestLogicReset = bitToBool <$> pTlr
    }
 where
  VirtualJtagPorts{..} = virtualJtag# index scans irOut tdo
  states = pCdr :> pSdr :> pE1dr :> pPdr :> pE2dr :> pUdr :> pCir :> pUir :> Nil
{-# INLINE virtualJtag #-}

-- | Decode the @virtual_state_*@ outputs, in the order of 'VirtualJtagState'
decodeState :: Vec 8 Bit -> Maybe VirtualJtagState
decodeState bs = fold (<|>) (zipWith (\b s -> if bitToBool b then Just s else Nothing) bs (iterateI succ CaptureDr))

-- | Outputs of 'virtualJtag#': the output ports of @sld_virtual_jtag@
data VirtualJtagPorts tck n = VirtualJtagPorts
  { pTck :: Clock tck
  , pTdi :: Signal tck Bit
  , pIrIn :: Signal tck (BitVector n)
  , pCdr :: Signal tck Bit
  , pSdr :: Signal tck Bit
  , pE1dr :: Signal tck Bit
  , pPdr :: Signal tck Bit
  , pE2dr :: Signal tck Bit
  , pUdr :: Signal tck Bit
  , pCir :: Signal tck Bit
  , pUir :: Signal tck Bit
  , pTlr :: Signal tck Bit
  }

-- | Primitive for 'virtualJtag'
virtualJtag# ::
  forall tck n m.
  (KnownDomain tck, KnownNat n, KnownNat m) =>
  Maybe Natural ->
  Vec m VirtualJtagScan ->
  Signal tck (BitVector n) ->
  Signal tck Bit ->
  VirtualJtagPorts tck n
-- Note that the primitive must not be strict in its inputs: the design
-- computes them from its outputs.
virtualJtag# _index scans _irOut _tdo =
  VirtualJtagPorts
    { pTck = clockGen
    , pTdi = mcTdi <$> cycles
    , pIrIn = fromInteger . mcIrIn <$> cycles
    , pCdr = state CaptureDr
    , pSdr = state ShiftDr
    , pE1dr = state Exit1Dr
    , pPdr = state PauseDr
    , pE2dr = state Exit2Dr
    , pUdr = state UpdateDr
    , pCir = state CaptureIr
    , pUir = state UpdateIr
    , pTlr = boolToBit . (== TestLogicReset) . mcTap <$> cycles
    }
 where
  cycles = fromList_lazy (modelCycles (natToNum @n) (toList scans))
  state s = boolToBit . (== Just s) . mcState <$> cycles
{-# OPAQUE virtualJtag# #-}
{-# ANN virtualJtag# (
  let
    primName = show 'virtualJtag#
    tfName = show 'virtualJtagBBF
  in
    InlineYamlPrimitive [minBound..] [__i|
      BlackBoxHaskell:
        name: #{primName}
        templateFunction: #{tfName}
        workInfo: Always
    |]) #-}

-- | 'BlackBoxFunction' for 'virtualJtag#'
virtualJtagBBF :: HasCallStack => BlackBoxFunction
virtualJtagBBF _isD _primName args _resTys
  | [ _knownDomain, _knownNatN
    , (either error P.id . Compat.termToDataError -> m)
    , (either error P.id . Compat.termToDataError -> index)
    , scansTerm
    , _irOut, _tdo ] <- lefts args
  = withSomeSNat m $ \(SNat :: SNat m) -> do
      hdl <- use (backend . fromSomeBackend hdlKind)
      let
        scans :: Vec m VirtualJtagScan
        !scans = either error P.id (Compat.termToDataError scansTerm)
        vhdlOnly :: String -> [BlackBox.BlackBoxTemplate]
        vhdlOnly s
          | hdl == VHDL = [[BlackBox.Text (LazyText.pack s)]]
          | otherwise = []
        meta = emptyBlackBoxMeta
          { bbKind = TDecl
          , bbLibrary = vhdlOnly "altera_mf"
          , bbImports = vhdlOnly "altera_mf.altera_mf_components.all"
          }
      pure (Right (meta, BBFunction (show 'virtualJtagTF) 0 (virtualJtagTF index (toList scans))))
virtualJtagBBF _ _ args _ = error ("virtualJtag: unexpected arguments: " <> ppShow args)

-- | Template function for 'virtualJtag#'
virtualJtagTF :: HasCallStack => Maybe Natural -> [VirtualJtagScan] -> TemplateFunction
virtualJtagTF index scans = TemplateFunction [5, 6] (const True) (virtualJtagBBTF index scans)

virtualJtagBBTF ::
  forall s.
  (HasCallStack, Backend s) =>
  Maybe Natural ->
  [VirtualJtagScan] ->
  BlackBoxContext ->
  State s Doc
virtualJtagBBTF index scans bbCtx
  | [_, _, _, _, _, irOut, tdo] <- P.map fst (DSL.tInputs bbCtx)
  , [result] <- DSL.tResults bbCtx
  , Product _ _ (clockTy : _) <- DSL.ety result
  = do
      let
        irWidth = DSL.tySize (DSL.ety irOut) :: Int
        (action, nScan, totalLength) = simAction irWidth scans
        declareBit nm = DSL.declare nm Bit
      instName <- Id.makeBasic "virtual_jtag_inst"
      DSL.declarationReturn bbCtx "virtual_jtag_block" $ do
        tck <- DSL.declare "tck" clockTy
        tdi <- declareBit "tdi"
        irIn <- DSL.declare "ir_in" (BitVector irWidth)
        states <- mapM (declareBit . fst) stateNames
        tlr <- declareBit "tlr"
        irOutBv <- DSL.toBV "ir_out" irOut
        DSL.instDecl Empty (Id.unsafeMake "sld_virtual_jtag") instName
          [ ("lpm_type", DSL.litTExpr (DSL.S "sld_virtual_jtag"))
          , ("lpm_hint", DSL.litTExpr (DSL.S "UNUSED"))
          , ("sld_auto_instance_index", DSL.litTExpr (DSL.S (maybe "YES" (const "NO") index)))
          , ("sld_instance_index", DSL.litTExpr (DSL.I (maybe 0 toInteger index)))
          , ("sld_ir_width", DSL.litTExpr (DSL.I (toInteger irWidth)))
          , ("sld_sim_n_scan", DSL.litTExpr (DSL.I (toInteger nScan)))
          , ("sld_sim_total_length", DSL.litTExpr (DSL.I totalLength))
          , ("sld_sim_action", DSL.litTExpr (DSL.S action))
          ]
          [ ("tdo", tdo), ("ir_out", irOutBv) ]
          ( [ ("tck", tck), ("tdi", tdi), ("ir_in", irIn) ]
         <> P.zip (P.map snd stateNames) states
         <> [ ("jtag_state_tlr", tlr) ] )
        pure [DSL.constructProduct (DSL.ety result) ([tck, tdi, irIn] <> states <> [tlr])]
  | otherwise = error ("virtualJtag: unexpected arguments: " <> ppShow (DSL.tInputs bbCtx))
 where
  stateNames =
    [ (s, "virtual_state_" <> s)
    | s <- ["cdr", "sdr", "e1dr", "pdr", "e2dr", "udr", "cir", "uir"] ]
