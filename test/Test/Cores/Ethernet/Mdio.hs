{-# LANGUAGE NumericUnderscores #-}

module Test.Cores.Ethernet.Mdio (
  tests,
) where

import Clash.Cores.Ethernet.Mdio

import Clash.Prelude

import qualified Data.List as L

import qualified Data.Map as M

import Hedgehog (Gen, Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Protocols
import qualified Protocols.Df as Df
import Protocols.Hedgehog

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit), testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- | Generate a random MDIO bus request.
genMdioRequest :: Gen MdioRequest
genMdioRequest = do
  suppressPreamble <- Gen.enumBounded
  phyAddr <- Gen.enumBounded
  regAddr <- Gen.enumBounded
  isRead <- Gen.bool
  writeData <- Gen.enumBounded
  pure
    $ if isRead
      then MdioRead suppressPreamble phyAddr regAddr
      else MdioWrite suppressPreamble phyAddr regAddr writeData

{- |
Stateful model of an MDIO bus: 32 potential PHYs, each which contain
32 registers of 16-bit words. Reads to a non-existing PHY must result
in an error. There is no way to detect whether a write went to a
non-existing PHY, so a write acknowledgement is always expected.

Each write request updates the state, if the addressed PHY is present.
Subsequent reads to the same address must result in the updated value.
-}
mdioControllerModel ::
  -- | Present PHY addresses
  [BitVector 5] ->
  -- | Generated requests
  [MdioRequest] ->
  -- | Expected responses
  [MdioResponse]
mdioControllerModel phys = mdioControllerModel' M.empty
 where
  mdioControllerModel' ::
    M.Map (BitVector 10) (BitVector 16) ->
    [MdioRequest] ->
    [MdioResponse]
  mdioControllerModel' _ [] = []
  mdioControllerModel' st (req : rs) = resp : mdioControllerModel' nextSt rs
   where
    combinedAddr = mdioPhyAddress req ++# mdioRegAddress req
    phyIsPresent = mdioPhyAddress req `L.elem` phys

    -- Upon a write request, update the registers of the addressed PHY.
    -- But, only if it exists in the model.
    nextSt = case (req, phyIsPresent) of
      (MdioWrite _ _ _ writeData, True) -> M.insert combinedAddr writeData st
      _ -> st

    resp = case (req, phyIsPresent, M.lookup combinedAddr st) of
      (MdioWrite{}, _, _) -> MdioWriteAck
      (_, False, _) -> MdioPhyError
      -- Assumes that registers of the PHY are initialized to all zeroes.
      (_, True, Nothing) -> MdioReadData 0
      (_, True, Just d) -> MdioReadData d

data MdioPhyState
  = Idle (Vec 32 (BitVector 16))
  | BusBusy (Vec 32 (BitVector 16)) (Index 32)
  | ReadStart (Vec 32 (BitVector 16))
  | ReadOpcode (Vec 32 (BitVector 16)) (Index 2)
  | ReadPhyAddress (Vec 32 (BitVector 16)) (Index 5) Bool
  | ReadRegAddress (Vec 32 (BitVector 16)) (Index 5) (BitVector 5) Bool
  | TurnAround (Vec 32 (BitVector 16)) (Index 2) (BitVector 5) Bool
  | HandleRequest (Vec 32 (BitVector 16)) (Index 16) (BitVector 5) Bool
  deriving (Generic, NFDataX)

mdioPhyT ::
  -- | Address
  BitVector 5 ->
  MdioPhyState ->
  -- | MDIO in
  Bit ->
  (MdioPhyState, (Bool, Bit))
mdioPhyT _ st@(Idle regs) mdioIn = (nextSt, (True, 0))
 where
  nextSt = if mdioIn == 0 then ReadStart regs else st
mdioPhyT _ (BusBusy regs i) _ = (nextSt, (True, 0))
 where
  nextSt = if i == 0 then Idle regs else BusBusy regs (i - 1)
mdioPhyT _ (ReadStart regs) _ = (ReadOpcode regs 0, (True, 0))
mdioPhyT _ (ReadOpcode regs i) mdioIn = (nextSt, (True, 0))
 where
  nextSt =
    if i == 0
      then ReadOpcode regs 1
      else ReadPhyAddress regs maxBound (mdioIn == 0)
mdioPhyT phyAddr (ReadPhyAddress regs i isRead) mdioIn = (nextSt, (True, 0))
 where
  nextSt = case (i == 0, mdioIn == phyAddr ! i) of
    (True, True) -> ReadRegAddress regs maxBound (deepErrorX "undefined initial register address") isRead
    (False, True) -> ReadPhyAddress regs (i - 1) isRead
    (_, False) -> BusBusy regs (23 + resize i)
mdioPhyT _ (ReadRegAddress regs i addr isRead) mdioIn = (nextSt, (True, 0))
 where
  nextAddr = addr .<<+ mdioIn
  nextSt =
    if i == 0
      then TurnAround regs 0 nextAddr isRead
      else ReadRegAddress regs (i - 1) nextAddr isRead
mdioPhyT _ (TurnAround regs i addr isRead) _ = (nextSt, (mdioOutEn, 0))
 where
  -- Pull MDIO low during the second bit of the turnaround if a read
  -- is requested. This indicates that we are present on the bus.
  mdioOutEn = not isRead
  nextSt
    | i == 0 && not isRead = TurnAround regs 1 addr isRead
    | otherwise = HandleRequest regs maxBound addr isRead
mdioPhyT _ (HandleRequest regs i addr isRead) mdioIn = (nextSt, (mdioOutEn, 0))
 where
  mdioOutEn = not isRead || bitToBool (regs !! addr ! i)

  nextRegs = if isRead then regs else replace addr (regs !! addr .<<+ mdioIn) regs

  nextSt =
    if i == 0
      then Idle nextRegs
      else HandleRequest nextRegs (i - 1) addr isRead

-- | MDIO slave for simulation purposes. Probably very inefficient in hardware.
mdioPhy ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | Static PHY address
  BitVector 5 ->
  -- | MDC
  Signal dom Bool ->
  -- | MDIO input
  Signal dom Bit ->
  -- | (MDIO output enable, MDIO out)
  (Signal dom Bool, Signal dom Bit)
mdioPhy phyAddr mdc mdioIn = (mdioOutEn, mdioOut)
 where
  fsmEnable = isRising False mdc
  st = regEn (Idle (repeat 0)) fsmEnable nextSt

  (mdioOutEn', mdioOut') = unbundle o

  (nextSt, o) = unbundle $ liftA2 (mdioPhyT phyAddr) st mdioIn

  mdioOutEn = regEn True fsmEnable mdioOutEn'
  mdioOut = regEn 0 fsmEnable mdioOut'

-- The MDIO controller does not support inbound backpressure.
-- TODO. Until clash-protocols has better support for CSignal testing,
-- we simply add a large FIFO to absorb any backpressure generated by stallC.
mdioDfFifo ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  (1 <= (DomainPeriod dom)) =>
  Circuit (Df dom MdioRequest) (CSignal dom (Maybe MdioResponse)) ->
  Circuit (Df dom MdioRequest) (Df dom MdioResponse)
mdioDfFifo ckt = Circuit go
 where
  go (fwdIn, bwdIn) = (bwdOut, fwdOut)
   where
    (bwdOut, fwdToFifo) = toSignals ckt (fwdIn, pure ())
    (_, fwdOut) = (toSignals $ Df.fifo d8) (Df.maybeToData <$> fwdToFifo, bwdIn)

-- | Test the MDIO controller with a single PHY connected to the bus.
prop_mdio_controller_single_phy :: Property
prop_mdio_controller_single_phy =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 501, eoStopAfterEmpty = 800}
    (Gen.list (Range.linear 1 100) genMdioRequest)
    (exposeClockResetEnable (mdioControllerModel [0]))
    (exposeClockResetEnable (mdioDfFifo ckt))
 where
  ckt ::
    forall dom.
    (HiddenClockResetEnable dom) =>
    (1 <= (DomainPeriod dom)) =>
    Circuit (Df dom MdioRequest) (CSignal dom (Maybe MdioResponse))
  ckt = Circuit go
   where
    go (reqIn, _) = (Ack <$> ready, resp)
     where
      (resp, ready, mdioOut) = mdioController @dom d4 mdioIn (Df.dataToMaybe <$> reqIn)

      -- Connect the PHY to the bus.
      (phyMdioT, phyMdio) = mdioPhy 0 (_mdc mdioOut) (boolToBit <$> _mdioT mdioOut)

      mdioIn = mux phyMdioT 1 phyMdio

-- | Test the MDIO controller with two PHYs connected to the bus.
prop_mdio_controller_two_phys :: Property
prop_mdio_controller_two_phys =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 501, eoStopAfterEmpty = 800}
    (Gen.list (Range.linear 1 100) genMdioRequest)
    (exposeClockResetEnable (mdioControllerModel [3, 24]))
    (exposeClockResetEnable (mdioDfFifo ckt))
 where
  ckt ::
    forall dom.
    (HiddenClockResetEnable dom) =>
    (1 <= (DomainPeriod dom)) =>
    Circuit (Df dom MdioRequest) (CSignal dom (Maybe MdioResponse))
  ckt = Circuit go
   where
    go (reqIn, _) = (Ack <$> ready, resp)
     where
      (resp, ready, mdioOut) = mdioController @dom d7 mdioIn (Df.dataToMaybe <$> reqIn)

      -- Connect the PHYs to the bus.
      (phyMdioT1, phyMdio1) = mdioPhy 3 (_mdc mdioOut) (boolToBit <$> _mdioT mdioOut)
      (phyMdioT2, phyMdio2) = mdioPhy 24 (_mdc mdioOut) (boolToBit <$> _mdioT mdioOut)

      -- As only one PHY is allowed to be active at a time, AND-ing their
      -- outputs is sufficient.
      mdioIn = mux (phyMdioT1 .&&. phyMdioT2) 1 (liftA2 (.&.) phyMdio1 phyMdio2)

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 100))
      $(testGroupGenerator)
