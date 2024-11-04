{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
  Copyright  :  (C) 2024, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Provides an MDIO bus controller as specified in IEEE 802.3 Clause 22.
-}
module Clash.Cores.Ethernet.Mdio (
  MdioRequest (..),
  MdioResponse (..),
  mdioController,
) where

import Clash.Prelude

-- | MDIO bus request.
data MdioRequest
  = MdioRead
      { mdioPhyAddress :: BitVector 5
      , mdioRegAddress :: BitVector 5
      }
  | MdioWrite
      { mdioPhyAddress :: BitVector 5
      , mdioRegAddress :: BitVector 5
      , mdioWriteData :: BitVector 16
      }
  deriving (Generic, NFDataX, Show, ShowX)

-- | MDIO bus response.
data MdioResponse
  = -- | Write was successful.
    MdioWriteAck
  | -- | Read was successful.
    MdioReadData (BitVector 16)
  | -- | Raised when the PHY does not pull MDIO low during the second cycle of
    --   the turnaround of a read request. There are multiple potential causes
    --   for this to occur:
    --
    --   1. There is no PHY at the requested address.
    --   2. The PHY is held in reset.
    --   3. The PHY's clock is not running.
    MdioPhyError
  deriving (Generic, NFDataX, Show, ShowX)

data MdioMasterState
  = Idle
  | SendPreamble
      { stopara :: BitVector 14
      , wdata :: BitVector 16
      , counter :: Index 32
      , read :: Bool
      }
  | SendStOpPaRa
      { stopara :: BitVector 14
      , wdata :: BitVector 16
      , counter :: Index 32
      , read :: Bool
      }
  | TurnAround
      { stopara :: BitVector 14
      , wdata :: BitVector 16
      , counter :: Index 32
      , read :: Bool
      }
  | SendData
      { stopara :: BitVector 14
      , wdata :: BitVector 16
      , counter :: Index 32
      }
  | RecvData
      { stopara :: BitVector 14
      , wdata :: BitVector 16
      , counter :: Index 32
      , rbuf :: BitVector 16
      }
  deriving (Eq, Generic, NFDataX, Show, ShowX)

startSequence :: BitVector 2
startSequence = 0b01

readOpcode :: BitVector 2
readOpcode = 0b10

writeOpcode :: BitVector 2
writeOpcode = 0b01

mdioT ::
  MdioMasterState ->
  -- | (Request, MDIO input)
  (Maybe MdioRequest, Bit) ->
  ( MdioMasterState
  , (Bool, Maybe MdioResponse)
  )
mdioT Idle (mdioReq, _) = (nextSt, (True, Nothing))
 where
  -- Addresses are sent MSB first!
  nextSt = case mdioReq of
    Nothing -> Idle
    Just req -> case req of
      MdioRead{..} ->
        SendPreamble
          (startSequence ++# readOpcode ++# mdioPhyAddress ++# mdioRegAddress)
          (deepErrorX "undefined write data")
          0
          True
      MdioWrite{..} ->
        SendPreamble
          (startSequence ++# writeOpcode ++# mdioPhyAddress ++# mdioRegAddress)
          mdioWriteData
          0
          False
mdioT (SendPreamble buf dat c rd) _ = (nextSt, (True, Nothing))
 where
  nextSt
    | c == maxBound = SendStOpPaRa buf dat 0 rd
    | otherwise = SendPreamble buf dat (c + 1) rd
mdioT (SendStOpPaRa buf dat c rd) _ = (nextSt, (mdioOutEn, Nothing))
 where
  mdioOutEn = msb buf == 1
  nextSt
    | c == 13 = TurnAround buf dat 0 rd
    | otherwise = SendStOpPaRa (shiftL buf 1) dat (c + 1) rd
mdioT (TurnAround buf dat c rd) (_, mdioIn) = (nextSt, (mdioOutEn, resp))
 where
  mdioOutEn = not (c == 1 && not rd)

  -- If we are reading, the PHY should pull the MDIO line down during the second
  -- bit of the turnaround time. If this does not happen, then there is no PHY
  -- at the requested address. Therefore, we signal an error.
  (nextSt, resp) = case (c == 0, rd) of
    (True, False) ->
      (TurnAround buf dat 1 rd, Nothing)
    (True, True) ->
      (TurnAround buf dat 1 rd, Nothing)
    (False, False) ->
      (SendData buf dat 0, Nothing)
    (False, True) ->
      if mdioIn == 0
        then (RecvData buf dat 0 (deepErrorX "undefined read data"), Nothing)
        else (Idle, Just MdioPhyError)
mdioT (SendData buf dat c) _ = (nextSt, (mdioOutEn, resp))
 where
  mdioOutEn = msb dat == 1

  resp
    | c == 15 = Just MdioWriteAck
    | otherwise = Nothing

  nextSt
    | c == 15 = Idle
    | otherwise = SendData buf (shiftL dat 1) (c + 1)
mdioT (RecvData _ _ c rb) (_, mdioIn) = (nextSt, (True, resp))
 where
  newDat = rb .<<+ mdioIn

  resp
    | c == 15 = Just (MdioReadData newDat)
    | otherwise = Nothing

  nextSt
    | c == 15 = Idle
    | otherwise = RecvData (deepErrorX "") (deepErrorX "") (c + 1) newDat

{- |
MDIO bus controller which provides request-response based access to the internal
registers of up to 32 Ethernet PHYs.

The MDIO output enable signal should be used to drive the MDIO line via a
tristate buffer:

- If it is @False@, pull MDIO down to the ground.
- If it is @True@, release MDIO and let the pull-up resistor do the work.

A logical 1 should /NEVER/ be written to the MDIO line.

The frequency of MDC is configurable by the @clockDivider@ parameter. For
example, if @clockDivider = 20@ and the frequency of the system clock is
50 MHz, MDC will run at 2.5 MHz. Refer to the datasheet of your Ethernet PHY
to determine the maximum frequency of MDC.
-}
mdioController ::
  forall (dom :: Domain) (clockDivider :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat (DomainPeriod dom)) =>
  (1 <= (DomainPeriod dom)) =>
  (1 <= Div (clockDivider + 3) 4) =>
  -- | Clock divider
  SNat clockDivider ->
  -- | Read/Write request
  Signal dom (Maybe MdioRequest) ->
  -- | Value of the MDIO pin
  Signal dom Bit ->
  -- | (Response from PHY, MDC, MDIO output enable (active low))
  (Signal dom (Maybe MdioResponse), Signal dom Bool, Signal dom Bool)
mdioController SNat reqS mdioIn = (resp, mdc, mdioOutEnable)
 where
  mdc = oscillate False (SNat @(clockDivider `DivRU` 2))

  counter :: Signal dom (Index (clockDivider `DivRU` 4))
  counter = regEn 0 (not <$> mdc .&&. not <$> gavePulse) (satSucc SatWrap <$> counter)

  gavePulse = register False ((pulse .||. gavePulse) .&&. not <$> mdc)
  pulse = counter .==. pure maxBound .&&. not <$> gavePulse

  -- If we are in idle, we should immediately handle the request.
  -- Else, we only enable the FSM in the middle of MDC's low period, because:
  -- 1. When we drive the MDIO line, we have to guarantee a stable value 10 ns
  --    before the rising edge of MDC, and MDIO has to remain stable 10 ns after
  --    the rising edge of MDC.
  -- 2. The PHY drives the MDIO line some time after the rising edge of MDC.
  --    the exact time depends on the PHY, but sampling MDIO during
  --    the middle of MDC's low period gives the PHY the most time.
  fsmEnable = isRising False (pulse .||. (Idle ==) <$> st)

  (st', o) = unbundle $ liftA2 mdioT st (bundle (reqS, mdioIn))
  (mdioOutEnable', resp') = unbundle o

  st = regEn Idle fsmEnable st'
  mdioOutEnable = regEn True fsmEnable mdioOutEnable'

  resp = (\(r, e) -> if e then r else Nothing) <$> bundle (resp', fsmEnable)
{-# OPAQUE mdioController #-}
