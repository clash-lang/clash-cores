{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{- |
  Copyright  :  (C) 2024, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Provides an MDIO bus controller as specified in IEEE 802.3 Clause 22.
-}
module Clash.Cores.Ethernet.Mdio (
  MdioOutput (..),
  MdioRequest (..),
  MdioResponse (..),
  mdioController,
) where

import Control.DeepSeq (NFData)

import Clash.Prelude

-- | MDIO bus request.
data MdioRequest
  = MdioRead
      { mdioPhyAddress :: BitVector 5
      -- ^ Which PHY to address.
      , mdioRegAddress :: BitVector 5
      -- ^ Which of the PHYs registers to select.
      }
  | MdioWrite
      { mdioPhyAddress :: BitVector 5
      -- ^ Which PHY to address.
      , mdioRegAddress :: BitVector 5
      -- ^ Which of the PHYs registers to select.
      , mdioWriteData :: BitVector 16
      -- ^ The data to write.
      }
  deriving (Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | MDIO bus response.
data MdioResponse
  = -- | Write done. Not necessarily successful, because writes to a
    --   non-existent PHY do not cause an error.
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
  deriving (Eq, Generic, NFData, NFDataX, Show, ShowX)

{- |
MDIO pin drivers from the controller's point of view. `_mdioT` and `_mdioO`
directly map to the inputs of bidirectional buffer primitives found on
FPGAs.
-}
data MdioOutput dom = MdioOutput
  { _mdc :: Signal dom Bool
  -- ^ Output to the unidirectional MDC pin.
  , _mdioT :: Signal dom Bool
  -- ^ MDIO output enable, active low.
  , _mdioO :: Signal dom Bit
  -- ^ Value to drive over the MDIO pin. Note that this is always
  --   driven low, as the MDIO pin must be connected to a pull-up resistor.
  --   If we want to drive MDIO high, we simply de-assert `_mdioT`.
  }

-- | State registers of the MDIO controller.
data MdioMasterState
  = Idle
      { _frame :: BitVector 32
      -- ^ The MDIO frame to transmit, without preamble.
      , _readData :: BitVector 16
      -- ^ Accumulator for data reads.
      , _counter :: Unsigned 5
      -- ^ Keeps track of where we are in the current frame.
      , _writeEnable :: Bool
      -- ^ Was the last request a write?
      , _phyAbsent :: Bool
      -- ^ Asserted if the PHY did not pull the MDIO line low during
      --   the turnaround phase.
      , _valid :: Bool
      -- ^ If high, we must transmit a `MdioResponse`.
      }
  | SendPreamble
      { _frame :: BitVector 32
      , _readData :: BitVector 16
      , _counter :: Unsigned 5
      , _writeEnable :: Bool
      , _phyAbsent :: Bool
      , _valid :: Bool
      }
  | SendFrame
      { _frame :: BitVector 32
      , _readData :: BitVector 16
      , _counter :: Unsigned 5
      , _writeEnable :: Bool
      , _phyAbsent :: Bool
      , _valid :: Bool
      }
  deriving (Eq, Generic, NFDataX, Show, ShowX)

controllerIsIdle :: MdioMasterState -> Bool
controllerIsIdle Idle{} = True
controllerIsIdle _ = False

startSequence :: BitVector 2
startSequence = 0b01

readOpcode :: BitVector 2
readOpcode = 0b10

writeOpcode :: BitVector 2
writeOpcode = 0b01

-- | Map an MDIO request to an MDIO frame without preamble.
buildMdioFrame :: MdioRequest -> BitVector 32
buildMdioFrame req = case req of
  -- Addresses are sent MSB first!
  MdioRead phyAddr regAddr ->
    startSequence ++# readOpcode ++# phyAddr ++# regAddr ++# 0b11 ++# (0xFFFF :: BitVector 16)
  MdioWrite phyAddr regAddr writeData ->
    startSequence ++# writeOpcode ++# phyAddr ++# regAddr ++# 0b10 ++# writeData

{- |
Computes the next state of the MDIO controller given the
current state, the input request, and the value of the MDIO pin.
-}
mdioNextState ::
  -- | Current state
  MdioMasterState ->
  -- | (Request, MDIO input)
  (Maybe MdioRequest, Bit) ->
  -- | Next state
  MdioMasterState
mdioNextState st@Idle{} (mdioReq, _) = nextSt
 where
  nextSt = case mdioReq of
    Nothing -> st{_valid = False}
    Just req ->
      SendPreamble
        { _frame = buildMdioFrame req
        , _readData = deepErrorX "mdioNextState: undefined _readData"
        , _counter = 0
        , _writeEnable = case req of
            MdioRead{} -> False
            MdioWrite{} -> True
        , _phyAbsent = deepErrorX "mdioNextState: undefined _phyAbsent"
        , _valid = False
        }
mdioNextState st@SendPreamble{..} (_, _) = nextSt
 where
  nextSt =
    if _counter == maxBound
      then
        SendFrame
          { _frame = _frame
          , _readData = deepErrorX "mdioNextState: undefined _readData"
          , _counter = 0
          , _writeEnable = _writeEnable
          , _phyAbsent = deepErrorX "mdioNextState: undefined _phyAbsent"
          , _valid = False
          }
      else st{_counter = _counter + 1}
mdioNextState st@SendFrame{..} (_, mdioIn) = nextSt
 where
  nextFrame = shiftL _frame 1
  -- The PHY transmits the read data MSB first, so we shift it in at
  -- the LSB side.
  nextReadData = _readData .<<+ mdioIn

  nextPhyAbsent =
    if _counter == 15
      then not _writeEnable && mdioIn /= 0
      else _phyAbsent

  nextSt =
    if _counter == maxBound
      then
        Idle
          { _frame = nextFrame
          , _readData = nextReadData
          , _counter = deepErrorX "mdioNextState: undefined _counter"
          , _writeEnable = _writeEnable
          , _phyAbsent = _phyAbsent
          , _valid = True
          }
      else
        st
          { _frame = nextFrame
          , _readData = nextReadData
          , _counter = _counter + 1
          , _phyAbsent = nextPhyAbsent
          }

-- | Map the MDIO controller state to the tristate output enable (active low).
toMdioT :: MdioMasterState -> Bool
toMdioT st = case st of
  SendFrame{_frame = f} -> bitToBool (msb f)
  _ -> True

-- | Map the MDIO controller state to a response.
toMdioResp :: MdioMasterState -> Maybe MdioResponse
toMdioResp st = case (_valid st, _writeEnable st, _phyAbsent st) of
  (False, _, _) -> Nothing
  (True, _, True) -> Just MdioPhyError
  (True, False, False) -> Just (MdioReadData (_readData st))
  (True, True, False) -> Just MdioWriteAck

{- |
MDIO bus controller which provides request-response based access to the
internal registers of up to 32 Ethernet PHYs.

The signals in the `MdioOutput` record are all registered and can
be directly connected to I/O buffers. It is assumed that the MDIO
pin is connected to a pull-up resistor.

The frequency of MDC is configurable by the @clockDivider@ parameter. For
example, if @clockDivider = 20@ and the frequency of the system clock is
50 MHz, MDC will run at 2.5 MHz. Refer to the data sheet of your Ethernet PHY
to determine the maximum frequency of MDC. If you are not sure, a frequency
of 2.5 MHz or lower should be safe to use.

__NB__: @clockDivider@ must be at least 4. If this is not the case, the
controller is unable to change the MDIO line at the correct time.
-}
mdioController ::
  forall (dom :: Domain) (clockDivider :: Nat).
  (HiddenClockResetEnable dom) =>
  (2 <= clockDivider `Div` 2) =>
  -- | Clock divider
  SNat clockDivider ->
  -- | Value of the MDIO pin
  Signal dom Bit ->
  -- | MDIO request
  Signal dom (Maybe MdioRequest) ->
  -- | (Request-Response bus, Request Ready, MDIO pin drivers)
  (Signal dom (Maybe MdioResponse), Signal dom Bool, MdioOutput dom)
mdioController SNat mdioIn reqS = (toMdioResp <$> st, readyOut, mdioDrivers)
 where
  (mdc, pulse) = mdcGenerator (SNat @clockDivider) (not . controllerIsIdle <$> st)

  -- If we are in idle, we should immediately handle the request.
  -- Else, we only enable the FSM in the middle of MDC's low period, because:
  -- 1. When we drive the MDIO line, we have to guarantee a stable value 10 ns
  --    before the rising edge of MDC, and MDIO has to remain stable 10 ns after
  --    the rising edge of MDC.
  -- 2. The PHY drives the MDIO line some time after the rising edge of MDC.
  --    the exact time depends on the PHY, but sampling MDIO during
  --    the middle of MDC's low period gives the PHY the most time.
  fsmEnable = pulse .||. controllerIsIdle <$> st

  s0 =
    Idle
      { _frame = deepErrorX "mdioController: undefined _frame"
      , _readData = deepErrorX "mdioController: undefined _readData"
      , _counter = deepErrorX "mdioController: undefined _counter"
      , _writeEnable = deepErrorX "mdioController: undefined _writeEnable"
      , _phyAbsent = deepErrorX "mdioController: undefined _phyAbsent"
      , _valid = False
      }

  st :: Signal dom MdioMasterState
  st =
    regEn
      s0
      fsmEnable
      (liftA2 mdioNextState st (bundle (reqS, mdioIn)))

  readyOut =
    hideReset
      (\rst -> unsafeToActiveLow rst .&&. controllerIsIdle <$> st)

  mdioDrivers =
    MdioOutput
      { _mdc = mdc
      , -- Only change on pulse, do not use @fsmEnable@! If we use that, the
        -- last bit of a write request is lost, because @fsmEnable@ is
        -- immediately high when we are back in the idle state.
        _mdioT = regEn True pulse (toMdioT <$> st)
      , _mdioO = 0
      }
{-# OPAQUE mdioController #-}

-- | Generate MDC from the controller clock by clock division.
mdcGenerator ::
  forall (dom :: Domain) (clockDivider :: Nat).
  (HiddenClockResetEnable dom) =>
  (2 <= clockDivider `Div` 2) =>
  -- | Clock divider
  SNat clockDivider ->
  -- | Enable generation of the clock and pulses
  Signal dom Bool ->
  -- | (MDC, pulse)
  (Signal dom Bool, Signal dom Bool)
mdcGenerator SNat en = (mdcOut, pulse)
 where
  counter :: Signal dom (Index (clockDivider `Div` 2))
  counter = register 0 (satSucc SatWrap <$> counter)

  mdc :: Signal dom Bool
  mdc = regEn True ((== maxBound) <$> counter) (mux en (not <$> mdc) (pure True))

  -- Delay for another cycle for proper alignment with respect to MDIO
  mdcOut = register True mdc

  -- This signal is high when the counter is roughly halfway.
  -- Note that `shiftR` 1 is division by 2, this trick avoids index
  -- overflow when @clockDivider <= 5@.
  pulse' = (== (maxBound `shiftR` 1)) <$> counter

  -- We give a pulse when MDC is halfway through its low period.
  pulse :: Signal dom Bool
  pulse = register False ((not <$> mdc) .&&. pulse')
