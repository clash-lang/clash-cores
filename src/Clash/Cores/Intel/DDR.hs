{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

DDR I/O registers of Intel FPGAs (@ALTDDIO_IN@, @ALTDDIO_OUT@ and
@ALTDDIO_BIDIR@). 'altddioIn' and 'altddioOut' are re-exported from
"Clash.Intel.DDR" in @clash-prelude@; 'altddioBidir' combines them on a
bidirectional pad, with the same conventions.

For general information about DDR primitives see "Clash.Explicit.DDR". For
more information on ALTDDIO see the /ALTDDIO_IN, ALTDDIO_OUT, and ALTDDIO_BIDIR
IP Cores User Guide/
(<https://www.intel.com/content/www/us/en/docs/programmable/683148/current/altddio-in-altddio-out-and-altddio-bidir.html>).
-}

module Clash.Cores.Intel.DDR
  ( -- * Input and output
    altddioIn
  , altddioOut
    -- * Bidirectional
  , altddioBidir
  , OeRegister (..)
  ) where

import Clash.Explicit.Prelude

import Clash.Explicit.DDR (ddrOut#)
import Clash.Signal.Internal (Signal ((:-)))
import Clash.Intel.DDR (altddioIn, altddioOut)
import Data.Bifunctor (bimap)
import Data.Proxy (Proxy (..))
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Cores.Intel.Primitive.Internal (alteraMf)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( BiSignalInPort (..), BiSignalOutPort, ClockPort (..), Param (..), Port (..)
  , ResetPort (..), inst, unPort
  )

{- | How 'altddioBidir' registers its output enable. In both modes, the output
enable is registered on the rising edge of the clock, together with the data
it goes with.
-}
data OeRegister
  = -- | The output enable takes effect together with the data: the pad is
    -- driven during both halves of the clock cycle in which the data is
    -- output (@oe_reg = \"REGISTERED\"@).
    OeRegistered
  | -- | Like 'OeRegistered', but enabling the output is delayed by half a clock
    -- cycle: when the output was disabled in the previous clock cycle, the pad
    -- is only driven in the second half of the clock cycle
    -- (@extend_oe_disable = \"ON\"@). Disabling the output isn't delayed.
    -- Memory interfaces use this to keep a bus released for half a cycle
    -- longer.
    OeRegisteredExtended
  deriving (Show, Eq, Generic, NFDataX)

{- | Bidirectional DDR I/O register (@ALTDDIO_BIDIR@): an 'altddioOut' and an
'altddioIn' on the same bidirectional pad, clocked by the same clock.

The output side takes a pair @(i0, i1)@ to drive onto the pad, or 'Nothing' to
put the pad in high impedance. Like for 'altddioOut', @i0@ is clocked out on
the /rising/ edge and @i1@ on the /falling/ edge, and @i0@ comes before @i1@ in
time. The output enable is registered along with the data, see 'OeRegister'.

The input side returns the pair @(o0, o1)@ sampled from the pad, whether this
register drives it or not. Like for 'altddioIn', @o0@ is the data clocked in
on the /falling/ edge and @o1@ is the data clocked in on the /rising/ edge, and
@o0@ comes before @o1@ in time. So reading back the register's own output
returns the pair that was driven three clock cycles before.

Reset values are @0@, and the reset puts the pad in high impedance. Whether
the reset is asynchronous (@aclr@) or synchronous (@sclr@) depends on the
domain. Of the device families Quartus Prime Lite supports, only Cyclone V
supports synchronous resets: Cyclone IV, Cyclone 10 LP and MAX 10 need a domain
with asynchronous resets. The enable applies to both the input and the output
side.

Pass the 'BiSignalIn' of a top entity argument as the pad, and return the
'BiSignalOut' from the top entity, see "Clash.Signal.BiSignal".

__NB__: This primitive only supports rising edges as the active edge.

The simulation model matches Intel's simulation model (@altera_mf@) for Cyclone
IV E, Cyclone V and MAX 10. Unlike 'altddioIn', it applies the reset and enable
of a clock cycle to the data sampled on the falling edge in that clock cycle,
like the hardware does. 'altddioIn' (through 'Clash.Explicit.DDR.ddrIn#')
applies those of the previous clock cycle, so the two differ in the clock cycle
after a reset or a disabled clock cycle.

The device family is passed as an t'SSymbol' like for 'altddioIn', but also
needs a 'KnownSymbol' constraint (which an t'SSymbol' literal satisfies): Clash
can't reduce the device family to a literal parameter by pattern matching on
the t'SSymbol'.
-}
altddioBidir ::
  forall deviceFamily ds a dom domDDR.
  ( HasCallStack
  , KnownDomain dom
  , KnownDomain domDDR
  , DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  , DomainActiveEdge dom ~ 'Rising
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family, for example @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  OeRegister ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Pad (@padio@)
  BiSignalIn ds domDDR (BitSize a) ->
  -- | Normal speed pair @(i0, i1)@ to drive onto the pad, or 'Nothing' for
  -- high impedance (@datain_h@, @datain_l@ and @oe@)
  Signal dom (Maybe (a, a)) ->
  -- | Pad (@padio@) and the normal speed pair @(o0, o1)@ read from it
  -- (@dataout_l@, @dataout_h@)
  (BiSignalOut ds domDDR (BitSize a), Signal dom (a, a))
altddioBidir devFam oeReg clk rst en pad out =
  withFrozenCallStack $
    case altddioBidir# devFam oeReg clk rst en pad i0 i1 oe of
      (padOut, o) -> (padOut, bimap unpack unpack <$> o)
 where
  i0 = pack . fst . fromJustX <$> out
  i1 = pack . snd . fromJustX <$> out
  oe = isJust <$> out

altddioBidir# ::
  forall deviceFamily ds n dom domDDR.
  ( HasCallStack
  , KnownDomain dom
  , KnownDomain domDDR
  , DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  , DomainActiveEdge dom ~ 'Rising
  , KnownNat n
  , KnownSymbol deviceFamily
  ) =>
  SSymbol deviceFamily ->
  OeRegister ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  BiSignalIn ds domDDR n ->
  -- | @datain_h@
  Signal dom (BitVector n) ->
  -- | @datain_l@
  Signal dom (BitVector n) ->
  -- | @oe@
  Signal dom Bool ->
  (BiSignalOut ds domDDR n, Signal dom (BitVector n, BitVector n))
altddioBidir# _devFam oeReg clk rst en pad i0 i1 oe
  | clashSimulation =
      ( writeToBiSignal pad (mkOut <$> oeDdr <*> ddrOut# clk rst en 0 i0 i1)
      , ddrInModel rst en (readFromBiSignal pad)
      )
  | otherwise = (unPort padOut, bundle (unPort dataoutL, unPort dataoutH))
 where
  mkOut :: Bool -> BitVector n -> Maybe (BitVector n)
  mkOut True d = Just d
  mkOut False _ = Nothing

  -- The output enable at DDR rate. 'ddrOut#' outputs the pair registered on a
  -- rising edge during that clock cycle, so registering the output enable
  -- the same way aligns it with the data.
  oeDdr :: Signal domDDR Bool
  oeDdr = case oeReg of
    OeRegistered -> ddrOut# clk rst en False oe oe
    OeRegisteredExtended ->
      -- The first half of a clock cycle is only enabled if the previous
      -- clock cycle was enabled as well
      ddrOut# clk rst en False ((&&) <$> oe <*> oePrev) oe
  oePrev = register clk rst en False oe

  enBit = boolToBit <$> fromEnable en
  extendOeDisable = case oeReg of
    OeRegistered -> "OFF"
    OeRegisteredExtended -> "ON"

  (padOut, dataoutL, dataoutH) = ports
  ports ::
    ( BiSignalOutPort "padio" ds domDDR n
    , Port "dataout_l" dom (BitVector n)
    , Port "dataout_h" dom (BitVector n)
    )
  ports = case resetKind @dom of
    SAsynchronous ->
      inst
        (alteraMf "altddio_bidir")
        (Param @"width" @Integer (natToNum @n))
        (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
        (Param @"power_up_high" @String "OFF")
        (Param @"oe_reg" @String "REGISTERED")
        (Param @"extend_oe_disable" @String extendOeDisable)
        (Param @"implement_input_in_lcell" @String "UNUSED")
        (Param @"invert_output" @String "OFF")
        (Param @"lpm_type" @String "altddio_bidir")
        (BiSignalInPort @"padio" pad)
        (ClockPort @"inclock" clk)
        (Port @"inclocken" enBit)
        (ClockPort @"outclock" clk)
        (Port @"outclocken" enBit)
        (ResetPort @"aclr" @'ActiveHigh rst)
        (Port @"datain_h" i0)
        (Port @"datain_l" i1)
        (Port @"oe" (boolToBit <$> oe))
    SSynchronous ->
      inst
        (alteraMf "altddio_bidir")
        (Param @"width" @Integer (natToNum @n))
        (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
        (Param @"power_up_high" @String "OFF")
        (Param @"oe_reg" @String "REGISTERED")
        (Param @"extend_oe_disable" @String extendOeDisable)
        (Param @"implement_input_in_lcell" @String "UNUSED")
        (Param @"invert_output" @String "OFF")
        (Param @"lpm_type" @String "altddio_bidir")
        (BiSignalInPort @"padio" pad)
        (ClockPort @"inclock" clk)
        (Port @"inclocken" enBit)
        (ClockPort @"outclock" clk)
        (Port @"outclocken" enBit)
        (ResetPort @"sclr" @'ActiveHigh rst)
        (Port @"datain_h" i0)
        (Port @"datain_l" i1)
        (Port @"oe" (boolToBit <$> oe))

{- | Simulation model of the input side of 'altddioBidir'.

Like 'Clash.Explicit.DDR.ddrIn#', but the reset and enable of a clock cycle also apply to the
falling edge in that clock cycle. Following the conventions of 'Clash.Explicit.DDR.ddrIn#' and
'ddrOut#', DDR sample @2k-1@ is the level of the pad during the first half of
clock cycle @k@, and DDR sample @2k@ the level during the second half. On the
falling edge in clock cycle @k@, ALTDDIO latches DDR sample @2k-1@, using the
reset and enable of clock cycle @k@. 'Clash.Explicit.DDR.ddrIn#' uses those of clock cycle @k-1@
instead, which differs from Intel's simulation model in the cycle after a reset
or a disabled clock cycle.
-}
ddrInModel ::
  forall n dom domDDR.
  (KnownDomain dom, KnownNat n) =>
  Reset dom ->
  Enable dom ->
  Signal domDDR (BitVector n) ->
  Signal dom (BitVector n, BitVector n)
ddrInModel rst en ddr =
  go (0, 0, 0) (unsafeToActiveHigh rst) (fromEnable en) (pairs ddr)
 where
  isAsync = case resetKind @dom of
    SAsynchronous -> True
    SSynchronous -> False

  -- The DDR samples of a clock cycle
  pairs :: Signal domDDR a -> Signal dom (a, a)
  pairs ~(a :- ~(b :- rest)) = (a, b) :- pairs rest

  -- State: the registered outputs (@dataout_l@ and @dataout_h@), and the
  -- second DDR sample of the previous clock cycle
  go (l, h, prev) ~(r :- rs) ~(e :- es) ~((x0, x1) :- xs) =
    out :- (r `seq` e `seq` go (l', h', x1) rs es xs)
   where
    out
      | isAsync && r = (0, 0)
      | otherwise = (l, h)
    (l', h')
      | r && (isAsync || e) = (0, 0)
      | e = (prev, x0)
      | otherwise = (l, h)
