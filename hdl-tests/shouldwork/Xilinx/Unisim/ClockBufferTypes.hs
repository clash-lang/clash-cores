{-# LANGUAGE AllowAmbiguousTypes #-}
module ClockBufferTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Unisim.ClockBuffer
import Clash.Cores.Xilinx.Ibufds (ibufdsClock)
import UnisimTestCommon

-- | Input domain of the clock buffers
type A = XilinxSystem

-- Output domains of the clock buffers: divided by 1, 2, 3 and 8
createDomain vXilinxSystem{vName="G1", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="G2", vPeriod=2 * hzToPeriod 100e6}
createDomain vXilinxSystem{vName="G3", vPeriod=3 * hzToPeriod 100e6}
createDomain vXilinxSystem{vName="G8", vPeriod=8 * hzToPeriod 100e6}

-- | Number of cycles in which the clock buffers are disabled at the start of
-- the simulation, to make sure the UNISIM models are out of their global
-- set/reset (asserted for the first 100 ns).
startupCycles :: Index 32
startupCycles = 16

-- | A counter clocked by a buffered clock, observed from the input domain. The
-- observed sequence shows at which input clock edges the buffered clock
-- ticked. The counter is sampled from the input domain, not the other way
-- around: the buffered clock's edges arrive a few delta cycles after those of
-- the input clock in VHDL simulation, so the other way around would sample
-- the input domain's registers after they've been updated.
--
-- The observations are masked up to and including the first edge of the
-- buffered clock: Clash's model of the buffered clock has its first edge at the
-- start of the simulation instead, see "Clash.Cores.Xilinx.Unisim.ClockBuffer".
observe ::
  (KnownDomain dom) =>
  Clock A ->
  Clock dom ->
  Signal A (Unsigned 8)
observe clkA clkB =
  delay clkA enableGen 0 $
    mux (cycleCount clkA .<=. pure firstEdge) 0 (unsafeSynchronizer clkB clkA cnt)
 where
  cnt = delay clkB enableGen 0 (cnt + 1)

-- | Input clock edge at which the buffered clocks produce their first edge
firstEdge :: Index 32
firstEdge = startupCycles + 1

-- | Random enable, deasserted before 'firstEdge' and asserted at 'firstEdge'.
-- After that, the enable is asserted in /threshold/ out of 16 cycles, i.e.,
-- always for a threshold of 16. The enable is driven by a register, so its
-- value at input clock edge /k/ depends on the cycle count at edge /k - 1/.
randomEnable :: Int -> Unsigned 5 -> Clock A -> Enable A
randomEnable seed threshold clkA =
  toEnable $ delay clkA enableGen False $
    (\n r -> n == startupCycles || (n > startupCycles && resize (r :: Unsigned 4) < threshold))
      <$> cycleCount clkA
      <*> genTestData seed clkA

-- | Random clear, asserted before 'firstEdge' and deasserted at 'firstEdge'.
-- After that, the clear is asserted in one out of 32 cycles.
randomClear :: Int -> Clock A -> Reset A
randomClear seed clkA =
  unsafeFromActiveHigh $ delay clkA enableGen True $
    (\n r -> n < startupCycles || (n > startupCycles && r == (0 :: Unsigned 5)))
      <$> cycleCount clkA
      <*> genTestData seed clkA

dutBufg :: Clock A -> Signal A (Unsigned 8)
dutBufg clkA = observe clkA (bufg clkA)

dutBufgce :: Clock A -> Signal A (Unsigned 8)
dutBufgce clkA =
  observe clkA (bufgce @A @G1 clkA (randomEnable randomSeed 10 clkA))

dutBufgceDiv ::
  forall n dom.
  ( KnownDomain dom
  , 1 <= n, n <= 8
  , DomainPeriod dom ~ n * DomainPeriod A ) =>
  SNat n ->
  -- | Enable threshold (out of 16)
  Unsigned 5 ->
  Clock A ->
  Signal A (Unsigned 8)
dutBufgceDiv n threshold clkA =
  observe clkA (bufgceDiv @n @A @dom n clkA clr ena)
 where
  ena = randomEnable randomSeed threshold clkA
  clr = randomClear (randomSeed + 1) clkA


-- | Like 'observe', but masking the observations up to and including the given
-- input clock edge
observeFrom ::
  (KnownDomain dom) =>
  Index 32 ->
  Clock A ->
  Clock dom ->
  Signal A (Unsigned 8)
observeFrom edge clkA clkB =
  delay clkA enableGen 0 $
    mux (cycleCount clkA .<=. pure edge) 0 (unsafeSynchronizer clkB clkA cnt)
 where
  cnt = delay clkB enableGen 0 (cnt + 1)

-- | Random mask, deasserted up to and including 'firstEdge', and asserted in
-- one out of eight cycles after that
randomMask :: Int -> Clock A -> Signal A Bool
randomMask seed clkA =
  delay clkA enableGen False $
    (\n r -> n >= startupCycles && r == (0 :: Unsigned 3))
      <$> cycleCount clkA
      <*> genTestData seed clkA

-- | A 'bufgGt' with random clears, enables and masks. Its first output edge is
-- two input edges after 'firstEdge', as its clear and enable are synchronized.
dutBufgGt ::
  forall n dom.
  ( KnownDomain dom
  , 1 <= n, n <= 8
  , DomainPeriod dom ~ n * DomainPeriod A ) =>
  SNat n ->
  -- | Enable threshold (out of 16)
  Unsigned 5 ->
  Clock A ->
  Signal A (Unsigned 8)
dutBufgGt n threshold clkA =
  observeFrom (firstEdge + 2) clkA (bufgGt @n @A @dom n clkA clr clrMask ena ceMask)
 where
  ena = randomEnable randomSeed threshold clkA
  clr = randomClear (randomSeed + 1) clkA
  clrMask = randomMask (randomSeed + 2) clkA
  ceMask = randomMask (randomSeed + 3) clkA

-- | A clock through an 'obufdsGte4' or 'obufdsGte3', and back through an
-- 'ibufdsClock'. The latter only produces a clock if the outputs of the former
-- are complementary.
dutObufdsGte :: Bool -> Clock A -> Signal A (Unsigned 8)
dutObufdsGte gte3 clkA = observe clkA (ibufdsClock (obuf clkA))
 where
  obuf = if gte3 then obufdsGte3 else obufdsGte4
