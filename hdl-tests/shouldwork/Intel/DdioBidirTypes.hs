module DdioBidirTypes where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.DDR

import IntelTestCommon

-- | Domain of the DDR pad of 'Dom' (and 'DomSync')
createDomain vSystem{vName = "Fast", vPeriod = 5000}

-- | Like 'System', but with a synchronous reset
createDomain vSystem{vName = "DomSync", vResetKind = Synchronous}

-- | Number of cycles per test bench
nCycles :: Int
nCycles = 300

type Data = BitVector 3

-- | Stimuli: reset, enable, which register drives the wire (if any), and the
-- data both of them would drive
type Stimuli = (Bool, Bool, BitVector 2, (Data, Data), (Data, Data))

genStimuli :: Int -> [Stimuli]
genStimuli = evalG $ do
  ss <- listOf nCycles gen
  -- Start in reset
  pure ([(True, True, 0, (0, 0), (0, 0)) | _ <- [1 .. 3 :: Int]] <> ss)
 where
  gen =
    (,,,,)
      <$> chance 3
      <*> (not <$> chance 15)
      <*> genBV
      <*> ((,) <$> genBV <*> genBV)
      <*> ((,) <$> genBV <*> genBV)

{- | Two 'altddioBidir's sharing a wire with pull-up resistors, one with
'OeRegistered' and one with 'OeRegisteredExtended'. Either of them, or
neither, drives the wire. Returns the pairs both of them read.
-}
dut ::
  forall dom deviceFamily.
  ( KnownDomain dom
  , DomainPeriod dom ~ (2 * DomainPeriod Fast)
  , DomainActiveEdge dom ~ 'Rising
  , KnownSymbol deviceFamily
  ) =>
  SSymbol deviceFamily ->
  Clock dom ->
  Signal dom Stimuli ->
  Signal dom ((Data, Data), (Data, Data))
dut fam clk stim = bundle (strengthenPair oA, strengthenPair oB)
 where
  (rstB, enB, sel, as, bs) = unbundle stim
  rst = unsafeFromActiveHigh rstB
  en = toEnable enB
  outA = mux ((== 0) <$> sel) (Just <$> as) (pure Nothing)
  outB = mux ((== 1) <$> sel) (Just <$> bs) (pure Nothing)

  wire :: BiSignalIn 'PullUp Fast 3
  wire = veryUnsafeToBiSignalIn (mergeBiSignalOuts (wireA :> wireB :> pullUp wire :> Nil))
  (wireA, oA) = altddioBidir fam OeRegistered clk rst en wire outA
  (wireB, oB) = altddioBidir fam OeRegisteredExtended clk rst en wire outB

  strengthenPair o = bundle (strengthenBV o0, strengthenBV o1)
   where
    (o0, o1) = unbundle o
