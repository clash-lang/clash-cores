{-# LANGUAGE TemplateHaskell #-}

{- | Compares the Haskell models of the ROMs and the RAMs with two clocks in
"Clash.Cores.Intel.Memory" against Intel's simulation model (@altera_mf@), on
Cyclone V (M10K), Cyclone IV E and MAX 10 (M9K), with port B on a clock that
is faster, slower or the same as the clock of port A.
-}
module AltsyncramDualClock where

import Clash.Explicit.Prelude

import Clash.Cores.Intel.Memory

import AltsyncramDualClockTypes
import AltsyncramTypes (withInit)
import IntelTestCommon

-- | This 'topEntity' exists to make @clash-testsuite@ happy. Without it cannot
-- find the test benches.
topEntity :: Unsigned 1
topEntity = 0

tbRomCycloneV :: Signal System Bool
tbRomCycloneV =
  tb (dutRom (SSymbol @"Cyclone V") (PortConfig False NewData))
    $(blobs (dutRom (SSymbol @"Cyclone V") (PortConfig False NewData)) (fst (genStimuli False False 10 1)))
{-# ANN tbRomCycloneV (TestBench 'topEntity) #-}

tbRomMax10Reg :: Signal System Bool
tbRomMax10Reg =
  tb (dutRom (SSymbol @"MAX 10") (PortConfig True NewData))
    $(blobs (dutRom (SSymbol @"MAX 10") (PortConfig True NewData)) (fst (genStimuli False False 10 2)))
{-# ANN tbRomMax10Reg (TestBench 'topEntity) #-}

tbDpRomCycloneIV :: Signal System Bool
tbDpRomCycloneIV =
  tb2 (dutDpRom @_ @B7 (SSymbol @"Cyclone IV E") MixedOldData (PortConfig True NewData) (PortConfig False NewData))
    $(uncurry (blobs2 (dutDpRom @_ @B7 (SSymbol @"Cyclone IV E") MixedOldData (PortConfig True NewData) (PortConfig False NewData)))
        (genStimuli False False 7 3))
{-# ANN tbDpRomCycloneIV (TestBench 'topEntity) #-}

tbDpRomCycloneV :: Signal System Bool
tbDpRomCycloneV =
  tb2 (dutDpRom @_ @System (SSymbol @"Cyclone V") MixedDontCare (PortConfig False NewData) (PortConfig True NewData))
    $(uncurry (blobs2 (dutDpRom @_ @System (SSymbol @"Cyclone V") MixedDontCare (PortConfig False NewData) (PortConfig True NewData)))
        (genStimuli False False 10 4))
{-# ANN tbDpRomCycloneV (TestBench 'topEntity) #-}

tbSdpCycloneV :: Signal System Bool
tbSdpCycloneV =
  tb2 (dutSdp @_ @B7 (SSymbol @"Cyclone V") withInit (PortConfig False NewData))
    $(uncurry (blobs2 (dutSdp @_ @B7 (SSymbol @"Cyclone V") withInit (PortConfig False NewData)))
        (genStimuli True False 7 5))
{-# ANN tbSdpCycloneV (TestBench 'topEntity) #-}

tbSdpCycloneIVReg :: Signal System Bool
tbSdpCycloneIVReg =
  tb2 (dutSdp @_ @B25 (SSymbol @"Cyclone IV E") oldMixed (PortConfig True NewData))
    $(uncurry (blobs2 (dutSdp @_ @B25 (SSymbol @"Cyclone IV E") oldMixed (PortConfig True NewData)))
        (genStimuli True False 25 6))
{-# ANN tbSdpCycloneIVReg (TestBench 'topEntity) #-}

tbSdpMax10Same :: Signal System Bool
tbSdpMax10Same =
  tb2 (dutSdp @_ @System (SSymbol @"MAX 10") oldMixed (PortConfig False NewData))
    $(uncurry (blobs2 (dutSdp @_ @System (SSymbol @"MAX 10") oldMixed (PortConfig False NewData)))
        (genStimuli True False 10 7))
{-# ANN tbSdpMax10Same (TestBench 'topEntity) #-}

tbTdpCycloneV :: Signal System Bool
tbTdpCycloneV =
  tb2 (dutTdp @_ @B7 (SSymbol @"Cyclone V") withInit (PortConfig False NewData) (PortConfig True DontCare))
    $(uncurry (blobs2 (dutTdp @_ @B7 (SSymbol @"Cyclone V") withInit (PortConfig False NewData) (PortConfig True DontCare)))
        (genStimuli True True 7 8))
{-# ANN tbTdpCycloneV (TestBench 'topEntity) #-}

tbTdpCycloneIV :: Signal System Bool
tbTdpCycloneIV =
  tb2 (dutTdp @_ @B25 (SSymbol @"Cyclone IV E") oldMixed (PortConfig True OldData) (PortConfig False NewDataOldMaskedBytes))
    $(uncurry (blobs2 (dutTdp @_ @B25 (SSymbol @"Cyclone IV E") oldMixed (PortConfig True OldData) (PortConfig False NewDataOldMaskedBytes)))
        (genStimuli True True 25 9))
{-# ANN tbTdpCycloneIV (TestBench 'topEntity) #-}

tbTdpMax10Same :: Signal System Bool
tbTdpMax10Same =
  tb2 (dutTdp @_ @System (SSymbol @"MAX 10") withInit{mixedReadDuringWrite = MixedOldData} (PortConfig False OldData) (PortConfig False NewData))
    $(uncurry (blobs2 (dutTdp @_ @System (SSymbol @"MAX 10") withInit{mixedReadDuringWrite = MixedOldData} (PortConfig False OldData) (PortConfig False NewData)))
        (genStimuli True True 10 10))
{-# ANN tbTdpMax10Same (TestBench 'topEntity) #-}
