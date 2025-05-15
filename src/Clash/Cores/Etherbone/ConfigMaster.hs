{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.ConfigMaster where

import Clash.Cores.Etherbone.Base
import Clash.Prelude
import Protocols
import qualified Protocols.Df as Df

import Data.Maybe

type ConfigReg = BitVector 64

-- | Transact (read) operations on the Config space.
--
-- Any @WishboneOperation@ with @ConfigAddressSpace@ is handled by this circuit.
-- Writes are silently ignored. Read operations access the @configSpace@. Each
-- register in this space is 64-bits, as defiend in the Etherbone specification.
-- With @dataWidth@ configured as 32-bits, you would need two reads to read a
-- full register.
--
-- The ConfigSpace has the following registers (at the specified index):
--
-- 0. Error register. Shift register that keeps track of errors on the WB bus.
-- 1. Self-describing bus base address.
-- 2. Packet counter. Counts the number of correctly received packets.
configMasterC
  :: forall dom addrWidth dat configRegs.
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat configRegs
  , BitPack dat
  , NFDataX dat
  , Show dat
  , 4 <= ByteSize dat
  , 1 <= BitSize dat
  , DivRU 64 (BitSize dat) * BitSize dat ~ 64
  )
  -- | Self-describing bus base address
  => BitVector addrWidth
  -- | Optional user-defined config registers
  -> Signal dom (Vec configRegs ConfigReg)
  -> Circuit ( Df.Df dom (WishboneOperation addrWidth (ByteSize dat) dat)
             , CSignal dom (Maybe Bit)
             )
             (Df.Df dom (WishboneResult dat))
configMasterC sdbAddress userConfigRegs = Circuit go
  where
    go ((iFwd, errBit), oBwd) = ((oBwd, pure ()), oFwd)
      where
        -- Shift register keeping track of wishbone bus errors.
        errorReg = register (0 :: ConfigReg) $ errorRegT <$> errBit <*> errorReg
        errorRegT Nothing  er = er
        errorRegT (Just b) er = er .<<+ b

        -- The SDB base address
        sdbAddress' = pure $ resize sdbAddress

        -- Count the number of packages received.
        -- This is counting the number of 'end-of-packet' signals without an
        -- 'abort'.
        packetCounter = register (0 :: ConfigReg)
          $ packetCounterT <$> packetCounter <*> iFwd <*> oBwd
        packetCounterT cnt fwd (Ack bwd)
          | isLast fwd && bwd && not (isAbort fwd) = cnt + 1
          | otherwise = cnt
        isLast  = maybe False _opEOP
        isAbort = maybe False _opAbort

        -- The Etherbone internal config registers
        etherboneConfigRegs
          =  errorReg
          :> sdbAddress'
          :> packetCounter
          :> Nil

        configSpace = (++) <$> bundle etherboneConfigRegs <*> userConfigRegs

        reMap ::
          ConfigReg -> Vec (DivRU 64 (BitSize dat)) (BitVector (BitSize dat))
        reMap = bitCoerce

        configSpaceRemapped = fmap (concatMap reMap) configSpace
        outData = regSelect <$> iFwd <*> configSpaceRemapped
        out dat inp = WishboneResult <$> dat <*> (_opEOR <$> inp) <*> (_opEOP <$> inp)
        oFwd = out <$> outData <*> iFwd

        -- Drops ops for the @WishboneAddressSpace@, write ops and ops with
        -- abort set. Selects to correct Word from the @configSpace@.
        regSelect ::
          ( KnownNat n )
          => Maybe (WishboneOperation addrWidth (ByteSize dat) dat)
          -> Vec n (BitVector (BitSize dat))
          -> Maybe (Maybe dat)
        regSelect Nothing _ = Nothing
        regSelect (Just WishboneOperation{..}) cs
          | _opAddrSpace == WishboneAddressSpace
                          = Nothing
          | _opAbort      = Nothing
          | isJust _opDat = Just Nothing
          | otherwise     = Just $ Just $ bitCoerce (cs !! index)
            where
              bitsToShift = natToNum @(CLog 2 (ByteSize dat))
              index = shiftR _opAddr bitsToShift

