{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Etherbone.ConfigMaster where

import Clash.Prelude
import Protocols
import qualified Protocols.Df as Df
import Clash.Cores.Etherbone.Base
import Data.Maybe

-- | Transact (read) operations on the Config space.
--
-- Any @WishboneOperation@ with @ConfigAddressSpace@ is handled by this circuit.
-- Write are silently ignored. Read operations access the @configSpace@. Each
-- register in this space is 64-bits. With @dataWidth@ configured as 32-bits,
-- you would need two writes to read a full register.
--
-- The ConfigSpace has the following registers (at the specified index):
--  0: Error register. Shift register that keeps track of errors on the WB bus.
--  1: Self-describing bus base address.
--  2: Packet counter. Counts the number of correctly received packets.
configMasterC
  :: forall dom addrWidth dat selWidth configRegs.
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat configRegs
  , BitPack dat
  , NFDataX dat
  , Show dat
  , 4 <= ByteSize dat
  , 1 <= BitSize dat
  , Div 64 (BitSize dat) * BitSize dat ~ 64
  )
  => BitVector addrWidth
  -> Signal dom (Vec configRegs (BitVector 64))
  -> Circuit ( Df.Df dom (WishboneOperation addrWidth selWidth dat)
             , CSignal dom (Maybe Bit)
             )
             (Df.Df dom (WishboneResult dat))
configMasterC sdbAddress userConfigRegs = Circuit go
  where
    go ((iFwd, errBit), oBwd) = ((oBwd, pure ()), oFwd)
      where
        -- Shift register keeping track of wishbone bus errors.
        errorReg = register (0 :: BitVector 64) $ errorRegT <$> errBit <*> errorReg
        errorRegT Nothing  er = er
        errorRegT (Just b) er = shiftL er 1 .|. resize (pack b)

        -- The SDB base address
        sdbAddress' = pure $ resize sdbAddress

        -- Count the number of packages received.
        -- This is counting the number of 'last' operations where the 'abort'
        -- bit was not set.
        packetCounter = register (0 :: BitVector 64)
          $ mux (    isLast <$> iFwd
                .&&. not <$> prevLast
                .&&. not . isAbort <$> iFwd
                ) (packetCounter + 1) packetCounter
        prevLast = register False (isLast <$> iFwd)
        isLast (Df.Data WishboneOperation{_opLast=True}) = True
        isLast _ = False
        isAbort (Df.Data WishboneOperation{_opAbort=True}) = True
        isAbort _ = False

        configSpace = (++) <$> bundle (errorReg :> sdbAddress' :> packetCounter :> Nil) <*> userConfigRegs

        reMap ::
          BitVector 64 -> Vec (Div 64 (BitSize dat)) (BitVector (BitSize dat))
        reMap = bitCoerce

        configSpaceRemapped = fmap (concatMap reMap) configSpace
        outData = regSelect <$> iFwd <*> configSpaceRemapped
        out dat inp = WishboneResult <$> dat <*> (_opLast <$> inp)
        oFwd = out <$> outData <*> iFwd

        regSelect ::
          ( KnownNat n )
          => Df.Data (WishboneOperation addrWidth selWidth dat)
          -> Vec n (BitVector (BitSize dat))
          -> Df.Data (Maybe dat)
        regSelect Df.NoData _ = Df.NoData
        regSelect (Df.Data WishboneOperation{..}) cs
          | _addrSpace == WishboneAddressSpace
                          = Df.NoData
          | _opAbort      = Df.NoData
          | isJust _opDat = Df.Data Nothing
          | otherwise     = Df.Data $ Just $ bitCoerce (cs !! index)
            where
              bitsToShift = natToNum @(CLog 2 (ByteSize dat))
              index = shiftR _opAddr bitsToShift

