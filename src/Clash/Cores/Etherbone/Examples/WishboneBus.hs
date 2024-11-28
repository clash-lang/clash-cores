{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}

module Clash.Cores.Etherbone.Examples.WishboneBus where

import Clash.Cores.Etherbone.Base
import Clash.Cores.Etherbone.Examples.Interconnect (singleMasterInterconnect)
import Clash.Prelude
import Protocols
import Protocols.Wishbone

type DataWidth = 4
type AddrWidth = 32
type WBData = BitVector (DataWidth * 8)

wishboneCircuit ::
  ( HiddenClockResetEnable dom )
  => Circuit (Wishbone dom 'Standard AddrWidth WBData) ()
wishboneCircuit = circuit $ \wbBus -> do
  [wb0, wb1] <- singleMasterInterconnect @_ @_ @AddrWidth (0b0 :> 0b1 :> Nil) -< wbBus
  wishboneScratchpad @_ @WBData @_ d4 -< wb0
  wishboneRom @_ @_ @WBData sdbRom -< wb1

-- Wishbone connected register file for testing writes and reads
wishboneScratchpad
  :: forall dom a addrWidth n .
  ( HiddenClockResetEnable dom
  , NFDataX a
  , Bits a
  , BitPack a
  , KnownNat addrWidth
  , 1 <= n
  , 1 <= Div (BitSize a + 7) 8
  ) => SNat n -> Circuit (Wishbone dom Standard addrWidth a) ()
wishboneScratchpad SNat = fromSignals circ
  where
    circ
      :: (Signal dom (WishboneM2S addrWidth selWidth a), ())
      -> (Signal dom (WishboneS2M a), ())
    circ (wb, _) = (out <$> prevSel <*> prevVal, ())
      where
        selected = (busCycle <$> wb) .&&. strobe <$> wb
        writeEnabled = selected .&&. writeEnable <$> wb
        
        prevSel = register False selected
        prevVal = register undefined scratchpadValue

        scratchpadValue :: Signal dom a
        scratchpadValue = scratchpad $ params <$> writeEnabled <*> wb
        params we x = ( we
                      , unpack (resize index) :: Index n
                      , writeData x
                      )
          where
            bitsToShift = natToNum @(CLog 2 (BitSize a `DivRU` 8))
            index = shiftR (addr x) bitsToShift

        -- @a here is a type application, forcing 'dat' from 'emptyWishboneS2M'
        -- to the 'a' type in this scope (so that the NFDataX constraint is set)
        out :: Bool -> a -> WishboneS2M a
        out ack val = (emptyWishboneS2M @a) { readData=val, acknowledge=ack }
{-# OPAQUE wishboneScratchpad #-}

scratchpad
  :: forall n dom a . (KnownNat n, HiddenClockResetEnable dom, NFDataX a, BitPack a)
  => Signal dom (Bool, Index n, a)
  -> Signal dom a
-- scratchpad = mealy scratchpadT (repeat $ unpack 0)
scratchpad = mealy scratchpadT (map unpack $ iterateI (+1) 0)

-- n: number of registers in regfile
scratchpadT :: (KnownNat n, BitPack a) => Vec n a -> (Bool, Index n, a) -> (Vec n a, a)
scratchpadT pad (ok, adr, val) = (pad', el)
  where
    pad'
      | ok        = replace adr val pad
      | otherwise = pad
    el
      | ok        = unpack 0
      | otherwise = pad !! adr


-- Self describing bus related types and functions
-- This is a Wishbone connected block of memory describing the Wishbone bus.
-- Spec: https://ohwr.org/project/fpga-config-space

data SdbInterconnect = SdbInterconnect
  { _sdbMagic   :: BitVector 32
  , _sdbRecords :: BitVector 16
  , _sdbFmtVer  :: BitVector 8
  , _sdbBusType :: BitVector 8
  } deriving (Generic, Show, BitPack, NFDataX)

data SdbDevice = SdbDevice
  { _sdbClass    :: BitVector 16
  , _sdbVerMajor :: BitVector 8
  , _sdbVerMinor :: BitVector 8
  , _sdbPad0     :: BitVector 24
  , _sdbEndian   :: Bit
  , _sdbPad1     :: BitVector 3
  , _sdbWidth    :: BitVector 4
  } deriving (Generic, Show, BitPack, NFDataX)

data SdbComponent = SdbComponent
  { _sdbDevice    :: BitVector 64
  , _sdbAddrFirst :: BitVector 64
  , _sdbAddrLast  :: BitVector 64
  , _sdbVendorId  :: BitVector 64
  , _sdbDeviceId  :: BitVector 32
  , _sdbVersion   :: Unsigned 32
  , _sdbDate      :: BitVector 32
  , _sdbName      :: Vec 19 (BitVector 8)  -- String
  , _sdbType      :: BitVector 8
  } deriving (Generic, Show, BitPack, NFDataX)

sdbComponentDefault :: SdbComponent
sdbComponentDefault
  = SdbComponent 
  { _sdbDevice    = undefined

  , _sdbAddrFirst = undefined
  , _sdbAddrLast  = undefined

  , _sdbVendorId  = 0xe9ad2f4133c2c314  -- xxh64 of 'QBayLogic'
  , _sdbDeviceId  = 0x00000000
  , _sdbVersion   = 0x00000001
  , _sdbDate      = 0x20241119
  , _sdbName      = undefined
  , _sdbType      = 1
  }

charConvert :: Char -> BitVector 8
charConvert = fromIntegral . fromEnum

sdbCrossbar :: SdbComponent
sdbCrossbar = sdbComponentDefault
  { _sdbDevice = pack $ SdbInterconnect
    { _sdbMagic   = 0x5344422D
    , _sdbRecords = 2
    , _sdbFmtVer  = 1
    , _sdbBusType = 0
    }
  , _sdbAddrFirst = 0x00000000
  , _sdbAddrLast  = 0xffffffff

  , _sdbName      = fmap charConvert $(listToVecTH "EtherboneDemo      ")
  , _sdbType      = 0
  }

scratchpadSdb :: SdbComponent
scratchpadSdb
  = sdbComponentDefault
  { _sdbDevice = pack $ SdbDevice
    { _sdbClass    = 0
    , _sdbVerMajor = 1
    , _sdbVerMinor = 1
    , _sdbPad0     = 0
    , _sdbEndian   = 1
    , _sdbPad1     = 0
    , _sdbWidth    = 0b0100
    }
  , _sdbAddrFirst  = 0x00000000
  , _sdbAddrLast   = 0x0000000f
  , _sdbName       = fmap charConvert $(listToVecTH "Scratchpad         ")
  }

-- TODO: Make dataWidth variable
sdbRom :: forall dom addrWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , BitPack dat
  , NFDataX dat
  , BitSize dat ~ 8*DataWidth
  )
  => Signal dom (BitVector addrWidth) -> Signal dom dat
sdbRom adr = rom (bitCoerce sdbData) (unpack <$> adr)
  where
    sdbData = sdbCrossbar :> scratchpadSdb :> Nil

wishboneRom :: forall dom addrWidth dat.
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , NFDataX dat
  , BitSize dat ~ 8*DataWidth
  )
  => (Signal dom (BitVector addrWidth) -> Signal dom dat)
  -> Circuit (Wishbone dom Standard addrWidth dat) ()
wishboneRom myRom = Circuit go
  where
    go (iFwd, _) = (out <$> dat <*> ack, ())
      where
        ack = (busCycle <$> iFwd) .&&. (strobe <$> iFwd)

        bitsToShift = natToNum @(CLog 2 (ByteSize dat))

        adr = shiftR <$> (addr <$> iFwd) <*> bitsToShift
        dat = myRom adr
        out x ackn = (emptyWishboneS2M @dat) { readData=x, acknowledge=ackn}
