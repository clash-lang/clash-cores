{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Block RAM of Intel FPGAs (@altsyncram@) in single-port, simple dual-port and
true dual-port mode, with one or two clocks, single-port and dual-port ROM,
and the RAM-based shift register @altshift_taps@. For more
information see the /Embedded Memory User Guide/
(<https://www.intel.com/content/www/us/en/docs/programmable/683240/current/embedded-memory-user-guide.html>)
and the /Shift Register (RAM-based) IP Core User Guide/
(<https://www.intel.com/content/www/us/en/docs/programmable/683490/current/shift-register-ram-based-ip-core-user-guide.html>).

= Ports

Addresses are 'Index'es, so the depth of a memory doesn't need to be a power of
two. A write is a @'Just' (byteEnable, data)@, with one bit in the byte enable
per byte of the data: byte /i/ consists of bits @[i * w, (i + 1) * w)@ of the
'pack'ed data, with @w@ the width of a byte. Use a byte enable of a single bit
to write whole words. Which byte widths a device supports depends on its
memory blocks; 8 and 9 are supported by all of them.

A port reads the word at its address in every enabled clock cycle, whether it
writes or not. The read data appears on the output in the next clock cycle, or
a cycle later with an output register. While the enable is deasserted, the
memory doesn't read or write, and its outputs keep their values.

= Read during write

When a port writes, it also reads the address it writes to: a /same-port/
read-during-write. What it reads depends on its 'ReadDuringWrite' mode. When a
port reads an address the other port writes, the read is a /mixed-port/
read-during-write, and returns the old contents ('MixedOldData') or undefined
data ('MixedDontCare'). When both ports write the same address, the bytes they
both write become undefined, and so do the read results of both ports.

When the ports have different clocks (e.g. 'altsyncramTrueDualPortDualClock'),
the relation between the clocks is unknown, and so is the result of a
mixed-port read-during-write. The simulation models consider a write of a port
to be in progress from the clock edge that writes until the next clock edge at
which the port is enabled: Intel's VHDL simulation model only completes a write
then. A read of the other port in that time returns undefined data in the
bytes written. When both ports write the same address in overlapping cycles,
the bytes they both write become undefined.

Not every memory block supports every mode, see the user guide:

  * 'OldData' isn't supported by M10K (Cyclone V) and M20K blocks in
    single-port and true dual-port mode.
  * 'NewDataOldMaskedBytes' is only supported by M9K blocks (Cyclone IV,
    Cyclone 10 LP, MAX 10).

= Initialization

Without an initialization file, the memory starts with all zeroes. An
initialization file is a Memory Initialization File (@.mif@), see
'writeMifFile'. Quartus and the simulators look for it relative to the
directory they run in, so use an absolute path, or a path relative to your
Quartus project. ROMs need one.

MAX 10 devices only support initialized memories (including ROMs) in an
internal configuration mode with memory initialization, e.g.
@set_global_assignment -name INTERNAL_FLASH_UPDATE_MODE \"SINGLE IMAGE WITH ERAM\"@
in the Quartus settings file.

= Unsupported features

The ports of a RAM or ROM either share a clock and clock enable (@clock0@,
@clocken0@), or each have their own (@clock0@, @clocken0@ and @clock1@,
@clocken1@). Separate input and output clocks or clock enables, read enables
(@rden_a@, @rden_b@), address stalls, clearing the output registers (@aclr0@,
@aclr1@), ECC and ports of different widths are not supported. Neither is the
error correction code megafunction @altecc@: Quartus ships no simulation model
of it.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Intel.Memory
  ( -- * RAM
    altsyncramSinglePort
  , altsyncramSimpleDualPort
  , altsyncramTrueDualPort
    -- * RAM with two clocks
  , altsyncramSimpleDualPortDualClock
  , altsyncramTrueDualPortDualClock
    -- * ROM
  , altsyncramRom
  , altsyncramDualPortRom
    -- * Shift register
  , altshiftTaps
    -- * Configuration
  , AltsyncramConfig (..)
  , defAltsyncramConfig
  , PortConfig (..)
  , defPortConfig
  , ReadDuringWrite (..)
  , MixedReadDuringWrite (..)
  , RamBlockType (..)
    -- * Initialization files
  , writeMifFile
  , mifContents
  , readMifFile
  ) where

import Clash.Explicit.Prelude

import Clash.Signal.Internal (ClockAB (..), Signal ((:-)), clockTicks)
import Clash.Sized.Internal.BitVector (BitVector (BV))
import Control.Exception (evaluate)
import Data.Char (isDigit, isHexDigit, isSpace, toUpper, digitToInt)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import qualified Prelude as P

import Clash.Cores.Intel.Primitive.Internal (alteraMf)
import Clash.Cores.Xilinx.Xpm.Cdc.Internal
  ( ClockPort (..), Param (..), Port (..), inst, unPort )

-- | What a port reads when it writes (same-port read-during-write)
data ReadDuringWrite
  = -- | The new data, and undefined data in the bytes it doesn't write
    -- (@NEW_DATA_NO_NBE_READ@)
    NewData
  | -- | The new data, and the old data in the bytes it doesn't write
    -- (@NEW_DATA_WITH_NBE_READ@). Only supported by M9K blocks.
    NewDataOldMaskedBytes
  | -- | The old data (@OLD_DATA@). Not supported by M10K and M20K blocks in
    -- single-port and true dual-port mode.
    OldData
  | -- | Undefined data (@DONT_CARE@)
    DontCare
  deriving (Show, Eq, Generic, NFDataX)

-- | What a port reads when the other port writes the same address
-- (mixed-port read-during-write)
data MixedReadDuringWrite
  = -- | The old data (@OLD_DATA@)
    MixedOldData
  | -- | Undefined data (@DONT_CARE@)
    MixedDontCare
  deriving (Show, Eq, Generic, NFDataX)

-- | Memory block to use (@ram_block_type@)
data RamBlockType
  = -- | Let Quartus choose (@AUTO@)
    AutoBlock
  | -- | M9K (Cyclone IV, Cyclone 10 LP, MAX 10)
    M9K
  | -- | M10K (Cyclone V)
    M10K
  | -- | M20K (Arria 10, Cyclone 10 GX, Stratix V)
    M20K
  | -- | M144K (Stratix IV)
    M144K
  deriving (Show, Eq, Generic, NFDataX)

-- | Configuration of a port
data PortConfig = PortConfig
  { outputRegister :: Bool
  -- ^ Register the output (@outdata_reg@), which delays the read data by a
  -- cycle
  , readDuringWrite :: ReadDuringWrite
  -- ^ What the port reads when it writes (@read_during_write_mode_port@).
  -- Ignored for the read port of a simple dual-port RAM.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration of a port: no output register, and 'NewData'
defPortConfig :: PortConfig
defPortConfig = PortConfig{outputRegister = False, readDuringWrite = NewData}

-- | Configuration of the memory
data AltsyncramConfig = AltsyncramConfig
  { ramBlockType :: RamBlockType
  -- ^ Memory block to use (@ram_block_type@)
  , mixedReadDuringWrite :: MixedReadDuringWrite
  -- ^ What a port reads when the other port writes the same address
  -- (@read_during_write_mode_mixed_ports@). Ignored for single-port RAMs.
  , initFile :: Maybe FilePath
  -- ^ Initialization file (@init_file@), a @.mif@ file, see 'writeMifFile'.
  -- 'Nothing' starts with all zeroes.
  }
  deriving (Show, Eq, Generic, NFDataX)

-- | Default configuration: memory block chosen by Quartus, 'MixedDontCare',
-- and no initialization file
defAltsyncramConfig :: AltsyncramConfig
defAltsyncramConfig =
  AltsyncramConfig
    { ramBlockType = AutoBlock
    , mixedReadDuringWrite = MixedDontCare
    , initFile = Nothing
    }

ramBlockTypeParam :: RamBlockType -> String
ramBlockTypeParam = \case
  AutoBlock -> "AUTO"
  M9K -> "M9K"
  M10K -> "M10K"
  M20K -> "M20K"
  M144K -> "M144K"

rdwParam :: ReadDuringWrite -> String
rdwParam = \case
  NewData -> "NEW_DATA_NO_NBE_READ"
  NewDataOldMaskedBytes -> "NEW_DATA_WITH_NBE_READ"
  OldData -> "OLD_DATA"
  DontCare -> "DONT_CARE"

mixedRdwParam :: MixedReadDuringWrite -> String
mixedRdwParam = \case
  MixedOldData -> "OLD_DATA"
  MixedDontCare -> "DONT_CARE"

outdataRegParam :: PortConfig -> String
outdataRegParam p = if outputRegister p then "CLOCK0" else "UNREGISTERED"

-- | 'outdataRegParam' for port B of a RAM with two clocks
outdataRegParamB :: PortConfig -> String
outdataRegParamB p = if outputRegister p then "CLOCK1" else "UNREGISTERED"

initFileParam :: AltsyncramConfig -> String
initFileParam cfg = fromMaybe "UNUSED" (initFile cfg)

-- * RAM

{- | Single-port RAM (@altsyncram@ with @operation_mode = \"SINGLE_PORT\"@)
holding @n@ words of type @a@. See "Clash.Cores.Intel.Memory" for the behavior
of the port.

The simulation model matches Intel's simulation model (@altera_mf@) for
Cyclone IV E and Cyclone V.
-}
altsyncramSinglePort ::
  forall n nBytes a dom deviceFamily.
  ( HasCallStack
  , KnownDomain dom
  , KnownNat n
  , 2 <= n
  , KnownNat nBytes
  , 1 <= nBytes
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  AltsyncramConfig ->
  PortConfig ->
  -- | @clock0@
  Clock dom ->
  -- | @clocken0@
  Enable dom ->
  -- | Address (@address_a@)
  Signal dom (Index n) ->
  -- | Write: byte enable (@byteena_a@) and data (@data_a@), or 'Nothing'
  -- (@wren_a@)
  Signal dom (Maybe (BitVector nBytes, a)) ->
  -- | Read data (@q_a@)
  Signal dom a
altsyncramSinglePort _ cfg cfgA clk en addr wr
  | clashSimulation =
      unpack . fst
        <$> bundle
          ( model @n @(BitSize a) @nBytes cfg cfgA defPortConfig clk en
              (bundle (addr, fmap (fmap pack) <$> wr))
              (pure (0, Nothing)) )
  | otherwise = unpack <$> unPort qA
 where
  qA :: Port "q_a" dom (BitVector (BitSize a))
  qA =
    inst
      (alteraMf "altsyncram")
      (Param @"operation_mode" @String "SINGLE_PORT")
      (Param @"width_a" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_a" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_a" @Integer (natToNum @n))
      (Param @"width_byteena_a" @Integer (natToNum @nBytes))
      (Param @"byte_size" @Integer (natToNum @(BitSize a) `P.div` natToNum @nBytes))
      (Param @"outdata_reg_a" @String (outdataRegParam cfgA))
      (Param @"outdata_aclr_a" @String "NONE")
      (Param @"clock_enable_input_a" @String "NORMAL")
      (Param @"clock_enable_output_a" @String "NORMAL")
      (Param @"read_during_write_mode_port_a" @String (rdwParam (readDuringWrite cfgA)))
      (Param @"ram_block_type" @String (ramBlockTypeParam (ramBlockType cfg)))
      (Param @"init_file" @String (initFileParam cfg))
      (Param @"power_up_uninitialized" @String "FALSE")
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"lpm_type" @String "altsyncram")
      (ClockPort @"clock0" clk)
      (Port @"clocken0" (boolToBit <$> fromEnable en))
      (Port @"address_a" (pack <$> addr))
      (Port @"wren_a" (boolToBit . isJust <$> wr))
      (Port @"byteena_a" (maybe 0 fst <$> wr))
      (Port @"data_a" (pack . snd . fromJustX <$> wr))

{- | Simple dual-port RAM (@altsyncram@ with @operation_mode =
\"DUAL_PORT\"@) holding @n@ words of type @a@, with a write port (A) and a read
port (B). See "Clash.Cores.Intel.Memory" for the behavior of the ports. A read
of the address that is written in the same cycle is a mixed-port
read-during-write, see 'mixedReadDuringWrite'.

The simulation model matches Intel's simulation model (@altera_mf@) for
Cyclone IV E and Cyclone V.
-}
altsyncramSimpleDualPort ::
  forall n nBytes a dom deviceFamily.
  ( HasCallStack
  , KnownDomain dom
  , KnownNat n
  , 2 <= n
  , KnownNat nBytes
  , 1 <= nBytes
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  AltsyncramConfig ->
  -- | Configuration of the read port. Its 'readDuringWrite' is ignored.
  PortConfig ->
  -- | @clock0@
  Clock dom ->
  -- | @clocken0@
  Enable dom ->
  -- | Write: address (@address_a@), byte enable (@byteena_a@) and data
  -- (@data_a@), or 'Nothing' (@wren_a@)
  Signal dom (Maybe (Index n, BitVector nBytes, a)) ->
  -- | Read address (@address_b@)
  Signal dom (Index n) ->
  -- | Read data (@q_b@)
  Signal dom a
altsyncramSimpleDualPort _ cfg cfgB clk en wr rdAddr
  | clashSimulation =
      unpack . snd
        <$> bundle
          ( model @n @(BitSize a) @nBytes cfg defPortConfig cfgB clk en
              ((\w -> case w of
                  Just (a, be, d) -> (a, Just (be, pack d))
                  Nothing -> (0, Nothing)) <$> wr)
              ((\a -> (a, Nothing)) <$> rdAddr) )
  | otherwise = unpack <$> unPort qB
 where
  qB :: Port "q_b" dom (BitVector (BitSize a))
  qB =
    inst
      (alteraMf "altsyncram")
      (Param @"operation_mode" @String "DUAL_PORT")
      (Param @"width_a" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_a" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_a" @Integer (natToNum @n))
      (Param @"width_byteena_a" @Integer (natToNum @nBytes))
      (Param @"byte_size" @Integer (natToNum @(BitSize a) `P.div` natToNum @nBytes))
      (Param @"width_b" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_b" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_b" @Integer (natToNum @n))
      (Param @"address_reg_b" @String "CLOCK0")
      (Param @"outdata_reg_b" @String (outdataRegParam cfgB))
      (Param @"outdata_aclr_b" @String "NONE")
      (Param @"address_aclr_b" @String "NONE")
      (Param @"clock_enable_input_a" @String "NORMAL")
      (Param @"clock_enable_input_b" @String "NORMAL")
      (Param @"clock_enable_output_b" @String "NORMAL")
      (Param @"read_during_write_mode_mixed_ports" @String (mixedRdwParam (mixedReadDuringWrite cfg)))
      (Param @"ram_block_type" @String (ramBlockTypeParam (ramBlockType cfg)))
      (Param @"init_file" @String (initFileParam cfg))
      (Param @"power_up_uninitialized" @String "FALSE")
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"lpm_type" @String "altsyncram")
      (ClockPort @"clock0" clk)
      (Port @"clocken0" (boolToBit <$> fromEnable en))
      (Port @"address_a" (pack . (\(a, _, _) -> a) . fromJustX <$> wr))
      (Port @"wren_a" (boolToBit . isJust <$> wr))
      (Port @"byteena_a" (maybe 0 (\(_, be, _) -> be) <$> wr))
      (Port @"data_a" (pack . (\(_, _, d) -> d) . fromJustX <$> wr))
      (Port @"address_b" (pack <$> rdAddr))

{- | True dual-port RAM (@altsyncram@ with @operation_mode =
\"BIDIR_DUAL_PORT\"@) holding @n@ words of type @a@, with two ports that read
and write. See "Clash.Cores.Intel.Memory" for the behavior of the ports.

The simulation model matches Intel's simulation model (@altera_mf@) for
Cyclone IV E and Cyclone V.
-}
altsyncramTrueDualPort ::
  forall n nBytes a dom deviceFamily.
  ( HasCallStack
  , KnownDomain dom
  , KnownNat n
  , 2 <= n
  , KnownNat nBytes
  , 1 <= nBytes
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  AltsyncramConfig ->
  -- | Configuration of port A
  PortConfig ->
  -- | Configuration of port B
  PortConfig ->
  -- | @clock0@
  Clock dom ->
  -- | @clocken0@
  Enable dom ->
  -- | Port A: address (@address_a@) and write (@byteena_a@, @data_a@ and
  -- @wren_a@)
  Signal dom (Index n, Maybe (BitVector nBytes, a)) ->
  -- | Port B: address (@address_b@) and write (@byteena_b@, @data_b@ and
  -- @wren_b@)
  Signal dom (Index n, Maybe (BitVector nBytes, a)) ->
  -- | Read data of port A (@q_a@) and port B (@q_b@)
  (Signal dom a, Signal dom a)
altsyncramTrueDualPort _ cfg cfgA cfgB clk en portA portB
  | clashSimulation =
      let
        (oA, oB) =
          model @n @(BitSize a) @nBytes cfg cfgA cfgB clk en
            (fmap (fmap (fmap pack)) <$> portA)
            (fmap (fmap (fmap pack)) <$> portB)
      in (unpack <$> oA, unpack <$> oB)
  | otherwise = (unpack <$> unPort qA, unpack <$> unPort qB)
 where
  (addrA, wrA) = unbundle portA
  (addrB, wrB) = unbundle portB
  (qA, qB) = ports
  ports :: (Port "q_a" dom (BitVector (BitSize a)), Port "q_b" dom (BitVector (BitSize a)))
  ports =
    inst
      (alteraMf "altsyncram")
      (Param @"operation_mode" @String "BIDIR_DUAL_PORT")
      (Param @"width_a" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_a" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_a" @Integer (natToNum @n))
      (Param @"width_byteena_a" @Integer (natToNum @nBytes))
      (Param @"byte_size" @Integer (natToNum @(BitSize a) `P.div` natToNum @nBytes))
      (Param @"outdata_reg_a" @String (outdataRegParam cfgA))
      (Param @"outdata_aclr_a" @String "NONE")
      (Param @"read_during_write_mode_port_a" @String (rdwParam (readDuringWrite cfgA)))
      (Param @"width_b" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_b" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_b" @Integer (natToNum @n))
      (Param @"width_byteena_b" @Integer (natToNum @nBytes))
      (Param @"address_reg_b" @String "CLOCK0")
      (Param @"indata_reg_b" @String "CLOCK0")
      (Param @"wrcontrol_wraddress_reg_b" @String "CLOCK0")
      (Param @"byteena_reg_b" @String "CLOCK0")
      (Param @"outdata_reg_b" @String (outdataRegParam cfgB))
      (Param @"outdata_aclr_b" @String "NONE")
      (Param @"address_aclr_b" @String "NONE")
      (Param @"read_during_write_mode_port_b" @String (rdwParam (readDuringWrite cfgB)))
      (Param @"read_during_write_mode_mixed_ports" @String (mixedRdwParam (mixedReadDuringWrite cfg)))
      (Param @"clock_enable_input_a" @String "NORMAL")
      (Param @"clock_enable_output_a" @String "NORMAL")
      (Param @"clock_enable_input_b" @String "NORMAL")
      (Param @"clock_enable_output_b" @String "NORMAL")
      (Param @"ram_block_type" @String (ramBlockTypeParam (ramBlockType cfg)))
      (Param @"init_file" @String (initFileParam cfg))
      (Param @"power_up_uninitialized" @String "FALSE")
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"lpm_type" @String "altsyncram")
      (ClockPort @"clock0" clk)
      (Port @"clocken0" (boolToBit <$> fromEnable en))
      (Port @"address_a" (pack <$> addrA))
      (Port @"wren_a" (boolToBit . isJust <$> wrA))
      (Port @"byteena_a" (maybe 0 fst <$> wrA))
      (Port @"data_a" (pack . snd . fromJustX <$> wrA))
      (Port @"address_b" (pack <$> addrB))
      (Port @"wren_b" (boolToBit . isJust <$> wrB))
      (Port @"byteena_b" (maybe 0 fst <$> wrB))
      (Port @"data_b" (pack . snd . fromJustX <$> wrB))

-- * ROM

{- | Single-port ROM (@altsyncram@ with @operation_mode = \"ROM\"@) holding @n@
words of type @a@, with the contents of the initialization file
('initFile', required). See "Clash.Cores.Intel.Memory" for the behavior of the
port. The 'mixedReadDuringWrite' of the configuration and the
'readDuringWrite' of the port are ignored.

The simulation model matches Intel's simulation model (@altera_mf@) for
Cyclone IV E, Cyclone V and MAX 10.
-}
altsyncramRom ::
  forall n a dom deviceFamily.
  ( HasCallStack
  , KnownDomain dom
  , KnownNat n
  , 2 <= n
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  AltsyncramConfig ->
  PortConfig ->
  -- | @clock0@
  Clock dom ->
  -- | @clocken0@
  Enable dom ->
  -- | Address (@address_a@)
  Signal dom (Index n) ->
  -- | Read data (@q_a@)
  Signal dom a
altsyncramRom _ cfg cfgA clk en addr
  | clashSimulation =
      unpack . fst
        <$> bundle
          ( model @n @(BitSize a) @1 (romConfig cfg) cfgA defPortConfig clk en
              ((\a -> (a, Nothing)) <$> addr)
              (pure (0, Nothing)) )
  | otherwise = unpack <$> unPort qA
 where
  qA :: Port "q_a" dom (BitVector (BitSize a))
  qA =
    inst
      (alteraMf "altsyncram")
      (Param @"operation_mode" @String "ROM")
      (Param @"width_a" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_a" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_a" @Integer (natToNum @n))
      (Param @"outdata_reg_a" @String (outdataRegParam cfgA))
      (Param @"address_aclr_a" @String "NONE")
      (Param @"outdata_aclr_a" @String "NONE")
      (Param @"clock_enable_input_a" @String "NORMAL")
      (Param @"clock_enable_output_a" @String "NORMAL")
      (Param @"ram_block_type" @String (ramBlockTypeParam (ramBlockType cfg)))
      (Param @"init_file" @String (initFileParam (romConfig cfg)))
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"lpm_type" @String "altsyncram")
      (ClockPort @"clock0" clk)
      (Port @"clocken0" (boolToBit <$> fromEnable en))
      (Port @"address_a" (pack <$> addr))

{- | Dual-port ROM (@altsyncram@ with @operation_mode = \"BIDIR_DUAL_PORT\"@
and no writes) holding @n@ words of type @a@, with the contents of the
initialization file ('initFile', required). The ports have their own clocks
(@clock0@ and @clock1@), which can be the same. See
"Clash.Cores.Intel.Memory" for the behavior of the ports. The
'readDuringWrite' of the ports is ignored. Use 'MixedOldData' for M9K blocks
(Cyclone IV, Cyclone 10 LP and MAX 10), for which Intel's simulation model
warns that it doesn't support anything else, and 'MixedDontCare' otherwise.

The simulation model matches Intel's simulation model (@altera_mf@) for
Cyclone IV E, Cyclone V and MAX 10.
-}
altsyncramDualPortRom ::
  forall n a domA domB deviceFamily.
  ( HasCallStack
  , KnownDomain domA
  , KnownDomain domB
  , KnownNat n
  , 2 <= n
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  AltsyncramConfig ->
  -- | Configuration of port A
  PortConfig ->
  -- | Configuration of port B
  PortConfig ->
  -- | @clock0@
  Clock domA ->
  -- | @clocken0@
  Enable domA ->
  -- | Address of port A (@address_a@)
  Signal domA (Index n) ->
  -- | @clock1@
  Clock domB ->
  -- | @clocken1@
  Enable domB ->
  -- | Address of port B (@address_b@)
  Signal domB (Index n) ->
  -- | Read data of port A (@q_a@) and port B (@q_b@)
  (Signal domA a, Signal domB a)
altsyncramDualPortRom _ cfg cfgA cfgB clkA enA addrA clkB enB addrB
  | clashSimulation =
      let
        (oA, oB) =
          model2 @n @(BitSize a) @1 (romConfig cfg) cfgA cfgB
            clkA enA ((\a -> (a, Nothing)) <$> addrA)
            clkB enB ((\a -> (a, Nothing)) <$> addrB)
      in (unpack <$> oA, unpack <$> oB)
  | otherwise = (unpack <$> unPort qA, unpack <$> unPort qB)
 where
  (qA, qB) = ports
  ports :: (Port "q_a" domA (BitVector (BitSize a)), Port "q_b" domB (BitVector (BitSize a)))
  ports =
    inst
      (alteraMf "altsyncram")
      (Param @"operation_mode" @String "BIDIR_DUAL_PORT")
      (Param @"width_a" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_a" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_a" @Integer (natToNum @n))
      (Param @"outdata_reg_a" @String (outdataRegParam cfgA))
      (Param @"outdata_aclr_a" @String "NONE")
      (Param @"address_aclr_a" @String "NONE")
      (Param @"width_b" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_b" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_b" @Integer (natToNum @n))
      (Param @"address_reg_b" @String "CLOCK1")
      (Param @"indata_reg_b" @String "CLOCK1")
      (Param @"wrcontrol_wraddress_reg_b" @String "CLOCK1")
      (Param @"outdata_reg_b" @String (outdataRegParamB cfgB))
      (Param @"outdata_aclr_b" @String "NONE")
      (Param @"address_aclr_b" @String "NONE")
      (Param @"read_during_write_mode_mixed_ports" @String (mixedRdwParam (mixedReadDuringWrite cfg)))
      (Param @"clock_enable_input_a" @String "NORMAL")
      (Param @"clock_enable_output_a" @String "NORMAL")
      (Param @"clock_enable_input_b" @String "NORMAL")
      (Param @"clock_enable_output_b" @String "NORMAL")
      (Param @"ram_block_type" @String (ramBlockTypeParam (ramBlockType cfg)))
      (Param @"init_file" @String (initFileParam (romConfig cfg)))
      (Param @"power_up_uninitialized" @String "FALSE")
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"lpm_type" @String "altsyncram")
      (ClockPort @"clock0" clkA)
      (Port @"clocken0" (boolToBit <$> fromEnable enA))
      (Port @"address_a" (pack <$> addrA))
      (Port @"wren_a" (pure 0 :: Signal domA Bit))
      (ClockPort @"clock1" clkB)
      (Port @"clocken1" (boolToBit <$> fromEnable enB))
      (Port @"address_b" (pack <$> addrB))
      (Port @"wren_b" (pure 0 :: Signal domB Bit))

-- | Check that a ROM has an initialization file
romConfig :: HasCallStack => AltsyncramConfig -> AltsyncramConfig
romConfig cfg = case initFile cfg of
  Just _ -> cfg
  Nothing -> error "altsyncram: a ROM needs an initialization file (initFile)"

-- * Dual-clock RAM

{- | Simple dual-port RAM (@altsyncram@ with @operation_mode =
\"DUAL_PORT\"@) holding @n@ words of type @a@, with a write port (A) and a read
port (B) with their own clocks (@clock0@ and @clock1@). See
"Clash.Cores.Intel.Memory" for the behavior of the ports, and
'altsyncramSimpleDualPort' for a RAM with a single clock.

The relation between the clocks is unknown, so the result of reading an
address that is being written is undefined, see "Read during write" in
"Clash.Cores.Intel.Memory". The simulation model ignores the
'mixedReadDuringWrite' of the configuration, but it does end up in the HDL:
use 'MixedDontCare' for M10K and M20K blocks (Quartus doesn't support
'MixedOldData' with two clocks), and 'MixedOldData' for M9K blocks (Cyclone IV,
Cyclone 10 LP and MAX 10), for which Intel's simulation model warns that it
doesn't support anything else.

The simulation model matches Intel's simulation model (@altera_mf@) for
Cyclone IV E, Cyclone V and MAX 10.
-}
altsyncramSimpleDualPortDualClock ::
  forall n nBytes a domA domB deviceFamily.
  ( HasCallStack
  , KnownDomain domA
  , KnownDomain domB
  , KnownNat n
  , 2 <= n
  , KnownNat nBytes
  , 1 <= nBytes
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  AltsyncramConfig ->
  -- | Configuration of the read port. Its 'readDuringWrite' is ignored.
  PortConfig ->
  -- | @clock0@
  Clock domA ->
  -- | @clocken0@
  Enable domA ->
  -- | Write: address (@address_a@), byte enable (@byteena_a@) and data
  -- (@data_a@), or 'Nothing' (@wren_a@)
  Signal domA (Maybe (Index n, BitVector nBytes, a)) ->
  -- | @clock1@
  Clock domB ->
  -- | @clocken1@
  Enable domB ->
  -- | Read address (@address_b@)
  Signal domB (Index n) ->
  -- | Read data (@q_b@)
  Signal domB a
altsyncramSimpleDualPortDualClock _ cfg cfgB clkA enA wr clkB enB rdAddr
  | clashSimulation =
      fmap unpack . snd
        $ model2 @n @(BitSize a) @nBytes cfg defPortConfig cfgB
            clkA enA
            ((\w -> case w of
                Just (a, be, d) -> (a, Just (be, pack d))
                Nothing -> (0, Nothing)) <$> wr)
            clkB enB
            ((\a -> (a, Nothing)) <$> rdAddr)
  | otherwise = unpack <$> unPort qB
 where
  qB :: Port "q_b" domB (BitVector (BitSize a))
  qB =
    inst
      (alteraMf "altsyncram")
      (Param @"operation_mode" @String "DUAL_PORT")
      (Param @"width_a" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_a" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_a" @Integer (natToNum @n))
      (Param @"width_byteena_a" @Integer (natToNum @nBytes))
      (Param @"byte_size" @Integer (natToNum @(BitSize a) `P.div` natToNum @nBytes))
      (Param @"width_b" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_b" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_b" @Integer (natToNum @n))
      (Param @"address_reg_b" @String "CLOCK1")
      (Param @"outdata_reg_b" @String (outdataRegParamB cfgB))
      (Param @"outdata_aclr_b" @String "NONE")
      (Param @"address_aclr_b" @String "NONE")
      (Param @"clock_enable_input_a" @String "NORMAL")
      (Param @"clock_enable_input_b" @String "NORMAL")
      (Param @"clock_enable_output_b" @String "NORMAL")
      (Param @"read_during_write_mode_mixed_ports" @String (mixedRdwParam (mixedReadDuringWrite cfg)))
      (Param @"ram_block_type" @String (ramBlockTypeParam (ramBlockType cfg)))
      (Param @"init_file" @String (initFileParam cfg))
      (Param @"power_up_uninitialized" @String "FALSE")
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"lpm_type" @String "altsyncram")
      (ClockPort @"clock0" clkA)
      (Port @"clocken0" (boolToBit <$> fromEnable enA))
      (Port @"address_a" (pack . (\(a, _, _) -> a) . fromJustX <$> wr))
      (Port @"wren_a" (boolToBit . isJust <$> wr))
      (Port @"byteena_a" (maybe 0 (\(_, be, _) -> be) <$> wr))
      (Port @"data_a" (pack . (\(_, _, d) -> d) . fromJustX <$> wr))
      (ClockPort @"clock1" clkB)
      (Port @"clocken1" (boolToBit <$> fromEnable enB))
      (Port @"address_b" (pack <$> rdAddr))

{- | True dual-port RAM (@altsyncram@ with @operation_mode =
\"BIDIR_DUAL_PORT\"@) holding @n@ words of type @a@, with two ports that read
and write, with their own clocks (@clock0@ and @clock1@). See
"Clash.Cores.Intel.Memory" for the behavior of the ports, and
'altsyncramTrueDualPort' for a RAM with a single clock.

The relation between the clocks is unknown, so the result of reading an
address that the other port is writing is undefined, see "Read during write"
in "Clash.Cores.Intel.Memory". As for 'altsyncramSimpleDualPortDualClock',
use 'MixedDontCare' for M10K and M20K blocks and 'MixedOldData' for M9K
blocks.

The simulation model matches Intel's simulation model (@altera_mf@) for
Cyclone IV E, Cyclone V and MAX 10.
-}
altsyncramTrueDualPortDualClock ::
  forall n nBytes a domA domB deviceFamily.
  ( HasCallStack
  , KnownDomain domA
  , KnownDomain domB
  , KnownNat n
  , 2 <= n
  , KnownNat nBytes
  , 1 <= nBytes
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  AltsyncramConfig ->
  -- | Configuration of port A
  PortConfig ->
  -- | Configuration of port B
  PortConfig ->
  -- | @clock0@
  Clock domA ->
  -- | @clocken0@
  Enable domA ->
  -- | Port A: address (@address_a@) and write (@byteena_a@, @data_a@ and
  -- @wren_a@)
  Signal domA (Index n, Maybe (BitVector nBytes, a)) ->
  -- | @clock1@
  Clock domB ->
  -- | @clocken1@
  Enable domB ->
  -- | Port B: address (@address_b@) and write (@byteena_b@, @data_b@ and
  -- @wren_b@)
  Signal domB (Index n, Maybe (BitVector nBytes, a)) ->
  -- | Read data of port A (@q_a@) and port B (@q_b@)
  (Signal domA a, Signal domB a)
altsyncramTrueDualPortDualClock _ cfg cfgA cfgB clkA enA portA clkB enB portB
  | clashSimulation =
      let
        (oA, oB) =
          model2 @n @(BitSize a) @nBytes cfg cfgA cfgB
            clkA enA (fmap (fmap (fmap pack)) <$> portA)
            clkB enB (fmap (fmap (fmap pack)) <$> portB)
      in (unpack <$> oA, unpack <$> oB)
  | otherwise = (unpack <$> unPort qA, unpack <$> unPort qB)
 where
  (addrA, wrA) = unbundle portA
  (addrB, wrB) = unbundle portB
  (qA, qB) = ports
  ports :: (Port "q_a" domA (BitVector (BitSize a)), Port "q_b" domB (BitVector (BitSize a)))
  ports =
    inst
      (alteraMf "altsyncram")
      (Param @"operation_mode" @String "BIDIR_DUAL_PORT")
      (Param @"width_a" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_a" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_a" @Integer (natToNum @n))
      (Param @"width_byteena_a" @Integer (natToNum @nBytes))
      (Param @"byte_size" @Integer (natToNum @(BitSize a) `P.div` natToNum @nBytes))
      (Param @"outdata_reg_a" @String (outdataRegParam cfgA))
      (Param @"outdata_aclr_a" @String "NONE")
      (Param @"read_during_write_mode_port_a" @String (rdwParam (readDuringWrite cfgA)))
      (Param @"width_b" @Integer (natToNum @(BitSize a)))
      (Param @"widthad_b" @Integer (natToNum @(CLog 2 n)))
      (Param @"numwords_b" @Integer (natToNum @n))
      (Param @"width_byteena_b" @Integer (natToNum @nBytes))
      (Param @"address_reg_b" @String "CLOCK1")
      (Param @"indata_reg_b" @String "CLOCK1")
      (Param @"wrcontrol_wraddress_reg_b" @String "CLOCK1")
      (Param @"byteena_reg_b" @String "CLOCK1")
      (Param @"outdata_reg_b" @String (outdataRegParamB cfgB))
      (Param @"outdata_aclr_b" @String "NONE")
      (Param @"address_aclr_b" @String "NONE")
      (Param @"read_during_write_mode_port_b" @String (rdwParam (readDuringWrite cfgB)))
      (Param @"read_during_write_mode_mixed_ports" @String (mixedRdwParam (mixedReadDuringWrite cfg)))
      (Param @"clock_enable_input_a" @String "NORMAL")
      (Param @"clock_enable_output_a" @String "NORMAL")
      (Param @"clock_enable_input_b" @String "NORMAL")
      (Param @"clock_enable_output_b" @String "NORMAL")
      (Param @"ram_block_type" @String (ramBlockTypeParam (ramBlockType cfg)))
      (Param @"init_file" @String (initFileParam cfg))
      (Param @"power_up_uninitialized" @String "FALSE")
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"lpm_type" @String "altsyncram")
      (ClockPort @"clock0" clkA)
      (Port @"clocken0" (boolToBit <$> fromEnable enA))
      (Port @"address_a" (pack <$> addrA))
      (Port @"wren_a" (boolToBit . isJust <$> wrA))
      (Port @"byteena_a" (maybe 0 fst <$> wrA))
      (Port @"data_a" (pack . snd . fromJustX <$> wrA))
      (ClockPort @"clock1" clkB)
      (Port @"clocken1" (boolToBit <$> fromEnable enB))
      (Port @"address_b" (pack <$> addrB))
      (Port @"wren_b" (boolToBit . isJust <$> wrB))
      (Port @"byteena_b" (maybe 0 fst <$> wrB))
      (Port @"data_b" (pack . snd . fromJustX <$> wrB))

-- * Shift register

{- | RAM-based shift register with taps (@altshift_taps@): a chain of
@nTaps * distance@ registers for words of type @a@, with a tap after every
@distance@ registers. Returns the output of the last register (@shiftout@) and
the taps (@taps@), the nearest tap first: tap /k/ is the input delayed by
@(k + 1) * distance@ enabled clock cycles. The last tap equals @shiftout@.

The registers start cleared (@power_up_state = \"CLEARED\"@). There is no
reset: Quartus Prime Lite 21.1 doesn't support the synchronous clear (@sclr@)
of Intel's simulation model, and its implementation of the asynchronous clear
(@aclr@) only clears the output registers of the RAM, while the simulation
model clears the whole shift register.

The simulation model matches Intel's simulation model (@altera_mf@) cycle by
cycle.
-}
altshiftTaps ::
  forall nTaps distance a dom deviceFamily.
  ( HasCallStack
  , KnownDomain dom
  , KnownNat nTaps
  , 1 <= nTaps
  , KnownNat distance
  , 3 <= distance
  , BitPack a
  , NFDataX a
  , KnownSymbol deviceFamily
  ) =>
  -- | The FPGA family (@intended_device_family@), for example
  -- @SSymbol \@\"Cyclone V\"@
  SSymbol deviceFamily ->
  -- | Number of taps (@number_of_taps@)
  SNat nTaps ->
  -- | Distance between the taps (@tap_distance@), at least 3
  SNat distance ->
  -- | @clock@
  Clock dom ->
  -- | @clken@
  Enable dom ->
  -- | @shiftin@
  Signal dom a ->
  -- | @shiftout@, and @taps@ with the nearest tap first
  (Signal dom a, Signal dom (Vec nTaps a))
altshiftTaps _ SNat SNat clk en shiftin
  | clashSimulation = shiftModel
  | otherwise = (unpack <$> unPort shiftout, reverse . unpack <$> unPort taps)
 where
  (shiftout, taps) = ports
  ports ::
    ( Port "shiftout" dom (BitVector (BitSize a))
    , Port "taps" dom (BitVector (nTaps * BitSize a))
    )
  ports =
    inst
      (alteraMf "altshift_taps")
      (Param @"number_of_taps" @Integer (natToNum @nTaps))
      (Param @"tap_distance" @Integer (natToNum @distance))
      (Param @"width" @Integer (natToNum @(BitSize a)))
      (Param @"power_up_state" @String "CLEARED")
      (Param @"intended_device_family" @String (symbolVal (Proxy @deviceFamily)))
      (Param @"lpm_type" @String "altshift_taps")
      (ClockPort @"clock" clk)
      (Port @"clken" (boolToBit <$> fromEnable en))
      (Port @"shiftin" (pack <$> shiftin))

  total = natToNum @nTaps * natToNum @distance :: Int

  -- The registers, nearest first
  shiftModel =
    unbundle (go (P.replicate total (unpack 0)) (fromEnable en) shiftin)
  go regs ~(e :- es) ~(x :- xs) =
    out :- (e `seq` go regs' es xs)
   where
    out = (P.last regs, taps0)
    taps0 = map (\k -> regs P.!! ((fromIntegral k + 1) * natToNum @distance - 1)) indicesI
    regs'
      | e = x : P.init regs
      | otherwise = regs

-- * Memory initialization files

{- | Contents of a Memory Initialization File (@.mif@) for a memory with the
given words, in hexadecimal.
-}
mifContents :: forall n. (KnownNat n) => [BitVector n] -> String
mifContents ws =
  P.unlines $
    [ "WIDTH=" <> show (natToNum @n :: Integer) <> ";"
    , "DEPTH=" <> show (P.length ws) <> ";"
    , "ADDRESS_RADIX=HEX;"
    , "DATA_RADIX=HEX;"
    , "CONTENT BEGIN"
    ]
      <> [ "  " <> showHex i "" <> " : " <> showHex (toInteger w) "" <> ";"
         | (i, w) <- P.zip [0 :: Integer ..] ws
         ]
      <> ["END;"]

{- | Write a Memory Initialization File (@.mif@) for a memory with the given
words. To write it while compiling, use Template Haskell, e.g.

@
\$(runIO (writeMifFile "/path/to/ram.mif" [0 :: BitVector 8 .. 255]) >> [| () |])
@
-}
writeMifFile :: (KnownNat n) => FilePath -> [BitVector n] -> IO ()
writeMifFile path ws = writeFile path (mifContents ws)

{- | Read a Memory Initialization File (@.mif@) for a memory with the given
number of words. Supports the radixes @HEX@, @BIN@, @OCT@, @DEC@ and @UNS@, and
content lines with single addresses, address ranges (@[a..b]@) and multiple
values. Words the file doesn't mention are zero.
-}
readMifFile :: forall n. (KnownNat n) => Int -> FilePath -> IO [BitVector n]
readMifFile depth path = do
  contents <- readFile path
  let ws = parseMif depth contents
  _ <- evaluate (P.length ws)
  pure ws

parseMif :: forall n. (KnownNat n) => Int -> String -> [BitVector n]
parseMif depth src = [IntMap.findWithDefault 0 i entries | i <- [0 .. depth - 1]]
 where
  -- Strip comments: @--@ until the end of the line, and @%...%@
  stripComments = \case
    [] -> []
    '-' : '-' : rest -> stripComments (P.dropWhile (/= '\n') rest)
    '%' : rest -> stripComments (P.drop 1 (P.dropWhile (/= '%') rest))
    c : rest -> c : stripComments rest

  statements = splitOn ';' (stripComments src)
  splitOn c s = case P.break (== c) s of
    (a, []) -> [a]
    (a, _ : rest) -> a : splitOn c rest

  trim = L.dropWhileEnd isSpace . P.dropWhile isSpace
  upper = P.map toUpper

  header key = P.lookup key
    [ (trim (upper k), trim (upper (P.drop 1 v)))
    | st <- statements
    , let (k, v) = P.break (== '=') st
    , not (P.null v)
    ]
  addrRadix = fromMaybe "UNS" (header "ADDRESS_RADIX")
  dataRadix = fromMaybe "UNS" (header "DATA_RADIX")

  parseNum :: String -> String -> Integer
  parseNum radix s = case radix of
    "HEX" -> digits 16 isHexDigit
    "BIN" -> digits 2 (`P.elem` ("01" :: String))
    "OCT" -> digits 8 (`P.elem` ("01234567" :: String))
    _ -> case s of
      '-' : rest -> negate (parseNum "UNS" rest)
      _ -> digits 10 isDigit
   where
    digits base ok
      | P.all ok s && not (P.null s) = P.foldl (\acc c -> acc * base + toInteger (digitToInt c)) 0 s
      | otherwise = error ("readMifFile: bad number " <> show s)

  -- The statements after @CONTENT BEGIN@, up to @END@
  contentStatements = go statements
   where
    go [] = []
    go (st : sts)
      | "CONTENT" `L.isPrefixOf` trim (upper st) =
          let (_, afterBegin) = breakOnStr "BEGIN" st in afterBegin : sts
      | otherwise = go sts

  entries = IntMap.fromList (P.concatMap entry contentStatements)
  entry st0 = case P.break (== ':') (trim st0) of
    (_, []) -> []
    (addrPart, _ : valPart) ->
      let
        vals = P.map (fromInteger . parseNum dataRadix) (P.words valPart)
        addrs = case trim addrPart of
          '[' : range ->
            let (a, b) = breakOnStr ".." (P.takeWhile (/= ']') range)
            in [parseNum addrRadix (trim a) .. parseNum addrRadix (trim b)]
          a -> [parseNum addrRadix a ..]
      in case vals of
        [v] | '[' : _ <- trim addrPart -> [(fromInteger a, v) | a <- addrs]
        _ -> P.zip (P.map fromInteger addrs) vals

-- | Split a string at the first occurrence of a separator, dropping the
-- separator
breakOnStr :: String -> String -> (String, String)
breakOnStr sep = go []
 where
  go acc [] = (P.reverse acc, [])
  go acc s@(c : cs)
    | sep `L.isPrefixOf` P.map toUpper s = (P.reverse acc, P.drop (P.length sep) s)
    | otherwise = go (c : acc) cs

-- * Simulation model

-- | A word of which all bits are undefined
undefWord :: forall w. KnownNat w => BitVector w
undefWord = BV (2 P.^ (natToNum @w :: Int) - 1) 0

-- | Bits of the bytes enabled in a byte enable
byteMaskOf :: forall w nBytes. (KnownNat w, KnownNat nBytes) => BitVector nBytes -> BitVector w
byteMaskOf be = case maybeIsX be of
  Nothing -> undefWord
  Just _ ->
    P.foldl (.|.) 0
      [ (2 P.^ byteWidth - 1) `shiftL` (i * byteWidth)
      | i <- [0 .. nBytes - 1]
      , testBit be i
      ]
 where
  nBytes = natToNum @nBytes :: Int
  byteWidth = natToNum @w `P.div` nBytes

-- | Combine two words: the bits in the mask from the first, the others from
-- the second. Undefined bits stay undefined.
mergeWords :: forall w. KnownNat w => BitVector w -> BitVector w -> BitVector w -> BitVector w
mergeWords mask (BV m1 v1) (BV m2 v2) =
  BV ((m1 .&. mk) .|. (m2 .&. nmk)) ((v1 .&. mk) .|. (v2 .&. nmk))
 where
  BV _ mk = mask
  nmk = (2 P.^ (natToNum @w :: Int) - 1) `xor` mk

-- | A word of which the bits in the mask are undefined
undefBitsOf :: forall w. KnownNat w => BitVector w -> BitVector w -> BitVector w
undefBitsOf mask w = mergeWords mask undefWord w

-- | Initial contents of a memory: the initialization file, or all zeroes
initialContents :: forall n w. (KnownNat n, KnownNat w) => Maybe FilePath -> IntMap.IntMap (BitVector w)
initialContents file = IntMap.fromList (P.zip [0 ..] contents)
 where
  depth = natToNum @n :: Int
  contents = case file of
    Nothing -> P.replicate depth 0
    Just f -> unsafePerformIO (readMifFile depth f)

{- | Simulation model of a single-clock @altsyncram@ with two ports, working on
'BitVector's. Port B can be read-only. Returns the outputs of both ports.
-}
model ::
  forall n w nBytes dom.
  (KnownDomain dom, KnownNat n, KnownNat w, KnownNat nBytes) =>
  AltsyncramConfig ->
  PortConfig ->
  PortConfig ->
  Clock dom ->
  Enable dom ->
  -- | Port A: address and write
  Signal dom (Index n, Maybe (BitVector nBytes, BitVector w)) ->
  -- | Port B: address and write
  Signal dom (Index n, Maybe (BitVector nBytes, BitVector w)) ->
  (Signal dom (BitVector w), Signal dom (BitVector w))
model AltsyncramConfig{..} cfgA cfgB clk en portA portB =
  ( outReg cfgA (fst <$> latches)
  , outReg cfgB (snd <$> latches) )
 where
  undef = undefWord @w
  byteMask = byteMaskOf @w @nBytes
  mergeBits = mergeWords @w
  undefBits = undefBitsOf @w
  initMem = initialContents @n @w initFile

  -- One enabled clock cycle: new memory contents and read results
  ramCycle ::
    IntMap.IntMap (BitVector w) ->
    (Index n, Maybe (BitVector nBytes, BitVector w)) ->
    (Index n, Maybe (BitVector nBytes, BitVector w)) ->
    (IntMap.IntMap (BitVector w), (BitVector w, BitVector w))
  ramCycle mem (addrA0, wrA) (addrB0, wrB) =
    case (maybeIsX addrA0, maybeIsX addrB0) of
      (Just addrA1, Just addrB1) -> go (fromIntegral addrA1) (fromIntegral addrB1)
      _ -> (IntMap.map (const undef) mem, (undef, undef))
   where
    go addrA addrB = (mem2, (readA, readB))
     where
      old a = IntMap.findWithDefault undef a mem
      maskA = maybe 0 (byteMask . fst) wrA
      maskB = maybe 0 (byteMask . fst) wrB
      -- Both ports write the same bytes
      clash
        | addrA == addrB = maskA .&. maskB
        | otherwise = 0
      writeWord a mask d m = IntMap.adjust (mergeBits mask d) a m
      mem1 = case wrA of
        Just (_, d) -> writeWord addrA maskA d mem
        Nothing -> mem
      mem2' = case wrB of
        Just (_, d) -> writeWord addrB maskB d mem1
        Nothing -> mem1
      mem2 = IntMap.adjust (undefBits clash) addrA mem2'

      sameRead cfg addr wr mask = case wr of
        Nothing -> old addr
        Just (_, d) -> case readDuringWrite cfg of
          NewData -> undefBits (complement mask) d
          NewDataOldMaskedBytes -> mergeBits mask d (old addr)
          OldData -> old addr
          DontCare -> undef

      -- The other port writes the address this port reads
      mixed otherAddr otherWr otherMask addr r = case otherWr of
        Just _ | otherAddr == addr -> case mixedReadDuringWrite of
          MixedOldData -> r
          MixedDontCare -> undefBits otherMask r
        _ -> r

      -- Both ports write the same address: undefined in hardware
      bothWrite = isJust wrA && isJust wrB && addrA == addrB

      readA
        | bothWrite = undef
        | otherwise = mixed addrB wrB maskB addrA (sameRead cfgA addrA wrA maskA)
      readB
        | bothWrite = undef
        | otherwise = mixed addrA wrA maskA addrB (sameRead cfgB addrB wrB maskB)

  -- The output latches of both ports: the read results of the last enabled
  -- cycle
  latches = go initMem (0, 0) (fromEnable en) portA portB
   where
    go mem out ~(e :- es) ~(a :- as) ~(b :- bs) =
      out :- (e `seq` go mem' out' es as bs)
     where
      (mem', out')
        | e = ramCycle mem a b
        | otherwise = (mem, out)

  outReg cfg l
    | outputRegister cfg = register clk noReset en 0 l
    | otherwise = l

{- | Simulation model of a dual-clock @altsyncram@ with two ports, working on
'BitVector's: port A in domain @a@, port B in domain @b@. Port B can be
read-only. Returns the outputs of both ports.

With different clocks, the order of the edges of the two ports decides which
of them sees the other's writes. A write of a port completes some time after
the edge that writes, so we consider it /in flight/ until the next edge at which
the port is enabled (Intel's VHDL simulation model only completes it then). When a port reads an address that the
other port is writing (in flight, or at the same edge), the bytes written are
undefined in the read data. When both ports write the same address, the bytes
they both write become undefined.
-}
model2 ::
  forall n w nBytes a b.
  (KnownDomain a, KnownDomain b, KnownNat n, KnownNat w, KnownNat nBytes) =>
  AltsyncramConfig ->
  PortConfig ->
  PortConfig ->
  Clock a ->
  Enable a ->
  -- | Port A: address and write
  Signal a (Index n, Maybe (BitVector nBytes, BitVector w)) ->
  Clock b ->
  Enable b ->
  -- | Port B: address and write
  Signal b (Index n, Maybe (BitVector nBytes, BitVector w)) ->
  (Signal a (BitVector w), Signal b (BitVector w))
model2 cfg cfgA cfgB clkA enA portA clkB enB portB =
  (outReg clkA enA cfgA latchA, outReg clkB enB cfgB latchB)
 where
  undef = undefWord @w
  byteMask = byteMaskOf @w @nBytes
  undefBits = undefBitsOf @w

  (latchA, latchB) =
    go (clockTicks clkA clkB) (initialContents @n @w (initFile cfg)) (0, 0)
      (Nothing, Nothing) (fromEnable enA) portA (fromEnable enB) portB

  -- The writes in flight of both ports: address and bits written
  go ::
    [ClockAB] ->
    IntMap.IntMap (BitVector w) ->
    (BitVector w, BitVector w) ->
    (Maybe (Int, BitVector w), Maybe (Int, BitVector w)) ->
    Signal a Bool ->
    Signal a (Index n, Maybe (BitVector nBytes, BitVector w)) ->
    Signal b Bool ->
    Signal b (Index n, Maybe (BitVector nBytes, BitVector w)) ->
    (Signal a (BitVector w), Signal b (BitVector w))
  go [] _ _ _ _ _ _ _ = error "altsyncram: clockTicks should be infinite"
  go (tick : ticks) mem (oA, oB) (fA, fB) eas@(~(eA :- eas1)) as@(~(opA :- as1)) ebs@(~(eB :- ebs1)) bs@(~(opB :- bs1)) =
    case tick of
      ClockA -> (oA :- restA, restB)
       where
        (mem', oA', fA') = edge cfgA mem oA fA fB eA opA
        (restA, restB) = go ticks mem' (oA', oB) (fA', fB) eas1 as1 ebs bs
      ClockB -> (restA, oB :- restB)
       where
        (mem', oB', fB') = edge cfgB mem oB fB fA eB opB
        (restA, restB) = go ticks mem' (oA, oB') (fA, fB') eas as ebs1 bs1
      ClockAB -> (oA :- restA, oB :- restB)
       where
        -- Both ports see the other's write of this edge, and read the
        -- contents from before the edge
        fA1 = if eA then writeOf opA else fA
        fB1 = if eB then writeOf opB else fB
        (memA, oA', _) = edge cfgA mem oA fA fB1 eA opA
        (_, oB', _) = edge cfgB mem oB fB fA1 eB opB
        mem' = case opB of
          (addrB, Just _) | eB -> writeAt memA fA1 addrB opB
          _ -> memA
        (restA, restB) = go ticks mem' (oA', oB') (fA1, fB1) eas1 as1 ebs1 bs1

  writeOf (addr, wr) = case (maybeIsX addr, wr) of
    (Just a, Just (be, _)) -> Just (fromIntegral a, byteMask be)
    _ -> Nothing

  -- Write the data of an operation, making the bytes that the other port
  -- writes (in flight) undefined
  writeAt mem other addr0 (_, wr) = case (maybeIsX addr0, wr) of
    (Just addr1, Just (be, d)) ->
      let
        addr = fromIntegral addr1
        mask = byteMask be
        hit = case other of
          Just (a, m) | a == addr -> m .&. mask
          _ -> 0
      in IntMap.adjust (undefBits hit . mergeWords mask d) addr mem
    (Nothing, Just _) -> IntMap.map (const undef) mem
    _ -> mem

  -- An edge of a port: new memory contents, read data and write in flight
  edge pCfg mem out own other en op@(addr0, wr)
    | not en = (mem, out, own)
    | otherwise = case maybeIsX addr0 of
        Nothing -> (IntMap.map (const undef) mem, undef, Nothing)
        Just addr1 ->
          let
            addr = fromIntegral addr1
            old = IntMap.findWithDefault undef addr mem
            otherHit = case other of
              Just (a, m) | a == addr -> m
              _ -> 0
            readData = undefBits otherHit $ case wr of
              Nothing -> old
              Just (be, d) -> case readDuringWrite pCfg of
                NewData -> undefBits (complement (byteMask be)) d
                NewDataOldMaskedBytes -> mergeWords (byteMask be) d old
                OldData -> old
                DontCare -> undef
          in (writeAt mem other addr0 op, readData, writeOf op)

  outReg :: KnownDomain dom => Clock dom -> Enable dom -> PortConfig -> Signal dom (BitVector w) -> Signal dom (BitVector w)
  outReg clk en pCfg l
    | outputRegister pCfg = register clk noReset en 0 l
    | otherwise = l
