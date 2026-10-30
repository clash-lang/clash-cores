{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Wrappers for Xilinx's @XPM_MEMORY@ macros, see
  [UG974](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/XPM_MEMORY_TDPRAM):

  * 'xpmMemoryTdpram': true dual port RAM, with two ports that read and write
  * 'xpmMemorySdpram': simple dual port RAM, with a write port and a read port
  * 'xpmMemorySpram': single port RAM
  * 'xpmMemorySprom': single port ROM

  Each function instantiates the corresponding macro when generating HDL, and
  uses a simulation model when simulating in Haskell. The model follows the
  behavioral model Vivado's simulator uses, and is tested against it.

  = Ports

  Addresses are 'Index'es, so the depth of a memory does not need to be a power
  of two. Writing ports have a write enable with one bit per byte (a
  'BitVector'), where byte /i/ consists of bits @[i * w, (i + 1) * w)@ of the
  'pack'ed data, with @w@ the width of a byte: the width of the data divided by
  the number of bits in the write enable. Use a write enable of a single bit to
  write whole words. Otherwise, bytes must be /8/ or /9/ bits wide.

  A port does nothing while its enable is deasserted: it does not read or
  write, and its read register keeps its value. A port reads on every enabled
  clock cycle. When it writes at the same time, its read result depends on
  its 'WriteMode'. With a read latency of /1/, the read register is the output
  register, and the reset sets it to the reset value. With a read latency @n@
  larger than /1/, the read register is followed by @n - 1@ output registers.
  The last output register is enabled by @regce@ and reset by the reset; the
  registers in between take the value of the previous register when the port
  was enabled @n - 1@ cycles earlier. Resets are synchronous or asynchronous,
  depending on the domain, and active high or low, depending on the domain.
  Registers start (at power-up) with the reset value, except in 'WriteFirst'
  and 'ReadFirst' mode with a read latency larger than /1/: then the read
  register and the registers in between start at zero.

  = Clocks

  Ports in the same domain share a clock: the macro is instantiated with
  @CLOCKING_MODE = \"common_clock\"@. Ports in different domains are considered
  to have independent clocks (@CLOCKING_MODE = \"independent_clock\"@). The
  model assumes the active edges of the clocks of two different domains
  coincide at the start of the simulation, like they do in Clash's test benches.
  Only domains with a rising active edge are supported. Dynamic clocks are not
  supported.

  = Collisions

  Two ports accessing the same address can collide. The model returns
  'XException's (undefined bits) for read results and memory contents that are
  undefined in hardware:

  [Block RAM, common clock] When both ports write the same address, the written
  bits become undefined, and so do the read results of both ports. When one port
  reads an address the other port writes, the read result is undefined, unless
  both ports are in 'ReadFirst' mode, in which case it is the old contents.
  This also applies to 'AutoPrimitive'.

  [UltraRAM] Port A's operation happens before port B's: port B reads data port
  A writes, and port A reads the old contents of an address port B writes. When
  both write the same address, port B's data wins. In simple dual port,
  'WriteFirst' mode, port B reads the data port A writes.

  [Distributed RAM, common clock] Port B reads the old contents of an address
  port A writes. With a read latency of zero, the output shows the new contents
  after the clock edge.

  [Independent clocks] A read or write is a collision if the other port wrote
  the same address at the same time or slightly earlier: within the collision
  window of Vivado's simulation model. This window is 3000 ps, or the period of
  the fastest clock if that is shorter. Colliding reads are undefined, and so
  are both writes when both ports write. Like Vivado's simulation model, an
  access is not considered a collision if the other port writes the same address
  slightly /later/. In hardware, accesses within the clock-to-clock setup time
  of the RAM collide, regardless of their order. Vivado's simulation model
  also makes the reads of /both/ ports undefined while one port is about to
  write an address the other port wrote within the collision window, even
  when they read other addresses. The model does not do this.

  Undefined enables, addresses, and write enables make the affected outputs and
  memory contents undefined too.

  = Unsupported features

  The following features of the macros are not supported. The wrappers set
  their parameters to fixed values.

  * Error correction (@ECC_MODE@): only supported for simple dual port RAMs with
    a width that is a multiple of /64/ and without initialization. Fixed to
    @\"no_ecc\"@.
  * Sleep (@WAKEUP_TIME@, @sleep@ port) and automatic sleep for UltraRAM
    (@AUTO_SLEEP_TIME@): power saving features with intricate behavior in
    Vivado's simulation model. Fixed to @\"disable_sleep\"@ and /0/.
  * Asymmetric ports (different read and write widths): fixed to symmetric.
  * Initialization with a string (@MEMORY_INIT_PARAM@), which is limited to
    memories of 4 kbit; use 'InitFile' (and 'initFileTH') instead.
  * Mixed primitives (@MEMORY_PRIMITIVE = \"mixed\"@).
  * Synthesis and simulation tuning: @CASCADE_HEIGHT@, @MEMORY_OPTIMIZATION@,
    @MESSAGE_CONTROL@, @SIM_ASSERT_CHK@, @USE_EMBEDDED_CONSTRAINT@,
    @USE_MEM_INIT@, @USE_MEM_INIT_MMI@ and @WRITE_PROTECT@ use their default
    values.
-}
module Clash.Cores.Xilinx.Xpm.Memory
  ( -- * Memories
    xpmMemoryTdpram
  , xpmMemoryTdpramWith
  , xpmMemorySdpram
  , xpmMemorySdpramWith
  , xpmMemorySpram
  , xpmMemorySpramWith
  , xpmMemorySprom
  , xpmMemorySpromWith

    -- * Configuration
  , XpmMemoryConfig(..)
  , defaultXpmMemoryConfig
  , XpmMemoryPortConfig(..)
  , defaultXpmMemoryPortConfig
  , MemoryPrimitive(..)
  , WriteMode(..)

    -- * Initialization
  , MemoryInit(..)
  , xpmMemFile
  , initFileTH
  ) where

import Clash.Cores.Xilinx.Xpm.Memory.Internal
import Clash.Cores.Xilinx.Xpm.Memory.Sdpram
import Clash.Cores.Xilinx.Xpm.Memory.Spram
import Clash.Cores.Xilinx.Xpm.Memory.Sprom
import Clash.Cores.Xilinx.Xpm.Memory.Tdpram
