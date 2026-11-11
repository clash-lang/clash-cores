# Revision history for clash-cores

## 0.1.0.0 -- YYYY-mm-dd

* Add SPI master and slave cores
* Add Lattice Semi SB_IO primitive
* Add UART core
* Add CRC core
* Add I2C controller core, ported from the `clash-compiler` examples
* Fix `inst` rendering an undefined value instead of an instance when it is not applied to any ports or parameters ([#80](https://github.com/clash-lang/clash-cores/issues/80))
* Fix `inst` connecting a `BiSignalInPort` to the instance through an intermediate signal, which made it impossible for the instance to drive the bidirectional signal
* Fix `inst` failing when some, but not all, of its output ports have a zero-width type, such as a `BiSignalOutPort` next to a `Port`. Zero-width input ports are now left out of the instantiation too. ([#35](https://github.com/clash-lang/clash-cores/issues/35))
* Fix `inst` connecting a `ResetPort` output whose polarity does not match its domain's polarity to the inverted signal, instead of inverting the output of the instance
* Add support for differential clock outputs (`DiffClockPort` and `NamedDiffClockPort`) to `inst` ([#33](https://github.com/clash-lang/clash-cores/issues/33))
* Add `BitVectorParam` to `inst`, for parameters that are a `bit_vector` in VHDL, such as the `INIT` generic of `LUT6` ([#81](https://github.com/clash-lang/clash-cores/issues/81))
* Add `BoolStringParam` to `inst`, for boolean parameters that are a `boolean` in VHDL but a `"TRUE"`/`"FALSE"` string in Verilog, such as the `IBUF_LOW_PWR` parameter of `IBUF` ([#82](https://github.com/clash-lang/clash-cores/issues/82))
* Add `BitBiSignalInPort` to `inst`, for single-bit bidirectional ports that are a `std_logic` in VHDL, such as the `IO` port of `IOBUF`. A 1-bit `BiSignalInPort` renders as a `std_logic_vector(0 downto 0)`, which VHDL does not allow to be connected to such ports.
* Add `Clash.Cores.Intel.IO`: wrappers for the I/O buffer primitives of Intel FPGAs `ALT_OUTBUF_TRI`, `ALT_IOBUF`, `OPNDRN`, `ALT_INBUF_DIFF`, `ALT_OUTBUF_DIFF` and `ALT_IOBUF_DIFF`
* Fix `inst` failing on a `BiSignalInPort` or `BiSignalOutPort` whose width is a type family application, such as `BitSize a`
* Add `Clash.Cores.Intel.DDR` with `altddioBidir`, a wrapper for `ALTDDIO_BIDIR` on a `BiSignal` pad, completing `altddioIn` and `altddioOut` of `clash-prelude` (which it re-exports)
* Add `Clash.Cores.Intel.Cdc` with `stdSynchronizer` and `stdSynchronizerBundle`, wrappers for `altera_std_synchronizer` and `altera_std_synchronizer_bundle`
* Add `Clash.Cores.Intel.Fifo` with `scfifo`, a wrapper for Intel's single-clock FIFO `scfifo`
* Add `dcfifo` to `Clash.Cores.Intel.Fifo`, a wrapper for Intel's dual-clock FIFO `dcfifo`
* Add `Clash.Cores.Intel.Memory` with `altsyncramSinglePort`, `altsyncramSimpleDualPort` and `altsyncramTrueDualPort`, wrappers for Intel's block RAM `altsyncram`, and functions to write and read Memory Initialization Files
* Add `altshiftTaps` to `Clash.Cores.Intel.Memory`, a wrapper for Intel's RAM-based shift register `altshift_taps`
* Add `Clash.Cores.Intel.Clock` with `altclkctrl`, a global clock buffer with an enable (`ALTCLKCTRL`) for Cyclone IV E, Cyclone 10 LP and MAX 10
* Add `Clash.Cores.Intel.Internal.instWithIntelIp`, the Intel counterpart of `instWithXilinxWizard`: it instantiates Intel IP and writes a Platform Designer system (`.qsys` file) that generates it. If `CLASH_INTEL_SIM_FAMILY` is set, Clash also runs `qsys-generate` and adds the IP's simulation model to its output.
* Add `Clash.Cores.Intel.Floating`, support for Intel's Floating Point Functions IP (`altera_fp_functions`): addition, subtraction, multiplication, division, multiply-add, comparisons, and conversions from and to 32 bit integers and `Double`, with bit-exact simulation models. The API follows `Clash.Cores.Xilinx.Floating`.
* Add `Clash.Cores.Intel.Divide` and `Clash.Cores.Intel.Sqrt`, wrappers for Intel's `lpm_divide` and `altsqrt` megafunctions (integer division and square root), with bit-exact simulation models
* Add `altsyncramRom` and `altsyncramDualPortRom` (ROMs) and `altsyncramSimpleDualPortDualClock` and `altsyncramTrueDualPortDualClock` (RAMs with a clock per port) to `Clash.Cores.Intel.Memory`
* Add `avalonStClockCrosser` to `Clash.Cores.Intel.Cdc`, the Avalon-ST Handshake Clock Crosser of Platform Designer
* Add a read enable (`rden_b`) to the read port of `altsyncramSimpleDualPort` and `altsyncramSimpleDualPortDualClock`: their read address is now a `Maybe`
* Add `Clash.Cores.Intel.Cordic`, support for the sine and cosine function of Intel's CORDIC IP (`altera_CORDIC`), for signed and unsigned angles, with bit-exact simulation models. The user guide only promises faithfully rounded results; the models reproduce the datapath of the IP, which was inferred by observing it as a black box.
* Add `atan2`, `atan2Unsigned` and the `vectorTranslate` family to `Clash.Cores.Intel.Cordic`: the arctangent and vector translation (angle and magnitude, with or without scale factor compensation) functions of Intel's CORDIC IP, with bit-exact simulation models inferred from observing the IP as a black box. Formats whose datapath wasn't measured raise an error in simulation.
* Add `vectorRotate` to `Clash.Cores.Intel.Cordic`: the vector rotation function of Intel's CORDIC IP (without scale factor compensation), with a bit-exact simulation model inferred from observing the IP as a black box, including inputs outside the documented ranges.
* Add `atan2Lut` and `atan2UnsignedLut` to `Clash.Cores.Intel.Cordic`: the arctangent of Intel's CORDIC IP with its "LUT Size Optimization" option, which computes the angle of the vector translation. The vector translation functions now take a `DeviceFamily`: on Cyclone V their angle is computed with tables of six rotation directions instead of four, which round differently; their models were wrong on Cyclone V before. The module documents where the IP differs from its user guide.
