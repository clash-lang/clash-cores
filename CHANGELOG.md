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
* Fix `instWithIntelIp` referring to the memory initialization files of IP, like the tables of floating point division, by a path that doesn't exist, so that their simulation stopped at time 0
* Add `sqrt`, `recip`, `rsqrt`, `exp` and `log` to `Clash.Cores.Intel.Floating`, with simulation models that give the IP's results bit for bit, although Intel only documents them to be faithfully rounded. The models were obtained by black-box testing of the IP; their results depend on the device family, given as a `DeviceFamily` argument
* Add `log2` and `log10` to `Clash.Cores.Intel.Floating`, with simulation models that give the IP's results bit for bit, obtained like the ones of `log`
* Add `exp2` and `exp10` to `Clash.Cores.Intel.Floating`, with simulation models that give the IP's results bit for bit, obtained like the ones of `exp`. Unlike what Intel documents, their results are not always faithfully rounded
* Add `log1p` (`log (1 + x)`) to `Clash.Cores.Intel.Floating`, with a simulation model that gives the IP's results bit for bit, obtained like the one of `log`
* Add `cbrt` (cube root) to `Clash.Cores.Intel.Floating`, with a simulation model that gives the IP's results bit for bit, obtained like the one of `sqrt`
* Add `hypot` (three-dimensional hypotenuse, `sqrt (x^2 + y^2 + z^2)`) to `Clash.Cores.Intel.Floating`, with a simulation model that gives the IP's results bit for bit, obtained by black-box testing of the IP like the one of `sqrt`. Unlike what Intel documents, its results are not always faithfully rounded
* Add `sinPi` and `cosPi` (`sin (pi * x)` and `cos (pi * x)`) to `Clash.Cores.Intel.Floating`, with simulation models that give the IP's results bit for bit, obtained like the ones of `log`. Unlike what Intel documents, the results of `sinPi` are not always faithfully rounded
* Add `tanPi` (`tan (pi * x)`) to `Clash.Cores.Intel.Floating`, with a simulation model that gives the IP's results bit for bit, obtained like the one of `sinPi`. Unlike what Intel documents, its results are not always faithfully rounded
* Add `Clash.Cores.Intel.Floating.Half` and `Clash.Cores.Intel.Floating.Half.Explicit`: Intel's Floating Point Functions IP in half precision (`Half`). Supports `sqrt`, `cbrt`, `hypot`, `recip`, `rsqrt`, `exp`, `exp2`, `exp10`, `log`, `log2`, `log10`, `log1p`, `sinPi`, `cosPi` and `tanPi`, with simulation models that give the IP's results bit for bit, obtained by black-box testing of the IP and verified on all inputs (for `hypot`: on all pairs and 50 million triples)
* Add `Clash.Cores.Intel.Floating.Double` and `Clash.Cores.Intel.Floating.Double.Explicit`: Intel's Floating Point Functions IP in double precision (`Double`). Supports `add`, `sub`, `mul`, `div`, `sqrt` and `recip`, with simulation models that give the IP's results bit for bit (`sqrt` obtained by black-box testing of the IP)
* Add `sin`, `cos` and `tan` (of an angle in radians) to `Clash.Cores.Intel.Floating`, with simulation models that give the IP's results bit for bit, obtained like the ones of `sinPi`, `cosPi` and `tanPi`. Unlike what Intel documents, their results are not always faithfully rounded
