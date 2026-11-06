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
* Add `componentDeclaration` to `InstConfig`, which makes `inst` declare the component it instantiates in VHDL, for primitives that no VHDL library declares for every simulator. Bidirectional ports are declared as `inout`, `BitVectorParam`s as `bit_vector` and `Bit` parameters as `bit`.
* `sbio` now simulates all pin types, including registered and inverted outputs and registered output enables. It is now tested against Yosys' simulation model of `SB_IO`. `LATCH_INPUT_VALUE` only affects the `PIN_INPUT*_LATCH` pin types, like in Yosys' model. In VHDL, `sbio` now declares `PACKAGE_PIN` as `inout` and connects `std_logic`s to `CLOCK_ENABLE` and `OUTPUT_ENABLE`. Clock domains with a falling active edge set `NEG_TRIGGER`. The hidden module `Clash.Cores.LatticeSemi.Ice40.Blackboxes.IO` is gone: `sbio` uses `inst`.
* Add `sbioDdr`, a variant of `sbio` with its pad in a domain with half the period of the clock, to simulate the double data rate features of `SB_IO`
* Add `sbGb`, `sbGbSignal`, `sbGbIo` and `sbGbIoClock`, wrappers for the iCE40 global buffers `SB_GB` and `SB_GB_IO`
* Add `sbioOd`, a wrapper for the open-drain I/O cell `SB_IO_OD` of the iCE40 UltraPlus devices
* Add `Clash.Cores.LatticeSemi.Ice40.Memory` with `sbRam40_4k`, a wrapper for the iCE40 block RAM `SB_RAM40_4K` and its variants with inverted clocks, with typed read and write modes and initial contents
* Add `sbSpram256ka`, a wrapper for the 256 kbit single-port RAM `SB_SPRAM256KA` of the iCE40 UltraPlus devices, including its low-power modes
