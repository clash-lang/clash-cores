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
* Fix the VHDL of the ECP5 `bidirectionalBuffer`, which declared the pad of `BB`/`BBPU`/`BBPD` as an input. It is now instantiated with `inst` and takes its component declarations from `ecp5u.components`, like Lattice Diamond designs do. This removes `Clash.Cores.LatticeSemi.Ecp5.Blackboxes.IO`.
* Test the ECP5 `bidirectionalBuffer` against the simulation models of Yosys
* Add `ilvds`, `olvds` and `obco`, wrappers for the ECP5 differential I/O buffers `ILVDS`, `OLVDS` and `OBCO`
