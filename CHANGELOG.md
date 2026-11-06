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
* Add `Clash.Cores.Intel.Internal.instWithIntelIp`, the Intel counterpart of `instWithXilinxWizard`: it instantiates Intel IP and writes a Platform Designer system (`.qsys` file) that generates it. If `CLASH_INTEL_SIM_FAMILY` is set, Clash also runs `qsys-generate` and adds the IP's simulation model to its output.
* Add `Clash.Cores.Intel.Floating`, support for Intel's Floating Point Functions IP (`altera_fp_functions`): addition, subtraction, multiplication, division, multiply-add, comparisons, and conversions from and to 32 bit integers and `Double`, with bit-exact simulation models. The API follows `Clash.Cores.Xilinx.Floating`.
* Add `Clash.Cores.Intel.Divide` and `Clash.Cores.Intel.Sqrt`, wrappers for Intel's `lpm_divide` and `altsqrt` megafunctions (integer division and square root), with bit-exact simulation models
