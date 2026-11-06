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
* Add `Clash.Cores.Xilinx.Unisim`, re-exporting all UNISIM primitives
* Add `bufg`, `bufgce` and `bufgceDiv`, wrappers for Xilinx's `BUFG`, `BUFGCE` and `BUFGCE_DIV` clock buffers. Gated and divided clocks are simulated cycle-accurately.
* Add `ibufds`, `obufds`, `ibufdsGte4` and `ibufdsGte3`, wrappers for Xilinx's `IBUFDS` (for data signals), `OBUFDS`, `IBUFDS_GTE4` and `IBUFDS_GTE3` buffers
* Add `iddre1` and `oddre1`, wrappers for Xilinx's `IDDRE1` and `ODDRE1` DDR registers, following the conventions of `Clash.Xilinx.DDR`
* Add `iserdese3` and `oserdese3`, wrappers for Xilinx's `ISERDESE3` and `OSERDESE3` serializers/deserializers
* Add `idelaye3`, `odelaye3` and `idelayctrl`, wrappers for Xilinx's `IDELAYE3`, `ODELAYE3` and `IDELAYCTRL` primitives. The delay lines' data paths are simulated without delay; their tap counters are simulated cycle-accurately.
* Add `startupE3`, `usrAccessE2` and `icapE3`, wrappers for Xilinx's `STARTUPE3`, `USR_ACCESSE2` and `ICAPE3` configuration primitives
* Add `Clash.Cores.Xilinx.Unisim.Series7`, re-exporting the UNISIM primitives of 7 series devices
* Add `bufio`, `bufr`, `bufrDiv`, `bufh`, `bufhce`, `bufmr` and `bufmrce`, wrappers for the 7 series clock buffers `BUFIO`, `BUFR`, `BUFH`, `BUFHCE`, `BUFMR` and `BUFMRCE`. Gated and divided clocks are simulated cycle-accurately, including clears of `BUFR` while its input clock is stopped.
