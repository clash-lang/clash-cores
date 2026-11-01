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
