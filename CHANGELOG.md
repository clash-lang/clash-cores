# Revision history for clash-cores

## 0.1.0.0 -- YYYY-mm-dd

* Add SPI master and slave cores
* Add Lattice Semi SB_IO primitive
* Add UART core
* Add CRC core
* Add I2C controller core, ported from the `clash-compiler` examples
* Add half and double precision to the Xilinx Floating-Point core:
  `Clash.Cores.Xilinx.Floating.Half`, `Clash.Cores.Xilinx.Floating.Double` and
  `Clash.Cores.Xilinx.Floating.Generic`, each with an `.Explicit` variant
