# Revision history for clash-cores

## 0.1.0.0 -- YYYY-mm-dd

* Add SPI master and slave cores
* Add Lattice Semi SB_IO primitive
* Add UART core
* Add CRC core
* Add I2C controller core, ported from the `clash-compiler` examples
* Add half and double precision to the Xilinx Floating-Point core:
  `Clash.Cores.Xilinx.Floating.Half` and `Clash.Cores.Xilinx.Floating.Double`,
  each with an `.Explicit` variant
* Add square root (`sqrt`), absolute value (`abs`), conversions from 64-bit
  integers (`fromU64`, `fromS64`), conversion to `Signed 32` (`toS32`) and
  conversions between precisions (`toHalf`, `toFloat` and `toDouble`) to the
  Xilinx Floating-Point core
