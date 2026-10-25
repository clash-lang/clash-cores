# Revision history for clash-cores

## 0.1.0.0 -- YYYY-mm-dd

* Add SPI master and slave cores
* Add Lattice Semi SB_IO primitive
* Add UART core
* Add CRC core
* Add I2C controller core, ported from the `clash-compiler` examples
* Add half and double precision to the Xilinx Floating-Point core
  (`Clash.Cores.Xilinx.Floating.Half` and `Clash.Cores.Xilinx.Floating.Double`),
  and `Clash.Cores.Xilinx.Floating.Generic`, which is generic in the precision;
  each has an `.Explicit` variant
* Add square root (`sqrt`), absolute value (`abs`), conversions from 64-bit
  integers (`fromU64`, `fromS64`), conversion to `Signed 32` (`toS32`) and
  conversions between precisions (`convert`, and `toHalf`, `toFloat` and
  `toDouble` in the modules for a single precision) to the Xilinx
  Floating-Point core
