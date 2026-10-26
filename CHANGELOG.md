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
  `toDouble` in the per-precision modules) to the Xilinx Floating-Point core
* Add reciprocal (`recip`), reciprocal square root (`rsqrt`), exponential
  (`exp`) and natural logarithm (`log`) to the Xilinx Floating-Point core, in
  every precision. Unlike the other operations, Clash simulation does not match
  the IP bit for bit: it gives the correctly rounded results, while the IP is
  only accurate to one unit in the last place, so they can differ in the last
  bit
* Add fused multiply-add (`fma`, `x * y + z` with a single rounding) to the
  Xilinx Floating-Point core, in every precision
* Add the accumulator (`accum`, `accumWith` and `accumWithFlags`, with
  `AccumConfig`, `defAccumConfig` and `AccumFlags`) to the Xilinx
  Floating-Point core, in every precision. It sums in fixed point with
  configurable bounds and rounds its results towards zero; `accumWithFlags`
  also outputs its overflow and invalid operation flags. The documentation of
  `Clash.Cores.Xilinx.Floating.Generic` describes how to derive the exception
  flags of the other operations from their inputs and results
