# Revision history for clash-cores

## 0.1.0.0 -- YYYY-mm-dd

* Add SPI master and slave cores
* Add Lattice Semi SB_IO primitive
* Add UART core
* Add CRC core
* Add I2C controller core, ported from the `clash-compiler` examples
* Add `Clash.Cores.Xilinx.Fir`, a wrapper for Xilinx's FIR Compiler LogiCORE IP
  * Single rate filters for a sample every clock cycle (`firWith`) and for
    valid-qualified samples with longer sample periods (`firValidWith`)
  * Integer decimation (`firDecimateWith`)
  * Integer interpolation (`firInterpolateWith`)
