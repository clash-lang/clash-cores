# Revision history for clash-cores

## 0.1.0.0 -- YYYY-mm-dd

* Add SPI master and slave cores
* Add Lattice Semi SB_IO primitive
* Add UART core
* Add CRC core
* Add I2C controller core, ported from the `clash-compiler` examples
* Add `Clash.Cores.Xilinx.Unisim`, re-exporting all UNISIM primitives
* Add `bufg`, `bufgce` and `bufgceDiv`, wrappers for Xilinx's `BUFG`, `BUFGCE` and `BUFGCE_DIV` clock buffers. Gated and divided clocks are simulated cycle-accurately.
* Add `ibufds`, `obufds` and `ibufdsGte4`, wrappers for Xilinx's `IBUFDS` (for data signals), `OBUFDS` and `IBUFDS_GTE4` buffers
