# Revision history for clash-cores

## 0.1.0.0 -- YYYY-mm-dd

* Add SPI master and slave cores
* Add Lattice Semi SB_IO primitive
* Add UART core
* Add CRC core
* Add I2C controller core, ported from the `clash-compiler` examples
* Add `Clash.Cores.Xilinx.Unisim`, re-exporting all UNISIM primitives
* Add `bufg`, `bufgce` and `bufgceDiv`, wrappers for Xilinx's `BUFG`, `BUFGCE` and `BUFGCE_DIV` clock buffers. Gated and divided clocks are simulated cycle-accurately.
* Add `ibufds`, `obufds`, `ibufdsGte4` and `ibufdsGte3`, wrappers for Xilinx's `IBUFDS` (for data signals), `OBUFDS`, `IBUFDS_GTE4` and `IBUFDS_GTE3` buffers
* Add `iddre1` and `oddre1`, wrappers for Xilinx's `IDDRE1` and `ODDRE1` DDR registers, following the conventions of `Clash.Xilinx.DDR`
* Add `iserdese3` and `oserdese3`, wrappers for Xilinx's `ISERDESE3` and `OSERDESE3` serializers/deserializers
* Add `idelaye3`, `odelaye3` and `idelayctrl`, wrappers for Xilinx's `IDELAYE3`, `ODELAYE3` and `IDELAYCTRL` primitives. The delay lines' data paths are simulated without delay; their tap counters are simulated cycle-accurately.
