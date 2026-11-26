# Revision history for clash-cores

## 0.1.0.0 -- YYYY-mm-dd

* Add SPI master and slave cores
* Add Lattice Semi SB_IO primitive
* Add UART core
* Add CRC core
* Add I2C controller core, ported from the `clash-compiler` examples
* Add `spiMasterTransaction`, an SPI master that keeps slave select asserted
  for transactions of more than one word
* Breaking change in `spiSlave`, `spiSlaveLatticeSBIO` and
  `spiSlaveLatticeBB`, SPI modes 1 and 3: the slave now latches each word on
  the first shift edge of the word, where its first bit must be on MISO.
  Before, the slave latched the next word on the last sample edge of the
  previous word, and the first word only while slave select was high. Thus,
  user logic that changes the data input when a word ends, or when slave
  select goes low, now sends the changed data one word early. Keep each word
  stable on the data input until the slave latches it. In return, the slave
  can now send a response to a word in the next word, in all modes.
* `spiMaster` with a half period of 2 or more (MISO latch): the master now
  also delays its data input, so it sends the word that was on the input in
  the clock cycle in which the transfer starts. Before, it used the input of
  one clock cycle later.
