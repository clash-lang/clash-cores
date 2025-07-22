# Clash Cores
**Warning: Still under construction**

This repository contains a number of existing useful circuits written in Clash.

These circuits include:
- SPI core
- UART core
- CRC core
- Etherbone core
- 8b10b line encoder/decoder
- SGMII PCS receiver/transmitter
- Various wrappers around xilinx IPs (blockram, dcfifo, floating, ila, xpm ...)

Note: The `SGMII` circuits require `clash-prelude >= 1.9.0` and will be skipped if built against an earlier version of Clash.

This code was originally a sub-folder within the clash-compiler repo, but now it's being moved into its own repository. This move is still in progress. You can still find the previous clash-cores folder [here](https://github.com/clash-lang/clash-compiler/tree/master/clash-cores)  and see the status of moving it to this repo [here](https://github.com/clash-lang/clash-compiler/issues/2757)

If you're looking for a pre-built Clash circuit and don't find it here, you can also check out [Clash Protocols](https://github.com/clash-lang/clash-protocols/tree/main/clash-protocols/src/Protocols) to see if it exists there.

You can also look through Adam Walker's excellent list of [prebuilt Clash circuits](https://github.com/adamwalker/clash-utils).
