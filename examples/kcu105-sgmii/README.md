# KCU105 SGMII test design

A minimal design for the Xilinx KCU105 that connects the pure-Clash SGMII PCS of
clash-cores (`Clash.Cores.Sgmii.sgmii`) to the on-board Marvell 88E1111 PHY
through a synchronous LVDS serializer and deserializer, and echoes received
frames back to the link partner. It exists to exercise the PCS, and in
particular its 8b/10b coder, against real hardware.

## Design

* `Kcu105.Sgmii.Domains`: the clock domains. The PHY supplies the 625 MHz
  SGMII clock; an MMCM derives the 625 MHz SERDES bit clock, the 312.5 MHz
  SERDES parallel clock and the 125 MHz code group clock from it. The board's
  125 MHz oscillator is only used for the LEDs and the reset. `Line1250` is a
  simulation-only domain with one bit per cycle for the serial line.
* `Kcu105.Sgmii.Primitives`: `IBUFDS`, `OBUFDS`, `ISERDESE3` and `OSERDESE3`
  (4-bit DDR mode) and `IDELAYE3` (count mode, tap loaded from the fabric),
  each with a behavioural model for simulation. Bit 0 of a nibble is the bit
  on the line first; the receive and transmit nibble order can be reversed at
  run time in case the primitives order their bits the other way.
* `Kcu105.Sgmii.Gearbox`: 4-to-10 and 10-to-4 bit gearboxes, and the clock
  crossings through dual-clock FIFOs (pairs of code groups, one pair per five
  SERDES cycles and per two code group cycles).
* `Kcu105.Sgmii.Serdes`: the receive and transmit paths.
* `Kcu105.Sgmii.Top`: `sgmiiDemo` (simulatable) and `topEntity` with the VIO,
  ILA and LEDs.

This follows the structure of Xilinx's own synchronous "SGMII over LVDS"
design for UltraScale, minus its eye monitor: the receive delay tap is set
through the VIO.

## Building

Everything runs from the clash-cores Nix dev shell (`nix develop` in the
repository root; the flake includes this package), with the untracked project
file `cabal.project.nix`:

    cabal --project-file=cabal.project.nix run kcu105-sgmii:test:sim   # simulation tests
    examples/kcu105-sgmii/vivado/run.sh hdl                              # Verilog into _build/clash
    examples/kcu105-sgmii/vivado/run.sh build                            # bitstream and probes into _build/vivado
    examples/kcu105-sgmii/vivado/run.sh program                          # program the board via hw_server
    examples/kcu105-sgmii/vivado/run.sh vio                              # read the VIO
    SET="vio_ctrl_tap=40" examples/kcu105-sgmii/vivado/run.sh vio        # set an output probe, then read

`run.sh build` uses Vivado Enterprise 2022.1 and the node-locked KU040 license
of the USB Ethernet dongle (`LICENSE=zeldam` by default, see the license
files next to the Vivado installation). The dongle has to be plugged in. For
a dry run without a license use `PART=xcku035-ffva1156-2-e LICENSE=`.

## Bring-up

1. Connect the KCU105's RJ45 to the USB Ethernet dongle; NetworkManager's
   `FPGA` profile gives the dongle 10.0.0.1/24.
2. Program the board. LED 7 blinks (board clock), LED 0 lights when the MMCM
   is locked to the PHY clock.
3. Read the VIO. `vio_bs_ok` and `vio_sync_ok` show comma alignment and
   synchronisation. If they stay low, sweep `vio_ctrl_tap` (0..511, the
   delay line covers more than one bit period) and try `vio_ctrl_rx_reverse`.
   `vio_ctrl_pcs_reset` resets the PCS after a change.
4. With sync, `vio_xmit` becomes 1 (data) once auto-negotiation completes and
   `vio_link_speed` shows the negotiated speed (2 = 1000 Mb/s). LEDs 1..3
   show alignment, sync and link. If the PHY does not accept our transmit
   stream, try `vio_ctrl_tx_reverse`.
5. Send frames from the host with `host/echo_test.sh eth0 50`: it sends
   broadcast UDP datagrams and reports how many frames the dongle received
   back (the kernel drops echoed frames carrying its own MAC but counts them).
   `vio_frames` counts received frames; `vio_rx_errors` counts cycles with
   `RX_ER`, which includes the carrier extension after every frame. With
   `tcpdump -i eth0 -e` (needs the `pcap` group or root) the echoes are
   visible one by one. LED 4 toggles with received frames, LEDs 5 and 6 flag
   FIFO errors.
6. `vivado/run.sh sweep` sweeps the delay tap and prints alignment and sync
   per tap; `vivado/run.sh ila` captures the next received frame into
   `_build/vivado/ila.csv` (the GMII data includes the preamble).
7. The ILA `ilaSgmii` captures the received code groups, decoded bytes and
   status bits; open the probes file in the Vivado hardware manager.
