# Pin constraints for the KCU105 (board file xilinx.com:kcu105:part0:1.7).
# The clocks themselves are defined in the SDC file Clash generates.

# Board clock and reset
set_property -dict {PACKAGE_PIN G10 IOSTANDARD LVDS} [get_ports CLK_125MHZ_p]
set_property -dict {PACKAGE_PIN F10 IOSTANDARD LVDS} [get_ports CLK_125MHZ_n]
set_property -dict {PACKAGE_PIN AN8 IOSTANDARD LVCMOS18} [get_ports CPU_RESET]

# SGMII: 625 MHz clock and serial data from and to the Marvell PHY
set_property -dict {PACKAGE_PIN P26 IOSTANDARD LVDS_25} [get_ports SGMIICLK_p]
set_property -dict {PACKAGE_PIN N26 IOSTANDARD LVDS_25} [get_ports SGMIICLK_n]
set_property -dict {PACKAGE_PIN P24 IOSTANDARD DIFF_HSTL_I_18} [get_ports SGMII_RX_p]
set_property -dict {PACKAGE_PIN P25 IOSTANDARD DIFF_HSTL_I_18} [get_ports SGMII_RX_n]
set_property -dict {PACKAGE_PIN N24 IOSTANDARD DIFF_HSTL_I_18} [get_ports SGMII_TX_p]
set_property -dict {PACKAGE_PIN M24 IOSTANDARD DIFF_HSTL_I_18} [get_ports SGMII_TX_n]

# LEDs GPIO_LED_0_LS .. GPIO_LED_7_LS
set_property -dict {PACKAGE_PIN AP8 IOSTANDARD LVCMOS18} [get_ports {GPIO_LED[0]}]
set_property -dict {PACKAGE_PIN H23 IOSTANDARD LVCMOS18} [get_ports {GPIO_LED[1]}]
set_property -dict {PACKAGE_PIN P20 IOSTANDARD LVCMOS18} [get_ports {GPIO_LED[2]}]
set_property -dict {PACKAGE_PIN P21 IOSTANDARD LVCMOS18} [get_ports {GPIO_LED[3]}]
set_property -dict {PACKAGE_PIN N22 IOSTANDARD LVCMOS18} [get_ports {GPIO_LED[4]}]
set_property -dict {PACKAGE_PIN M22 IOSTANDARD LVCMOS18} [get_ports {GPIO_LED[5]}]
set_property -dict {PACKAGE_PIN R23 IOSTANDARD LVCMOS18} [get_ports {GPIO_LED[6]}]
set_property -dict {PACKAGE_PIN P23 IOSTANDARD LVCMOS18} [get_ports {GPIO_LED[7]}]

# Timing
set_clock_groups -asynchronous \
  -group [get_clocks -include_generated_clocks CLK_125MHZ_p] \
  -group [get_clocks -include_generated_clocks SGMIICLK_p]
set_false_path -from [get_ports CPU_RESET]
# The serial lines are handled by the SERDES primitives; there is no fabric timing path
set_false_path -from [get_ports {SGMII_RX_p SGMII_RX_n}]
set_false_path -to [get_ports {SGMII_TX_p SGMII_TX_n}]
# Clocks of the design: the clock wizard IP drives the SERDES bit clock
# (clk_out1) and the code group clock (clk_out2); a BUFGCE_DIV divides the bit
# clock into the SERDES parallel clock. The XDC is read unmanaged, so Tcl is
# allowed here.
set wizPins [get_pins -of_objects [get_cells -hierarchical -filter {REF_NAME =~ topEntity_clk_wiz_*_clk_wiz}]]
set bitClkNet [get_nets -of_objects [filter $wizPins {REF_PIN_NAME == clk_out1}]]
set pcsClk [get_clocks -of_objects [filter $wizPins {REF_PIN_NAME == clk_out2}]]
set divPin [get_pins -of_objects [get_cells -hierarchical -filter {REF_NAME == BUFGCE_DIV}] -filter {REF_PIN_NAME == O}]
set divClkNet [get_nets -of_objects $divPin]
set divClk [get_clocks -of_objects $divPin]

# The SERDES parallel clock and the code group clock only meet in dual-clock
# FIFOs and quasi-static status values
set_clock_groups -asynchronous -group $divClk -group $pcsClk
# Match the routing delay of the SERDES bit clock and its divided clock, as the
# Xilinx LVDS SGMII design does
set_property CLOCK_DELAY_GROUP serdes_clocks [list $bitClkNet $divClkNet]
