# Program the KCU105 through the running hw_server and load the debug probes.
# Environment: BIT (bitstream), LTX (probes file), optional HW_SERVER (default localhost:3121).
set url [expr {[info exists env(HW_SERVER)] ? $env(HW_SERVER) : "localhost:3121"}]
open_hw_manager
connect_hw_server -url $url
open_hw_target
set dev [lindex [get_hw_devices xcku0*] 0]
current_hw_device $dev
set_property PROGRAM.FILE $env(BIT) $dev
set_property PROBES.FILE $env(LTX) $dev
set_property FULL_PROBES.FILE $env(LTX) $dev
program_hw_devices $dev
refresh_hw_device $dev
puts "PROGRAM_DONE"
