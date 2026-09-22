# Read all VIO probes, optionally after setting output probes.
# Environment: LTX (probes file); optional SET, a list of NAME=VALUE pairs;
# optional HW_SERVER.
set url [expr {[info exists env(HW_SERVER)] ? $env(HW_SERVER) : "localhost:3121"}]
open_hw_manager
connect_hw_server -url $url
open_hw_target
set dev [lindex [get_hw_devices xcku0*] 0]
current_hw_device $dev
set_property PROBES.FILE $env(LTX) $dev
set_property FULL_PROBES.FILE $env(LTX) $dev
refresh_hw_device $dev
set vio [lindex [get_hw_vios -of_objects $dev] 0]
if {[info exists env(SET)]} {
  foreach assignment $env(SET) {
    lassign [split $assignment =] name value
    set probe [get_hw_probes $name -of_objects $vio]
    set_property OUTPUT_VALUE $value $probe
    commit_hw_vio $probe
    puts "SET $name = $value"
  }
}
refresh_hw_vio $vio
foreach probe [lsort [get_hw_probes -of_objects $vio]] {
  set name [get_property NAME $probe]
  if {[get_property TYPE $probe] eq "vio_input"} {
    puts [format "%-24s %s" $name [get_property INPUT_VALUE $probe]]
  } else {
    puts [format "%-24s %s (output)" $name [get_property OUTPUT_VALUE $probe]]
  }
}
puts "VIO_DONE"
