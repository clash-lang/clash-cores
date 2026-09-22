# Capture received frames with the ILA: trigger on the rising edge of rx_dv,
# generate traffic on the host, upload the samples and write them as CSV.
# Environment: LTX; optional IFACE (default eth0), CSV (default ila.csv), HW_SERVER.
set url [expr {[info exists env(HW_SERVER)] ? $env(HW_SERVER) : "localhost:3121"}]
set ifc [expr {[info exists env(IFACE)] ? $env(IFACE) : "eth0"}]
set csv [expr {[info exists env(CSV)] ? $env(CSV) : "ila.csv"}]
open_hw_manager
connect_hw_server -url $url
open_hw_target
set dev [lindex [get_hw_devices xcku0*] 0]
current_hw_device $dev
set_property PROBES.FILE $env(LTX) $dev
set_property FULL_PROBES.FILE $env(LTX) $dev
refresh_hw_device $dev
set ila [lindex [get_hw_ilas -of_objects $dev] 0]
set dv [get_hw_probes -of_objects $ila -filter {NAME =~ */ila_rx_dv}]
set_property TRIGGER_COMPARE_VALUE eq1'bR $dv
set_property CONTROL.TRIGGER_POSITION 64 $ila
set_property CONTROL.DATA_DEPTH 2048 $ila
run_hw_ila $ila
after 500
catch {exec ping -b -c 3 -i 0.2 -W 1 -I $ifc 10.0.0.255} out
wait_on_hw_ila -timeout 10 $ila
upload_hw_ila_data $ila
write_hw_ila_data -force -csv_file $csv [current_hw_ila_data]
puts "ILA_DONE $csv"
