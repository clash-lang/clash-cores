# Sweep the receive delay tap and record alignment and synchronisation for
# each value, in a single hardware manager session.
# Environment: LTX (probes file); optional STEP (default 8), DWELL (ms, default 100),
# RX_REVERSE / TX_REVERSE (0/1, default 0), HW_SERVER.
set url [expr {[info exists env(HW_SERVER)] ? $env(HW_SERVER) : "localhost:3121"}]
set step [expr {[info exists env(STEP)] ? $env(STEP) : 8}]
set dwell [expr {[info exists env(DWELL)] ? $env(DWELL) : 100}]
set rxRev [expr {[info exists env(RX_REVERSE)] ? $env(RX_REVERSE) : 0}]
set txRev [expr {[info exists env(TX_REVERSE)] ? $env(TX_REVERSE) : 0}]
open_hw_manager
connect_hw_server -url $url
open_hw_target
set dev [lindex [get_hw_devices xcku0*] 0]
current_hw_device $dev
set_property PROBES.FILE $env(LTX) $dev
set_property FULL_PROBES.FILE $env(LTX) $dev
refresh_hw_device $dev
set vio [lindex [get_hw_vios -of_objects $dev] 0]
proc setout {vio name value} {
  set p [get_hw_probes -of_objects $vio -filter "NAME =~ */$name"]
  set_property OUTPUT_VALUE_RADIX UNSIGNED $p
  set_property OUTPUT_VALUE $value $p
  commit_hw_vio $p
}
proc getin {vio name} {
  set p [get_hw_probes -of_objects $vio -filter "NAME =~ */$name"]
  set_property INPUT_VALUE_RADIX UNSIGNED $p
  return [get_property INPUT_VALUE $p]
}
setout $vio vio_ctrl_rx_reverse $rxRev
setout $vio vio_ctrl_tx_reverse $txRev
puts "SWEEP rx_reverse=$rxRev tx_reverse=$txRev step=$step dwell=${dwell}ms"
puts [format "%5s %6s %5s %7s %4s %5s %8s" tap locked bs_ok sync_ok xmit speed frames]
for {set tap 0} {$tap < 512} {incr tap $step} {
  setout $vio vio_ctrl_tap $tap
  after $dwell
  refresh_hw_vio $vio
  # sample twice to see whether sync is stable
  set s1 [getin $vio vio_sync_ok]
  after $dwell
  refresh_hw_vio $vio
  set s2 [getin $vio vio_sync_ok]
  puts [format "%5d %6s %5s %3s/%-3s %4s %5s %8s" $tap [getin $vio vio_locked] [getin $vio vio_bs_ok] $s1 $s2 [getin $vio vio_xmit] [getin $vio vio_link_speed] [getin $vio vio_frames]]
}
puts "SWEEP_DONE"
