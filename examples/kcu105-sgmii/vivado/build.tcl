# Synthesis, implementation and bitstream generation for the Clash output.
# Environment: HDL_DIR (Clash output directory with clash-manifest.json),
# CONNECTOR (clashConnector.tcl of clash-lib), PART, XDC, OUT_DIR.
set hdlDir $env(HDL_DIR)
set connector $env(CONNECTOR)
set part $env(PART)
set xdc $env(XDC)
set outDir $env(OUT_DIR)

set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR
source -notrace $connector
file delete -force $outDir/ip
file mkdir $outDir/ip $outDir/reports $outDir/checkpoints
clash::readMetadata $hdlDir

set hasIp [expr [llength [clash::GetAllTclIfaces {purposes createIp}]] > 0]
if {$hasIp} {
  create_project -in_memory -part $part
  set ips [clash::createIp -dir $outDir/ip]
  set ipFiles [get_property IP_FILE [get_ips $ips]]
  close_project
}

create_project -in_memory -part $part
clash::readHdl
if {$hasIp} {
  read_ip $ipFiles
  set_property GENERATE_SYNTH_CHECKPOINT false [get_files $ipFiles]
  generate_target {synthesis simulation} [get_ips $ips]
}
clash::readXdc {early normal late}
set_property TOP $clash::topEntity [current_fileset]
read_xdc -unmanaged $xdc

synth_design -name $clash::topEntity -mode default -part $part
report_timing_summary -file $outDir/reports/post_synth_timing_summary.rpt
report_utilization -hierarchical -file $outDir/reports/post_synth_util.rpt
write_checkpoint -force $outDir/checkpoints/post_synth.dcp

opt_design
place_design
phys_opt_design
route_design
report_timing_summary -file $outDir/reports/post_route_timing_summary.rpt
report_utilization -hierarchical -file $outDir/reports/post_route_util.rpt
report_clock_utilization -file $outDir/reports/post_route_clocks.rpt
write_checkpoint -force $outDir/checkpoints/post_route.dcp

set wns [get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]]
puts "WORST_SETUP_SLACK $wns"
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
write_bitstream -force $outDir/topEntity.bit
write_debug_probes -force $outDir/topEntity.ltx
puts "BUILD_DONE"
