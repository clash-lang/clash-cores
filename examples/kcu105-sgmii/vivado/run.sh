#!/usr/bin/env bash
# Driver for the KCU105 SGMII test design.
#
#   run.sh hdl       generate Verilog with Clash (run inside the clash-cores Nix shell)
#   run.sh build     synthesise, implement and write the bitstream and probes file
#   run.sh program   program the board through hw_server
#   run.sh vio       read the VIO probes; SET="vio_ctrl_tap=12 vio_ctrl_rx_reverse=1" sets outputs first
#
# Environment: VIVADO (installation, default Vivado Enterprise 2022.1),
# PART (default xcku040-ffva1156-2-e; xcku035-ffva1156-2-e needs no license),
# LICENSE (name of the license dongle, default zeldam; empty for none).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
pkg=$(cd "$here/.." && pwd)
root=$(cd "$pkg/../.." && pwd)
build=$pkg/_build
hdl=$build/clash/Kcu105.Sgmii.Top.topEntity
out=$build/vivado

: "${VIVADO:=/opt/tools/Xilinx/VivadoEnterprise/Vivado/2022.1}"
: "${PART:=xcku040-ffva1156-2-e}"
: "${LICENSE:=zeldam}"

# Vivado 2022.1 wants libtinfo.so.5, which this system no longer ships
mkdir -p "$build/shim"
ln -sf /usr/lib/x86_64-linux-gnu/libtinfo.so.6 "$build/shim/libtinfo.so.5"
export LD_LIBRARY_PATH=$build/shim${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
if [ -n "$LICENSE" ]; then
  export XILINXD_LICENSE_FILE=$VIVADO/../${LICENSE}_License.lic
fi

vivado_batch() {
  mkdir -p "$out"
  # Run inside the output directory: Vivado drops scratch files into the cwd
  (cd "$out" && "$VIVADO/bin/vivado" -mode batch -nojournal -log "$out/$1.log" -source "$here/$1.tcl")
}

case "${1:-}" in
  hdl)
    cd "$root"
    cabal --project-file=cabal.project.nix run kcu105-sgmii:exe:clash -- \
      Kcu105.Sgmii.Top --verilog -fclash-hdldir "$build/clash" -fclash-clear
    ;;
  build)
    connector=${CONNECTOR:-$(ghc-pkg field clash-lib data-dir --simple-output)/data-files/tcl/clashConnector.tcl}
    HDL_DIR=$hdl CONNECTOR=$connector PART=$PART XDC=$here/kcu105.xdc OUT_DIR=$out vivado_batch build
    ;;
  program)
    BIT=$out/topEntity.bit LTX=$out/topEntity.ltx vivado_batch program
    ;;
  vio)
    LTX=$out/topEntity.ltx vivado_batch vio
    ;;
  *)
    echo "usage: $0 hdl|build|program|vio" >&2
    exit 2
    ;;
esac
