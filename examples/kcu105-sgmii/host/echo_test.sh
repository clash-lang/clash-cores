#!/usr/bin/env bash
# Frame echo test from the host, without packet capture: send broadcast UDP
# datagrams towards the board's link and watch the receive counters of the
# dongle. Every frame the design receives is echoed back; the host's kernel
# drops frames carrying its own source MAC, but still counts them as received.
#
#   echo_test.sh [interface] [count] [payload bytes]      default: eth0, 20, 100
set -euo pipefail
ifc=${1:-eth0}
count=${2:-20}
size=${3:-100}
stats=/sys/class/net/$ifc/statistics
snap() { echo "$(cat $stats/rx_packets) $(cat $stats/rx_bytes) $(cat $stats/rx_errors) $(cat $stats/rx_crc_errors) $(cat $stats/rx_frame_errors)"; }
addr=$(ip -4 -o addr show dev "$ifc" | awk '{print $4}' | cut -d/ -f1)
read -r rx0 rxb0 rxe0 rxc0 rxf0 <<<"$(snap)"
python3 - "$addr" "$count" "$size" <<'PY'
import socket, sys, time
addr, count, size = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
s.bind((addr, 0))
for i in range(count):
    s.sendto(bytes([i & 0xff]) * size, ("10.0.0.255", 9))
    time.sleep(0.01)
PY
sleep 1
read -r rx1 rxb1 rxe1 rxc1 rxf1 <<<"$(snap)"
echo "sent $count broadcast UDP datagrams of $size bytes on $ifc ($((size + 42)) bytes per frame)"
echo "received: $((rx1 - rx0)) frames, $((rxb1 - rxb0)) bytes, errors $((rxe1 - rxe0)) (crc $((rxc1 - rxc0)), frame $((rxf1 - rxf0)))"
