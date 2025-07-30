set -xueo pipefail

# TESTING ONLY!!
THREADS=4

source /opt/tools/Xilinx/Vivado/2022.1/settings64.sh
bin/cores-hdl-tests:cores-hdl-tests -j$THREADS --hide-successes
