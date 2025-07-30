set -xueo pipefail

# This requires build.sh to be ran first
# Run this within a Nix development shell

# Copy over the master & local overwrite config
cp .ci/cabal.project-import-master cabal.project
cp .ci/cabal.project.local cabal.project.local

# Remove all source-repository-package inclusions of cabal.project files
# This is because it overwrites Nix sources, which breaks the build
find . -name 'cabal.project*' | xargs sed -i '/^source-repository-package/,/^$/d'

# the required clash build tool breaks Nix dependency management
# I.. don't quite know why this is needed..?
sed -i '/^  build-tool-depends:/,+1d' hdl-tests/cores-hdl-tests.cabal

# cabal update

# Build the cores package
# cabal v2-build cores-hdl-tests --write-ghc-environment-files always

# TODO: make this use actual core count
THREADS=4

# source /opt/tools/Xilinx/Vivado/2022.1/settings64.sh

# USE_GLOBAL_CLASH=1 $(cabal list-bin cores-hdl-tests) -j$THREADS --hide-successes
cores-hdl-tests -j$THREADS --hide-successes

