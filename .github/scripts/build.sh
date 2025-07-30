set -xueo pipefail

# Run this within a Nix development shell

### PATCHING ###

# Copy over the master & local overwrite config
cp .ci/cabal.project-import-master cabal.project
cp .ci/cabal.project.local cabal.project.local

# Remove all source-repository-package inclusions of cabal.project files
# This is because it overwrites Nix sources, which breaks the build
find . -name 'cabal.project*' | xargs sed -i '/^source-repository-package/,/^$/d'

# the required clash build tool breaks Nix dependency management
# I.. don't quite know why this is needed..?
sed -i '/^  build-tool-depends:/,+1d' hdl-tests/cores-hdl-tests.cabal

### BUILDING ###

# Update cabal hackage list
cabal update

# Build the hdl-cores-tests
TEST=cores-hdl-tests:cores-hdl-tests
cabal v2-build $TEST
ln -s "$(realpath --relative-to=bin "$(cabal list-bin $TEST)")" hdl-tests

