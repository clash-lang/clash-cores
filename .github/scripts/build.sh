set -xueo pipefail

# Patch cabal.project files so they:
# 1. Don't include source mentions
# 2. Use latest, not 1.8
# sed -i '/^source-repository-package/,/^$/d' cabal.project*
# sed -i 's/1\.8/master/' cabal.project
rm cabal.project
cat > cabal.project << EOF
packages:
  clash-cores.cabal
  hdl-tests
EOF

# Make sure cabal has a package list
cabal update

# Build tests and put them in a known location (bin)
mkdir bin
TESTS="
clash-cores:doctests
clash-cores:unit-tests
cores-hdl-tests:cores-hdl-tests
"
for TEST in $TESTS; do
  cabal v2-build $TEST
  ln -s "$(realpath --relative-to=bin "$(cabal list-bin $TEST)")" bin/$TEST
done
