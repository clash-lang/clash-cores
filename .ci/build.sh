#!/bin/bash
set -xueo pipefail

# test that we can create a build plan with the index-state in cabal.project
mv cabal.project.local cabal.project.local.disabled
[[ ! -f cabal.project.freeze ]] || mv cabal.project.freeze cabal.project.freeze.disabled
if ! cabal v2-build --dry-run all > /dev/null; then
  echo Maybe the index-state should be updated?
  exit 1
fi
mv cabal.project.local.disabled cabal.project.local
[[ ! -f cabal.project.freeze.disabled ]] || mv cabal.project.freeze.disabled cabal.project.freeze

# Build with default constraints
cabal v2-build all

# Put all the test binaries in a predictable location
TESTS="
clash-cores:doctests
clash-cores:unittests
clash-cores-test-suite:clash-cores-test-suite
"
mkdir bin
for TEST in $TESTS; do
  ln -s "$(realpath --relative-to=bin "$(cabal list-bin $TEST)")" bin/$TEST
done
