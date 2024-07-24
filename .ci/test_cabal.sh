#!/bin/bash
set -xeou pipefail

cabal v2-build all -fci
cabal v2-run unittests -fci --enable-tests
cabal v2-run doctests -fci --enable-tests
cabal v2-sdist
