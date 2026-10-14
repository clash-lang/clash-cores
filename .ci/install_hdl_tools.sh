#!/bin/bash
# Fetch the HDL simulators used by the HDL testsuite from the nixpkgs pinned in
# flake.lock and add them to the PATH of subsequent GitHub Actions steps. The
# self-hosted runners have Nix, but no Docker, so we can't use the clash-ci
# image like we did on GitLab.
set -xueo pipefail

nixpkgs=$(jq -r '.nodes.nixpkgs.locked | "github:\(.owner)/\(.repo)/\(.rev)"' flake.lock)

for pkg in ghdl-llvm iverilog verilator; do
  bin_dir="$(nix build --no-link --print-out-paths "${nixpkgs}#${pkg}^out")/bin"
  echo "${bin_dir}" >> "${GITHUB_PATH:-/dev/stdout}"
done
