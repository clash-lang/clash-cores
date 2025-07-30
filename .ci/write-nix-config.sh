#!/bin/bash
set -xueo pipefail

cat > /etc/nix/nix.conf << EOF
build-users-group = nixbld
sandbox = true

cores = 0
max-jobs = 1

experimental-features = nix-command flakes

substituters = https://cache.nixos.org/ http://192.168.102.136:9200/public
trusted-substituters = http://192.168.102.136:9200/public
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= qbaylogic:E+mHLefkbkx3rcy72m1VtDnshciQ2Zt/APqDRlVhKPs=
EOF

nix --experimental-features nix-command config show

nix build .#
echo $(nix path-info .#)

