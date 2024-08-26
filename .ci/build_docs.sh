#!/bin/bash
set -xueo pipefail

mkdir -p hadocs

# Cache dependencies
cabal v2-build clash-cores -O0 --enable-documentation --only-dependencies

cabal v2-haddock clash-cores -O0 --enable-documentation \
  |  tee haddock_log

set +e

# Temporarily disabled, as there are hundreds of TH-generated instances without
# documentation.
#if grep -q "Missing documentation" haddock_log; then
#  echo -e "\e[1m\e[31mMissing documentation! Scroll up for full log.\e[0m"
#  grep --color=always -n -C 5 "Missing documentation" haddock_log
#  exit 1
#fi

out_of_scope_warn="If you qualify the identifier, haddock can try to link it anyway"
if grep -q "${out_of_scope_warn}" haddock_log; then
  echo -e "\e[1m\e[31mIdentifier out of scope in ${pkg}! Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 "${out_of_scope_warn}" haddock_log
  exit 1
fi

link_dest_warn="could not find link destinations for:"
if grep -q "${link_dest_warn}" haddock_log; then
  echo -e "\e[1m\e[31mCould not find link destination in ${pkg}! Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 "${link_dest_warn}" haddock_log
  exit 1
fi

ambiguous_warn="You may be able to disambiguate the identifier by"
if grep -q "${ambiguous_warn}" haddock_log; then
  echo -e "\e[1m\e[31mAmbiguous identifier found in ${pkg}! Scroll up for full log.\e[0m"
  grep --color=always -n -C 5 "${ambiguous_warn}" haddock_log
  exit 1
fi

# Copy documention to hadocs/
ln -s "$(dirname "$(tail -n1 haddock_log)")" hadocs/${pkg}
