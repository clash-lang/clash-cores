#!/bin/bash
set -xou pipefail

grep -E ' $' -n -r . --include=*.{hs,hs-boot,sh} --exclude-dir=dist-newstyle
if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi

set -e

# Print out versions for debugging purposes
cabal --version
ghc --version

# This might happen during tags on GitLab CI
CI_COMMIT_BRANCH=${CI_COMMIT_BRANCH:-no_branch_set_by_ci}

if [[ "$CLASH_BRANCH" == "master" ]]; then
  cp .ci/cabal.project-import-master cabal.project
fi

cabal v2-update | tee cabal_update_output

# File may exist as part of a dist.tar.zst
if [ ! -f cabal.project.local ]; then
  cp .ci/cabal.project.local .

  set +u
  if [[ "$WORKAROUND_GHC_MMAP_CRASH" == "yes" ]]; then
    sed -i 's/-workaround-ghc-mmap-crash/+workaround-ghc-mmap-crash/g' cabal.project.local
  fi
  set -u

  # Fix index-state to prevent rebuilds if Hackage changes between build -> test.
  # Note we can't simply set it to a timestamp of "now", as Cabal will error out
  # when its index state is older than what's mentioned in cabal.project(.local).
  most_recent_index_state=$(grep "The index-state is set to" cabal_update_output | grep -E -o '[^ ]+Z\.$' | tr -d .)
  sed -i "s/HEAD/${most_recent_index_state}/g" cabal.project.local
fi

cat cabal.project
cat cabal.project.local

rm -f ${HOME}/.cabal/config
cabal user-config init
sed -i "s/-- ghc-options:/ghc-options: -j$THREADS/g" ${HOME}/.cabal/config
set +u
if [[ "$WORKAROUND_GHC_MMAP_CRASH" == "yes" ]]; then
  sed -i "s/ghc-options:/ghc-options: +RTS -xm20000000 -RTS -with-rtsopts=-xm20000000/g" ${HOME}/.cabal/config
fi
set -u
sed -i "s/^[- ]*jobs:.*/jobs: $CABAL_JOBS/g" ${HOME}/.cabal/config
sed -i "/remote-repo-cache:.*/d" ${HOME}/.cabal/config
cat ${HOME}/.cabal/config
