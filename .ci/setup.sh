#!/bin/bash
set -xou pipefail

grep -E ' $' -n -r . --include=*.{hs,hs-boot,sh} --exclude-dir=dist-newstyle
if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi

set -e

THREADS=${THREADS:-$(.ci/effective_cpus.sh)}
CABAL_JOBS=${CABAL_JOBS:-$THREADS}
CABAL_DIR=${CABAL_DIR:-$HOME/.cabal}

# Print out versions for debugging purposes
cabal --version
ghc --version

if [[ "$CLASH_BRANCH" == "master" ]]; then
  cp .ci/cabal.project-import-master cabal.project
fi

.ci/retry.sh cabal v2-update | tee cabal_update_output

if [ ! -f cabal.project.local ]; then
  cp .ci/cabal.project.local .

  # Fix index-state to prevent rebuilds if Hackage changes between build -> test.
  # Note we can't simply set it to a timestamp of "now", as Cabal will error out
  # when its index state is older than what's mentioned in cabal.project(.local).
  most_recent_index_state=$(grep "The index-state is set to" cabal_update_output | grep -E -o '[^ ]+Z\.$' | tr -d .)
  sed -i "s/HEAD/$most_recent_index_state/g" cabal.project.local
fi

cat cabal.project
cat cabal.project.local

rm -f $CABAL_DIR/config
cabal user-config init
sed -i "s/-- ghc-options:/ghc-options: -j$THREADS/g" $CABAL_DIR/config
sed -i "s/^[- ]*jobs:.*/jobs: $CABAL_JOBS/g" $CABAL_DIR/config
sed -i "/remote-repo-cache:.*/d" $CABAL_DIR/config
cat $CABAL_DIR/config
