#! /usr/bin/env bash

# This script shows some parts of the .git directory together with the output
# of some Git commands (e.g. `log`). This is useful to compare with the state
# managed by Gitcraft.hs as operations are applied.

set -e

cd ${TMP_DIR}

cat .git/HEAD
find .git/refs/heads/ -type f -print -exec cat {} \;
git branch
if [[ -f .git/refs/heads/master ]] ; then
  git log --no-decorate
fi
